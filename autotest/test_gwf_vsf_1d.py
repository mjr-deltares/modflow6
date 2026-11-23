"""
Tests for the VSF_1D_ONLY option of the variably saturated flow (VSF) package.

With VSF_1D_ONLY the Richards formulation is applied to vertical connections
only. Horizontal connections only conduct flow (with the saturated
conductance) when both cells are fully saturated, i.e. the pressure head at
the node is non-negative (head at or above the cell center) in both cells.

The tests:

- compare VSF and VSF_1D_ONLY for a laterally uniform 3D model, which has
  no horizontal flow, so the results must be identical;
- check that horizontal flow is zero for all connections involving a cell
  with a negative pressure head, in a 2D (xz) water-table model;
- benchmark VSF against VSF_1D_ONLY for a larger 3D infiltration model
  (marked slow), reporting run times and iteration counts.

The benchmark model size can be changed with the environment variables
VSF_BENCH_NROW, VSF_BENCH_NCOL and VSF_BENCH_NLAY. The scenario can be changed
with VSF_BENCH_WT0 (initial water table, cm), VSF_BENCH_RCH (peak recharge as a
fraction of K) and VSF_BENCH_HOURS (simulation length). Setting a high water
table and recharge (e.g. 100, 0.2, 6) gives a near-saturated domain.
"""

import json
import os
import re
import time
from pathlib import Path

import flopy
import numpy as np
import pytest
from gwf_test_utils import get_vsf_soil_data

HK = 0.00944  # cm/s, Celia et al. (1990)


def build_sim(
    ws,
    exe,
    name,
    nlay,
    nrow,
    ncol,
    delr,
    delc,
    height,
    strt,
    vsf_1d_only,
    newton,
    perlen,
    dtmin,
    dt0,
    dtmax,
    rch_rate=None,
    chd_data=None,
    dvclose=1e-6,
):
    """Build a transient VSF model in cm and s with the Haverkamp soil."""
    sim = flopy.mf6.MFSimulation(sim_name=name, sim_ws=ws, exe_name=exe)
    tdis = flopy.mf6.ModflowTdis(
        sim, time_units="SECONDS", nper=1, perioddata=[(perlen, 1, 1.0)]
    )
    tdis.ats.initialize(
        maxats=1,
        perioddata=[(0, dt0, dtmin, dtmax, 2.0, 2.0)],
        filename=f"{name}.ats",
    )

    gwf = flopy.mf6.ModflowGwf(
        sim,
        modelname=name,
        save_flows=True,
        newtonoptions="newton" if newton else None,
    )
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        complexity="COMPLEX",
        outer_dvclose=dvclose,
        inner_dvclose=0.01 * dvclose,
        outer_maximum=100,
        inner_maximum=300,
        linear_acceleration="BICGSTAB",
    )
    sim.register_ims_package(ims, [gwf.name])

    delz = height / nlay
    botm = [height - (k + 1) * delz for k in range(nlay)]
    flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=height,
        botm=botm,
    )
    flopy.mf6.ModflowGwfic(gwf, strt=strt)
    flopy.mf6.ModflowGwfnpf(gwf, icelltype=0, k=HK, k33=HK)
    flopy.mf6.ModflowGwfsto(gwf, ss=1e-6, sy=0.0, iconvert=0, transient={0: True})

    soil = get_vsf_soil_data("Celia1990-eq10-Haverkamp")
    flopy.mf6.ModflowGwfvsf(
        gwf,
        ivarsat=1,
        soil_model="haverkamp",
        storage_scheme="chord-slope" if not newton else None,
        kr_averaging="arithmetic",
        vsf_1d_only=vsf_1d_only,
        porosity=soil["porosity"],
        satres=soil["satres"],
        alphahvk=soil["alpha"],
        nhvk=soil["n"],
        betahvk=soil["beta"],
        khvk=soil["k"],
    )

    if rch_rate is not None:
        flopy.mf6.ModflowGwfrcha(gwf, recharge=rch_rate)
    if chd_data:
        flopy.mf6.ModflowGwfchd(gwf, stress_period_data={0: chd_data})

    flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=f"{name}.hds",
        budget_filerecord=f"{name}.cbc",
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
        printrecord=[("BUDGET", "LAST")],
    )
    return sim


def run_sim(sim):
    t0 = time.perf_counter()
    success, buff = sim.run_simulation(silent=True)
    wall = time.perf_counter() - t0
    assert success, f"simulation {sim.name} failed:\n" + "\n".join(buff[-20:])
    return wall


def get_heads(ws, name):
    return flopy.utils.HeadFile(Path(ws) / f"{name}.hds").get_data()


def get_flowja(ws, name):
    cbc = flopy.utils.CellBudgetFile(Path(ws) / f"{name}.cbc", precision="double")
    return cbc.get_data(text="FLOW-JA-FACE")[-1].flatten()


def get_ia_ja(ws, name):
    grb = flopy.mf6.utils.MfGrdFile(Path(ws) / f"{name}.dis.grb")
    return grb.ia, grb.ja


def get_run_stats(ws):
    """Parse solver statistics and timing from the listing files."""
    stats = {}
    sim_lst = (Path(ws) / "mfsim.lst").read_text()
    m = re.search(r"Elapsed run time:\s*(.*)", sim_lst)
    stats["elapsed"] = m.group(1).strip() if m else None
    for key, label in [
        ("t_formulate", "Total formulate time"),
        ("t_solution", "Total solution time"),
    ]:
        m = re.search(rf"{label}\s*:?\s*([0-9.Ee+-]+)", sim_lst)
        stats[key] = float(m.group(1)) if m else None

    # IMS summary per step: "<n> CALLS TO NUMERICAL SOLUTION IN TIME STEP"
    # followed by "<n> TOTAL ITERATIONS" (inner)
    calls = [int(c) for c in re.findall(r"(\d+) CALLS TO NUMERICAL", sim_lst)]
    inner = [int(c) for c in re.findall(r"(\d+) TOTAL ITERATIONS", sim_lst)]
    stats["n_steps"] = len(calls)
    stats["n_outer"] = int(np.sum(calls))
    stats["n_inner"] = int(np.sum(inner))
    return stats


def horizontal_mask(ia, ja, ncpl):
    """Mask (size nja) of horizontal connections for a structured grid."""
    nodes = len(ia) - 1
    n = np.repeat(np.arange(nodes), np.diff(ia))
    return (n // ncpl) == (ja // ncpl)


# --- uniform columns: 1D and full VSF must give identical results ---


@pytest.mark.parametrize("newton", [False, True], ids=["picard", "newton"])
def test_uniform_columns(function_tmpdir, targets, newton):
    nlay, nrow, ncol = 20, 3, 3
    height = 100.0
    strt = np.full((nlay, nrow, ncol), 40.0)
    kwargs = dict(
        exe=targets["mf6"],
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=10.0,
        delc=10.0,
        height=height,
        strt=strt,
        newton=newton,
        perlen=1800.0,
        dtmin=60.0,
        dt0=60.0,
        dtmax=60.0,
        rch_rate=0.1 * HK,
        dvclose=1e-9,
    )

    heads = {}
    for case, one_d in [("full", False), ("1d", True)]:
        ws = function_tmpdir / case
        name = f"vsf{case}"
        sim = build_sim(ws, name=name, vsf_1d_only=one_d, **kwargs)
        sim.write_simulation(silent=True)
        run_sim(sim)
        heads[case] = get_heads(ws, name)

    # infiltration must have raised the heads near the top, while the
    # column remains unsaturated there
    assert heads["full"][0].min() > strt[0].min() + 1.0
    assert heads["full"][0].max() < height
    # laterally uniform: no horizontal flow, so identical solutions
    assert np.allclose(heads["1d"], heads["full"], rtol=0.0, atol=1e-6), (
        f"max diff {np.abs(heads['1d'] - heads['full']).max()}"
    )


# --- no horizontal flow for connections with a partially saturated cell ---


@pytest.mark.parametrize("newton", [False, True], ids=["picard", "newton"])
def test_horizontal_flow_saturated_only(function_tmpdir, targets, newton):
    nlay, nrow, ncol = 20, 1, 20
    height, width = 200.0, 800.0
    delr = width / ncol
    delz = height / nlay
    h_left, h_right = 180.0, 20.0
    strt = np.full((nlay, nrow, ncol), h_right)
    chd_data = []
    for k in range(nlay):
        zc = height - (k + 0.5) * delz
        if zc < h_left:
            chd_data.append([(k, 0, 0), h_left])
        if zc < h_right:
            chd_data.append([(k, 0, ncol - 1), h_right])

    name = "vsf1dwt"
    sim = build_sim(
        function_tmpdir,
        targets["mf6"],
        name,
        nlay,
        nrow,
        ncol,
        delr,
        delr,
        height,
        strt,
        vsf_1d_only=True,
        newton=newton,
        perlen=12 * 3600.0,
        dtmin=1.0,
        dt0=60.0,
        dtmax=3600.0,
        chd_data=chd_data,
    )
    sim.write_simulation(silent=True)
    run_sim(sim)

    heads = get_heads(function_tmpdir, name).flatten()
    flowja = get_flowja(function_tmpdir, name)
    ia, ja = get_ia_ja(function_tmpdir, name)
    nodes = len(ia) - 1
    n = np.repeat(np.arange(nodes), np.diff(ia))
    is_diag = n == ja
    is_horiz = horizontal_mask(ia, ja, nrow * ncol) & ~is_diag

    # fully saturated: non-negative pressure head at the node
    zc = np.repeat(height - (np.arange(nlay) + 0.5) * delz, nrow * ncol)
    full_sat = heads >= zc
    both_sat = full_sat[n] & full_sat[ja]

    # horizontal connections with an unsaturated cell carry no flow
    q_unsat = flowja[is_horiz & ~both_sat]
    assert np.all(q_unsat == 0.0), f"max |q| unsaturated: {np.abs(q_unsat).max()}"

    # but there is horizontal flow through the saturated zone
    q_sat = flowja[is_horiz & both_sat]
    assert np.abs(q_sat).max() > 0.0

    # mass balance per cell
    res = flowja[ia[:-1]]
    assert np.allclose(res, 0.0, atol=1e-6), f"max residual {np.abs(res).max()}"


# --- performance: full VSF vs VSF_1D_ONLY for a 3D infiltration model ---


def build_bench(ws, exe, name, vsf_1d_only, newton):
    nlay = int(os.environ.get("VSF_BENCH_NLAY", 40))
    nrow = int(os.environ.get("VSF_BENCH_NROW", 20))
    ncol = int(os.environ.get("VSF_BENCH_NCOL", 20))
    height = 200.0  # cm
    delr = delc = 50.0  # cm
    wt0 = float(os.environ.get("VSF_BENCH_WT0", 60.0))  # initial water table, cm
    rch_frac = float(os.environ.get("VSF_BENCH_RCH", 0.05))  # peak rch / K
    hours = float(os.environ.get("VSF_BENCH_HOURS", 12.0))

    # spatially varying infiltration on the top layer (patches)
    x = (np.arange(ncol) + 0.5) / ncol
    y = (np.arange(nrow) + 0.5) / nrow
    xx, yy = np.meshgrid(x, y)
    pattern = 0.5 * (1.0 + np.sin(4 * np.pi * xx) * np.sin(4 * np.pi * yy))
    rch = rch_frac * HK * pattern

    # drain the saturated zone at the left boundary
    delz = height / nlay
    chd_data = []
    for k in range(nlay):
        if height - (k + 0.5) * delz < wt0:
            for i in range(nrow):
                chd_data.append([(k, i, 0), wt0])

    return build_sim(
        ws,
        exe,
        name,
        nlay,
        nrow,
        ncol,
        delr,
        delc,
        height,
        np.full((nlay, nrow, ncol), wt0),
        vsf_1d_only=vsf_1d_only,
        newton=newton,
        perlen=hours * 3600.0,
        dtmin=1.0,
        dt0=10.0,
        dtmax=900.0,
        rch_rate=rch,
        chd_data=chd_data,
    )


@pytest.mark.slow
@pytest.mark.parametrize("newton", [False, True], ids=["picard", "newton"])
def test_benchmark(function_tmpdir, targets, newton):
    results = {}
    heads = {}
    for case, one_d in [("full", False), ("1d", True)]:
        ws = function_tmpdir / case
        name = f"bench{case}"
        sim = build_bench(ws, targets["mf6"], name, one_d, newton)
        sim.write_simulation(silent=True)
        wall = run_sim(sim)
        stats = get_run_stats(ws)
        stats["wall"] = wall
        results[case] = stats
        heads[case] = get_heads(ws, name)

    # head differences, split by cells saturated in both runs and the rest
    nlay = heads["full"].shape[0]
    delz = 200.0 / nlay
    zc = (200.0 - (np.arange(nlay) + 0.5) * delz)[:, None, None]
    sat = (heads["full"] >= zc) & (heads["1d"] >= zc)
    dh = np.abs(heads["1d"] - heads["full"])
    results["n_cells"] = int(dh.size)
    results["n_saturated"] = int(sat.sum())
    results["max_head_diff_saturated"] = float(dh[sat].max()) if sat.any() else 0.0
    results["max_head_diff_unsaturated"] = (
        float(dh[~sat].max()) if (~sat).any() else 0.0
    )
    results["mean_head_diff"] = float(dh.mean())
    results["speedup"] = results["full"]["wall"] / results["1d"]["wall"]

    summary = json.dumps(results, indent=2)
    (function_tmpdir / "benchmark.json").write_text(summary)
    print(f"\nVSF_1D_ONLY benchmark ({'newton' if newton else 'picard'}):")
    print(summary)
