"""
Test for the residual-driven pseudo-time stepping (pseudo-transient
continuation) development option of the VSF package: DEV_PSEUDO_TIMESTEP.

The test uses the Celia (1990) infiltration column with a single, large time
step, a regime in which the standard Picard iteration converges only slowly.
It verifies that, with the option enabled:

  1. the converged solution is unchanged (the pseudo-storage term vanishes at
     convergence), and
  2. the number of outer iterations is reduced.

The option is a development feature that flopy does not know about, so it is
written into the VSF OPTIONS block by post-processing the generated input file.
"""

import os
import re
import subprocess

import flopy
import numpy as np
from gwf_test_utils import get_balance_error, get_vsf_soil_data


def build_model(ws, exe, eps_pseu=None, dt=900.0, nouter=1000):
    column_height = 42.0
    nlay, nrow, ncol = 42, 1, 1
    perlen = dt
    nstp = 1
    delr = delc = 10.0
    delz = column_height / nlay
    top = delz
    laytyp = 0
    botm = [top - (ilay + 1) * delz for ilay in range(nlay)]
    hk = 0.00944  # cm/s

    hp_upper = -20.7
    hp_lower = -61.5
    h_upper = hp_upper + botm[0] + 0.5 * delz
    h_lower = hp_lower + botm[-1] + 0.5 * delz
    strt = np.zeros((nlay, nrow, ncol))
    strt[:, 0, 0] = [botm[i] + 0.5 * delz + hp_lower for i in range(nlay)]

    hclose = 1e-6

    name = "celia"
    sim = flopy.mf6.MFSimulation(sim_name=name, version="mf6", exe_name=exe, sim_ws=ws)
    flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=1, perioddata=[(perlen, nstp, 1.0)]
    )
    gwfname = "gwf_" + name
    gwf = flopy.mf6.MFModel(
        sim, model_type="gwf6", modelname=gwfname, model_nam_file=f"{gwfname}.nam"
    )
    gwf.name_file.save_flows = True
    flopy.mf6.ModflowIms(
        sim,
        print_option="ALL",
        outer_dvclose=hclose,
        outer_maximum=nouter,
        under_relaxation="NONE",
        inner_maximum=300,
        inner_dvclose=hclose,
        rcloserecord=1e-6,
        linear_acceleration="CG",
        relaxation_factor=1.0,
        filename=f"{gwfname}.ims",
    )
    flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=np.ones((nlay, nrow, ncol), dtype=int),
    )
    flopy.mf6.ModflowGwfic(gwf, strt=strt)
    flopy.mf6.ModflowGwfnpf(gwf, icelltype=laytyp, k=hk, k33=hk)
    flopy.mf6.ModflowGwfsto(gwf, ss=0.0001, sy=0.3, iconvert=0, transient={0: True})
    soil_data = get_vsf_soil_data("Celia1990-eq10-Haverkamp")
    flopy.mf6.ModflowGwfvsf(
        gwf,
        iunsat=1,
        storage_scheme="modified-picard",
        kr_averaging="arithmetic",
        soil_model="Haverkamp",
        porosity=soil_data["porosity"],
        satres=soil_data["satres"],
        alphahvk=soil_data["alpha"],
        nhvk=soil_data["n"],
        betahvk=soil_data["beta"],
        khvk=soil_data["k"],
    )
    c = {0: [[(0, 0, 0), h_upper], [(nlay - 1, 0, 0), h_lower]]}
    flopy.mf6.ModflowGwfchd(gwf, maxbound=2, stress_period_data=c, pname="CHD-1")
    flopy.mf6.ModflowGwfoc(
        gwf,
        head_filerecord=f"{gwfname}.hds",
        saverecord=[("HEAD", "LAST")],
    )
    sim.write_simulation(silent=True)

    if eps_pseu is not None:
        _inject_pseudo_timestep(os.path.join(ws, f"{gwfname}.vsf"), eps_pseu)

    return gwfname


def _inject_pseudo_timestep(vsf_file, eps_pseu):
    """Add DEV_PSEUDO_TIMESTEP to the VSF OPTIONS block.

    flopy does not know this development option, so it is written directly
    into the input file just before END OPTIONS.
    """
    with open(vsf_file) as f:
        text = f.read()
    text, n = re.subn(
        r"(\n\s*END OPTIONS)",
        f"\n  DEV_PSEUDO_TIMESTEP {eps_pseu}\\1",
        text,
        count=1,
        flags=re.IGNORECASE,
    )
    assert n == 1, "could not locate END OPTIONS in the VSF input file"
    with open(vsf_file, "w") as f:
        f.write(text)


def _count_outer_iterations(lst_file):
    """Total outer iterations from the IMS INNER ITERATION SUMMARY.

    Each outer iteration starts with an inner iteration numbered 1.
    """
    total = 0
    with open(lst_file) as f:
        for line in f:
            parts = line.split()
            if len(parts) >= 3:
                try:
                    tot, outer, inner = (int(parts[0]), int(parts[1]), int(parts[2]))
                except ValueError:
                    continue
                if tot >= 1 and outer >= 1 and inner == 1:
                    total += 1
    return total


def _run(ws, exe):
    result = subprocess.run([str(exe)], cwd=ws, capture_output=True, text=True)
    assert "Normal termination of simulation" in result.stdout, (
        f"mf6 did not terminate normally in {ws}:\n{result.stdout}"
    )


def test_pseudo_timestep(function_tmpdir, targets):
    exe = targets["mf6"]
    gwfname = "gwf_celia"

    # baseline: standard Picard, no pseudo-time stepping
    ws_off = function_tmpdir / "off"
    build_model(ws_off, exe)
    _run(ws_off, exe)

    # with residual-driven pseudo-time stepping enabled
    ws_on = function_tmpdir / "on"
    build_model(ws_on, exe, eps_pseu=0.1)
    _run(ws_on, exe)

    # both runs must be mass conservative
    err_off = get_balance_error(ws_off / f"{gwfname}.lst")
    err_on = get_balance_error(ws_on / f"{gwfname}.lst")
    assert abs(err_off) < 1e-5, f"baseline balance error {err_off} too large"
    assert abs(err_on) < 1e-5, f"pseudo balance error {err_on} too large"

    # the converged solution must be identical (the pseudo-storage term
    # vanishes at convergence and does not change the answer)
    h_off = flopy.utils.HeadFile(ws_off / f"{gwfname}.hds").get_data(idx=-1).flatten()
    h_on = flopy.utils.HeadFile(ws_on / f"{gwfname}.hds").get_data(idx=-1).flatten()
    assert np.allclose(h_off, h_on, atol=1e-4), (
        f"max head difference {np.max(np.abs(h_off - h_on))} too large"
    )

    # the pseudo-time step must improve convergence for this hard time step
    n_off = _count_outer_iterations(ws_off / f"{gwfname}.lst")
    n_on = _count_outer_iterations(ws_on / f"{gwfname}.lst")
    assert n_on < n_off, (
        f"pseudo-time stepping did not reduce outer iterations (off={n_off}, on={n_on})"
    )
