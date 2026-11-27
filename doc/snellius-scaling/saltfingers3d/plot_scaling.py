"""Scaling figures for the saltfingers3D benchmark on Snellius (rome partition).

Reads the CSV files in ./data (extracted from the MODFLOW 6 profiler output and
memory summaries of the rank-0 / per-rank listing files), fits the cost of the
one-time edge-count loop in GwfNpfType%calc_max_conns, writes the corrected
scaling table to ./data/derived_scaling.csv and the figures to ./figures.

Run from anywhere:  pixi run python doc/snellius-scaling/saltfingers3d/plot_scaling.py
"""

import csv
import statistics as st
from collections import defaultdict
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import FixedLocator, NullLocator

HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
FIGS = HERE / "figures"

# machine / model facts (see README.md)
RANKS_PER_NODE = 8
NCPL_TOTAL = 230_400  # cells per layer, undivided model
NUMA_DOMAIN_GB = 32.0  # rome node: NPS4, 8 domains x 32 GB
NODE_GB = 256.0
L2_KB = 512.0  # per core, AMD EPYC 7H12
L3_KB = 16 * 1024.0  # per 4-core CCX

# reference palette (dataviz skill, light mode), fixed order
C_MEAS = "#257de0"  # slot 1, blue
C_CORR = "#f47230"  # slot 2, orange
STACK = ["#257de0", "#f47230", "#00b894", "#f2a600", "#ee7fac", "#008a17"]
INK = "#191919"
INK2 = "#666666"
GRID = "#e6e6e6"

mpl.rcParams.update(
    {
        "font.size": 9,
        "axes.edgecolor": INK2,
        "axes.labelcolor": INK,
        "axes.titlesize": 10,
        "axes.titleweight": "bold",
        "axes.titlelocation": "left",
        "axes.spines.top": False,
        "axes.spines.right": False,
        "axes.grid": True,
        "grid.color": GRID,
        "grid.linewidth": 0.6,
        "xtick.color": INK2,
        "ytick.color": INK2,
        "legend.frameon": False,
        "lines.linewidth": 2.0,
        "lines.markersize": 5,
        "savefig.dpi": 200,
        "savefig.bbox": "tight",
    }
)


def read_csv(name):
    with open(DATA / name, newline="") as f:
        return list(csv.DictReader(f))


def log2_axis(ax, nprocs, rotate=False):
    ax.set_xscale("log", base=2)
    ax.xaxis.set_major_locator(FixedLocator(nprocs))
    ax.xaxis.set_minor_locator(NullLocator())
    ax.set_xticklabels([str(n) for n in nprocs], rotation=45 if rotate else 0)
    ax.set_xlabel("MPI processes (8 per node)")


def shade_single_node(ax, label=True, y=0.03, va="bottom"):
    ax.axvspan(0.75, 11, color="#f2f2f2", zorder=0, lw=0)
    if label:
        ax.text(
            2.9,
            y,
            "one node",
            transform=ax.get_xaxis_transform(),
            ha="center",
            va=va,
            color=INK2,
            fontsize=8,
        )


def save(fig, name):
    FIGS.mkdir(exist_ok=True)
    for ext in ("png", "pdf"):
        fig.savefig(FIGS / f"{name}.{ext}")
    plt.close(fig)


# ---------------------------------------------------------------- load data
timings = read_csv("timings_rank0.csv")
iters = read_csv("iterations.csv")
budget = read_csv("budget_per_rank.csv")
waits = {
    (r["nproc"], r["rank"]): float(r["wait_s"])
    for r in read_csv("mpi_wait_per_rank.csv")
}
loopmax = {
    int(r["nproc"]): float(r["max_cells_x_edges"])
    for r in read_csv("edge_loop_max_rank.csv")
}
memory = read_csv("memory_per_rank.csv")

by_n = defaultdict(list)
for r in timings:
    by_n[int(r["nproc"])].append(r)
NPROCS = sorted(by_n)


def mean(n, key):
    return st.mean(float(r[key]) for r in by_n[n])


run = {n: [float(r["run_s"]) for r in by_n[n]] for n in NPROCS}
run_mean = {n: st.mean(v) for n, v in run.items()}

# ------------------------------------------- fit the calc_max_conns edge loop
# budget time per rank = a * cells + c * cells * edges   (all 20 time steps)
cells = np.array([int(r["ncpl"]) * int(r["nlay"]) for r in budget], float)
edges = np.array([float(r["nedges"]) for r in budget])
bud = np.array([float(r["budget_s"]) for r in budget])
X = np.c_[cells, cells * edges]
(a_fit, c_fit), *_ = np.linalg.lstsq(X, bud, rcond=None)
bud_pred = X @ np.array([a_fit, c_fit])
r2_budget = 1 - ((bud - bud_pred) ** 2).sum() / ((bud - bud.mean()) ** 2).sum()

# independent check: other ranks wait for the slowest rank's loop
loop_rank = {
    (r["nproc"], r["rank"]): c_fit * int(r["ncpl"]) * int(r["nlay"]) * int(r["nedges"])
    for r in budget
}
wait_pred, wait_meas = [], []
for n in sorted({k[0] for k in loop_rank}, key=int):
    ks = [k for k in loop_rank if k[0] == n]
    mx = max(loop_rank[k] for k in ks)
    for k in ks:
        wait_pred.append(mx - loop_rank[k])
        wait_meas.append(waits[k])
wait_pred, wait_meas = np.array(wait_pred), np.array(wait_meas)
wslope, wicpt = np.polyfit(wait_pred, wait_meas, 1)
wcorr = np.corrcoef(wait_pred, wait_meas)[0, 1]

# memory per rank = m_cell * cells + m_edge * edges + const   (ranks 2-64p)
mem_rank = np.array([float(r["mem_gb"]) for r in budget])
Xm = np.c_[cells, edges, np.ones_like(edges)]
(m_cell, m_edge, m_const), *_ = np.linalg.lstsq(Xm, mem_rank, rcond=None)
r2_mem = (
    1
    - ((mem_rank - Xm @ np.array([m_cell, m_edge, m_const])) ** 2).sum()
    / ((mem_rank - mem_rank.mean()) ** 2).sum()
)

loop = {n: c_fit * loopmax[n] for n in NPROCS}  # wall-time cost (slowest rank)
corr_mean = {n: run_mean[n] - loop[n] for n in NPROCS}

t1, t8 = run_mean[1], run_mean[8]
t1c, t8c = corr_mean[1], corr_mean[8]


def nodes(n):
    return max(1, n // RANKS_PER_NODE)


eff1_m = {n: t1 / run_mean[n] / n for n in NPROCS}
eff1_c = {n: t1c / corr_mean[n] / n for n in NPROCS}
effn_m = {n: t8 / run_mean[n] / nodes(n) for n in NPROCS if n >= 8}
effn_c = {n: t8c / corr_mean[n] / nodes(n) for n in NPROCS if n >= 8}

with open(DATA / "derived_scaling.csv", "w", newline="") as f:
    w = csv.writer(f, lineterminator="\n")
    w.writerow(
        [
            "nproc",
            "nodes",
            "n_runs",
            "run_mean_s",
            "run_min_s",
            "run_max_s",
            "edge_loop_s",
            "run_corrected_s",
            "eff_vs_1p",
            "eff_vs_1p_corrected",
            "eff_per_node_vs_8p",
            "eff_per_node_vs_8p_corrected",
        ]
    )
    for n in NPROCS:
        w.writerow(
            [
                n,
                nodes(n),
                len(run[n]),
                f"{run_mean[n]:.1f}",
                f"{min(run[n]):.1f}",
                f"{max(run[n]):.1f}",
                f"{loop[n]:.1f}",
                f"{corr_mean[n]:.1f}",
                f"{eff1_m[n]:.3f}",
                f"{eff1_c[n]:.3f}",
                f"{effn_m[n]:.3f}" if n >= 8 else "",
                f"{effn_c[n]:.3f}" if n >= 8 else "",
            ]
        )

# ------------------------------------------------------- fig 1: wall time
fig, ax = plt.subplots(figsize=(6.2, 4.2))
shade_single_node(ax)
x = np.array(NPROCS, float)
ideal = t8c * 8 / x
ax.plot(x, ideal, ls=(0, (4, 3)), lw=1.2, color=INK2, zorder=1)
ax.text(
    300,
    t8c * 8 / 300 * 0.55,
    "ideal, from corrected 8p",
    color=INK2,
    fontsize=8,
    ha="right",
    va="top",
)
ym = np.array([run_mean[n] for n in NPROCS])
lo = ym - np.array([min(run[n]) for n in NPROCS])
hi = np.array([max(run[n]) for n in NPROCS]) - ym
ax.errorbar(
    x,
    ym,
    yerr=[lo, hi],
    color=C_MEAS,
    marker="o",
    capsize=2,
    lw=2,
    label="measured (mean, min-max of sets)",
    zorder=3,
)
ax.plot(
    x,
    [corr_mean[n] for n in NPROCS],
    color=C_CORR,
    marker="s",
    lw=2,
    label="corrected (edge loop removed)",
    zorder=4,
)
ax.set_yscale("log")
ax.set_ylabel("wall-clock time (s)")
log2_axis(ax, NPROCS)
ax.set_title("Strong scaling: wall-clock time")
ax.legend(loc="upper right")
ax.annotate(
    f"{run_mean[1] / 3600:.1f} h",
    (1, run_mean[1]),
    textcoords="offset points",
    xytext=(9, 3),
    color=INK,
    fontsize=8,
)
ax.annotate(
    f"{run_mean[1024]:.0f} s",
    (1024, run_mean[1024]),
    textcoords="offset points",
    xytext=(-6, -15),
    ha="center",
    color=INK,
    fontsize=8,
)
ax.annotate(
    f"{run_mean[2048]:.0f} s (1 run)",
    (2048, run_mean[2048]),
    textcoords="offset points",
    xytext=(-38, 8),
    color=INK,
    fontsize=8,
)
save(fig, "fig1_wall_time")

# ------------------------------------------------------ fig 2: efficiency
fig, (axa, axb) = plt.subplots(1, 2, figsize=(10.0, 4.0), sharey=True)
shade_single_node(axa, y=0.97, va="top")
axa.axhline(1.0, color=INK2, lw=1, ls=(0, (4, 3)))
axa.plot(x, [eff1_m[n] for n in NPROCS], color=C_MEAS, marker="o", label="measured")
axa.plot(x, [eff1_c[n] for n in NPROCS], color=C_CORR, marker="s", label="corrected")
axa.set_title("(a) Efficiency relative to 1 process")
axa.set_ylabel("parallel efficiency")
log2_axis(axa, NPROCS, rotate=True)
axa.legend(loc="lower left")

xn = [n for n in NPROCS if n >= 8]
axb.axhline(1.0, color=INK2, lw=1, ls=(0, (4, 3)))
axb.plot(xn, [effn_m[n] for n in xn], color=C_MEAS, marker="o", label="measured")
axb.plot(xn, [effn_c[n] for n in xn], color=C_CORR, marker="s", label="corrected")
axb.set_title("(b) Efficiency per node relative to 1 node (8p)")
log2_axis(axb, xn, rotate=True)
axb.set_xlabel("MPI processes (nodes = processes / 8)")
for ax_ in (axa, axb):
    ax_.yaxis.set_major_formatter(mpl.ticker.PercentFormatter(1.0, decimals=0))
    ax_.set_ylim(0, 1.3)
save(fig, "fig2_efficiency")

# ------------------------------- fig 3: flow linear solve per inner iteration
inner = {int(r["nproc"]): int(r["inner1"]) for r in iters if r["set"] == "run_1"}
t_it = {n: mean(n, "lin1_s") / inner[n] for n in NPROCS}
eff_it = {n: t_it[1] / t_it[n] / n for n in NPROCS}
reuse_kb = {n: 2 * (NCPL_TOTAL / n) * 8 / 1024 for n in NPROCS}

fig, (axa, axb) = plt.subplots(
    2, 1, figsize=(6.2, 5.6), sharex=True, gridspec_kw={"height_ratios": [3, 2]}
)
shade_single_node(axa)
axa.axhline(1.0, color=INK2, lw=1, ls=(0, (4, 3)))
axa.plot(x, [eff_it[n] for n in NPROCS], color=C_MEAS, marker="o")
axa.yaxis.set_major_formatter(mpl.ticker.PercentFormatter(1.0, decimals=0))
axa.set_ylim(0.8, 1.35)
axa.set_ylabel("efficiency per\ninner iteration")
axa.set_title("(a) Flow linear solve: time per inner iteration vs 1 process")
for n, off in ((1, (0, 8)), (16, (0, 8)), (2048, (-6, -15))):
    lab = f"{t_it[n]:.2f} s" if t_it[n] >= 1 else f"{1000 * t_it[n]:.3g} ms"
    axa.annotate(
        lab + " / it",
        (n, eff_it[n]),
        textcoords="offset points",
        xytext=off,
        ha="center",
        fontsize=8,
        color=INK,
    )

shade_single_node(axb, label=False)
axb.plot(x, [reuse_kb[n] for n in NPROCS], color=C_MEAS, marker="o")
for val, lab in ((L2_KB, "L2 per core (512 KB)"), (L3_KB, "L3 per CCX (16 MB)")):
    axb.axhline(val, color=INK2, lw=1, ls=(0, (2, 2)))
    axb.text(1500, val * 1.15, lab, color=INK2, fontsize=8, ha="right", va="bottom")
axb.set_yscale("log")
axb.set_ylabel("2 x cells per layer\nx 8 B (KB)")
axb.set_title("(b) Reuse distance of vertical neighbours in memory")
log2_axis(axb, NPROCS)
save(fig, "fig3_per_iteration")

# -------------------------------------------- fig 4: time breakdown (rank 0)
comp_names = [
    "flow linear solve",
    "flow formulate",
    "transport solve",
    "initialize",
    "edge loop (calc_max_conns)",
    "other",
]
shares = []
for n in NPROCS:
    total = run_mean[n]
    parts = [
        mean(n, "lin1_s"),
        mean(n, "form1_s"),
        mean(n, "solve2_s"),
        mean(n, "init_s"),
        loop[n],
    ]
    parts.append(total - sum(parts))
    shares.append([p / total for p in parts])
shares = np.array(shares)

fig, ax = plt.subplots(figsize=(7.6, 4.0))
pos = np.arange(len(NPROCS))
bottom = np.zeros(len(NPROCS))
for i, name in enumerate(comp_names):
    ax.bar(
        pos,
        shares[:, i],
        bottom=bottom,
        width=0.72,
        color=STACK[i],
        edgecolor="white",
        linewidth=1.0,
        label=name,
    )
    bottom += shares[:, i]
ax.set_xticks(pos, [str(n) for n in NPROCS])
ax.set_xlabel("MPI processes (8 per node)")
ax.set_ylabel("share of wall-clock time (rank 0)")
ax.yaxis.set_major_formatter(mpl.ticker.PercentFormatter(1.0, decimals=0))
ax.set_ylim(0, 1)
ax.grid(axis="x", visible=False)
ax.set_title("Where the time goes (mean of sets, rank-0 profiler)")
ax.legend(loc="upper left", bbox_to_anchor=(1.01, 1.0))
save(fig, "fig4_breakdown")

# --------------------------------------- fig 5: validation of the correction
fig, (axa, axb) = plt.subplots(1, 2, figsize=(9.0, 3.9))
lim = [min(bud.min(), bud_pred.min()) * 0.8, max(bud.max(), bud_pred.max()) * 1.2]
axa.plot(lim, lim, color=INK2, lw=1, ls=(0, (4, 3)))
axa.scatter(
    bud_pred, bud, s=16, color=C_MEAS, edgecolor="white", linewidth=0.6, zorder=3
)
axa.set_xscale("log")
axa.set_yscale("log")
axa.set_xlim(lim)
axa.set_ylim(lim)
axa.set_xlabel("model: a·cells + c·cells·edges (s)")
axa.set_ylabel("measured budget time per rank (s)")
axa.set_title("(a) Budget time, all ranks 2-64p")
axa.text(
    0.04,
    0.96,
    f"c = {c_fit * 1e9:.2f} ns per cell x edge\n"
    f"$R^2$ = {r2_budget:.4f}, n = {len(bud)}",
    transform=axa.transAxes,
    va="top",
    fontsize=8,
    color=INK,
)

lim = [0, max(wait_pred.max(), wait_meas.max()) * 1.05]
axb.plot(lim, lim, color=INK2, lw=1, ls=(0, (4, 3)))
axb.scatter(
    wait_pred, wait_meas, s=16, color=C_MEAS, edgecolor="white", linewidth=0.6, zorder=3
)
axb.set_xlim(lim)
axb.set_ylim(lim)
axb.set_xlabel("predicted wait: slowest rank's loop - own loop (s)")
axb.set_ylabel("measured MPI wait after budgets (s)")
axb.set_title("(b) Independent check: MPI wait per rank")
axb.text(
    0.04,
    0.96,
    f"fit: wait = {wslope:.2f} x predicted + {wicpt:.0f} s\nr = {wcorr:.3f}",
    transform=axb.transAxes,
    va="top",
    fontsize=8,
    color=INK,
)
save(fig, "fig5_correction_validation")

# --------------------------------------------------- fig 6: memory per rank
mem = defaultdict(list)
for r in memory:
    mem[int(r["nproc"])].append(float(r["mem_gb"]))
fig, (axa, axb) = plt.subplots(1, 2, figsize=(10.0, 4.0))
shade_single_node(axa, y=0.97, va="top")
mx_ = np.array([max(mem[n]) for n in NPROCS])
mn_ = np.array([min(mem[n]) for n in NPROCS])
md_ = np.array([st.mean(mem[n]) for n in NPROCS])
axa.vlines(x, mn_, mx_, color=C_MEAS, lw=4, alpha=0.35)
axa.plot(x, md_, color=C_MEAS, marker="o", label="mean per rank (bar: min-max)")
axa.plot(
    x,
    [sum(mem[1]) / n for n in NPROCS],
    color=INK2,
    lw=1,
    ls=(0, (4, 3)),
    label="1p total / N",
)
axa.axhline(NUMA_DOMAIN_GB, color=INK2, lw=1.2)
axa.text(
    1500, NUMA_DOMAIN_GB * 1.12, "NUMA domain: 32 GB", color=INK, fontsize=8, ha="right"
)
axa.set_yscale("log")
axa.set_ylabel("MODFLOW memory per rank (GB)")
axa.set_title("(a) Memory per rank vs. NUMA domain")
log2_axis(axa, NPROCS, rotate=True)
axa.legend(loc="lower left")

axb.plot(x, [sum(mem[n]) for n in NPROCS], color=C_MEAS, marker="o")
axb.set_ylim(0, None)
axb.set_ylabel("MODFLOW memory, all ranks (GB)")
axb.set_title("(b) Total memory, all ranks")
log2_axis(axb, NPROCS, rotate=True)
save(fig, "fig6_memory")

print(
    f"edge-loop fit: a = {a_fit * 1e6:.2f} us/cell (20 steps), "
    f"c = {c_fit * 1e9:.3f} ns/(cell x edge), R2 = {r2_budget:.5f}"
)
print(
    f"memory fit: {m_cell * 1e6:.2f} KB/cell, {m_edge * 1e6:.1f} KB/edge, "
    f"const {m_const:.2f} GB, R2 = {r2_mem:.3f}"
)
print(f"wait check: slope {wslope:.3f}, intercept {wicpt:.1f} s, r = {wcorr:.4f}")
for n in NPROCS:
    print(
        f"{n:5d}  meas {run_mean[n]:9.1f}  loop {loop[n]:7.1f}  "
        f"corr {corr_mean[n]:9.1f}  "
        f"E1 {eff1_m[n]:.2f}/{eff1_c[n]:.2f}"
        + (f"  Enode {effn_m[n]:.2f}/{effn_c[n]:.2f}" if n >= 8 else "")
    )
