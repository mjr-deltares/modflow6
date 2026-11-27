"""Scaling figures for the CONUS *_disv_ptcfix sweep on Snellius (rome partition).

Reads the CSV files in ./data (extracted from the MODFLOW 6 profiler output, the
memory summaries in the per-rank listing files, the HPC partition files and the
model input), plus the 32p rerun with ngwm_043 and ngwm_056 swapped between ranks
(swap32_*.csv) and the outer-iteration histories (outer_iterations.csv);
writes ./data/derived_scaling.csv and the figures to ./figures.

Run from anywhere:  pixi run python doc/snellius-scaling/conus/plot_scaling.py
"""

import csv
from collections import defaultdict
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import FixedLocator, NullLocator, PercentFormatter

HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
FIGS = HERE / "figures"

BASE_N = 16  # baseline: 2 nodes at 8 ranks per node
NUMA_DOMAIN_GB = 32.0

# reference palette (dataviz skill, light mode), fixed order
C1 = "#257de0"  # slot 1, blue
C2 = "#f47230"  # slot 2, orange
STACK = ["#257de0", "#f47230", "#00b894", "#f2a600"]
INK = "#191919"
INK2 = "#666666"
GRID = "#e6e6e6"
SWAP_LABEL = "32p, 043/056 swapped"

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
    ax.set_xlabel("MPI processes")


def shade_4ppn(ax, label=True, y=0.97, va="top"):
    ax.axvspan(2.8, 11.3, color="#f2f2f2", zorder=0, lw=0)
    if label:
        ax.text(
            5.6,
            y,
            "4 ranks\nper node",
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
runs = {int(r["nproc"]): r for r in read_csv("runs.csv")}
NPROCS = sorted(runs)
N8 = [n for n in NPROCS if int(runs[n]["ranks_per_node"]) == 8]
ranks = defaultdict(dict)
for r in read_csv("ranks.csv"):
    ranks[int(r["nproc"])][int(r["rank"])] = {k: float(v) for k, v in r.items()}
cells = {r["model"]: int(r["ncpl"]) * int(r["nlay"]) for r in read_csv("models.csv")}
exchanges = read_csv("exchanges.csv")
part = defaultdict(dict)
for r in read_csv("partitions.csv"):
    part[int(r["nproc"])][r["model"]] = int(r["rank"])


def r0(n, key):
    return ranks[n][0][key]


run_s = {n: r0(n, "run_s") for n in NPROCS}
outer = {n: int(runs[n]["outer"]) for n in NPROCS}
inner = {n: int(runs[n]["inner"]) for n in NPROCS}
nodes = {n: int(runs[n]["nodes"]) for n in NPROCS}
t_it = {n: r0(n, "linsolve_s") / inner[n] for n in NPROCS}  # s per inner iteration

# the 32p rerun with ngwm_043 on the same rank as ngwm_049
swap_run = read_csv("swap32_run.csv")[0]
swap_ranks = {
    int(r["rank"]): {k: float(v) for k, v in r.items()}
    for r in read_csv("swap32_ranks.csv")
}
swap_part = {r["model"]: int(r["rank"]) for r in read_csv("swap32_partition.csv")}
SW = 32
sw_run = swap_ranks[0]["run_s"]
sw_outer, sw_inner = int(swap_run["outer"]), int(swap_run["inner"])
sw_t_it = swap_ranks[0]["linsolve_s"] / sw_inner
sw_eff_wall = run_s[BASE_N] / sw_run / (SW / BASE_N)
sw_eff_it = t_it[BASE_N] / sw_t_it / (SW / BASE_N)

eff_wall = {n: run_s[BASE_N] / run_s[n] / (n / BASE_N) for n in N8}
eff_it = {n: t_it[BASE_N] / t_it[n] / (n / BASE_N) for n in N8}

# cross-rank exchange edges and memory per rank
rows = []
cross_tot = {}
for n in NPROCS:
    p = part[n]
    cross = defaultdict(int)
    tot = 0
    for x in exchanges:
        a, b, ne = x["model1"], x["model2"], int(x["nexg"])
        if p[a] != p[b]:
            cross[p[a]] += ne
            cross[p[b]] += ne
            tot += ne
    cross_tot[n] = tot
    rc = defaultdict(int)
    for m, r in p.items():
        rc[r] += cells[m]
    for r, v in ranks[n].items():
        rows.append((rc[r], cross[r], v["mem_gb"]))
A = np.array(rows, float)
Xm = np.c_[A[:, 0], A[:, 1], np.ones(len(A))]
(m_cell, m_edge, m_const), *_ = np.linalg.lstsq(Xm, A[:, 2], rcond=None)
r2_mem = (
    1
    - ((A[:, 2] - Xm @ [m_cell, m_edge, m_const]) ** 2).sum()
    / ((A[:, 2] - A[:, 2].mean()) ** 2).sum()
)
mem_tot = {n: sum(v["mem_gb"] for v in ranks[n].values()) for n in NPROCS}
sw_cross = sum(
    int(x["nexg"])
    for x in exchanges
    if swap_part[x["model1"]] != swap_part[x["model2"]]
)
sw_mem = sum(v["mem_gb"] for v in swap_ranks.values())

with open(DATA / "derived_scaling.csv", "w", newline="") as f:
    w = csv.writer(f, lineterminator="\n")
    w.writerow(
        [
            "nproc",
            "nodes",
            "ranks_per_node",
            "run_s",
            "outer",
            "inner",
            "linsolve_s_per_inner",
            "eff_per_node_vs_16p",
            "eff_per_inner_vs_16p",
            "cross_rank_edges",
            "mem_total_gb",
        ]
    )
    for n in NPROCS:
        w.writerow(
            [
                n,
                nodes[n],
                runs[n]["ranks_per_node"],
                f"{run_s[n]:.1f}",
                outer[n],
                inner[n],
                f"{t_it[n]:.4f}",
                f"{eff_wall[n]:.3f}" if n in eff_wall else "",
                f"{eff_it[n]:.3f}" if n in eff_it else "",
                cross_tot[n],
                f"{mem_tot[n]:.1f}",
            ]
        )
    w.writerow(
        [
            "32_swap",
            4,
            8,
            f"{sw_run:.1f}",
            sw_outer,
            sw_inner,
            f"{sw_t_it:.4f}",
            f"{sw_eff_wall:.3f}",
            f"{sw_eff_it:.3f}",
            sw_cross,
            f"{sw_mem:.1f}",
        ]
    )

x = np.array(NPROCS, float)
x8 = np.array(N8, float)

# ------------------------------------------------------- fig 1: wall time
fig, ax = plt.subplots(figsize=(6.2, 4.2))
shade_4ppn(ax, y=0.03, va="bottom")
ax.plot(x, run_s[BASE_N] * BASE_N / x, ls=(0, (4, 3)), lw=1.2, color=INK2, zorder=1)
ax.text(
    45,
    run_s[BASE_N] * BASE_N / 45 * 0.62,
    "ideal from 16",
    color=INK2,
    fontsize=8,
    ha="right",
)
ax.plot(
    x8, [run_s[n] for n in N8], color=C1, marker="o", zorder=3, label="8 ranks per node"
)
n4 = [n for n in NPROCS if n not in N8]
ax.plot(
    n4,
    [run_s[n] for n in n4],
    ls="none",
    marker="o",
    mfc="white",
    mec=C1,
    mew=1.5,
    zorder=3,
    label="4 ranks per node",
)
ax.plot(
    [SW],
    [sw_run],
    ls="none",
    marker="D",
    ms=6,
    color=C2,
    zorder=4,
    label=SWAP_LABEL,
)
for n in (4, 16, 256):
    lab = f"{run_s[n] / 3600:.1f} h" if run_s[n] > 3600 else f"{run_s[n]:.0f} s"
    ax.annotate(
        lab,
        (n, run_s[n]),
        textcoords="offset points",
        xytext=(6, 5),
        fontsize=8,
        color=INK,
    )
ax.set_yscale("log")
ax.set_ylabel("wall-clock time (s)")
ax.set_title("CONUS strong scaling: wall-clock time")
log2_axis(ax, NPROCS)
ax.legend(loc="upper right")
save(fig, "fig1_wall_time")

# ------------------------------------------------------ fig 2: efficiency
fig, ax = plt.subplots(figsize=(6.2, 4.0))
ax.axhline(1.0, color=INK2, lw=1, ls=(0, (4, 3)))
ax.plot(
    x8,
    [eff_it[n] for n in N8],
    color=C2,
    marker="s",
    label="linear solve per inner iteration",
)
ax.plot(x8, [eff_wall[n] for n in N8], color=C1, marker="o", label="wall-clock")
ax.plot([SW], [sw_eff_it], ls="none", marker="D", ms=6, mfc="white", mec=INK, mew=1.5)
ax.plot(
    [SW],
    [sw_eff_wall],
    ls="none",
    marker="D",
    ms=6,
    color=INK,
    label=SWAP_LABEL + " (open: per inner iteration)",
)
ax.yaxis.set_major_formatter(PercentFormatter(1.0, decimals=0))
ax.set_ylim(0, 1.2)
ax.set_ylabel("efficiency per node")
ax.set_title("Efficiency per node relative to 16 processes (2 nodes)")
log2_axis(ax, N8)
ax.set_xlabel("MPI processes (8 per node; nodes = processes / 8)")
ax.legend(loc="lower left")
save(fig, "fig2_efficiency")

# ----------------------------------------------------- fig 3: iterations
fig, (axa, axb) = plt.subplots(1, 2, figsize=(10.0, 3.8))
for ax_, val, title, lab in (
    (axa, outer, "(a) Outer (PTC/Newton) iterations", "outer iterations"),
    (axb, inner, "(b) Inner (BiCGSTAB) iterations", "inner iterations"),
):
    shade_4ppn(ax_)
    ax_.plot(x, [val[n] for n in NPROCS], color=C1, marker="o")
    sv = sw_outer if val is outer else sw_inner
    ax_.plot(
        [SW], [sv], ls="none", marker="D", ms=6, color=C2, zorder=4, label=SWAP_LABEL
    )
    ax_.annotate(
        f"{sv:,}",
        (SW, sv),
        textcoords="offset points",
        xytext=(0, -14),
        ha="center",
        fontsize=8,
        color=C2,
    )
    ax_.legend(loc="lower right", fontsize=8)
    ax_.set_ylim(0, max(val.values()) * 1.15)
    ax_.set_ylabel(lab)
    ax_.set_title(title)
    log2_axis(ax_, NPROCS)
    for n in (NPROCS[0], NPROCS[-1]):
        ax_.annotate(
            f"{val[n]:,}",
            (n, val[n]),
            textcoords="offset points",
            xytext=(0, 7),
            ha="center",
            fontsize=8,
            color=INK,
        )
save(fig, "fig3_iterations")

# -------------------------------------------- fig 4: time breakdown (rank 0)
names = ["linear solve", "formulate", "initialize", "other"]
shares = []
for n in NPROCS:
    parts = [r0(n, "linsolve_s"), r0(n, "formulate_s"), r0(n, "init_s")]
    parts.append(run_s[n] - sum(parts))
    shares.append([p_ / run_s[n] for p_ in parts])
shares = np.array(shares)
fig, ax = plt.subplots(figsize=(7.0, 3.8))
pos = np.arange(len(NPROCS))
bottom = np.zeros(len(NPROCS))
for i, name in enumerate(names):
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
ax.set_xlabel("MPI processes")
ax.set_ylabel("share of wall-clock time (rank 0)")
ax.yaxis.set_major_formatter(PercentFormatter(1.0, decimals=0))
ax.set_ylim(0, 1)
ax.grid(axis="x", visible=False)
ax.set_title("Where the time goes (rank-0 profiler)")
ax.legend(loc="upper left", bbox_to_anchor=(1.01, 1.0))
save(fig, "fig4_breakdown")

# --------------------------------------------------------- fig 5: memory
fig, (axa, axb) = plt.subplots(1, 2, figsize=(10.0, 3.9))
shade_4ppn(axa, y=0.45, va="center")
mn = np.array([min(v["mem_gb"] for v in ranks[n].values()) for n in NPROCS])
mx = np.array([max(v["mem_gb"] for v in ranks[n].values()) for n in NPROCS])
md = np.array([np.mean([v["mem_gb"] for v in ranks[n].values()]) for n in NPROCS])
axa.vlines(x, mn, mx, color=C1, lw=4, alpha=0.35)
axa.plot(x, md, color=C1, marker="o", label="mean per rank (bar: min-max)")
axa.plot(
    x, mem_tot[NPROCS[0]] / x, color=INK2, lw=1, ls=(0, (4, 3)), label="4p total / N"
)
axa.axhline(NUMA_DOMAIN_GB, color=INK2, lw=1.2)
axa.text(
    250, NUMA_DOMAIN_GB * 1.12, "NUMA domain: 32 GB", color=INK, fontsize=8, ha="right"
)
axa.set_yscale("log")
axa.set_ylabel("MODFLOW memory per rank (GB)")
axa.set_title("(a) Memory per rank")
log2_axis(axa, NPROCS)
axa.legend(loc="lower left")

ce = np.array([cross_tot[n] for n in NPROCS], float)
mt = np.array([mem_tot[n] for n in NPROCS])
pred = (
    m_cell * sum(cells.values()) + m_edge * 2 * ce + m_const * x
)  # each edge counts on 2 ranks
axb.plot(
    ce / 1e3, pred, color=INK2, lw=1, ls=(0, (4, 3)), label="fit, summed over ranks"
)
axb.legend(loc="lower right")
axb.scatter(ce / 1e3, mt, s=28, color=C1, edgecolor="white", linewidth=0.8, zorder=3)
axb.scatter(
    [sw_cross / 1e3], [sw_mem], s=34, marker="D", color=C2, edgecolor="white", zorder=4
)
for n, xv, yv in zip(NPROCS, ce, mt):
    axb.annotate(
        f"{n}p",
        (xv / 1e3, yv),
        textcoords="offset points",
        xytext=(6, -10),
        fontsize=8,
        color=INK,
    )
axb.set_xlabel("exchange edges between ranks (thousands)")
axb.set_ylabel("MODFLOW memory, all ranks (GB)")
axb.set_title("(b) Total memory vs. cross-rank edges")
axb.text(
    0.04,
    0.96,
    f"per rank: {m_cell * 1e6:.2f} KB/cell + "
    f"{m_edge * 1e6:.0f} KB per cross-rank edge\n"
    f"$R^2$ = {r2_mem:.4f} over {len(A)} ranks",
    transform=axb.transAxes,
    va="top",
    fontsize=8,
    color=INK,
)
save(fig, "fig5_memory")

print(
    f"memory fit: {m_cell * 1e6:.2f} KB/cell + {m_edge * 1e6:.1f} KB/cross-rank edge "
    f"+ {m_const:.3f} GB/rank, "
    f"R2 = {r2_mem:.4f}"
)
for n in NPROCS:
    print(
        f"{n:4d} nodes {nodes[n]:3d} run {run_s[n]:9.1f} "
        f"outer {outer[n]:4d} inner {inner[n]:6d} "
        f"s/it {t_it[n]:.4f} "
        + (f"Ewall {eff_wall[n]:.2f} Eit {eff_it[n]:.2f}" if n in N8 else "")
    )

# ------------------------------------- fig 6: drain conductance distribution
# The cell that limits convergence at 32p: ngwm_049, cell 344000, on the
# 043-049 exchange boundary (cross-rank in all runs).
REF_MODEL, REF_CELL, REF_COND = "ngwm_049", 344000, 49570.6172
hist = defaultdict(dict)
for r in read_csv("drain_conductance_hist.csv"):
    hist[r["group"]][float(r["bin_log10_lo"])] = int(r["count"])
summ = {r["group"]: r for r in read_csv("drain_conductance_summary.csv")}
groups = (
    ("all", "all 256 models", C1),
    ("m049", REF_MODEL, C2),
)
fig, ax = plt.subplots(figsize=(7.0, 3.8))
for g, lab, col in groups:
    bins = sorted(hist[g])
    tot = sum(hist[g].values())
    # exceedance: share of drain cells with conductance >= lower edge of each bin
    xs, ys, rest = [], [], tot
    for b in bins:
        xs.append(10**b)
        ys.append(rest / tot)
        rest -= hist[g][b]
    above = int(summ[g]["n_ge_ref"]) / int(summ[g]["n"])
    ax.step(xs, ys, where="post", color=col, lw=1.8, label=lab)
    ax.plot([REF_COND], [above], marker="o", color=col, ms=6, zorder=4)
    ax.annotate(
        f"{100 * above:.1f}%",
        (REF_COND, above),
        textcoords="offset points",
        xytext=(8, 4 if g == "m049" else -12),
        fontsize=8,
        color=INK,
    )
ax.axvline(REF_COND, color=INK, lw=1.0, ls=(0, (4, 3)))
ax.annotate(
    f"{REF_MODEL} cell {REF_CELL}: {REF_COND:.3g}",
    (REF_COND, 0.98),
    xycoords=("data", "axes fraction"),
    xytext=(6, 0),
    textcoords="offset points",
    va="top",
    fontsize=8,
    color=INK,
)
ax.set_xscale("log")
ax.set_ylim(0, 1.02)
ax.yaxis.set_major_formatter(PercentFormatter(1.0, decimals=0))
ax.set_xlabel("drain conductance")
ax.set_ylabel("share of drain cells at or above")
ax.set_title("Drain conductances and the cell that limits convergence at 32p")
ax.legend(loc="lower left", fontsize=8)
save(fig, "fig6_drain_conductance")

# ------------------------- fig 7: outer-iteration histories and the 32p swap
OUTER_DVCLOSE = 1e-4
hist_o = defaultdict(list)
for r in read_csv("outer_iterations.csv"):
    hist_o[r["run"]].append(
        (int(r["outer"]), int(r["inner"]), float(r["max_abs_change"]))
    )
styles = (
    ("16p", "16p", "#b3b3b3", (0, (1, 1.5))),
    ("64p", "64p", "#999999", (0, (4, 2))),
    ("128p", "128p", "#bfbfbf", (0, (6, 2, 1, 2))),
    ("32p", "32p, original partition", C1, "-"),
    ("32p_swap", SWAP_LABEL, C2, "-"),
)
fig, (axa, axb) = plt.subplots(1, 2, figsize=(10.0, 3.9))
for key, lab, col, ls in styles:
    h = np.array(hist_o[key])
    lw = 2.0 if key.startswith("32p") else 1.4
    axa.plot(h[:, 0], np.cumsum(h[:, 1]), color=col, ls=ls, lw=lw, label=lab)
    axb.plot(h[:, 0], h[:, 2], color=col, ls=ls, lw=lw if lw > 1.5 else 1.0, label=lab)
    axa.annotate(
        f"{int(h[:, 1].sum()):,}",
        (h[-1, 0], h[:, 1].sum()),
        textcoords="offset points",
        xytext=(4, 0),
        va="center",
        fontsize=7.5,
        color=col if key.startswith("32p") else INK2,
    )
axa.axhline(5000, color=INK2, lw=0.8)
axa.text(
    110,
    4700,
    "first 100 outer iterations: all capped at 50",
    fontsize=7.5,
    color=INK2,
    va="top",
)
axa.set_xlabel("outer iteration")
axa.set_ylabel("cumulative inner iterations")
axa.set_title("(a) Cumulative inner iterations")
axa.set_xlim(0, 450)
axa.legend(loc="lower right", fontsize=8)
axb.axhline(OUTER_DVCLOSE, color=INK, lw=1.0, ls=(0, (4, 3)))
axb.text(5, OUTER_DVCLOSE * 1.6, "OUTER_DVCLOSE", fontsize=7.5, color=INK)
axb.set_yscale("log")
axb.set_xlim(0, 450)
axb.set_xlabel("outer iteration")
axb.set_ylabel("largest |head change|, all ranks")
axb.set_title("(b) Nonlinear convergence")
save(fig, "fig7_swap_test")

print(
    f"32p swap: run {sw_run:.1f} outer {sw_outer} inner {sw_inner} s/it {sw_t_it:.4f} "
    f"Ewall {sw_eff_wall:.2f} Eit {sw_eff_it:.2f} cross {sw_cross} mem {sw_mem:.1f}"
)
