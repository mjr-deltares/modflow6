# MPB (Multi-Point Boundary) Package — Design & Implementation Plan

## 1. Goal

Add a new GWF boundary package, **MPB** (Multi-Point Boundary), in which the
dependent variable of a boundary cell (head for GWF; later concentration /
temperature for GWT/GWE) is prescribed as a **linear combination of the
dependent variables of an arbitrary set of other (non-boundary) cells**:

```
h_b = c_i + Σ_j ( w_ij * h_{cell_j} )
```

where, for boundary `i`:
- `h_b`      is the dependent variable in the target boundary cell,
- `c_i`      is a constant offset,
- `w_ij`     is the weight applied to source cell `j`,
- `h_cell_j` is the dependent variable in source cell `j`.

The package is modeled on **CHD** ([`gwf-chd.f90`](src/Model/GroundWaterFlow/gwf-chd.f90)),
except the prescribed value is not constant — it depends on the current solution.

## 2. Agreed decisions (this iteration)

1. **One package, with a switch** between two assembly strategies
   (default = explicit/lagged). See §4.
2. **First scope = GWF only** (head). GWT/GWE (concentration/temperature)
   deferred to a later iteration.
3. **Input shape = normalized, unlimited term list** (no configurable
   `MAXTERMS` cap). See §3.
4. **Static coupling structure.** Source cells and weights are defined once
   (`CONNECTIONDATA`); only the constant offset `c_i` and status vary per
   `PERIOD`. See §3.2.
5. **Source cells may be boundary cells (including other MPB cells).** The
   mechanism should mimic an interior-domain stencil calculation *at the
   boundary*, so an MPB cell must be allowed to depend on a boundary cell as
   well. Consequences: potentially slower explicit convergence, and possible
   ill-definedness when the references are effectively circular — which we
   **trap on** (detect and error out). See §4.4 and §7.

## 3. Input design

### 3.1 Why normalized (long-format) instead of a MAXTERMS cap

MODFLOW 6 already solves "a feature coupled to an arbitrary number of cells"
with a normalized list, e.g. MAW
([`gwf-maw.dfn`](doc/mf6io/mf6ivar/dfn/gwf-maw.dfn)):
`PACKAGEDATA` has one row per feature (with `ngwfnodes`), and `CONNECTIONDATA`
has one row per `(feature, connection)` pair keyed by the feature number.
The per-feature count is effectively unlimited and simply discovered by reading
the flat list. This fits IDM's fixed-width-row reader (variable-length period
rows are poorly supported) and delivers the desired dynamic/unlimited behavior
with no hard cap.

### 3.2 Proposed blocks (GWF)

- **OPTIONS**
  - `BOUNDNAMES`, `PRINT_INPUT`, `PRINT_FLOWS`, `SAVE_FLOWS`, `AUXILIARY`,
    `OBS6` (standard boundary options).
  - `COUPLING {EXPLICIT | IMPLICIT}` — selects assembly strategy
    (default `EXPLICIT`).
- **DIMENSIONS**
  - `NMPB` — number of MPB boundaries (target cells).
  - (Total number of terms is discovered from the term list; a `ngwfnodes`-style
    per-boundary count may be carried in `PACKAGEDATA` for allocation, following
    the MAW pattern.)
- **PACKAGEDATA** (static): one row per boundary
  - `ifno` (boundary number), target `cellid`, `nterm` (number of terms),
    optional `boundname`.
- **CONNECTIONDATA** (static): one row per term
  - `ifno`, `iterm`, source `cellid`, `weight`.
- **PERIOD** (optional, time-varying): one row per boundary
  - `ifno`, `c` (constant offset), `status`, `aux`.

**Decision:** the *coupling structure* (source cells + weights) is **static**
(`CONNECTIONDATA`, defined once), while the *constant offset* `c_i` and status
are **time-varying** (`PERIOD`). This mirrors MAW (static connections) + CHD
(per-period scalars). Per-period weights are out of scope.

## 4. The two assembly strategies (the switch)

Both are feasible with existing infrastructure; they share ~80% of the code
(input, cell/term lists, weights, residual-based budget) and differ mainly in
`*_ad` / `*_fc` (and, for implicit, `*_ac` / `*_mc` / `iasym`).

### 4.1 EXPLICIT (lagged, Picard) — default

Behaves like CHD, but the prescribed value is recomputed each outer (nonlinear)
iteration from the latest heads:

```
value(i) = c_i + Σ_j w_ij * h_{cell_j}^{(k)}
```

- Mark the target cell Dirichlet (`ibound < 0`), as CHD does in
  [`chd_rp`](src/Model/GroundWaterFlow/gwf-chd.f90).
- Write `xnew(node) = value(i)` in a modified `*_ad` (as
  [`chd_ad`](src/Model/GroundWaterFlow/gwf-chd.f90) does).
- `*_fc` is a no-op; the solver enforces the identity row for Dirichlet cells in
  [`NumericalSolution.f90`](src/Solution/NumericalSolution.f90) (sets diag = 1,
  zeroes off-diagonals, `rhs = x`).
- Budget flow recovered as the row residual, like
  [`calc_chd_rate`](src/Model/GroundWaterFlow/gwf-chd.f90).

**Pros**
- Minimal, low risk; reuses the entire CHD path.
- Keeps the matrix symmetric (CG-friendly); no sparsity fill-in.

**Cons / ramifications**
- Turns an otherwise-linear problem into an **iterated** one (needs multiple
  outer iterations where CHD needs one).
- Convergence depends on the coupling "gain" (`Σ w_ij`); strong feedback or
  loops can be slow or divergent and may need under-relaxation.
- **Convergence-criteria pitfall:** because `x(node)` is overwritten each outer
  iteration, the lag must be reflected in the outer-convergence (DVCLOSE) check,
  or the solver can declare convergence with a stale coupling. Needs explicit
  handling/verification.

### 4.2 IMPLICIT (assembled into the matrix)

Keep the target cell **active** (`ibound > 0`) and replace its governing row with
the constraint `h_b − Σ_j w_ij h_{cell_j} = c_i`:

- `*_ac`: add sparsity entries `(b, cell_j)` for each term — same mechanism MAW
  uses in [`maw_ac`](src/Model/GroundWaterFlow/gwf-maw.f90) via
  `sparse%addconnection`.
- `*_mc`: cache global matrix positions (cf.
  [`maw_mc`](src/Model/GroundWaterFlow/gwf-maw.f90)).
- `*_fc`: zero the target cell's row **and** `rhs(b)` (remove NPF/STO
  contributions), set diagonal = 1, set each off-diagonal to `−w_ij`, set
  `rhs(b) = c_i`.
- Set `this%iasym = 1`; the solution auto-switches to the asymmetric solver via
  [`gwf_get_iasym`](src/Model/GroundWaterFlow/gwf.f90), which already polls
  `packobj%iasym`.
- Budget flow recovered as the row residual.

**Pros**
- Exact, **single-outer-iteration** resolution for linear problems; robust
  regardless of weight magnitudes; no Picard lag.
- Correct transient response.

**Cons / ramifications**
- **Breaks matrix symmetry** → forces IMS into BiCGSTAB with nonsymmetric
  storage: more memory and generally slower per iteration for the whole
  solution, not just MPB cells.
- **Fill-in / bandwidth growth:** connections to arbitrary distant cells add
  nonzeros far from the diagonal, degrading the ILU preconditioner (more linear
  iterations).
- **Row-overwrite ordering is delicate:** MPB `*_fc` must run after NPF/STO have
  filled row `b`, and must reliably zero both the row entries and `rhs(b)`.
  Needs a matrix "zero row" primitive; main correctness risk.
- Parallel/PETSc path and the symmetric-Dirichlet fixups in the solver must be
  verified for the new asymmetric coupling.

### 4.3 Comparison

| Aspect | EXPLICIT (lagged) | IMPLICIT (in matrix) |
|---|---|---|
| Solver symmetry | Preserved (CG) | Broken → BiCGSTAB |
| Sparsity / fill-in | None | Off-diagonal fill to arbitrary cells |
| Convergence | Needs outer iterations; weight-dependent | Single iteration (linear); robust |
| Implementation risk | Low (reuse CHD) | Higher (row overwrite, ordering, asym, parallel) |
| Accuracy (transient / strong coupling) | Lag error, possible divergence | Exact |
| Global performance cost | Localized; extra outer iters | Whole-solution asymmetric + preconditioner hit |

**Rollout:** implement EXPLICIT first (low-risk baseline), then IMPLICIT behind
the same `COUPLING` switch.

### 4.4 Source cells that are boundary / MPB cells (stencil at the boundary)

To mimic how a stencil is evaluated in the interior active domain, an MPB target
cell must be able to reference **boundary cells**, including **other MPB cells**.
This generalizes §7's deferred "loops" question into an explicit requirement.

Behavior per strategy:

- **EXPLICIT:** source values are read from the latest solution vector
  (`xnew`), which already contains the up-to-date value of any referenced
  boundary/MPB cell for that outer iteration. No special assembly is needed, but
  chains of MPB-on-MPB references lengthen the effective dependency path and can
  **slow outer-iteration convergence** (each link adds one iteration of lag).
  Document and, if needed, apply under-relaxation.
- **IMPLICIT:** referencing another active cell's row is just another
  off-diagonal `−w_ij` entry, which is well-posed **as long as the overall
  constraint system is non-singular**. A referenced cell that is itself a
  Dirichlet MPB (`ibound < 0`) contributes via the solver's existing
  Dirichlet-to-RHS handling.

**Circular-reference trap (both strategies).** Chained references can form a
cycle (A depends on B, B depends on A, directly or transitively). A pure cycle
with unit gains is ill-defined (singular / no unique solution). We **detect and
error out**:

- Build a directed dependency graph over MPB target cells (edge `i → j` when
  boundary `i` has a term referencing MPB cell `j`).
- During package setup, run a cycle check (DFS / topological sort). If a cycle
  is found, emit a clear error naming the involved boundaries and stop (via
  `store_error` / `store_error_filename`, as CHD does).
- **Policy: reject any cycle among MPB cells.** This is a deliberate,
  conservative choice (not just an initial simplification). Permitting
  *contractive* cycles (well-posed cycles — see §4.5) would only be considered
  if a compelling use case arises; none has so far.

### 4.5 Contractive vs ill-defined cycles (clarification)

A "cycle" means the MPB definitions reference each other so that a value
ultimately depends on itself (A depends on B, B depends on A — directly or
through a longer chain). Whether such a cycle is solvable depends on the
**product of the weights around the loop (the loop gain)**.

Treat the MPB constraints as a linear system. For a 2-cell loop:

```
h_A = c_A + w_AB * h_B
h_B = c_B + w_BA * h_A
```

Substituting one into the other:

```
h_A = c_A + w_AB * (c_B + w_BA * h_A)
    = c_A + w_AB*c_B + (w_AB * w_BA) * h_A
```

So `(1 − w_AB*w_BA) * h_A = c_A + w_AB*c_B`. Let the **loop gain** be
`g = w_AB * w_BA`.

- **Contractive cycle (|g| < 1):** `1 − g ≠ 0`, so a unique solution exists:
  `h_A = (c_A + w_AB*c_B) / (1 − g)`. The fixed-point iteration also converges
  because each pass around the loop shrinks the error by the factor `g`. This is
  well-posed — these are the cycles we *could* choose to allow later.
  - *Example:* `w_AB = 0.5`, `w_BA = 0.5` ⇒ `g = 0.25`. With `c_A = c_B = 0`
    the only solution is `h_A = h_B = 0`; with nonzero offsets it still has one
    unique, finite solution, and explicit iteration converges (error ×0.25 per
    loop).

- **Non-contractive, |g| = 1 (e.g. the unit-gain case):** `w_AB = w_BA = 1`
  gives `g = 1`, so `1 − g = 0`. The equation becomes `0 = c_A + c_B`:
  either **no solution** (if `c_A + c_B ≠ 0`) or **infinitely many**
  (if `c_A + c_B = 0`, any `h_A` with `h_B = h_A + c_B` works). Singular /
  ill-defined — this is the case the trap must catch. Explicit iteration does
  not contract (error unchanged each loop), so it fails to converge too.

- **Divergent (|g| > 1):** e.g. `w_AB = w_BA = 2` ⇒ `g = 4`. A unique algebraic
  solution still exists (`1 − g = −3 ≠ 0`), but explicit fixed-point iteration
  **diverges** (error ×4 per loop). So "contractive" (|g| < 1), not merely
  "non-singular", is the condition that makes the EXPLICIT strategy usable.

For loops longer than two cells the same idea generalizes: form the MPB
weight sub-matrix `W` (entry `W_ij = w_ij` for each term, 0 otherwise); the
system `(I − W) h = c` is well-posed when `I − W` is non-singular, and the
EXPLICIT iteration converges when the **spectral radius** `ρ(W) < 1` (the
multi-cell generalization of `|g| < 1`). **This section is background only:** the
package rejects *every* cycle regardless of gain (§4.4). A contractive-cycle
exception (reject only `ρ(W) ≥ 1`) would be revisited solely if a compelling use
case emerges.

## 5. Files to add / modify (modeled on CHD and SPG)

Templates: [`gwf-chd.f90`](src/Model/GroundWaterFlow/gwf-chd.f90) (Dirichlet +
residual budget), [`gwf-spg.f90`](src/Model/GroundWaterFlow/gwf-spg.f90)
(minimal recent package), [`gwf-maw.f90`](src/Model/GroundWaterFlow/gwf-maw.f90)
(`ac`/`mc` off-diagonal coupling).

1. **Input definition** — `doc/mf6io/mf6ivar/dfn/gwf-mpb.dfn` (blocks in §3.2).
2. **Generate IDM** — run
   [`mf6ivar.py`](doc/mf6io/mf6ivar/mf6ivar.py) to produce `src/Idm/gwf-mpbidm.f90`
   and `.tex`/`.md` docs; add the `MPB` case to
   [`IdmGwfDfnSelector.f90`](src/Idm/selector/IdmGwfDfnSelector.f90).
3. **Package module** — `src/Model/GroundWaterFlow/gwf-mpb.f90`:
   `MpbType` extends `BndExtType`; `mpb_create`; strategy-specific
   `bnd_ad` / `bnd_fc` (+ `bnd_ac` / `bnd_mc` + `iasym` for IMPLICIT);
   residual-based `bnd_cq` / budget; a CSR-like offset map from boundary → its
   terms.
4. **Register in GWF** — add `'MPB6 '` to `GWF_MULTIPKG`
   ([`gwf.f90:128`](src/Model/GroundWaterFlow/gwf.f90)), add `case ('MPB6')` in
   `package_create` ([`gwf.f90`](src/Model/GroundWaterFlow/gwf.f90)), and add
   `'MPB6'` to the package-type convert list (~`gwf.f90:1587`).
5. **Build** — add the new source to [`src/meson.build`](src/meson.build).
6. **Tests & docs** — autotests under `autotest/` (verify EXPLICIT and IMPLICIT
   agree, and that a degenerate single-term `w=1, c` reproduces an equivalent CHD
   setup); input documentation.

## 6. Validation strategy

- **Equivalence:** single term with `w=1`, `c=const` ⇒ equals a CHD at the same
  value; compare heads and budgets.
- **Cross-strategy:** EXPLICIT vs IMPLICIT converge to the same solution on a
  linear steady-state case (within solver tolerance).
- **Coupling strength:** sweep weights to characterize EXPLICIT convergence
  behavior and document guidance / relaxation needs.
- **Budget:** MPB in/out flows close the global budget (residual method).
- **Transient:** confirm IMPLICIT has no one-iteration lag vs EXPLICIT.
- **Stencil / chained references:** a case where MPB cells reference other
  boundary / MPB cells (interior-stencil mimic) converges correctly; and a
  deliberately circular configuration is **trapped** with a clear error.
- **Inactive source cell:** referencing an inactive (`ibound = 0`) source cell
  is **rejected** with a clear validation error.

## 7. Resolved decisions and deferred items

1. **Loops / circular references** — **Decided:** MPB source cells *may* be
   boundary cells, including other MPB cells (to mimic an interior stencil at the
   boundary). **All circular references are rejected** with a clear error (§4.4)
   — a deliberate, conservative policy, not just an initial simplification.
   Permitting *contractive* cycles (weight sub-matrix spectral radius < 1 — see
   §4.5 for what this means and a worked example) would be considered **only if a
   compelling use case arises; none has so far.**
2. **Static vs per-period terms** — **Decided:** coupling structure is
   **static** — source cells *and* weights are defined once in
   `CONNECTIONDATA`; only the constant offset `c_i` and status are time-varying
   (`PERIOD`). Per-period weights are out of scope.
3. **Inactive source cells** — **Decided:** source cells may be boundary cells
   (including other MPB cells; ordinary boundaries keep `ibound > 0`, so
   referencing them is fine), but an **inactive** source cell (`ibound = 0`) is
   **rejected** with a validation error, since it has no meaningful dependent
   variable.
4. **GWT/GWE extension** — **Deferred.** Concentration/temperature analog is out
   of scope for now; revisit after the GWF package is implemented and decide then
   whether to share this module or mirror the CNC/CTP pattern.
