# Keff Provider

An abstract interface that gives boundary (and other) packages the directional,
anisotropy-resolved **effective hydraulic conductivity** quantities they need to
build a conductance, without depending on NPF internals.

## Why

Packages such as the seepage-face boundary need the effective K of a cell in a
specific direction (a face normal), which can be anisotropic, plus — under an
unsaturated (Richards) formulation — the cell's relative permeability and its
head derivative. That logic and the conductivity data live in NPF. Reaching into
NPF memory from each consumer duplicates physics and couples packages to NPF
internals. This interface centralizes the capability and is injected by the
model.

Note the scope: the provider exposes effective-K *quantities*, not a conductance
(C, units L^2/T). The current effective K along a direction is `krel * eff_hy`;
assembling a conductance `C = krel * eff_hy * A / L` from these and the cell/face
geometry (area `A`, distance `L`) is the consumer's responsibility, because that
assembly varies by boundary type (drain vs seepage vs GHB) and formulation
(saturated thickness, Newton).

## Design

- `KeffProviderType` (abstract, `src/Model/ModelUtilities/KeffProvider.f90`)
  - deferred `eff_hy(n, vg) -> hy`: **saturated** effective K of reduced node
    `n` resolved along unit vector `vg`. The full conductivity ellipsoid is
    projected onto `vg`, so no connection/orientation flag is required
    (direction-only).
  - deferred `krel(n) -> kr`: current relative permeability (1 when saturated,
    < 1 where an unsaturated formulation is active).
  - deferred `dkrel_dh(n) -> dkrdh`: head derivative of `krel` (0 when
    saturated), for Newton-Raphson boundary terms.
- `GwfNpfType%calc_eff_hy(n, vg)` (`gwf-npf.f90`)
  - connection-agnostic entry point that projects the cell's conductivity
    ellipsoid (`k11/k22/k33` + `angle1/2/3`) onto `vg` via `hyeff`, using the
    `iavgkeff` averaging method. Equivalent to `hy_eff` for every case but
    expressed purely in terms of a direction.
  - `GwfNpfType%krel` / `%dkrdh` arrays hold the relative permeability and its
    head derivative. They default to 1 and 0 and stay inert until an
    unsaturated flow formulation (e.g. UZR) populates them, so saturated models
    are unaffected.
- `NpfKeffProviderType` (`src/Model/ModelUtilities/NpfKeffProvider.f90`)
  - concrete adapter holding a `GwfNpfType` pointer, delegating `eff_hy` to
    `calc_eff_hy` and `krel`/`dkrel_dh` to the NPF arrays. Needed because
    `GwfNpfType` already extends `NumericalPackageType` and Fortran is
    single-inheritance, so NPF cannot also extend the abstract provider directly.
- `GwfModelType%keff_provider` (`gwf.f90`)
  - created in `gwf_ar` (`create_npf_keff_provider`), deallocated in `gwf_da`.
- Injection hook on `BndType` (`BoundaryPackage.f90`)
  - `keff_provider` pointer + `set_keff_provider`; `gwf_ar` calls
    `packobj%set_keff_provider(this%keff_provider)` for every boundary package.
    The base implementation only stores the (possibly null) pointer, so there is
    no behavior change until a package actually consumes it.

```
consumer -> KeffProviderType%{eff_hy,krel,dkrel_dh} -> NpfKeffProvider -> GwfNpfType
```

## Scope of this branch

Infrastructure only: the provider is created, held by the model, and injected
into every boundary package, but not yet consumed, so there is **no behavior
change** (all existing tests pass, library + `mf6` build clean). `krel` is 1 and
`dkrel_dh` is 0 everywhere until an unsaturated formulation populates the NPF
arrays. The first real consumer (the seepage-face boundary, initially for
Richards flow) reads `this%keff_provider` and assembles its own conductance
`krel * eff_hy * A / L`, with `dkrel_dh` feeding the Newton terms.

## Testing

`autotest/TestKeffProvider.f90` (test-drive) covers abstract dispatch of all
three deferred procedures via a mock and the directional `hyeff` values. It is
registered but only built/run when the `test-drive` dependency is available.

## Future

Additional providers (e.g. XT3D-specialized, or non-NPF flow formulations) can
implement `KeffProviderType` and be injected the same way. BUY/VSC, which
currently pull NPF memory directly, could migrate later.
