!> @brief NPF-backed implementation of KeffProviderType.
!!
!! Adapter that lets packages obtain a directional effective hydraulic
!! conductivity from NPF without depending on NPF internals. Because
!! GwfNpfType already extends NumericalPackageType (single inheritance),
!! the capability is exposed through this thin delegating adapter rather
!< than by making NPF extend the abstract provider directly.
module NpfKeffProviderModule
  use KindModule, only: DP, I4B
  use KeffProviderModule, only: KeffProviderType
  use GwfNpfModule, only: GwfNpfType
  implicit none
  private
  public :: NpfKeffProviderType
  public :: create_npf_keff_provider

  type, extends(KeffProviderType) :: NpfKeffProviderType
    type(GwfNpfType), pointer :: npf => null() !< flow package supplying the conductivity
  contains
    procedure :: eff_hy => npf_eff_hy
  end type NpfKeffProviderType

contains

  !> @brief Allocate an NPF-backed provider and return it as the abstract type.
  subroutine create_npf_keff_provider(provider, npf)
    class(KeffProviderType), pointer, intent(out) :: provider
    type(GwfNpfType), pointer, intent(in) :: npf
    ! local
    type(NpfKeffProviderType), pointer :: npf_provider

    allocate (npf_provider)
    npf_provider%npf => npf
    provider => npf_provider
  end subroutine create_npf_keff_provider

  function npf_eff_hy(this, n, vg) result(hy)
    class(NpfKeffProviderType), intent(in) :: this
    integer(I4B), intent(in) :: n
    real(DP), dimension(3), intent(in) :: vg
    real(DP) :: hy

    hy = this%npf%calc_eff_hy(n, vg)
  end function npf_eff_hy

end module NpfKeffProviderModule
