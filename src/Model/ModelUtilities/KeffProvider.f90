!> @brief Abstract provider of effective hydraulic conductivity quantities.
!!
!! Decouples boundary (and other) packages that need a face/segment effective
!! hydraulic conductivity from the concrete flow package that owns the
!! conductivity data (e.g. NPF). A concrete provider is injected by the model;
!! callers depend only on this interface.
!!
!! The provider exposes the effective-K quantities a boundary needs to build a
!! conductance, but not a conductance itself: eff_hy is the saturated,
!! anisotropy-resolved directional K (L/T); krel is the current relative
!! permeability scaling (1 when saturated); and dkrel_dh is its head
!! derivative (for Newton-Raphson). The current effective K along a direction
!! is krel * eff_hy. Assembling a conductance from these and the cell/face
!! geometry (area, distance) is the consumer's responsibility.
!<
module KeffProviderModule
  use KindModule, only: DP, I4B
  implicit none
  private
  public :: KeffProviderType

  type, abstract :: KeffProviderType
  contains
    procedure(eff_hy_if), deferred :: eff_hy
    procedure(krel_if), deferred :: krel
    procedure(dkrel_dh_if), deferred :: dkrel_dh
  end type KeffProviderType

  abstract interface
    !> @brief Saturated effective hydraulic conductivity of cell n resolved in
    !! the direction of the unit vector vg. The full anisotropy ellipsoid is
    !! resolved from vg alone, so no connection/orientation flag is required.
    !! This is the saturated reference K; multiply by krel for the current
    !< effective K under unsaturated conditions.
    function eff_hy_if(this, n, vg) result(hy)
      import :: KeffProviderType, DP, I4B
      class(KeffProviderType), intent(in) :: this
      integer(I4B), intent(in) :: n !< reduced node number
      real(DP), dimension(3), intent(in) :: vg !< unit direction vector
      real(DP) :: hy
    end function eff_hy_if

    !> @brief Current relative permeability of cell n. Returns 1 for a fully
    !! saturated cell; less than 1 where an unsaturated flow formulation (e.g.
    !< UZR) is active. eff_hy returns the saturated K, so callers multiply by this.
    function krel_if(this, n) result(kr)
      import :: KeffProviderType, DP, I4B
      class(KeffProviderType), intent(in) :: this
      integer(I4B), intent(in) :: n !< reduced node number
      real(DP) :: kr
    end function krel_if

    !> @brief Derivative of the relative permeability of cell n with respect to
    !! head. Returns 0 for saturated cells; nonzero where an unsaturated flow
    !< formulation supplies it. Used to build Newton-Raphson boundary terms.
    function dkrel_dh_if(this, n) result(dkrdh)
      import :: KeffProviderType, DP, I4B
      class(KeffProviderType), intent(in) :: this
      integer(I4B), intent(in) :: n !< reduced node number
      real(DP) :: dkrdh
    end function dkrel_dh_if
  end interface

end module KeffProviderModule
