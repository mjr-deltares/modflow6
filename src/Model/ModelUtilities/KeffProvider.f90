!> @brief Abstract provider of directional effective hydraulic conductivity.
!!
!! Decouples boundary (and other) packages that need a face/segment effective
!! hydraulic conductivity from the concrete flow package that owns the
!! conductivity data (e.g. NPF). A concrete provider is injected by the model;
!! callers depend only on this interface.
!!
!! The provider returns a hydraulic conductivity (L/T), not a conductance
!! (L^2/T). Assembling a conductance from the returned K and the cell/face
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
  end type KeffProviderType

  abstract interface
    !> @brief Effective hydraulic conductivity of cell n resolved in the
    !! direction of the unit vector vg. The full anisotropy ellipsoid is
    !! resolved from vg alone, so no connection/orientation flag is required.
    !<
    function eff_hy_if(this, n, vg) result(hy)
      import :: KeffProviderType, DP, I4B
      class(KeffProviderType), intent(in) :: this
      integer(I4B), intent(in) :: n !< reduced node number
      real(DP), dimension(3), intent(in) :: vg !< unit direction vector
      real(DP) :: hy
    end function eff_hy_if
  end interface

end module KeffProviderModule
