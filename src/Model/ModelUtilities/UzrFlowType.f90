module UzrFlowModule 
  use KindModule, only: I4B, LGP, DP
  use ConstantsModule, only: DONE, DTWO, DHALF, DZERO
  use MatrixBaseModule, only: MatrixBaseType
  use BaseDisModule, only: DisBaseType
  use GwfNpfModule, only: GwfNpfType
  use GwfNpfExtModule, only: GwfNpfExtType
  implicit none
  private

  public :: UzrFlowType

  type, extends(GwfNpfExtType) :: UzrFlowType
    integer(I4B), pointer, dimension(:), contiguous :: uzr_iunsat => null()
    class(DisBaseType), pointer :: gwf_dis => null()
    type(GwfNpfType), pointer :: gwf_npf => null()
  contains
    procedure :: initialize
    procedure :: is_active => uft_is_active
    procedure :: fc => uft_fc
    procedure :: fn => uft_fn
    procedure :: destroy
  end type UzrFlowType

contains

  subroutine initialize(this, iunsat, dis, npf)
    class(UzrFlowType), intent(inout) :: this
    integer(I4B), pointer, dimension(:), contiguous, intent(in) :: iunsat
    class(DisBaseType), pointer :: dis
    type(GwfNpfType), pointer, intent(in) :: npf

    this%uzr_iunsat => iunsat

    this%gwf_dis => dis
    this%gwf_npf => npf

  end subroutine initialize

  function uft_is_active(this, n, m) result(is_active)
    class(UzrFlowType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    logical(LGP) :: is_active

    is_active = .false.
    if (this%uzr_iunsat(n) == 1 .or. this%uzr_iunsat(m) == 1) then
      is_active = .true.
    end if

  end function uft_is_active

  subroutine uft_fc(this, n, m, ipos, matrix_sln, rhs, idxglo, hnew)
    class(UzrFlowType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    integer(I4B), intent(in) :: ipos
    class(MatrixBaseType), pointer, intent(inout) :: matrix_sln
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(in) :: hnew
    ! local
    real(DP) :: sat_cond !< conductance at full saturation
    real(DP) :: cond !< conductance at current saturation
    real(DP) :: z_n !< the nodal elevation for n
    real(DP) :: z_m !< the nodal elevation for m
    real(DP) :: kr_n !< rel. permeability for node n
    real(DP) :: kr_m !< rel. permeability for node m
    real(DP) :: kr_weighted !< weighted rel. permeability between nodes
    integer(I4B) :: idiag !<  diagonal position
    integer(I4B) :: isymcon !< position of reverse connection m-n

    sat_cond = this%gwf_npf%condsat(this%gwf_dis%con%jas(ipos))

    z_n = this%gwf_dis%bot(n) + &
           DHALF * (this%gwf_dis%top(n) - this%gwf_dis%bot(n))
    kr_n = rel_permeability(hnew(n), z_n)
    z_m = this%gwf_dis%bot(m) + &
           DHALF * (this%gwf_dis%top(m) - this%gwf_dis%bot(m))
    kr_m = rel_permeability(hnew(m), z_m)

    ! mean
    kr_weighted = 0.5 * (kr_n + kr_m)

    ! ! geometric mean
    ! kr_weighted = sqrt(kr_n * kr_m)

    ! ! upstream
    ! if (hnew(n) > hnew(m)) then
    !   kr_weighted = kr_n
    ! else
    !   kr_weighted = kr_m
    ! end if

    cond = kr_weighted * sat_cond

    ! Fill row n
    idiag = this%gwf_dis%con%ia(n)
    call matrix_sln%add_value_pos(idxglo(ipos), cond)
    call matrix_sln%add_value_pos(idxglo(idiag), -cond)

    ! Fill row m
    isymcon = this%gwf_dis%con%isym(ipos)
    idiag = this%gwf_dis%con%ia(m)
    call matrix_sln%add_value_pos(idxglo(isymcon), cond)
    call matrix_sln%add_value_pos(idxglo(idiag), -cond)

  end subroutine uft_fc

  subroutine uft_fn(this, n, m, ipos)
    class(UzrFlowType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    integer(I4B), intent(in) :: ipos
  end subroutine uft_fn

  subroutine destroy(this)
    class(UzrFlowType) :: this

    this%gwf_dis => null()
    this%gwf_npf => null()
    this%uzr_iunsat => null()

  end subroutine destroy

  function rel_permeability(head, elevation) result(k_r)
    real(DP), intent(in) :: head
    real(DP), intent(in) :: elevation
    real(DP) :: k_r
    ! local
    real(DP) :: hp !< pressure head
    real(DP) :: A, gamma

    hp = head - elevation

    A = 1.175e+06_DP
    gamma = 4.74

    if (hp > DZERO) then
      k_r = DONE
    else
      k_r = A / (A + abs(hp) ** gamma)
    end if

  end function rel_permeability

end module UzrFlowModule