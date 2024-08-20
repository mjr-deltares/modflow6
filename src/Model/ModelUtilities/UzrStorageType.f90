module UzrStorageModule 
  use KindModule, only: I4B, LGP, DP
  use ConstantsModule, only: DONE, DTWO, DHALF, DZERO, DPREC
  use MatrixBaseModule, only: MatrixBaseType
  use BaseDisModule, only: DisBaseType
  use GwfStoModule, only: GwfStoType
  use GwfStoExtModule, only: GwfStoExtType
  implicit none
  private

  public :: UzrStorageType

  type, extends(GwfStoExtType) :: UzrStorageType
    integer(I4B), pointer, dimension(:), contiguous :: uzr_iunsat => null()
    class(DisBaseType), pointer :: gwf_dis => null()
    type(GwfStoType), pointer :: gwf_sto => null()
  contains
    procedure :: initialize
    procedure :: is_active => uft_is_active
    procedure :: fc => uft_fc
    procedure :: fn => uft_fn
    procedure :: destroy
  end type UzrStorageType

contains

  subroutine initialize(this, iunsat, dis, sto)
    class(UzrStorageType), intent(inout) :: this
    integer(I4B), pointer, dimension(:), contiguous, intent(in) :: iunsat
    class(DisBaseType), pointer :: dis
    type(GwfStoType), pointer, intent(in) :: sto

    this%uzr_iunsat => iunsat

    this%gwf_dis => dis
    this%gwf_sto => sto

  end subroutine initialize

  function uft_is_active(this, n) result(is_active)
    class(UzrStorageType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    logical(LGP) :: is_active

    is_active = .false.
    if (this%uzr_iunsat(n) == 1) then
      is_active = .true.
    end if

  end function uft_is_active

  subroutine uft_fc(this, n, matrix_sln, rhs, idxglo, h_old, h_new)
    use GwfStorageUtilsModule, only: SsCapacity, SyCapacity
    use TdisModule, only: delt
    class(UzrStorageType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    class(MatrixBaseType), pointer, intent(inout) :: matrix_sln
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(in) :: h_old
    real(DP), dimension(:), intent(in) :: h_new
    ! local
    real(DP) :: sc1 !< specific storage capacity
    real(DP) :: sc2 !< specific yield capacity
    real(DP) :: top !< the top of the cell
    real(DP) :: bot !< the bottom of the cell
    real(DP) :: area !< area of the cell
    real(DP) :: thk !< cell thickness
    real(DP) :: z_n !< the nodal elevation for n
    real(DP) :: s_new !< the current saturation
    real(DP) :: s_old !< the saturation at the previous time level
    real(DP) :: dsdh !< the capacity function as the derivative of s(h)
    real(DP) :: aterm !< the matrix coefficient
    real(DP) :: rhsterm !< the term for the RHS
    integer(I4B) :: idiag !< the position of the diagonal element

    idiag = this%gwf_dis%con%ia(n)

    top = this%gwf_dis%top(n)
    bot = this%gwf_dis%bot(n)
    area = this%gwf_dis%area(n)
    z_n = this%gwf_dis%bot(n) + &
          DHALF * (this%gwf_dis%top(n) - this%gwf_dis%bot(n))

    sc1 = SsCapacity(this%gwf_sto%istor_coef, top, bot, area, this%gwf_sto%ss(n))
    s_new = eff_saturation(h_new(n), z_n)

    ! specific storage
    aterm = -sc1 * s_new / delt
    rhsterm = -sc1 * s_new * h_old(n) / delt
    call matrix_sln%add_value_pos(idxglo(idiag), aterm)
    rhs(n) = rhs(n) + rhsterm

    ! unsaturated storage (modified Picard)
    sc2 = SyCapacity(this%gwf_dis%area(n), this%gwf_sto%sy(n))
    thk = top - bot

    s_old = eff_saturation(h_old(n), z_n)
      
    ! part 1: change in s
    rhsterm = sc2 * thk * (s_new - s_old) / delt
    rhs(n) = rhs(n) + rhsterm

    ! part 2: change in h
    dsdh = eff_saturation_derivative(h_new(n), z_n)
    aterm = -sc2 * thk * dsdh / delt
    rhsterm = -sc2 * thk * dsdh * h_new(n) / delt
    call matrix_sln%add_value_pos(idxglo(idiag), aterm)
    rhs(n) = rhs(n) + rhsterm

  end subroutine uft_fc

  subroutine uft_fn(this, n)
    class(UzrStorageType), intent(inout) :: this
    integer(I4B), intent(in) :: n
  end subroutine uft_fn

  subroutine destroy(this)
    class(UzrStorageType) :: this
  end subroutine destroy

  function eff_saturation(head, elevation) result(s_eff)
    real(DP), intent(in) :: head
    real(DP), intent(in) :: elevation
    real(DP) :: s_eff
    ! local
    real(DP) :: hp !< pressure head
    real(DP) :: alpha, beta
    real(DP) :: theta_s, theta_r, theta

    alpha = 1.611e+06
    beta = 3.96
    theta_s = 0.287
    theta_r = 0.075

    hp = head - elevation

    if (hp > DZERO) then
      s_eff = DONE
    else
      theta = alpha * (theta_s - theta_r) / (alpha + abs(hp) ** beta) + theta_r
      s_eff = theta / theta_s ! scale from water content to saturation
    end if

  end function eff_saturation

  function eff_saturation_derivative(head, elevation) result(ds_dh)
    real(DP), intent(in) :: head
    real(DP), intent(in) :: elevation
    real(DP) :: ds_dh
    ! local
    real(DP) :: hp !< pressure head
    real(DP) :: alpha, beta
    real(DP) :: theta_s, theta_r
    real(DP) :: num, denom

    alpha = 1.611e+06
    beta = 3.96
    theta_s = 0.287
    theta_r = 0.075

    hp = head - elevation
    if (hp > DZERO) then
      ds_dh = DZERO
    else
      num = alpha * beta * (theta_s - theta_r) * (abs(hp)**(beta - 1))
      denom = (alpha + abs(hp)**beta)**2
      ds_dh = (num / denom) / theta_s
    end if

  end function eff_saturation_derivative


end module UzrStorageModule