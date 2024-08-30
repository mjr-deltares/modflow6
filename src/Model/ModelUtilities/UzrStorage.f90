module UzrStorageModule 
  use KindModule, only: I4B, LGP, DP
  use ConstantsModule, only: DONE, DTWO, DHALF, DZERO, DPREC, LENVARNAME
  use MatrixBaseModule, only: MatrixBaseType
  use BaseDisModule, only: DisBaseType
  use GwfStoModule, only: GwfStoType
  use GwfStoExtModule, only: GwfStoExtType
  use UzrSoilModelModule, only: SoilModelType
  implicit none
  private

  public :: UzrStorageType

  type, extends(GwfStoExtType) :: UzrStorageType
    class(SoilModelType), pointer :: soil_model => null()

    integer(I4B), pointer :: storage_scheme => null() !< see UZR option
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

  subroutine initialize(this, iunsat, scheme, soil_model, dis, sto)
    class(UzrStorageType), intent(inout) :: this
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: iunsat
    integer(I4B), pointer, intent(in) :: scheme
    class(SoilModelType), pointer, intent(in) :: soil_model
    class(DisBaseType), pointer, intent(in) :: dis
    type(GwfStoType), pointer, intent(in) :: sto

    this%uzr_iunsat => iunsat

    this%storage_scheme => scheme
    this%soil_model => soil_model

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
    use GwfStorageUtilsModule, only: SsCapacity
    class(UzrStorageType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    class(MatrixBaseType), pointer, intent(inout) :: matrix_sln
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(in) :: h_old
    real(DP), dimension(:), intent(in) :: h_new
    ! local
    integer(I4B) :: idiag !< the position of the diagonal element
    real(DP) :: sc1 !< specific storage capacity
    real(DP) :: top !< the top of the cell
    real(DP) :: bot !< the bottom of the cell
    real(DP) :: area !< area of the cell
    real(DP) :: thk !< cell thickness
    real(DP) :: z !< the nodal elevation for n
    real(DP) :: s1 !< the current saturation
    real(DP) :: s0 !< the saturation at the previous time level
    real(DP) :: Cm !< the current moisture capacity
    real(DP) :: dsdh_lim !< slope
    real(DP) :: h1 !< the current saturation
    real(DP) :: h0 !< the saturation at the previous time level
    real(DP) :: psi1 !< the current saturation
    real(DP) :: psi0 !< the saturation at the previous time level
    real(DP) :: phi !< the cell porosity
    real(DP) :: aterm !< the matrix coefficient
    real(DP) :: rhsterm !< the term for the RHS

    idiag = this%gwf_dis%con%ia(n)

    top = this%gwf_dis%top(n)
    bot = this%gwf_dis%bot(n)
    thk = top - bot
    area = this%gwf_dis%area(n)

    h1 = h_new(n)
    h0 = h_old(n)
    z = this%gwf_dis%bot(n) + DHALF * (top - bot)
    psi1 = h1 - z
    psi0 = h0 - z

    phi = this%soil_model%porosity(n)
    s1 = this%soil_model%saturation(psi1, n)
    s0 = this%soil_model%saturation(psi0, n)
    Cm = this%soil_model%capacity(psi1, n)
    dsdh_lim = Cm / phi
    

    sc1 = SsCapacity(this%gwf_sto%istor_coef, top, bot, area, this%gwf_sto%ss(n))

    ! specific storage
    call get_specific_storage_terms(s1, h0, sc1, aterm, rhsterm)
    call matrix_sln%add_value_pos(idxglo(idiag), aterm)
    rhs(n) = rhs(n) + rhsterm

    select case(this%storage_scheme)
    case (0, 1)
      ! chord slope
      call get_unsat_storage_terms_CS(s1, s0, h1, h0, dsdh_lim, &
                                      phi, z, area, thk, &
                                      aterm, rhsterm)
    case (2)
      ! modified picard
      call get_unsat_storage_terms_MP(s1, s0, h1, h0, Cm, phi, z, thk, &
                                      aterm, rhsterm)
    end select

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

  subroutine get_specific_storage_terms(s_new, h_old, sc1, aterm, rhsterm)
    use TdisModule, only: delt
    real(DP), intent(in) :: s_new
    real(DP), intent(in) :: h_old
    real(DP), intent(in) :: sc1
    real(DP), intent(inout) :: aterm
    real(DP), intent(inout) :: rhsterm

    ! specific storage
    aterm = -sc1 * s_new / delt
    rhsterm = aterm * h_old

  end subroutine get_specific_storage_terms

  subroutine get_unsat_storage_terms_CS(s_new, s_old, h_new, h_old, dsdh_lim, phi, &
                                        z, area, thk, aterm, rhsterm)
    use TdisModule, only: delt
    real(DP), intent(in) :: s_new
    real(DP), intent(in) :: s_old
    real(DP), intent(in) :: h_new
    real(DP), intent(in) :: h_old
    real(DP), intent(in) :: dsdh_lim
    real(DP), intent(in) :: phi
    real(DP), intent(in) :: z
    real(DP), intent(in) :: area
    real(DP), intent(in) :: thk
    real(DP), intent(inout) :: aterm
    real(DP), intent(inout) :: rhsterm
    ! local
    real(DP) :: slope

    ! chord sloping
    if (abs(h_new - h_old) > DPREC) then
      slope = (s_new - s_old) / (h_new - h_old)
    else
      slope = dsdh_lim
    end if

    aterm = -phi * area * thk * slope / delt
    rhsterm = aterm * h_old

  end subroutine get_unsat_storage_terms_CS

  subroutine get_unsat_storage_terms_MP(s_new, s_old, h_new, h_old, Cm, phi, &
                                        z_n, thk, aterm, rhsterm)
    use TdisModule, only: delt
    real(DP), intent(in) :: s_new
    real(DP), intent(in) :: s_old
    real(DP), intent(in) :: h_new
    real(DP), intent(in) :: h_old
    real(DP), intent(in) :: Cm
    real(DP), intent(in) :: phi
    real(DP), intent(in) :: z_n
    real(DP), intent(in) :: thk
    real(DP), intent(inout) :: aterm
    real(DP), intent(inout) :: rhsterm
    ! local
    real(DP) :: rhs_ds, rhs_dh, a_dh

    ! part 1: change in s
    rhs_ds = phi * thk * (s_new - s_old) / delt

    ! part 2: change in h
    a_dh = thk * Cm / delt
    rhs_dh = a_dh * h_new

    aterm = a_dh
    rhsterm = rhs_ds + rhs_dh

  end subroutine get_unsat_storage_terms_MP

end module UzrStorageModule