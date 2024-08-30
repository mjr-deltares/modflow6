module UzrFlowModule 
  use KindModule, only: I4B, LGP, DP
  use ConstantsModule, only: DONE, DTWO, DHALF, DZERO, LENVARNAME
  use MatrixBaseModule, only: MatrixBaseType
  use BaseDisModule, only: DisBaseType
  use GwfNpfModule, only: GwfNpfType
  use GwfNpfExtModule, only: GwfNpfExtType
  use UzrSoilModelModule, only: SoilModelType
  implicit none
  private

  public :: UzrFlowType
  public :: kr_averaging_name

  character(len=LENVARNAME), parameter, dimension(3) :: kr_averaging_name = &
      &[character(len=LENVARNAME) :: 'GEOMETRIC', 'ARITHMETIC', 'UPSTREAM']

  enum, bind(C)
    enumerator :: KR_GEOMETRIC = 1 !< Geometric mean of relative permeability
    enumerator :: KR_ARITHMETIC = 2 !< Arithmetic mean of relative permeability
    enumerator :: KR_UPSTREAM = 3 !< Upstream relative permeability
  end enum

  type, extends(GwfNpfExtType) :: UzrFlowType
    integer(I4B), pointer, dimension(:), contiguous :: iunsat => null() !< see UZR
    integer(I4B), pointer :: kr_averaging => null() !< see UZR
    class(SoilModelType), pointer :: soil_model => null() !< soil model used to get relative permeability
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

  subroutine initialize(this, iunsat, kr_avg, soil_model, dis, npf)
    class(UzrFlowType), intent(inout) :: this
    integer(I4B), pointer, dimension(:), contiguous, intent(in) :: iunsat
    integer(I4B), pointer :: kr_avg
    class(SoilModelType), pointer :: soil_model
    class(DisBaseType), pointer :: dis
    type(GwfNpfType), pointer, intent(in) :: npf

    this%iunsat => iunsat
    this%kr_averaging => kr_avg
    this%soil_model => soil_model

    this%gwf_dis => dis
    this%gwf_npf => npf

  end subroutine initialize

  function uft_is_active(this, n, m) result(is_active)
    class(UzrFlowType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    logical(LGP) :: is_active

    is_active = .false.
    if (this%iunsat(n) == 1 .or. this%iunsat(m) == 1) then
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
    real(DP) :: psi !< the pressure head
    real(DP) :: kr_n !< rel. permeability for node n
    real(DP) :: kr_m !< rel. permeability for node m
    real(DP) :: kr_avg !< weighted rel. permeability between nodes
    integer(I4B) :: idiag !<  diagonal position
    integer(I4B) :: isymcon !< position of reverse connection m-n

    sat_cond = this%gwf_npf%condsat(this%gwf_dis%con%jas(ipos))

    ! calculate k_r
    z_n = DHALF * (this%gwf_dis%bot(n) + this%gwf_dis%top(n))
    psi = hnew(n) - z_n
    kr_n = this%soil_model%krelative(psi, n)
    
    z_m = DHALF * (this%gwf_dis%bot(m) + this%gwf_dis%top(m))
    psi = hnew(m) - z_m
    kr_m = this%soil_model%krelative(psi, m)

    ! averaging of k_r
    kr_avg = kr_averaging(kr_n, kr_m, hnew(n), hnew(m), this%kr_averaging)

    ! calculate unsaturated conductance
    cond = kr_avg * sat_cond

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
    this%iunsat => null()

  end subroutine destroy

  pure function kr_averaging(kr_n, kr_m, h_n, h_m, iavg) result(kr_avg)
    real(DP), intent(in) :: kr_n !< kr for node n
    real(DP), intent(in) :: kr_m !< kr for node m
    real(DP), intent(in) :: h_n !< h for node n
    real(DP), intent(in) :: h_m !< h for node m
    integer(I4B), intent(in) :: iavg !< averaging method
    real(DP) :: kr_avg !< averaged kr

    select case (iavg)
    case (KR_GEOMETRIC)
      kr_avg = sqrt(kr_n * kr_m)
    case (KR_ARITHMETIC)
      kr_avg = DHALF * (kr_n + kr_m)
    case (KR_UPSTREAM)
      if (h_n > h_m) then
        kr_avg = kr_n
      else
        kr_avg = kr_m
      end if
    case default
      kr_avg = sqrt(kr_n * kr_m)
    end select

  end function kr_averaging

  pure function rel_permeability(head, elevation) result(k_r)
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