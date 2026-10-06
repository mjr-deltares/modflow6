module SpgModule
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DHALF, DONE, LENFTYPE, LENPACKAGENAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: count_errors, store_error, store_error_filename
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryHelperModule, only: create_mem_path
  use BndModule, only: BndType
  use BndExtModule, only: BndExtType
  use MatrixBaseModule

  implicit none
  private

  public :: spg_create
  public :: SpgType

  character(len=LENFTYPE) :: ftype = 'SPG'
  character(len=LENPACKAGENAME) :: text = '             SPG'

  !> @brief Specified-gradient-free seepage boundary package.
  !!
  !! Each listed cell behaves as a seepage face held at atmospheric pressure
  !! (zero pressure head, so total head equals the cell-center elevation) while
  !! water discharges into the cell (out of the simulated domain), and reverts
  !! to a no-flow boundary when the flow would reverse into the domain.  The
  !! active (constant-head) state is enforced either with a large penalty
  !! conductance (default) or, when IBOUND_TOGGLE is set, by toggling the cell
  !! to a true constant-head cell through the IBOUND array.
  !<
  type, extends(BndExtType) :: SpgType
    integer(I4B), pointer :: itoggle => null() !< 0 = penalty method, 1 = ibound-toggle method
    real(DP), pointer :: penalty_cond => null() !< penalty conductance for the active state
    integer(I4B), dimension(:), pointer, contiguous :: iseepstate => null() !< per-cell toggle state (1 held as constant head, 0 free/no-flow)
  contains
    procedure :: allocate_scalars => spg_allocate_scalars
    procedure :: allocate_arrays => spg_allocate_arrays
    procedure :: source_options => spg_source_options
    procedure :: bnd_rp => spg_rp
    procedure :: bnd_cf => spg_cf
    procedure :: bnd_fc => spg_fc
    procedure :: bnd_cq => spg_cq
    procedure :: bnd_da => spg_da
    procedure :: define_listlabel
    procedure, private :: seep_elevation
    procedure, private :: seepage_rate
  end type SpgType

contains

  !> @brief Create a new SPG package
  !<
  subroutine spg_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        mempath)
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: ibcnum
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    character(len=*), intent(in) :: mempath
    ! local
    type(SpgType), pointer :: spgobj

    ! allocate the object and assign values to object variables
    allocate (spgobj)
    packobj => spgobj

    ! create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype, mempath)
    packobj%text = text

    ! allocate scalars
    call packobj%allocate_scalars()

    ! initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ictMemPath = create_mem_path(namemodel, 'NPF')

  end subroutine spg_create

  !> @brief Source package options from the input context
  !<
  subroutine spg_source_options(this)
    use MemoryManagerExtModule, only: mem_set_value
    use GwfSpgInputModule, only: GwfSpgParamFoundType
    class(SpgType), intent(inout) :: this
    ! local
    type(GwfSpgParamFoundType) :: found

    ! source common bound options
    call this%BndExtType%source_options()

    call mem_set_value(this%penalty_cond, 'PENALTY_COND', &
                       this%input_mempath, found%penalty_cond)
    if (found%ibound_toggle) this%itoggle = 1

    if (found%ibound_toggle) then
      write (this%iout, '(4x,a)') &
        'SEEPAGE ACTIVE STATE ENFORCED WITH CONSTANT-HEAD IBOUND TOGGLE.'
    else
      write (this%iout, '(4x,a,1pg15.6)') &
        'SEEPAGE ACTIVE STATE ENFORCED WITH PENALTY CONDUCTANCE =', &
        this%penalty_cond
    end if

  end subroutine spg_source_options

  !> @brief Allocate scalars
  !<
  subroutine spg_allocate_scalars(this)
    class(SpgType) :: this

    ! base allocate
    call this%BndExtType%allocate_scalars()

    call mem_allocate(this%itoggle, 'ITOGGLE', this%memoryPath)
    call mem_allocate(this%penalty_cond, 'PENALTY_COND', this%memoryPath)

    this%itoggle = 0
    this%penalty_cond = 1.0e9_DP

  end subroutine spg_allocate_scalars

  !> @brief Allocate arrays
  !<
  subroutine spg_allocate_arrays(this, nodelist, auxvar)
    class(SpgType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! local
    integer(I4B) :: i

    ! call base type allocate arrays
    call this%BndExtType%allocate_arrays(nodelist, auxvar)

    ! per-cell seepage toggle state
    call mem_allocate(this%iseepstate, this%maxbound, 'ISEEPSTATE', &
                      this%memoryPath)
    do i = 1, this%maxbound
      this%iseepstate(i) = 0
    end do

  end subroutine spg_allocate_arrays

  !> @brief Read and prepare
  !!
  !! For the ibound-toggle method, release any previously held constant-head
  !! cells back to active before new period data replaces the cell list.
  !<
  subroutine spg_rp(this)
    use TdisModule, only: kper
    class(SpgType), intent(inout) :: this
    ! local
    integer(I4B) :: i, node

    if (this%itoggle == 1 .and. this%iper == kper) then
      do i = 1, this%nbound
        node = this%nodelist(i)
        if (node > 0 .and. this%iseepstate(i) == 1) then
          this%ibound(node) = 1
        end if
      end do
    end if

    call this%BndExtType%bnd_rp()

    if (this%itoggle == 1 .and. this%iper == kper) then
      do i = 1, this%nbound
        this%iseepstate(i) = 0
      end do
    end if

  end subroutine spg_rp

  !> @brief Formulate the HCOF and RHS terms (and toggle ibound if requested)
  !<
  subroutine spg_cf(this)
    class(SpgType) :: this
    ! local
    integer(I4B) :: i, node
    real(DP) :: z, head, rate

    if (this%nbound == 0) return

    do i = 1, this%nbound
      node = this%nodelist(i)
      this%hcof(i) = DZERO
      this%rhs(i) = DZERO

      ! skip permanently inactive cells
      if (this%ibound(node) == 0) cycle

      z = this%seep_elevation(node)

      if (this%itoggle == 0) then
        ! penalty method: hold head near z with a large conductance while the
        ! cell is discharging (head above the seepage elevation)
        if (this%ibound(node) > 0) then
          head = this%xnew(node)
          if (head > z) then
            this%hcof(i) = -this%penalty_cond
            this%rhs(i) = -this%penalty_cond * z
          end if
        end if
      else
        ! ibound-toggle method: switch between true constant head and no-flow
        if (this%iseepstate(i) == 1) then
          ! currently held: release when flow reverses out of the seepage cell
          rate = this%seepage_rate(node)
          if (rate < DZERO) then
            this%iseepstate(i) = 0
            this%ibound(node) = 1
          else
            this%xnew(node) = z
          end if
        else
          ! currently free (no-flow): hold when head rises above z
          head = this%xnew(node)
          if (head > z) then
            this%iseepstate(i) = 1
            this%ibound(node) = -this%ibcnum
            this%xnew(node) = z
          end if
        end if
      end if
    end do

  end subroutine spg_cf

  !> @brief Copy rhs and hcof into solution rhs and amat
  !<
  subroutine spg_fc(this, rhs, ia, idxglo, matrix_sln)
    class(SpgType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    class(MatrixBaseType), pointer :: matrix_sln
    ! local
    integer(I4B) :: i, n, ipos

    if (this%imover == 1) then
      write (errmsg, '(a,a)') 'Seepage mover not supported: ', this%packName
      call store_error(errmsg, terminate=.true.)
    end if

    ! Copy package rhs and hcof into solution rhs and amat.  For held cells in
    ! the ibound-toggle method these terms are zero; the constant head is
    ! enforced through the ibound array instead.
    do i = 1, this%nbound
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      call matrix_sln%add_value_pos(idxglo(ipos), this%hcof(i))
    end do

  end subroutine spg_fc

  !> @brief Calculate seepage flows for the budget
  !<
  subroutine spg_cq(this, x, flowja, iadv)
    class(SpgType), intent(inout) :: this
    real(DP), dimension(:), intent(in) :: x
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    integer(I4B), optional, intent(in) :: iadv
    ! local
    integer(I4B) :: i, node, idiag
    real(DP) :: rrate

    ! penalty method uses the standard hcof/rhs flow calculation
    if (this%itoggle == 0) then
      if (present(iadv)) then
        call this%BndExtType%bnd_cq(x, flowja, iadv)
      else
        call this%BndExtType%bnd_cq(x, flowja)
      end if
      return
    end if

    ! ibound-toggle method: held cells are constant head, so accumulate their
    ! rate from the surrounding intercell flows (as the CHD package does)
    if (this%nbound == 0) return
    do i = 1, this%nbound
      node = this%nodelist(i)
      rrate = DZERO
      if (node > 0) then
        if (this%iseepstate(i) == 1 .and. this%ibound(node) < 0) then
          idiag = this%dis%con%ia(node)
          rrate = this%seepage_rate(node)
          ! rrate is the inflow to the held constant-head cell; balance its
          ! flowja row, then report it as a sink (negative = out of the model)
          flowja(idiag) = flowja(idiag) + rrate
          rrate = -rrate
        end if
      end if
      this%simvals(i) = rrate
    end do

  end subroutine spg_cq

  !> @brief Seepage-face elevation datum (cell center) for cell node
  !<
  function seep_elevation(this, node) result(z)
    class(SpgType) :: this
    integer(I4B), intent(in) :: node
    real(DP) :: z

    z = DHALF * (this%dis%bot(node) + this%dis%top(node))
  end function seep_elevation

  !> @brief Net groundwater flow into cell node from its connections.
  !!
  !! A positive value is discharge into the seepage cell (out of the simulated
  !< domain); a negative value indicates flow out of the seepage cell.
  function seepage_rate(this, node) result(rate)
    class(SpgType) :: this
    integer(I4B), intent(in) :: node
    real(DP) :: rate
    ! local
    integer(I4B) :: ipos

    rate = DZERO
    do ipos = this%dis%con%ia(node) + 1, this%dis%con%ia(node + 1) - 1
      rate = rate - this%flowja(ipos) ! = -(outflow) = inflow
    end do
  end function seepage_rate

  !> @brief Define the list heading printed when PRINT_INPUT is used
  !<
  subroutine define_listlabel(this)
    class(SpgType), intent(inout) :: this

    this%listlabel = trim(this%filtyp)//' NO.'
    if (this%dis%ndim == 3) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'ROW'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'COL'
    elseif (this%dis%ndim == 2) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'CELL2D'
    else
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'NODE'
    end if
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if

  end subroutine define_listlabel

  !> @brief Deallocate memory
  !<
  subroutine spg_da(this)
    class(SpgType) :: this

    call this%BndExtType%bnd_da()

    call mem_deallocate(this%itoggle)
    call mem_deallocate(this%penalty_cond)
    call mem_deallocate(this%iseepstate, 'ISEEPSTATE', this%memoryPath)

  end subroutine spg_da

end module SpgModule
