module SpgModule
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DHALF, DONE, &
                             LENFTYPE, LENPACKAGENAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: count_errors, store_error, store_error_filename
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, &
                                 mem_setptr, mem_checkin
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
  !
  type, extends(BndExtType) :: SpgType
    integer(I4B), dimension(:), pointer, contiguous :: ihc => null() !< connection type (0 vertical face, 1 horizontal)
    real(DP), dimension(:), pointer, contiguous :: cl1 => null() !< distance from cell center to seepage face
    real(DP), dimension(:), pointer, contiguous :: hwva => null() !< seepage face area
    real(DP), dimension(:), pointer, contiguous :: angldegx => null() !< face-normal angle with x axis (degrees)
  contains
    procedure :: allocate_scalars => spg_allocate_scalars
    procedure :: allocate_arrays => spg_allocate_arrays
    procedure :: source_options => spg_source_options
    procedure :: bnd_cf => spg_cf
    procedure :: bnd_fc => spg_fc
    procedure :: bnd_da => spg_da
    procedure :: define_listlabel
    procedure, private :: effective_k
  end type SpgType

contains

  !> @brief Create a New SPG Package
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

  subroutine spg_source_options(this)
    class(SpgType), intent(inout) :: this

    ! source common bound options
    call this%BndExtType%source_options()

  end subroutine spg_source_options

  !> @brief Allocate scalars
  !<
  subroutine spg_allocate_scalars(this)
    class(SpgType) :: this !< this instance

    ! base allocate
    call this%BndExtType%allocate_scalars()

  end subroutine spg_allocate_scalars

  !> @brief Allocate arrays
  !<
  subroutine spg_allocate_arrays(this, nodelist, auxvar)
    class(SpgType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar

    ! call base type allocate arrays
    call this%BndExtType%allocate_arrays(nodelist, auxvar)

    ! set spg input context pointers
    call mem_setptr(this%ihc, 'IHC', this%input_mempath)
    call mem_setptr(this%cl1, 'CL1', this%input_mempath)
    call mem_setptr(this%hwva, 'HWVA', this%input_mempath)
    call mem_setptr(this%angldegx, 'ANGLDEGX', this%input_mempath)

    ! checkin spg input context pointers
    call mem_checkin(this%ihc, 'IHC', this%memoryPath, &
                     'IHC', this%input_mempath)
    call mem_checkin(this%cl1, 'CL1', this%memoryPath, &
                     'CL1', this%input_mempath)
    call mem_checkin(this%hwva, 'HWVA', this%memoryPath, &
                     'HWVA', this%input_mempath)
    call mem_checkin(this%angldegx, 'ANGLDEGX', this%memoryPath, &
                     'ANGLDEGX', this%input_mempath)

  end subroutine spg_allocate_arrays

  !> @brief Effective hydraulic conductivity of cell n in the direction of the
  !! seepage-face normal (defined by ihc and angldegx).
  !!
  !! TODO_UZR: currently returns a constant assumed conductivity (K = 1) so the
  !! conductance plumbing can be exercised.  This is where the anisotropic,
  !! direction-resolved effective K (built from ihc/angldegx) will be
  !! implemented.
  !<
  function effective_k(this, n, ihc, angldegx) result(hy)
    class(SpgType) :: this
    integer(I4B), intent(in) :: n !< reduced node number
    integer(I4B), intent(in) :: ihc !< connection type
    real(DP), intent(in) :: angldegx !< face-normal angle with x axis (degrees)
    real(DP) :: hy

    hy = DONE
  end function effective_k

  !> @brief Formulate the HCOF and RHS terms
  !<
  subroutine spg_cf(this)
    class(SpgType) :: this
    ! local
    integer(I4B) :: i, node
    real(DP) :: z, head, cond

    if (this%nbound .eq. 0) return

    ! Calculate hcof and rhs for each seepage face
    do i = 1, this%nbound
      node = this%nodelist(i)
      if (this%ibound(node) <= 0) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if

      ! seepage-face elevation datum (face centroid) used for the
      ! pressure-head switch psi = head - z
      z = DHALF * (this%dis%bot(node) + this%dis%top(node))
      head = this%xnew(node)
      if (head > z) then
        ! saturated at the face: directional conductance resolves anisotropy
        cond = this%effective_k(node, this%ihc(i), this%angldegx(i)) * &
               this%hwva(i) / this%cl1(i)
        this%hcof(i) = -cond
        this%rhs(i) = -cond * z
      else
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
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

    ! pakmvrobj fc
    if (this%imover == 1) then
      call this%pakmvrobj%fc()
    end if

    ! Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      call matrix_sln%add_value_pos(idxglo(ipos), this%hcof(i))

      ! If mover is active and this boundary is discharging,
      ! store available water (as positive value).
      ! TODO_UZR: implement mover
      if (this%imover == 1) then
        write (errmsg, '(a,a)') "Seepage Mover not supported: ", this%packName
        call store_error(errmsg, terminate=.true.)
      end if
    end do

  end subroutine spg_fc

  !> @brief Define the list heading that is written to iout when PRINT_INPUT
  !< option is used
  subroutine define_listlabel(this)

    class(SpgType), intent(inout) :: this

    ! create the header list label
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
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'IHC'
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'CL1'
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'FACE AREA'
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'ANGLDEGX'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if

  end subroutine define_listlabel

  !> @brief Deallocate memory
  !<
  subroutine spg_da(this)
    class(SpgType) :: this

    call this%BndExtType%bnd_da()

    call mem_deallocate(this%ihc, 'IHC', this%memoryPath)
    call mem_deallocate(this%cl1, 'CL1', this%memoryPath)
    call mem_deallocate(this%hwva, 'HWVA', this%memoryPath)
    call mem_deallocate(this%angldegx, 'ANGLDEGX', this%memoryPath)

  end subroutine spg_da

end module SpgModule
