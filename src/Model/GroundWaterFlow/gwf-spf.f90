module SpfModule
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DHALF, DONE, DEM6, DPIO180, &
                             LENFTYPE, LENPACKAGENAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: count_errors, store_error, store_error_filename
  use MemoryManagerModule, only: mem_allocate, mem_deallocate, &
                                 mem_setptr, mem_checkin
  use MemoryHelperModule, only: create_mem_path
  use ConductanceProviderModule, only: ConductanceProviderType
  use BndModule, only: BndType
  use BndExtModule, only: BndExtType
  use MatrixBaseModule

  implicit none
  private

  public :: spf_create
  public :: SpfType

  character(len=LENFTYPE) :: ftype = 'SPF'
  character(len=LENPACKAGENAME) :: text = '             SPF'
  !
  type, extends(BndExtType) :: SpfType
    integer(I4B), dimension(:), pointer, contiguous :: ihc => null() !< connection type (0 vertical face, 1 horizontal)
    real(DP), dimension(:), pointer, contiguous :: cl1 => null() !< distance from cell center to seepage face
    real(DP), dimension(:), pointer, contiguous :: hwva => null() !< seepage face area
    real(DP), dimension(:), pointer, contiguous :: angldegx => null() !< face-normal angle with x axis (degrees)
    class(ConductanceProviderType), pointer :: cond_provider => null() !< directional effective-K provider (injected by the model)
    logical(LGP), private, pointer :: some_option => null() !< some option
  contains
    procedure :: allocate_scalars => spf_allocate_scalars
    procedure :: allocate_arrays => spf_allocate_arrays
    procedure :: source_options => spf_source_options
    procedure :: bnd_cf => spf_cf
    procedure :: bnd_fc => spf_fc
    procedure :: bnd_da => spf_da
    procedure :: define_listlabel
    procedure, private :: effective_k
  end type SpfType

contains

  !> @brief Create a New SPF Package
  !<
  subroutine spf_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
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
    type(SpfType), pointer :: spfobj

    ! allocate the object and assign values to object variables
    allocate (spfobj)
    packobj => spfobj

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
    spfobj%some_option = .false.

  end subroutine spf_create

  subroutine spf_source_options(this)
    use MemoryManagerExtModule, only: mem_set_value
    use GwfSpfInputModule, only: GwfSpfParamFoundType
    class(SpfType), intent(inout) :: this
    ! local
    type(GwfSpfParamFoundType) :: found

    ! source common bound options
    call this%BndExtType%source_options()

    call mem_set_value(this%some_option, 'SOME_OPTION', &
                       this%input_mempath, found%some_option)

  end subroutine spf_source_options

  !> @brief Allocate scalars
  !<
  subroutine spf_allocate_scalars(this)
    class(SpfType) :: this !< this instance

    ! base allocate
    call this%BndExtType%allocate_scalars()

    call mem_allocate(this%some_option, 'DEV_VS2D_BND', this%memoryPath)

  end subroutine spf_allocate_scalars

  !> @brief Allocate arrays
  !<
  subroutine spf_allocate_arrays(this, nodelist, auxvar)
    class(SpfType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar

    ! call base type allocate arrays
    call this%BndExtType%allocate_arrays(nodelist, auxvar)

    ! set spf input context pointers
    call mem_setptr(this%ihc, 'IHC', this%input_mempath)
    call mem_setptr(this%cl1, 'CL1', this%input_mempath)
    call mem_setptr(this%hwva, 'HWVA', this%input_mempath)
    call mem_setptr(this%angldegx, 'ANGLDEGX', this%input_mempath)

    ! checkin spf input context pointers
    call mem_checkin(this%ihc, 'IHC', this%memoryPath, &
                     'IHC', this%input_mempath)
    call mem_checkin(this%cl1, 'CL1', this%memoryPath, &
                     'CL1', this%input_mempath)
    call mem_checkin(this%hwva, 'HWVA', this%memoryPath, &
                     'HWVA', this%input_mempath)
    call mem_checkin(this%angldegx, 'ANGLDEGX', this%memoryPath, &
                     'ANGLDEGX', this%input_mempath)

  end subroutine spf_allocate_arrays

  !> @brief Effective hydraulic conductivity of cell n in the direction of the
  !! seepage-face normal (built from ihc and angldegx), resolving anisotropy
  !< through the model's conductance provider.
  function effective_k(this, n, ihc, angldegx) result(hy)
    class(SpfType) :: this
    integer(I4B), intent(in) :: n !< reduced node number
    integer(I4B), intent(in) :: ihc !< connection type
    real(DP), intent(in) :: angldegx !< face-normal angle with x axis (degrees)
    real(DP) :: hy
    ! local
    real(DP), dimension(3) :: vg

    ! outward unit normal of the seepage face
    if (ihc == 0) then
      vg = [DZERO, DZERO, DONE]
    else
      vg = [cos(angldegx * DPIO180), sin(angldegx * DPIO180), DZERO]
    end if

    hy = this%cond_provider%eff_hy(n, ihc, vg)
  end function effective_k

  !> @brief Formulate the HCOF and RHS terms
  !<
  subroutine spf_cf(this)
    class(SpfType) :: this
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

  end subroutine spf_cf

  !> @brief Copy rhs and hcof into solution rhs and amat
  !<
  subroutine spf_fc(this, rhs, ia, idxglo, matrix_sln)
    class(SpfType) :: this
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

  end subroutine spf_fc

  !> @brief Define the list heading that is written to iout when PRINT_INPUT
  !< option is used
  subroutine define_listlabel(this)

    class(SpfType), intent(inout) :: this

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
  subroutine spf_da(this)
    class(SpfType) :: this

    call this%BndExtType%bnd_da()

    call mem_deallocate(this%ihc, 'IHC', this%memoryPath)
    call mem_deallocate(this%cl1, 'CL1', this%memoryPath)
    call mem_deallocate(this%hwva, 'HWVA', this%memoryPath)
    call mem_deallocate(this%angldegx, 'ANGLDEGX', this%memoryPath)

    ! provider is owned by the model; just disassociate
    nullify (this%cond_provider)

    call mem_deallocate(this%some_option)

  end subroutine spf_da

end module SpfModule
