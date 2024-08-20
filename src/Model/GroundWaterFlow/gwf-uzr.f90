module GwfUzrModule
  use KindModule, only: I4B, DP
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use GwfNpfModule, only: GwfNpfType  
  use GwfNpfExtModule, only: GwfNpfExtType
  use GwfStoModule, only: GwfStoType
  use GwfStoExtModule, only: GwfStoExtType
  use UzrFlowModule, only: UzrFlowType
  use UzrStorageModule, only: UzrStorageType
  implicit none
  private

  public :: GwfUzrType, uzr_cr

  type, extends(NumericalPackageType) :: GwfUzrType
    integer(I4B), dimension(:), pointer, contiguous :: iunsat => null() !< 0 = standard node, 1 = unsaturated (Richards) node
    class(UzrFlowType), pointer :: uzr_flow => null() !< the NPF flow extension
    class(UzrStorageType), pointer :: uzr_sto => null() !< the STO storage calculation extension
  contains
    procedure :: uzr_df
    procedure :: uzr_ar
    procedure :: uzr_da
    ! private
    procedure, private :: source_griddata
  end type GwfUzrType

contains

  subroutine uzr_cr(uzr_obj, name_model, input_mempath, inunit, iout)
    type(GwfUzrType), pointer, intent(inout) :: uzr_obj
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: input_mempath
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! local
    character(len=*), parameter :: fmtheader = &
      "(1x, /1x, 'UZR -- RICHARDS FLOW PACKAGE, VERSION 1, 3/30/2015', &
       &' INPUT READ FROM MEMPATH: ', A, /)"

    allocate (uzr_obj)

    call uzr_obj%set_names(1, name_model, 'UZR', 'UZR', input_mempath)
    call uzr_obj%allocate_scalars()

    uzr_obj%inunit = inunit
    uzr_obj%iout = iout
    
    ! print a message when enabled
    if (inunit > 0) then
      write (iout, fmtheader) input_mempath
    end if

  end subroutine uzr_cr

  subroutine uzr_df(this, dis, npf, sto)
    use MemoryManagerModule, only: mem_allocate
    class(GwfUzrType), intent(inout) :: this
    class(DisBaseType), pointer, intent(inout) :: dis
    type(GwfNpfType), pointer, intent(inout) :: npf
    type(GwfStoType), pointer, intent(inout) :: sto
    ! local
    class(GwfNpfExtType), pointer :: npf_ext
    class(GwfStoExtType), pointer :: sto_ext

    this%dis => dis

    ! allocate arrays
    call mem_allocate(this%iunsat, dis%nodes, "IUNSAT", this%memoryPath)

    ! load from idm
    call this%source_griddata()

    ! inject Richards into NPF:
    allocate (this%uzr_flow)
    call this%uzr_flow%initialize(this%iunsat, dis, npf)

    npf_ext => this%uzr_flow
    call npf%set_flow_extension(npf_ext)

    ! inject storage calculation
    allocate (this%uzr_sto)
    call this%uzr_sto%initialize(this%iunsat, dis, sto)

    sto_ext => this%uzr_sto
    call sto%set_storage_extension(sto_ext)

  end subroutine uzr_df

  subroutine source_griddata(this)
    use GwfUzrInputModule, only: GwfUzrParamFoundType
    use MemoryManagerExtModule, only: mem_set_value
    class(GwfUzrType), intent(inout) :: this
    ! local
    type(GwfUzrParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map

    map => null()
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser
    !
    ! -- set values
    call mem_set_value(this%iunsat, 'IUNSAT', this%input_mempath, &
                       map, found%iunsat)

  end subroutine source_griddata

  subroutine uzr_ar(this)
    class(GwfUzrType), intent(inout) :: this
    
  end subroutine uzr_ar

  subroutine uzr_da(this)
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorystore_remove
    use SimVariablesModule, only: idm_context
    class(GwfUzrType), intent(inout) :: this

    if (this%inunit > 0) then
      call this%uzr_flow%destroy()
      deallocate (this%uzr_flow)

      call mem_deallocate(this%iunsat)
    end if

    call memorystore_remove(this%name_model, 'UZR', idm_context)

    call this%NumericalPackageType%da()

  end subroutine uzr_da

end module GwfUzrModule