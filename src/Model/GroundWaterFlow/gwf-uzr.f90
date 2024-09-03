module GwfUzrModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENVARNAME
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryManagerExtModule, only: mem_set_value
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use GwfNpfModule, only: GwfNpfType  
  use GwfNpfExtModule, only: GwfNpfExtType
  use GwfStoModule, only: GwfStoType
  use GwfStoExtModule, only: GwfStoExtType
  use UzrFlowModule, only: UzrFlowType
  use UzrStorageModule, only: UzrStorageType
  use GwfUzrInputModule, only: GwfUzrParamFoundType
  use UzrSoilModelModule, only: SoilModelType
  use UzrSoilModelFactoryModule, only: init_soil_models, &
                                       get_soil_model, &
                                       destroy_soil_models, &
                                       soil_model_name, &
                                       BROOKS_COREY, HAVERKAMP, VANGENUCHTEN
  implicit none
  private

  public :: GwfUzrType, uzr_cr

  type, extends(NumericalPackageType) :: GwfUzrType
    integer(I4B), pointer :: storage_scheme => null() !< 0 = default, 1 = chord slope, 2 = mod. Picard
    integer(I4B), pointer :: soil_model_id => null() !< 0 = default, 1 = Brooks-Corey, 2 = Haverkamp, 3 = Van Genuchten
    class(SoilModelType), pointer :: soil_model => null() !< the soil model
    integer(I4B), pointer :: soil_model_kr_id => null() !< separate soil model used for relative permeability (development option)
    class(SoilModelType), pointer :: soil_model_kr => null() !< separate soil model used for relative permeability
    integer(I4B), pointer :: kr_averaging => null() !< 0 = default, 1 = geometric, 2 = arithm. mean, 3 = upstream
    integer(I4B), dimension(:), pointer, contiguous :: iunsat => null() !< 0 = standard node, 1 = unsaturated (Richards) node
    real(DP), dimension(:), pointer, contiguous :: porosity => null() !< the volumetric fraction of the pore space
    real(DP), dimension(:), pointer, contiguous :: sat_res => null() !< residual (also irreducible) saturation
    class(UzrFlowType), pointer :: uzr_flow => null() !< the NPF flow extension
    class(UzrStorageType), pointer :: uzr_sto => null() !< the STO storage calculation extension
  contains
    procedure :: uzr_df
    procedure :: uzr_ar
    procedure :: uzr_da
    procedure :: allocate_scalars
    ! private
    procedure, private :: source_options
    procedure, private :: source_griddata
    procedure, private :: check_griddata
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
    uzr_obj%storage_scheme = -1
    uzr_obj%soil_model_id = -1
    uzr_obj%soil_model_kr_id = -1
    uzr_obj%kr_averaging = -1
    
    ! print a message when enabled
    if (inunit > 0) then
      write (iout, fmtheader) input_mempath
    end if

  end subroutine uzr_cr

  subroutine uzr_df(this, dis, npf, sto)
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
    call mem_allocate(this%porosity, dis%nodes, 'POROSITY', this%memoryPath)
    call mem_allocate(this%sat_res, dis%nodes, 'SATRES', this%memoryPath)

    ! load from idm
    call this%source_options()
    call this%source_griddata()

    ! set up the soil model(s)
    call init_soil_models(this%porosity, this%sat_res, this%dis, &
                          this%memoryPath, this%input_mempath)
    this%soil_model => get_soil_model(this%soil_model_id)
    this%soil_model_kr => this%soil_model
    if (this%soil_model_kr_id /= this%soil_model_id) then
      ! dev option: separate model for kr
      this%soil_model_kr => get_soil_model(this%soil_model_kr_id)
    end if

    ! inject Richards into NPF:
    allocate (this%uzr_flow)
    call this%uzr_flow%initialize(this%iunsat, this%kr_averaging, &
                                  this%soil_model_kr, dis, npf)

    npf_ext => this%uzr_flow
    call npf%set_flow_extension(npf_ext)

    ! inject storage calculation
    allocate (this%uzr_sto)
    call this%uzr_sto%initialize(this%iunsat, this%storage_scheme, &
                                 this%soil_model, dis, sto)

    sto_ext => this%uzr_sto
    call sto%set_storage_extension(sto_ext)

  end subroutine uzr_df

  subroutine allocate_scalars(this)
    class(GwfUzrType) :: this

    call this%NumericalPackageType%allocate_scalars()

    call mem_allocate(this%soil_model_id, "SOIL_MODEL", this%memoryPath)
    call mem_allocate(this%soil_model_kr_id, "SOIL_MODEL_KR", this%memoryPath)
    call mem_allocate(this%storage_scheme, "STORAGE_SCHEME", this%memoryPath)
    call mem_allocate(this%kr_averaging, "KR_AVERAGING", this%memoryPath)

  end subroutine allocate_scalars

  subroutine source_options(this)
    use UzrFlowModule, only: kr_averaging_name
    use DevFeatureModule, only: dev_feature
    class(GwfUzrType), intent(inout) :: this
    ! local
    type(GwfUzrParamFoundType) :: found
    character(len=LENVARNAME), dimension(2) :: scheme_name = &
      &[character(len=LENVARNAME) :: 'CHORD-SLOPE', 'MODIFIED-PICARD']

    write (this%iout, '(1x,a)') 'Setting UZR options'

    this%soil_model_id = 0
    call mem_set_value(this%soil_model_id, 'SOIL_MODEL', this%input_mempath, &
                       soil_model_name, found%soil_model)
    if (this%soil_model_id > 0) then
      write (this%iout, '(4x,2a)') 'Soil model set to ', &
        trim(soil_model_name(this%soil_model_id))
    else
      this%soil_model_id = BROOKS_COREY ! default
    end if
    this%soil_model_kr_id = 0
    call mem_set_value(this%soil_model_kr_id, 'DEV_MODEL_KR', this%input_mempath, &
                       soil_model_name, found%dev_model_kr)
    if (this%soil_model_kr_id > 0) then
      call dev_feature('DEV_MODEL_KR is a development feature, install the &
            &nightly build or compile from source with IDEVELOPMODE = 1.')
      write (this%iout, '(4x,2a)') 'Soil model for relative permeability set to ', &
        trim(soil_model_name(this%soil_model_kr_id))
    else
      this%soil_model_kr_id = this%soil_model_id
    end if

    this%storage_scheme = 0
    if (this%inewton == 0) then
      ! chord-slope and mod. picard only make sense when not newton
      call mem_set_value(this%storage_scheme, 'STORAGE_SCHEME', this%input_mempath, &
                        scheme_name, found%storage_scheme)
    end if
    if (this%storage_scheme > 0) then
      write (this%iout, '(4x,2a)') 'Storage scheme set to ', &
        trim(scheme_name(this%storage_scheme))
    end if

    this%kr_averaging = 0
    call mem_set_value(this%kr_averaging, 'KR_AVERAGING', this%input_mempath, &
                       kr_averaging_name, found%kr_averaging)
    if (this%kr_averaging > 0) then
      write (this%iout, '(4x,2a)') 'Kr weighting set to ', &
        trim(kr_averaging_name(this%kr_averaging))
    end if

    write (this%iout, '(1x,a)') 'End setting UZR options'

  end subroutine source_options

  subroutine source_griddata(this)
    use MemoryManagerExtModule, only: mem_set_value
    class(GwfUzrType), intent(inout) :: this
    ! local
    type(GwfUzrParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map

    map => null()
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser

    call mem_set_value(this%iunsat, 'IUNSAT', this%input_mempath, &
                       map, found%iunsat)
    call mem_set_value(this%porosity, 'POROSITY', this%input_mempath, &
                       map, found%porosity)
    call mem_set_value(this%sat_res, 'SATRES', this%input_mempath, &
                       map, found%satres)
    call this%check_griddata(found)

  end subroutine source_griddata

  subroutine check_griddata(this, found)    
    class(GwfUzrType), intent(inout) :: this
    type(GwfUzrParamFoundType) :: found

  end subroutine check_griddata

  subroutine uzr_ar(this)
    class(GwfUzrType), intent(inout) :: this
    
  end subroutine uzr_ar

  subroutine uzr_da(this)
    use MemoryManagerExtModule, only: memorystore_remove
    use SimVariablesModule, only: idm_context
    class(GwfUzrType), intent(inout) :: this

    call mem_deallocate(this%soil_model_id)
    call mem_deallocate(this%soil_model_kr_id)
    call mem_deallocate(this%storage_scheme)
    call mem_deallocate(this%kr_averaging)

    if (this%inunit > 0) then
      call destroy_soil_models()

      call this%uzr_flow%destroy()
      deallocate (this%uzr_flow)

      call this%uzr_sto%destroy()
      deallocate (this%uzr_sto)
      
      call mem_deallocate(this%iunsat)
      call mem_deallocate(this%porosity)
      call mem_deallocate(this%sat_res)
    end if

    call memorystore_remove(this%name_model, 'UZR', idm_context)

    call this%NumericalPackageType%da()

  end subroutine uzr_da

end module GwfUzrModule