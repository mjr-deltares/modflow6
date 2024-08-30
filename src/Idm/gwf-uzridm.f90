! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfUzrInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_uzr_param_definitions
  public gwf_uzr_aggregate_definitions
  public gwf_uzr_block_definitions
  public GwfUzrParamFoundType
  public gwf_uzr_multi_package
  public gwf_uzr_subpackages

  type GwfUzrParamFoundType
    logical :: soil_model = .false.
    logical :: dev_model_kr = .false.
    logical :: storage_scheme = .false.
    logical :: kr_averaging = .false.
    logical :: iunsat = .false.
    logical :: porosity = .false.
    logical :: satres = .false.
    logical :: alphahvk = .false.
    logical :: nhvk = .false.
    logical :: betahvk = .false.
    logical :: khvk = .false.
    logical :: alphavgn = .false.
    logical :: nvgn = .false.
  end type GwfUzrParamFoundType

  logical :: gwf_uzr_multi_package = .false.

  character(len=16), parameter :: &
    gwf_uzr_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_soil_model = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'OPTIONS', & ! block
    'SOIL_MODEL', & ! tag name
    'SOIL_MODEL', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'model used for the soil characteristic functions', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_dev_model_kr = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'OPTIONS', & ! block
    'DEV_MODEL_KR', & ! tag name
    'DEV_MODEL_KR', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'model used for the relative permeability', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_storage_scheme = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'OPTIONS', & ! block
    'STORAGE_SCHEME', & ! tag name
    'STORAGE_SCHEME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'discretization method for storage calculation', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_kr_averaging = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'OPTIONS', & ! block
    'KR_AVERAGING', & ! tag name
    'KR_AVERAGING', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'relative permeability averaging method', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_iunsat = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'GRIDDATA', & ! block
    'IUNSAT', & ! tag name
    'IUNSAT', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    'unsaturated zone indicator', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_porosity = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'GRIDDATA', & ! block
    'POROSITY', & ! tag name
    'POROSITY', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    'porosity', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_satres = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'GRIDDATA', & ! block
    'SATRES', & ! tag name
    'SATRES', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    'residual saturation', & ! longname
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_alphahvk = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'GRIDDATA', & ! block
    'ALPHAHVK', & ! tag name
    'ALPHAHVK', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    'alpha', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_nhvk = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'GRIDDATA', & ! block
    'NHVK', & ! tag name
    'NHVK', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    'n', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_betahvk = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'GRIDDATA', & ! block
    'BETAHVK', & ! tag name
    'BETAHVK', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    'beta', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_khvk = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'GRIDDATA', & ! block
    'KHVK', & ! tag name
    'KHVK', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    'k', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_alphavgn = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'GRIDDATA', & ! block
    'ALPHAVGN', & ! tag name
    'ALPHAVGN', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    'alpha', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfuzr_nvgn = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'UZR', & ! subcomponent
    'GRIDDATA', & ! block
    'NVGN', & ! tag name
    'NVGN', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    'n', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .true., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_uzr_param_definitions(*) = &
    [ &
    gwfuzr_soil_model, &
    gwfuzr_dev_model_kr, &
    gwfuzr_storage_scheme, &
    gwfuzr_kr_averaging, &
    gwfuzr_iunsat, &
    gwfuzr_porosity, &
    gwfuzr_satres, &
    gwfuzr_alphahvk, &
    gwfuzr_nhvk, &
    gwfuzr_betahvk, &
    gwfuzr_khvk, &
    gwfuzr_alphavgn, &
    gwfuzr_nvgn &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwf_uzr_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType &
    ( &
    '', & ! component
    '', & ! subcomponent
    '', & ! block
    '', & ! tag name
    '', & ! fortran variable
    '', & ! type
    '', & ! shape
    '', & ! longname
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    ) &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_uzr_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ) &
    ]

end module GwfUzrInputModule
