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
    logical :: iunsat = .false.
  end type GwfUzrParamFoundType

  logical :: gwf_uzr_multi_package = .false.

  character(len=16), parameter :: &
    gwf_uzr_subpackages(*) = &
    [ &
    '                ' &
    ]

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
    gwf_uzr_param_definitions(*) = &
    [ &
    gwfuzr_iunsat &
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
