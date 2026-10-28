! ** Do Not Modify! MODFLOW 6 system generated file. **
module GwfVseInputModule
  use ConstantsModule, only: LENVARNAME
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public gwf_vse_param_definitions
  public gwf_vse_aggregate_definitions
  public gwf_vse_block_definitions
  public GwfVseParamFoundType
  public gwf_vse_multi_package
  public gwf_vse_is_advanced
  public gwf_vse_subpackages

  type GwfVseParamFoundType
    logical :: et_model = .false.
    logical :: h1 = .false.
    logical :: h2 = .false.
    logical :: h3 = .false.
    logical :: h4 = .false.
    logical :: auxiliary = .false.
    logical :: auxmultname = .false.
    logical :: boundnames = .false.
    logical :: iprpak = .false.
    logical :: iprflow = .false.
    logical :: ipakcb = .false.
    logical :: ts_filerecord = .false.
    logical :: ts6 = .false.
    logical :: filein = .false.
    logical :: ts6_filename = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: obs6_filename = .false.
    logical :: maxbound = .false.
    logical :: cellid = .false.
    logical :: rate = .false.
    logical :: auxvar = .false.
    logical :: boundname = .false.
  end type GwfVseParamFoundType

  logical :: gwf_vse_multi_package = .true.
  logical :: gwf_vse_is_advanced = .false.

  character(len=16), parameter :: &
    gwf_vse_subpackages(*) = &
    [ &
    '                ' &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfvse_et_model = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'ET_MODEL', & ! tag name
    'ET_MODEL', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'model used for the root-water uptake reduction', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_h1 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'H1', & ! tag name
    'H1', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'feddes anoxia pressure head', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_h2 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'H2', & ! tag name
    'H2', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'feddes upper optimal pressure head', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_h3 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'H3', & ! tag name
    'H3', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'feddes lower optimal pressure head', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_h4 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'H4', & ! tag name
    'H4', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'feddes wilting point pressure head', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_auxiliary = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'AUXILIARY', & ! tag name
    'AUXILIARY', & ! fortran variable
    'STRING', & ! type
    'NAUX', & ! shape
    'keyword to specify aux variables', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_auxmultname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'AUXMULTNAME', & ! tag name
    'AUXMULTNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'name of auxiliary variable for multiplier', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_boundnames = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'BOUNDNAMES', & ! tag name
    'BOUNDNAMES', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    '', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_iprpak = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_INPUT', & ! tag name
    'IPRPAK', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'print input to listing file', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_iprflow = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'IPRFLOW', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'print transpiration rates to listing file', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_ipakcb = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'IPAKCB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'save transpiration rates to budget file', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_ts_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'TS_FILERECORD', & ! tag name
    'TS_FILERECORD', & ! fortran variable
    'RECORD TS6 FILEIN TS6_FILENAME', & ! type
    '', & ! shape
    '', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_ts6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'TS6', & ! tag name
    'TS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'head keyword', & ! longname
    .true., & ! required
    .false., & ! developmode
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_filein = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'FILEIN', & ! tag name
    'FILEIN', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'file keyword', & ! longname
    .true., & ! required
    .false., & ! developmode
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_ts6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'TS6_FILENAME', & ! tag name
    'TS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'file name of time series information', & ! longname
    .true., & ! required
    .false., & ! developmode
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_obs_filerecord = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'OBS_FILERECORD', & ! tag name
    'OBS_FILERECORD', & ! fortran variable
    'RECORD OBS6 FILEIN OBS6_FILENAME', & ! type
    '', & ! shape
    '', & ! longname
    .false., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_obs6 = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6', & ! tag name
    'OBS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    'obs keyword', & ! longname
    .true., & ! required
    .false., & ! developmode
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_obs6_filename = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6_FILENAME', & ! tag name
    'OBS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'obs6 input filename', & ! longname
    .true., & ! required
    .false., & ! developmode
    .true., & ! multi-record
    .true., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_maxbound = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'DIMENSIONS', & ! block
    'MAXBOUND', & ! tag name
    'MAXBOUND', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    'maximum number of transpiration cells', & ! longname
    .true., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_cellid = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'PERIOD', & ! block
    'CELLID', & ! tag name
    'CELLID', & ! fortran variable
    'INTEGER1D', & ! type
    'NCELLDIM', & ! shape
    'cell identifier', & ! longname
    .true., & ! required
    .false., & ! developmode
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_rate = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'PERIOD', & ! block
    'RATE', & ! tag name
    'RATE', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    'potential transpiration rate', & ! longname
    .true., & ! required
    .false., & ! developmode
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_auxvar = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'PERIOD', & ! block
    'AUX', & ! tag name
    'AUXVAR', & ! fortran variable
    'DOUBLE1D', & ! type
    'NAUX', & ! shape
    'auxiliary variables', & ! longname
    .false., & ! required
    .false., & ! developmode
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .true. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwfvse_boundname = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'PERIOD', & ! block
    'BOUNDNAME', & ! tag name
    'BOUNDNAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    'transpiration name', & ! longname
    .false., & ! required
    .false., & ! developmode
    .true., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_vse_param_definitions(*) = &
    [ &
    gwfvse_et_model, &
    gwfvse_h1, &
    gwfvse_h2, &
    gwfvse_h3, &
    gwfvse_h4, &
    gwfvse_auxiliary, &
    gwfvse_auxmultname, &
    gwfvse_boundnames, &
    gwfvse_iprpak, &
    gwfvse_iprflow, &
    gwfvse_ipakcb, &
    gwfvse_ts_filerecord, &
    gwfvse_ts6, &
    gwfvse_filein, &
    gwfvse_ts6_filename, &
    gwfvse_obs_filerecord, &
    gwfvse_obs6, &
    gwfvse_obs6_filename, &
    gwfvse_maxbound, &
    gwfvse_cellid, &
    gwfvse_rate, &
    gwfvse_auxvar, &
    gwfvse_boundname &
    ]

  type(InputParamDefinitionType), parameter :: &
    gwfvse_spd = InputParamDefinitionType &
    ( &
    'GWF', & ! component
    'VSE', & ! subcomponent
    'PERIOD', & ! block
    'STRESS_PERIOD_DATA', & ! tag name
    'SPD', & ! fortran variable
    'RECARRAY CELLID RATE AUX BOUNDNAME', & ! type
    'MAXBOUND', & ! shape
    '', & ! longname
    .true., & ! required
    .false., & ! developmode
    .false., & ! multi-record
    .false., & ! preserve case
    .false., & ! layered
    .false. & ! timeseries
    )

  type(InputParamDefinitionType), parameter :: &
    gwf_vse_aggregate_definitions(*) = &
    [ &
    gwfvse_spd &
    ]

  type(InputBlockDefinitionType), parameter :: &
    gwf_vse_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false., & ! aggregate
    .false. & ! block_variable
    ), &
    InputBlockDefinitionType( &
    'PERIOD', & ! blockname
    .true., & ! required
    .true., & ! aggregate
    .true. & ! block_variable
    ) &
    ]

end module GwfVseInputModule
