module GridConnectionModule
  use KindModule, only: I4B, DP
  use SimModule, only: ustop
  use ConstantsModule, only: LENMEMPATH
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryHelperModule, only: create_mem_path
  use ListModule, only: ListType, isEqualIface, arePointersEqual
  use NumericalModelModule
  use DisConnExchangeModule
  use ConnectionsModule
  use SparseModule, only: sparsematrix
  implicit none
  private
  
  !> Data structure to hold a global cell identifier,
  !! using a pointer to the model and its local cell 
  !< index
  type, public :: GlobalCellType
    integer(I4B) :: index                                 !< the local index
    class(NumericalModelType), pointer :: model => null() !< the model
  end type
  
  ! TODO_MJR: this will disappear when we introduce dyn. mem.
  integer(I4B), parameter :: MaxNeighbors = 7
  
  ! a global cell with neighbors
  type, public :: CellWithNbrsType
    type(GlobalCellType) :: cell
    integer(I4B) :: nrOfNbrs = 0
    type(CellWithNbrsType), dimension(:), pointer, contiguous :: neighbors => null()
  end type
  
  ! a model with neighbors
  type, private :: ModelWithNbrsType
      class(NumericalModelType), pointer :: model => null()
      integer(I4B) :: nrOfNbrs = 0
      type(ModelWithNbrsType), dimension(:), pointer, contiguous :: neighbors => null()
  end type
  
  !> This class is used to construct the connections object for 
  !! the interface model's spatial discretization/grid. 
  !! 
  !! It works as follows:
  !!
  !! 1: construct basic instance, allocate data structures
  !!    based on nr. of primary connections
  !! 2: add primary connections from exchanges
  !! 3: add secondary, tertiary, ... MODEL connections, depending
  !!    on the size of the computational stencil
  !! 4: extend the connection, creating the full data structure
  !!    and relations
  !! 5: build the connections object, from which the grid for
  !!    the interface model can be constructed
  !!
  !! A note on the different indices:
  !!
  !! We have
  !!
  !! - GLOBAL index, which technically labels the row in the solution matrix
  !! - REGIONAL index, running over all models that participate in the interface grid
  !! - LOCAL index, local to each model
  !! - INTERFACE index, numbering the cells in the interface grid
  !<
  type, public :: GridConnectionType

    character(len=LENMEMPATH) :: memoryPath
    integer(I4B) :: intStencilDepth !< stencil size for the interior
    integer(I4B) :: extStencilDepth !< stencil size for the exterior

    class(NumericalModelType), pointer :: model => null()
    
    integer(I4B), pointer :: linkCapacity => null()
    
    integer(I4B), pointer :: nrOfBoundaryCells => null()                        !< nr of boundary cells with connection to another model
    type(CellWithNbrsType), dimension(:), pointer :: boundaryCells => null()    !< cells on our side of the primary connections
    type(CellWithNbrsType), dimension(:), pointer :: connectedCells => null()   !< cells on the neighbors side of the primary connection
    type(ModelWithNbrsType), pointer :: modelWithNbrs => null()                 !< tree structure with the neigboring model topology
    type(ListType) :: exchanges                                                 !< all relevant exchanges for this connection, up to
                                                                                !! the required depth
        
    integer(I4B), pointer :: nrOfCells => null()                                !< the total number of cells in the interface
    type(GlobalCellType), dimension(:), pointer :: idxToGlobal => null()        !< a map from interface index to global coordinate
    integer(I4B), dimension(:), pointer, contiguous :: idxToGlobalIdx => null() !< a (flat) map from interface index to global index
     
    integer(I4B), dimension(:), pointer :: regionalToInterfaceIdxMap => null()  !< (sparse) mapping from regional index to interface ixd
    type(ListType)                      :: regionalModels                       !< the models participating in the interface
    integer(I4B), dimension(:), pointer :: regionalModelOffset => null()        !< the new offset to compactify the range of indices
    integer(I4B), pointer               :: indexCount => null()                 !< counts the number of cells in the interface
    type(ConnectionsType), pointer      :: connections => null()                !< sparse matrix with the connections
    integer(I4B), dimension(:), pointer :: connectionMask => null()             !< to mask out connections from the amat coefficient calculation
    
  contains
    ! public
    procedure, pass(this) :: construct
    procedure, private, pass(this) :: allocateScalars, allocateArrays
    procedure, public, pass(this) :: deallocate
    procedure, pass(this) :: connectCell
    procedure, pass(this) :: addModelLink 
    procedure, pass(this) :: extendConnection
    
    ! protected
    procedure, pass(this) :: isPeriodic
    
    ! private routines
    procedure, private, pass(this) :: buildConnections
    procedure, private, pass(this) :: addNeighbors
    procedure, private, pass(this) :: addNeighborCell
    procedure, private, pass(this) :: addRemoteNeighbors
    procedure, private, pass(this) :: connectModels
    procedure, private, pass(this) :: addToRegionalModels
    procedure, private, pass(this) :: getRegionalModelOffset
    procedure, private, pass(this) :: getInterfaceIndex
    procedure, private, pass(this) :: getModelWithNbrs
    procedure, private, pass(this) :: getExchangeData
    procedure, private, pass(this) :: registerInterfaceCells
    procedure, private, pass(this) :: makePrimaryConnections
    procedure, private, pass(this) :: connectNeighborCells
    procedure, private, pass(this) :: fillConnectionDataInternal
    procedure, private, pass(this) :: fillConnectionDataFromExchanges
    procedure, private, pass(this) :: createConnectionMask
    procedure, private, pass(this) :: maskConnections
    procedure, private, pass(this) :: setMaskOnConnection
  end type
  
  contains

  !> @brief Construct the GridConnection and allocate
  !! the data structures for the primary connections
  !<
  subroutine construct(this, model, nCapacity, connectionName)
    class(GridConnectionType), intent(inout) :: this
    class(NumericalModelType), pointer, intent(in) :: model
    integer(I4B) :: nCapacity ! reserves memory
    character(len=*) :: connectionName
    ! local

    this%model => model
    this%memoryPath = create_mem_path(connectionName, 'GC')

    call this%allocateScalars()
    call this%allocateArrays(nCapacity)
    
    allocate(this%modelWithNbrs)
    allocate(this%boundaryCells(nCapacity))
    allocate(this%connectedCells(nCapacity))
    
    this%modelWithNbrs%model => model
    call this%addToRegionalModels(model)
    
    this%linkCapacity = nCapacity
    this%nrOfBoundaryCells = 0

    this%intStencilDepth = 1
    this%extStencilDepth = 1

  end subroutine construct
  
  !> @brief Connect neighboring cells at the interface
  !<
  subroutine connectCell(this, idx1, model1, idx2, model2)
    class(GridConnectionType), intent(in) :: this   !< this grid connection
    integer(I4B)                          :: idx1   !< local index cell 1
    class(NumericalModelType), pointer    :: model1 !< model of cell 1
    integer(I4B)                          :: idx2   !< local index cell 2
    class(NumericalModelType), pointer    :: model2 !< model of cell 2
            
    this%nrOfBoundaryCells = this%nrOfBoundaryCells + 1    
    if (this%nrOfBoundaryCells > this%linkCapacity) then
      write(*,*) 'Error: nr of cell connections exceeds capacity in grid connection, terminating...'
      call ustop()
    end if
    
    if (associated(model1, this%model)) then
      this%boundaryCells(this%nrOfBoundaryCells)%cell%index = idx1
      this%boundaryCells(this%nrOfBoundaryCells)%cell%model => this%model

      this%connectedCells(this%nrOfBoundaryCells)%cell%index = idx2
      this%connectedCells(this%nrOfBoundaryCells)%cell%model => model2
    else if (associated(model2, this%model)) then
      this%boundaryCells(this%nrOfBoundaryCells)%cell%index = idx2
      this%boundaryCells(this%nrOfBoundaryCells)%cell%model => this%model

      this%connectedCells(this%nrOfBoundaryCells)%cell%index = idx1
      this%connectedCells(this%nrOfBoundaryCells)%cell%model => model1
    else
      write(*,*) 'Error: unable to connect cells outside the model'
      call ustop()
    end if
  
  end subroutine connectCell
  
  ! this is called for two models of same type that are connected through an
  ! exchange, need this for global topology
  ! NOTE: assumption here is only 1 exchange exists between any two models,
  ! can we do that??
  subroutine addModelLink(this, connEx)
    class(GridConnectionType), intent(inout)  :: this
    class(DisConnExchangeType), pointer     :: connEx
    ! local
        
    call this%connectModels(this%modelWithNbrs, connEx, this%extStencilDepth)
    
  end subroutine addModelLink
  
  recursive subroutine connectModels(this, modelNbrs, connEx, depth)
    class(GridConnectionType), intent(inout)   :: this
    class(ModelWithNbrsType), intent(inout) :: modelNbrs
    class(DisConnExchangeType), pointer     :: connEx
    integer(I4B)                            :: depth
    ! local
    integer(I4B) :: inbr, newDepth
    class(NumericalModelType), pointer      :: neighborModel
    class(*), pointer :: exObjPtr
    procedure(isEqualIface), pointer :: areEqualMethod
    
    if (depth < 1) then
      return
    end if
    
    neighborModel => null()    
    
    ! is it a direct neighbor:
    if (associated(modelNbrs%model, connEx%model1)) then
      neighborModel => connEx%model2
    else if (associated(modelNbrs%model, connEx%model2)) then
      neighborModel => connEx%model1
    end if
    
    ! and/or maybe its connected to one of the neighbors:
    newDepth = depth - 1
    do inbr = 1, modelNbrs%nrOfNbrs
      call this%connectModels(modelNbrs%neighbors(inbr), connEx, newDepth)
    end do
    
    ! do not add until here, after the recursion, to prevent 
    ! back-and-forth connecting of models...
    if (associated(neighborModel)) then           
      if (.not. associated(modelNbrs%neighbors)) then
        allocate(modelNbrs%neighbors(MaxNeighbors))
        modelNbrs%nrOfNbrs = 0
      end if
      modelNbrs%neighbors(modelNbrs%nrOfNbrs + 1)%model => neighborModel
      modelNbrs%nrOfNbrs = modelNbrs%nrOfNbrs + 1
      
      ! add to array of all neighbors
      call this%addToRegionalModels(neighborModel)
      
      ! add to list of exchanges
      exObjPtr => connEx
      areEqualMethod => arePointersEqual
      if (.not. this%exchanges%Contains(exObjPtr, areEqualMethod)) then
        call AddDisConnExchangeToList(this%exchanges, connEx)
      end if
    end if
    
  end subroutine connectModels
  
  subroutine addToRegionalModels(this, modelToAdd)
    class(GridConnectionType), intent(inout) :: this  
    class(NumericalModelType), pointer    :: modelToAdd
    ! local
    class(*), pointer :: mPtr    
    procedure(isEqualIface), pointer :: areEqualMethod

    mPtr => modelToAdd
    areEqualMethod => arePointersEqual
    if (.not. this%regionalModels%Contains(mPtr, areEqualMethod)) then
      call AddNumericalModelToList(this%regionalModels, modelToAdd)
    end if
    
  end subroutine addToRegionalModels

  ! build the connection topology to deal with neighbors-of-neighbors
  subroutine extendConnection(this)    
    class(GridConnectionType), intent(inout) :: this 
    ! local 
    integer(I4B) :: remoteDepth, localDepth
    integer(I4B) :: icell
    integer(I4B) :: imod, regionSize, offset
    class(NumericalModelType), pointer :: numModel
   
    ! we need (stencildepth-1) extra cells for the interior
    remoteDepth = this%extStencilDepth
    localDepth = 2*this%intStencilDepth - 1
    
    ! first add the neighbors for the interior, localOnly because 
    ! connections crossing model boundary will be added anyway
    do icell = 1, this%nrOfBoundaryCells
      call this%addNeighbors(this%boundaryCells(icell), localDepth, this%connectedCells(icell)%cell, local=.true.)
    end do
    ! and for the exterior
    do icell = 1, this%nrOfBoundaryCells
      call this%addNeighbors(this%connectedCells(icell), remoteDepth, this%boundaryCells(icell)%cell)
    end do
    
    ! set up mapping for the region (models participating in interface model grid)
    allocate(this%regionalModelOffset(this%regionalModels%Count()))
    regionSize = 0
    offset = 0
    do imod = 1, this%regionalModels%Count()
      numModel => GetNumericalModelFromList(this%regionalModels, imod)
      regionSize = regionSize + numModel%dis%nodes
      this%regionalModelOffset(imod) = offset
      offset = offset + numModel%dis%nodes
    end do
    ! init to -1, meaning 'interface index was not assigned yet'
    allocate(this%regionalToInterfaceIdxMap(regionSize))
    this%regionalToInterfaceIdxMap = -1
    
    call this%buildConnections()
    
  end subroutine extendConnection
    
  ! routine for finding neighbors-of-neighbors, recursively
  recursive subroutine addNeighbors(this, cellNbrs, depth, mask, local)
    use SimModule, only: ustop
    class(GridConnectionType), intent(inout)  :: this
    type(CellWithNbrsType), intent(inout)     :: cellNbrs    
    integer(I4B), intent(inout)               :: depth
    type(GlobalCellType), optional            :: mask
    logical, optional                         :: local ! controls whether only local (within the same model) neighbors are added
    ! local
    integer(I4B)                              :: nbrIdx, ipos, inbr
    type(ConnectionsType), pointer            :: conn
    integer(I4B)                              :: newDepth
    type(ModelWithNbrsType), pointer          :: modelWithNbrs
    logical                                   :: localOnly
    
    if (.not. present(local)) then
      localOnly = .false. ! default
    else
      localOnly = local
    end if
    
    ! if depth == 1, then we are not adding neighbors but use
    ! the boundary and connected cell only
    if (depth < 2) then
      return
    end if
    newDepth = depth - 1
    
    conn => cellNbrs%cell%model%dis%con
    
    ! find neighbors local to this cell by looping through grid connections
    do ipos=conn%ia(cellNbrs%cell%index) + 1, conn%ia(cellNbrs%cell%index+1) - 1
      nbrIdx = conn%ja(ipos)
      call this%addNeighborCell(cellNbrs, nbrIdx, cellNbrs%cell%model, mask)
    end do
        
    ! find and add remote nbr (from a different model, and
    ! not going back into the main model)
    if (.not. localOnly) then
      call this%getModelWithNbrs(cellNbrs%cell%model, modelWithNbrs)
      call this%addRemoteNeighbors(cellNbrs, modelWithNbrs, mask)
    end if
    
    ! now find nbr-of-nbr
    do inbr=1, cellNbrs%nrOfNbrs
      call this%addNeighbors(cellNbrs%neighbors(inbr), newDepth, cellNbrs%cell, local)
    end do
    
  end subroutine addNeighbors
  
  ! looks whether this models has any neighbors, then finds
  ! the exchange and from the n-m pairs in there, it adds
  ! the neighboring cells
  subroutine addRemoteNeighbors(this, cellNbrs, modelWithNbrs, mask)
    class(GridConnectionType), intent(inout)      :: this
    type(CellWithNbrsType), intent(inout)         :: cellNbrs
    type(ModelWithNbrsType), intent(in), pointer  :: modelWithNbrs
    type(GlobalCellType), optional                :: mask
    ! local
    integer(I4B) :: inbr, iexg
    type(DisConnExchangeType), pointer :: connEx
    
    do inbr = 1, modelWithNbrs%nrOfNbrs
      connEx => this%getExchangeData(cellNbrs%cell%model, modelWithNbrs%neighbors(inbr)%model)
      if (.not. associated(connEx)) then
        write(*,*) 'Error finding exchange data for models, should never happen: terminating...'
        call ustop()
      end if
      
      ! loop over n-m links in the exchange
      if (associated(cellNbrs%cell%model, connEx%model1)) then
        ! we were outside, no need to go back into this%model: 
        ! those cells will be added anyhow
        if (associated(connEx%model2, this%model)) cycle 
        do iexg = 1, connEx%nexg
          if (connEx%nodem1(iexg) == cellNbrs%cell%index) then
            ! we have a link, now add foreign neighbor
            call this%addNeighborCell(cellNbrs, connEx%nodem2(iexg), connEx%model2, mask)
          end if
        end do
      end if
      ! and the reverse
      if (associated(cellNbrs%cell%model, connEx%model2)) then
        ! we were outside, no need to go back into this%model: 
        ! those cells will be added anyhow
        if (associated(connEx%model1, this%model)) cycle
        do iexg = 1, connEx%nexg
          if (connEx%nodem2(iexg) == cellNbrs%cell%index) then
            ! we have a link, now add foreign neighbor
            call this%addNeighborCell(cellNbrs, connEx%nodem1(iexg), connEx%model1, mask)
          end if
        end do
      end if
      
    end do
    
  end subroutine addRemoteNeighbors
  
  ! returns the numerical exchange data for the pair of models
  function getExchangeData(this, model1, model2) result(connEx)
    class(GridConnectionType), intent(inout)        :: this
    class(NumericalModelType), pointer, intent(in)  :: model1, model2
    type(DisConnExchangeType), pointer            :: connEx
    ! local
    type(DisConnExchangeType), pointer            :: connExLocal    
    integer(I4B) :: i
    
    connEx => null()
        
    do i = 1, this%exchanges%Count()
      connExLocal => GetDisConnExchangeFromList(this%exchanges, i)
      if (associated(model1, connExLocal%model1) .and. associated(model2, connExLocal%model2)) then
        connEx => connExLocal
        exit
      end if
      if (associated(model1, connExLocal%model2) .and. associated(model2, connExLocal%model1)) then
        connEx => connExLocal
        exit
      end if
    end do
    
  end function getExchangeData
  
  subroutine getModelWithNbrs(this, model, modelWithNbr)
    class(GridConnectionType), intent(in)           :: this
    class(NumericalModelType), pointer, intent(in)  :: model
    type(ModelWithNbrsType), pointer, intent(out)   :: modelWithNbr
    ! local
    integer(I4B) :: i
    
    ! traverse the tree, currently two deep but this can be made recursive
    if (associated(model, this%modelWithNbrs%model)) then    
      modelWithNbr => this%modelWithNbrs
    else
      do i = 1, this%modelWithNbrs%nrOfNbrs
        if (associated(model, this%modelWithNbrs%neighbors(i)%model)) then  
          modelWithNbr => this%modelWithNbrs%neighbors(i)
        end if
      end do
    end if
    
  end subroutine getModelWithNbrs
  
  subroutine addNeighborCell(this, cellNbrs, newNbrIdx, nbrModel, mask)    
    class(GridConnectionType), intent(in) :: this
    type(CellWithNbrsType), intent(inout) :: cellNbrs 
    integer(I4B), intent(in)              :: newNbrIdx
    class(NumericalModelType), pointer    :: nbrModel
    type(GlobalCellType), optional        :: mask
    ! local
    integer(I4B) :: nbrCnt
    
    if (present(mask)) then
      if (newNbrIdx == mask%index .and. associated(nbrModel, mask%model)) then
        return
      end if
    end if
    
    ! TODO_MJR: dynamic memory
    if (.not. associated(cellNbrs%neighbors)) then
      allocate(cellNbrs%neighbors(MaxNeighbors))
    end if
    
    nbrCnt = cellNbrs%nrOfNbrs
    if (nbrCnt + 1 > MaxNeighbors) then
       write(*,*) 'Error extending connections in GridConnection, max. nr. of neighbors exceeded: terminating...'
       call ustop()  
    end if
        
    cellNbrs%neighbors(nbrCnt + 1)%cell%index = newNbrIdx
    cellNbrs%neighbors(nbrCnt + 1)%cell%model => nbrModel  
    cellNbrs%nrOfNbrs = nbrCnt + 1
  end subroutine
  
  ! builds a sparse matrix holding all cell connections,
  ! with new indices, and stores the mapping to the global ids
  subroutine buildConnections(this)
    class(GridConnectionType), intent(inout) :: this 
    ! local
    integer(I4B) :: icell, iconn
    integer(I4B), dimension(:), allocatable :: nnz
    type(SparseMatrix), pointer :: sparse     
    integer(I4B) :: ierror    
    type(ConnectionsType), pointer :: conn
            
    ! generate interface cell indices, recursively and build mapping. 
    ! Start with boundaryCells, this way the internal interface nodes 
    ! will be numbered contiguously
    this%indexCount = 0
    do icell = 1, this%nrOfBoundaryCells
      call this%registerInterfaceCells(this%boundaryCells(icell))
    end do
    do icell = 1, this%nrOfBoundaryCells
      call this%registerInterfaceCells(this%connectedCells(icell))
    end do
    this%nrOfCells = this%indexCount
    
    ! to map the interface index to global coordinates
    allocate(this%idxToGlobal(this%nrOfCells))
    call mem_allocate(this%idxToGlobalIdx, this%nrOfCells, 'IDXTOGLOBALIDX', this%memoryPath)
    
    ! create sparse, to temporarily hold connections
    allocate(sparse)
    allocate(nnz(this%nrOfCells))
    nnz = MaxNeighbors+1
    call sparse%init(this%nrOfCells, this%nrOfCells, nnz)
    
    ! now (recursively) add the connections for the boundary cells
    ! start with the primary connection (n-m from the exchanges)
    call this%makePrimaryConnections(sparse)   
    ! then into own domain
    do icell = 1, this%nrOfBoundaryCells
      call this%connectNeighborCells(this%boundaryCells(icell), sparse)
    end do
    ! and same for the neighbors of connected cells
    do icell = 1, this%nrOfBoundaryCells
      call this%connectNeighborCells(this%connectedCells(icell), sparse)
    end do
    
     ! create connections from sparse, and fill
    allocate(this%connections)
    conn => this%connections
    call conn%allocate_scalars(this%memoryPath)
    conn%nodes = this%nrOfCells
    conn%nja = sparse%nnz
    conn%njas = (conn%nja -  conn%nodes) / 2
    call conn%allocate_arrays()
    do iconn = 1, conn%njas
      conn%anglex(iconn) = -999.
    end do

    call sparse%filliaja(conn%ia, conn%ja, ierror)  
    if (ierror /= 0) then
      write(*,*) 'Error filling ia/ja connections in GridConnection: terminating...'
      call ustop()
    end if    
    call fillisym(conn%nodes, conn%nja, conn%ia, conn%ja, conn%isym)
    call filljas(conn%nodes, conn%nja, conn%ia, conn%ja, conn%isym, conn%jas)  
    ! and done with it
    call sparse%destroy()
    
    ! fill connection data from models and exchanges
    call this%fillConnectionDataInternal()    
    call this%fillConnectionDataFromExchanges()
    
    ! set the masks on connections
    call this%createConnectionMask()
    
  end subroutine buildConnections 
 
  recursive subroutine registerInterfaceCells(this, cellWithNbrs)
    class(GridConnectionType), intent(inout) :: this
    type(CellWithNbrsType)                   :: cellWithNbrs
    ! local
    integer(I4B) :: offset, inbr
    integer(I4B) :: regionIdx  ! unique idx in the region (all connected models)
    integer(I4B) :: ifaceIdx   ! unique idx in the interface grid
    
    offset = this%getRegionalModelOffset(cellWithNbrs%cell%model)
    regionIdx = offset + cellWithNbrs%cell%index
    ifaceIdx = this%getInterfaceIndex(cellWithNbrs%cell)
    if (ifaceIdx == -1) then
      this%indexCount = this%indexCount + 1
      ifaceIdx = this%indexCount
      this%regionalToInterfaceIdxMap(regionIdx) = ifaceIdx
    end if
    
    ! and also for its neighbors
    do inbr = 1, cellWithNbrs%nrOfNbrs
      call this%registerInterfaceCells(cellWithNbrs%neighbors(inbr))
    end do
      
  end subroutine registerInterfaceCells
  
  subroutine makePrimaryConnections(this, sparse)
    class(GridConnectionType), intent(inout)  :: this
    type(SparseMatrix), pointer               :: sparse
    ! local
    integer(I4B) :: icell
    integer(I4B) :: ifaceIdx, ifaceIdxNbr
    
    do icell = 1, this%nrOfBoundaryCells  
      ifaceIdx = this%getInterfaceIndex(this%boundaryCells(icell)%cell)
      ifaceIdxNbr = this%getInterfaceIndex(this%connectedCells(icell)%cell)
      
      ! set mapping
      this%idxToGlobal(ifaceIdx) = this%boundaryCells(icell)%cell
      this%idxToGlobal(ifaceIdxNbr) = this%connectedCells(icell)%cell
      
      ! add diagonals to sparse
      call sparse%addconnection(ifaceIdx, ifaceIdx, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdxNbr, 1)
      
      ! and cross terms
      call sparse%addconnection(ifaceIdx, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdx, 1)
    end do
    
  end subroutine makePrimaryConnections
  
  recursive subroutine connectNeighborCells(this, cell, sparse)
    class(GridConnectionType), intent(inout)  :: this
    type(CellWithNbrsType)                    :: cell
    type(SparseMatrix), pointer               :: sparse
    ! local
    integer(I4B) :: ifaceIdx, ifaceIdxNbr    ! unique idx in the interface grid
    integer(I4B) :: inbr
    
    ifaceIdx = this%getInterfaceIndex(cell%cell)   
    do inbr = 1, cell%nrOfNbrs
      ifaceIdxNbr = this%getInterfaceIndex(cell%neighbors(inbr)%cell)      
      this%idxToGlobal(ifaceIdxNbr) = cell%neighbors(inbr)%cell      
      
      call sparse%addconnection(ifaceIdxNbr, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdx, ifaceIdxNbr, 1)
      call sparse%addconnection(ifaceIdxNbr, ifaceIdx, 1)
      
      ! recurse
      call this%connectNeighborCells(cell%neighbors(inbr), sparse)
    end do
    
  end subroutine connectNeighborCells
  
  ! fill ihc, cl1, cl2, hwva, anglex (with size=njas) for all internal connections
  subroutine fillConnectionDataInternal(this)
    use ConstantsModule, only: DPI, DTWOPI
    class(GridConnectionType), intent(inout)  :: this
    ! local
    type(ConnectionsType), pointer :: conn, connOrig      
    integer(I4B) :: n, m, ipos, isym, iposOrig, isymOrig
    type(GlobalCellType), pointer :: ncell, mcell
    
    conn => this%connections
    
    do n = 1, conn%nodes
      do ipos=conn%ia(n)+1, conn%ia(n+1)-1
        m = conn%ja(ipos)
        if (n > m) cycle
        
        isym = conn%jas(ipos)
        ncell => this%idxToGlobal(n)
        mcell => this%idxToGlobal(m)
        if (associated(ncell%model, mcell%model)) then
          ! within same model, straight copy
          connOrig => ncell%model%dis%con
          iposOrig = connOrig%getjaindex(ncell%index, mcell%index)   
          if (iposOrig == 0) then
            ! periodic boundary conditions can add connections between cells in 
            ! the same model, but they are dealt with through the exchange data
            if (this%isPeriodic(ncell%index, mcell%index)) cycle
            
            ! this should not be possible
            write(*,*) 'Error: cannot find cell connection in model grid'
            call ustop() 
          end if
          
          isymOrig = connOrig%jas(iposOrig)
          conn%hwva(isym) = connOrig%hwva(isymOrig)
          conn%ihc(isym) = connOrig%ihc(isymOrig)
          if (ncell%index < mcell%index) then
            conn%cl1(isym) = connOrig%cl1(isymOrig)
            conn%cl2(isym) = connOrig%cl2(isymOrig)
            conn%anglex(isym) = connOrig%anglex(isymOrig)
          else
            conn%cl1(isym) = connOrig%cl2(isymOrig)
            conn%cl2(isym) = connOrig%cl1(isymOrig)
            conn%anglex(isym) = mod(connOrig%anglex(isymOrig) + DPI, DTWOPI)
          end if
        end if
      end do
    end do
  end subroutine fillConnectionDataInternal
  
  ! fill connection data for all exchanges, using symmetry
  subroutine fillConnectionDataFromExchanges(this)
    use ConstantsModule, only: DPI, DTWOPI, DPIO180
    use ArrayHandlersModule, only: ifind    
    class(GridConnectionType), intent(inout)  :: this
    ! local
    integer(I4B) :: inx, iexg, ivalAngldegx
    integer(I4B) :: ipos, isym
    integer(I4B) :: nOffset, mOffset, nIfaceIdx, mIfaceIdx
    class(DisConnExchangeType), pointer :: connEx
    type(ConnectionsType), pointer :: conn
    
    conn => this%connections
    
    do inx = 1, this%exchanges%Count()
      connEx => GetDisConnExchangeFromList(this%exchanges, inx) 
       
      ivalAngldegx = -1
      if (connEx%naux > 0) then
        ivalAngldegx = ifind(connEx%auxname, 'ANGLDEGX')
        if (ivalAngldegx > 0) then
          conn%ianglex = 1
        end if
      end if
      
      nOffset = this%getRegionalModelOffset(connEx%model1)
      mOffset = this%getRegionalModelOffset(connEx%model2)
      do iexg = 1, connEx%nexg
        nIfaceIdx = this%regionalToInterfaceIdxMap(noffset + connEx%nodem1(iexg))
        mIfaceIdx = this%regionalToInterfaceIdxMap(moffset + connEx%nodem2(iexg))
        ! not all nodes from the exchanges are part of the interface grid 
        ! (think of exchanges between neigboring models, and their neighbors)
        if (nIFaceIdx == -1 .or. mIFaceIdx == -1) then
          cycle
        end if
        
        ipos = conn%getjaindex(nIfaceIdx, mIfaceIdx)        
        ! (see prev. remark) sometimes the cells are in the interface grid, 
        ! but the connection isn't. This can happen for leaf nodes of the grid.
        if (ipos == 0) then
          ! no match, safely cycle
          cycle
        end if        
        isym = conn%jas(ipos)
          
        ! note: cl1 equals L_nm: the length from cell n to the shared
        ! face with cell m (and cl2 analogously for L_mn)
        if (nIfaceIdx < mIfaceIdx) then
          conn%cl1(isym) = connEx%cl1(iexg)
          conn%cl2(isym) = connEx%cl2(iexg)
          if (ivalAngldegx > 0) then
            conn%anglex(isym) = connEx%auxvar(ivalAngldegx,iexg) * DPIO180
          end if
        else
          conn%cl1(isym) = connEx%cl2(iexg)
          conn%cl2(isym) = connEx%cl1(iexg)
          if (ivalAngldegx > 0) then
            conn%anglex(isym) = mod(connEx%auxvar(ivalAngldegx,iexg) + 180.0_DP, 360.0_DP) * DPIO180
          end if
        end if
        conn%hwva(isym) = connEx%hwva(iexg)
        conn%ihc(isym) = connEx%ihc(iexg)
                         
      end do        
    end do
    
  end subroutine fillConnectionDataFromExchanges  
  
  !> @brief Create the connection masks
  !!
  !! The level indicates the nr of connections away from
  !! the remote neighbor, the diagonal term holds the negated
  !< value of their nearest connection
  subroutine createConnectionMask(this)
    class(GridConnectionType), intent(inout) :: this !< instance of this grid connection
    ! local
    integer(I4B) :: icell, inbr, n, ipos
    integer(I4B) :: level, newMask
    type(CellWithNbrsType), pointer :: cell, nbrCell
    
    ! set all masks to zero to begin with
    do ipos = 1, this%connections%nja
      call this%connections%set_mask(ipos, 0)  
    end do
    
    ! remote connections remain masked
    ! now set mask for exchange connections (level == 1)
    do icell = 1, this%nrOfBoundaryCells  
      call this%setMaskOnConnection(this%boundaryCells(icell), this%connectedCells(icell), 1)
    end do
    
    ! now extend mask recursively into the internal domain (level > 1)
    do icell = 1, this%nrOfBoundaryCells
      cell => this%boundaryCells(icell)      
      do inbr = 1, cell%nrOfNbrs
        nbrCell => this%boundaryCells(icell)%neighbors(inbr)
        level = 2 ! this is incremented within the recursion
        call this%maskConnections(this%boundaryCells(icell), this%boundaryCells(icell)%neighbors(inbr), level)
      end do      
    end do
    
    ! set normalized mask:
    ! =1 for links with connectivity <= interior stencil depth
    ! =0 otherwise
    do n = 1, this%connections%nodes 
      ! set diagonals to zero
      call this%connections%set_mask(this%connections%ia(n), 0)
      
      do ipos = this%connections%ia(n) + 1, this%connections%ia(n + 1) - 1
        newMask = 0
        if (this%connections%mask(ipos) > 0) then
          if (this%connections%mask(ipos) < this%intStencilDepth + 1) then
            newMask = 1
          end if
        end if        
        ! set mask on off-diag
        call this%connections%set_mask(ipos, newMask)
      end do      
    end do
    
  end subroutine createConnectionMask
  
  ! recursively mask connections, increasing the level as we go
  recursive subroutine maskConnections(this, cell, nbrCell, level)
    class(GridConnectionType), intent(inout) :: this
    type(CellWithNbrsType), intent(inout) :: cell, nbrCell
    integer(I4B), intent(in) :: level
    ! local
    integer(I4B) :: inbr, newLevel
    
    ! this will set a mask on both diagonal, and both cross terms
    call this%setMaskOnConnection(cell, nbrCell, level)
    call this%setMaskOnConnection(nbrCell, cell, level)
    
    ! recurse on nbrs-of-nbrs
    newLevel = level + 1
    do inbr = 1, nbrCell%nrOfNbrs        
      call this%maskConnections(nbrCell, nbrCell%neighbors(inbr), newLevel)
    end do
    
  end subroutine maskConnections
  
  ! mask the connection from a cell to its neighbor cell,
  ! (and not the transposed!)
  subroutine setMaskOnConnection(this, cell, nbrCell, level)
    class(GridConnectionType), intent(inout) :: this
    type(CellWithNbrsType), intent(inout) :: cell, nbrCell
    integer(I4B), intent(in) :: level
    ! local
    integer(I4B) :: ifaceIdx, ifaceIdxNbr
    integer(I4B) :: iposdiag, ipos
    integer(I4B) :: currentLevel
    
    ifaceIdx = this%getInterfaceIndex(cell%cell)
    ifaceIdxNbr = this%getInterfaceIndex(nbrCell%cell)
      
    ! diagonal
    iposdiag = this%connections%getjaindex(ifaceIdx, ifaceIdx)
    currentLevel = this%connections%mask(iposdiag)
    if (currentLevel == 0 .or. level < currentLevel) then
      call this%connections%set_mask(iposdiag, level)
    end if
    ! cross term
    ipos = this%connections%getjaindex(ifaceIdx, ifaceIdxNbr)
    currentLevel = this%connections%mask(ipos)
    if (currentLevel == 0 .or. level < currentLevel) then
      call this%connections%set_mask(ipos, level)
    end if
    
  end subroutine setMaskOnConnection
  
  ! helper routine to convert global cell to index in interface grid
  function getInterfaceIndex(this, cell) result(ifaceIdx)
    class(GridConnectionType), intent(inout)  :: this
    type(GlobalCellType), intent(in)          :: cell
    integer(I4B)                              :: ifaceIdx
    ! local
    integer(I4B) :: offset, regionIdx
    
    offset = this%getRegionalModelOffset(cell%model)
    regionIdx = offset + cell%index
    ifaceIdx = this%regionalToInterfaceIdxMap(regionIdx)
  end function getInterfaceIndex
  
  function getRegionalModelOffset(this, model) result(offset)
    class(GridConnectionType), intent(inout) :: this
    class(NumericalModelType), pointer       :: model
    integer(I4B)                             :: offset
    ! local
    integer(I4B) :: im
    class(NumericalModelType), pointer :: modelInList
    offset = 0
    do im = 1, this%regionalModels%Count()
       modelInList => GetNumericalModelFromList(this%regionalModels, im)
       if (associated(model, modelInList)) then
         offset = this%regionalModelOffset(im)
         return
       end if
    end do
    
  end function getRegionalModelOffset
  
  subroutine allocateScalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(GridConnectionType) :: this
      
    call mem_allocate(this%linkCapacity, 'LINKCAP', this%memoryPath)
    call mem_allocate(this%nrOfBoundaryCells, 'NRBNDCELLS', this%memoryPath)
    call mem_allocate(this%indexCount, 'IDXCOUNT', this%memoryPath)
    call mem_allocate(this%nrOfCells, 'NRCELLS', this%memoryPath)
    
  end subroutine allocateScalars
  
  subroutine allocateArrays(this, nConns)
    class(GridConnectionType), intent(in) :: this
    integer(I4B) :: nConns
    
  end subroutine allocateArrays  
  
  subroutine deallocate(this)
  use MemoryManagerModule, only: mem_deallocate
    class(GridConnectionType) :: this
    
    call mem_deallocate(this%linkCapacity)
    call mem_deallocate(this%nrOfBoundaryCells)
    call mem_deallocate(this%indexCount)
    call mem_deallocate(this%nrOfCells)

    ! arrays
    deallocate(this%modelWithNbrs)
    deallocate(this%boundaryCells)
    deallocate(this%connectedCells)

    call mem_deallocate(this%idxToGlobalIdx)
    
  end subroutine deallocate
  
  ! test if the connection between node n and m, who are both 
  ! assumed to be part of this%model, is periodic
  function isPeriodic(this, n, m) result(periodic)
    class(GridConnectionType), intent(in) :: this
    integer(I4B) :: n, m
    logical :: periodic
    ! local
    integer(I4B) :: icell
    
    periodic = .false.    
    do icell = 1, this%nrOfCells
      if (.not. associated(this%boundaryCells(icell)%cell%model, this%connectedCells(icell)%cell%model)) cycle
      
      ! one way
      if (this%boundaryCells(icell)%cell%index == n) then
        if (this%connectedCells(icell)%cell%index == m) then
          periodic = .true.
          return
        end if
      end if
      ! or the other
      if (this%boundaryCells(icell)%cell%index == m) then
        if (this%connectedCells(icell)%cell%index == n) then
          periodic = .true.
          return
        end if
      end if
      
    end do
    
  end function
  
end module GridConnectionModule
