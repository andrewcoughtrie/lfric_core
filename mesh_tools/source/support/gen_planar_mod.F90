!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief   Module to define the gen_planar_type
!> @details This type is a subclass of the ugrid_generator_type. It describes
!>          a planar mesh format suitable for storage as a ugrid file.
!>          All required connectivity is calculated via the generate method
!>          and made available to the ugrid writer.
!>
!-------------------------------------------------------------------------------
module gen_planar_mod
!-------------------------------------------------------------------------------

  use calc_global_cell_map_mod,       only: calc_global_cell_map
  use constants_mod,                  only: r_def, i_def, l_def, str_def, &
                                            str_long, imdi
  use global_mesh_map_collection_mod, only: global_mesh_map_collection_type
  use global_mesh_map_mod,            only: generate_global_mesh_map_id
  use log_mod,                        only: log_event, log_scratch_space, &
                                            LOG_LEVEL_ERROR, LOG_LEVEL_INFO
  use reference_element_mod,          only: reference_element_type, &
                                            reference_cube_type,    &
                                            W, S, E, N,             &
                                            SWB, SEB, NWB, NEB
  use ugrid_generator_mod,            only: ugrid_generator_type

  implicit none

  private

  public :: gen_planar_type
  public :: NON_PERIODIC_ID

  ! Mesh Vertex directions: local aliases for reference_element_mod values
  integer(i_def), parameter :: NW = NWB
  integer(i_def), parameter :: NE = NEB
  integer(i_def), parameter :: SE = SEB
  integer(i_def), parameter :: SW = SWB

  integer(i_def), parameter :: OPEN     =  100
  integer(i_def), parameter :: CLOSED   = -100
  integer(i_def), parameter :: PERIODIC =  101

  integer(i_def), parameter :: NON_PERIODIC_ID = -1

  integer(i_def), parameter :: NPANELS = 1

  integer(i_def), parameter :: NBORDERS = 4

  ! Prefix for error messages
  character(len=*),   parameter :: PREFIX = "[Planar Mesh] "
  character(str_def), parameter :: MESH_CLASS = "plane"

  ! Flag to print out mesh data for debugging purposes
  logical(l_def),     parameter :: DEBUG = .false.

  type, extends(ugrid_generator_type) :: gen_planar_type

    private

    character(str_def)  :: mesh_name
    character(str_def)  :: mesh_class
    character(str_def)  :: coord_units_x
    character(str_def)  :: coord_units_y
    character(str_long) :: constructor_inputs
    integer(i_def)      :: edge_cells_x, edge_cells_y
    real(r_def)         :: dx, dy

    integer(i_def)      :: npanels
    integer(i_def)      :: nmaps

    integer(i_def)      :: n_nodes
    integer(i_def)      :: n_edges
    integer(i_def)      :: n_faces
    logical(l_def)      :: periodic_x
    logical(l_def)      :: periodic_y

    integer(i_def), allocatable :: north_cells(:)
    integer(i_def), allocatable :: east_cells(:)
    integer(i_def), allocatable :: south_cells(:)
    integer(i_def), allocatable :: west_cells(:)

    character(str_def), allocatable :: target_mesh_names(:)
    integer(i_def),     allocatable :: target_edge_cells_x(:)
    integer(i_def),     allocatable :: target_edge_cells_y(:)
    type(global_mesh_map_collection_type), allocatable :: global_mesh_maps

    integer(i_def), allocatable :: cell_next(:,:)     ! (4, edge_cells_x*edge_cells_y)
    integer(i_def), allocatable :: verts_on_cell(:,:) ! (4, edge_cells_x*edge_cells_y)
    integer(i_def), allocatable :: edges_on_cell(:,:) ! (4, edge_cells_x*edge_cells_y)
    integer(i_def), allocatable :: verts_on_edge(:,:) ! (2, edge_cells_x*edge_cells_y)
    real(r_def),    allocatable :: vert_coords(:,:)   ! (2, edge_cells_x*edge_cells_y)
    real(r_def),    allocatable :: cell_coords(:,:)   ! (2, edge_cells_x*edge_cells_y)

    ! Hold variables from the reference element.
    ! Done because the available Cray compiler has internal compiler errors
    ! when attempting to include the reference_element_type.
    integer(i_def) :: nodes_per_face
    integer(i_def) :: edges_per_face
    integer(i_def) :: nodes_per_edge

  contains

    procedure :: generate
    procedure :: get_metadata
    procedure :: get_dimensions
    procedure :: get_coordinates
    procedure :: get_connectivity
    procedure :: get_global_mesh_maps
    procedure :: write_mesh

    procedure :: clear

    ! Objects should have a finaliser, however the inclusion of a finaliser
    ! in this object causes the current version of the Cray compiler (8.4.3)
    ! to fail to compile. Later versions appear to have no issue with this
    ! code.
    !
    ! It's not clear as to why the Cray compiler fails internally for the
    ! this object, while passing others with similar constructs.
    ! This finaliser code should be uncommented when we are using a later
    ! version of the Cray compiler, such as (8.7.0). (LFRic ticket #1989)
    !
    ! final     :: gen_planar_final

  end type gen_planar_type

!-------------------------------------------------------------------------------
  interface gen_planar_type
    module procedure gen_planar_constructor
  end interface gen_planar_type
!-------------------------------------------------------------------------------

contains

!-------------------------------------------------------------------------------
!> @brief   Constructor for gen_planar_type
!> @details Accepts inputs to configure a planar mesh. Intergrid mesh maps
!>          may be are optionally specified with varying numbers of grid cells.
!>
!>          Note: All target meshes specified for mapping to must be an integer
!>                factor of the main mesh, e.g. edge_cells_x=3;
!>                target_edge_cells_x=[6,9,12,15]
!>
!> @param[in] mesh_name       Name of this mesh topology
!> @param[in] edge_cells_x    Number of cells in planar mesh x-axis
!> @param[in] edge_cells_y    Number of cells in planar mesh y-axis
!> @param[in] periodic_x      Logical for specifying periodicity in x-axis
!> @param[in] periodic_y      Logical for specifying periodicity in y-axis
!> @param[in] domain_x        Domain size in x-axis
!> @param[in] domain_y        Domain size in y-axis
!> @param[in] target_mesh_names [optional]
!>                            Names of mesh(es) to map to
!> @param[in] target_edge_cells_x [optional]
!>                            Number of cells in x axis of
!>                            target mesh(es) to map to
!> @param[in] target_edge_cells_y [optional]
!>                            Number of cells in y axis of
!>                            target mesh(es) to map to
!-------------------------------------------------------------------------------
function gen_planar_constructor( reference_element,          &
                                 mesh_name,                  &
                                 edge_cells_x, edge_cells_y, &
                                 periodic_x, periodic_y,     &
                                 domain_x, domain_y,         &
                                 target_mesh_names,          &
                                 target_edge_cells_x,        &
                                 target_edge_cells_y )       &
                            result( self )

  implicit none

  character(str_def), intent(in) :: mesh_name
  integer(i_def),     intent(in) :: edge_cells_x, edge_cells_y
  logical(l_def),     intent(in) :: periodic_x, periodic_y
  real(r_def),        intent(in) :: domain_x, domain_y

  character(str_def), optional, intent(in) :: target_mesh_names(:)
  integer(i_def),     optional, intent(in) :: target_edge_cells_x(:)
  integer(i_def),     optional, intent(in) :: target_edge_cells_y(:)

  type( gen_planar_type ) :: self

  class(reference_element_type), intent(in) :: reference_element

  character(str_long) :: target_mesh_names_str
  character(str_long) :: target_edge_cells_x_str
  character(str_long) :: target_edge_cells_y_str
  character(str_def)  :: rchar1
  character(str_def)  :: rchar2
  character(str_def)  :: lchar1
  character(str_def)  :: lchar2
  integer(i_def)      :: nodes_x
  integer(i_def)      :: nodes_y
  integer(i_def)      :: i
  integer(i_def)      :: n_edges
  integer(i_def)      :: remainder = 0_i_def

  integer(i_def) :: min_cells_x
  integer(i_def) :: min_cells_y

  ! At present this mesh generator stratedy only supports 2d quad elements
  ! i.e. cube elements
  select type(reference_element)
    type is (reference_cube_type)
      ! Carry on
    class default
      call log_event( PREFIX//'Un-supported reference element type. ' // &
                      'Use reference_cube_type.', LOG_LEVEL_ERROR )
  end select

  self%nodes_per_face = reference_element%get_number_2d_vertices()
  self%edges_per_face = reference_element%get_number_2d_edges()
  self%nodes_per_edge = reference_element%get_number_verts_per_edge()

  min_cells_x = 1
  min_cells_y = 1
  if (periodic_x) min_cells_x = 2
  if (periodic_y) min_cells_y = 2

  if ( edge_cells_x < min_cells_x .or. &
       edge_cells_y < min_cells_y ) then
    call log_event( PREFIX//"For chosen periodic bounds:.", LOG_LEVEL_INFO )
    write(log_scratch_space,'(A,I0)') PREFIX//'minimum edge_cells_x = ', min_cells_x
    call log_event( log_scratch_space, LOG_LEVEL_INFO )
    write(log_scratch_space,'(A,I0)') PREFIX//'minimum edge_cells_y = ', min_cells_y
    call log_event( log_scratch_space, LOG_LEVEL_INFO )
    call log_event( PREFIX//"Invalid dimension choices.", LOG_LEVEL_ERROR )
  end if


  self%mesh_name    = trim(mesh_name)
  self%mesh_class   = trim(MESH_CLASS)
  self%edge_cells_x = edge_cells_x
  self%edge_cells_y = edge_cells_y
  self%npanels      = NPANELS
  self%nmaps        = 0_i_def
  self%periodic_x   = periodic_x
  self%periodic_y   = periodic_y

  if (domain_x <= 0.0_r_def)                                       &
      call log_event( PREFIX//" x-domain argument must be > 0.0",  &
                      LOG_LEVEL_ERROR )

  if (domain_y <= 0.0_r_def)                                       &
      call log_event( PREFIX//" y-domain argument must be > 0.0.", &
                      LOG_LEVEL_ERROR )

  self%dx = domain_x / self%edge_cells_x
  self%dy = domain_y / self%edge_cells_y

  write(lchar1,'(L8)') periodic_x
  write(lchar2,'(L8)') periodic_y
  write(rchar1,'(F10.2)') domain_x
  write(rchar2,'(F10.2)') domain_y
  write(self%constructor_inputs,'(2(A,I0),(A))')   &
      'edge_cells_x=', self%edge_cells_x,    ';'// &
      'edge_cells_y=', self%edge_cells_y,    ';'// &
      'periodic_x='//trim(adjustl(lchar1))// ';'// &
      'periodic_y='//trim(adjustl(lchar2))// ';'// &
      'domain_x='//trim(adjustl(rchar1))//   ';'// &
      'domain_y='//trim(adjustl(rchar2))

  ! Initialise as a plane with cyclic boundaries
  nodes_x = self%edge_cells_x
  nodes_y = self%edge_cells_y
  n_edges = 2 * self%edge_cells_x * self%edge_cells_y

  ! Modify properties based on any non-periodic axes
  if (.not. self%periodic_x) then
    nodes_x = nodes_x + 1
    n_edges = n_edges + self%edge_cells_y
  end if

  if (.not. self%periodic_y) then
    nodes_y = nodes_y + 1
    n_edges = n_edges + self%edge_cells_x
  end if

  ! Update properties for the resulting object
  self%n_nodes = nodes_x * nodes_y
  self%n_edges = n_edges
  self%n_faces = self%edge_cells_x * self%edge_cells_y

  allocate(self%edges_on_cell(self%edges_per_face, self%n_faces))
  allocate(self%verts_on_edge(self%nodes_per_edge, self%n_edges))

  self%edges_on_cell = imdi
  self%verts_on_edge = imdi

  if ( present(target_edge_cells_x) .and. &
       present(target_edge_cells_y) .and. &
       present(target_mesh_names) ) then

    if ( size(target_edge_cells_x) == size(target_mesh_names) .and. &
         size(target_edge_cells_y) == size(target_mesh_names) ) then

      self%nmaps = size(target_mesh_names)
      allocate(self%target_mesh_names(self%nmaps))
      allocate(self%target_edge_cells_x(self%nmaps))
      allocate(self%target_edge_cells_y(self%nmaps))


      self%target_mesh_names(:)   = target_mesh_names(:)
      self%target_edge_cells_x(:) = target_edge_cells_x(:)
      self%target_edge_cells_y(:) = target_edge_cells_y(:)

      do i=1, self%nmaps

        ! Check that mesh is not being mapped to itself
        if (self%edge_cells_x == target_edge_cells_x(i) .and. &
            self%edge_cells_y == target_edge_cells_y(i)) then
          write(log_scratch_space, '(A)') &
               'Invalid target while attempting to map mesh to itself'
          call log_event( trim(log_scratch_space), LOG_LEVEL_ERROR )
        end if

        ! for x-axis
        if (target_edge_cells_x(i) < self%edge_cells_x) then
          remainder = mod(self%edge_cells_x, target_edge_cells_x(i))
          write(log_scratch_space,'(2(A,I0),A)')             &
              'Target edge_cells_x[',target_edge_cells_x(i), &
              '] must be a factor of source edge_cells_x[',  &
              self%edge_cells_x, ']'

        else if (target_edge_cells_x(i) > self%edge_cells_x) then
          remainder = mod(target_edge_cells_x(i), self%edge_cells_x)
          write(log_scratch_space,'(2(A,I0),A)')              &
               'Source edge_cells_x[',target_edge_cells_x(i), &
               '] must be a factor of target edge_cells_x[',  &
               self%edge_cells_x, ']'
        end if

        if (remainder == 0_i_def) then
          self%target_edge_cells_x(i) = target_edge_cells_x(i)
        else
          call log_event( trim(log_scratch_space), LOG_LEVEL_ERROR )
        end if

        ! for y-axis
        if (target_edge_cells_y(i) < self%edge_cells_y) then
          remainder = mod(self%edge_cells_y, target_edge_cells_y(i))
          write(log_scratch_space,'(2(A,I0),A)')               &
               'Target edge_cells_y[',target_edge_cells_y(i),  &
               '] must be a factor of source edge_cells_y[',   &
               self%edge_cells_y, ']'
        else if (target_edge_cells_y(i) > self%edge_cells_y) then
          remainder = mod(target_edge_cells_y(i), self%edge_cells_y)
          write(log_scratch_space,'(2(A,I0),A)')               &
               'Source edge_cells_y[',target_edge_cells_y(i),  &
               '] must be a factor of target edge_cells_y[',   &
               self%edge_cells_y, ']'
        end if

        if (remainder == 0_i_def) then
          self%target_edge_cells_y(i) = target_edge_cells_y(i)
        else
          call log_event( trim(log_scratch_space), LOG_LEVEL_ERROR )
        end if

      end do

    else

      write(log_scratch_space,'(A)') &
           'All optional array inputs for target meshes must be of the same length.'
      call log_event(trim(log_scratch_space),  LOG_LEVEL_ERROR)

    end if

    write(target_mesh_names_str, '(A)')   "'"//trim(adjustl(target_mesh_names(1)))//"'"
    write(target_edge_cells_x_str,'(I0)') target_edge_cells_x(1)
    write(target_edge_cells_y_str,'(I0)') target_edge_cells_y(1)
    if (size(target_mesh_names) > 1) then
      do i=2, self%nmaps
        write(target_mesh_names_str,'(A)')                  &
            trim(adjustl(target_mesh_names_str)) // ",'" // &
            trim(adjustl(target_mesh_names(i)))  // "'"
        write(target_edge_cells_x_str,'(A,I0)')             &
            trim(adjustl(target_edge_cells_x_str)) // ',',  &
            target_edge_cells_x(i)
        write(target_edge_cells_y_str,'(A,I0)')             &
            trim(adjustl(target_edge_cells_y_str)) // ',',  &
            target_edge_cells_y(i)
      end do
    end if


    write(target_mesh_names_str,'(A)')    &
        'target_mesh_names=['//trim(adjustl(target_mesh_names_str))//']'
    write(target_edge_cells_x_str,'(A)')  &
        'target_edge_cells_x=['//trim(adjustl(target_edge_cells_x_str))//']'
    write(target_edge_cells_y_str,'(A)')  &
        'target_edge_cells_y=['//trim(adjustl(target_edge_cells_y_str))//']'


    write(self%constructor_inputs,'(A)')        &
        trim(self%constructor_inputs) // ';' // &
        trim(target_mesh_names_str)   // ';' // &
        trim(target_edge_cells_x_str) // ';' // &
        trim(target_edge_cells_y_str)

  end if

  return
end function gen_planar_constructor


!-------------------------------------------------------------------------------
!> @brief   Calculates mesh cell adjacency. (private subroutine)
!> @details Allocates and populates the instance's cell_next(:,:) array
!>          with the id of each cell to which the index cell is adjacent.
!>
!-------------------------------------------------------------------------------
subroutine calc_adjacency(self)

  implicit none

  class(gen_planar_type), intent(inout) :: self

  integer(i_def) :: ncells_x, ncells_y, ncells, nedges_cell
  integer(i_def) :: cell, astat

  integer(i_def) :: border_types(nborders)

  ncells_x    = self%edge_cells_x
  ncells_y    = self%edge_cells_y
  ncells      = ncells_x * ncells_y
  nedges_cell = self%edges_per_face

  allocate( self%cell_next(nedges_cell, ncells ), stat=astat)
  if (astat /= 0_i_def) then
    call log_event( PREFIX//"Failure to allocate cell_next.", &
                    LOG_LEVEL_ERROR )
  end if

  if (self%periodic_x) then
    border_types(W) = periodic
    border_types(E) = periodic
  else
    border_types(W) = closed
    border_types(E) = closed
  end if

  if (self%periodic_y) then
    border_types(S) = periodic
    border_types(N) = periodic
  else
    border_types(S) = closed
    border_types(N) = closed
  end if

  self%cell_next = imdi

  !======================================
  allocate( self%north_cells (ncells_x) )
  allocate( self%south_cells (ncells_x) )
  allocate( self%east_cells  (ncells_y) )
  allocate( self%west_cells  (ncells_y) )

  ! Capture cells on edge of domain
  do cell=1, ncells_x
    self%north_cells(cell) = cell
    self%south_cells(cell) = cell + (ncells_y-1)*ncells_x
  end do

  do cell=1, ncells_y
    self%east_cells(cell) = cell*ncells_x
    self%west_cells(cell) = (cell-1)*ncells_x + 1
  end do

  !======================================
  do cell=1, ncells
    ! Cell default values
    self%cell_next(W, cell) = cell - 1
    self%cell_next(S, cell) = cell + ncells_x
    self%cell_next(E, cell) = cell + 1
    self%cell_next(N, cell) = cell - ncells_x
  end do

  call set_border_adjacency( self, W, self%west_cells,  border_types(W) )
  call set_border_adjacency( self, S, self%south_cells, border_types(S) )
  call set_border_adjacency( self, E, self%east_cells,  border_types(E) )
  call set_border_adjacency( self, N, self%north_cells, border_types(N) )

end subroutine calc_adjacency


!-------------------------------------------------------------------------------
!> @brief   Calculates the adjacency of border cells.
!>          (private subroutine)
!> @details Special attention is required for adjacency on border cells dependng
!>          on whether the domain is periodic in the x and y directions.
!>
!> @param[in]  edge_index    The index of the edge to set on the border cells
!> @param[in]  border_cells  Array of cell ids of those cells on the given border
!> @param[in]  border_type   Specifies if the border is periodic of not using the
!>                           PERIODIC or CLOSED parameters
!-------------------------------------------------------------------------------
subroutine set_border_adjacency( self, edge_index, border_cells, border_type )

  implicit none

  type(gen_planar_type), intent(inout) :: self

  integer(i_def), intent(in) :: edge_index
  integer(i_def), intent(in) :: border_cells(:)
  integer(i_def), intent(in) :: border_type

  integer(i_def) :: i
  integer(i_def) :: ncells_x
  integer(i_def) :: ncells_y
  integer(i_def) :: ncells_border
  integer(i_def) :: cell_id

  ncells_x = self%edge_cells_x
  ncells_y = self%edge_cells_y
  ncells_border = size(border_cells)

  select case(border_type)
  case(CLOSED)
    do i=1, ncells_border
      self%cell_next(edge_index, border_cells(i)) = NON_PERIODIC_ID
    end do

  case(PERIODIC)
    select case(edge_index)
    case(N)
      do i=1, ncells_border
        cell_id = border_cells(i)
        self%cell_next(edge_index, cell_id) = (ncells_y-1)*ncells_x + cell_id
      end do

    case(E)
      do i=1, ncells_border
        cell_id = border_cells(i)
        self%cell_next(edge_index, cell_id) = border_cells(i) - (ncells_x-1)
      end do

    case(S)
      do i=1, ncells_border
        cell_id = border_cells(i)
        self%cell_next(edge_index, cell_id) = cell_id - (ncells_y-1)*ncells_x
      end do

    case(W)
      do i=1, ncells_border
        cell_id = border_cells(i)
        self%cell_next(edge_index, cell_id) = cell_id + ncells_x-1
      end do
    end select

  case default
    ! (Non-Periodic), As a namelist input, absent logicals are
    ! default .false., so it is consistence to make the default
    ! border type as non-periodic
    do i=1, ncells_border
      self%cell_next(edge_index, border_cells(i)) = NON_PERIODIC_ID
    end do

  end select

  return
end subroutine set_border_adjacency


!-------------------------------------------------------------------------------
!> @brief   For each cell, calculates the four vertices whih comprise it.
!>          (private subsroutine)
!> @details Allocates and populates the instance's mesh(:,:) array with
!>          the vertices which form each cell.
!>
!-------------------------------------------------------------------------------
subroutine calc_face_to_vert(self)

  implicit none

  class(gen_planar_type), intent(inout) :: self

  integer(i_def), allocatable :: verts_on_cell(:,:)

  integer(i_def) :: edge_cells_x, edge_cells_y, ncells
  integer(i_def) :: y, vert, cell, astat
  integer(i_def) :: nverts_cell
  integer(i_def) :: node_id

  nverts_cell  = self%nodes_per_face
  edge_cells_x = self%edge_cells_x
  edge_cells_y = self%edge_cells_y
  ncells       = self%n_faces

  allocate(verts_on_cell(nverts_cell, ncells), stat=astat)
  if (astat /= 0) then
    call log_event( PREFIX//"Failure to allocate verts_on_cell array.", &
                    LOG_LEVEL_ERROR )
  end if

  verts_on_cell(:,:) = imdi

  !=====================================================================
  ! FIRST ROW of panel
  !=====================================================================

  ! First cell of first row
  !------------------------
  cell     = 1
  node_id  = 1

  do vert = 1, nverts_cell
    verts_on_cell(vert, cell) = node_id
    node_id = node_id+1
  end do


  ! East neighbour
  if (self%cell_next(E, cell) /= NON_PERIODIC_ID ) then
    verts_on_cell(NW , self%cell_next(E, cell)) = verts_on_cell(NE, cell)
    verts_on_cell(SW , self%cell_next(E, cell)) = verts_on_cell(SE, cell)
  end if

  ! South neighbour
  if (self%cell_next(S, cell) /= NON_PERIODIC_ID ) then
    verts_on_cell(NW , self%cell_next(S, cell)) = verts_on_cell(SW, cell)
    verts_on_cell(NE , self%cell_next(S, cell)) = verts_on_cell(SE, cell)
  end if


  ! Inner cells of first row
  !-------------------------
  if (edge_cells_x > 2) then
    do cell = 2, edge_cells_x-1
      verts_on_cell(SE, cell) = node_id
      verts_on_cell(NE, cell) = node_id+1
      node_id = node_id + 2

      ! East neighbour
      verts_on_cell(NW , self%cell_next(E, cell)) = verts_on_cell(NE, cell)
      verts_on_cell(SW , self%cell_next(E, cell)) = verts_on_cell(SE, cell)

      ! South neighbour
      if (self%cell_next(S, cell) /= NON_PERIODIC_ID ) then
        verts_on_cell(NW , self%cell_next(S, cell)) = verts_on_cell(SW, cell)
        verts_on_cell(NE , self%cell_next(S, cell)) = verts_on_cell(SE, cell)
      end if
    end do
  end if

  ! Last cell of first row
  !-------------------------
  if (edge_cells_x > 1) then
    cell = edge_cells_x
    if (self%periodic_x) then
      ! Copy node id from left edge of domain
      verts_on_cell(SE, cell) = verts_on_cell(SW, cell-edge_cells_x+1)
      verts_on_cell(NE, cell) = verts_on_cell(NW, cell-edge_cells_x+1)
    else
      verts_on_cell(SE, cell) = node_id
      verts_on_cell(NE, cell) = node_id + 1
      node_id = node_id + 2
    end if

    ! South neighbour
    if (self%cell_next(S, cell) /= NON_PERIODIC_ID ) then
      verts_on_cell(NW , self%cell_next(S, cell)) = verts_on_cell(SW, cell)
      verts_on_cell(NE , self%cell_next(S, cell)) = verts_on_cell(SE, cell)
    end if
  end if
  ! END FIRST ROW of panel
  !=====================================================================


  !=====================================================================
  ! INNER ROWS of panel
  !
  if (edge_cells_y > 2) then

    do y = 1, edge_cells_y-2
      ! First cell in row
      !------------------
      cell = (y*edge_cells_x) + 1
      verts_on_cell(SW, cell) = node_id
      verts_on_cell(SE, cell) = node_id+1
      node_id = node_id+2

      ! South neighbour
      verts_on_cell(NW , self%cell_next(S, cell)) = verts_on_cell(SW, cell)
      verts_on_cell(NE , self%cell_next(S, cell)) = verts_on_cell(SE, cell)

      ! East neighbour
      if (edge_cells_x > 1) then
        verts_on_cell(SW , self%cell_next(E, cell)) = verts_on_cell(SE, cell)
      end if

      ! Inner cells of row
      !-------------------
      if (edge_cells_x > 2) then
        do cell = y*edge_cells_x+2, (y+1)*edge_cells_x-1
          verts_on_cell(SE, cell) = node_id
          node_id = node_id+1

          ! South neighbour
          verts_on_cell(NW, self%cell_next(S, cell)) = verts_on_cell(SW, cell)
          verts_on_cell(NE, self%cell_next(S, cell)) = verts_on_cell(SE, cell)

          ! East neighbour
          verts_on_cell(SW , self%cell_next(E, cell)) = verts_on_cell(SE, cell)
        end do
      end if

      ! Last cell of row
      !-------------------
      if (edge_cells_x > 1) then
        cell = (y+1)*edge_cells_x
        if (self%periodic_x) then
          verts_on_cell(SE, cell) = verts_on_cell(SW, self%cell_next(E, cell))
        else
          verts_on_cell(SE, cell) = node_id
          node_id = node_id+1
        end if

        ! South neighbour
        verts_on_cell(NW, self%cell_next(S, cell)) = verts_on_cell(SW, cell)
        verts_on_cell(NE, self%cell_next(S, cell)) = verts_on_cell(SE, cell)
      end if

    end do
  end if

  !========================
  ! BOTTOM EDGE of Panel
  !========================
  if (self%periodic_y) then

    ! Copy from top edge row
    do cell = ncells-edge_cells_x+1, ncells
      verts_on_cell(SW, cell) = verts_on_cell(NW, self%cell_next(S, cell) )
      verts_on_cell(SE, cell) = verts_on_cell(NE, self%cell_next(S, cell) )
    end do

    node_id = node_id - 1

  else

    if (edge_cells_y > 1) then
      ! Always do first cell in bottom row
      ! First cell in bottom row
      cell = ncells-edge_cells_x+1
      verts_on_cell(SW, cell) = node_id
      verts_on_cell(SE, cell) = node_id+1
      node_id = node_id+2

      ! Inner cells in bottom row
      do cell = ncells-edge_cells_x+2, ncells-1
        verts_on_cell(SW, cell) = verts_on_cell(SE, self%cell_next(W, cell))
        verts_on_cell(SE, cell) = node_id
        node_id = node_id+1
      end do

      ! Last cell in bottom row
      cell = ncells
      if (edge_cells_x > 1) then
        verts_on_cell(SW, cell) = verts_on_cell(SE, self%cell_next(W, cell))
        if (self%periodic_x) then
          verts_on_cell(SE, cell) = verts_on_cell(SW, self%cell_next(E, cell))
          node_id = node_id - 1
        else
          verts_on_cell(SE, cell) = node_id
        end if
      end if
    end if
  end if ! self%periodic_y

  call move_alloc(verts_on_cell, self%verts_on_cell)

  return
end subroutine calc_face_to_vert


!-------------------------------------------------------------------------------
!> @brief   Calculates the edges which are found on each cell and the
!>          pair of vertices which are found on each edge.(private subroutine)
!> @details Allocates and populates both the edges_on_cell and
!>          verts_on_edge arrays for the instance.
!>
!-------------------------------------------------------------------------------
subroutine calc_edges(self)

  implicit none

  class(gen_planar_type), intent(inout)  :: self

  integer(i_def) :: edge_cells_x, edge_cells_y, ncells
  integer(i_def) :: cell
  integer(i_def) :: edge_id

  edge_cells_x = self%edge_cells_x
  edge_cells_y = self%edge_cells_y
  ncells = self%edge_cells_x * self%edge_cells_y

  cell     = 1
  edge_id  = 1

  ! The assumed working layout for the folowing code with respect to
  ! cell/panel is
  !
  ! ID Origin     Top (North)
  !    (NW) O--------------------O (NE)
  !         |                    +
  !         |   edge/vertex id   +
  !         |  + anti-clockwise  +
  !   Left  |   \   Numbering    + Right
  !  (West) |    \               + (East)
  !         |     +------->      +
  !         |                    +
  !         |                    +
  !    (SE) O+-+-+-+-+-+-+-+-+-+-O (SE)
  !             Bottom (South)
  !
  ! This is a 2D grid, and compass points are used to orientate
  ! around the panel/cell. (top, left, right, bottom) could
  ! have been used, though top and bottom could have been
  ! confused with 3D cells. However, for readability,
  ! the terms (left, bottom, top, right) are used interchangably
  ! with the cardinal compass directions in the above diagram.
  ! It should also be noted that the cardinal compass directions
  ! are only used as an alternative way to reference the elements
  ! on the the panel/cells, i.e. the Top (North) edge of a cell
  ! may not actually be aligned in the same direction as geographic
  ! north.
  !
  ! When considered as a panel, the Eastern and Southern edges
  ! are taken as the `ghost` edges/vertices in the case of any
  ! periodicity.
  !
  ! So that vertices on edges are consistent, listed vertices
  ! connected to edges will go from N-S, or W-E.

  !--------------------------------------------
  ! Top panel row, cell@left panel edge (ID:1)
  !--------------------------------------------
  ! Cell western edge
  self%edges_on_cell(W, cell)      = edge_id
  self%verts_on_edge(1, edge_id)   = self%verts_on_cell(NW, cell)
  self%verts_on_edge(2, edge_id)   = self%verts_on_cell(SW, cell)

  ! Cell southern edge
  self%edges_on_cell(S, cell)  = edge_id+1
  self%verts_on_edge(1, edge_id+1) = self%verts_on_cell(SW, cell)
  self%verts_on_edge(2, edge_id+1) = self%verts_on_cell(SE, cell)

  ! Cell eastern edge
  self%edges_on_cell(E, cell)  = edge_id+2
  self%verts_on_edge(1, edge_id+2) = self%verts_on_cell(NE, cell)
  self%verts_on_edge(2, edge_id+2) = self%verts_on_cell(SE, cell)

  ! Cell northern edge
  self%edges_on_cell(N, cell)  = edge_id+3
  self%verts_on_edge(1, edge_id+3) = self%verts_on_cell(NW, cell)
  self%verts_on_edge(2, edge_id+3) = self%verts_on_cell(NE, cell)

  edge_id = edge_id+4

  !-----------------------------------------------------------
  ! Top panel row, remaining cells, i.e. IDs = 2:edge_cells_x
  !-----------------------------------------------------------
  if (edge_cells_x > 1) then
    do cell = 2, edge_cells_x

      ! Cell western edge
      self%edges_on_cell(W, cell) = self%edges_on_cell(E,self%cell_next(W,cell))

      ! Cell southern edge
      self%edges_on_cell(S, cell) = edge_id
      self%verts_on_edge(1, edge_id)  = self%verts_on_cell(SW, cell)
      self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SE, cell)
      edge_id = edge_id + 1

      !-------------------------
      if (cell == edge_cells_x) then
        ! This cell is on the right-hand edge of the panel.

        ! For a planar panel, if the cell is on the right-hand edge
        ! of the panel then the cell's eastern neighbour is actually
        ! the left-most cell on this row (because of the periodicity).
        ! At this point, left-most cell on this row has already
        ! had its edge ids assigned.

        ! In other words, the eastern edge of this cell exists on
        ! another cell where it has already been assigned an id.

        ! Cell eastern edge
        if (self%periodic_x) then
          self%edges_on_cell(E, cell) = self%edges_on_cell(W, self%cell_next(E,cell))

        else
          self%edges_on_cell(E, cell) = edge_id
          self%verts_on_edge(1, edge_id) = self%verts_on_cell(NE, cell)
          self%verts_on_edge(2, edge_id) = self%verts_on_cell(SE, cell)
          edge_id = edge_id + 1
        end if

        ! Cell northern edge
        self%edges_on_cell(N, cell)  = edge_id
        self%verts_on_edge(1, edge_id) = self%verts_on_cell(NW, cell)
        self%verts_on_edge(2, edge_id) = self%verts_on_cell(NE, cell)
        edge_id = edge_id+1

      else

        ! Cell eastern edge
        self%edges_on_cell(E, cell)  = edge_id
        self%verts_on_edge(1, edge_id) = self%verts_on_cell(NE, cell)
        self%verts_on_edge(2, edge_id) = self%verts_on_cell(SE, cell)
        edge_id = edge_id+1

        ! Cell northern edge
        self%edges_on_cell(N, cell)  = edge_id
        self%verts_on_edge(1, edge_id) = self%verts_on_cell(NW, cell)
        self%verts_on_edge(2, edge_id) = self%verts_on_cell(NE, cell)
        edge_id = edge_id+1

      end if

    end do
  end if

  !-----------------------------------------
  ! Internal panel rows
  !-----------------------------------------
  if (edge_cells_y > 2) then
    do cell = edge_cells_x+1, ncells-edge_cells_x

      if (mod(cell,edge_cells_x) == 1) then
        ! This cell is on the left-hand panel edge
        !-------------------------------------------

        ! Cell western edge
        self%edges_on_cell(W, cell)  = edge_id
        self%verts_on_edge(1, edge_id)   = self%verts_on_cell(NW, cell)
        self%verts_on_edge(2, edge_id)   = self%verts_on_cell(SW, cell)


        ! Cell southern edge
        self%edges_on_cell(S, cell)  = edge_id+1
        self%verts_on_edge(1, edge_id+1) = self%verts_on_cell(SW, cell)
        self%verts_on_edge(2, edge_id+1) = self%verts_on_cell(SE, cell)

        ! Cell eastern edge
        self%edges_on_cell(E, cell)  = edge_id+2
        self%verts_on_edge(1, edge_id+2) = self%verts_on_cell(NE, cell)
        self%verts_on_edge(2, edge_id+2) = self%verts_on_cell(SE, cell)

        edge_id = edge_id+3

      else if (mod(cell,edge_cells_x) == 0) then
        ! This cell is on the right-hand panel edge
        !--------------------------------------------
        if (edge_cells_x==1) then
          self%edges_on_cell(W, cell)  = edge_id
          self%verts_on_edge(1, edge_id)   = self%verts_on_cell(NW, cell)
          self%verts_on_edge(2, edge_id)   = self%verts_on_cell(SW, cell)
          edge_id = edge_id + 1
        else
          ! Cell western edge
          self%edges_on_cell(W, cell) = self%edges_on_cell(E,self%cell_next(W,cell))
        end if

        ! Cell southern edge
        self%edges_on_cell(S, cell) = edge_id
        self%verts_on_edge(1, edge_id)  = self%verts_on_cell(SW, cell)
        self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SE, cell)

        edge_id = edge_id+1

        ! Cell eastern edge
        if (self%periodic_x) then
          self%edges_on_cell(E, cell) = self%edges_on_cell(W, self%cell_next(E, cell))
        else

          self%edges_on_cell(E, cell) = edge_id
          self%verts_on_edge(1, edge_id)  = self%verts_on_cell(NE, cell)
          self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SE, cell)
          edge_id = edge_id+1
        end if

      else
        ! This cell is internal to the panel
        !-------------------------------------------

        ! Cell western edge
        self%edges_on_cell(W, cell) = self%edges_on_cell(E,self%cell_next(W,cell))

        ! Cell southern edge
        self%edges_on_cell(S, cell) = edge_id
        self%verts_on_edge(1, edge_id)  = self%verts_on_cell(SW, cell)
        self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SE, cell)

        ! Cell eastern edge
        self%edges_on_cell(E, cell)  = edge_id+1
        self%verts_on_edge(1, edge_id+1) = self%verts_on_cell(NE, cell)
        self%verts_on_edge(2, edge_id+1) = self%verts_on_cell(SE, cell)

        edge_id=edge_id+2

      end if

      ! Cell northern edge
      ! The northern edges on these cells exist on the cells in
      ! the row above/previous to this one. Those edges have
      ! already been assigned ids.
      self%edges_on_cell(N, cell) = self%edges_on_cell(S,self%cell_next(N,cell))

    end do ! Panel inner rows
  end if

  if (edge_cells_y > 1) then
    ! Panel bottom row
    do cell = ncells-edge_cells_x+1, ncells

      if (mod(cell,edge_cells_x) == 1 ) then

        ! This cell is on the left-hand panel edge
        !-------------------------------------------

        ! Cell western edge
        self%edges_on_cell(W, cell) = edge_id
        self%verts_on_edge(1, edge_id)  = self%verts_on_cell(NW, cell)
        self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SW, cell)
        edge_id = edge_id+1

        ! Cell southern edge
        if (self%periodic_y) then
          ! For a planar panel, if the cell is on the bottom edge
          ! of the panel then the cell's southern neighbour is actually
          ! the on the top-edge of the panel (because of the periodicity).
          ! At this point, all cells on the top-edge of the panel have
          ! had their edge ids assigned.

          ! In other words, the southern edge of this cell exists on
          ! another cell where it has already been assigned an id.
          self%edges_on_cell(S, cell) = self%edges_on_cell(N, self%cell_next(S, cell))
        else
          self%edges_on_cell(S, cell) = edge_id
          self%verts_on_edge(1, edge_id)  = self%verts_on_cell(SW, cell)
          self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SE, cell)
          edge_id = edge_id+1
        end if

        ! Cell eastern edge
        self%edges_on_cell(E, cell)  = edge_id
        self%verts_on_edge(1, edge_id) = self%verts_on_cell(NE, cell)
        self%verts_on_edge(2, edge_id) = self%verts_on_cell(SE, cell)
        edge_id = edge_id+1

      else if (mod(cell,edge_cells_x) == 0) then
        ! This cell is on the right-hand panel edge
        !--------------------------------------------
        ! Cell western edge
        if (edge_cells_x == 1) then
          self%edges_on_cell(W, cell) = edge_id
          self%verts_on_edge(1, edge_id)  = self%verts_on_cell(NW, cell)
          self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SW, cell)
          edge_id = edge_id+1
        else if (edge_cells_x > 1) then
          self%edges_on_cell(W, cell) = self%edges_on_cell(E,self%cell_next(W,cell))
        end if

        ! Cell southern edge
        if (self%periodic_y) then
          ! For a planar panel, if the cell is on the bottom edge
          ! of the panel then the cell's southern neighbour is actually
          ! the on the top-edge of the panel (because of the periodicity).
          ! At this point, all cells on the top-edge of the panel have
          ! had their edge ids assigned.

          ! In other words, the southern edge of this cell exists on
          ! another cell where it has already been assigned an id.
          self%edges_on_cell(S, cell) = self%edges_on_cell(N, self%cell_next(S, cell))
        else
          self%edges_on_cell(S, cell) = edge_id
          self%verts_on_edge(1, edge_id)  = self%verts_on_cell(SW, cell)
          self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SE, cell)
          edge_id = edge_id+1
        end if

        ! Cell eastern edge
        if (self%periodic_x) then
          ! For a planar panel, if the cell is on the right-hand edge
          ! of the panel then the cell's eastern neighbour is actually
          ! the left-most cell on this row (because of the periodicity).
          ! At this point, left-most cell on this row has already
          ! had its edge ids assigned.

          ! In other words, the eastern edge of this cell exists on
          ! another cell where it has already been assigned an id.
          self%edges_on_cell(E, cell) = self%edges_on_cell(W, self%cell_next(E,cell))
        else
          self%edges_on_cell(E, cell) = edge_id
          self%verts_on_edge(1, edge_id)  = self%verts_on_cell(NE, cell)
          self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SE, cell)
          edge_id = edge_id+1
        end if

      else

        ! This cell is an inner cell on the bottom row
        !---------------------------------------------
        ! Cell western edge
        self%edges_on_cell(W, cell) = self%edges_on_cell(E, self%cell_next(W,cell))

        ! Cell southern edge
        if (self%periodic_y) then
          ! For a planar panel, if the cell is on the bottom edge
          ! of the panel then the cell's southern neighbour is actually
          ! the on the top-edge of the panel (because of the periodicity).
          ! At this point, all cells on the top-edge of the panel have
          ! had their edge ids assigned.

          ! In other words, the southern edge of this cell exists on
          ! another cell where it has already been assigned an id.
          self%edges_on_cell(S, cell) = self%edges_on_cell(N, self%cell_next(S, cell))

        else

          self%edges_on_cell(S, cell) = edge_id
          self%verts_on_edge(1, edge_id)  = self%verts_on_cell(SW, cell)
          self%verts_on_edge(2, edge_id)  = self%verts_on_cell(SE, cell)
          edge_id = edge_id+1

        end if

        ! Cell eastern edge
        self%edges_on_cell(E, cell) = edge_id
        self%verts_on_edge(1, edge_id) = self%verts_on_cell(NE, cell)
        self%verts_on_edge(2, edge_id) = self%verts_on_cell(SE, cell)
        edge_id = edge_id+1

      end if

      ! Cell north edge
      ! The northern edges on these cells exist on the cells in
      ! the row above/previous to this one. Those edges have
      ! already been assigned ids.
      self%edges_on_cell(N, cell) = self%edges_on_cell(S,self%cell_next(N,cell))

    end do
  end if

  return
end subroutine calc_edges


!-------------------------------------------------------------------------------
!> @brief   Calculates the coordinates of vertices in the mesh.(private subroutine)
!> @details Assigns an (x,y) coordinate in units of dx and dy to each mesh
!>          vertex according to its Cartesian position in the mesh.
!>          Note: The origin of the mesh is constrained to always be located
!>                on a node. In cases were the centre of the domain would fall
!>                inside a cell, the nearest node S/W would be used.
!>
!-------------------------------------------------------------------------------
subroutine calc_coords(self)

  implicit none

  class(gen_planar_type), intent(inout)  :: self

  real(r_def), allocatable :: vert_coords(:,:)

  integer(i_def) :: ncells, edge_cells_x, edge_cells_y
  integer(i_def) :: cell, astat, row, column

  real(r_def) :: x_coord
  real(r_def) :: y_coord
  real(r_def) :: offset_x
  real(r_def) :: offset_y

  edge_cells_x = self%edge_cells_x
  edge_cells_y = self%edge_cells_y
  ncells = edge_cells_x*edge_cells_y

  allocate(vert_coords(2, self%n_nodes), stat=astat)
  if (astat /= 0) then
    call log_event( PREFIX//"Failure to allocate vert_coords.", &
                    LOG_LEVEL_ERROR )
  end if

  ! Origin (0,0) is centre of mesh. Cell 1 is in top left of mesh panel.
  ! Top NW node will be present in both periodic and non-periodic meshs
  offset_x = (-1.0_r_def*self%dx*self%edge_cells_x)/2_r_def
  offset_y = (self%dy*self%edge_cells_y)/2_r_def

  if (mod( self%edge_cells_x, 2 ) == 1) then
    offset_x = offset_x + self%dx/2_r_def
  end if

  if (mod( self%edge_cells_y, 2 ) == 1) then
    offset_y = offset_y + self%dy/2_r_def
  end if

  ! The cells begin numbering in rows from NW corner of panel
  cell=1
  do row=1, self%edge_cells_y
    do column=1, self%edge_cells_x
      vert_coords(1, self%verts_on_cell(NW, cell)) = (column-1) * self%dx
      vert_coords(2, self%verts_on_cell(NW, cell)) = (row-1)    * self%dy * (-1.0_r_def)
      cell=cell+1
    end do
  end do

  if (.not. self%periodic_x) then
    ! Vertices on east edge of panel
    x_coord = edge_cells_x*self%dx
    y_coord = 0.0_r_def
    do cell=1, size(self%east_cells)
      vert_coords(1, self%verts_on_cell(NE, self%east_cells(cell))) = x_coord
      vert_coords(2, self%verts_on_cell(NE, self%east_cells(cell))) = y_coord - (self%dy*(cell-1))
    end do
  end if

  ! Vertices on south edge of panel
  if (.not. self%periodic_y) then
    x_coord = 0.0_r_def
    y_coord = -1.0_r_def*edge_cells_y*self%dy

    do cell=1, size(self%south_cells)
      vert_coords(1, self%verts_on_cell(SW, self%south_cells(cell))) = x_coord + (self%dx*(cell-1))
      vert_coords(2, self%verts_on_cell(SW, self%south_cells(cell))) = y_coord
    end do
  end if

  ! Coords of SE panel vertex
  if (.not. self%periodic_x .and. .not. self%periodic_y) then
    cell=ncells
    vert_coords(1, self%verts_on_cell(SE, cell)) = self%dx * self%edge_cells_x
    vert_coords(2, self%verts_on_cell(SE, cell)) = self%dy * self%edge_cells_y * (-1.0_r_def)
  end if

  vert_coords(1,:) =  vert_coords(1,:) + offset_x
  vert_coords(2,:) =  vert_coords(2,:) + offset_y

  self%coord_units_x = 'm'
  self%coord_units_y = 'm'

  call move_alloc(vert_coords, self%vert_coords)

  return
end subroutine calc_coords


!-------------------------------------------------------------------------------
!> @brief   Calculates the mesh cell centres.(private subroutine)
!> @details The face centres for the mesh are calculated based on the current
!>          node coordinates of the node on the NW corner of the cell.
!>          The node_cordinates are assumed to be in [m] in the
!>          x,y plane. Resulting face centre coordinates are in [m].
!>
!-------------------------------------------------------------------------------
subroutine calc_cell_centres(self)

  implicit none

  class(gen_planar_type), intent(inout) :: self

  integer(i_def) :: ncells

  ! Counters
  integer(i_def) :: cell, base_vert


  ncells = self%npanels*self%edge_cells_x*self%edge_cells_y

  ! 1.0 Initialise the face centres
  if (.not. allocated(self%cell_coords)) allocate( self%cell_coords(2,ncells) )
  self%cell_coords(:,:) = 0.0_r_def

  ! 2.0 Open cells have `ghost` nodes/edges and are located
  ! on the Eastern/Southern edges of the panel. Identify the open cells
  ! assuming that the numbering is along rows beginning from the NW corner
  ! of the panel. All cells, including the open cells have a unique NW
  ! vertex. Use this NW and self%dx, self%dy to calculate the face centre,
  ! assuming the cells are parallel in the x and y directions.
  do cell=1, ncells
    base_vert = self%verts_on_cell(NW, cell)
    self%cell_coords(1,cell) = self%vert_coords(1, base_vert) + self%dx/2.0_r_def
    self%cell_coords(2,cell) = self%vert_coords(2, base_vert) - self%dy/2.0_r_def
  end do

end subroutine calc_cell_centres


!-------------------------------------------------------------------------------
!> @brief Populates the arguments with the dimensions defining
!>        the planar mesh.
!>
!> @param[out]  num_nodes           The number of nodes on the mesh.
!> @param[out]  num_edges           The number of edges on the mesh.
!> @param[out]  num_faces           The number of faces on the mesh.
!> @param[out]  num_nodes_per_face  The number of nodes around each face.
!> @param[out]  num_edges_per_face  The number of edges around each face.
!> @param[out]  num_nodes_per_face  The number of nodes around each edge.
!-------------------------------------------------------------------------------
subroutine get_dimensions( self,                                   &
                           num_nodes, num_edges, num_faces,        &
                           num_nodes_per_face, num_edges_per_face, &
                           num_nodes_per_edge )
  implicit none

  class(gen_planar_type),    intent(in) :: self

  integer(i_def), intent(out) :: num_nodes
  integer(i_def), intent(out) :: num_edges
  integer(i_def), intent(out) :: num_faces
  integer(i_def), intent(out) :: num_nodes_per_face
  integer(i_def), intent(out) :: num_edges_per_face
  integer(i_def), intent(out) :: num_nodes_per_edge

  num_nodes = self%n_nodes
  num_edges = self%n_edges
  num_faces = self%n_faces

  num_nodes_per_face = self%nodes_per_face
  num_edges_per_face = self%edges_per_face
  num_nodes_per_edge = self%nodes_per_edge

  return
end subroutine get_dimensions


!-------------------------------------------------------------------------------
!> @brief   Populates the argument array with the coordinates of the
!>          mesh's vertices.
!> @details Exposes the instance's vert_coords array to the caller.
!>
!> @param[out]  node_coordinates  The argument to receive the vert_coords data.
!> @param[out]  cell_coordinates  The argument to receive cell centre coordinates.
!> @param[out]  coord_units_x     Units for x-coordinates
!> @param[out]  coord_units_y     Units for y-coordinates
!-------------------------------------------------------------------------------
subroutine get_coordinates(self, node_coordinates, &
                                 cell_coordinates, &
                                 coord_units_x,    &
                                 coord_units_y)

  implicit none

  class(gen_planar_type), intent(in)  :: self
  real(r_def),            intent(out) :: node_coordinates(:,:)
  real(r_def),            intent(out) :: cell_coordinates(:,:)
  character(str_def),     intent(out) :: coord_units_x
  character(str_def),     intent(out) :: coord_units_y


  node_coordinates = self%vert_coords
  cell_coordinates = self%cell_coords
  coord_units_x    = self%coord_units_x
  coord_units_y    = self%coord_units_y

  return
end subroutine get_coordinates


!-------------------------------------------------------------------------------
!> @brief   Populates the argument arrays with the corresponding mesh
!>          connectivity information.
!> @details Implements the connectivity-providing interface required
!>          by the ugrid writer.
!>
!> @param[out]  face_node_connectivity  Face-node connectivity.
!> @param[out]  edge_node_connectivity  Edge-node connectivity.
!> @param[out]  face_edge_connectivity  Face-edge connectivity.
!> @param[out]  face_face_connectivity  Face-face connectivity.
!-------------------------------------------------------------------------------
subroutine get_connectivity( self,                   &
                             face_node_connectivity, &
                             edge_node_connectivity, &
                             face_edge_connectivity, &
                             face_face_connectivity )

  implicit none

  class(gen_planar_type), intent(in) :: self
  integer(i_def), intent(out) :: face_node_connectivity(:,:)
  integer(i_def), intent(out) :: edge_node_connectivity(:,:)
  integer(i_def), intent(out) :: face_edge_connectivity(:,:)
  integer(i_def), intent(out) :: face_face_connectivity(:,:)

  face_node_connectivity = self%verts_on_cell
  edge_node_connectivity = self%verts_on_edge
  face_edge_connectivity = self%edges_on_cell
  face_face_connectivity = self%cell_next

  return
end subroutine get_connectivity


!-------------------------------------------------------------------------------
!> @brief   Generates the mesh and connectivity.
!> @details Calls each of the instance methods which calculate the
!>          specified mesh and populate the arrays.
!>
!-------------------------------------------------------------------------------
subroutine generate(self)

  implicit none

  class(gen_planar_type), intent(inout) :: self

  call calc_adjacency(self)
  call calc_face_to_vert(self)
  call calc_edges(self)
  if (self%nmaps > 0) call calc_global_mesh_maps(self)
  call calc_coords(self)
  call calc_cell_centres(self)

  if (DEBUG) call write_mesh(self)

  return
end subroutine generate


!-------------------------------------------------------------------------------
!> @brief   Generates requested global mesh maps.(private subroutine)
!> @details A map is generated for each requested target mesh based on this
!>          mesh objects mesh details, and those of the requested target
!>          meshes using calc_global_cell_map.
!>
!-------------------------------------------------------------------------------
subroutine calc_global_mesh_maps(self)

  implicit none

  class(gen_planar_type), intent(inout) :: self

  integer(i_def) :: source_id, source_cpp, source_ncells, &
                    target_edge_cells_x, target_edge_cells_y, target_cpp, &
                    target_ncells, target_cells_per_source_cell,i
  integer(i_def), allocatable :: cell_map(:,:)

  allocate( self%global_mesh_maps, source=global_mesh_map_collection_type())

  source_id  = 1
  source_cpp = self%edge_cells_x*self%edge_cells_y
  source_ncells = source_cpp*self%npanels

  do i=1, size(self%target_mesh_names)

    target_edge_cells_x = self%target_edge_cells_x(i)
    target_edge_cells_y = self%target_edge_cells_y(i)
    target_cpp          = target_edge_cells_x*target_edge_cells_y
    target_ncells       = target_cpp*self%npanels
    target_cells_per_source_cell = max(1,target_ncells/source_ncells)
    allocate(cell_map(target_cells_per_source_cell,source_ncells))

    call calc_global_cell_map(self, target_edge_cells_x, target_edge_cells_y, cell_map )
    call self%global_mesh_maps%add_global_mesh_map( source_id, i+1, cell_map )
    deallocate(cell_map)

  end do

  return
end subroutine calc_global_mesh_maps


!-----------------------------------------------------------------------------
!> @brief Returns mesh metadata information.
!> @details This subroutine is provided as a means to request specific metadata
!>          from the current mesh configuration.
!> @param[out]  mesh_name          [optional] Name of mesh instance to generate
!> @param[out]  mesh_class         [optional] Primitive shape, i.e. sphere, plane
!> @param[out]  periodic_x         [optional] Periodic in E-W direction.
!> @param[out]  periodic_y         [optional] Periodic in N-S direction.
!> @param[out]  npanels            [optional] Number of panels use to describe mesh
!> @param[out]  edge_cells_x       [optional] Number of panel edge cells (x-axis).
!> @param[out]  edge_cells_y       [optional] Number of panel edge cells (y-axis).
!> @param[out]  constructor_inputs [optional] Inputs used to create this mesh from
!>                                            the mesh_generator
!> @param[out]  nmaps              [optional] Number of maps to create with this mesh
!>                                            as source mesh
!> @param[out]  maps_mesh_names    [optional] Mesh names of the target meshes that
!>                                            this mesh has maps for.
!> @param[out]  maps_edge_cells_x  [optional] Number of panel edge cells (x-axis) of
!>                                            target mesh(es) to create map(s) for.
!> @param[out]  maps_edge_cells_y  [optional] Number of panel edge cells (y-axis) of
!>                                            target mesh(es) to create map(s) for.
!-----------------------------------------------------------------------------
subroutine get_metadata( self,               &
                         mesh_name,          &
                         mesh_class,         &
                         periodic_x,         &
                         periodic_y,         &
                         npanels,            &
                         edge_cells_x,       &
                         edge_cells_y,       &
                         constructor_inputs, &
                         nmaps,              &
                         maps_mesh_names,    &
                         maps_edge_cells_x,  &
                         maps_edge_cells_y )
  implicit none

  class(gen_planar_type),        intent(in)  :: self
  character(str_def),  optional, intent(out) :: mesh_name
  character(str_def),  optional, intent(out) :: mesh_class
  logical(l_def),      optional, intent(out) :: periodic_x
  logical(l_def),      optional, intent(out) :: periodic_y
  integer(i_def),      optional, intent(out) :: npanels
  integer(i_def),      optional, intent(out) :: edge_cells_x
  integer(i_def),      optional, intent(out) :: edge_cells_y
  integer(i_def),      optional, intent(out) :: nmaps
  character(str_long), optional, intent(out) :: constructor_inputs

  character(str_def), optional, allocatable, intent(out) :: maps_mesh_names(:)
  integer(i_def),     optional, allocatable, intent(out) :: maps_edge_cells_x(:)
  integer(i_def),     optional, allocatable, intent(out) :: maps_edge_cells_y(:)

  if (present(mesh_name))          mesh_name          = self%mesh_name
  if (present(mesh_class))         mesh_class         = self%mesh_class
  if (present(periodic_x))         periodic_x         = self%periodic_x
  if (present(periodic_y))         periodic_y         = self%periodic_y
  if (present(npanels))            npanels            = self%npanels
  if (present(edge_cells_x))       edge_cells_x       = self%edge_cells_x
  if (present(edge_cells_y))       edge_cells_y       = self%edge_cells_y
  if (present(constructor_inputs)) constructor_inputs = trim(self%constructor_inputs)
  if (present(nmaps))              nmaps              = self%nmaps

  if (self%nmaps > 0) then
    if (present(maps_mesh_names))   maps_mesh_names   = self%target_mesh_names
    if (present(maps_edge_cells_x)) maps_edge_cells_x = self%target_edge_cells_x
    if (present(maps_edge_cells_y)) maps_edge_cells_y = self%target_edge_cells_y
  end if

  return
end subroutine get_metadata


!-------------------------------------------------------------------------------
!> @brief  Gets the global mesh map collection which uses
!>         this mesh as the source mesh.
!>
!> @return global_mesh_maps global_mesh_map_collection_type
!-------------------------------------------------------------------------------
function get_global_mesh_maps(self) result(global_mesh_maps)

  implicit none

  class(gen_planar_type), target, intent(in) :: self

  type(global_mesh_map_collection_type), pointer :: global_mesh_maps

  nullify(global_mesh_maps)
  global_mesh_maps => self%global_mesh_maps

  return
end function get_global_mesh_maps


!-------------------------------------------------------------------------------
!> @brief   Subroutine to manually deallocate any memory used by the object.
!>
!-------------------------------------------------------------------------------
subroutine clear(self)

    implicit none

    class(gen_planar_type), intent(inout) :: self

    if (allocated(self%north_cells))   deallocate( self%north_cells )
    if (allocated(self%east_cells))    deallocate( self%east_cells  )
    if (allocated(self%south_cells))   deallocate( self%south_cells )
    if (allocated(self%west_cells))    deallocate( self%west_cells  )

    if (allocated(self%cell_next))     deallocate( self%cell_next     )
    if (allocated(self%verts_on_cell)) deallocate( self%verts_on_cell )
    if (allocated(self%edges_on_cell)) deallocate( self%edges_on_cell )
    if (allocated(self%verts_on_edge)) deallocate( self%verts_on_edge )
    if (allocated(self%vert_coords))   deallocate( self%vert_coords   )
    if (allocated(self%cell_coords))   deallocate( self%cell_coords   )

    if (allocated(self%global_mesh_maps)) then
      call self%global_mesh_maps%clear()
      deallocate( self%global_mesh_maps )
    end if

    return
  end subroutine clear


!-------------------------------------------------------------------------------
!> @brief Writes out the mesh and connectivity for debugging purposes.
!>
!-------------------------------------------------------------------------------
subroutine write_mesh(self)

  use global_mesh_map_collection_mod, only: global_mesh_map_collection_type
  use global_mesh_map_mod, only: global_mesh_map_type
  use iso_fortran_env,     only: stdout => output_unit

  implicit none

  class(gen_planar_type), intent(in) :: self

  integer(i_def) :: i, j, ncells
  integer(i_def) :: cell, edge, vert

  type(global_mesh_map_type), pointer :: global_mesh_map => null()

  integer(i_def), allocatable :: cell_map (:,:)
  integer(i_def), allocatable :: tmp_map (:,:)
  integer(i_def) :: nsource
  integer(i_def) :: ntarget_per_cell

  character(str_long) :: tmp_str

  ncells = self%edge_cells_x * self%edge_cells_y

  write(stdout,'(A)')    "====DEBUG INFO===="
  write(stdout,'(A)')    "Mesh name: "// trim(self%mesh_name)
  write(stdout,'(A)')    "Class:     "// trim(self%mesh_class)
  write(stdout,'(A,I0)') "Panels:    ", self%npanels
  write(stdout,'(A,I0)') "Panel edge cells (x): ", self%edge_cells_x
  write(stdout,'(A,I0)') "Panel edge cells (y): ", self%edge_cells_y
  write(stdout,'(A,L1)') "Periodic in x-axis: ", self%periodic_x
  write(stdout,'(A,L1)') "Periodic in y-axis: ", self%periodic_y
  write(stdout,'(A,I0)') 'Number of nodes: ', self%n_nodes
  write(stdout,'(A,I0)') 'Number of edges: ', self%n_edges
  write(stdout,'(A,I0)') 'Number of cells: ', self%n_faces
  write(stdout,'(A)')    "Co-ord (x) units: "// trim(self%coord_units_x)
  write(stdout,'(A)')    "Co-ord (y) units: "// trim(self%coord_units_y)
  write(stdout,'(A,I0)') "Mappings to other meshes: ", self%nmaps
  do i=1, self%nmaps
    write(stdout,'(T4,A,2(I0,A))') trim(self%target_mesh_names(i))//     &
                                   '(', self%target_edge_cells_x(i),',', &
                                        self%target_edge_cells_y(i),')'
  end do

  write(stdout,'(A)') ''
  write(stdout,'(A)') '=========================='
  write(stdout,'(A)') ' Cell adjacency (W,S,E,N)'
  write(stdout,'(A)') '=========================='
  do cell=1, self%n_faces
    tmp_str=''
    write(tmp_str,'(I07,A,4(I07,"  "))') cell,' => ', self%cell_next(:,cell)
    write(stdout,('(A)')) trim(tmp_str)
  end do


  write(stdout,'(A)') ''
  write(stdout,'(A)') '================================='
  write(stdout,'(A)') ' Vertices on cells (SW,SE,NW,NE)'
  write(stdout,'(A)') '================================='
  do cell=1, self%n_faces
    tmp_str=''
    write(tmp_str,'(I07,A,4(I07,"  "))') cell,' => ', self%verts_on_cell(:,cell)
    write(stdout,('(A)')) trim(tmp_str)
  end do


  write(stdout,'(A)') ''
  write(stdout,'(A)') '================================='
  write(stdout,'(A)') ' Edges on cells (W,S,E,N)'
  write(stdout,'(A)') '================================='
  do cell=1, self%n_faces
    tmp_str=''
    write(tmp_str,'(I07,A,4(I07,"  "))') cell, ' => ', self%edges_on_cell(:,cell)
    write(stdout,('(A)')) trim(tmp_str)
  end do


  write(stdout,'(A)') ''
  write(stdout,'(A)') '================================='
  write(stdout,'(A)') ' Vertices on edges, N-S/W-E'
  write(stdout,'(A)') '================================='
  do edge=1, self%n_edges
    tmp_str=''
    write(stdout,'(I07,A,I07,A,I07)')             &
        edge, ' => ', self%verts_on_edge(1,edge), &
        ' -- ', self%verts_on_edge(2,edge)
  end do


  write(stdout,'(A)')  ''
  write(stdout,'(A)') '================================='
  write(stdout,'(A)') ' Node Coordinates (x,y)'
  write(stdout,'(A)') '================================='
  do vert=1, self%n_nodes
    tmp_str=''
    write(tmp_str,'(I07,A,F10.4,A,F10.4,A)')     &
        vert,' => ( ', self%vert_coords(1,vert), &
        ', ', self%vert_coords(2,vert), ' )'
    write(stdout,('(A)')) trim(tmp_str)
  end do

  write(stdout,'(A)') ''
  write(stdout,'(A)') '================================='
  write(stdout,'(A)') ' Mappings to other meshes'
  write(stdout,'(A)') '================================='

  do i=1, self%nmaps
    global_mesh_map  => self%global_mesh_maps%get_global_mesh_map(1,i+1)
    nsource          = global_mesh_map%get_nsource_cells()
    ntarget_per_cell = global_mesh_map%get_ntarget_cells_per_source_cell()
    if (allocated(cell_map)) deallocate(cell_map)
    if (allocated(tmp_map)) deallocate(tmp_map)
    allocate(cell_map(ntarget_per_cell, nsource))
    allocate(tmp_map(ntarget_per_cell, 1))
    do j=1, nsource
      call global_mesh_map%get_cell_map([j], tmp_map)
      cell_map(:, j) = tmp_map(:,1)
    end do
    write(stdout,'(4(A,I0),A)')                                                        &
        trim(self%mesh_name)//'(', self%edge_cells_x, ',', self%edge_cells_y,') => '// &
        trim(self%target_mesh_names(i))//'(', self%target_edge_cells_x(i), ',',        &
        self%target_edge_cells_y(i), '):'
    do j=1, nsource
      write(stdout,'(I7,A,10(I0," "))') j,' => ' , cell_map(:, j)
    end do
  end do

  write(stdout,'(A)')    "====END DEBUG INFO===="

  return
end subroutine write_mesh

subroutine gen_planar_final(self)

    implicit none

    type (gen_planar_type), intent(inout) :: self

    call self%clear()

    return
end subroutine gen_planar_final

!-------------------------------------------------------------------------------
end module gen_planar_mod
