!-----------------------------------------------------------------------------
! (c) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Cell_locator API
!>

module cell_locator_api_mod

  use constants_mod,                 only: i_def, l_def, r_def, PI
  use field_mod,                     only: field_type, field_proxy_type
  use finite_element_config_mod,     only: cellshape, &
                                           cellshape_quadrilateral
  use base_mesh_config_mod,          only: geometry, &
                                           geometry_spherical
  use fs_continuity_mod,             only: W0, W3
  use mesh_mod,                      only: mesh_type
  use mesh_collection_mod,           only: mesh_collection
  use function_space_mod,            only: function_space_type
  use function_space_collection_mod, only: function_space_collection
  use project_output_mod,            only: project_output
  use runtime_constants_mod,         only: get_coordinates
  use log_mod,                       only: log_event,       &
                                           LOG_LEVEL_ERROR, &
                                           LOG_LEVEL_INFO
  use cell_locator_config_mod,       only: input_target_points_filename, &
                                           vtk_grid_filename, &
                                           num_cells_per_bucket, &
                                           rescale_z, z_lo, z_hi
  use psykal_lite_mod,               only: invoke_nodal_coordinates_kernel, &
                                           invoke_pointwise_convert_xyz2llr

  use mpi_mod,                       only: get_comm_size, get_comm_rank

  use netcdf, only: nf90_open, nf90_write, nf90_nowrite, nf90_noerr,        &
                    nf90_strerror, nf90_put_var, nf90_get_var, nf90_put_att, &
                    nf90_def_var, nf90_inq_varid, nf90_int, nf90_double,     &
                    nf90_clobber, nf90_enddef, nf90_inquire_dimension,       &
                    nf90_inq_dimid, nf90_def_dim, nf90_create, nf90_close,   &
                    nf90_inquire_variable, nf90_int64, nf90_hdf5

  use, intrinsic :: iso_c_binding, only: c_size_t, c_double, c_int, &
                                         c_long_long, c_ptr, c_size_t

  use mnt_celllocator_capi_mod,       only: mnt_celllocator_new, &
                                            mnt_celllocator_del, &
                                            mnt_celllocator_setpointsptr, &
                                            mnt_celllocator_build, &
                                            mnt_celllocator_find, &
                                            mnt_celllocator_dumpgrid, &
                                            mnt_celllocator_interppoint

  implicit none

  type cell_locator_api_type

    private

    !=========================================================================
    ! Copy of mesh vertices which will be passed as a pointer to the C++ code
    !=========================================================================
    !< Source grid vertices
    real(c_double), allocatable       :: vert_coords(:)

    !=========================================================================
    ! Opaque handle:
    !=========================================================================
    !< Address of pointer to C++ object
    type(c_ptr)                       :: address_of_cpp_ptr

    !=========================================================================
    ! Interpolation results:
    !=========================================================================
    !< Target points, dimensions are (3, npts)
    real(r_def), allocatable          :: target_points(:, :)

    !< Parametric coordinates, dimensions are (3, npts)
    real(r_def), allocatable          :: pcoords(:, :)

    !< Interpolation errors, dimension (npts)
    real(r_def), allocatable          :: dist_error_square(:)

    !< Cell indices, 0 based, dimension (npts)
    integer(c_long_long), allocatable :: cell_ids_0(:)

    !< Point Ids, 0 based, dimension (npts)
    integer(i_def), allocatable       :: point_ids_0(:)

    !< Number of target points found
    integer(i_def)                    :: number_points_found

    !==========================================================================
    ! MPI:
    !==========================================================================
    character(len=9)                  :: my_mpi_rank_string
    integer(i_def)                    :: my_mpi_rank
    integer(i_def)                    :: num_mpi_ranks

  contains

    procedure :: clear => cell_locator_api_clear
    procedure :: build => cell_locator_api_build
    procedure :: find => cell_locator_api_find
    procedure :: read_points => cell_locator_api_read_points
    procedure :: get_number_of_target_points => &
                   cell_locator_api_get_number_of_target_points
    procedure :: get_target_point => cell_locator_api_get_target_point
    procedure :: get_proc_filename => cell_locator_api_get_proc_filename
    procedure :: set_result => cell_locator_api_set_result
    procedure :: write_results => cell_locator_api_write_results

    ! Intel compiler has issues with the destructor
    !final :: cell_locator_api_destructor

  end type cell_locator_api_type

  interface cell_locator_api_type
    module procedure cell_locator_api_constructor
  end interface


  ! Methods
  contains

  !> @brief Constructor
  !> @param[inout] self instance of cell_locator_api_type
  !> @param[out] ier error code (0 = OK)
  function cell_locator_api_constructor( ier ) result( self )

    implicit none
    type(cell_locator_api_type)                 :: self
    integer(i_def), intent(out)                 :: ier

    ier = mnt_celllocator_new( self%address_of_cpp_ptr )

    ! Read the target points from configuration file. This will allocate
    ! self%target_points and self%cell_ids_0, etc.
    call cell_locator_api_read_points( self, input_target_points_filename, &
                                       'target_points', ier )
    if ( ier /= 0 ) then
      call log_event( &
          'cell_locator_api_constructor: Error occurred when reading target ' &
           // 'point file "' &
           // input_target_points_filename // '"', LOG_LEVEL_ERROR )
    endif

    self%number_points_found = 0

  end function cell_locator_api_constructor

  !> @brief Reclaim memory
  !> @param[inout] self instance of cell_locator_api_type
  !> @param[out] ier error code (0 = OK)
  subroutine cell_locator_api_clear( self, ier )

    implicit none
    class(cell_locator_api_type), intent(inout) :: self
    integer(i_def), intent(out)                 :: ier

    if ( allocated( self%vert_coords ) ) deallocate( self%vert_coords )
    if ( allocated( self%pcoords ) ) deallocate( self%pcoords )
    if ( allocated( self%dist_error_square ) ) &
                    deallocate( self%dist_error_square )
    if ( allocated( self%cell_ids_0 ) ) deallocate( self%cell_ids_0 )
    if ( allocated( self%point_ids_0 ) ) deallocate( self%point_ids_0 )
    if ( allocated( self%target_points) ) deallocate( self%target_points )

    ier = mnt_celllocator_del( self%address_of_cpp_ptr )

  end subroutine cell_locator_api_clear

  !> @brief Destructor
  !> @param[inout] self instance of cell_locator_api_type
  subroutine cell_locator_api_destructor( self )

    implicit none
    type(cell_locator_api_type), intent(inout) :: self
    integer(i_def)                             :: ier

    call self%clear( ier )

  end subroutine cell_locator_api_destructor


  !> @brief Build the cell locator object
  !> @param[inout] self instance of cell_locator_api_type
  !> @param[in] mesh_id mesh ID
  !> @param[out] ier error code (0 = OK)
  subroutine cell_locator_api_build( self, mesh_id, ier )

    implicit none
    class(cell_locator_api_type), intent(inout) :: self
    integer(i_def), intent(in)                  :: mesh_id
    integer(i_def), intent(out)                 :: ier

    ! Local constants and variables
    type(mesh_type), pointer                    :: mesh => null()
    integer(i_def)                              :: i, j, k, idx, nlayers
    integer(i_def)                              :: ncells_vtk, ncells_local
    integer(i_def)                              :: ndofs_per_cell, idof
    type(function_space_type), pointer          :: output_field_fs => null()
    type(field_type), pointer                   :: chi(:) => null()
    type(field_type)                            :: coord_output(3)
    type(field_proxy_type), target              :: proxy_coord_output(3)
    integer(i_def), pointer                     :: map_f(:) => null()
    real(r_def)                                 :: lon_base, dlon, minz, maxz
    integer(i_def), dimension(1)                :: min_idx
    integer(i_def)                              :: ndigits_max
    character(len=9)                            :: fmt
    character(len=1024)                         :: vtk_grid_filename_proc
    character(len=128)                          :: msg
    real(r_def)                                 :: x, y, z, r, rho, lam, the

    ! Interrogate mesh to get basic dimensions
    mesh => mesh_collection%get_mesh( mesh_id )

    self%my_mpi_rank = get_comm_rank()
    self%num_mpi_ranks = get_comm_size()

    ! Construct a string of the type '0012345' where 12345 is
    ! the MPI rank (number of preceding zeros are dependent on number
    ! of ranks)
    self%my_mpi_rank_string = ''

    ! Max number of digits
    ndigits_max = 1
    if ( self%num_mpi_ranks > 1 ) then
      ndigits_max = int( log10( real( self%num_mpi_ranks - 1 ) ) ) + 1
    endif

    write( fmt, '("(i",i0,".",i0,")")' ) ndigits_max, ndigits_max

    ! Rank with leading 0s
    write( self%my_mpi_rank_string, fmt ) self%my_mpi_rank

    ! Partitions contain local cells, outer halos, and ghost cells
    ! Last edge will give us only local cells
    ncells_local = mesh%get_last_edge_cell()
    nlayers = mesh%get_nlayers()
    nullify(mesh)

    ! Only transfer local cells to VTK, ignore halo and LFRic ghost cells
    ncells_vtk = ncells_local*nlayers

    ! Determine vertex coordinates by extracting dof coordinates from a W0
    ! field, 0 for linear elements, W0 for nodal
    output_field_fs => function_space_collection%get_fs( mesh_id, 0, W0 )
    do i = 1, 3
      ! Create 3 new scalar fields
      call coord_output(i)%initialise( vector_space = output_field_fs )
      ! Access the field through a proxy
      proxy_coord_output(i) = coord_output(i)%get_proxy()
    end do
    nullify(output_field_fs)

    ! The dofs are ordered local - annexed - halo
    ndofs_per_cell = proxy_coord_output(1)%vspace%get_ndf()
    write( msg, '(A, I4, A, I6, A, I4)' ) 'ndofs_per_cell = ', ndofs_per_cell, &
                ' ncells_local = ', ncells_local, ' nlayers = ', nlayers
    call log_event( msg, LOG_LEVEL_INFO )

    ! Convert field to physical nodal output & sample chi on nodal points;
    ! convert result to lon, lat, rad if requested
    chi => get_coordinates()
    call invoke_nodal_coordinates_kernel( coord_output, chi )

    nullify( chi )

    ! Assemble vertex coordinates
    allocate( self%vert_coords(ncells_local*nlayers*ndofs_per_cell*3) )

    ! Loop over all cells and their dofs/vertices, extract dof coordinates
    ! and store their IDs
    ! Local cells are those with the lowest local cell indices
    idx = 0
    do i = 1, ncells_local
      map_f => proxy_coord_output( 1 )%vspace%get_cell_dofmap( i )
      do k = 1, nlayers
        do j = 1, ndofs_per_cell
          idof = map_f(j) + k - 1
          self%vert_coords( idx + 1 ) = proxy_coord_output( 1 )%data( idof )
          self%vert_coords( idx + 2 ) = proxy_coord_output( 2 )%data( idof )
          self%vert_coords( idx + 3 ) = proxy_coord_output( 3 )%data( idof )
          idx = idx + 3
        end do
      end do
    end do
    nullify( map_f )

    if (  geometry == geometry_spherical ) then

      ! Convert to spherical geometry and compute min/max of elevation. In principle we
      ! could have used "call invoke_pointwise_convert_xyz2llr( coord_output )" to convert
      ! the coordinates from cartesian to lon-lat-elevation but this would have required us
      ! to invoke:
      ! do i = 1, 3
      !   call coord_output(i)%halo_exchange(depth=1)
      ! end do
      ! to synchronise the halo. To avoid the communication cost from halo exchange, we
      ! perform our own conversion (taking advantage of computing the min/max of elevation
      ! along the way)
      minz = huge( 1._r_def )
      maxz = -huge( 1._r_def )
      idx = 0
      do i = 1, ncells_local
        do k = 1, nlayers
          do j = 1, ndofs_per_cell

            x = self%vert_coords( idx + 1 )
            y = self%vert_coords( idx + 2 )
            z = self%vert_coords( idx + 3 )

            rho = sqrt(x**2 + y**2)
            r = sqrt(rho**2 + z**2)
            the = atan2(z, rho)
            lam = modulo(atan2(y, x), 2._r_def * PI)

            self%vert_coords( idx + 1 ) = lam
            self%vert_coords( idx + 2 ) = the
            self%vert_coords( idx + 3 ) = r

            idx = idx + 3

            minz = min( minz, r )
            maxz = max( maxz, r )
          enddo

        enddo
      enddo

      ! Add/subtract periodic length.
      ! minimise the differences in longitude between a vertex and its base
      ! (1st) vertex. This ensures that each cell is a positive are/volume.
      idx = 0
      do i = 1, ncells_local
        do k = 1, nlayers

          ! First vertex acts as base point
          lon_base = self%vert_coords( idx + 1 )
          idx = idx + 3

          ! Iterate over the remaining vertices
          do j = 2, ndofs_per_cell

            dlon = self%vert_coords( idx + 1 ) - lon_base

            min_idx = minloc( [ abs( dlon - 2._r_def*PI ), &
                                abs( dlon ), &
                                abs( dlon + 2._r_def*PI )] )

            ! Correct the longitude
            self%vert_coords( idx + 1 ) = self%vert_coords( idx + 1 ) + &
                                          ( min_idx(1) - 2 )*2._r_def*PI

            idx = idx + 3
          enddo
        enddo
      enddo

      ! Rescale the vertical axis, if desired
      if ( rescale_z ) then
        ! Reset the elevations
        self%vert_coords( 3::3 ) = z_lo + ( z_hi - z_lo ) * &
            (  self%vert_coords(3::3) - minz ) / ( maxz - minz )
      endif

    endif ! end of spherical geometry

    ! Build the mesh by setting the vertices of each cell
    ier = mnt_celllocator_setpointsptr( self%address_of_cpp_ptr, &
                                        ndofs_per_cell, &
                                        int( ncells_vtk, kind=c_size_t ), &
                                        self%vert_coords(1) )
    if ( ier /= 0 ) then
      call log_event( &
        'cell_locator_api_build: Error after calling setpoints', &
        LOG_LEVEL_ERROR )
    endif

    ! Build the locator
    ier = mnt_celllocator_build( self%address_of_cpp_ptr, &
             num_cells_per_bucket )
    if ( ier /= 0 ) then
      call log_event( &
          'cell_locator_api_build: Error after calling build', &
          LOG_LEVEL_ERROR )
    endif

    ! Save the grid in VTK file
    if ( len_trim( vtk_grid_filename ) > 0 ) then
      vtk_grid_filename_proc = &
         self%get_proc_filename( trim( vtk_grid_filename ) )
      ier = mnt_celllocator_dumpgrid( self%address_of_cpp_ptr, &
         trim( adjustl( vtk_grid_filename_proc ) ), &
         len( trim( vtk_grid_filename_proc ), c_size_t ) )
      if ( ier /= 0 ) then
        call log_event( 'cell_locator_api_build: Error after calling ' &
             // 'dumpgrid', &
             LOG_LEVEL_ERROR )
      endif
    endif

  end subroutine cell_locator_api_build

  !> @brief Look for the cell that contains a target point
  !> @param[inout] self instance of cell_locator_api_type
  !> @param[in] point target point
  !> @param[out] cell_id_0 cell ID (>= 0 if found)
  !> @param[out] pcoords unit cell parametric coordinates
  !> @param[out] dist_error_square square of interpolation error in
  !              physical space
  !> @param[out] ier error code (0 = OK)
  subroutine cell_locator_api_find( self, point, cell_id_0, pcoords, &
                                    dist_error_square, ier )

    implicit none
    class(cell_locator_api_type), intent(inout) :: self
    real(r_def), intent(in)                     :: point(:)
    integer(c_long_long), intent(out)           :: cell_id_0
    real(r_def), intent(out)                    :: pcoords(:)
    real(r_def), intent(out)                    :: dist_error_square
    integer(i_def), intent(out)                 :: ier

    ! Local variables
    real(r_def)                                 :: interp_point1(3), &
                                                   target_point1(3), &
                                                   pcoords1(3)
    character(len=16)                           :: error_str

    ! Initialise to bad values
    pcoords1(1:3) = -1._r_def
    dist_error_square = huge(1._r_def)
    cell_id_0 = -999

    ! Ensure that arget point and pcoords are contiguous in memory by
    ! copying to local variables
    target_point1(1:3) = point(1:3)

    ! Find the cell, must pass the first index for arrays
    ier = mnt_celllocator_find( self%address_of_cpp_ptr, target_point1(1), &
                                cell_id_0, pcoords1(1) )

    if ( ier == 0 .and. cell_id_0 >= 0 ) then

      ! Copy back the result
      pcoords(1:3) = pcoords1(1:3)

      ! Compute the interpolation error
      ier = mnt_celllocator_interppoint( self%address_of_cpp_ptr, cell_id_0, &
                                         pcoords1(1), interp_point1(1) )
      if (ier == 0) then
        dist_error_square = dot_product( interp_point1 - point, &
                                         interp_point1 - point )
      else
        write(error_str, '(I16)') ier
        call log_event( &
            'cell_locator_api_find: Failure after calling ' &
            // 'mnt_celllocator_interp_point, error code is "' &
            // trim( adjustl( error_str ) )//'"', LOG_LEVEL_INFO )
      endif

    else

      ! Not finding the point, may be because running in parallel (we expect
      ! many points not found when the local domain is smaller than the
      ! global domain)

      if (ier /= 0) then
        ! Wondering what error could trigger this?
        write(error_str, '(I16)') ier
        call log_event( &
            'cell_locator_api_find: Failure after calling ' &
            // 'mnt_celllocator_find, ' &
            // 'error code is "' &
            // trim( adjustl( error_str ) )//'"', LOG_LEVEL_INFO )
      endif

    endif

  end subroutine cell_locator_api_find

  !> @brief Read target points from NetCDF file
  !> @param[inout] self instance of cell_locator_api_type
  !> @param[in] file_name file name
  !> @param[in] pont_var_name  variable name of the target point array
  !> @param[out] ier error code (0 = OK)
  subroutine cell_locator_api_read_points( self, file_name, &
                                           point_var_name, ier )

    implicit none
    class(cell_locator_api_type), intent(inout) :: self
    character(len=*), intent(in)                :: file_name
    character(len=*), intent(in)                :: point_var_name
    integer(i_def), intent(out)                 :: ier

    ! Local variables
    integer(i_def) :: ncid, varid, ndimids, ndims, npts, xtype
    integer(i_def) :: dimdids(2)


    ! Open file
    ier = nf90_open( trim(file_name), nf90_nowrite, ncid )
    if ( ier /= nf90_noerr ) then
      call log_event( 'cell_locator_api_read_points: Could not open file "' &
                       //trim( file_name )//'"', LOG_LEVEL_ERROR )
    endif

    ! Get the variable ID
    ier = nf90_inq_varid( ncid, trim(point_var_name), varid )
    if ( ier /= nf90_noerr ) then
      call log_event( 'cell_locator_api_read_points: Could not find ' &
        // 'variable "' &
        //trim( point_var_name )//'"', LOG_LEVEL_ERROR )
      ier = nf90_close( ncid )
    endif


    ! Check
    ier = nf90_inquire_variable( ncid=ncid, varid=varid, &
                                 xtype=xtype, ndims=ndimids )

    if ( ndimids /= 2 ) then
      call log_event( 'cell_locator_api_read_points: variable "' &
                       //trim( point_var_name )//'" must have rank 2', &
                       LOG_LEVEL_ERROR )
      ier = nf90_close( ncid )
    endif

    if ( xtype /= nf90_double ) then
      call log_event( 'cell_locator_api_read_points: variable "' &
                      //trim( point_var_name )//'" must be nf90_double', &
                      LOG_LEVEL_ERROR )
      ier = nf90_close( ncid )
    endif

    ! Get the dimensions
    ier = nf90_inquire_variable( ncid=ncid, varid=varid, dimids=dimdids )
    ier = nf90_inquire_dimension( ncid, dimdids(1), len=ndims )
    ier = nf90_inquire_dimension( ncid, dimdids(2), len=npts )
    if ( ndims /= 3 ) then
      call log_event( &
            'cell_locator_api_read_points: first dimension of variable "' &
            //trim( point_var_name )//'" must be 3 (3D)', LOG_LEVEL_ERROR )
      ier = nf90_close( ncid )
    endif

    ! Allocate
    allocate( self%target_points(ndims, npts) )
    allocate( self%cell_ids_0(npts) )
    allocate( self%point_ids_0(npts) )
    allocate( self%dist_error_square(npts) )
    allocate( self%pcoords(ndims, npts) )

    ! Initialise
    self%target_points = huge(1._r_def)
    self%cell_ids_0 = -1
    self%point_ids_0 = -1
    self%dist_error_square = huge(1._r_def)
    self%pcoords = huge(1._r_def)

    ! Read variable
    ier = nf90_get_var( ncid, varid, self%target_points )
    if ( ier /= nf90_noerr ) then
      call log_event( &
           'cell_locator_api_read_points: could not read variable "' &
           //trim( point_var_name )//'"', LOG_LEVEL_ERROR )
      ier = nf90_close( ncid )
    endif

    ! Close file
    ier = nf90_close( ncid )

  end subroutine cell_locator_api_read_points

  !> @brief Write results to NetCDF file
  !> @param[inout] self instance of cell_locator_api_type
  !> @param[in] file_name file name
  !> @param[out] ier error code (0 = OK)
  subroutine cell_locator_api_write_results( self, file_name, ier )

    implicit none
    class(cell_locator_api_type), intent(inout) :: self
    character(len=*), intent(in)                :: file_name
    integer(i_def), intent(out)                 :: ier

    ! Local variables
    integer(i_def) :: ncid, pt_dim, three_dim, cellids_var, pointids_var, &
                    pcoords_var, dist_error_square_var

    integer(i_def) :: oned_dims(1), twod_dims(2)

    !< NetCDF file name with MPI rank attached
    character(len=1024)                         :: file_name_rk

    file_name_rk = file_name
    if ( self%num_mpi_ranks > 1 ) then
      ! Parallel execution, save the results in file, one per process
      file_name_rk = self%get_proc_filename( file_name )
      call log_event( 'cell_locator_api_write_results: writing file to ' &
             // file_name_rk, LOG_LEVEL_INFO )
    endif

    ! Create file
    ier = nf90_create( trim( file_name_rk ), NF90_HDF5, ncid )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not create file "' &
        // trim( file_name )//'"', LOG_LEVEL_ERROR )
    endif

    ! Create dimensions
    ier = nf90_def_dim( ncid, 'number_points_found', &
        self%number_points_found, pt_dim ) ! 3D
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not define dim "npts"', &
        LOG_LEVEL_ERROR )
    endif
    ier = nf90_def_dim( ncid, "three", 3, three_dim )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not define dim "three"', &
        LOG_LEVEL_ERROR )
    endif

    ! Create variables
    oned_dims(1) = pt_dim
    twod_dims(1) = three_dim
    twod_dims(2) = pt_dim

    ier = nf90_def_var( ncid, "point_ids_0", NF90_INT, oned_dims, &
                        pointids_var )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not define var ' &
        // '"point_ids_0"', &
        LOG_LEVEL_ERROR )
    endif
    ier = nf90_def_var( ncid, "cell_ids_0", NF90_INT64, oned_dims, &
                        cellids_var )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not define var '&
        // '"cell_ids_0"', &
        LOG_LEVEL_ERROR )
    endif
    ier = nf90_def_var( ncid, "pcoords", NF90_DOUBLE, twod_dims, pcoords_var )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not define var "pcoords"', &
        LOG_LEVEL_ERROR )
    endif
    ier = nf90_def_var( ncid, "dist_error_square", NF90_DOUBLE, oned_dims, &
          dist_error_square_var )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not define var ' &
        // '"dist_error_square"', &
        LOG_LEVEL_ERROR )
    endif

    ! End define mode. This tells netCDF we are done defining metadata.
    ier = nf90_enddef( ncid )

    ! Write the data
    ier = nf90_put_var( ncid, pointids_var, &
          self%point_ids_0(1:self%number_points_found) )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not write ' &
        // '"point_ids_0"', &
        LOG_LEVEL_ERROR )
    endif
    ier = nf90_put_var( ncid, cellids_var, &
            self%cell_ids_0(1:self%number_points_found) )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not write ' &
        // '"cell_ids_0"', &
        LOG_LEVEL_ERROR )
    endif
    ier = nf90_put_var( ncid, pcoords_var, &
            self%pcoords(:, 1:self%number_points_found) )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not write "pcoords"', &
        LOG_LEVEL_ERROR )
    endif
    ier = nf90_put_var( ncid, dist_error_square_var, &
            self%dist_error_square(1:self%number_points_found) )
    if ( ier /= nf90_noerr ) then
      call log_event( &
        'cell_locator_api_write_results: Could not write ' &
        // '"dist_error_square"', &
        LOG_LEVEL_ERROR )
    endif

    ! Close file
    ier = nf90_close( ncid )

  end subroutine cell_locator_api_write_results

  !> @brief Get the number of target points
  !> @param[inout] self instance of cell_locator_api_type
  !> @return number
  function cell_locator_api_get_number_of_target_points( self ) &
             result( num )

    implicit none
    class(cell_locator_api_type), intent(inout) :: self
    integer(i_def)                              :: num

    num = size( self%target_points, 2 )

  end function cell_locator_api_get_number_of_target_points

  !> @brief Get the target point
  !> @param[inout] self instance of cell_locator_api_type
  !> @param[in] index of target point
  !> @return x, y, z
  function cell_locator_api_get_target_point( self, index ) &
             result( target_point )

    implicit none
    class(cell_locator_api_type), intent(inout) :: self
    integer(i_def), intent(in)                  :: index
    real(r_def)                                 :: target_point(3)

    target_point = self%target_points(:, index)

  end function cell_locator_api_get_target_point

  !> @brief Get the processor dependent file name
  !> @param[inout] self instance of cell_locator_api_type
  !> @param[in] file_name file name file name without process number embedded
  !> @return file name with process number embedded, e.g. toto_012.nc
  function cell_locator_api_get_proc_filename( self, file_name ) &
             result( indexed_filename )

    implicit none
    class(cell_locator_api_type), intent(inout) :: self
    character(len=*), intent(in)                :: file_name
    character(len=24)                           :: indexed_filename

    ! Local variables
    character(len=1025)                         :: base_filename
    character(len=8)                            :: suffix
    integer(i_def)                              :: n, i

    ! Find the suffix and base name of the file (the part before the suffix)
    suffix = ''
    base_filename = trim( file_name )
    n = len_trim( file_name )
    do i = n - 1, 1, -1
      if ( file_name(i:i) == '.' ) then
        suffix = file_name(i + 1:n)
        base_filename = file_name(1:i - 1)
      endif
    enddo

    indexed_filename = trim( base_filename ) // '_' &
                       // trim( self%my_mpi_rank_string ) // '.' // &
                       trim( suffix )

  end function cell_locator_api_get_proc_filename

  !> @brief Set/store the partial result
  !> @param[inout] self instance of cell_locator_api_type
  !> @param[in] count current target index on this processor
  !> @param[in] ipoint global target point index (1-based)
  !> @param[in] cell_id_0 current cell index (0-based)
  !> @param[in] pcoords current cell parametric coordinates
  !> @param[in] dist_error_square square distance of error
  subroutine cell_locator_api_set_result( self, count, ipoint, cell_id_0, &
                                          pcoords, dist_error_square )

    implicit none
    class(cell_locator_api_type), intent(inout) :: self
    integer(i_def), intent(in)                  :: count
    integer(i_def), intent(in)                  :: ipoint
    integer(c_long_long), intent(in)            :: cell_id_0
    real(r_def), intent(in)                     :: pcoords(3)
    real(r_def), intent(in)                     :: dist_error_square

    self%point_ids_0(count) = ipoint - 1
    self%cell_ids_0(count) = cell_id_0
    self%pcoords(:, count) = pcoords
    self%dist_error_square(count) = dist_error_square
    self%number_points_found = count

  end subroutine cell_locator_api_set_result


end module cell_locator_api_mod
