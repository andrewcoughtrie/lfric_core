!-----------------------------------------------------------------------------
! (c) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file LICENCE which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @brief init and final for fem specific choices for model

!> @details Create and destroy a collection of function spaces and the coordinate
!>          field (chi)

module create_fem_mod

  use chi_transform_mod,              only : init_chi_transforms, &
                                             final_chi_transforms
  use constants_mod,                  only : i_def, i_native, l_def
  use base_mesh_config_mod,           only : geometry, geometry_planar
  use finite_element_config_mod,      only : element_order, coordinate_order, &
                                             spherical_coord_order,           &
                                             spherical_coord_system,          &
                                             spherical_coord_system_xyz
  use halo_routing_collection_mod,    only : halo_routing_collection_type, &
                                             halo_routing_collection
  use field_mod,                      only : field_type
  use fs_continuity_mod,              only : W0, W1, W2, W3, Wtheta, Wchi
  use formulation_config_mod,         only : l_multigrid
  use function_space_mod,             only : function_space_type
  use function_space_collection_mod,  only : function_space_collection_type, &
                                             function_space_collection
  use function_space_chain_mod,       only : function_space_chain_type,         &
                                             single_layer_function_space_chain, &
                                             multigrid_function_space_chain,    &
                                             W2_multigrid_function_space_chain, &
                                             wtheta_multigrid_function_space_chain
  use assign_coordinate_field_mod,    only : assign_coordinate_field
  use log_mod,                        only : log_event,         &
                                             LOG_LEVEL_INFO,    &
                                             LOG_LEVEL_ERROR,   &
                                             log_scratch_space

  implicit none

  private
  public :: init_fem, final_fem

  contains
    !==================================================================================
    !> @brief Initialises the coordinate fields (chi) and FEM components
    !> @param[in]     mesh_id               Mesh id used for chi field
    !> @param[in,out] chi_xyz               XYZ Coordinate field
    !> @param[in,out] chi_sph               Spherically-based Coordinate field
    !> @param[in,out] panel_id              Field giving the ID of the mesh panels
    !> @param[in]     shifted_mesh_id       Optional, mesh id used for shifted chi field
    !> @param[in,out] shifted_chi_xyz       Optional, XYZ spatial coordinates of vertically shifted mesh
    !> @param[in,out] shifted_chi_sph       Optional, Spherically-based coordinates of vertically shifted mesh
    !> @param[in]     double_level_mesh_id  Optional, mesh id used for double-level chi field
    !> @param[in,out] double_level_chi_xyz  Optional, XYZ spatial coordinates of double level mesh
    !> @param[in,out] double_level_chi_sph  Optional, Spherically-based coordinates of double level mesh
    !> @param[in]     multigrid_mesh_ids    Optional, mesh id array for multigrid function spaces chain
    !> @param[in]     multigrid_2d_mesh_ids Optional, 2d-mesh id array for multigrid function spaces chain
    !==================================================================================
    subroutine init_fem( mesh_id, chi_xyz, chi_sph, panel_id,               &
                         shifted_mesh_id, shifted_chi_xyz, shifted_chi_sph, &
                         double_level_mesh_id, double_level_chi_xyz,        &
                         double_level_chi_sph,                              &
                         multigrid_mesh_ids, multigrid_2D_mesh_ids          )

    implicit none

    ! Coordinate field
    integer(i_def),   intent(in)    :: mesh_id
    type(field_type), intent(inout) :: chi_xyz(:)
    type(field_type), intent(inout) :: chi_sph(:)
    type(field_type), intent(inout) :: panel_id

    integer(i_def),   optional, intent(in)    :: shifted_mesh_id
    type(field_type), optional, intent(inout) :: shifted_chi_xyz(:)
    type(field_type), optional, intent(inout) :: shifted_chi_sph(:)
    integer(i_def),   optional, intent(in)    :: double_level_mesh_id
    type(field_type), optional, intent(inout) :: double_level_chi_xyz(:)
    type(field_type), optional, intent(inout) :: double_level_chi_sph(:)
    integer(i_def),   optional, intent(in)    :: multigrid_mesh_ids(:)
    integer(i_def),   optional, intent(in)    :: multigrid_2d_mesh_ids(:)

    logical(l_def) :: create_shifted_chi        = .false.
    logical(l_def) :: create_double_level_chi   = .false.
    logical(l_def) :: create_multigrid_fs_chain = .false.

    integer(i_native), parameter :: fs_list(5) = [W0, W1, W2, W3, Wtheta]

    type(function_space_type), pointer :: fs => null()
    type(function_space_type), pointer :: shifted_fs => null()
    type(function_space_type), pointer :: double_level_fs => null()

    integer(i_native) :: fs_index

    integer(i_def) :: chi_xyz_space
    integer(i_def) :: chi_sph_space
    integer(i_def) :: coord
    integer(i_def) :: mesh_ctr

    ! Set control flags
    !=================================================================
    if ( l_multigrid                 .and. &
         present(multigrid_mesh_ids) .and. &
         present(multigrid_2d_mesh_ids) ) create_multigrid_fs_chain = .true.

    if ( present(shifted_mesh_id) .and. &
         present(shifted_chi_xyz) ) create_shifted_chi = .true.

    if ( present(double_level_mesh_id) .and. &
         present(double_level_chi_xyz) ) create_double_level_chi = .true.


    ! Create a collection for holding FEM info that is specific to a field
    allocate( halo_routing_collection, &
              source = halo_routing_collection_type() )

    call log_event( 'FEM specifics: creating function spaces...', LOG_LEVEL_INFO )

    allocate( function_space_collection, &
              source = function_space_collection_type() )

    ! Create function spaces from W0 to Wtheta
    ! =========================================
    do fs_index = 1, size(fs_list)
      fs => function_space_collection%get_fs( mesh_id,       &
                                              element_order, &
                                              fs_list(fs_index) )
    end do


    ! Create model coordinate field
    ! =========================================

    ! Initialise coordinate transformations
    call init_chi_transforms()

    if ( coordinate_order == 0 ) then
      chi_xyz_space = W0
      call log_event( "FEM specifics: Computing W0 xyz coordinate fields", LOG_LEVEL_INFO )
    else
      chi_xyz_space = Wchi
      call log_event( "FEM specifics: Computing Wchi xyz coordinate fields", LOG_LEVEL_INFO )
    end if

    fs => function_space_collection%get_fs(mesh_id, coordinate_order, chi_xyz_space)

    do coord = 1, size(chi_xyz)
      call chi_xyz(coord)%initialise(vector_space = fs )
    end do

    fs => function_space_collection%get_fs(mesh_id, 0, W3)
    call panel_id%initialise(vector_space = fs )

    call assign_coordinate_field(chi_xyz, panel_id, mesh_id, spherical_coords=.false.)

    if ( spherical_coord_order < 1 ) then
      call log_event( "Error: Spherical coordinate order is less than 1", LOG_LEVEL_ERROR )
    else
      chi_sph_space = Wchi
      call log_event( "FEM specifics: Computing Wchi sph coordinate fields", LOG_LEVEL_INFO )
    end if
    fs => function_space_collection%get_fs(mesh_id, spherical_coord_order, chi_sph_space)

    ! Check that the geometry is compatible with the coordinate system
    if ( (geometry == geometry_planar) .and. &
         (spherical_coord_system /= spherical_coord_system_xyz) ) then
        call log_event( "Error: For planar geometry must use spherical_coord_system = xyz", LOG_LEVEL_ERROR )
    end if

    do coord = 1, size(chi_sph)
      call chi_sph(coord)%initialise(vector_space = fs )
    end do

    call assign_coordinate_field(chi_sph, panel_id, mesh_id, spherical_coords=.true.)

    ! Create shifted mesh coordinate field.
    ! =========================================
    if ( create_shifted_chi ) then
      call log_event( "FEM specifics: Making shifted level mesh spaces",     &
                      LOG_LEVEL_INFO )
      shifted_fs => function_space_collection%get_fs( shifted_mesh_id,       &
                                                      coordinate_order,      &
                                                      chi_xyz_space )
      do coord = 1, size(chi_xyz)
        call shifted_chi_xyz(coord)%initialise(vector_space = shifted_fs)
      end do

      shifted_fs => function_space_collection%get_fs( shifted_mesh_id,       &
                                                      spherical_coord_order, &
                                                      chi_sph_space )
      do coord = 1, size(chi_sph)
        call shifted_chi_sph(coord)%initialise(vector_space = shifted_fs)
      end do

      call assign_coordinate_field(shifted_chi_xyz, panel_id,                &
                                   shifted_mesh_id, spherical_coords=.false.)

      call assign_coordinate_field(shifted_chi_sph, panel_id, &
                                   shifted_mesh_id, spherical_coords=.true.)

      nullify(shifted_fs)
    end if

    ! Create double level coordinate field.
    ! =========================================
    if ( create_double_level_chi ) then
      call log_event( "FEM specifics: Making double level mesh spaces", &
                      LOG_LEVEL_INFO )

      double_level_fs =>                                                &
          function_space_collection%get_fs( double_level_mesh_id,       &
                                            coordinate_order,           &
                                            chi_xyz_space )
      do coord = 1, size(chi_xyz)
        call double_level_chi_xyz(coord)%initialise(vector_space = double_level_fs)
      end do

      double_level_fs =>                                                &
          function_space_collection%get_fs( double_level_mesh_id,       &
                                            spherical_coord_order,      &
                                            chi_sph_space )
      do coord = 1, size(chi_sph)
        call double_level_chi_sph(coord)%initialise(vector_space = double_level_fs)
      end do

      call assign_coordinate_field(double_level_chi_xyz, panel_id,      &
                                   double_level_mesh_id, spherical_coords=.false.)

      call assign_coordinate_field(double_level_chi_sph, panel_id,      &
                                   double_level_mesh_id, spherical_coords=.true.)

      nullify( double_level_fs )
    end if

    ! Create function space chains
    ! =========================================
    if ( create_multigrid_fs_chain ) then

      multigrid_function_space_chain        = function_space_chain_type()
      w2_multigrid_function_space_chain     = function_space_chain_type()
      wtheta_multigrid_function_space_chain = function_space_chain_type()

      write(log_scratch_space,'(A,I1,A)')                     &
          'Intialising MultiGrid ', size(multigrid_mesh_ids), &
          '-level function space chain.'
      call log_event( log_scratch_space, LOG_LEVEL_INFO )

      do mesh_ctr = 1, size(multigrid_mesh_ids)
        ! Make sure this function_space is in the collection
        fs => function_space_collection%get_fs( multigrid_mesh_ids(mesh_ctr), &
                                                0, W3 )
        call multigrid_function_space_chain%add( fs )

        fs => function_space_collection%get_fs( multigrid_mesh_ids(mesh_ctr), &
                                                0, W2 )
        call w2_multigrid_function_space_chain%add( fs )

        fs => function_space_collection%get_fs( multigrid_mesh_ids(mesh_ctr), &
                                                0, Wtheta )
        call wtheta_multigrid_function_space_chain%add( fs )
      end do

      single_layer_function_space_chain = function_space_chain_type()
      do mesh_ctr = 1, size(multigrid_2d_mesh_ids)
        fs => function_space_collection%get_fs( multigrid_2d_mesh_ids(mesh_ctr), &
                                                0, W3 )
        call single_layer_function_space_chain%add( fs )
      end do

    end if ! create_multigrid_fs_chain

    nullify( fs )
    call log_event( 'FEM specifics created', LOG_LEVEL_INFO )

  end subroutine init_fem

  !==================================================================================
  !> @brief Finalises the function_space_collection
  !==================================================================================
  subroutine final_fem()

    implicit none

    if (allocated(function_space_collection)) then
      call function_space_collection%clear()
      deallocate(function_space_collection)
    end if

    call final_chi_transforms()

  end subroutine final_fem

end module create_fem_mod
