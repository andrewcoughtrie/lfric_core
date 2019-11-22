!-------------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------
!> @brief create physics prognostics
!> @details Creates the physics prognostic fields
module create_physics_prognostics_mod

  use constants_mod,                  only : i_def, l_def
  use field_mod,                      only : field_type,                       &
                                             write_interface,                  &
                                             checkpoint_write_interface,       &
                                             checkpoint_read_interface
  use finite_element_config_mod,      only : element_order
  use function_space_collection_mod,  only : function_space_collection
  use field_collection_mod,           only : field_collection_type
  use fs_continuity_mod,              only : W2, W3, Wtheta
  use function_space_mod,             only : function_space_type
  use log_mod,                        only : log_event,                        &
                                             LOG_LEVEL_INFO,                   &
                                             LOG_LEVEL_ERROR
  use radiation_config_mod,           only : n_radstep
  use aerosol_config_mod,             only : c_aerosol,                        &
                                             c_aerosol_glomap_mode_climatology
  use section_choice_config_mod,      only : cloud, cloud_none,                &
                                             aerosol, aerosol_um,              &
                                             radiation, radiation_none
  use cloud_config_mod,               only : scheme, &
                                             scheme_pc2
  use time_config_mod,                only : timestep_start, timestep_end
  implicit none

  private
  public :: create_physics_prognostics

contains
  !>@brief Routine to initialise the field objects required by the physics
  !> @param[in]    mesh_id The identifier given to the current 3d mesh
  !> @param[in]    twod_mesh_id The identifier given to the current 2d mesh
  !> @param[inout] prognostic_fields A collection of the fields that make up the
  !>                                 prognostic variables in the model
  !> @param[out]   derived_fields Collection of FD fields derived from FE fields
  !> @param[out]   aerosol_fields Collection of FD aerosol fields
  !> @param[out]   cloud_fields Collection of FD cloud fields
  !> @param[out]   twod_fields Collection of two dimensional fields
  !> @param[out]   radstep_fields Collection of radiation timestep fields
  !> @param[out]   physics_incs Collection of physics increments
  !> @param[out]   orography_fields Collection of orography fields
  !> @param[out]   jules_ancils Ancillary fields for Jules
  !> @param[out]   jules_prognostics Prognostic fields for Jules
  subroutine create_physics_prognostics( mesh_id,                              &
                                         twod_mesh_id,                         &
                                         depository,                           &
                                         prognostic_fields,                    &
                                         derived_fields,                       &
                                         aerosol_fields,                       &
                                         cloud_fields,                         &
                                         twod_fields,                          &
                                         radstep_fields,                       &
                                         physics_incs,                         &
                                         orography_fields,                     &
                                         jules_ancils,                         &
                                         jules_prognostics )

    implicit none

    integer(i_def), intent(in) :: mesh_id
    integer(i_def), intent(in) :: twod_mesh_id

    ! Collections of fields
    type(field_collection_type), intent(inout) :: depository
    type(field_collection_type), intent(inout) :: prognostic_fields
    type(field_collection_type), intent(out) :: twod_fields
    type(field_collection_type), intent(out) :: aerosol_fields
    type(field_collection_type), intent(out) :: cloud_fields
    type(field_collection_type), intent(out) :: derived_fields
    type(field_collection_type), intent(out) :: radstep_fields
    type(field_collection_type), intent(out) :: physics_incs
    type(field_collection_type), intent(out) :: orography_fields
    type(field_collection_type), intent(out) :: jules_ancils
    type(field_collection_type), intent(out) :: jules_prognostics

    ! pointers to vector spaces
    type(function_space_type), pointer :: vector_space => null()

    type( field_type ), pointer :: theta => null()

    ! Each column of a higher-order discontinuous field will be used to
    ! represent multi-dimensional quantities like tiles, plant functional
    ! types and sea ice categories. Set parameters for the orders required:
    integer(i_def) :: tile_order = 2 ! Enough space for 27 tiles
    integer(i_def) :: pft_order  = 1 ! Enough space for 8 plant functional types
    integer(i_def) :: sice_order = 1 ! Enough space for 8 sea ice categories
    integer(i_def) :: soil_order = 1 ! Enough space for 8 soil levels

    integer(i_def) :: theta_space
    logical(l_def) :: checkpoint_restart_flag
    logical(l_def) :: advection_flag

    call log_event( 'Create physics prognostics', LOG_LEVEL_INFO )


    theta => prognostic_fields%get_field('theta')
    theta_space = theta%which_function_space()

    if (theta_space /= Wtheta)then
      call log_event( 'Physics: requires theta variable to be in Wtheta',      &
                      LOG_LEVEL_ERROR )
    end if

    if (element_order > 0)then
      call log_event( 'Physics: requires lowest order elements',               &
                      LOG_LEVEL_ERROR )
    end if

    !========================================================================
    ! Here we create some field collections
    !========================================================================
    derived_fields  =  field_collection_type(name='derived_fields')
    checkpoint_restart_flag = .false.

    ! Wtheta fields
    vector_space => function_space_collection%get_fs(mesh_id, 0, Wtheta)

    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'w_physics',      vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'rho_in_wth',     vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'wetrho_in_wth',  vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'exner_in_wth',   vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'w_physics_star', vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'theta_star',     vector_space, checkpoint_restart_flag )

    ! W3 fields
    vector_space => function_space_collection%get_fs(mesh_id, 0, W3)

    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'u1_in_w3',      vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'u2_in_w3',      vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'u3_in_w3',      vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'theta_in_w3',   vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'wetrho_in_w3',  vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'u1_in_w3_star', vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'u2_in_w3_star', vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'u3_in_w3_star', vector_space, checkpoint_restart_flag )

    ! W2 fields
    vector_space => function_space_collection%get_fs(mesh_id, 0, W2)

    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'u_physics',      vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'u_physics_star', vector_space, checkpoint_restart_flag )
    call add_physics_field( derived_fields, depository, prognostic_fields,     &
      'u_star',         vector_space, checkpoint_restart_flag )


    !========================================================================
    ! Here we create some 2d fields for the UM physics
    !========================================================================
    twod_fields = field_collection_type(name='twod_fields')
    vector_space => function_space_collection%get_fs(twod_mesh_id, 0, W3)
    checkpoint_restart_flag = .true.

    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'tstar',   vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'zh',      vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'z0msea',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'conv_rain',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'conv_snow',  vector_space, checkpoint_restart_flag, twod=.true. )

    checkpoint_restart_flag = .false.
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'ntml',    vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'cumulus', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'cos_zenith_angle',   vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'lit_fraction',       vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'ls_rain',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'ls_snow',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'zlcl_mixed',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'blend_height_tq',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'blend_height_uv',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'zh_nonloc',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'shallow_flag',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'uw0_flux',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'vw0_flux',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'lcl_height',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'parcel_top',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'level_parcel_top',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'wstar_2d',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'thv_flux',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'parcel_buoyancy',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'qsat_at_lcl',  vector_space, checkpoint_restart_flag, twod=.true. )

    ! a couple of BL arrays which are helpful to group together as higher
    ! order fields
    vector_space => function_space_collection%get_fs(twod_mesh_id, 1, W3)

    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'rhokm_surf',  vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( twod_fields, depository, prognostic_fields,        &
      'bl_types',  vector_space, checkpoint_restart_flag, twod=.true. )

    !========================================================================
    ! Here we create some glomap aerosol fields
    !========================================================================

    aerosol_fields = field_collection_type(name='aerosol_fields')
    vector_space => function_space_collection%get_fs(mesh_id, 0, Wtheta)

    if ( ( aerosol == aerosol_um ) .and.                                       &
         ( c_aerosol == c_aerosol_glomap_mode_climatology ) ) then
      checkpoint_restart_flag = .true.
    else
      checkpoint_restart_flag = .false.
    end if

    ! Nucleation Soluble number density
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'nd_nuc_sol', vector_space, checkpoint_restart_flag )

    ! Nucleation Soluble sulphate aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'nuc_sol_su', vector_space, checkpoint_restart_flag )

    ! Nucleation Soluble organic carbon aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'nuc_sol_oc', vector_space, checkpoint_restart_flag )

    ! Aitken Soluble number density
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'nd_ait_sol', vector_space, checkpoint_restart_flag )

    ! Aitken Soluble sulphate aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'ait_sol_su', vector_space, checkpoint_restart_flag )

    ! Aitken Soluble black carbon aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'ait_sol_bc', vector_space, checkpoint_restart_flag )

    ! Aitken Soluble organic carbon aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'ait_sol_oc', vector_space, checkpoint_restart_flag )

    ! Accumulation Soluble number density
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'nd_acc_sol', vector_space, checkpoint_restart_flag )

    ! Accumulation Soluble sulphate aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'acc_sol_su', vector_space, checkpoint_restart_flag )

    ! Accumulation Soluble black carbon aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'acc_sol_bc', vector_space, checkpoint_restart_flag )

    ! Accumulation Soluble organic carbon aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'acc_sol_oc', vector_space, checkpoint_restart_flag )

    ! Accumulation Soluble sea salt aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'acc_sol_ss', vector_space, checkpoint_restart_flag )

    ! Coarse Soluble number density
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'nd_cor_sol', vector_space, checkpoint_restart_flag )

    ! Coarse Soluble sulphate aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'cor_sol_su', vector_space, checkpoint_restart_flag )

    ! Coarse Soluble black carbon aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'cor_sol_bc', vector_space, checkpoint_restart_flag )

    ! Coarse Soluble organic carbon aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'cor_sol_oc', vector_space, checkpoint_restart_flag )

    ! Coarse Soluble sea salt aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'cor_sol_ss', vector_space, checkpoint_restart_flag )

    ! Aitken Insoluble number density
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'nd_ait_ins', vector_space, checkpoint_restart_flag )

    ! Aitken Insoluble black carbon aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'ait_ins_bc', vector_space, checkpoint_restart_flag )

    ! Aitken Insoluble organic carbon aerosol mmr
    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'ait_ins_oc', vector_space, checkpoint_restart_flag )

    !========================================================================
    ! Here we create CDNC (and later RADAER) fields
    !========================================================================

    ! No checkpoint restart for these fields
    checkpoint_restart_flag = .false.

    call add_physics_field( aerosol_fields, depository, prognostic_fields,     &
      'cloud_drop_no_conc', vector_space, checkpoint_restart_flag )

    !========================================================================
    ! Here we create some cloud fields
    !========================================================================

    cloud_fields = field_collection_type(name='cloud_fields')
    vector_space => function_space_collection%get_fs(mesh_id, 0, Wtheta)
    if (cloud /= cloud_none)then
      checkpoint_restart_flag = .true.
    end if

    ! convective cloud field prognostics
    call add_physics_field(cloud_fields, depository, prognostic_fields, &
      'cca', vector_space, checkpoint_restart_flag)
    call add_physics_field(cloud_fields, depository, prognostic_fields, &
      'ccw', vector_space, checkpoint_restart_flag)

    call add_physics_field(cloud_fields, depository, prognostic_fields, &
      'area_fraction',   vector_space, checkpoint_restart_flag)
    call add_physics_field(cloud_fields, depository, prognostic_fields, &
      'rh_crit_wth',     vector_space, checkpoint_restart_flag)

    if ( scheme == scheme_pc2 ) then
      advection_flag=.true.
    else
      advection_flag=.false.
    endif

    call add_physics_field(cloud_fields, depository, prognostic_fields, &
      'liquid_fraction', vector_space, checkpoint_restart_flag,         &
      advection_flag=advection_flag)
    call add_physics_field(cloud_fields, depository, prognostic_fields, &
      'ice_fraction',    vector_space, checkpoint_restart_flag,         &
      advection_flag=advection_flag)
    call add_physics_field(cloud_fields, depository, prognostic_fields, &
      'bulk_fraction',   vector_space, checkpoint_restart_flag,         &
      advection_flag=advection_flag)

    !========================================================================
    ! Radiation timestep fields
    !========================================================================
    radstep_fields = field_collection_type(name='radstep_fields')
    if (radiation /= radiation_none) then
      ! Checkpoint unless both the first timestep of this run and the
      ! first timestep of the next run are radiation timesteps
      checkpoint_restart_flag = &
        mod(timestep_start-1, n_radstep) /= 0 .or. &
        mod(timestep_end,     n_radstep) /= 0
    else
      checkpoint_restart_flag = .false.
    end if

    vector_space => function_space_collection%get_fs(mesh_id, 0, Wtheta)
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'sw_heating_rate_rts', vector_space, checkpoint_restart_flag )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'lw_heating_rate_rts', vector_space, checkpoint_restart_flag )

    vector_space => function_space_collection%get_fs(twod_mesh_id, 0, W3)
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'lw_down_surf_rts', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'sw_down_surf_rts', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'sw_direct_surf_rts', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'sw_down_blue_surf_rts', vector_space, checkpoint_restart_flag,          &
      twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'sw_direct_blue_surf_rts', vector_space, checkpoint_restart_flag,        &
      twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'cos_zenith_angle_rts', vector_space, checkpoint_restart_flag,           &
      twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'lit_fraction_rts', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'stellar_irradiance_rts', vector_space, checkpoint_restart_flag,         &
      twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'sin_stellar_declination_rts', vector_space, checkpoint_restart_flag,    &
      twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'stellar_eqn_of_time_rts', vector_space, checkpoint_restart_flag,        &
      twod=.true. )

    ! Multidimensional fields currently not able to be checkpointed.
    checkpoint_restart_flag = .false.
    vector_space => function_space_collection%get_fs(twod_mesh_id, tile_order, W3)
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'lw_up_tile_rts', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'sw_up_tile_rts', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( radstep_fields, depository, prognostic_fields,     &
      'sw_up_blue_tile_rts', vector_space, checkpoint_restart_flag,            &
      twod=.true. )

    !========================================================================
    ! Increment values from individual physics parametrizations
    ! either for use by subsequent parametrizations or as diagnostics
    !========================================================================
    physics_incs = field_collection_type(name='physics_incs')
    checkpoint_restart_flag = .false. ! no need to dump any of these

    vector_space => function_space_collection%get_fs(mesh_id, 0, Wtheta)
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dt_bl', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dmv_bl', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dt_conv', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dmv_conv', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dmcl_conv', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dmcf_conv', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dtl_mphys', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dmt_mphys', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'sw_heating_rate', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'lw_heating_rate', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'rhokm_bl', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'ngstress_bl', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'bq_bl', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'bt_bl', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dtrdz_tq_bl', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'rdz_uv_bl', vector_space, checkpoint_restart_flag )

    ! Increments on pgrid (not U and V grids) but rho levels
    vector_space => function_space_collection%get_fs(mesh_id, 0, W3)
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'du_conv', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dv_conv', vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'rhokh_bl',     vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'moist_flux_bl',     vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'heat_flux_bl',     vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'dtrdz_uv_bl',     vector_space, checkpoint_restart_flag )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'rdz_tq_bl',     vector_space, checkpoint_restart_flag )

    vector_space => function_space_collection%get_fs(twod_mesh_id, 0, W3)
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'lw_down_surf', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'sw_down_surf', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'sw_direct_surf', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'sw_down_blue_surf', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( physics_incs, depository, prognostic_fields,       &
      'sw_direct_blue_surf', vector_space, checkpoint_restart_flag,            &
      twod=.true. )

    !========================================================================
    ! Here we create some 2D orography fields
    !========================================================================
    orography_fields = field_collection_type(name='orography_fields')
    vector_space => function_space_collection%get_fs(twod_mesh_id, 0, W3)
    checkpoint_restart_flag = .true.

    call add_physics_field( orography_fields, depository, prognostic_fields,   &
      'sd_orog', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( orography_fields, depository, prognostic_fields,   &
      'grad_xx_orog', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( orography_fields, depository, prognostic_fields,   &
      'grad_xy_orog', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( orography_fields, depository, prognostic_fields,   &
      'grad_yy_orog', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( orography_fields, depository, prognostic_fields,   &
      'peak_to_trough_orog', vector_space, checkpoint_restart_flag,            &
      twod=.true. )
    call add_physics_field( orography_fields, depository, prognostic_fields,   &
      'silhouette_area_orog', vector_space, checkpoint_restart_flag,           &
      twod=.true. )

    !========================================================================
    ! Surface fields (a temporary treatment for multi-dimensional fields
    !                 using higher-order fields)
    !========================================================================

    ! Jules ancillaries
    jules_ancils = field_collection_type(name='jules_ancils')
    checkpoint_restart_flag = .false. ! #1920 required for this to work

    vector_space => function_space_collection%get_fs(twod_mesh_id, tile_order, W3)
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'tile_fraction', vector_space, checkpoint_restart_flag, twod=.true. )

    vector_space => function_space_collection%get_fs(twod_mesh_id, pft_order, W3)
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'leaf_area_index', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'canopy_height', vector_space, checkpoint_restart_flag, twod=.true. )

    vector_space => function_space_collection%get_fs(twod_mesh_id, 0, W3)
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_albedo', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_roughness', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_thermal_cond', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_carbon_content', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'albedo_obs_sw', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'albedo_obs_vis', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'albedo_obs_nir', vector_space, checkpoint_restart_flag, twod=.true. )

    vector_space => function_space_collection%get_fs(twod_mesh_id, soil_order, W3)
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_moist_wilt', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_moist_crit', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_moist_sat', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_cond_sat', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_thermal_cap', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'soil_suction_sat', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_ancils, depository, prognostic_fields,       &
      'clapp_horn_b', vector_space, checkpoint_restart_flag, twod=.true. )

    ! Jules prognostics
    jules_prognostics = field_collection_type(name='jules_prognostics')
    checkpoint_restart_flag = .false. ! #1920 required for this to work

    vector_space => function_space_collection%get_fs(twod_mesh_id, tile_order, W3)
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'tile_temperature', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'tile_snow_mass', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'tile_snow_rgrain', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'n_snow_layers', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'snow_depth', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'canopy_water', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'lw_up_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'sw_up_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'sw_up_blue_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'tile_heat_flux', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'tile_moisture_flux', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'alpha1_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'ashtf_prime_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'dtstar_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'fraca_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'z0h_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'z0m_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'rhokh_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'chr1p5m_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'resfs_tile', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'canhc_tile', vector_space, checkpoint_restart_flag, twod=.true. )

    vector_space => function_space_collection%get_fs(twod_mesh_id, 0, W3)
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'snow_soot', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'surface_conductance', vector_space, checkpoint_restart_flag,            &
      twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'chloro_sea', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'ustar', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'soil_moist_avail', vector_space, checkpoint_restart_flag, twod=.true. )

    vector_space => function_space_collection%get_fs(twod_mesh_id, soil_order, W3)
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'soil_temperature', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'soil_moisture', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'unfrozen_soil_moisture', vector_space, checkpoint_restart_flag,         &
      twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'frozen_soil_moisture', vector_space, checkpoint_restart_flag,           &
      twod=.true. )

    vector_space => function_space_collection%get_fs(twod_mesh_id, sice_order, W3)
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'sea_ice_thickness', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'sea_ice_pond_frac', vector_space, checkpoint_restart_flag, twod=.true. )
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'sea_ice_pond_depth', vector_space, checkpoint_restart_flag, twod=.true. )

    ! This needs to be order 3 as it is land tile (9) x soil levels (4) = 36
    vector_space => function_space_collection%get_fs(twod_mesh_id, 3, W3)
    call add_physics_field( jules_prognostics, depository, prognostic_fields,  &
      'tile_water_extract', vector_space, checkpoint_restart_flag, twod=.true. )

  end subroutine create_physics_prognostics

  !>@brief Add field to field collection and set its write,
  !>       checkpoint and restart behaviour
  !> @param[in,out] field_collection Field collection that 'name' will be added to
  !> @param[in]     name             Name of field to be added to collection
  !> @param[in]     vector_space     Function space of field to set behaviour for
  !> @param[in]     checkpoint_restart_flag  Flag to allow checkpoint and
  !>                                        restart behaviour of field to be set
  !> @param[in]     twod             Optional flag to determine if this is a
  !>                                        2D field for diagnostic output
  !> @param[in]     advection_flag   Optional flag whether this field is to be advected

  subroutine add_physics_field(field_collection, &
                               depository, prognostic_fields, &
                               name, vector_space, &
                               checkpoint_restart_flag, twod, advection_flag)

    use io_config_mod,      only : use_xios_io, &
                                   write_diag
    use io_mod,             only : xios_write_field_face,        &
                                   xios_write_field_single_face, &
                                   checkpoint_write_xios,        &
                                   checkpoint_write_netcdf,      &
                                   checkpoint_read_netcdf,       &
                                   checkpoint_read_xios

    implicit none

    character(*), intent(in)                   :: name
    type(field_collection_type), intent(inout) :: field_collection
    type(field_collection_type), intent(inout) :: depository
    type(field_collection_type), intent(inout) :: prognostic_fields
    type(function_space_type), intent(in)      :: vector_space
    logical(l_def), intent(in)                 :: checkpoint_restart_flag
    logical, optional, intent(in)              :: twod
    logical(l_def), optional, intent(in)       :: advection_flag
    !Local variables
    type(field_type)                           :: new_field

    ! pointers for xios write interface
    procedure(write_interface), pointer   :: write_diag_behaviour => null()
    procedure(checkpoint_write_interface), pointer  :: checkpoint_write_behaviour => null()
    procedure(checkpoint_read_interface), pointer   :: checkpoint_read_behaviour => null()

    if ( use_xios_io) then
      checkpoint_write_behaviour => checkpoint_write_xios
      checkpoint_read_behaviour => checkpoint_read_xios
    else
      checkpoint_write_behaviour => checkpoint_write_netcdf
      checkpoint_read_behaviour => checkpoint_read_netcdf
    endif

    if (present(advection_flag)) then
      call new_field%initialise( vector_space, name=trim(name), advection_flag=advection_flag )
    else
      call new_field%initialise( vector_space, name=trim(name) )
    end if

    if (use_xios_io .and. write_diag) then
      ! All physics fields currently require output on faces...
      write_diag_behaviour => xios_write_field_face
      if (present(twod))then
        if (twod) write_diag_behaviour => xios_write_field_single_face
      end if
      call new_field%set_write_behaviour(write_diag_behaviour)
    end if
    if ( checkpoint_restart_flag ) then
      call new_field%set_checkpoint_write_behaviour(checkpoint_write_behaviour)
      call new_field%set_checkpoint_read_behaviour(checkpoint_read_behaviour)
    endif

    ! Add the field to the depository
    call depository%add_field(new_field)
    ! Put a pointer to the field in the required collection
    call field_collection%add_reference_to_field( depository%get_field(name) )
    ! If checkpointing the field, put a pointer to it in the prognostics collection
    if ( checkpoint_restart_flag ) then
      call prognostic_fields%add_reference_to_field( depository%get_field(name) )
    endif

  end subroutine add_physics_field

end module create_physics_prognostics_mod
