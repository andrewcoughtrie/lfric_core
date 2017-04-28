!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @brief init functionality for gravity wave simulations

!> @details Handles init of prognostic and coordinate fields

module init_gravity_wave_mod

  use assign_coordinate_field_mod,    only : assign_coordinate_field
  use constants_mod,                  only : i_def
  use field_mod,                      only : field_type
  use finite_element_config_mod,      only : element_order
  use function_space_collection_mod,  only : function_space_collection
  use fs_continuity_mod,              only : W0, W2, W3, Wtheta
  use gw_init_fields_alg_mod,         only : gw_init_fields_alg
  use log_mod,                        only : log_event,         &
                                             LOG_LEVEL_INFO, &
                                             LOG_LEVEL_ERROR
  use restart_control_mod,            only : restart_type
  use gw_miniapp_constants_config_mod,only : b_space, &
                                             gw_miniapp_constants_b_space_w0, &
                                             gw_miniapp_constants_b_space_w3, &
                                             gw_miniapp_constants_b_space_wtheta
  use runtime_constants_mod,          only : create_runtime_constants

  implicit none


  contains

  subroutine init_gravity_wave(mesh_id, wind, pressure, buoyancy, restart)

    integer(i_def), intent(in)               :: mesh_id
    ! prognostic fields
    type( field_type ), intent(inout)        :: wind, pressure, buoyancy
    type(restart_type), intent(in)           :: restart
    integer(i_def)                           :: buoyancy_space

    call log_event( 'gravity wave: initialisation...', LOG_LEVEL_INFO )

    
    ! Create prognostic fields
    select case(b_space)
      case(gw_miniapp_constants_b_space_w0)
        buoyancy_space = W0
        call log_event( "gravity wave: Using W0 for buoyancy", LOG_LEVEL_INFO )
      case(gw_miniapp_constants_b_space_w3)
        buoyancy_space = W3
        call log_event( "gravity wave: Using W3 for buoyancy", LOG_LEVEL_INFO )
      case(gw_miniapp_constants_b_space_wtheta)
        buoyancy_space = Wtheta
        call log_event( "gravity wave: Using Wtheta for buoyancy", LOG_LEVEL_INFO )
      case default
        call log_event( "gravity wave: Invalid buoyancy space", LOG_LEVEL_ERROR )
    end select

    wind = field_type( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, W2) )
    buoyancy = field_type( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, buoyancy_space) )
    pressure = field_type( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, W3) )

    ! Create runtime_constants object. This in turn creates various things
    ! needed by the timestepping algorithms such as mass matrix operators, mass
    ! matrix diagonal fields and the geopotential field
    call create_runtime_constants(mesh_id)

    ! Initialise prognostic fields
    call gw_init_fields_alg( mesh_id, wind, pressure, buoyancy, restart)


    call log_event( 'buondary test initialised', LOG_LEVEL_INFO )

  end subroutine init_gravity_wave

end module init_gravity_wave_mod
