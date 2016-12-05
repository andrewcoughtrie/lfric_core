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
  use finite_element_config_mod,      only : element_order, coordinate_order
  use function_space_collection_mod,  only : function_space_collection
  use fs_continuity_mod,              only : W0, W2, W3, Wtheta, Wchi
  use gw_init_fields_alg_mod,         only : gw_init_fields_alg
  use log_mod,                        only : log_event,         &
                                             LOG_LEVEL_INFO
  use restart_control_mod,            only : restart_type

  implicit none


  contains

  subroutine init_gravity_wave(mesh_id, chi, wind, pressure, buoyancy, restart)

    integer(i_def), intent(in)               :: mesh_id
    ! coordinate field
    type( field_type ), intent(inout)        :: chi(3)
    ! prognostic fields
    type( field_type ), intent(inout)        :: wind, pressure, buoyancy
    type(restart_type), intent(in)           :: restart

    integer(i_def)                           :: coord
    integer(i_def)                           :: chi_space

    call log_event( 'gravity wave: initialisation...', LOG_LEVEL_INFO )

    ! Calculate coordinates
    if ( coordinate_order == 0 ) then
      chi_space = W0
      call log_event( "gravity wave: Computing W0 coordinate fields", LOG_LEVEL_INFO )
    else
      chi_space = Wchi
      call log_event( "gravity wave: Computing Wchi coordinate fields", LOG_LEVEL_INFO )
    end if
    do coord = 1,3
      chi(coord) = field_type (vector_space =                                    &
                   function_space_collection%get_fs(mesh_id,coordinate_order,chi_space) )
    end do
    call assign_coordinate_field(chi, mesh_id)


    ! Create prognostic fields
    wind = field_type( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, W2) )
    buoyancy = field_type( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, Wtheta) )
    pressure = field_type( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, W3) )

    ! Initialise prognostic fields
    call gw_init_fields_alg( mesh_id, chi, wind, pressure, buoyancy, restart)


    call log_event( 'buondary test initialised', LOG_LEVEL_INFO )

  end subroutine init_gravity_wave

end module init_gravity_wave_mod
