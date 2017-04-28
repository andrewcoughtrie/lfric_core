!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @brief init functionality for dynamo

!> @details Handles init of prognostic fields

module init_dynamo_mod

  use base_mesh_config_mod,           only : geometry, &
                                             base_mesh_geometry_spherical
  use constants_mod,                  only : i_def
  use field_mod,                      only : field_type
  use finite_element_config_mod,      only : element_order, wtheta_on
  use function_space_collection_mod,  only : function_space_collection
  use fs_continuity_mod,              only : W0, W1, W2, W3, Wtheta
  use init_prognostic_fields_alg_mod, only : init_prognostic_fields_alg
  use log_mod,                        only : log_event,         &
                                             LOG_LEVEL_INFO
  use restart_control_mod,            only : restart_type
  use formulation_config_mod,         only : transport_only
  use transport_config_mod,           only : scheme, &
                                             operators, &
                                             transport_scheme_method_of_lines, &
                                             transport_operators_fv
  use mr_indices_mod,                 only : nummr
  use runtime_constants_mod,          only : create_runtime_constants

  implicit none


  contains

  subroutine init_dynamo(mesh_id, u, rho, theta, rho_in_wth, mr, xi, restart)

    integer(i_def), intent(in)               :: mesh_id
    ! prognostic fields
    type( field_type ), intent(inout)        :: u, rho, theta, xi
    type( field_type ), intent(inout)        :: mr(nummr), rho_in_wth
    type(restart_type), intent(in)           :: restart

    integer(i_def)                           :: imr

    call log_event( 'Dynamo: initialisation...', LOG_LEVEL_INFO )

    
    ! Create prognostic fields
    if ( (transport_only .and. &
         scheme == transport_scheme_method_of_lines .and. &
         operators == transport_operators_fv) .or. &
         wtheta_on  ) then
      ! Only use Wtheta for fv method of lines transport or if wtheta_on
      theta = field_type( vector_space = &
                             function_space_collection%get_fs(mesh_id, element_order, Wtheta) )
    else
      theta = field_type( vector_space = &
                         function_space_collection%get_fs(mesh_id, element_order, W0) )
    end if
    xi    = field_type( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, W1) )
    u     = field_type( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, W2) )
    rho   = field_type( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, W3) )

    rho_in_wth = field_type( vector_space = & 
       function_space_collection%get_fs(mesh_id, element_order, theta%which_function_space()))

    do imr = 1,nummr
      mr(imr) = field_type( vector_space = &
         function_space_collection%get_fs(mesh_id, element_order, theta%which_function_space()) )
    end do

    ! Create runtime_constants object. This in turn creates various things
    ! needed by the timestepping algorithms such as mass matrix operators, mass
    ! matrix diagonal fields and the geopotential field
    call create_runtime_constants(mesh_id)

    ! Initialise prognostic fields
    call init_prognostic_fields_alg( mesh_id, u, rho, theta, rho_in_wth, mr, xi, restart)

    call log_event( 'Dynamo initialised', LOG_LEVEL_INFO )

  end subroutine init_dynamo

end module init_dynamo_mod
