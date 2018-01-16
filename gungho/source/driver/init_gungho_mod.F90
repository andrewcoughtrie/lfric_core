!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @brief Init functionality for gungho model
!> @details Creates and initialises prognostic fields, also runtime_constants
!>          specific to the model

module init_gungho_mod

  use constants_mod,                  only : i_def
  use field_mod,                      only : field_type, write_interface
  use finite_element_config_mod,      only : element_order, wtheta_on
  use fs_continuity_mod,              only : W0, W1, W2, W3, Wtheta
  use function_space_collection_mod , only : function_space_collection
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
  use output_config_mod,              only : write_xios_output
  use io_mod,                         only : xios_write_field_node, &
                                             xios_write_field_face


  implicit none


contains

  !>@brief Initialise the gungho model
  !> @param[in] mesh_id Identifier of the mesh
  !> @param[inout] chi Spatial coordinates
  !> @param[inout] u Wind field
  !> @param[inout] rho Density field
  !> @param[inout] theta Potential temperature field
  !> @param[inout] exner Exner pressure field
  !> @param[inout] rho_in_wth Density in the temperature space
  !> @param[inout] mr Moisture mixing ratios
  !> @param[inout] xi Vorticity
  !> @param[in] restart Restart dump to read prognostic fields from
  subroutine init_gungho( mesh_id, chi, u, rho, theta, exner, rho_in_wth, mr, xi, restart )

    integer(i_def), intent(in)               :: mesh_id
    ! Prognostic fields
    type( field_type ), intent(inout)        :: u, rho, theta, exner
    type( field_type ), intent(inout)        :: mr(nummr)
    ! Diagnostic fields
    type( field_type ), intent(inout)        :: xi, rho_in_wth
    ! Coordinate fields
    type( field_type ), intent(inout)        :: chi(:)

    type(restart_type), intent(in)           :: restart

    integer(i_def)                           :: imr

    procedure(write_interface), pointer      :: tmp_ptr

    call log_event( 'GungHo: initialisation...', LOG_LEVEL_INFO )

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
                        function_space_collection%get_fs(mesh_id, element_order, W1), &
                        output_space = W3)
    u     = field_type( vector_space = &
                        function_space_collection%get_fs(mesh_id, element_order, W2), &
                        output_space = W3)
    rho   = field_type( vector_space = &
                        function_space_collection%get_fs(mesh_id, element_order, W3) )
    exner = field_type( vector_space = &
                        function_space_collection%get_fs(mesh_id, element_order, W3), &
                        output_space = W3)

    rho_in_wth = field_type( vector_space = & 
        function_space_collection%get_fs(mesh_id, element_order, theta%which_function_space()) )

    do imr = 1,nummr
      mr(imr) = field_type( vector_space = &
      function_space_collection%get_fs(mesh_id, element_order, theta%which_function_space()) )
    end do

    if (write_xios_output) then
       ! Fields that are output on the XIOS face domain

       tmp_ptr => xios_write_field_face

       call xi%set_write_field_behaviour(tmp_ptr)
       call u%set_write_field_behaviour(tmp_ptr)
       call rho%set_write_field_behaviour(tmp_ptr)
       call exner%set_write_field_behaviour(tmp_ptr)

       ! Fields that are output on the XIOS node domain

       ! Theta is a special case as it can be on face (if function space is WTheta) 
       ! or node (if function space is W0)
       if (theta%which_function_space() == Wtheta) then
          call theta%set_write_field_behaviour(tmp_ptr)
       else
          tmp_ptr => xios_write_field_node
          call theta%set_write_field_behaviour(tmp_ptr)
       end if

       ! Moisture diagnostics use the same type of field write as Theta
       call theta%get_write_field_behaviour(tmp_ptr)
       do imr = 1,nummr
         call mr(imr)%set_write_field_behaviour(tmp_ptr)
       end do
    end if

    ! Create runtime_constants object. This in turn creates various things
    ! needed by the timestepping algorithms such as mass matrix operators, mass
    ! matrix diagonal fields and the geopotential field
    call create_runtime_constants(mesh_id, chi)

    ! Initialise prognostic fields
    call init_prognostic_fields_alg( u, rho, theta, exner, &
                                     rho_in_wth, mr, xi, restart )

    call log_event( 'Gungho initialised', LOG_LEVEL_INFO )

  end subroutine init_gungho

end module init_gungho_mod
