!-----------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Creates and initialises the prognostic fields

!> @details Create and initialises the prognostic fields and place them in the
!>          prognostic field collection

module multires_coupling_prognostics_mod

  use constants_mod,                              only : i_def
  use field_mod,                                  only : field_type
  use field_parent_mod,                           only : write_interface, &
                                                         checkpoint_write_interface, &
                                                         checkpoint_read_interface
  use field_collection_mod,                       only : field_collection_type
  use finite_element_config_mod,                  only : element_order
  use function_space_collection_mod,              only : function_space_collection
  use fs_continuity_mod,                          only : W2, W3, WTHETA
  use io_config_mod,                              only : write_diag,       &
                                                         use_xios_io,      &
                                                         checkpoint_write, &
                                                         checkpoint_read
  use io_mod,                                     only : checkpoint_read_netcdf, &
                                                         checkpoint_write_netcdf
  use lfric_xios_read_mod,                        only : checkpoint_read_xios
  use lfric_xios_write_mod,                       only : write_field_face, &
                                                         checkpoint_write_xios
  use log_mod,                                    only : log_event, &
                                                         LOG_LEVEL_INFO
  use init_mrc_prognostics_alg_mod,               only : init_mrc_prognostics_alg

  implicit none

  private
  public init_multires_coupling_prognostics, create_multires_coupling_prognostics

  contains

  !> @brief An interface that creates all fields on all meshes, and initialises fields
  !>        on selected meshes
  !> @param [in] output_mesh_id The identifier of the output mesh
  !> @param [in] dynamics_mesh_id The identifier of the dynamics mesh
  !> @param [in] physics_mesh_id The identifier of the physics mesh
  !> @param [in,out] output_prognostic_fields A collection of all the output prognostic fields.
  !> @param [in,out] dynamics_prognostic_fields A collection of all the dynamics prognostic fields.
  !> @param [in,out] physics_prognostic_fields A collection of all the physics prognostic fields.
  subroutine init_multires_coupling_prognostics(output_mesh_id,             &
                                                dynamics_mesh_id,           &
                                                physics_mesh_id,            &
                                                output_prognostic_fields,   &
                                                dynamics_prognostic_fields, &
                                                physics_prognostic_fields )

    implicit none

    integer(kind=i_def), intent(in)              :: output_mesh_id
    integer(kind=i_def), intent(in)              :: dynamics_mesh_id
    integer(kind=i_def), intent(in)              :: physics_mesh_id
    type( field_collection_type ), intent(inout) :: output_prognostic_fields
    type( field_collection_type ), intent(inout) :: dynamics_prognostic_fields
    type( field_collection_type ), intent(inout) :: physics_prognostic_fields

    ! Create the prognostic fields
    call create_multires_coupling_prognostics(output_mesh_id, output_prognostic_fields)
    call create_multires_coupling_prognostics(dynamics_mesh_id, dynamics_prognostic_fields)
    call create_multires_coupling_prognostics(physics_mesh_id, physics_prognostic_fields)

    ! Initialise the prognostic fields on the dynamics mesh. Note in future
    ! fields may be initialised on different meshes
    call init_mrc_prognostics_alg(dynamics_prognostic_fields)
    call init_mrc_prognostics_alg(physics_prognostic_fields)
    call init_mrc_prognostics_alg(output_prognostic_fields)

  end subroutine init_multires_coupling_prognostics

  !> @brief Create the prognostic fields and places them in the depository
  !> @param [in] mesh_id The identifier of the primary mesh
  !> @param [in,out] prognostics A collection of all the prognostic fields.
  !>                             For the multires_coupling miniapp, the prognostics
  !>                             are all the fields in the depository - so the
  !>                             two collections contain the same fields
  subroutine create_multires_coupling_prognostics(mesh_id, prognostics)

    implicit none
    integer(kind=i_def), intent(in)              :: mesh_id
    type( field_collection_type ), intent(inout) :: prognostics

    type(field_type) :: u
    type(field_type) :: rho
    type(field_type) :: theta

    procedure(write_interface),            pointer :: tmp_write_ptr
    procedure(checkpoint_write_interface), pointer :: tmp_checkpoint_write_ptr
    procedure(checkpoint_read_interface),  pointer :: tmp_checkpoint_read_ptr

    call u%initialise( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, W2), &
                       name="u" )
    call rho%initialise( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, W3), &
                       name="rho" )
    call theta%initialise( vector_space = &
                       function_space_collection%get_fs(mesh_id, element_order, WTHETA), &
                       name="theta" )

    ! Set I/O behaviours for diagnostic output
    if (write_diag .and. use_xios_io) then
       ! Fields that are output on the XIOS face domain
       tmp_write_ptr => write_field_face
       call u%set_write_behaviour(tmp_write_ptr)
       call rho%set_write_behaviour(tmp_write_ptr)
       call theta%set_write_behaviour(tmp_write_ptr)
    end if

    ! Set I/O behaviours for checkpoint / restart
    if ( checkpoint_write .or. checkpoint_read) then
      if (use_xios_io) then
        ! Use XIOS for checkpoint / restart
        tmp_checkpoint_write_ptr => checkpoint_write_xios
        tmp_checkpoint_read_ptr => checkpoint_read_xios
        call log_event( 'GungHo: Using XIOS for checkpointing...', LOG_LEVEL_INFO )
      else
        ! Use old checkpoint and restart methods
        tmp_checkpoint_write_ptr => checkpoint_write_netcdf
        tmp_checkpoint_read_ptr => checkpoint_read_netcdf
        call log_event( 'GungHo: Using NetCDF for checkpointing...', LOG_LEVEL_INFO )
      end if

      call u%set_checkpoint_write_behaviour(tmp_checkpoint_write_ptr)
      call rho%set_checkpoint_write_behaviour(tmp_checkpoint_write_ptr)
      call theta%set_checkpoint_write_behaviour(tmp_checkpoint_write_ptr)


      call u%set_checkpoint_read_behaviour(tmp_checkpoint_read_ptr)
      call rho%set_checkpoint_read_behaviour(tmp_checkpoint_read_ptr)
      call theta%set_checkpoint_read_behaviour(tmp_checkpoint_read_ptr)

    end if

    ! Store the prognostic fields
    call prognostics%add_field(u)
    call prognostics%add_field(rho)
    call prognostics%add_field(theta)

  end subroutine create_multires_coupling_prognostics

end module multires_coupling_prognostics_mod
