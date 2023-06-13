!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @page Miniapp multires_coupling program

!> @brief Main program used for multires_coupling miniapp

!> @details Calls init, run and finalise routines from a driver module

program multires_coupling

  use cli_mod,                      only : get_initial_filename
  use driver_comm_mod,              only : init_comm, final_comm
  use driver_config_mod,            only : init_config, final_config
  use driver_log_mod,               only : init_logger, final_logger
  use gungho_model_data_mod,        only : model_data_type
  use log_mod,                      only : log_event, log_level_trace
  use mpi_mod,                      only : global_mpi
  use multires_coupling_mod,        only : multires_required_namelists
  use multires_coupling_driver_mod, only : initialise, run, finalise

  implicit none

  character(*), parameter :: program_name = "multires_coupling"

  ! Model run working data set
  type (model_data_type) :: dynamics_mesh_model_data
  type (model_data_type) :: physics_mesh_model_data

  character(:), allocatable :: filename

  call init_comm( program_name )
  call get_initial_filename( filename )
  call init_config( filename, multires_required_namelists )
  deallocate( filename )
  call init_logger( global_mpi%get_comm(), program_name )

  ! Create the dynamics depository, prognostics and diagnostics field collections
  call dynamics_mesh_model_data%depository%initialise(name='depository', table_len=100)
  call dynamics_mesh_model_data%prognostic_fields%initialise(name="prognostics", table_len=100)
  call dynamics_mesh_model_data%diagnostic_fields%initialise(name="diagnostics", table_len=100)

  ! Create the physics depository, prognostics and diagnostics field collections
  call physics_mesh_model_data%depository%initialise(name='depository', table_len=100)
  call physics_mesh_model_data%prognostic_fields%initialise(name="prognostics", table_len=100)
  call physics_mesh_model_data%diagnostic_fields%initialise(name="diagnostics", table_len=100)

  call log_event( 'Initialising' // program_name // ' ...', log_level_trace )
  call initialise( dynamics_mesh_model_data, &
                   physics_mesh_model_data,  &
                   global_mpi,               &
                   program_name )
  call run( dynamics_mesh_model_data, physics_mesh_model_data )
  call log_event( 'Finalising ' // program_name // ' ...', log_level_trace )
  call finalise( dynamics_mesh_model_data, &
                 physics_mesh_model_data,  &
                 program_name )

  call final_logger( program_name )
  call final_config()
  call final_comm()

end program multires_coupling
