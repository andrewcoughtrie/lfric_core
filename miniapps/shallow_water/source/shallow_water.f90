!-----------------------------------------------------------------------------
! (c) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @page shallow_water Shallow water equations miniapp
!> This is code that uses the LFRic infrastructure to build a shallow water
!> model that includes some of the GungHo routines.
!>
!> @brief Main program used to simulate shallow water equations.
!>
!> @details This top-level code simply calls initialise, run and finalise
!>          routines that are required to run the model.

program shallow_water

  use cli_mod,                      only: get_initial_filename
  use driver_comm_mod,              only: init_comm, final_comm
  use driver_config_mod,            only: init_config, final_config
  use driver_log_mod,               only: init_logger, final_logger
  use log_mod,                      only: log_event,       &
                                          log_level_trace, &
                                          log_scratch_space
  use mpi_mod,                      only: global_mpi
  use shallow_water_mod,            only: shallow_water_required_namelists
  use shallow_water_model_data_mod, only: model_data_type
  use shallow_water_driver_mod,     only: initialise, &
                                          run,        &
                                          finalise

  implicit none

  character(*), parameter :: program_name = "shallow_water"

  ! Model run working data set
  type(model_data_type)     :: model_data

  character(:), allocatable :: filename

  call init_comm( program_name )
  call get_initial_filename( filename )
  call init_config( filename, shallow_water_required_namelists )
  deallocate( filename )
  call init_logger( global_mpi%get_comm(), program_name )

  ! Create the depository and prognostics field collections
  call model_data%depository%initialise(name='depository', table_len=100)
  call model_data%prognostic_fields%initialise(name="prognostics", table_len=100)

  call log_event( 'Initialising Infrastructure ...', log_level_trace )
  call initialise( model_data, global_mpi, program_name )
  write(log_scratch_space,'("Running ", A, "...")') program_name
  call log_event( log_scratch_space, log_level_trace )
  call run( model_data )
  call log_event( 'Finalising ' // program_name // ' ...', log_level_trace )
  call finalise( model_data, program_name )

  call final_logger( program_name )
  call final_config()
  call final_comm()

end program shallow_water
