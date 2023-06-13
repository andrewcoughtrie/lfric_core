!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> This is a code that uses the LFRic infrastructure to build a model that
!> includes the GungHo dynamical core and physics parametrisation schemes
!> that are currently provided through the use of unified model code.

!> @brief Main program used to illustrate an atmospheric model built using
!>        LFRic infrastructure

!> @details This top-level code simply calls initialise, run and finalise
!>          routines that are required to run the atmospheric model.

program lfric_atm

  use cli_mod,               only : get_initial_filename
  use driver_comm_mod,       only : init_comm, final_comm
  use driver_config_mod,     only : init_config, final_config
  use driver_log_mod,        only : init_logger, final_logger
  use gungho_mod,            only : gungho_required_namelists
  use gungho_driver_mod,     only : initialise, run, finalise
  use gungho_model_data_mod, only : model_data_type
  use mpi_mod,               only : global_mpi

  implicit none

  ! Model run working data set
  type(model_data_type) :: model_data

  character(*), parameter :: application_name = "lfric_atm"

  character(:), allocatable :: filename

  call init_comm( application_name )
  call get_initial_filename( filename )
  call init_config( filename, gungho_required_namelists )
  deallocate( filename )
  call init_logger( global_mpi%get_comm(), application_name )

  ! Create the depository, prognostics and diagnostics field collections
  call model_data%depository%initialise(name='depository', table_len=100)
  call model_data%prognostic_fields%initialise(name="prognostics", table_len=100)
  call model_data%diagnostic_fields%initialise(name="diagnostics", table_len=100)

  call initialise( application_name, model_data, global_mpi )
  call run( application_name, model_data )
  call finalise( application_name, model_data )

  call final_logger( application_name )
  call final_config()
  call final_comm()

end program lfric_atm
