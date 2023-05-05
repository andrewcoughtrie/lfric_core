!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @page linear Linear model Program
!> This is a code that uses the LFRic infrastructure to build a model that
!> is the tangent linear/ perturbation forecast for gungho.

!> @brief Main program used to illustrate linear model functionality.

!> @details This top-level code simply calls initialise, run and finalise
!>          routines that are required to run the model.

program linear_model

  use cli_mod,           only : get_initial_filename
  use driver_comm_mod,   only : init_comm, final_comm
  use linear_driver_mod, only : initialise, run, finalise
  use mpi_mod,           only : global_mpi

  implicit none

  character(*), parameter :: application_name = "linear_model"

  character(:), allocatable :: filename

  call get_initial_filename( filename )
  call init_comm( application_name )

  call initialise( application_name, filename, global_mpi )

  call run( application_name )

  call finalise( application_name )
  call final_comm()

end program linear_model
