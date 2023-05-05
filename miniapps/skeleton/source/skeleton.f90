!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @page Miniapp skeleton program

!> @brief Main program used to illustrate how to write LFRic miniapps.

!> @details Calls init, run and finalise routines from a driver module

program skeleton

  use cli_mod,             only : get_initial_filename
  use driver_comm_mod,            only : init_comm, final_comm
  use mpi_mod,             only : global_mpi
  use skeleton_driver_mod, only : initialise, run, finalise

  implicit none

  character(:), allocatable :: filename

  call get_initial_filename( filename )
  call init_comm("skeleton")

  call initialise( filename, global_mpi )

  call run()

  call finalise()
  call final_comm()

end program skeleton
