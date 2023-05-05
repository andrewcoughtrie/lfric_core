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
  use mpi_mod,                      only : global_mpi
  use multires_coupling_mod,        only : program_name
  use multires_coupling_driver_mod, only : initialise, run, finalise

  implicit none

  character(:), allocatable :: filename

  call get_initial_filename( filename )
  call init_comm( program_name )

  call initialise( filename, global_mpi )

  call run()

  call finalise()
  call final_comm()

end program multires_coupling
