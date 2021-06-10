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
  use mod_wait,                     only : init_wait
  use mpi_mod,                      only : finalise_comm, &
                                           initialise_comm
  use multires_coupling_driver_mod, only : initialise, run, finalise
  use xios,                         only : xios_finalize, &
                                           xios_initialize

  implicit none

  character(*), parameter :: xios_id = "multires_coupling"

  character(:), allocatable :: filename
  integer                   :: world_communicator = -999
  integer                   :: model_communicator = -999

  ! Initialse mpi and create the default communicator: mpi_comm_world
  call initialise_comm( world_communicator )

  ! Initialise XIOS and get back the split mpi communicator
  call init_wait()
  call xios_initialize( xios_id, return_comm=model_communicator )

  call get_initial_filename( filename )
  call initialise( filename, model_communicator )
  deallocate( filename )

  call run()

  call finalise()

  ! Finalise XIOS
  call xios_finalize()

  ! Finalise mpi and release the communicator
  call finalise_comm()

end program multires_coupling
