!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @page io_dev io_dev Miniapp
!> Test program for the XIOS IO implementation.

!> @brief Main program used to test XIOS setup and output of a field.

program io_dev

  use cli_mod,           only: get_initial_filename
  use driver_comm_mod,   only: init_comm, final_comm
  use io_dev_driver_mod, only: initialise, run, finalise
  use mpi_mod,           only: global_mpi

  implicit none

  character(:), allocatable :: filename

  call get_initial_filename( filename )

  call init_comm( "io_dev" )
  call initialise( filename, global_mpi )

  call run()

  call finalise()
  call final_comm()

end program io_dev
