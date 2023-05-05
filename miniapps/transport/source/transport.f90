!-----------------------------------------------------------------------------
! (c) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @page transport Transport miniapp
!> Program file for running transport miniapp. Subroutine calls include initialise_transport(),
!> run_transport() and finalise_transport().
program transport

  use cli_mod,              only: get_initial_filename
  use driver_comm_mod,      only: init_comm, final_comm
  use mpi_mod,              only: global_mpi
  use transport_mod,        only: program_name
  use transport_driver_mod, only: initialise_transport, &
                                  run_transport,        &
                                  finalise_transport

  implicit none

  character(:), allocatable :: filename

  call get_initial_filename( filename )
  call init_comm( program_name )

  call initialise_transport( filename, global_mpi )

  call run_transport()

  call finalise_transport()
  call final_comm()

end program transport
