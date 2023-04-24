!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> Module controlling the initialisation and finalisation of the model
!! communicator
!>
module driver_comm_mod

  use constants_mod,         only: i_native, l_def
  use halo_comms_mod,        only: initialise_halo_comms, &
                                   finalise_halo_comms
  use mpi_mod,               only: global_mpi, create_comm, destroy_comm

! MCT flag used for models coupled to OASIS-MCT ocean model
#ifdef MCT
  use coupler_mod,           only: cpl_initialize, cpl_finalize
#endif
! USE_XIOS flag used for models using the XIOS I/O server
#ifdef USE_XIOS
  use lfric_xios_driver_mod, only: lfric_xios_initialise, lfric_xios_finalise
#endif

  implicit none

  public :: init_comm, final_comm
  private

  ! Private module variable describes if this module created the MPI comm
  logical(l_def) :: comm_created = .false.

contains

  !> @brief  Initialises the model communicator
  !>
  !> @param[in]    program_name  The model name
  !> @param[in]    input_comm    An optional argument that can be supplied if
  !>                             mpi has been initialised outside the model.
  !>                             In that case, this provides the communicator
  !>                             that should be used
  subroutine init_comm( program_name, input_comm )

    implicit none

    character(len=*),       intent(in)           :: program_name
    integer(kind=i_native), optional, intent(in) :: input_comm

    integer(kind=i_native) :: world_communicator = -999
    integer(kind=i_native) :: model_communicator = -999

    logical :: comm_is_split

    ! Comm has not been split yet
    comm_is_split = .false.

    ! Get the world communicator
    if (present(input_comm)) then
      ! set eqaul to world communicator
      world_communicator = input_comm
    else
      ! Create a communicator by initialsing mpi and returning mpi_comm_world
      call create_comm( world_communicator )
      comm_created = .true.
    endif

#ifdef MCT
    ! Initialise OASIS coupling and get back the split communicator
    call cpl_initialize( model_communicator )
    comm_is_split = .true.
#endif

#ifdef USE_XIOS
    ! Initialise XIOS and get back the split communicator (if not split already)
    ! (This looks like the communicator from XIOS will just overwrite the one
    ! from Oasis, above, but there's magic when XIOS and Oasis are built
    ! together in a special way that means the correct communicator is returned)
    call lfric_xios_initialise( program_name, model_communicator, comm_is_split )
    comm_is_split = .true.
#endif

    ! If communicator has not been split, set model comm as world comm
    if (.not. comm_is_split) model_communicator = world_communicator

    !Store the MPI communicator for later use
    call global_mpi%initialise( model_communicator )

    ! Initialise halo functionality
    call initialise_halo_comms( model_communicator )

  end subroutine init_comm

  !> @brief  Finalises the model communicator
  subroutine final_comm()

    implicit none

#ifdef USE_XIOS
    ! Finalise XIOS
    call lfric_xios_finalise()
#endif

#ifdef MCT
    ! FInalise OASIS coupling
    call cpl_finalize()
#endif

    ! Finalise halo exchange functionality
    call finalise_halo_comms()

    ! Finalise the mpi object
    call global_mpi%finalise()
    ! Release the communicator if it is ours to release. If a communicator has
    ! been provided to LFRic, then that is someone else's responsibility
    if(comm_created)call destroy_comm()

  end subroutine final_comm

end module driver_comm_mod
