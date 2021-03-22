!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief A simple I/O system.
!>
!> This I/O system has very limited functionality but it does not depend on 3rd
!> party libraries.
!>
module simple_io_mod

  use constants_mod,         only : i_timestep, r_second
  use io_context_mod,        only : io_context_type
  use log_mod,               only : log_event, log_level_error
  use io_context_mod,        only : io_context_initialiser_type
  use simple_io_context_mod, only : simple_io_context_type

  implicit none

  private
  public :: initialise_simple_io

  !> @brief Context initialisation callback class.
  !>
  type, extends(io_context_initialiser_type) :: setup_simple_io_type
    private
  contains
    private
    procedure, public :: initialise => initialise_setup_simple_io
    procedure, public :: callback   => callback_setup_simple_io
  end type setup_simple_io_type

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Sets up declared setup_simple_io_type object.
  !>
  subroutine initialise_setup_simple_io( this )
    implicit none
    class(setup_simple_io_type), intent(in) :: this
  end subroutine initialise_setup_simple_io

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Performs additional context initialisation.
  !>
  subroutine callback_setup_simple_io( this, context )
    implicit none
    class(setup_simple_io_type), intent(inout) :: this
    class(io_context_type),      intent(in)    :: context
  end subroutine callback_setup_simple_io

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Creates a simp_io_context_type.
  !>
  !> @param[out] context           Freshly minted context object.
  !> @param[in]  start_time        Time of first time step.
  !> @param[in]  end_time          Time at end of last time step.
  !> @param[in]  spinup_period     Number of seconds in spinup period.
  !> @param[in]  seconds_per_step  Number of seconds in one time step.
  !>
  subroutine initialise_simple_io( context,       &
                                   start_time,    &
                                   end_time,      &
                                   spinup_period, &
                                   seconds_per_step )

    implicit none

    class(io_context_type), intent(out), allocatable :: context
    character(*),           intent(in)               :: start_time
    character(*),           intent(in)               :: end_time
    real(r_second),         intent(in)               :: spinup_period
    real(r_second),         intent(in)               :: seconds_per_step

    type(setup_simple_io_type) :: callback

    integer :: rc

    call callback%initialise()

    allocate( simple_io_context_type::context, stat=rc )
    if (rc /= 0) then
      call log_event( "Unable to allocate simple I/O context.", &
                      log_level_error )
    end if
    select type(context)
      class is (simple_io_context_type)
        call context%initialise( callback,      &
                                 start_time,    &
                                 end_time,      &
                                 spinup_period, &
                                 seconds_per_step )
      ! No need for default as we allocated it just above
    end select

  end subroutine initialise_simple_io

end module simple_io_mod
