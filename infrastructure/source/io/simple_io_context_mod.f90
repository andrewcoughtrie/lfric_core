!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Simple context supporting only a simple time-step clock.
!>
module simple_io_context_mod

  use clock_mod,         only : clock_type
  use constants_mod,     only : i_native, r_second
  use io_context_mod,    only : io_context_type, io_context_initialiser_type
  use log_mod,           only : log_event, log_level_error
  use step_calendar_mod, only : step_calendar_type

  implicit none

  private

  !> @brief I/O context with no external dependencies.
  !>
  !> This context may be used where simplified I/O which does not require
  !> additional libraries is desirable.
  !>
  type, public, extends(io_context_type) :: simple_io_context_type
    private
    type(clock_type), allocatable :: clock
  contains
    private
    procedure, public :: initialise
    procedure, public :: get_clock
  end type simple_io_context_type

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Set up a declared simple_io_context object.
  !>
  !> @param[in] callback          Object to call mid initialisation.
  !> @param[in] start_time        Tim of first step.
  !> @param[in] finish_time       Time at end of last step.
  !> @param[in] spinup_period     Number of seconds in spinup period.
  !> @param[in] seconds_per_step  Number of seconds in a timestep.
  !>
  subroutine initialise( this,                    &
                         callback,                &
                         start_time, finish_time, &
                         spinup_period,           &
                         seconds_per_step )

    implicit none

    class(simple_io_context_type),      intent(inout), target :: this
    class(io_context_initialiser_type), intent(inout)         :: callback
    character(*),                       intent(in) :: start_time
    character(*),                       intent(in) :: finish_time
    real(r_second),                     intent(in) :: spinup_period
    real(r_second),                     intent(in) :: seconds_per_step

    type(step_calendar_type), allocatable :: calendar
    integer(i_native)                     :: rc

    allocate( calendar, stat=rc )
    if (rc /= 0) then
      call log_event( "Unable to allocate calendar", log_level_error )
    end if

    allocate( this%clock, stat=rc )
    if (rc /= 0) then
      call log_event( "Failed to allocate clock", log_level_error )
    end if
    call this%clock%initialise( calendar, start_time, finish_time, &
                                seconds_per_step, spinup_period )

    !> @todo Rather than using this callback we might prefer to pass arrays
    !>       of objects which describe things to be set up. Alternatively we
    !>       could provide calls to "add axis" and "add file" with a final
    !>       "complete context".
    !>
    call callback%callback( this )

  end subroutine initialise

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Gets the clock associated with this context.
  !>
  !> @return Context's clock object.
  !>
  function get_clock( this ) result(clock)

    implicit none

    class(simple_io_context_type), intent(in), target :: this
    class(clock_type), pointer :: clock

    clock => this%clock

  end function get_clock

end module simple_io_context_mod
