!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Integrate the model clock with the XIOS clock.
!>
module lfric_xios_clock_mod

  use calendar_mod,  only : calendar_type
  use clock_mod,     only : clock_type
  use constants_mod, only : i_timestep, r_second
  use xios,          only : operator(+),         &
                            xios_date,           &
                            xios_duration,       &
                            xios_get_start_date, &
                            xios_set_start_date, &
                            xios_set_timestep,   &
                            xios_update_calendar

  implicit none

  private

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Integrates the model's clock with XIOS.
  !>
  type, public, extends(clock_type) :: lfric_xios_clock_type
    private
    integer :: step_offset
  contains
    private
    procedure, public :: initialise
    procedure, public :: post_context
    procedure, public :: tick
  end type lfric_xios_clock_type

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Sets up an XIOS clock object.
  !>
  !> @param [in] calendar          Interprets human readable times.
  !> @param [in] first             Time of first step.
  !> @param [in] last              Time of last step.
  !> @param [in] seconds_per_step  Length of a time step in seconds.
  !> @param [in] spinup_period     Number of seconds in spinup period.
  !>
  subroutine initialise( this,             &
                         calendar,         &
                         first,            &
                         last,             &
                         seconds_per_step, &
                         spinup_period )

    implicit none

    class(lfric_xios_clock_type), intent(inout) :: this
    class(calendar_type), intent(in)    :: calendar
    character(*),         intent(in)    :: first
    character(*),         intent(in)    :: last
    real(r_second),       intent(in)    :: seconds_per_step
    real(r_second),       intent(in)    :: spinup_period

    type(xios_duration) :: timestep_length_for_xios

    call this%clock_type%initialise( calendar,         &
                                     first,            &
                                     last,             &
                                     seconds_per_step, &
                                     spinup_period )
    this%step_offset = this%get_first_step() - 1

    ! Set the XIOS time-step from the model clock
    timestep_length_for_xios%second = this%get_seconds_per_step()
    call xios_set_timestep( timestep_length_for_xios )

  end subroutine initialise

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Performs post context setup.
  !>
  subroutine post_context( this )

    implicit none

    class(lfric_xios_clock_type), intent(in) :: this

    integer(i_timestep) :: checkpoint_step

    checkpoint_step = this%get_first_step() - 1
    if (checkpoint_step < 1) then
      checkpoint_step = 1
    end if
    call xios_update_calendar( checkpoint_step )

  end subroutine post_context

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Advances the clock by one step.
  !>
  function tick( this )

    implicit none

    class(lfric_xios_clock_type), intent(inout) :: this
    logical                                     :: tick

    tick = this%clock_type%tick()
    call xios_update_calendar( this%get_step() )

  end function tick

end module lfric_xios_clock_mod
