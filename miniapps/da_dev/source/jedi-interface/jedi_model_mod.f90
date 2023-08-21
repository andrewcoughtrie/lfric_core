!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing a class than handles the non-linear model
!>
!> @details This module includes a class that handles the non-linear models
!>          time stepping. An included forecast application uses the model
!>          init, step and final to run a forecast.
!>
module jedi_model_mod

  use constants_mod,                 only : i_timestep
  use lfric_da_datetime_mod,         only : jedi_datetime_type
  use lfric_da_duration_mod,         only : jedi_duration_type
  use jedi_state_mod,                only : jedi_state_type
  use log_mod,                       only : log_event,          &
                                            log_scratch_space,  &
                                            LOG_LEVEL_ERROR

  implicit none

  private

type, public :: jedi_model_type
  private

  !> The data map between external field data and LFRic fields
  integer( kind=i_timestep ) :: datetime_duration_dt

contains

  !> Model initialiser.
  procedure, public  :: initialise

  !> Methods
  procedure, private :: model_init
  procedure, private :: model_step
  procedure, private :: model_final

  !> Run a forecast
  procedure, public  :: forecast

  !> Finalizer
  final              :: jedi_model_destructor

end type jedi_model_type

!------------------------------------------------------------------------------
! Contained functions/subroutines
!------------------------------------------------------------------------------
contains

!> @brief    Initialiser for jedi_model_type
!>
!> @param [in] datetime_duration_dt The time step duration
subroutine initialise( self, datetime_duration_dt )

  implicit none

  class( jedi_model_type ),   intent(inout) :: self
  integer( kind=i_timestep ), intent(in)    :: datetime_duration_dt

  self%datetime_duration_dt = datetime_duration_dt

end subroutine initialise


!> @brief    Initialise the model
!>
!> @param [inout] state State object to be used in the model initialise
subroutine model_init(self, state)

  implicit none

  class( jedi_model_type ), target, intent(in) :: self
  type( jedi_state_type ),  intent(inout)      :: state

end subroutine model_init

!> @brief    Step the model
!>
!> @param [inout] state State object to propagated
subroutine model_step(self, state)

  use lfric_da_fake_nl_driver_mod, only : model_clock, step

  implicit none

  class( jedi_model_type ), target, intent(inout) :: self
  type( jedi_state_type ),          intent(inout) :: state

  ! Local
  logical :: clock_stopped

  ! check the clock
  clock_stopped = .not. model_clock%tick()
  ! If the clock has finished then it will just get the
  ! data at the end of the file - this prevents that
  if ( clock_stopped ) then
    write ( log_scratch_space, '(A)' ) &
            "Model::model_step::The LFRic clock has stopped."
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  endif

  call step( state%model_data )

  ! Copy fields from model data
  call state%from_model_data()

  ! update the state time
  call state%update_time( self%datetime_duration_dt )

end subroutine model_step


!> @brief    Finalise the model
!>
!> @param [inout] state State object to be used in the model finalise
subroutine model_final(self, state)

  implicit none

  class( jedi_model_type ), target, intent(in) :: self
  type( jedi_state_type ),  intent(inout)      :: state

end subroutine model_final

!> @brief    Finalize the jedi_pseudo_model_type
!>
subroutine jedi_model_destructor(self)

  implicit none

  type(jedi_model_type), intent(inout) :: self

end subroutine jedi_model_destructor

!------------------------------------------------------------------------------
! OOPS defined forecast method
!------------------------------------------------------------------------------

!> @brief    Run a forecast using the model init, step and final
!>
!> @param [inout] state           The state object to propagate
!> @param [in] datetime_duration  The duration of the forecast
subroutine forecast(self, state, datetime_duration)

  implicit none

  class( jedi_model_type ), target, intent(inout) :: self
  type( jedi_state_type ),          intent(inout) :: state
  type( jedi_duration_type ),       intent(in)    :: datetime_duration

  ! Local
  type( jedi_datetime_type ) :: datetime_end

  ! End time
  datetime_end = state%datetime + datetime_duration

  call self%model_init( state )
  ! initialize the post processor
  ! post.initialize(state, ...);
  ! In a H(X) the following call would be used to do spatial interpolations
  ! on the Atlas/dummy field data
  ! post%process(state)

  ! loop until date_time_end
  do while ( datetime_end%is_ahead( state%datetime ) )
    call self%model_step( state )
    ! post%process(state)
  end do

  ! post.finalize(state);
  call self%model_final( state )

end subroutine forecast

end module jedi_model_mod
