!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Wrap the XIOS context in an object for easier management and cleaner coding.
!>
module lfric_xios_context_mod

  use clock_mod,            only : clock_type
  use constants_mod,        only : i_native, &
                                   r_second, &
                                   l_def
  use io_context_mod,       only : io_context_type, &
                                   io_context_initialiser_type
  use lfric_xios_clock_mod, only : lfric_xios_clock_type
  use lfric_xios_file_mod,  only : xios_file_type
  use log_mod,              only : log_event,       &
                                   log_level_error, &
                                   log_level_info
  use step_calendar_mod,    only : step_calendar_type
  use xios,                 only : xios_close_context_definition, &
                                   xios_context,                  &
                                   xios_context_finalize,         &
                                   xios_context_initialize,       &
                                   xios_get_handle,               &
                                   xios_set_current_context

  implicit none

  public :: filelist_populator
  private

  !> Manages interactions with XIOS.
  !>
  type, public, extends(io_context_type) :: lfric_xios_context_type
    private
    character(:),                 allocatable :: id
    type(xios_context)                        :: handle
    class(lfric_xios_clock_type), allocatable :: clock
    type(xios_file_type),         allocatable :: filelist(:)

  contains
    private
    procedure, public :: initialise
    procedure, public :: get_clock
    procedure, public :: get_filelist
    final :: finalise
  end type lfric_xios_context_type

abstract interface

  subroutine filelist_populator(files_list, clock)
    import xios_file_type, clock_type
    type(xios_file_type), allocatable, intent(out) :: files_list(:)
    class(clock_type),                 intent(in)  :: clock
  end subroutine filelist_populator

end interface

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Set up an XIOS context object.
  !>
  !> @param [in]     id                Unique identifying string.
  !> @param [in]     communicator      MPI communicator used by context.
  !> @param [in,out] callback          Object to handle model specifics.
  !> @param [in]     start_time        Time of first step.
  !> @param [in]     finish_time       Time of last step.
  !> @param [in]     spinup_period     Number of seconds in spinup period.
  !> @param [in]     seconds_per_step  Number of seconds in a time step.
  !> @param [in]     timer_flag        Flag for use of subroutine timers.
  !> @param [in]     populate_filelist Procedure use to populate list of files
  !>
  subroutine initialise( this, id, communicator,  &
                         callback,                &
                         start_time, finish_time, &
                         spinup_period,           &
                         seconds_per_step,        &
                         timer_flag,              &
                         populate_filelist )

    implicit none

    class(lfric_xios_context_type),     intent(inout) :: this
    character(*),                       intent(in)    :: id
    integer(i_native),                  intent(in)    :: communicator
    class(io_context_initialiser_type), intent(inout) :: callback
    character(*),                       intent(in)    :: start_time
    character(*),                       intent(in)    :: finish_time
    real(r_second),                     intent(in)    :: spinup_period
    real(r_second),                     intent(in)    :: seconds_per_step
    logical(l_def), optional,           intent(in)    :: timer_flag
    procedure(filelist_populator),  &
                     pointer, optional, intent(in)    :: populate_filelist

    type(step_calendar_type), allocatable :: calendar
    integer(i_native)                     :: rc

    call xios_context_initialize( id, communicator )
    call xios_get_handle( id, this%handle )
    call xios_set_current_context( this%handle )

    allocate( calendar, stat=rc )
    if (rc /= 0) then
      call log_event( "Unable to allocate calendar", log_level_error )
    end if

    allocate( this%clock,                                            &
              source=lfric_xios_clock_type( calendar,                &
                                            start_time, finish_time, &
                                            seconds_per_step,        &
                                            spinup_period,           &
                                            timer_flag=timer_flag ), &
              stat=rc )
    if (rc /= 0) then
      call log_event( "Unable to allocate clock", log_level_error )
    end if

    if (present(populate_filelist)) then
      call populate_filelist(this%filelist, this%clock)
    else
      allocate(this%filelist(0))
    end if

    !> @todo Rather than using this callback we might prefer to pass arrays
    !>       of objects which describe things to be set up. Alternatively we
    !>       could provide calls to "add axis" and "add file" with a final
    !>       "complete context".
    !>
    call callback%callback( this )

    call xios_close_context_definition()

  end subroutine initialise

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Clean up an XIOS context object.
  !>
  subroutine finalise( this )

    implicit none

    type(lfric_xios_context_type), intent(in) :: this

    call xios_context_finalize()

  end subroutine finalise

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the clock associated with this context.
  !>
  !> @return Clock object.
  !>
  function get_clock( this ) result(clock)

    implicit none

    class(lfric_xios_context_type), intent(in), target :: this
    class(clock_type), pointer :: clock

    clock => this%clock

  end function get_clock

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the context's file list
  !>
  !> @return  The list of XIOS file type objects used by the model
  !>
  function get_filelist( this ) result( filelist )

    implicit none

    class(lfric_xios_context_type), intent(in) :: this
    type(xios_file_type), allocatable :: filelist(:)

    filelist = this%filelist

  end function get_filelist

end module lfric_xios_context_mod
