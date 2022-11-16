!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Wrap the XIOS context in an object for easier management and cleaner code.
!>
module lfric_xios_context_mod

  use clock_mod,            only : clock_type
  use constants_mod,        only : i_native, &
                                   r_second, &
                                   l_def
  use field_mod,            only : field_type
  use file_mod,             only : file_type
  use io_context_mod,       only : io_context_type
  use lfric_xios_file_mod,  only : lfric_xios_file_type
  use log_mod,              only : log_event,       &
                                   log_level_error, &
                                   log_level_info
  use step_calendar_mod,    only : step_calendar_type
  use lfric_xios_clock_mod, only : lfric_xios_clock_type
  use lfric_xios_setup_mod, only : init_xios_dimensions, &
                                   setup_xios_files
  use lfric_xios_file_mod,  only : lfric_xios_file_type
  use lfric_xios_utils_mod, only : parse_date_as_xios
  use linked_list_mod,      only : linked_list_type, linked_list_item_type
  use xios,                 only : xios_context,                  &
                                   xios_context_initialize,       &
                                   xios_close_context_definition, &
                                   xios_context_finalize,         &
                                   xios_date,                     &
                                   xios_define_calendar,          &
                                   xios_get_handle,               &
                                   xios_set_current_context,      &
                                   xios_update_calendar
  use mod_wait,             only : init_wait

  implicit none

  private

  !> Contains an instance of an XIOS context and manages interactions between
  !> the model and the context.
  type, public, extends(io_context_type) :: lfric_xios_context_type
    private
    character(:),                 allocatable :: id
    type(xios_context)                        :: handle
    class(lfric_xios_clock_type), allocatable :: clock
    type(linked_list_type)                    :: filelist
    logical                                   :: uses_timers = .false.

  contains
    private
    procedure, public :: initialise
    procedure, public :: get_clock
    procedure, public :: get_filelist
    procedure, public :: advance
    procedure, public :: set_timer_flag
    final :: finalise
  end type lfric_xios_context_type

contains

  !> @brief Set up an XIOS context object.
  !>
  !> @param [in]     id                Unique identifying string.
  !> @param [in]     communicator      MPI communicator used by context.
  !> @param [in]     chi               Array of coordinate fields
  !> @param [in]     panel_id          Panel ID field
  !> @param [in]     start_time        Time of first step.
  !> @param [in]     finish_time       Time of last step.
  !> @param [in]     spinup_period     Number of seconds in spinup period.
  !> @param [in]     seconds_per_step  Number of seconds in a time step.
  !> @param [in]     calendar_start    Start date for calendar
  !> @param [in]     calendar_type     Type of calendar.
  !> @param [in]     alt_coords        Array of coordinate fields for alternative meshes
  !> @param [in]     alt_panel_ids     Panel ID fields for alternative meshes
  subroutine initialise( this, id, communicator,          &
                         chi, panel_id,                   &
                         start_time, finish_time,         &
                         spinup_period, seconds_per_step, &
                         calendar_start, calendar_type,   &
                         alt_coords, alt_panel_ids )

    implicit none

    class(lfric_xios_context_type),   intent(inout) :: this
    character(*),                     intent(in)    :: id
    integer(i_native),                intent(in)    :: communicator
    class(field_type),                intent(in)    :: chi(:)
    class(field_type),                intent(in)    :: panel_id
    character(*),                     intent(in)    :: start_time
    character(*),                     intent(in)    :: finish_time
    real(r_second),                   intent(in)    :: spinup_period
    real(r_second),                   intent(in)    :: seconds_per_step
    character(*),                     intent(in)    :: calendar_start
    character(*),                     intent(in)    :: calendar_type
    type(field_type),       optional, intent(in)    :: alt_coords(:,:)
    type(field_type),       optional, intent(in)    :: alt_panel_ids(:)

    type(step_calendar_type), allocatable :: calendar
    integer(i_native)                     :: rc
    type(xios_date)                       :: calendar_start_xios

    type(linked_list_item_type), pointer :: loop => null()
    type(lfric_xios_file_type),  pointer :: file => null()

    call xios_context_initialize( id, communicator )
    call xios_get_handle( id, this%handle )
    call xios_set_current_context( this%handle )

    ! Calendar start is adjusted when clock is initialised
    calendar_start_xios = parse_date_as_xios(trim(adjustl(calendar_start)))
    call xios_define_calendar( type=calendar_type,              &
                               time_origin=calendar_start_xios, &
                               start_date=calendar_start_xios )

    allocate( calendar, stat=rc )
    if (rc /= 0) then
      call log_event( "Unable to allocate calendar", log_level_error )
    end if

    allocate( this%clock,                                            &
              source=lfric_xios_clock_type( calendar,                &
                                            start_time, finish_time, &
                                            seconds_per_step,        &
                                            this%uses_timers ),      &
              stat=rc )
    if (rc /= 0) then
      call log_event( "Unable to allocate clock", log_level_error )
    end if

    ! Run XIOS setup routines
    call init_xios_dimensions(chi, panel_id, alt_coords, alt_panel_ids)
    if (this%filelist%get_length() > 0) call setup_xios_files(this%filelist)

    ! Close the context definition - no more I/O operations can be defined
    ! after this point
    call xios_close_context_definition()

    ! Read all files that need to be read from
    if (this%filelist%get_length() > 0) then
      loop => this%filelist%get_head()
      do while (associated(loop))
        select type(list_item => loop%payload)
          type is (lfric_xios_file_type)
            file => list_item
            if (file%mode_is_read()) call file%recv_fields()
        end select
        loop => loop%next
      end do
    end if


  end subroutine initialise

  !> Finaliser for lfric_xios_context object.
  subroutine finalise( this )

    implicit none

    type(lfric_xios_context_type), intent(inout) :: this

    type(linked_list_item_type), pointer :: loop => null()
    type(lfric_xios_file_type),  pointer :: file => null()

    ! Perform final write
    if (this%filelist%get_length() > 0) then
      loop => this%filelist%get_head()
      do while (associated(loop))
        select type( list_item => loop%payload )
          type is (lfric_xios_file_type)
            file => list_item
            if (file%mode_is_write()) call file%send_fields()
        end select
        loop => loop%next
      end do
    end if

    ! Finalise the XIOS context - all data will be written to disk and files
    ! will be closed.
    call xios_context_finalize()

    ! We have closed the context on our end, but we need to make sure that XIOS
    ! has closed the files for all servers before we process them.
    call init_wait()

    ! Close all files in list
    if (this%filelist%get_length() > 0) then
      loop => this%filelist%get_head()
      do while (associated(loop))
        select type( list_item => loop%payload )
          type is (lfric_xios_file_type)
            file => list_item
            call file%file_close()
        end select
        loop => loop%next
      end do
    end if

    nullify(loop)
    nullify(file)

  end subroutine finalise

  !> Advances the XIOS context forward in time, performing all I/O operations
  !> expected by XIOS at the end and beginning of the current and subsequent
  !> timesteps.
  !>
  !> @param[in] clock The model's clock
  subroutine advance(this, clock)

    implicit none

    class(lfric_xios_context_type), intent(inout) :: this
    class(clock_type),              intent(in)    :: clock

    type(linked_list_item_type), pointer :: loop => null()
    type(lfric_xios_file_type),  pointer :: file => null()

    ! Write all files that need to be written to
    if (this%filelist%get_length() > 0) then
      loop => this%filelist%get_head()
      do while (associated(loop))
        select type(list_item => loop%payload)
          type is (lfric_xios_file_type)
            file => list_item
            if (file%mode_is_write()) call file%send_fields()
        end select
        loop => loop%next
      end do
    end if

    ! Update XIOS calendar
    call xios_update_calendar( clock%get_step() - clock%get_first_step() + 1 )

    ! Read all files that need to be read from
    if (this%filelist%get_length() > 0) then
      loop => this%filelist%get_head()
      do while (associated(loop))
        select type(list_item => loop%payload)
          type is (lfric_xios_file_type)
            file => list_item
            if (file%mode_is_read()) call file%recv_fields()
        end select
        loop => loop%next
      end do
    end if

    nullify(loop)
    nullify(file)

  end subroutine advance

  !> Gets the clock associated with this context.
  !>
  !> @return Clock object.
  function get_clock( this ) result(clock)

    implicit none

    class(lfric_xios_context_type), intent(in), target :: this
    class(clock_type), pointer :: clock

    clock => this%clock

  end function get_clock

  !> Gets the file list associated with this context.
  !>
  !> @return Linked list of file objects
  function get_filelist( this ) result(filelist)

    implicit none

    class(lfric_xios_context_type), intent(in), target :: this
    type(linked_list_type), pointer :: filelist

    filelist => this%filelist

  end function get_filelist

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Tells I/O context whether to use subroutine timers
  !>
  !> @param[in] timer_flag
  !>
  subroutine set_timer_flag( this, timer_flag )

    implicit none

    class(lfric_xios_context_type), target, intent(inout) :: this
    logical,                                intent(in)    :: timer_flag

    this%uses_timers = timer_flag

  end subroutine set_timer_flag

end module lfric_xios_context_mod
