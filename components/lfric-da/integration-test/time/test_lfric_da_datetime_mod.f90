!-----------------------------------------------------------------------------
! (c) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!>@brief Tests the jedi_datetime type init_lfric_calendar_start routine
module test_lfric_da_datetime_mod

  use constants_mod,                 only : i_timestep, l_def
  use lfric_da_datetime_mod,         only : jedi_datetime_type
  use log_mod,                       only : log_event, &
                                            LOG_LEVEL_INFO

  implicit none

  private
  public test_init_lfric_calendar_start,     &
         test_init_lfric_calendar_start_err, &
         test_init_string_err,               &
         test_copy_from_jedi_datetime_err,   &
         test_add_duration_to_datetime,      &
         test_duration_from_datetimes

  character(len=128) :: output

contains

  !> @brief Test initialising a datetime from
  !!        the lfric calendar_start namelist variable
  subroutine test_init_lfric_calendar_start()

    implicit none

    type(jedi_datetime_type) :: jedi_datetime
    integer(i_timestep)      :: date, returned_date
    integer(i_timestep)      :: time, returned_time

    date = 2457389
    time = 54000

    call jedi_datetime%init_lfric_calendar_start()
    call jedi_datetime%get_date(returned_date)
    call jedi_datetime%get_time(returned_time)

    if ( (returned_date == date) .and. (returned_time == time) ) then
      call log_event( 'test PASS', LOG_LEVEL_INFO )
    else
      write ( output, '(4(A, I10))' ) 'test FAIL with values: date = ',  &
                                      returned_date, ' time = ',         &
                                      returned_time, 'expected date = ', &
                                      date, ' time = ', time
      call log_event( output, LOG_LEVEL_INFO )
    end if

  end subroutine test_init_lfric_calendar_start

  !> @brief Test logging an error initialising a datetime
  !!        from the lfric calendar_start namelist variable
  !!        if it hasn't been read yet
  subroutine test_init_lfric_calendar_start_err()

    implicit none

    type(jedi_datetime_type) :: jedi_datetime

    call jedi_datetime%init_lfric_calendar_start()

  end subroutine test_init_lfric_calendar_start_err

  !> @brief Tests logging an error when initialising a datetime
  !!        with a bad string
  subroutine test_init_string_err()

    implicit none

    type(jedi_datetime_type) :: jedi_datetime

    call jedi_datetime%init( '12-34-@5 11:34:23.6' )

  end subroutine test_init_string_err

  !> @brief Tests logging an error when attempting to initialise
  !!        a datetime with another uninitialised datetime
  subroutine test_copy_from_jedi_datetime_err()

    use lfric_da_duration_mod, only : jedi_duration_type

    implicit none

    type(jedi_datetime_type) :: jedi_datetime
    type(jedi_datetime_type) :: jedi_datetime_2

    jedi_datetime_2 = jedi_datetime

  end subroutine test_copy_from_jedi_datetime_err

  !> @brief Tests adding a jedi_duration instance to a
  !!        jedi_datetime instance
  subroutine test_add_duration_to_datetime()

    use lfric_da_duration_mod, only : jedi_duration_type

    implicit none

    type(jedi_datetime_type) :: jedi_datetime
    type(jedi_duration_type) :: jedi_duration

    integer(i_timestep) :: time, returned_time

    call jedi_datetime%init( 20230405, 161930 )
    time = 58772

    call jedi_duration%init(2)

    jedi_datetime = jedi_datetime + jedi_duration
    call jedi_datetime%get_time( returned_time )
    if ( time == returned_time ) then
      call log_event( 'test PASS', LOG_LEVEL_INFO )
    else
      write ( output, '(2(A, I10))' ) 'test FAIL with values: time = ', &
                                      returned_time, ' expected time = ', time
      call log_event( output, LOG_LEVEL_INFO )
    end if

  end subroutine test_add_duration_to_datetime

  !> @brief Test getting a duration instance by subtracting
  !!        one jedi datetime from another
  subroutine test_duration_from_datetimes()

    use lfric_da_duration_mod, only : jedi_duration_type

    implicit none

    type(jedi_datetime_type) :: jedi_datetime
    type(jedi_datetime_type) :: jedi_datetime_2
    type(jedi_duration_type) :: jedi_duration

    integer(i_timestep) :: seconds

    logical(l_def) :: pass
    pass = .true.

    call jedi_datetime%init(   20230405, 161930 )
    call jedi_datetime_2%init( 20230405, 161931 )

    ! Test getting difference between 2 datetimes
    jedi_duration = jedi_datetime_2 - jedi_datetime
    call jedi_duration%get_duration( seconds )
    if ( 1 /= seconds ) pass = .false.

    ! Test getting difference between 2 datetimes
    jedi_duration = jedi_datetime - jedi_datetime_2
    call jedi_duration%get_duration( seconds )
    if ( -1 /= seconds ) pass = .false.

    ! Test case if over a days difference
    call jedi_datetime_2%init( 20230406, 161931 )
    jedi_duration = jedi_datetime_2 - jedi_datetime
    call jedi_duration%get_duration( seconds )
    if ( 86401 /= seconds ) pass = .false.

    if ( pass ) then
      call log_event( 'test PASS', LOG_LEVEL_INFO )
    else
      call log_event( 'test FAIL', LOG_LEVEL_INFO )
    end if

  end subroutine test_duration_from_datetimes

end module test_lfric_da_datetime_mod
