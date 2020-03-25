!-----------------------------------------------------------------------------
! (c) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Handles calling of calender update for gungho

module gungho_update_calendar_mod

  use clock_mod,       only : clock_type
  use constants_mod,   only : i_timestep
  use io_config_mod,   only : use_xios_io
  use log_mod,         only : log_event, &
                              LOG_LEVEL_INFO
  use xios,            only : xios_update_calendar

  implicit none

  private
  public gungho_update_calendar

  contains

  !> @brief Call calendar update if required
  !> @param[in] timestep number of current timestep
  subroutine gungho_update_calendar( clock )

    implicit none

    class(clock_type), intent(in) :: clock

    integer(i_timestep) :: new_step

    ! Update XIOS calendar if we are using it for diagnostic output or
    ! checkpoint
    if ( use_xios_io ) then
      call log_event( "Gungho: Updating XIOS timestep", LOG_LEVEL_INFO )
      new_step = clock%get_step() - clock%get_first_step() + 1
      call xios_update_calendar( new_step )
    end if

  end subroutine gungho_update_calendar

end module gungho_update_calendar_mod
