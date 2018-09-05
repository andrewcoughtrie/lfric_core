!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!>@brief Drives the execution of the Lfric_atm model.
!>
!> This is a temporary solution until we have a proper driver layer.
!>
module lfric_atm_driver_mod


  use constants_mod,              only : i_def, imdi
  use planet_constants_mod,       only : set_planet_constants
  use gungho_driver_mod,          only : gungho_initialise => initialise, &
                                         gungho_run => run,               &
                                         gungho_finalise => finalise

  implicit none

  private
  public initialise, run, finalise

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !>@brief Sets up required state in preparation for run.
  !>@param[in] filename Name of the file containing the desired configuration 
  subroutine initialise( filename )

    implicit none

    character(:),intent(in), allocatable :: filename

    call gungho_initialise( filename )

    ! Set derived planet constants and presets
    call set_planet_constants()

  end subroutine initialise

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !>@brief Timesteps the model, calling the desired timestepping algorithm based
  !upon the configuration
  subroutine run()

    implicit none

    call gungho_run()

  end subroutine run

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !>@brief Tidies up after a run.
  subroutine finalise()

    implicit none

    call gungho_finalise()

  end subroutine finalise

end module lfric_atm_driver_mod
