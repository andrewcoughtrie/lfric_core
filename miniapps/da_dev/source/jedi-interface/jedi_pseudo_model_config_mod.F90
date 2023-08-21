!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing a configuration for the pseudo model emulator.
!>
!> @details A class is defined to hold the configuration data required to
!>          construct the pseudo model emulator. An initialiser is included and
!>          this is currently hard coded. In JEDI this information would be
!>          stored in a yaml file and eckit is used to parse and store a
!>          configuration object.
!>
module jedi_pseudo_model_config_mod

  use constants_mod,         only : i_def, str_def
  use lfric_da_datetime_mod, only : jedi_datetime_type

  implicit none

  private

type, public :: jedi_pseudo_model_config_type

  !> List of the dates to be read
  type( jedi_datetime_type ), allocatable :: datetime_states(:)

  !> File prefix for read
  character(len=str_def)             :: read_file_prefix

contains

  !> jedi_pseudo_model initialiser.
  procedure :: initialise

  !> Finalizer
  final     :: jedi_pseudo_model_config_destructor

end type jedi_pseudo_model_config_type

!------------------------------------------------------------------------------
! Contained functions/subroutines
!------------------------------------------------------------------------------
contains

!> @brief    Initialiser for jedi_pseudo_model_config_type
!>
subroutine initialise( self )

  implicit none

  class( jedi_pseudo_model_config_type ), intent(inout) :: self

  ! Local
  integer( kind=i_def )      :: i, datetime_entries
  type( jedi_datetime_type ) :: next_datetime

  call next_datetime%init_lfric_calendar_start()

  datetime_entries = 9
  allocate(self%datetime_states(datetime_entries))

  ! initialise datetime states 1-9 seconds after
  ! lfric calendar_start namelist variable time
  do i = 1, datetime_entries
    call next_datetime%add_seconds( 1 )
    self%datetime_states(i) = next_datetime
  end do

  self%read_file_prefix="read_"

end subroutine initialise

!> @brief    Finalizer for jedi_pseudo_model_config_type
!>
subroutine jedi_pseudo_model_config_destructor(self)!

  implicit none

  type(jedi_pseudo_model_config_type), intent(inout) :: self

  if ( allocated(self%datetime_states) ) deallocate(self%datetime_states)

end subroutine jedi_pseudo_model_config_destructor

end module jedi_pseudo_model_config_mod
