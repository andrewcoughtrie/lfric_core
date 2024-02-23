!-----------------------------------------------------------------------------
! (c) Crown copyright 2024 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief A dummy method for use when running without evolving time-step

module no_timestep_alg_mod

  use log_mod,                            only: log_event,         &
                                                log_scratch_space, &
                                                LOG_LEVEL_WARNING
  use timestep_method_mod,       only: timestep_method_type
  use gungho_modeldb_mod,        only: modeldb_type

  implicit none

  type, extends(timestep_method_type), public :: no_timestep_type
    private

  contains
    private

    procedure, public  :: step
    procedure, public  :: finalise

  end type no_timestep_type

  ! Constructor for type
  interface no_timestep_type
    module procedure no_timestep_init
  end interface no_timestep_type

contains

  !> @brief Fulfils API without doing anything
  !> @param[in] modeldb Holds the model state
  function no_timestep_init(modeldb) result(self)

    implicit none

    type(no_timestep_type) :: self

    type(modeldb_type),     intent(in), target  :: modeldb

    write( log_scratch_space, &
                    '(A, A)' ) 'CAUTION: Running with no timestepping. ' // &
                    ' Prognostic fields not evolved'
    call log_event( log_scratch_space, LOG_LEVEL_WARNING )

  end function no_timestep_init

  !> @brief Fulfils API without doing anything
  !> @param[in] modeldb Holds the model state
  subroutine step( self, modeldb )

    implicit none

    class(no_timestep_type), intent(inout) :: self

    type(modeldb_type),              intent(in), target  :: modeldb
    write( log_scratch_space, &
                    '(A, A)' ) 'CAUTION: Running with no timestepping. ' // &
                    ' Prognostic fields not evolved'
    call log_event( log_scratch_space, LOG_LEVEL_WARNING )

  end subroutine step

  !> @brief Fulfils API without doing anything
  subroutine finalise( self )
    implicit none

    class(no_timestep_type), intent(inout) :: self

  end subroutine finalise


end module no_timestep_alg_mod
