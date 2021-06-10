!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Controls infrastructure related information used by the model

module multires_coupling_infrastructure_mod

  use configuration_mod,     only : final_configuration
  use constants_mod,         only : i_def, i_native, PRECISION_REAL
  use multires_coupling_mod, only : load_configuration
  use convert_to_upper_mod,  only : convert_to_upper
  use derived_config_mod,    only : set_derived_config
  use log_mod,               only : log_event,          &
                                    log_set_level,      &
                                    log_scratch_space,  &
                                    initialise_logging, &
                                    finalise_logging,   &
                                    LOG_LEVEL_ALWAYS,   &
                                    LOG_LEVEL_ERROR,    &
                                    LOG_LEVEL_WARNING,  &
                                    LOG_LEVEL_INFO,     &
                                    LOG_LEVEL_DEBUG,    &
                                    LOG_LEVEL_TRACE
  use mpi_mod,               only : store_comm, &
                                    get_comm_size, get_comm_rank
  use yaxt,                  only : xt_initialize, xt_finalize

  implicit none

  private
  public initialise_infrastructure, finalise_infrastructure

contains

  !> @brief Initialises the infrastructure used by the model
  !> @param [inout] model_communicator The MPI communicator for use within the model
  !>                     (not XIOS' communicator)
  !> @param [in] filename The name of the configuration namelist file
  !> @param [in] program_name An identifier given to the model begin run
  subroutine initialise_infrastructure( model_communicator, &
                                        filename,           &
                                        program_name )

    use logging_config_mod, only: run_log_level,          &
                                  key_from_run_log_level, &
                                  RUN_LOG_LEVEL_ERROR,    &
                                  RUN_LOG_LEVEL_INFO,     &
                                  RUN_LOG_LEVEL_DEBUG,    &
                                  RUN_LOG_LEVEL_TRACE,    &
                                  RUN_LOG_LEVEL_WARNING
    implicit none

    integer(i_native), intent(in) :: model_communicator
    character(*),      intent(in) :: filename
    character(*),      intent(in) :: program_name

    integer(i_def) :: total_ranks, local_rank

    integer(i_native) :: log_level

    ! Initialise YAXT
    call xt_initialize( model_communicator )

    !Store the MPI communicator for later use
    call store_comm( model_communicator )

    ! Get the rank information
    total_ranks = get_comm_size()
    local_rank  = get_comm_rank()

    call initialise_logging( local_rank, total_ranks, program_name )

    call load_configuration( filename )

    select case (run_log_level)
    case( RUN_LOG_LEVEL_ERROR )
      log_level = LOG_LEVEL_ERROR
    case( RUN_LOG_LEVEL_WARNING )
      log_level = LOG_LEVEL_WARNING
    case( RUN_LOG_LEVEL_INFO )
      log_level = LOG_LEVEL_INFO
    case( RUN_LOG_LEVEL_DEBUG )
      log_level = LOG_LEVEL_DEBUG
    case( RUN_LOG_LEVEL_TRACE )
      log_level = LOG_LEVEL_TRACE
    end select

    call log_set_level( log_level )

    write(log_scratch_space,'(A)')                              &
        'Runtime message logging severity set to log level: '// &
        convert_to_upper(key_from_run_log_level(run_log_level))
    call log_event( log_scratch_space, LOG_LEVEL_ALWAYS )

    write(log_scratch_space,'(A)')                        &
        'Application built with '//trim(PRECISION_REAL)// &
        '-bit real numbers'
    call log_event( log_scratch_space, LOG_LEVEL_ALWAYS )

    call set_derived_config( .true. )

  end subroutine initialise_infrastructure


  !> @brief Finalises infrastructure used by the model
  !> @param [in] program_name An identifier given to the model
  subroutine finalise_infrastructure( program_name )

    implicit none

    character(*),      intent(in) :: program_name

    ! Finalise namelist configurations
    call final_configuration()

    ! Finalise YAXT
    call xt_finalize()

    call log_event( program_name//' completed.', LOG_LEVEL_ALWAYS )

    ! Finalise the logging system
    call finalise_logging()

  end subroutine finalise_infrastructure

end module multires_coupling_infrastructure_mod
