!-----------------------------------------------------------------------------
! (c) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Drives the execution of the io_dev miniapp.
!>
!> This is a temporary solution until we have a proper driver layer.
!>
module io_dev_driver_mod

  ! Infrastructure
  use clock_mod,           only: clock_type
  use constants_mod,       only: i_def, i_native, imdi, &
                                 i_timestep
  use field_mod,           only: field_type
  use io_context_mod,      only: io_context_type
  use log_mod,             only: log_event,         &
                                 log_scratch_space, &
                                 LOG_LEVEL_ALWAYS,  &
                                 LOG_LEVEL_INFO
  use variable_fields_mod, only: update_variable_fields
  ! Configuration
  use io_config_mod,       only: use_xios_io, &
                                 write_diag,  &
                                 write_dump,  &
                                 diagnostic_frequency
  use io_dev_config_mod,   only: timestepping_on
  ! IO_Dev driver modules
  use io_dev_mod,          only: program_name
  use io_dev_data_mod,     only: io_dev_data_type,      &
                                 create_model_data,     &
                                 initialise_model_data, &
                                 output_model_data,     &
                                 finalise_model_data
  use io_dev_model_mod,    only: initialise_infrastructure, &
                                 finalise_infrastructure
  ! Algorithms
  use io_dev_timestep_alg_mod,    only: io_dev_timestep_alg

  implicit none

  private

  public initialise, run, finalise

  ! Model working data set
  type (io_dev_data_type) :: model_data
  ! Coordinate field
  type(field_type), target, dimension(3) :: chi_xyz
  type(field_type), target, dimension(3) :: chi_sph
  type(field_type), target               :: panel_id

  ! Mesh IDs
  integer(i_def) :: mesh_id
  integer(i_def) :: twod_mesh_id

  class(io_context_type), allocatable :: io_context

  contains

  !> @brief Sets up required state in preparation for run.
  !> @param[in] filename     Name of the file containing the desired model
  !>                         configuration
  !> @param[in] communicator The MPI communicator for use within the model
  subroutine initialise( filename, communicator )

    implicit none

    character(*),      intent(in) :: filename
    integer(i_native), intent(in) :: communicator

    ! Initialise infrastructure and setup constants
    call initialise_infrastructure( filename,     &
                                    program_name, &
                                    communicator, &
                                    mesh_id,      &
                                    twod_mesh_id, &
                                    chi_xyz,      &
                                    chi_sph,      &
                                    panel_id,     &
                                    io_context )

    ! Instantiate the fields stored in model_data
    call create_model_data( model_data,  &
                            mesh_id,     &
                            twod_mesh_id )

    ! Initialise the fields stored in the model_data
    call initialise_model_data( model_data, chi_xyz, io_context%get_clock() )


  end subroutine initialise

  !>@brief Timesteps the model, calling the desired timestepping algorithm based
  !>upon the configuration
  subroutine run()

    implicit none

    class(clock_type), pointer :: clock

    write(log_scratch_space,'(A)') 'Running '//program_name//' ...'
    call log_event( log_scratch_space, LOG_LEVEL_ALWAYS )

    clock => io_context%get_clock()

    ! Model step
    do while( clock%tick() )

      ! Update time-varying fields
      call update_variable_fields( model_data%variable_field_times, &
                                   clock, model_data%core_fields )

      if ( timestepping_on ) then
        call io_dev_timestep_alg( model_data%alg_fields, clock )
      end if

      ! Write out the fields
      if ( (mod( clock%get_step(), diagnostic_frequency ) == 0) ) then
        call log_event( program_name//': Writing XIOS output', LOG_LEVEL_INFO)
        call output_model_data( model_data )
      end if

    end do

  end subroutine run

  !> @brief Tidies up after a model run.
  subroutine finalise()

    implicit none

    call log_event( 'Finalising '//program_name//' ...', LOG_LEVEL_ALWAYS )

    ! Destroy the fields stored in model_data
    call finalise_model_data( model_data, io_context%get_clock() )

    ! Finalise infrastructure and constants
    call finalise_infrastructure( program_name )

  end subroutine finalise

end module io_dev_driver_mod
