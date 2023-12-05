!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Container for the working data set of the IO_Dev model run, including
!> methods to initialise and finalise the data set
!>
!> This module provides a type to hold all the model fields and methods to
!> initialise (create and read) and finalise (write and destroy) the
!> data contained within the type.
!>
module io_dev_data_mod

  ! Infrastructure
  use constants_mod,                    only : i_def
  use io_dev_modeldb_mod,               only : modeldb_type
  use field_mod,                        only : field_type
  use field_collection_mod,             only : field_collection_type
  use log_mod,                          only : log_event,      &
                                               LOG_LEVEL_INFO, &
                                               LOG_LEVEL_ERROR
  use mesh_mod,                         only : mesh_type
  use timer_mod,                        only : timer
  use variable_fields_mod,              only : init_variable_fields, &
                                               update_variable_fields
  ! Configuration
  use files_config_mod,                 only : checkpoint_stem_name
  use io_config_mod,                    only : write_diag, write_dump, &
                                               checkpoint_read,        &
                                               checkpoint_write,       &
                                               subroutine_timers
  use io_dev_config_mod,                only : field_initialisation,            &
                                               field_initialisation_start_dump, &
                                               time_variation,                  &
                                               time_variation_analytic,         &
                                               time_variation_ancil,            &
                                               time_variation_none
  ! I/O methods
  use lfric_xios_read_mod,              only : read_state, read_checkpoint
  use lfric_xios_write_mod,             only : write_state, write_checkpoint
  ! IO_Dev modules
  use io_dev_init_mod,                  only : setup_io_dev_fields
  use io_dev_init_fields_alg_mod,       only : io_dev_init_fields_alg
  use io_dev_checksum_alg_mod,          only : io_dev_checksum_alg
  use io_dev_timestep_alg_mod,          only : io_dev_timestep_alg

  implicit none

  private

  public create_model_data, initialise_model_data, &
         update_model_data, finalise_model_data, output_model_data

contains

  !> @brief Create the fields contained in model_data
  !> @param[in,out] modeldb The database holding the model state.
  !> @param[in]     chi          A size 3 array of fields holding the mesh
  !>                             coordinates
  !> @param[in]     panel_id     A field with the IDs of mesh panels
  !> @param[in]     mesh         The current 3d mesh
  !> @param[in]     twod_mesh    The current 2d mesh
  !> @param[in]     alt_mesh     An alternative I/O mesh
  !>
  subroutine create_model_data( modeldb, chi, panel_id, &
                                mesh, twod_mesh, alt_mesh )

    implicit none

    type( modeldb_type ),                 intent(inout) :: modeldb
    type( field_type ),                   intent(in)    :: chi(3)
    type( field_type ),                   intent(in)    :: panel_id
    type( mesh_type ), pointer,           intent(in)    :: mesh
    type( mesh_type ), pointer,           intent(in)    :: twod_mesh
    type( mesh_type ), pointer, optional, intent(in)    :: alt_mesh

    ! Assign the field_collection to a handle
    ! Stores all the fields used by the model - the remaining collections store
    ! pointers to the data in the depository.
    type( field_collection_type ), pointer              :: depository
    ! Field collection holding fields for dumps
    type( field_collection_type ), pointer              :: dump_fields
    ! Field collection holding fields which can be processed by PSyClone
    type( field_collection_type ), pointer              :: alg_fields
    depository  => modeldb%fields%get_field_collection("depository")
    dump_fields => modeldb%fields%get_field_collection("dump_fields")
    alg_fields  => modeldb%fields%get_field_collection("alg_fields")

    ! Create model data fields
    call setup_io_dev_fields( mesh,                           &
                              twod_mesh,                      &
                              depository,                    &
                              dump_fields,                    &
                              alg_fields,                     &
                              modeldb%variable_field_times,   &
                              alt_mesh )

    ! Initialise data before I/O is called
    call io_dev_init_fields_alg( depository, chi, panel_id )

  end subroutine create_model_data

  !> @brief Initialises the working data set dependent of namelist configuration
  !> @param[in,out] modeldb The database holding the model state.
  !> @param[in]     chi         A size 3 array of fields holding the mesh
  !>                            coordinates
  !> @param[in]     panel_id   A field with the IDs of mesh panels
  !>
  subroutine initialise_model_data( modeldb, chi, panel_id )

    implicit none

    type( modeldb_type ), intent(inout) :: modeldb
    type( field_type ),   intent(in)    :: chi(3)
    type( field_type ),   intent(in)    :: panel_id

    type( field_collection_type ), pointer :: depository
    depository => modeldb%fields%get_field_collection("depository")

    ! Time varying init
    if (time_variation == time_variation_ancil) then
      call log_event( "IO_Dev: Initialising fields from time_varying ancillary", LOG_LEVEL_INFO )
      if ( subroutine_timers ) call timer('init_variable_fields')
      call init_variable_fields( modeldb%variable_field_times, &
                                   modeldb%clock, depository )
      if ( subroutine_timers ) call timer('init_variable_fields')
    end if

  end subroutine initialise_model_data

  !> @brief Updates the working data set dependent of namelist configuration
  !> @param[in,out] modeldb The database holding the model state.
  subroutine update_model_data( modeldb )

    implicit none

    type( modeldb_type ), intent(inout) :: modeldb

    type( field_collection_type ), pointer :: depository
    type( field_collection_type ), pointer :: alg_fields
    depository => modeldb%fields%get_field_collection("depository")
    alg_fields => modeldb%fields%get_field_collection("alg_fields")

    !---------------------------------------------------------------
    ! Separate update calls are made based on model configuration
    !---------------------------------------------------------------
    select case ( time_variation )

    case ( time_variation_analytic )
      call log_event( "IO_Dev: Updating fields analytically", LOG_LEVEL_INFO )
      if (alg_fields%get_length() /= 0) then
        call io_dev_timestep_alg( alg_fields, modeldb%clock )
      end if

    case ( time_variation_ancil )
      call log_event( "IO_Dev: Updating fields from time_varying ancillary", LOG_LEVEL_INFO )
      if ( subroutine_timers ) call timer('update_variable_fields')
      call update_variable_fields( modeldb%variable_field_times, &
                                   modeldb%clock, depository )
      if ( subroutine_timers ) call timer('update_variable_fields')

    case ( time_variation_none )
      call log_event( "IO_Dev: No time variation for this run", LOG_LEVEL_INFO )

    case default
      call log_event( "IO_Dev: Invalid choice for time-variation namelist", LOG_LEVEL_ERROR )

    end select

  end subroutine update_model_data


  !> @brief Writes out a checkpoint and dump file dependent on namelist options
  !> @param[in,out] modeldb The database holding the model state.
  subroutine output_model_data( modeldb )

    implicit none

    type( modeldb_type ), intent(inout), target :: modeldb

    type( field_collection_type ), pointer :: depository
    type( field_collection_type ), pointer :: alg_fields
    depository => modeldb%fields%get_field_collection("depository")
    alg_fields => modeldb%fields%get_field_collection("alg_fields")

    !===================== Write initial output ======================!
    if ( modeldb%clock%is_initialisation() ) then
      if ( subroutine_timers ) call timer('write_state: initial')
        if (alg_fields%get_length() /= 0) then
          call write_state( alg_fields, prefix='initial_' )
        end if
      if ( subroutine_timers ) call timer('write_state: initial')
    end if

    !=================== Write fields to diagnostic files ====================!
    if ( write_diag ) then
      if ( subroutine_timers ) call timer('write_state: diagnostic')
      call write_state( depository )
      if ( subroutine_timers ) call timer('write_state: diagnostic')
    end if

  end subroutine output_model_data

  !> @brief Routine to destroy all the field collections in the working data set
  !> @param[in,out] modeldb The database holding the model state.
  subroutine finalise_model_data( modeldb )

    implicit none

      type(modeldb_type),  intent(inout) :: modeldb

      type( field_collection_type ), pointer :: depository
      type( field_collection_type ), pointer :: dump_fields
      type( field_collection_type ), pointer :: alg_fields
      depository  => modeldb%fields%get_field_collection("depository")
      dump_fields => modeldb%fields%get_field_collection("dump_fields")
      alg_fields  => modeldb%fields%get_field_collection("alg_fields")

      !======================== Write checksum output ==========================
      if (alg_fields%get_length() /= 0) then
        call io_dev_checksum_alg( alg_fields )
      end if

      ! Clear all the fields in each field collection
      call depository%clear()
      call dump_fields%clear()

      call log_event( 'finalise_model_data: all fields have been cleared', &
                       LOG_LEVEL_INFO )

  end subroutine finalise_model_data

end module io_dev_data_mod
