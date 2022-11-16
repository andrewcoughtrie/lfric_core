! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE lfricinp_setup_io_mod

USE constants_mod,                 ONLY: i_def, str_max_filename
USE driver_model_data_mod,         ONLY: model_data_type
USE file_mod,                      ONLY: FILE_MODE_READ,                       &
                                         FILE_MODE_WRITE
USE log_mod,                       ONLY: log_event, log_scratch_space,         &
                                         LOG_LEVEL_INFO, LOG_LEVEL_ERROR
USE lfric_xios_file_mod,           ONLY: lfric_xios_file_type
USE linked_list_mod,               ONLY: linked_list_type
USE time_config_mod,               ONLY: timestep_end
IMPLICIT NONE

INTEGER, PARAMETER              :: max_number_ancfiles = 20
CHARACTER(LEN=str_max_filename) :: checkpoint_read_file  = 'unset'
CHARACTER(LEN=str_max_filename) :: checkpoint_write_file = 'unset'
CHARACTER(LEN=str_max_filename) :: ancil_file_map(max_number_ancfiles) = 'unset'
LOGICAL :: checkpoint_write, checkpoint_read, ancil_read

PRIVATE
PUBLIC :: checkpoint_write_file, checkpoint_read_file, ancil_file_map,         &
          init_io_setup, init_lfricinp_files

CONTAINS

SUBROUTINE init_io_setup(io_nl)

USE lfricinp_unit_handler_mod, ONLY: get_free_unit

IMPLICIT NONE

CHARACTER(LEN=*), INTENT(IN) :: io_nl

CHARACTER(LEN=512) :: message = 'No namelist read'
INTEGER            :: unit_number
INTEGER            :: status = -1

NAMELIST /iofiles/ checkpoint_read,                                            &
                   checkpoint_write,                                           &
                   ancil_read,                                                 &
                   checkpoint_read_file,                                       &
                   checkpoint_write_file,                                      &
                   ancil_file_map

CALL get_free_unit(unit_number)

OPEN(UNIT=unit_number, FILE=io_nl, IOSTAT=status, IOMSG=message)
IF (status /= 0) CALL log_event(message, LOG_LEVEL_ERROR)

READ(unit_number, NML=iofiles, IOSTAT=status, IOMSG=message)
IF (status /= 0) CALL log_event(message, LOG_LEVEL_ERROR)

CLOSE(unit_number)

END SUBROUTINE init_io_setup


SUBROUTINE init_lfricinp_files(files_list, model_data)

IMPLICIT NONE

TYPE(linked_list_type),                   INTENT(OUT) :: files_list
CLASS(model_data_type), OPTIONAL, TARGET, INTENT(IN)  :: model_data

INTEGER, PARAMETER                     :: checkpoint_frequency = 1
CHARACTER(LEN=str_max_filename)        :: ancil_xios_file_id,                 &
                                          ancil_file_path,                    &
                                          afm
INTEGER                                :: split_idx, i

IF (ancil_read) THEN
  ! Set ancil file reading context information for all required ancil files
  DO i = 1, max_number_ancfiles
    ! Exit loop if entry in acil file map is unset
    afm = ancil_file_map(i)
    IF( TRIM(afm) == 'unset') EXIT
    ! From ancil file map string extract ancil xios file id and ancil file path
    split_idx = INDEX(afm, ':')
    ancil_xios_file_id = afm(1:split_idx-1)
    ancil_file_path = afm(split_idx+1:)
    ! Initial ancil file and insert file in file list
    CALL files_list%insert_item( lfric_xios_file_type(                          &
                                              TRIM(ancil_file_path),            &
                                              xios_id=TRIM(ancil_xios_file_id), &
                                              io_mode=FILE_MODE_READ) )
  END DO
END IF

! Setup checkpoint writing context information
IF (checkpoint_write) THEN
  ! Create checkpoint filename from stem and first timestep
  CALL files_list%insert_item( lfric_xios_file_type(                            &
                                              TRIM(checkpoint_write_file),      &
                                              xios_id="lfric_checkpoint_write", &
                                              io_mode=FILE_MODE_WRITE,          &
                                              freq=checkpoint_frequency ) )
END IF

! Setup checkpoint reading context information
IF (checkpoint_read) THEN
  ! Create checkpoint filename from stem
  CALL files_list%insert_item( lfric_xios_file_type(                            &
                                               TRIM(checkpoint_read_file),      &
                                               xios_id="lfric_checkpoint_read", &
                                               io_mode=FILE_MODE_READ ) )
END IF

END SUBROUTINE init_lfricinp_files

END MODULE lfricinp_setup_io_mod