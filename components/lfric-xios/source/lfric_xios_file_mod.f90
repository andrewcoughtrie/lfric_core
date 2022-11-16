!-----------------------------------------------------------------------------
! (C) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!>  @brief Module containing a file type for XIOS interface
!>
module lfric_xios_file_mod

  use constants_mod,                 only: i_native, str_def, str_max_filename
  use field_mod,                     only: field_type
  use field_parent_mod,              only: field_parent_type
  use field_collection_mod,          only: field_collection_type
  use field_collection_iterator_mod, only: field_collection_iterator_type
  use file_mod,                      only: file_type, FILE_MODE_READ, FILE_MODE_WRITE
  use lfric_xios_process_output_mod, only: process_output_file
  use lfric_xios_field_mod,          only: lfric_xios_field_type
  use log_mod,                       only: log_event, log_level_error, &
                                           log_level_trace, log_level_debug
  use mesh_mod,                      only: mesh_type
  use mod_wait,                      only: init_wait
  use xios,                          only: xios_file, xios_is_valid_file,    &
                                           xios_add_child, xios_get_handle,  &
                                           xios_set_attr, xios_filegroup,    &
                                           xios_fieldgroup, xios_duration,   &
                                           xios_date, xios_get_current_date, &
                                           xios_get_start_date,              &
                                           xios_get_timestep, operator(+),   &
                                           operator(*), operator(<=)

  implicit none

private

!> @brief Container for file properties need by XIOS
!>
type, public, extends(file_type) :: lfric_xios_file_type
  private

  !> Due to the pattern of initialising an object within LFRic and then later
  !! using the information in that object to initialise the corresponding object
  !! within the XIOS context we need to keep two versions of the same
  !! information present.
  !>
  !> Model (pre-XIOS) representations
  !> The ID of the XIOS file
  character(str_def)          :: xios_id
  !> The path to the file
  character(str_max_filename) :: path
  !> The I/O mode of open file (read or write)
  integer(i_native)           :: io_mode
  !> How the contents of the file evolve with time
  integer(i_native)           :: operation
  !> The file frequency in timesteps
  integer(i_native)           :: freq_ts = -999
  !> @todo field_group is slated for removal, it is a leftover placeholder
  !!       needed to make checkpointing work for lfric_atm/gungho, but once
  !!       they are upgraded to use the "fields_in_file" API this can be removed.
  character(str_def)          :: field_group = "unset"
  !> The XIOS ID of the field group contained within the file
  character(str_def)          :: field_group_id
  !> Flag denoting if the file has been closed
  logical :: is_closed = .false.

  !> XIOS representations
  !> Internal XIOS representation of the file
  type(xios_file)             :: handle
  !> The file frequency as an XIOS duration
  type(xios_duration)         :: frequency
  !> The date of the next operation to be performed on the file by XIOS.
  type(xios_date)             :: next_operation
  !> An array of lfric_xios_field_type objects attached to this file
  type(lfric_xios_field_type), allocatable :: fields(:)

contains
  procedure, public :: file_new
  procedure, public :: file_open
  procedure, public :: file_close
  procedure, public :: register_with_context
  procedure, public :: mode_is_read
  procedure, public :: mode_is_write
  procedure, public :: recv_fields
  procedure, public :: send_fields
  final             :: lfric_xios_file_final

end type lfric_xios_file_type

interface lfric_xios_file_type
  module procedure lfric_xios_file_constructor
end interface

integer(i_native), public, parameter :: OPERATION_ONCE       = 1938
integer(i_native), public, parameter :: OPERATION_TIMESERIES = 3406

contains

!> Constructs an LFRic-XIOS file type object
!>
!> @param[in] file_name      The name/path of the file
!> @param[in] xios_id        The XIOS ID of the file
!> @param[in] io_mode        Enum denoting whether file is input or output
!> @param[in] freq           The frequency in timesteps that the file is
!!                           read-from/written-to
!> @param[in] operation      Enum denoting the kind of I/O done by the file
!> @param[in] field_group_id XIOS ID of the field group contained in the file
!> @param[in] fields_in_file Array of fields contained in the file
function lfric_xios_file_constructor( file_name, xios_id, io_mode, freq, &
                                      operation, field_group_id,         &
                                      fields_in_file ) result(self)

  implicit none

  type(lfric_xios_file_type)  :: self
  character(len=*),            intent(in) :: file_name
  character(len=*),            intent(in) :: xios_id
  integer(i_native),           intent(in) :: io_mode
  integer(i_native), optional, intent(in) :: freq
  integer(i_native), optional, intent(in) :: operation
  character(len=*),  optional, intent(in) :: field_group_id
  type(field_collection_type), &
                     optional, intent(in) :: fields_in_file

  type(field_collection_iterator_type) :: iter
  class(field_parent_type), pointer    :: fld => null()

  integer(i_native) :: field_index

  self%path = file_name

  self%xios_id = xios_id

  self%io_mode = io_mode

  if (present(operation)) self%operation = operation

  if (present(freq)) then
    if (freq < 1) then
      call log_event( "XIOS files cannot have negative frequency", &
                      log_level_error )
    end if
    self%freq_ts = freq
  end if

  if (present(field_group_id)) self%field_group = field_group_id

  ! Set up XIOS fields representing attached field collection
  if (present(fields_in_file)) then
    if (.not. present(operation)) then
      call log_event( "XIOS file containing model fields must have a defined operation", &
                      log_level_error )
    end if
    allocate(self%fields(fields_in_file%get_length()))
    self%field_group_id = trim(self%xios_id)//"_fields"
    call iter%initialise(fields_in_file)
    do field_index = 1, fields_in_file%get_length()
      fld => iter%next()
      self%fields(field_index) = lfric_xios_field_type(fld, fieldgroup_id=self%field_group_id)
    end do
  end if

  return

end function lfric_xios_file_constructor

!> @brief  Initialises an LFRic-XIOS file object
!> @param[in] file_name Path to the file
subroutine file_new(self, file_name)

  implicit none

  class(lfric_xios_file_type), intent(inout) :: self
  character(len=*),            intent(in)    :: file_name

  self%path = file_name

end subroutine file_new

!> @brief  Opens the LFRic definition of an output file after it has been
!!         opened by XIOS
!> @param[in] file_name Path to the file
subroutine file_open(self, file_name)

  implicit none

  class(lfric_xios_file_type), intent(inout) :: self
  character(len=*),            intent(in)    :: file_name

  ! TODO Write orography

end subroutine file_open

!> @brief Performs final routines before closure of XIOS file
subroutine file_close(self)

  implicit none

  class(lfric_xios_file_type), intent(inout) :: self

  if (self%is_closed) return

  if ( self%io_mode == FILE_MODE_WRITE ) then
    call log_event( "Waiting for XIOS to close file ["//trim(self%path)//".nc]", &
                    log_level_debug )
    call init_wait()
    call process_output_file(trim(self%path)//".nc")
  end if

  self%is_closed = .true.

end subroutine file_close

!> @brief Registers a file with an XIOS context
subroutine register_with_context(self)

  implicit none

  class(lfric_xios_file_type),  intent(inout) :: self

  type(xios_filegroup)   :: file_definition
  type(xios_fieldgroup)  :: field_group_hdl, file_fields
  type(xios_duration)    :: timestep_duration
  type(xios_date)        :: start_date

  integer(i_native) :: i

  call log_event( "Registering XIOS file ["//trim(self%xios_id)//"]", &
                  log_level_trace )

  ! Register or get handle of file from XIOS
  if (xios_is_valid_file(trim(self%xios_id))) then
    call xios_get_handle( trim(self%xios_id), self%handle )
  else
    call xios_get_handle("file_definition", file_definition)
    call xios_add_child(file_definition, self%handle, trim(self%xios_id))
  end if

  ! Set file path
  call xios_set_attr( self%handle, name=trim(adjustl(self%path)) )

  ! Set I/O mode (no need for case defalt as we will fall back to XIOS's
  ! default behaviour)
  select case(self%io_mode)
  case (FILE_MODE_READ)
    call xios_set_attr( self%handle, mode="read" )
  case (FILE_MODE_WRITE)
    call xios_set_attr( self%handle, mode="write" )
    ! Create CF-compliant time description
    call xios_set_attr( self%handle, time_counter="exclusive", &
                                     time_counter_name="time" )
  end select

  ! Set XIOS duration object second value equal to file output frequency
  call xios_get_timestep(timestep_duration)
  if (.not. self%freq_ts == -999) then
    self%frequency = self%freq_ts * timestep_duration
  else
    self%frequency = timestep_duration
  end if

  call xios_set_attr( self%handle, output_freq=self%frequency )

  call xios_get_start_date(start_date)
  self%next_operation = start_date + self%frequency

  ! Set up fields in file
  if (allocated(self%fields)) then
    call xios_add_child(self%handle, file_fields, self%field_group_id)

    ! Set the temporal operation for fields in the file
    select case(self%operation)
    case(OPERATION_ONCE)
      call xios_set_attr(file_fields, operation="once")
      if (self%io_mode == FILE_MODE_READ) then
        call xios_set_attr(file_fields, freq_offset=timestep_duration)
      end if
    case(OPERATION_TIMESERIES)
      call xios_set_attr(file_fields, operation="instant")
    end select

    ! Iterate over field collection and register fields
    do i = 1, size(self%fields)
      call self%fields(i)%register()
    end do

    ! Enable field collection
    call xios_set_attr(file_fields, enabled=.true. )

  end if

  ! LEGACY
  ! If there is an associated field group, enable it
  if ( .not. trim(self%field_group) == "unset" ) then
    call xios_get_handle( trim(self%field_group), field_group_hdl )
    call xios_set_attr( field_group_hdl, enabled=.true. )
  end if

  ! Enable file
  call xios_set_attr( self%handle, enabled=.true. )

end subroutine register_with_context

!> Returns true if the file is in "read" mode
function mode_is_read(self) result(file_mode_is_read)

  implicit none

  class(lfric_xios_file_type), intent(inout) :: self
  logical :: file_mode_is_read

  file_mode_is_read = (self%io_mode == FILE_MODE_READ)

end function mode_is_read

!> Returns true if the file is in "write" mode
function mode_is_write(self) result(file_mode_is_write)

  implicit none

  class(lfric_xios_file_type), intent(inout) :: self
  logical :: file_mode_is_write

  file_mode_is_write = (self%io_mode == FILE_MODE_WRITE)

end function mode_is_write

!> @brief Reads all registered field entries in a file
subroutine recv_fields(self)

  implicit none

  class(lfric_xios_file_type),  intent(inout) :: self

  type(xios_date)   :: model_date
  integer(i_native) :: i

  ! Don't do any operations if file is closed
  if (self%is_closed) return

  ! Don't do any operations if there are no field descriptions in the file
  if (.not. allocated(self%fields)) return

  ! Don't try to write if file is in read mode
  if (self%io_mode /= FILE_MODE_READ) then
    call log_event( "Can't perform read operations on file ["//     &
                    trim(self%xios_id)//"]: file not in read mode", &
                    log_level_error)
  end if

  call xios_get_current_date(model_date)
  if (self%next_operation <= model_date) then

    ! Iterate over field collection and read fields
    do i = 1, size(self%fields)
      call self%fields(i)%recv()
    end do

    ! If file should only be operated on once, close it, else set the time for
    ! the next operation
    if (self%operation == OPERATION_ONCE) then
      call self%file_close()
    else
      self%next_operation = self%next_operation + self%frequency
    end if

  end if

end subroutine recv_fields

!> @brief Writes all fields registered with a file
subroutine send_fields(self)

  implicit none

  class(lfric_xios_file_type),  intent(inout) :: self

  type(xios_date)   :: model_date
  integer(i_native) :: i

  ! Do not do any operations if file is closed
  if (self%is_closed) return

  ! Do not do any operations if there are no field descriptions in the file
  if (.not. allocated(self%fields)) return

  ! Do not try to write if file is not in write mode
  if (self%io_mode /= FILE_MODE_WRITE) then
    call log_event( "Cannot perform write operations on file ["//     &
                    trim(self%xios_id)//"]: file not in write mode", &
                    log_level_error)
  end if

  call xios_get_current_date(model_date)
  if (self%next_operation <= model_date) then

    call log_event( "Writing fields to file ["//trim(self%xios_id)//"]", &
                    log_level_trace)

    ! Iterate over field collection and write fields
    do i = 1, size(self%fields)
      call self%fields(i)%send()
    end do

    ! If file should only be operated on once, close it, else set the time for
    ! the next operation
    if (self%operation == OPERATION_ONCE) then
      call self%file_close()
    else
      self%next_operation = self%next_operation + self%frequency
    end if

  end if

end subroutine send_fields

!> Finaliser for lfric_xios_file_type object
subroutine lfric_xios_file_final(self)

  implicit none

  type(lfric_xios_file_type), intent(inout) :: self

  if (allocated(self%fields)) deallocate(self%fields)

end subroutine lfric_xios_file_final

end module lfric_xios_file_mod
