!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!>  @brief   File handler for NetCDF field files.
!!
!!  @details Implementation of the field_io_strategy class for NetCDF format.
!-------------------------------------------------------------------------------
module field_io_ncdf_mod

use constants_mod, only: r_def, str_max_filename, str_long
use netcdf, only: nf90_max_name, nf90_open, nf90_write, nf90_noerr,        &
                  nf90_strerror, nf90_put_var, nf90_get_var, nf90_put_att, &      
                  nf90_def_var, nf90_inq_varid, nf90_int, nf90_double,     &
                  nf90_clobber, nf90_enddef, nf90_inquire_dimension,       &
                  nf90_inq_dimid, nf90_def_dim, nf90_create, nf90_close,   &
                  nf90_64bit_offset
implicit none
private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief    NetCDF quad file type
!!
!! @details  Implements the ugrid file type for NetCDF files storing 2D quads.  
!-------------------------------------------------------------------------------

type, public :: field_io_ncdf_type
  private

  !Dimension lengths
  integer :: field_size                    !< Length of field array

  integer                     :: ncid      !< NetCDF file ID
  character(str_max_filename) :: file_name !< Filename

  !Variable ids
  integer :: field_dim_id    !< NetCDF-assigned ID for the field dimensions.
  integer :: field_data_id   !< NetCDF-assigned ID for the field_data

contains
  procedure :: get_dimensions
  procedure :: read_field_data
  procedure :: write_field_data
  procedure :: file_open
  procedure :: file_close
  procedure :: file_new
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!>  @brief   Open an existing NetCDF file.  
!!
!!  @param[in,out]  self         The NetCDF file object.
!!  @param[in]      file_name    Name of the file to open.
!-------------------------------------------------------------------------------

subroutine file_open(self, file_name)
  implicit none

  !Arguments
  class(field_io_ncdf_type), intent(inout) :: self
  character(len=*),          intent(in)    :: file_name

  !Internal variables
  integer :: ierr
  character(*), parameter :: routine = 'file_open'
  character(str_long) :: cmess

  self%file_name = file_name

  cmess = 'Opening file, "'//trim(self%file_name)//'"'
  ierr = nf90_open( trim(self%file_name), nf90_write, self%ncid )
  call check_err(ierr, routine, cmess)

  !Set up the variable ids
  call inquire_ids(self)

  return
end subroutine file_open

!-------------------------------------------------------------------------------
!>  @brief   Closes a NetCDF file.
!!
!!  @param[in]  self   The NetCDF file object.
!-------------------------------------------------------------------------------

subroutine file_close(self)
  implicit none

  !Arguments
  class(field_io_ncdf_type), intent(inout) :: self

  !Internal variables
  integer :: ierr
  character(*), parameter :: routine = 'file_close'
  character(str_long) :: cmess  = ''

  cmess = 'Closing file, "'//trim(self%file_name)//'"'
  ierr = nf90_close( self%ncid )
  call check_err(ierr, routine, cmess)

  return
end subroutine file_close

!-------------------------------------------------------------------------------
!>  @brief          Create a new NetCDF file.
!!
!!  @description    Creates an opens a new, clean NetCDF file. If a file of the
!!                  same name already exists, this routine will clobber it.
!!
!!  @param[in,out]  self      The NetCDF file object.
!!  @param[in]      file_name The name of the file to create/open.
!-------------------------------------------------------------------------------

subroutine file_new(self, file_name)
  implicit none

  !Arguments
  class(field_io_ncdf_type), intent(inout) :: self
  character(len=*),          intent(in)    :: file_name

  !Internal variables
  integer :: ierr
  character(*), parameter :: routine = 'file_new'
  character(str_long) :: cmess   = ''

  self%file_name = file_name

  ! Create the NetCDF file with 64-bit offsets to support large file sizes
  cmess = 'Creating file, "'//trim(self%file_name)//'"'
  ierr = nf90_create( path=trim(self%file_name),                 &
                      cmode=ior(nf90_clobber,nf90_64bit_offset), &
                      ncid=self%ncid )

  call check_err(ierr, routine, cmess)

  return
end subroutine file_new

!-------------------------------------------------------------------------------
!>  @brief   Defines NetCDF dimensions in the NetCDF file.
!!
!!  @details Sets dimension lengths in the NetCDF file, and sets the associated
!!           dimension ids in the NetCDF file object. The dimension lengths are
!!           used for sizes of other arrays within the NetCDF file.
!!
!!  @param[in,out]  self   The NetCDF file object.
!-------------------------------------------------------------------------------

subroutine define_dimensions(self)
  implicit none

  !Arguments
  class(field_io_ncdf_type), intent(inout) :: self

  !Internal variables
  integer :: ierr
  character(*), parameter :: routine = 'define_dimensions'
  character(str_long) :: cmess   = ''

  ! define dimensions
  cmess = 'Defining dimension, "field_size"'
  ierr = nf90_def_dim(self%ncid, 'field_size',  self%field_size,     &
                                                self%field_dim_id)
  call check_err(ierr, routine, cmess)

  return
end subroutine define_dimensions

!-------------------------------------------------------------------------------
!>  @brief    Defines NetCDF variables in the NetCDF file.
!!
!!  @details  Tells NetCDF what variables are going to be in the file.
!!            Array lengths are specified via the pre-existing NetCDF dimension
!!            IDs, which were obtained elsewhere in this module.
!!
!!  @param[in,out]  self   The NetCDF file object.
!-------------------------------------------------------------------------------

subroutine define_variables(self)
  implicit none

  !Arguments
  class(field_io_ncdf_type), intent(inout) :: self

  !Internal variables
  integer :: ierr
  character(*), parameter :: routine = 'define_variables'
  character(str_long) :: cmess   = ''

  cmess = 'Defining variable, "field_data"'
  ierr = nf90_def_var(self%ncid, 'field_data', nf90_double,     &
                      [self%field_dim_id], self%field_data_id)

  call check_err(ierr, routine, cmess)

  return
end subroutine define_variables

!-------------------------------------------------------------------------------
!>  @brief    Assigns attributes to the NetCDF variables.
!!
!!  @details  Adds additional information to NetCDF variables that should have
!!            already been defined elsewhere in this module.  Attributes include
!!            variable names and descriptions.
!!
!!  @param[in]   self   The NetCDF file object.
!-------------------------------------------------------------------------------

subroutine assign_attributes(self)
  implicit none

  !Arguments
  class(field_io_ncdf_type), intent(in) :: self

  !Internal variables
  integer :: ierr
  character(*), parameter :: routine = 'assign_attributes'
  character(str_long) :: cmess

  cmess = 'Asssigning attirbute, "field_data"'
  ierr = nf90_put_att(self%ncid, self%field_data_id,       &
                      'field_data', 'Field data array.')
  call check_err(ierr, routine, cmess)

  return
end subroutine assign_attributes

!-------------------------------------------------------------------------------
!>  @brief    Gets dimension ids and variable ids from the open NetCDF file.
!!
!!  @details  NetCDF files refer to dimensions and variables by an id, the value
!!            of which is determined by the NetCDF library. This routine finds
!!            dimension and variable ids for all variables of interest in the
!!            open NetCDF file.
!!
!!  @param[in,out]   self   The NetCDF file object.
!-------------------------------------------------------------------------------

subroutine inquire_ids(self)
  implicit none

  !Arguments
  type(field_io_ncdf_type), intent(inout) :: self

  !Internal variables
  integer :: ierr
  character(*), parameter :: routine = 'inquire_ids'
  character(str_long) :: cmess

  !Field size
  cmess = 'Getting variable-id for, "field_size"'
  ierr = nf90_inq_dimid(self%ncid, 'field_size', self%field_dim_id)
  call check_err(ierr, routine, cmess)
  cmess = 'Getting variable-id for, "field_data"'
  ierr = nf90_inq_varid(self%ncid, 'field_data', self%field_data_id)
  call check_err(ierr, routine, cmess)

  return
end subroutine inquire_ids

!-------------------------------------------------------------------------------
!>  @brief    Calls logger on error.
!!
!!  @details  Checks the error code returned by the NetCDF file. If an error is
!!            detected, the relevant error message is passed to the logger.
!!
!!  @param[in] ierr    The error code to check.
!!  @param[in] routine The routine name that call the error check
!!  @param[in] cmess   Comment message for the error report
!-------------------------------------------------------------------------------

subroutine check_err(ierr, routine, cmess)
  use log_mod, only: log_event, log_scratch_space, LOG_LEVEL_ERROR
  implicit none

  !Arguments
  integer,             intent(in) :: ierr
  character(*),        intent(in) :: routine
  character(str_long), intent(in) :: cmess

  if (ierr /= NF90_NOERR) then
    write(log_scratch_space,*) 'Error in field_io_ncdf ['//routine//']: '//&
      trim(cmess) // ' ' // trim(nf90_strerror(ierr))
    call log_event( trim(log_scratch_space), LOG_LEVEL_ERROR ) 
  end if 

  return
end subroutine check_err

!-------------------------------------------------------------------------------
!>  @brief    Gets dimension information from the NetCDF file, as integers.
!!
!!  @details  Calls NetCDF inquiry functions to determine array lengths, such as
!!            the number of nodes.
!!
!!  @param[in,out]   self           The NetCDF file object.
!!  @param[out]      field_size     The size of the field data array.
!-------------------------------------------------------------------------------

subroutine get_dimensions(self, field_size)
  implicit none

  !Arguments
  class(field_io_ncdf_type),  intent(inout) :: self
  integer,                    intent(out)   :: field_size

  integer :: ierr
  character(*), parameter :: routine = 'get_dimenstions'
  character(str_long) :: cmess

  !Get dimension lengths
  cmess = 'Getting dimensions for, "field_size"'
  ierr = nf90_inquire_dimension(self%ncid, self%field_dim_id,   &
                                       len=self%field_size)
  call check_err(ierr, routine, cmess)

  !Set output values.
  field_size = self%field_size

  return
end subroutine get_dimensions

!-------------------------------------------------------------------------------
!>  @brief    Reads data from the NetCDF file.
!!
!!  @details  Reads coordinate and connectivity information from the NetCDF file.
!!
!!  @param[in,out]  self                     The NetCDF file object.
!!  @param[out]     field_data               Field data read from the file.
!-------------------------------------------------------------------------------

subroutine read_field_data(self, field_data)
  implicit none

  !Arguments
  class(field_io_ncdf_type), intent(in)     :: self                        
  real(kind=r_def),          intent(out)    :: field_data(:)       

  !Internal variables
  integer :: ierr
  character(*), parameter :: routine = 'read_field_data'
  character(str_long) :: cmess

  !Field data itself
  cmess = 'Reading field data'
  ierr = nf90_get_var(self%ncid, self%field_data_id, field_data(:))
  call check_err(ierr, routine, cmess)

  return
end subroutine read_field_data

!-------------------------------------------------------------------------------
!>  @brief    Writes data to the NetCDF file.
!!
!!  @details  Writes dimension, coordinate and connectivity information
!!            to the NetCDF file.
!!
!!  @param[in,out]  self                     The NetCDF file object.
!!  @param[in]      field_data               The field data to write.
!-------------------------------------------------------------------------------

subroutine write_field_data(self, field_data)
  implicit none

  !Arguments
  class(field_io_ncdf_type), intent(inout)  :: self                        
  real(kind=r_def),          intent(in)     :: field_data(:)                   

  !Internal variables
  integer :: ierr
  character(*), parameter :: routine = 'write_field_data'
  character(str_long) :: cmess

  !Set array lengths
  self%field_size = size(field_data)

  !Set up NetCDF header
  call define_dimensions (self)
  call define_variables  (self)
  call assign_attributes (self)

  !End definitions before putting data in.
  cmess = 'Closing definitions'
  ierr = nf90_enddef(self%ncid)
  call check_err(ierr, routine, cmess)

  ! Write field data
  cmess = 'Writing field data'
  ierr = nf90_put_var(self%ncid, self%field_data_id, field_data(:))
  call check_err(ierr, routine, cmess)

  return
end subroutine write_field_data

end module field_io_ncdf_mod

