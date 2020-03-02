!-------------------------------------------------------------------------------
!(c) Crown copyright 2020 Met Office. All rights reserved.
!The file LICENCE, distributed with this code, contains details of the terms
!under which the code may be used.
!-------------------------------------------------------------------------------

!>  @brief Module for field reading routines
!>  @details Holds all routines for reading LFRic fields
module read_methods_mod

  use constants_mod,                 only: i_def, dp_xios
  use field_mod,                     only: field_type, field_proxy_type
  use field_collection_mod,          only: field_collection_type, &
                                           field_collection_iterator_type
  use field_parent_mod,              only: field_parent_type
  use files_config_mod,              only: checkpoint_stem_name
  use fs_continuity_mod,             only: W3
  use integer_field_mod,             only: integer_field_type
  use io_mod,                        only: ts_fname
  use log_mod,                       only: log_event,         &
                                           log_scratch_space, &
                                           LOG_LEVEL_INFO
  use xios

  implicit none

  private
  public :: checkpoint_read_netcdf,  &
            checkpoint_read_xios,    &
            read_field_face,         &
            read_field_single_face,  &
            read_state,              &
            read_checkpoint,         &
            dump_read_xios

contains

!> @brief   I/O handler for reading a netcdf checkpoint
!> @details Legacy method for reading checkpoints
!           Note this routine accepts a field name but
!           doesn't use it - this is to keep the interface
!           the same for all methods
!>@param[in] field_name Name of the field to read
!>@param[in] file_name Name of the file to read from
!>@param[in,out] field_proxy the proxy of the field to read data into
subroutine checkpoint_read_netcdf(field_name, file_name, field_proxy)
  use field_io_ncdf_mod,    only : field_io_ncdf_type

  implicit none

  character(len=*),       intent(in)    :: field_name
  character(len=*),       intent(in)    :: file_name
  type(field_proxy_type), intent(inout) :: field_proxy

  type(field_io_ncdf_type), allocatable :: ncdf_file

  allocate(ncdf_file)

  call ncdf_file%file_open( file_name )
  call ncdf_file%read_field_data ( field_proxy%data(:) )
  call ncdf_file%file_close()

  deallocate(ncdf_file)

end subroutine checkpoint_read_netcdf

!> @brief   I/O handler for reading an XIOS netcdf checkpoint
!> @details Note this routine accepts a filename but doesn't
!           use it - this is to keep the interface the same
!           for all methods
!>@param[in] xios_field_name XIOS unique id for the field
!>@param[in] file_name Name of the file to read
!>@param[in,out] field_proxy the proxy of the field to read into
subroutine checkpoint_read_xios(xios_field_name, file_name, field_proxy)

  implicit none

  character(len=*),       intent(in)    :: xios_field_name
  character(len=*),       intent(in)    :: file_name
  type(field_proxy_type), intent(inout) :: field_proxy

  integer(i_def) :: undf

  ! We only read in up to undf for the partition
  undf = field_proxy%vspace%get_last_dof_owned()

  call xios_recv_field(xios_field_name, field_proxy%data(1:undf))

end subroutine checkpoint_read_xios

!> @brief   Read a field in UGRID format on the face domain via XIOS
!>@param[in] xios_field_name XIOS identifier for the field
!>@param[in] field_proxy a field proxy to read data into
subroutine read_field_face(xios_field_name, field_proxy)

  implicit none

  character(len=*),       intent(in)    :: xios_field_name
  type(field_proxy_type), intent(inout) :: field_proxy

  integer(i_def) :: i, undf
  integer(i_def) :: fs_id
  integer(i_def) :: domain_size, axis_size
  real(dp_xios), allocatable :: recv_field(:)

  ! Get the size of undf as we only read in up to last owned
  undf = field_proxy%vspace%get_last_dof_owned()
  fs_id = field_proxy%vspace%which()

  ! get the horizontal / vertical domain sizes
  if ( fs_id == W3 ) then
    call xios_get_domain_attr('face_half_levels', ni=domain_size)
    call xios_get_axis_attr("vert_axis_half_levels", n_glo=axis_size)
  else
    call xios_get_domain_attr('face_full_levels', ni=domain_size)
    call xios_get_axis_attr("vert_axis_full_levels", n_glo=axis_size)
  end if

  ! Size the array to be what is expected
  allocate(recv_field(domain_size*axis_size))

  ! Read the data into a temporary array - this should be in the correct order
  ! as long as we set up the horizontal domain using the global index
  call xios_recv_field(xios_field_name, recv_field)

  ! We need to reshape the incoming data to get the correct data layout for the LFRic
  ! field - It is the reverse of what we did for writing
  do i = 0, axis_size-1
    field_proxy%data(i+1:undf:axis_size) = &
                     recv_field(i*(domain_size)+1:(i*(domain_size)) + domain_size)
  end do

  deallocate(recv_field)

end subroutine read_field_face

!> @brief   Read a single level field in UGRID format on the face domain via XIOS
!>@param[in] xios_field_name XIOS identifier for the field
!>@param[in] field_proxy a field proxy to read data into
subroutine read_field_single_face(xios_field_name, field_proxy)

  implicit none

  character(len=*),       intent(in)    :: xios_field_name
  type(field_proxy_type), intent(inout) :: field_proxy

  integer(i_def) :: i, undf
  integer(i_def) :: domain_size
  real(dp_xios), allocatable :: recv_field(:)

  ! Get the size of undf as we only read in up to last owned
  undf = field_proxy%vspace%get_last_dof_owned()

  ! Get the expected horizontal size
  ! all 2D fields are nominally in W3, hence half levels
  call xios_get_domain_attr('face_half_levels', ni=domain_size)

  ! Size the array to be what is expected
  allocate(recv_field(domain_size))

  ! Read the data into a temporary array - this should be in the correct order
  ! as long as we set up the horizontal domain using the global index
  call xios_recv_field(xios_field_name, recv_field)

  field_proxy%data(1:undf) = recv_field(1:domain_size)

  deallocate(recv_field)

end subroutine read_field_single_face

!> @brief   Read into a collection of fields
!> @details Iterate over a field collection and read each field
!>          into a collection, if it is enabled for read
!>@param[in,out] state -  the collection of fields to populate
subroutine read_state(state)

  implicit none

  type( field_collection_type ), intent(inout) :: state

  type( field_collection_iterator_type) :: iter

  class( field_parent_type ), pointer :: fld => null()

  iter = state%get_iterator()
  do
    if ( .not.iter%has_next() ) exit
    fld => iter%next()
    select type(fld)
      type is (field_type)
        if ( fld%can_read() ) then
          call log_event( &
            'Reading '//trim(adjustl(fld%get_name())), &
            LOG_LEVEL_INFO)
          call fld%read_field(trim(adjustl(fld%get_name())))
        else
          call log_event( 'Read method for  '// trim(adjustl(fld%get_name())) // &
                          ' not set up', LOG_LEVEL_INFO )
        end if
      type is (integer_field_type)
        ! todo: integer field i/o
    end select
  end do

  nullify(fld)

end subroutine read_state

!> @brief   Read from a checkpoint into a collection of fields
!> @details Iterate over a field collection and read each field
!>          into a collection, if it is enabled for checkpointing
!>@param[in] state -  the collection of fields to populate
!>@param[in] timestep the current timestep
subroutine read_checkpoint(state, timestep)

  implicit none

  type( field_collection_type ), intent(inout) :: state
  integer(i_def),                intent(in)    :: timestep

  type( field_collection_iterator_type) :: iter

  class( field_parent_type ), pointer :: fld => null()

  iter = state%get_iterator()
  do
    if ( .not.iter%has_next() ) exit
    fld => iter%next()
    select type(fld)
      type is (field_type)
        if ( fld%can_checkpoint() ) then
          call log_event( 'Reading checkpoint file to restart '// &
                           trim(adjustl(fld%get_name())), LOG_LEVEL_INFO)
          call fld%read_checkpoint( "restart_"//trim(adjustl(fld%get_name())), &
                                    trim(ts_fname(checkpoint_stem_name, "",    &
                                    trim(adjustl(fld%get_name())),timestep,"")) )
        else
          call log_event( 'Checkpointing for  '// trim(adjustl(fld%get_name())) // &
                          ' not set up', LOG_LEVEL_INFO )
        end if
      type is (integer_field_type)
        ! todo: integer field i/o
    end select
  end do

  nullify(fld)

end subroutine read_checkpoint

!> @brief   Read a field from a dump via XIOS
!>@param[in] xios_field_name XIOS identifier for the field
!>@param[in,out] field_proxy a field proxy containing the data to output
subroutine dump_read_xios(xios_field_name, field_proxy)

  implicit none

  character(len=*),       intent(in)    :: xios_field_name
  type(field_proxy_type), intent(inout) :: field_proxy

  call read_field_face("read_"//xios_field_name, field_proxy)

end subroutine dump_read_xios

end module read_methods_mod
