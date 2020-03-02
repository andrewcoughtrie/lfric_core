!-------------------------------------------------------------------------------
!(c) Crown copyright 2020 Met Office. All rights reserved.
!The file LICENCE, distributed with this code, contains details of the terms
!under which the code may be used.
!-------------------------------------------------------------------------------

!>  @brief Module for field writing routines
!>  @details Holds all routines for writing LFRic fields
module write_methods_mod

  use constants_mod,                 only: i_def, dp_xios, &
                                           str_max_filename
  use field_mod,                     only: field_type, field_proxy_type
  use field_collection_mod,          only: field_collection_type, &
                                           field_collection_iterator_type
  use field_parent_mod,              only: field_parent_type
  use files_config_mod,              only: checkpoint_stem_name
  use fs_continuity_mod,             only: W3
  use io_mod,                        only: ts_fname
  use integer_field_mod,             only: integer_field_type
  use log_mod,                       only: log_event,         &
                                           log_scratch_space, &
                                           LOG_LEVEL_INFO
  use xios

  implicit none

  private
  public :: nodal_write_field,        &
            checkpoint_write_netcdf,  &
            checkpoint_write_xios,    &
            write_field_node,         &
            write_field_single_face,  &
            write_field_face,         &
            write_field_edge,         &
            write_state,              &
            write_checkpoint,         &
            dump_write_xios

contains

!> @brief   Output a field in nodal format to text file
!>@param[in] nodal_coordinates field holding coordinate information
!>@param[in] level field holding level information
!>@param[in] nodal_output field holding diagnostic data
!>@param[in] fspace_dimension dimension of the field's function space
!>@param[in] output_unit file unit to write to
!>@param[in] fname file name to write to
!-------------------------------------------------------------------------------
subroutine nodal_write_field(nodal_coordinates, level, nodal_output, &
                             fspace_dimension, output_unit, fname)

  implicit none

  type(field_type),            intent(in) :: nodal_coordinates(3)
  type(field_type),            intent(in) :: level
  type(field_type),            intent(in) :: nodal_output(3)
  integer(i_def),              intent(in) :: fspace_dimension
  integer(i_def),              intent(in) :: output_unit
  character(str_max_filename), intent(in) :: fname

  type(field_proxy_type) :: x_p(3), l_p, n_p(3)
  integer(i_def) :: df, undf, i

  do i = 1, 3
    x_p(i) = nodal_coordinates(i)%get_proxy()
    n_p(i) = nodal_output(i)%get_proxy()
  end do

  l_p = level%get_proxy()
  undf = n_p(1)%vspace%get_last_dof_owned()

  open(output_unit, file = trim(fname), status = "replace")
  write(output_unit,'(A)') 'x = ['

  if ( fspace_dimension  == 1 ) then
    do df = 1, undf
      write(output_unit,'(5e25.15e3)') x_p(1)%data(df), x_p(2)%data(df), &
                                       x_p(3)%data(df), l_p%data(df),    &
                                       n_p(1)%data(df)
    end do
  else
    do df = 1, undf
      write(output_unit,'(7e25.15e3)') x_p(1)%data(df), x_p(2)%data(df), &
                                       x_p(3)%data(df), l_p%data(df),    &
                                       n_p(1)%data(df), n_p(2)%data(df), &
                                       n_p(3)%data(df)
    end do
  end if

  write(output_unit,'(A)') '];'
  close(output_unit)

end subroutine nodal_write_field


!> @brief   I/O handler for writing a netcdf checkpoint
!> @details Legacy method for writing checkpoints
!           Note this routine accepts a field name but
!           doesn't use it - this is to keep the interface
!           the same for all methods
!>@param[in] field_name Name of the field to write
!>@param[in] file_name Name of the file to write to
!>@param[in,out] field_proxy the proxy of the field to write
subroutine checkpoint_write_netcdf(field_name, file_name, field_proxy)
  use field_io_ncdf_mod,    only : field_io_ncdf_type

  implicit none

  character(len=*),       intent(in) :: field_name
  character(len=*),       intent(in) :: file_name
  type(field_proxy_type), intent(in) :: field_proxy

  type(field_io_ncdf_type), allocatable :: ncdf_file

  allocate(ncdf_file)

  call ncdf_file%file_new( file_name )

  call ncdf_file%write_field_data ( field_proxy%data(:) )
  call ncdf_file%file_close()

  deallocate(ncdf_file)

end subroutine checkpoint_write_netcdf


!> @brief   I/O handler for writing an XIOS netcdf checkpoint
!> @details Note this routine accepts a filename but doesn't
!           use it - this is to keep the interface the same
!           for all methods
!>@param[in] xios_field_name XIOS identifier for the field
!>@param[in] file_name Name of the file to write
!>@param[in,out] field_proxy the proxy of the field to write
subroutine checkpoint_write_xios(xios_field_name, file_name, field_proxy)

  implicit none

  character(len=*),       intent(in) :: xios_field_name
  character(len=*),       intent(in) :: file_name
  type(field_proxy_type), intent(in) :: field_proxy

  integer(i_def) :: undf

  undf = field_proxy%vspace%get_last_dof_owned()

  call xios_send_field(xios_field_name, field_proxy%data(1:undf))

end subroutine checkpoint_write_xios


!> @brief   Output a field in UGRID format on the node domain via XIOS
!>@param[in] xios_field_name XIOS identifier for the field
!>@param[in] field_proxy a field proxy containing the data to output
!-------------------------------------------------------------------------------
subroutine write_field_node(xios_field_name, field_proxy)

  implicit none

  character(len=*),       intent(in) :: xios_field_name
  type(field_proxy_type), intent(in) :: field_proxy

  integer(i_def) :: i, undf
  integer(i_def) :: domain_size, axis_size
  real(dp_xios), allocatable :: send_field(:)

  undf = field_proxy%vspace%get_last_dof_owned()

  ! Get the expected horizontal domain size for the rank
  call xios_get_domain_attr('node', ni=domain_size)
  ! Get the expected vertical axis size
  call xios_get_axis_attr("vert_axis_full_levels", n_glo=axis_size)

  ! Size the arrays to be what is expected
  allocate(send_field(domain_size*axis_size))

  ! All data are scalar fields

  ! We need to reshape the raw field data to get the correct data layout for UGRID
  ! At the moment field array data is 1D with levels ordered sequentially
  ! This is only true for current scalar fields on lowest order fs and may change

  ! First get the data on the same level ordered in chunks
  do i = 0, axis_size-1
    send_field(i*(domain_size)+1:(i*(domain_size)) + domain_size) = &
               field_proxy%data(i+1:undf:axis_size)
  end do

  ! Reshape into 2D horizontal + vertical levels for output
  call xios_send_field(xios_field_name, &
                       reshape (send_field, (/domain_size, axis_size/) ))

  deallocate(send_field)

end subroutine write_field_node


!> @brief   Output a single level field in UGRID format on the face domain via XIOS
!>@param[in] xios_field_name XIOS identifier for the field
!>@param[in] field_proxy a field proxy containing the data to output
!-------------------------------------------------------------------------------
subroutine write_field_single_face(xios_field_name, field_proxy)

  implicit none

  character(len=*),       intent(in) :: xios_field_name
  type(field_proxy_type), intent(in) :: field_proxy

  integer(i_def) :: undf
  integer(i_def) :: domain_size
  real(dp_xios), allocatable :: send_field(:)

  undf = field_proxy%vspace%get_last_dof_owned()

  ! Get the expected horizontal size
  ! all 2D fields are nominally in W3, hence half levels
  call xios_get_domain_attr('face_half_levels', ni=domain_size)

  ! Size the arrays to be what is expected
  allocate(send_field(domain_size))

  ! All data are scalar fields
  send_field(1:domain_size) = field_proxy%data(1:undf)

  call xios_send_field(xios_field_name, send_field)

  deallocate(send_field)

end subroutine write_field_single_face

!> @brief   Output a field in UGRID format on the face domain via XIOS
!>@param[in] xios_field_name XIOS identifier for the field
!>@param[in] field_proxy a field proxy containing the data to output
!-------------------------------------------------------------------------------
subroutine write_field_face(xios_field_name, field_proxy)

  implicit none

  character(len=*),       intent(in) :: xios_field_name
  type(field_proxy_type), intent(in) :: field_proxy

  integer(i_def) :: i, undf
  integer(i_def) :: fs_id
  integer(i_def) :: domain_size, axis_size
  real(dp_xios), allocatable :: send_field(:)

  undf = field_proxy%vspace%get_last_dof_owned()
  fs_id = field_proxy%vspace%which()

  ! Get the expected horizontal and vertical axis size
  if ( fs_id == W3 ) then
    call xios_get_domain_attr('face_half_levels', ni=domain_size)
    call xios_get_axis_attr("vert_axis_half_levels", n_glo=axis_size)
  else
    call xios_get_domain_attr('face_full_levels', ni=domain_size)
    call xios_get_axis_attr("vert_axis_full_levels", n_glo=axis_size)
  end if

  ! Size the arrays to be what is expected
  allocate(send_field(domain_size*axis_size))

  ! All data are scalar fields

  ! We need to reshape the raw field data to get the correct data layout for UGRID
  ! At the moment field array data is 1D with levels ordered sequentially
  ! This is only true for current scalar fields on lowest order fs and may change

  ! First get the data on the same level ordered in chunks
  do i = 0, axis_size-1
    send_field(i*(domain_size)+1:(i*(domain_size)) + domain_size) = &
               field_proxy%data(i+1:undf:axis_size)
  end do

  ! Reshape into 2D horizontal + vertical levels for output
  call xios_send_field(xios_field_name, &
                       reshape (send_field, (/domain_size, axis_size/) ))

  deallocate(send_field)

end subroutine write_field_face


!> @brief   Output a field in UGRID format on the edge domain via XIOS
!>@param[in] xios_field_name XIOS identifier for the field
!>@param[in] field_proxy a field proxy containing the data to output
!-------------------------------------------------------------------------------
subroutine write_field_edge(xios_field_name, field_proxy)

  implicit none

  character(len=*),       intent(in) :: xios_field_name
  type(field_proxy_type), intent(in) :: field_proxy

  integer(i_def) :: i, undf
  integer(i_def) :: fs_id
  integer(i_def) :: domain_size, axis_size
  real(dp_xios), allocatable :: send_field(:)

  undf = field_proxy%vspace%get_last_dof_owned()
  fs_id = field_proxy%vspace%which()

  ! Get the expected horizontal and vertical axis size
  call xios_get_domain_attr('edge_half_levels', ni=domain_size)
  call xios_get_axis_attr("vert_axis_half_levels", n_glo=axis_size)

  ! Size the arrays to be what is expected
  allocate(send_field(domain_size*axis_size))

  ! All data are scalar fields

  ! We need to reshape the raw field data to get the correct data layout for UGRID
  ! At the moment field array data is 1D with levels ordered sequentially
  ! This is only true for current scalar fields on lowest order fs and may change

  ! First get the data on the same level ordered in chunks
  do i = 0, axis_size-1
    send_field(i*(domain_size)+1:(i*(domain_size)) + domain_size) = &
               field_proxy%data(i+1:undf:axis_size)
  end do

  ! Reshape into 2D horizontal + vertical levels for output
  call xios_send_field(xios_field_name, &
                       reshape (send_field, (/domain_size, axis_size/) ))

  deallocate(send_field)

end subroutine write_field_edge

!> @brief   Write a collection of fields
!> @details Iterate over a field collection and write each field
!>          if it is enabled for write
!>@param[in] state - a collection of fields
subroutine write_state(state)

  implicit none

  type(field_collection_type), intent(inout) :: state

  type(field_collection_iterator_type) :: iter

  class(field_parent_type), pointer :: fld => null()

  iter = state%get_iterator()
  do
    if ( .not.iter%has_next() ) exit
    fld => iter%next()
    select type(fld)
      type is (field_type)
        if ( fld%can_write() ) then
          write(log_scratch_space,'(3A,I6)') &
              "Writing ", trim(adjustl(fld%get_name()))
          call log_event(log_scratch_space,LOG_LEVEL_INFO)
          call fld%write_field(trim(adjustl(fld%get_name())))
        else

          call log_event( 'Write method for '// trim(adjustl(fld%get_name())) // &
                      ' not set up', LOG_LEVEL_INFO )

        end if
      type is (integer_field_type)
        ! todo: integer field i/o
    end select
  end do

  nullify(fld)

end subroutine write_state

!> @brief   Write a checkpoint from a collection of fields
!> @details Iterate over a field collection and checkpoint each field
!>          if it is enabled for checkpointing
!>@param[in] state - a collection of fields to checkpoint
!>@param[in] timestep the current timestep
subroutine write_checkpoint(state, timestep)

  implicit none

  type(field_collection_type), intent(inout) :: state
  integer(i_def),              intent(in)    :: timestep

  type(field_collection_iterator_type) :: iter

  class(field_parent_type), pointer :: fld => null()

  iter = state%get_iterator()
  do
    if ( .not.iter%has_next() ) exit
    fld => iter%next()
    select type(fld)
      type is (field_type)
        if ( fld%can_checkpoint() ) then
          write(log_scratch_space,'(3A,I6)') &
                "Checkpointing ", trim(adjustl(fld%get_name())), &
                " at timestep ", timestep
          call log_event(log_scratch_space, LOG_LEVEL_INFO)
          call fld%write_checkpoint( "checkpoint_"//trim(adjustl(fld%get_name())), &
                                     trim(ts_fname(checkpoint_stem_name, "",       &
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

end subroutine write_checkpoint

!> @brief   Write a field to a dump via XIOS
!>@param[in] xios_field_name XIOS identifier for the field
!>@param[in] field_proxy a field proxy containing the data to output
subroutine dump_write_xios(xios_field_name, field_proxy)

  implicit none

  character(len=*),       intent(in) :: xios_field_name
  type(field_proxy_type), intent(in) :: field_proxy

  call write_field_face(xios_field_name, field_proxy)

end subroutine dump_write_xios

end module write_methods_mod
