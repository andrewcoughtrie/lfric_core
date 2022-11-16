!-------------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-------------------------------------------------------------------------------

!> @brief  Module to hold routines for processing XIOS output for compliance
!>         with downstream data requirements
!>
module lfric_xios_process_output_mod

  use constants_mod,            only: i_native
  use file_mod,                 only: FILE_MODE_WRITE,     &
                                      FILE_OP_OPEN
  use io_config_mod,            only: file_convention,       &
                                      file_convention_ugrid, &
                                      file_convention_cf
  use lfric_ncdf_field_mod,     only: lfric_ncdf_field_type
  use lfric_ncdf_file_mod,      only: lfric_ncdf_file_type
  use lfric_xios_constants_mod, only: dp_xios
  use log_mod,                  only: log_event, log_level_trace
  use mpi_mod,                  only: get_comm_rank

  implicit none

  public :: process_output_file, set_xios_geometry_planar
  private

  ! Public scaling factor for planar mesh coordinates to circumvent XIOS issue
  real(kind=dp_xios), public, parameter :: xyz_scaling_factor = 1.0e-4

  logical :: model_has_planar_geometry = .false.

contains

!> @brief Processes a NetCDF file produced by XIOS to align with the LFRic
!!        UGRID file format
!>
!> @param[in] file_path  The path to the NetCDF file to be edited
subroutine process_output_file(file_path)

  implicit none

  character(len=*), intent(in) :: file_path

  type(lfric_ncdf_file_type) :: file_ncdf
  logical                    :: file_exists

  ! Output processing must be done in serial
  if (get_comm_rank() /= 0) return

  call log_event("Processing output file: "//trim(file_path), log_level_trace)

  ! If file has not been written out, then don't attempt to process it
  inquire(file=trim(file_path), exist=file_exists)
  if (.not. file_exists) return

  ! Open output file
  file_ncdf = lfric_ncdf_file_type( trim(file_path),           &
                                    open_mode=FILE_OP_OPEN, &
                                    io_mode=FILE_MODE_WRITE )

  call format_version(file_ncdf)

  if (file_convention == file_convention_ugrid) then
    call format_mesh(file_ncdf)
  end if

  call file_ncdf%close_file()

end subroutine process_output_file

!> @brief Tags output file with the current version number of the LFRic file
!!        format
!>
!> @param[in] file_ncdf  The netcdf file to be edited
subroutine format_version(file_ncdf)

  implicit none

  type(lfric_ncdf_file_type), intent(inout) :: file_ncdf

  call file_ncdf%set_attribute("description", "LFRic file format v0.1.1")

  select case(file_convention)
  case (file_convention_ugrid)
    call file_ncdf%set_attribute("Conventions", "UGRID-1.0")

  case (file_convention_cf)
    call file_ncdf%set_attribute("Conventions", "CF")

  end select

end subroutine format_version

!> @brief Formats the mesh object in the output file
!>
!> @param[in] file_ncdf  The netcdf file to be edited
subroutine format_mesh(file_ncdf)

  implicit none

  type(lfric_ncdf_file_type), intent(inout) :: file_ncdf

  type(lfric_ncdf_field_type) :: mesh_var

  if (.not. file_ncdf%contains_var("Mesh2d")) return

  mesh_var = lfric_ncdf_field_type("Mesh2d", file_ncdf)

  if (model_has_planar_geometry) then
    call mesh_var%set_char_attribute("geometry", "planar")
    call fix_planar_coordinates(file_ncdf)
  else
    call mesh_var%set_char_attribute("geometry", "spherical")
  end if

end subroutine format_mesh

!> @brief Fixes issues with planar coordinates in output file
!>
!> @param[in] file_ncdf  The netcdf file to be edited
subroutine fix_planar_coordinates(file_ncdf)

  implicit none

  type(lfric_ncdf_file_type), intent(inout) :: file_ncdf

  type(lfric_ncdf_field_type) :: coord_field
  character(len=1)            :: dim
  character(len=13)           :: coord_field_names(6) = [ "Mesh2d_node_x", &
                                                          "Mesh2d_node_y", &
                                                          "Mesh2d_edge_x", &
                                                          "Mesh2d_edge_y", &
                                                          "Mesh2d_face_x", &
                                                          "Mesh2d_face_y"  ]
  integer(i_native)           :: i

  ! Modify coordinate fields to correctly represent output from model
  do i = 1, size(coord_field_names)
    ! Trim the last character from the dim name to get the dimension
    dim = coord_field_names(i)(len(coord_field_names(i)):len(coord_field_names(i)))

    coord_field = lfric_ncdf_field_type(trim(coord_field_names(i)), file_ncdf)
    call coord_field%set_char_attribute("standard_name", "projection_"//trim(dim)//"_coordinate")
    call coord_field%set_char_attribute("long_name", trim(dim)//" coordinate of projection")
    call coord_field%set_char_attribute("units","m")
    call coord_field%set_real_attribute("scale_factor", xyz_scaling_factor**(-1.0_dp_xios))
  end do

end subroutine fix_planar_coordinates

!> @brief Specifies that the model is running on a mesh with planar geometry
subroutine set_xios_geometry_planar()

  implicit none

  model_has_planar_geometry = .true.

end subroutine set_xios_geometry_planar

end module lfric_xios_process_output_mod