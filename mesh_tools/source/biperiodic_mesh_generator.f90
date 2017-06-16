!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! Generate a biperiodic mesh and write it to a UGRID format file.
!
! Usage:
!     cubedsphere_mesh_generator <filename>
!
!     filename - Controlling namelist file
!
program biperiodic_mesh_generator

  use biperiodic_mesh_generator_config_mod,                                  &
                      only : read_biperiodic_mesh_generator_namelist,        &
                             postprocess_biperiodic_mesh_generator_namelist, &
                             mesh_name, cells_in_x, cells_in_y,              &
                             cell_width, cell_height,                        &
                             mesh_filename
  use cli_mod,           only : get_initial_filename
  use constants_mod,     only : i_def
  use ESMF
  use genbiperiodic_mod, only : genbiperiodic_type
  use io_utility_mod,    only : open_file, close_file
  use log_mod,           only : log_scratch_space, log_event, &
                                LOG_LEVEL_INFO, LOG_LEVEL_ERROR
  use ncdf_quad_mod,     only : ncdf_quad_type
  use ugrid_2d_mod,      only : ugrid_2d_type
  use ugrid_file_mod,    only : ugrid_file_type

  implicit none

  type(ESMF_VM)  :: vm
  integer(i_def) :: rc

  character(:), allocatable :: filename
  integer(i_def)            :: namelist_unit

  type(genbiperiodic_type)            :: bpgen
  type(ugrid_2d_type)                 :: ugrid_2d
  class(ugrid_file_type), allocatable :: ugrid_file
  integer(i_def)                      :: fsize

  call ESMF_Initialize(vm=vm, defaultlogfilename="biperiodic.log", &
                       logkindflag=ESMF_LOGKIND_SINGLE, rc=rc)
  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to initialise ESMF.', &
                                          LOG_LEVEL_ERROR )

  call get_initial_filename( filename )
  namelist_unit = open_file( filename )
  call read_biperiodic_mesh_generator_namelist( namelist_unit, vm, 0 )
  call postprocess_biperiodic_mesh_generator_namelist( )
  call close_file( namelist_unit )
  deallocate( filename )

  allocate(ncdf_quad_type::ugrid_file)
  call ugrid_2d%set_file_handler(ugrid_file)

  bpgen = genbiperiodic_type( mesh_name,              &
                              cells_in_x, cells_in_y, &
                              cell_width, cell_height )

  call log_event( "Generating biperiodic mesh with...", LOG_LEVEL_INFO )
  write(log_scratch_space, "(A,I0)") "  cells_in_x: ", cells_in_x
  call log_event( log_scratch_space, LOG_LEVEL_INFO )
  write(log_scratch_space, "(A,I0)") "  cells_in_y: ", cells_in_y
  call log_event( log_scratch_space, LOG_LEVEL_INFO )
  write(log_scratch_space, "(A,F6.1)") "  cell_width: ", cell_width
  call log_event( log_scratch_space, LOG_LEVEL_INFO )
  write(log_scratch_space, "(A,F6.1)") "  cell_height: ", cell_height
  call log_event( log_scratch_space, LOG_LEVEL_INFO )

  call ugrid_2d%set_by_generator( bpgen )
  call ugrid_2d%write_to_file( trim(mesh_filename) )

  call log_event( "...generation complete.", LOG_LEVEL_INFO )
  inquire(file=mesh_filename, size=fsize)
  write( log_scratch_space, '(A,I0,A)' )                       &
      'Writing ugrid mesh to '//trim(adjustl(mesh_filename))// &
      ' - ', fsize, ' bytes written.'
  call log_event( log_scratch_space, LOG_LEVEL_INFO )

  call ESMF_Finalize(rc=rc)

end program biperiodic_mesh_generator
