!-----------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-----------------------------------------------------------------------------
!
! Load a UGRID format file and print some summary information.
!
! Usage:
!     summarise_ugrid <filename>
!
!     filename - UGRID file
!
program summarise_ugrid

  use cli_mod,         only : get_initial_filename
  use constants_mod,   only : i_def
  use iso_fortran_env, only : output_unit
  use ncdf_quad_mod,   only : ncdf_quad_type
  use ugrid_2d_mod,    only : ugrid_2d_type
  use ugrid_file_mod,  only : ugrid_file_type

  implicit none

  character(:), allocatable :: filename

  class(ugrid_file_type), allocatable :: ugrid_file
  type(ugrid_2d_type)                    :: infile

  integer(i_def) :: nodes, edges, faces
  integer(i_def) :: nodes_per_face, edges_per_face
  integer(i_def) :: nodes_per_edge, max_faces_per_node

  call get_initial_filename( filename, 'UGRID mesh file' )

  allocate(ncdf_quad_type::ugrid_file)

  call infile%set_file_handler(ugrid_file)
  call infile%read_from_file(trim(adjustl(filename)))

  call infile%get_dimensions(nodes, edges, faces, nodes_per_face, &
                            edges_per_face, nodes_per_edge, max_faces_per_node)

  write(output_unit,                                               &
        '("File ", A, " contains a ugrid mesh with dimensions:")') &
       trim(adjustl(filename))

  write(output_unit, "(A,19X,I0)") " Nodes: ", nodes
  write(output_unit, "(A,19X,I0)") " Edges: ", edges
  write(output_unit, "(A,19X,I0)") " Faces: ", faces
  write(output_unit, "(A,10X,I0)") " Nodes per face: ", nodes_per_face
  write(output_unit, "(A,10X,I0)") " Edges per face: ", edges_per_face
  write(output_unit, "(A,10X,I0)") " Nodes per edge: ", nodes_per_edge
  write(output_unit, "(A,2X,I0)") " Maximum faces per node: ", max_faces_per_node

end program summarise_ugrid
