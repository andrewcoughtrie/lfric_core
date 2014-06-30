!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Topology of a unit reference element
! includes ordering of topological entities and lookups for dof's
! Currently only includes a cube, but could be extended for triangles etc
!-------------------------------------------------------------------------------

module reference_element_mod

use constants_mod, only: dp

implicit none

! Incidence relationships
integer, allocatable :: vert_on_face(:,:), vert_on_edge(:,:), &
                        edge_on_face(:,:), edge_on_vert(:,:), &
                        face_on_edge(:,:)
! Vertex coodinates
real(kind=dp), allocatable :: x_vert(:,:)
! vector directions
real(kind=dp), allocatable :: normal_to_face(:,:), tangent_to_edge(:,:)

! Geometric information about the reference element
integer :: nverts, nfaces, nedges
integer :: nverts_h, nfaces_h, nedges_h

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains 

subroutine reference_cube()
  !-----------------------------------------------------------------------------
  ! Subroutine that defines topology of a reference unit cube 
  !-----------------------------------------------------------------------------
  implicit none
  
! 2D cell information
  nverts_h = 4 
  nfaces_h = 4  
  nedges_h = 4
  
! vertical extrusion  
  nverts = 2*nverts_h
  nfaces = nfaces_h + 2
  nedges = 3*nedges_h
  
  ! Allocate arrays
  allocate ( vert_on_face(nfaces,4) )
  allocate ( vert_on_edge(nedges,2) )
  allocate ( edge_on_face(nfaces,4) )
  allocate ( edge_on_vert(nverts,3) )
  allocate ( face_on_edge(nedges,2) )
  allocate ( x_vert(nverts,3) )
  allocate ( normal_to_face(nfaces,3))
  allocate ( tangent_to_edge(nedges,3) )

  x_vert(1,:) = (/ 0.0_dp, 0.0_dp, 0.0_dp /)
  x_vert(2,:) = (/ 1.0_dp, 0.0_dp, 0.0_dp /)
  x_vert(3,:) = (/ 1.0_dp, 1.0_dp, 0.0_dp /)
  x_vert(4,:) = (/ 0.0_dp, 1.0_dp, 0.0_dp /)
  x_vert(5,:) = (/ 0.0_dp, 0.0_dp, 1.0_dp /)
  x_vert(6,:) = (/ 1.0_dp, 0.0_dp, 1.0_dp /)
  x_vert(7,:) = (/ 1.0_dp, 1.0_dp, 1.0_dp /)
  x_vert(8,:) = (/ 0.0_dp, 1.0_dp, 1.0_dp /)

! vertices on each face - anticlockwise ordering
  vert_on_face(1,:) = (/ 1, 2, 6, 5 /)
  vert_on_face(2,:) = (/ 2, 3, 7, 6 /)
  vert_on_face(3,:) = (/ 3, 4, 8, 7 /)
  vert_on_face(4,:) = (/ 1, 4, 8, 5 /)
  vert_on_face(5,:) = (/ 1, 2, 3, 4 /)
  vert_on_face(6,:) = (/ 5, 6, 7, 8 /)

! Vertices at the end of each edge
  vert_on_edge(1 ,:) = (/ 1, 2 /)
  vert_on_edge(2 ,:) = (/ 2, 3 /)
  vert_on_edge(3 ,:) = (/ 3, 4 /)
  vert_on_edge(4 ,:) = (/ 4, 1 /)
  vert_on_edge(5 ,:) = (/ 1, 5 /)
  vert_on_edge(6 ,:) = (/ 2, 6 /)
  vert_on_edge(7 ,:) = (/ 3, 7 /)
  vert_on_edge(8 ,:) = (/ 4, 8 /)
  vert_on_edge(9 ,:) = (/ 5, 6 /)
  vert_on_edge(10,:) = (/ 6, 7 /)
  vert_on_edge(11,:) = (/ 7, 8 /)
  vert_on_edge(12,:) = (/ 8, 5 /)

! Edges on each face
  edge_on_face(1,:) = (/ 1, 6 , 9 , 5  /)
  edge_on_face(2,:) = (/ 2, 7 , 10, 6  /)
  edge_on_face(3,:) = (/ 3, 7 , 11, 8  /)
  edge_on_face(4,:) = (/ 4, 8 , 12, 5  /)
  edge_on_face(5,:) = (/ 1, 2 , 3 , 4  /)
  edge_on_face(6,:) = (/ 9, 10, 11, 12 /)

! Edges on each vertex
  edge_on_vert(1,:) = (/ 1,  4,  5 /)
  edge_on_vert(2,:) = (/ 1,  2,  6 /)
  edge_on_vert(3,:) = (/ 2,  3,  7 /)
  edge_on_vert(4,:) = (/ 3,  4,  8 /)
  edge_on_vert(5,:) = (/ 5,  9, 12 /)
  edge_on_vert(6,:) = (/ 6,  9, 10 /)
  edge_on_vert(7,:) = (/ 7, 10, 11 /)
  edge_on_vert(8,:) = (/ 8, 11, 12 /)

! Faces either side of each edge
  face_on_edge(1 ,:) = (/ 1, 5 /)
  face_on_edge(2 ,:) = (/ 2, 5 /)
  face_on_edge(3 ,:) = (/ 3, 5 /)
  face_on_edge(4 ,:) = (/ 4, 5 /)
  face_on_edge(5 ,:) = (/ 4, 1 /)
  face_on_edge(6 ,:) = (/ 1, 2 /)
  face_on_edge(7 ,:) = (/ 2, 3 /)
  face_on_edge(8 ,:) = (/ 3, 4 /)
  face_on_edge(9 ,:) = (/ 1, 6 /)
  face_on_edge(10,:) = (/ 2, 6 /)
  face_on_edge(11,:) = (/ 3, 6 /)
  face_on_edge(12,:) = (/ 4, 6 /)
  
! outward unit normal vector to each face  
  normal_to_face(1,:) = (/  0.0_dp,  1.0_dp,  0.0_dp /)
  normal_to_face(2,:) = (/  1.0_dp,  0.0_dp,  0.0_dp /)
  normal_to_face(3,:) = (/  0.0_dp,  1.0_dp,  0.0_dp /)
  normal_to_face(4,:) = (/  1.0_dp,  0.0_dp,  0.0_dp /)
  normal_to_face(5,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  normal_to_face(6,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  
! tangent vectors to each edge 
! convention is that vector points from vert_on_edge(i,1) > vert_on_edge(i,2)
  tangent_to_edge(1 ,:) = (/  1.0_dp,  0.0_dp,  0.0_dp /)
  tangent_to_edge(2 ,:) = (/  0.0_dp,  1.0_dp,  0.0_dp /)
  tangent_to_edge(3 ,:) = (/  1.0_dp,  0.0_dp,  0.0_dp /)
  tangent_to_edge(4 ,:) = (/  0.0_dp,  1.0_dp,  0.0_dp /)
  tangent_to_edge(5 ,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  tangent_to_edge(6 ,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  tangent_to_edge(7 ,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  tangent_to_edge(8 ,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  tangent_to_edge(9 ,:) = (/  1.0_dp,  0.0_dp,  0.0_dp /)
  tangent_to_edge(10,:) = (/  0.0_dp,  1.0_dp,  0.0_dp /)
  tangent_to_edge(11,:) = (/  1.0_dp,  0.0_dp,  0.0_dp /)
  tangent_to_edge(12,:) = (/  0.0_dp,  1.0_dp,  0.0_dp /)
  
end subroutine reference_cube

subroutine reference_triangle()
  !-----------------------------------------------------------------------------
  ! Subroutine that defines topology of a reference unit triangle
  !-----------------------------------------------------------------------------
  implicit none
  
  real(kind=dp), parameter :: rt3ov2 = sqrt(3.0_dp)/2.0_dp
  
! 2D cell information
  nverts_h = 3 
  nfaces_h = 3  
  nedges_h = 3
  
! vertical extrusion  
  nverts = 2*nverts_h
  nfaces = nfaces_h + 2
  nedges = 3*nedges_h
  
  ! Allocate arrays
  allocate ( vert_on_face(nfaces,4) )
  allocate ( vert_on_edge(nedges,2) )
  allocate ( edge_on_face(nfaces,4) )
  allocate ( edge_on_vert(nverts,3) )
  allocate ( face_on_edge(nedges,2) )
  allocate ( x_vert(nverts,3) )
  allocate ( normal_to_face(nfaces,3))
  allocate ( tangent_to_edge(nedges,3) )

  x_vert(1,:) = (/ 0.0_dp, 0.0_dp, 0.0_dp /)
  x_vert(2,:) = (/ 1.0_dp, 0.0_dp, 0.0_dp /)
  x_vert(3,:) = (/ 0.5_dp, rt3ov2, 0.0_dp /)
  x_vert(4,:) = (/ 0.0_dp, 0.0_dp, 1.0_dp /)
  x_vert(5,:) = (/ 1.0_dp, 0.0_dp, 1.0_dp /)
  x_vert(6,:) = (/ 0.5_dp, rt3ov2, 1.0_dp /)

! vertices on each face - anticlockwise ordering
  vert_on_face(1,:) = (/ 1, 2, 5, 4/)
  vert_on_face(2,:) = (/ 2, 3, 6, 4 /)
  vert_on_face(3,:) = (/ 3, 6, 4, 1 /)
  vert_on_face(4,:) = (/ 1, 2, 3, 0 /)
  vert_on_face(5,:) = (/ 4, 5, 6, 0 /)


! Vertices at the end of each edge
  vert_on_edge(1 ,:) = (/ 1, 2 /)
  vert_on_edge(2 ,:) = (/ 2, 3 /)
  vert_on_edge(3 ,:) = (/ 3, 1 /)
  vert_on_edge(4 ,:) = (/ 1, 4 /)
  vert_on_edge(5 ,:) = (/ 2, 5 /)
  vert_on_edge(6 ,:) = (/ 3, 6 /)
  vert_on_edge(7 ,:) = (/ 4, 5 /)
  vert_on_edge(8 ,:) = (/ 5, 6 /)
  vert_on_edge(9 ,:) = (/ 6, 4 /)

! Edges on each face
  edge_on_face(1,:) = (/ 1, 2, 7, 4 /)
  edge_on_face(2,:) = (/ 2, 6, 8, 5 /)
  edge_on_face(3,:) = (/ 6, 9, 4, 3 /)
  edge_on_face(4,:) = (/ 1, 2, 3, 0 /)
  edge_on_face(5,:) = (/ 4, 5, 6, 0 /)

! Edges on each vertex
  edge_on_vert(1,:) = (/ 1, 4, 3 /)
  edge_on_vert(2,:) = (/ 1, 2, 5 /)
  edge_on_vert(3,:) = (/ 2, 3, 6 /)
  edge_on_vert(4,:) = (/ 4, 7, 9 /)
  edge_on_vert(5,:) = (/ 5, 7, 8 /)
  edge_on_vert(6,:) = (/ 6, 8, 9 /)

! Faces either side of each edge
  face_on_edge(1 ,:) = (/ 1, 4 /)
  face_on_edge(2 ,:) = (/ 2, 4 /)
  face_on_edge(3 ,:) = (/ 3, 4 /)
  face_on_edge(4 ,:) = (/ 3, 1 /)
  face_on_edge(5 ,:) = (/ 1, 2 /)
  face_on_edge(6 ,:) = (/ 2, 3 /)
  face_on_edge(7 ,:) = (/ 1, 5 /)
  face_on_edge(8 ,:) = (/ 2, 5 /)
  face_on_edge(9 ,:) = (/ 3, 5 /)

  
! outward unit normal vector to each face  
  normal_to_face(1,:) = (/  0.0_dp, -1.0_dp,  0.0_dp /)
  normal_to_face(2,:) = (/  rt3ov2,  0.5_dp,  0.0_dp /)
  normal_to_face(3,:) = (/ -rt3ov2,  0.5_dp,  0.0_dp /)
  normal_to_face(5,:) = (/  0.0_dp,  0.0_dp, -1.0_dp /)
  normal_to_face(6,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  
! tangent vectors to each edge 
! convention is that vector points from vert_on_edge(i,1) > vert_on_edge(i,2)
  tangent_to_edge(1 ,:) = (/  1.0_dp,  0.0_dp,  0.0_dp /)
  tangent_to_edge(2 ,:) = (/ -0.5_dp,  rt3ov2,  0.0_dp /)
  tangent_to_edge(3 ,:) = (/ -0.5_dp, -rt3ov2,  0.0_dp /)  
  tangent_to_edge(4 ,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  tangent_to_edge(5 ,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  tangent_to_edge(6 ,:) = (/  0.0_dp,  0.0_dp,  1.0_dp /)
  tangent_to_edge(7 ,:) = (/  1.0_dp,  0.0_dp,  0.0_dp /)
  tangent_to_edge(8 ,:) = (/ -0.5_dp,  rt3ov2,  0.0_dp /)
  tangent_to_edge(9 ,:) = (/ -0.5_dp, -rt3ov2,  0.0_dp /)


  
end subroutine reference_triangle

end module reference_element_mod
