!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief A module that holds dofmaps for the seven element spaces 
!>
!> @detail The dofmaps for the seven element spaces are stored in this module. The 
!>         module also contains the code to calculate the dofmaps. This will eventually
!>         be replaced with code that reads them in from a file.

!-------------------------------------------------------------------------------
! Computes the dofmaps for the 7 element spaces given grid connectivity information
! requires: list of cell next to current cell
!           list of vertices on this cell
!-------------------------------------------------------------------------------
module dofmap_mod

use num_dof_mod
use reference_element_mod
use mesh_mod,          only: mesh_type
use constants_mod,     only: i_def, c_def
use configuration_mod, only: l_spherical
use log_mod,           only: log_event, LOG_LEVEL_ERROR

implicit none

!> A two dim integer arrays which hold the indirection maps (or dofmaps)
!! for the whole W0 function space over the bottom level of the domain.
integer, allocatable :: w0_dofmap(:,:)
!> A two dim integer arrays which hold the indirection maps (or dofmaps)
!! for the whole W1 function space over the bottom level of the domain.
integer, allocatable :: w1_dofmap(:,:)
!> A two dim integer arrays which hold the indirection maps (or dofmaps)
!! for the whole W2 function space over the bottom level of the domain.
integer, allocatable :: w2_dofmap(:,:)
!> A two dim integer arrays which hold the indirection maps (or dofmaps)
!! for the whole W3 function space over the bottom level of the domain.
integer, allocatable :: w3_dofmap(:,:)
!> A two dim integer arrays which hold the indirection maps (or dofmaps)
!! for the whole Wtheta function space over the bottom level of the domain.
integer, allocatable :: wtheta_dofmap(:,:)
!> A two dim integer arrays which hold the indirection maps (or dofmaps)
!! for the whole W2V function space over the bottom level of the domain.
integer, allocatable :: w2v_dofmap(:,:)
!> A two dim integer arrays which hold the indirection maps (or dofmaps)
!! for the whole W2H function space over the bottom level of the domain.
integer, allocatable :: w2h_dofmap(:,:)

!> An integer array which holds a unique global index for every dof in
!! the whole W0 function space 
integer(i_def), allocatable :: w0_global_dof_id(:)
!> An integer array which holds a unique global index for every dof in
!! the whole W1 function space
integer(i_def), allocatable :: w1_global_dof_id(:)
!> An integer array which holds a unique global index for every dof in
!! the whole W2 function space
integer(i_def), allocatable :: w2_global_dof_id(:)
!> An integer array which holds a unique global index for every dof in
!! the whole W3 function space
integer(i_def), allocatable :: w3_global_dof_id(:)
!> An integer array which holds a unique global index for every dof in
!! the whole Wtheta function space
integer(i_def), allocatable :: wtheta_global_dof_id(:)
!> An integer array which holds a unique global index for every dof in
!! the whole W2V function space
integer(i_def), allocatable :: w2v_global_dof_id(:)
!> An integer array which holds a unique global index for every dof in
!! the whole W2H function space
integer(i_def), allocatable :: w2h_global_dof_id(:)


!> The index within the dofmap of the last "owned" dof in the W0 function space
integer              :: w0_last_dof_owned
!> The index within the dofmap of the last "annexed" dof in the W0 function
!> space ("Annexed" dofs that those that are not owned, but are on owned cells)
integer              :: w0_last_dof_annexed
!> The index within the dofmap of the last of the halo dofs (from the various
!> depths of halo) in the W0 function space
integer, allocatable :: w0_last_dof_halo(:)
!> The index within the dofmap of the last "owned" dof in the W1 function space
integer              :: w1_last_dof_owned
!> The index within the dofmap of the last "annexed" dof in the W1 function
!> space ("Annexed" dofs that those that are not owned, but are on owned cells)
integer              :: w1_last_dof_annexed
!> The index within the dofmap of the last of the halo dofs (from the various
!> depths of halo) in the W1 function space
integer, allocatable :: w1_last_dof_halo(:)
!> The index within the dofmap of the last "owned" dof in the W2 function space
integer              :: w2_last_dof_owned
!> The index within the dofmap of the last "annexed" dof in the W2 function
!> space ("Annexed" dofs that those that are not owned, but are on owned cells)
integer              :: w2_last_dof_annexed
!> The index within the dofmap of the last of the halo dofs (from the various
!> depths of halo) in the W2 function space
integer, allocatable :: w2_last_dof_halo(:)
!> The index within the dofmap of the last "owned" dof in the W3 function space
integer              :: w3_last_dof_owned
!> The index within the dofmap of the last "annexed" dof in the W3 function
!> space ("Annexed" dofs that those that are not owned, but are on owned cells)
integer              :: w3_last_dof_annexed
!> The index within the dofmap of the last of the halo dofs (from the various
!> depths of halo) in the W3 function space
integer, allocatable :: w3_last_dof_halo(:)
!> The index within the dofmap of the last "owned" dof in the Wtheta function space
integer              :: wtheta_last_dof_owned
!> The index within the dofmap of the last "annexed" dof in the Wtheta function
!> space ("Annexed" dofs that those that are not owned, but are on owned cells)
integer              :: wtheta_last_dof_annexed
!> The index within the dofmap of the last of the halo dofs (from the various
!> depths of halo) in the Wtheta function space
integer, allocatable :: wtheta_last_dof_halo(:)
!> The index within the dofmap of the last "owned" dof in the W2V function space
integer              :: w2v_last_dof_owned
!> The index within the dofmap of the last "annexed" dof in the W2V function
!> space ("Annexed" dofs that those that are not owned, but are on owned cells)
integer              :: w2v_last_dof_annexed
!> The index within the dofmap of the last of the halo dofs (from the various
!> depths of halo) in the W2V function space
integer, allocatable :: w2v_last_dof_halo(:)
!> The index within the dofmap of the last "owned" dof in the W2H function space
integer              :: w2h_last_dof_owned
!> The index within the dofmap of the last "annexed" dof in the W2H function
!> space ("Annexed" dofs that those that are not owned, but are on owned cells)
integer              :: w2h_last_dof_annexed
!> The index within the dofmap of the last of the halo dofs (from the various
!> depths of halo) in the W2H function space
integer, allocatable :: w2h_last_dof_halo(:)


!> A two dim integer array which holds the orientation data for the
!> W0 function space
integer, allocatable :: w0_orientation(:,:)
!> A two dim integer array which holds the orientation data for the
!> W1 function space
integer, allocatable :: w1_orientation(:,:)
!> A two dim integer array which holds the orientation data for the
!> W2 function space
integer, allocatable :: w2_orientation(:,:)
!> A two dim integer array which holds the orientation data for the
!> W3 function space
integer, allocatable :: w3_orientation(:,:)
!> A two dim integer array which holds the orientation data for the
!> Wtheta function space
integer, allocatable :: wtheta_orientation(:,:)
!> A two dim integer array which holds the orientation data for the
!> W2V function space
integer, allocatable :: w2v_orientation(:,:)
!> A two dim integer array which holds the orientation data for the
!> W2H function space
integer, allocatable :: w2h_orientation(:,:)


!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains 

!> @brief Subroutine to get the dofmap and copy is into the function space
!> @param[in] mesh           Mesh object to base dof maps on
!> @param[in] function_space The function space
!> @param[in] ndf_entity     The number of dofs on each grid entity
!> @param[in] w_dof_entity ndofs for vert, edge, face, cell, exterior and
!> interior columns for W0-W2H
!> @param[in] w_unique_dofs ndofs for global, local, exterior & interior dofs in
!> columns for W0-W2H
subroutine get_dofmap(mesh, w_dof_entity, w_unique_dofs)
  
  implicit none
  
  type (mesh_type), intent(in) :: mesh

  integer, intent(in) :: w_dof_entity(7,0:5)
  integer, intent(in) :: w_unique_dofs(7,4) 

  integer(i_def) :: ncells

  ncells = mesh%get_ncells_2d_with_ghost()

  allocate( w0_dofmap(w_unique_dofs(1,2),0:ncells) )
  allocate( w1_dofmap(w_unique_dofs(2,2),0:ncells) )
  allocate( w2_dofmap(w_unique_dofs(3,2),0:ncells) )
  allocate( w3_dofmap(w_unique_dofs(4,2),0:ncells) )
  allocate( wtheta_dofmap(w_unique_dofs(5,2),0:ncells) )
  allocate( w2v_dofmap(w_unique_dofs(6,2),0:ncells) )
  allocate( w2h_dofmap(w_unique_dofs(7,2),0:ncells) )

  allocate( w0_global_dof_id(w_unique_dofs(1,1)) )
  allocate( w1_global_dof_id(w_unique_dofs(2,1)) )
  allocate( w2_global_dof_id(w_unique_dofs(3,1)) )
  allocate( w3_global_dof_id(w_unique_dofs(4,1)) )
  allocate( wtheta_global_dof_id(w_unique_dofs(5,1)) )
  allocate( w2v_global_dof_id(w_unique_dofs(6,1)) )
  allocate( w2h_global_dof_id(w_unique_dofs(7,1)) )

  allocate( w0_last_dof_halo(mesh%get_halo_depth()) )
  allocate( w1_last_dof_halo(mesh%get_halo_depth()) )
  allocate( w2_last_dof_halo(mesh%get_halo_depth()) )
  allocate( w3_last_dof_halo(mesh%get_halo_depth()) )
  allocate( wtheta_last_dof_halo(mesh%get_halo_depth()) )
  allocate( w2v_last_dof_halo(mesh%get_halo_depth()) )
  allocate( w2h_last_dof_halo(mesh%get_halo_depth()) )

  call dofmap_populate( mesh, &
                        ncells, &
                        w_unique_dofs(1,2), &
                        w_unique_dofs(1,1), &
                        w_dof_entity(1,:), &
                        select_entity_all, &
                        w0_dofmap, &
                        w0_global_dof_id, &
                        w0_last_dof_owned,&
                        w0_last_dof_annexed,&
                        w0_last_dof_halo)
  call dofmap_populate( mesh, &
                        ncells, &
                        w_unique_dofs(2,2), &
                        w_unique_dofs(2,1), &
                        w_dof_entity(2,:), &
                        select_entity_all, &
                        w1_dofmap, &
                        w1_global_dof_id, &
                        w1_last_dof_owned,&
                        w1_last_dof_annexed,&
                        w1_last_dof_halo)
  call dofmap_populate( mesh, &
                        ncells, &
                        w_unique_dofs(3,2), &
                        w_unique_dofs(3,1), &
                        w_dof_entity(3,:), &
                        select_entity_all, &
                        w2_dofmap, &
                        w2_global_dof_id, &
                        w2_last_dof_owned,&
                        w2_last_dof_annexed,&
                        w2_last_dof_halo)
  call dofmap_populate( mesh, &
                        ncells, &
                        w_unique_dofs(4,2), &
                        w_unique_dofs(4,1), &
                        w_dof_entity(4,:), &
                        select_entity_all, &
                        w3_dofmap, &
                        w3_global_dof_id, &
                        w3_last_dof_owned,&
                        w3_last_dof_annexed,&
                        w3_last_dof_halo)
  call dofmap_populate( mesh, &
                        ncells, &
                        w_unique_dofs(5,2), &
                        w_unique_dofs(5,1), &
                        w_dof_entity(5,:), &
                        select_entity_theta, &
                        wtheta_dofmap, &
                        wtheta_global_dof_id, &
                        wtheta_last_dof_owned,&
                        wtheta_last_dof_annexed,&
                        wtheta_last_dof_halo)
  call dofmap_populate( mesh, &
                        ncells, &
                        w_unique_dofs(6,2), &
                        w_unique_dofs(6,1), &
                        w_dof_entity(6,:), &
                        select_entity_w2v, &
                        w2v_dofmap, &
                        w2v_global_dof_id, &
                        w2v_last_dof_owned,&
                        w2v_last_dof_annexed,&
                        w2v_last_dof_halo)
  call dofmap_populate( mesh, &
                        ncells, &
                        w_unique_dofs(7,2), &
                        w_unique_dofs(7,1), &
                        w_dof_entity(7,:), &
                        select_entity_w2h, &
                        w2h_dofmap, &
                        w2h_global_dof_id, &
                        w2h_last_dof_owned,&
                        w2h_last_dof_annexed,&
                        w2h_last_dof_halo)


end subroutine get_dofmap

!> @brief Subroutine to compute the dofmap based upon grid connectivities for
!>        a function space
!> @param[in] mesh        Mesh object to base dof maps on
!> @param[in] ncells      The number of horizontal cells
!> @param[in] ndof_sum    The total number of dofs associated with a single cell
!> @param[in] total_dofs The total number of dofs in the domain (on this partition)
!> @param[in] ndf_entity  The number of dofs on each grid entity
!> @param[in] select_entity  Data type that holds lists of entities to use in the function space
!> @param[out] dofmap     The dofmap generated by the routine
!> @param[out] global_dof_id The globally unique id for each dof 
!> @param[out] last_dof_owned The index of the last owned dof in the dofmap
!> @param[out] last_dof_annexed The index of the last annexed dof in the dofmap
!>           (an annexed dof is one which is not owned, but is on an owned cell)
!> @param[out] last_dof_halo An array of the indices of the last halo dofs in
!>                           the various depths of halo
subroutine dofmap_populate( mesh, &
                            ncells, &
                            ndof_sum, &
                            total_dofs, &
                            ndof_entity, &
                            select_entity, &
                            dofmap, &
                            global_dof_id, &
                            last_dof_owned, &
                            last_dof_annexed, &
                            last_dof_halo)

  implicit none

  ! Mesh object to apply dofmap on 
  type (mesh_type), intent(in) :: mesh
  integer (i_def),  intent(in) :: ncells

! number of dofs per entity for this space
  integer, intent(in) :: ndof_entity(0:5)
! total number of dofs associated with each cell  
  integer, intent(in) :: ndof_sum
! total number of dofs in the domain (on this partition)
  integer(i_def), intent(in) :: total_dofs

! lists of entities to use in the function space
  type(select_entity_type), intent(in) :: select_entity

! output dofmap for this space
  integer, intent(out) :: dofmap(ndof_sum,0:ncells)

! output global dof ids for this space
  integer(i_def), intent(out) :: global_dof_id(total_dofs)

! output number of dofs that are owned, have been annexed by the neighbouring partition
! and are in the various levels of halo
  integer, intent(out) :: last_dof_owned
  integer, intent(out) :: last_dof_annexed
  integer, intent(out) :: last_dof_halo(:)

! Loop counters
  integer(i_def) :: icell, iface, iedge, ivert, idof, idepth, k

! Number of layers
  integer(i_def) :: nlayers

! Indices into the dofmap
  integer(i_def) :: id_owned, id_halo, id0, dof_idx
  integer(i_def) :: face_id, edge_id, vert_id
  integer(i_def) :: bottom_edge_id, top_edge_id, side_edge_id
  integer(i_def) :: bottom_vert_id, top_vert_id
! Number of entities for a single layer  
  integer :: nvert_layer, nedge_layer, nface_layer

! Start and end points of the cell indices to loop over
  integer :: start,finish

! entity dofmaps
  integer(i_def), allocatable :: dofmap_d0(:,:), &
                                 dofmap_d1(:,:), &
                                 dofmap_d2(:,:), &
                                 dofmap_d3(:,:)

! dof column heights for entities
  integer(i_def), allocatable :: dof_column_height_d0(:,:), &
                                 dof_column_height_d1(:,:), &
                                 dof_column_height_d2(:,:), &
                                 dof_column_height_d3(:,:)

! cell that owns the dofs on entities
  integer(i_def), allocatable :: dof_cell_owner_d0(:,:), &
                                 dof_cell_owner_d1(:,:), &
                                 dof_cell_owner_d2(:,:), &
                                 dof_cell_owner_d3(:,:)

! dof column heights for whole space
  integer(i_def), allocatable :: dof_column_height(:,:)

! owning cell of each entry in the dofamp
  integer(i_def), allocatable :: dof_cell_owner(:,:)

! cell id in global index space
  integer(i_def) :: global_cell_id

  allocate(dof_column_height(ndof_sum,0:ncells))
  allocate(dof_cell_owner(ndof_sum,0:ncells))

  ! dofmaps for a 3D horizontal layer
  nlayers     =   mesh%get_nlayers()
  nvert_layer = 2*mesh%get_nverts_2d()
  nedge_layer = 2*mesh%get_nedges_2d() + mesh%get_nverts_2d()
  nface_layer =   mesh%get_nedges_2d() + 2*ncells

  if ( ndof_entity(0) > 0 ) then
    allocate(            dofmap_d0(ndof_entity(0),nvert_layer) )
    allocate( dof_column_height_d0(ndof_entity(0),nvert_layer) )
    allocate(    dof_cell_owner_d0(ndof_entity(0),nvert_layer) )
  else
    allocate(            dofmap_d0(1,nvert_layer) )
    allocate( dof_column_height_d0(1,nvert_layer) )
    allocate(    dof_cell_owner_d0(1,nvert_layer) )
  end if
  if ( ndof_entity(1) > 0 ) then  
    allocate(            dofmap_d1(ndof_entity(1),nedge_layer) )
    allocate( dof_column_height_d1(ndof_entity(1),nedge_layer) )
    allocate(    dof_cell_owner_d1(ndof_entity(1),nedge_layer) )
  else
    allocate(            dofmap_d1(1,nedge_layer) )
    allocate( dof_column_height_d1(1,nedge_layer) )
    allocate(    dof_cell_owner_d1(1,nedge_layer) )
  end if  
  if ( ndof_entity(2) > 0 ) then  
    allocate(            dofmap_d2(ndof_entity(2),nface_layer) )
    allocate( dof_column_height_d2(ndof_entity(2),nface_layer) )
    allocate(    dof_cell_owner_d2(ndof_entity(2),nface_layer) )
  else
    allocate(            dofmap_d2(1,nface_layer) )
    allocate( dof_column_height_d2(1,nface_layer) )
    allocate(    dof_cell_owner_d2(1,nface_layer) )
  end if
  if ( ndof_entity(3) > 0 ) then    
    allocate(            dofmap_d3(ndof_entity(3),ncells) )
    allocate( dof_column_height_d3(ndof_entity(3),ncells) )
    allocate(    dof_cell_owner_d3(ndof_entity(3),ncells) )
  else
    allocate(            dofmap_d3(1,             ncells) )
    allocate( dof_column_height_d3(1,             ncells) )
    allocate(    dof_cell_owner_d3(1,             ncells) )
  end if

! initialise entity dofmaps
  dofmap_d0(:,:) = 0
  dofmap_d1(:,:) = 0
  dofmap_d2(:,:) = 0
  dofmap_d3(:,:) = 0

! assume we have all possible global connectivity information
! in practice this requires connectivity
! (3,2) -> faces on cells
! (3,1) -> edges on cells
! (3,0) -> vertices on cells

  id_owned = 1
  id_halo  = -1

! loop over 3 entities (cells) starting with core + owned + first depth halo
! then proceding with further halo depths as required

  start=1
  finish=mesh%get_num_cells_core() + &
         mesh%get_num_cells_owned() + &
         mesh%get_num_cells_halo(1)

  halo_loop: do idepth = 1, mesh%get_halo_depth()+1
    cell_loop: do icell = start, finish

! assign dofs for connectivity (3,3) (dofs in cell)
      if(mesh%is_cell_owned(icell))then
        do idof=1,ndof_entity(3)
          dofmap_d3(idof,icell) = id_owned
          dof_column_height_d3(idof,icell) = nlayers
          dof_cell_owner_d3(idof,icell) = icell
          id_owned = id_owned + nlayers
        end do
      else
        do idof=1,ndof_entity(3)
          dofmap_d3(idof,icell) = id_halo
          dof_column_height_d3(idof,icell) = nlayers
          dof_cell_owner_d3(idof,icell) = icell
          id_halo = id_halo - nlayers
        end do
      end if

! assign dofs for connectivity (3,2) (dofs on faces)
      do iface=1,nfaces_h
        if (any(select_entity % faces==iface)) then
          face_id = mesh%get_face_on_cell(iface,icell) 
          if(mesh%is_edge_owned(iface,icell))then
            if ( dofmap_d2(1,face_id) == 0 ) then
              do idof=1,ndof_entity(2)
                dofmap_d2(idof,face_id) = id_owned
                dof_column_height_d2(idof,face_id) = nlayers
                dof_cell_owner_d2(idof,face_id) = &
                                     mesh%get_edge_cell_owner(iface,icell)
                id_owned = id_owned + nlayers
              end do
            end if
          else
            if ( dofmap_d2(1,face_id) == 0 ) then
              do idof=1,ndof_entity(2)
                dofmap_d2(idof,face_id) = id_halo
                dof_column_height_d2(idof,face_id) = nlayers
                dof_cell_owner_d2(idof,face_id) = &
                                     mesh%get_edge_cell_owner(iface,icell)
                id_halo = id_halo - nlayers
              end do
            end if
          end if
        endif!select_entity
      end do
      if(mesh%is_cell_owned(icell))then
        id0 = id_owned
        do iface=nfaces_h+1,nfaces
          if (any(select_entity % faces==iface)) then
            face_id = mesh%get_face_on_cell(iface,icell) 
            if ( dofmap_d2(1,face_id) == 0 ) then
              do idof=1,ndof_entity(2)
                dofmap_d2(idof,face_id) = id_owned        
                if( iface == nfaces_h+1 )then
                  dof_column_height_d2(idof,face_id) = nlayers + 1
                else
                  dof_column_height_d2(idof,face_id) = 0
                end if
                dof_cell_owner_d2(idof,face_id) = icell
                id_owned = id_owned + nlayers + 1
              end do
            end if
            if (iface==nfaces_h+1) then
              id_owned = id0 + 1
            else
              id_owned = id_owned - 1
            end if
          endif!select_entity
        end do
      else
        id0 = id_halo
        do iface=nfaces_h+1,nfaces
          if (any(select_entity % faces==iface)) then
            face_id = mesh%get_face_on_cell(iface,icell) 
            if ( dofmap_d2(1,face_id) == 0 ) then
              do idof=1,ndof_entity(2)
                dofmap_d2(idof,face_id) = id_halo        
                if( iface == nfaces_h+1 )then
                  dof_column_height_d2(idof,face_id) = nlayers + 1
                else
                  dof_column_height_d2(idof,face_id) = 0
                end if
                dof_cell_owner_d2(idof,face_id) = icell
                id_halo = id_halo - nlayers - 1
              end do
            end if
            if (iface==nfaces_h+1) then
              id_halo = id0 - 1
            else
              id_halo = id_halo + 1
            end if
          endif!select_entity
        end do
      end if

! assign dofs for connectivity (3,1) (dofs on edges)  
      do iedge=1,nedges_h
        bottom_edge_id = mesh%get_edge_on_cell(iedge,icell)   
        top_edge_id    = mesh%get_edge_on_cell(iedge+nedges-nedges_h,icell)  
        if(mesh%is_edge_owned(iedge,icell))then
          if ( dofmap_d1(1,bottom_edge_id) == 0 ) then
            do idof=1,ndof_entity(1)
              dofmap_d1(idof,bottom_edge_id)  = id_owned
              dofmap_d1(idof,top_edge_id)     = id_owned + 1
              dof_column_height_d1(idof,bottom_edge_id) = nlayers + 1
              dof_column_height_d1(idof,top_edge_id   ) = 0
              dof_cell_owner_d1(idof,bottom_edge_id) = &
                                   mesh%get_edge_cell_owner(iedge,icell)
              dof_cell_owner_d1(idof,top_edge_id   ) = &
                                   mesh%get_edge_cell_owner(iedge,icell)
              id_owned = id_owned + nlayers + 1
            end do
          end if
        else
          if ( dofmap_d1(1,bottom_edge_id) == 0 ) then
            do idof=1,ndof_entity(1)
              dofmap_d1(idof,bottom_edge_id)  = id_halo
              dofmap_d1(idof,top_edge_id)     = id_halo - 1
              dof_column_height_d1(idof,bottom_edge_id) = nlayers + 1
              dof_column_height_d1(idof,top_edge_id   ) = 0
              dof_cell_owner_d1(idof,bottom_edge_id) = &
                                   mesh%get_edge_cell_owner(iedge,icell)
              dof_cell_owner_d1(idof,top_edge_id   ) = &
                                   mesh%get_edge_cell_owner(iedge,icell)
              id_halo = id_halo - nlayers - 1
            end do
          end if
        end if
      end do
      do iedge=nedges_h+1,nedges-nedges_h
        side_edge_id  = mesh%get_edge_on_cell(iedge,icell) 
        if(mesh%is_vertex_owned(iedge-nedges_h,icell))then
          if ( dofmap_d1(1,side_edge_id) == 0 ) then
            do idof=1,ndof_entity(1)
              dofmap_d1(idof,side_edge_id)  = id_owned
              dof_column_height_d1(idof,side_edge_id) = nlayers
              dof_cell_owner_d1(idof,side_edge_id) = &
                              mesh%get_vertex_cell_owner(iedge-nedges_h,icell)
              id_owned = id_owned + nlayers 
            end do
          end if
        else
          if ( dofmap_d1(1,side_edge_id) == 0 ) then
            do idof=1,ndof_entity(1)
              dofmap_d1(idof,side_edge_id)  = id_halo
              dof_column_height_d1(idof,side_edge_id) = nlayers
              dof_cell_owner_d1(idof,side_edge_id) = &
                              mesh%get_vertex_cell_owner(iedge-nedges_h,icell)
              id_halo = id_halo - nlayers 
            end do
          end if
        end if
      end do

! assign dofs for connectivity (3,0) (dofs on verts)    
      do ivert=1,nverts_h
        bottom_vert_id  = mesh%get_vert_on_cell(ivert,icell)
        top_vert_id     = mesh%get_vert_on_cell(ivert+nverts_h,icell)
        if(mesh%is_vertex_owned(ivert,icell))then
          if ( dofmap_d0(1,bottom_vert_id) == 0 ) then
            do idof=1,ndof_entity(0)
              dofmap_d0(idof,bottom_vert_id)  = id_owned
              dofmap_d0(idof,top_vert_id)     = id_owned + 1
              dof_column_height_d0(idof,bottom_vert_id) = nlayers + 1
              dof_column_height_d0(idof,top_vert_id   ) = 0
              dof_cell_owner_d0(idof,bottom_vert_id) =  &
                                   mesh%get_vertex_cell_owner(ivert,icell)
              dof_cell_owner_d0(idof,top_vert_id   ) =  &
                                   mesh%get_vertex_cell_owner(ivert,icell)
              id_owned = id_owned + nlayers + 1     
            end do
          end if
        else
          if ( dofmap_d0(1,bottom_vert_id) == 0 ) then
            do idof=1,ndof_entity(0)
              dofmap_d0(idof,bottom_vert_id)  = id_halo
              dofmap_d0(idof,top_vert_id)     = id_halo - 1
              dof_column_height_d0(idof,bottom_vert_id) = nlayers + 1
              dof_column_height_d0(idof,top_vert_id   ) = 0
              dof_cell_owner_d0(idof,bottom_vert_id) =  &
                                   mesh%get_vertex_cell_owner(ivert,icell)
              dof_cell_owner_d0(idof,top_vert_id   ) =  &
                                   mesh%get_vertex_cell_owner(ivert,icell)
              id_halo = id_halo - nlayers - 1  
            end do
          end if
        end if
      end do

      if(icell == mesh%get_num_cells_core() + mesh%get_num_cells_owned())then
        last_dof_owned = id_owned - 1
        last_dof_annexed = id_owned - id_halo - 2
      end if

    end do cell_loop

    if(idepth <= mesh%get_halo_depth()) &
      last_dof_halo(idepth) = id_owned - id_halo - 2

    start=finish+1
    if(idepth < mesh%get_halo_depth()) then
      finish=start+mesh%get_num_cells_halo(idepth+1)-1
    else
      finish=start+mesh%get_num_cells_ghost()-1
    end if

  end do halo_loop


! Copy from the dofmap_dn arrays into one dofmap array
  dof_column_height(:,:)=-999
  dof_cell_owner(:,:)=-999
  dofmap(:,:)=-999
  do icell=1,ncells
    dof_idx = 1
    ! dofs in cells
    do idof=1,ndof_entity(3)
      if( dofmap_d3(idof,icell) /= 0 )then
        if( dofmap_d3(idof,icell) > 0 )then
          dofmap(dof_idx,icell) = dofmap_d3(idof,icell)
        else if( dofmap_d3(idof,icell) < 0 )then
          dofmap(dof_idx,icell) = id_owned - (dofmap_d3(idof,icell) + 1)
        end if
        dof_column_height(dof_idx,icell) = dof_column_height_d3(idof,icell)
        dof_cell_owner(dof_idx,icell) = dof_cell_owner_d3(idof,icell)
        dof_idx = dof_idx + 1
      end if
    end do
    ! dofs on faces
    do iface=1,nfaces
      face_id = mesh%get_face_on_cell(iface,icell) 
      do idof=1,ndof_entity(2)
        if( dofmap_d2(idof,face_id) /= 0 )then
          if( dofmap_d2(idof,face_id) > 0 )then
            dofmap(dof_idx,icell) = dofmap_d2(idof,face_id)
          else if( dofmap_d2(idof,face_id) < 0 )then
            dofmap(dof_idx,icell) = id_owned - (dofmap_d2(idof,face_id) + 1)
          end if
          dof_column_height(dof_idx,icell) = dof_column_height_d2(idof,face_id)
          dof_cell_owner(dof_idx,icell) = dof_cell_owner_d2(idof,face_id)
          dof_idx = dof_idx + 1
        end if
      end do
    end do
    ! dofs on edges
    do iedge=1,nedges
      edge_id  = mesh%get_edge_on_cell(iedge,icell) 
      do idof=1,ndof_entity(1)
        if( dofmap_d1(idof,edge_id) /= 0 )then
          if( dofmap_d1(idof,edge_id) > 0 )then
            dofmap(dof_idx,icell) = dofmap_d1(idof,edge_id)
          else if( dofmap_d1(idof,edge_id) < 0 )then
            dofmap(dof_idx,icell) = id_owned - (dofmap_d1(idof,edge_id) + 1)
          end if
          dof_column_height(dof_idx,icell) = dof_column_height_d1(idof,edge_id)
          dof_cell_owner(dof_idx,icell) = dof_cell_owner_d1(idof,edge_id)
          dof_idx = dof_idx + 1
        end if
      end do
    end do 
    ! dofs on vertices
    do ivert=1,nverts
      vert_id  = mesh%get_vert_on_cell(ivert,icell)
      do idof=1,ndof_entity(0)
        if( dofmap_d0(idof,vert_id) /= 0 )then
          if( dofmap_d0(idof,vert_id) > 0 )then
            dofmap(dof_idx,icell) = dofmap_d0(idof,vert_id)
          else if( dofmap_d0(idof,vert_id) < 0 )then
            dofmap(dof_idx,icell) = id_owned - (dofmap_d0(idof,vert_id) + 1)
          end if
          dof_column_height(dof_idx,icell) = dof_column_height_d0(idof,vert_id)
          dof_cell_owner(dof_idx,icell) = dof_cell_owner_d0(idof,vert_id)
          dof_idx = dof_idx + 1
        end if
      end do
    end do
  end do

  dofmap(:,0) = 0

  if (allocated(dofmap_d0) ) deallocate( dofmap_d0 )
  if (allocated(dofmap_d1) ) deallocate( dofmap_d1 )
  if (allocated(dofmap_d2) ) deallocate( dofmap_d2 )
  if (allocated(dofmap_d3) ) deallocate( dofmap_d3 )

  if ( allocated(dof_column_height_d0) ) deallocate( dof_column_height_d0 )
  if ( allocated(dof_column_height_d1) ) deallocate( dof_column_height_d1 )
  if ( allocated(dof_column_height_d2) ) deallocate( dof_column_height_d2 )
  if ( allocated(dof_column_height_d3) ) deallocate( dof_column_height_d3 )

  if ( allocated(dof_cell_owner_d0) ) deallocate( dof_cell_owner_d0 )
  if ( allocated(dof_cell_owner_d1) ) deallocate( dof_cell_owner_d1 )
  if ( allocated(dof_cell_owner_d2) ) deallocate( dof_cell_owner_d2 )
  if ( allocated(dof_cell_owner_d3) ) deallocate( dof_cell_owner_d3 )

  ! Calculate a globally unique id for each dof, such that each partition
  ! that needs access to that dof will calculate the same id
  global_dof_id(:) = 0
  do icell=1,ncells
    global_cell_id=mesh%get_gid_from_lid(icell)
    do idof=1,ndof_sum
      if(icell == dof_cell_owner(idof,icell))then
        do k = 1, dof_column_height(idof, icell)
          global_dof_id(dofmap(idof,icell)+k-1) = &
                                     (global_cell_id-1)*ndof_sum*(nlayers+1) + &
                                     (idof-1)*(nlayers+1) + k
        end do
      end if
    end do
  end do

  if ( allocated(dof_column_height) ) deallocate( dof_column_height )
  if ( allocated(dof_cell_owner) ) deallocate( dof_cell_owner )

end subroutine dofmap_populate

!> @brief Subroutine to compute the orientation of vectors
!> @param[in] mesh          Mesh object to base dof maps on
!> @param[in] w_unique_dofs The number of dofs in each function space
!> @param[in] w_dof_entity
subroutine get_orientation(mesh, w_unique_dofs, w_dof_entity)
!-----------------------------------------------------------------------------
! Subroutine to read orientation
!-----------------------------------------------------------------------------
  
  implicit none

  type (mesh_type), intent(in) :: mesh
  integer,          intent(in) :: w_unique_dofs(7,4)
  integer,          intent(in) :: w_dof_entity(7,0:5)

  integer(i_def) :: ncells
  logical        :: is_scalar(7)

  is_scalar(:) = (/ .true., .false., .false., .true., .true., .false., .false. /)

  ncells = mesh%get_ncells_2d()

  allocate( w0_orientation(0:ncells,w_unique_dofs(1,2)) )
  allocate( w1_orientation(0:ncells,w_unique_dofs(2,2)) )
  allocate( w2_orientation(0:ncells,w_unique_dofs(3,2)) )
  allocate( w3_orientation(0:ncells,w_unique_dofs(4,2)) )
  allocate( wtheta_orientation(0:ncells,w_unique_dofs(5,2)) )
  allocate( w2v_orientation(0:ncells,w_unique_dofs(6,2)) )
  allocate( w2h_orientation(0:ncells,w_unique_dofs(7,2)) )

  call orientation_populate( mesh, ncells,                                &
                             w_unique_dofs(1,2),                          &
                             w_dof_entity(1,:),                           &
                             is_scalar(1),                                &
                             select_entity_all,                           &
                             w0_orientation )
  call orientation_populate( mesh, ncells,                                &
                             w_unique_dofs(2,2),                          &
                             w_dof_entity(2,:),                           &
                             is_scalar(2),                                &
                             select_entity_all,                           &
                             w1_orientation )
  call orientation_populate( mesh, ncells,                                &
                             w_unique_dofs(3,2),                          &
                             w_dof_entity(3,:),                           &
                             is_scalar(3),                                &
                             select_entity_all,                           &
                             w2_orientation )
  call orientation_populate( mesh, ncells,                                &
                             w_unique_dofs(4,2),                          &
                             w_dof_entity(4,:),                           &
                             is_scalar(4),                                &
                             select_entity_all,                           &
                             w3_orientation )
  call orientation_populate( mesh, ncells,                                &
                             w_unique_dofs(5,2),                          &
                             w_dof_entity(5,:),                           &
                             is_scalar(5),                                &
                             select_entity_theta,                         &
                             wtheta_orientation )
  call orientation_populate( mesh, ncells,                                &
                             w_unique_dofs(6,2),                          &
                             w_dof_entity(6,:),                           &
                             is_scalar(6),                                &
                             select_entity_w2v,                           &
                             w2v_orientation )
  call orientation_populate( mesh, ncells,                                &
                             w_unique_dofs(7,2),                          &
                             w_dof_entity(7,:),                           &
                             is_scalar(7),                                &
                             select_entity_w2h,                           &
                             w2h_orientation )

end subroutine get_orientation

!> @brief Subroutine to compute the orientation of vectors
!> @param[in] mesh         Mesh object to base dof maps on
!> @param[in] ncells       The number of horizontal cells
!> @param[in] ndf          The total number of dofs associated with a
!>                         single cell
!> @param[in] ndf_entity   The number of dofs associated with each grid
!>                         entity in a single cell
!> @param[in] is_scalar    Logical: true (scalar-space) or false (vector-space)
!> @param[in] select_entity  Data type that holds lists of entities to use in
!> the function space
!> @param[out] orientation The output orientation
subroutine orientation_populate(mesh, ncells, ndf, ndf_entity, is_scalar, select_entity, orientation)

  use reference_element_mod, only: nfaces_h, nedges_h, select_entity_type
  implicit none

  type (mesh_type), intent(in) :: mesh
  integer, intent(in)  :: ncells
  integer, intent(in)  :: ndf
  integer, intent(in)  :: ndf_entity(0:5)
  logical, intent(in)  :: is_scalar
  type(select_entity_type), intent(in) :: select_entity
  integer, intent(out) :: orientation(0:ncells,ndf)
  

  integer, allocatable :: face_orientation(:,:), edge_orientation(:,:)
  
  integer :: next_cell
  integer :: next_face, common_face, df_on_face  
  integer :: next_edge, common_edge, df_on_edge  
  integer :: cell, face, edge, df
  integer :: vert_1, vert_1_next

! ndof indicator
!  integer :: ndof_diff

  allocate( face_orientation(nfaces_h, ncells) )
  allocate( edge_orientation(nedges_h, ncells) )

! Check if this is vector or scalar space
  if (is_scalar) then
! orientation is not needed for scalar spaces (but set them to 1 anyway)  
    do cell = 0, ncells
      do df = 1,ndf
       orientation(cell,df) = 1
      end do
    end do
    return
  end if

! initialise all face and edge orientations to 0
  do cell = 1,ncells
    do face = 1,nfaces_h
      face_orientation(face,cell) = 0
    end do
    do edge = 1,nedges_h
      edge_orientation(edge,cell) = 0
    end do    
  end do
   
  do cell = 1,ncells
! Face orientation for this cell  
    do face = 1,nfaces_h
      if ( face_orientation(face,cell) == 0 ) then
        next_cell = mesh%get_cell_next(face,cell)
        common_face = 0
        do next_face = 1,nfaces_h
          if ( mesh%get_cell_next(next_face,next_cell) == cell) common_face = next_face
        end do
        face_orientation(face,cell) = 1
! if neighbouring faces are in set (1,1),(1,2),(2,2),(3,3),(3,4),(4,4)
! or the reverse (2,1), (4,3)
! then reverse orientation of one element
        if ( face == common_face &
        .or. face + common_face == 3 &
        .or. face + common_face == 7 ) then
          face_orientation(common_face,next_cell) = -1
        else
          face_orientation(common_face,next_cell) = 1
        end if
      end if   
    end do
! Edge orientation of this cell
    do edge = 1,nedges_h
! This works as horizontal edges == horizontal faces    
      if ( edge_orientation(edge,cell) == 0 ) then
        next_cell = mesh%get_cell_next(edge,cell)
        common_edge = 0
        do next_edge = 1,nedges_h
          if ( mesh%get_cell_next(next_edge,next_cell) == cell) common_edge = next_edge 
        end do
        edge_orientation(edge,cell) = 1
        
        vert_1      = mesh%get_vert_on_cell(edge,cell)
        vert_1_next = mesh%get_vert_on_cell(common_edge,next_cell)

! if neighbouring edges are (1,2), (2,1) or (3,4), (4,3) then
        if ( max(edge,common_edge) < 3 .or. min(edge,common_edge) > 2 ) then 
          if ( vert_1 == vert_1_next ) then
            edge_orientation(common_edge,next_cell) = 1
! if edges are in the opposite direction then reverse orientation            
          else
            edge_orientation(common_edge,next_cell) = -1
          end if  
        else
! else if neighbouring edges are (1,3), (1,4) or (2,3), (2,4) + symmetric changes then        
! if edges are in the same direction then reverse orientation         
          if ( vert_1 == vert_1_next ) then
            edge_orientation(common_edge,next_cell) = -1
          else
            edge_orientation(common_edge,next_cell) = 1
          end if           
        end if
      end if
    end do
  end do

! Populate cell orientation  
  do cell = 0, ncells
! initialise all orientations to 1
    do df = 1,ndf
     orientation(cell,df) = 1   
    end do
  end do
  
  do cell = 1, ncells 
! Overwrite dof orientation with face orientation 
! only applicable if ndf_entity(2) > 0
    df = ndf_entity(3) + 1 
    do face = 1,nfaces_h
      if (any(select_entity % faces==face)) then
        do df_on_face = 1,ndf_entity(2)
          orientation(cell,df) = face_orientation(face,cell)
          df = df + 1
        end do
      end if
    end do
! ! Overwrite dof orientation with edge orientation
! only applicable if ndf_entity(1) > 0
    df = ndf_entity(3) + nfaces*ndf_entity(2) + 1 
    do edge = 1,nedges_h
      do df_on_edge = 1,ndf_entity(1) 
        orientation(cell,df) = edge_orientation(edge,cell)
        df = df + 1
      end do
    end do 
  end do
  do cell = 0, ncells
    do df = 1,ndf
     if ( orientation(cell,df) == -1 ) write(6,*) cell,df
    end do
  end do    
end subroutine orientation_populate

end module dofmap_mod
