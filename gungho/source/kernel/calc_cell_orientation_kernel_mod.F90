!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel computes the orientations of halo cells in a partition.

!> @detail The kernel computes the orientation of core and halo cells in a 
!>         partition. The kernel assumes that the number of mpi partitions is a
!>         multiple of 6 which implies that no core cells of a partition overlap
!>         a cubed-sphere panel edge. Thus it is assumed that all core cells
!>         have the same orientation which we assign orientation 1. The kernel
!>         computes the orientation of cells using the W2 dof values and the 
!>         orientation can take the value 1, 2, 3 or 4.

module calc_cell_orientation_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_WRITE,                      &
                                    W3, GH_BASIS, CELLS
use constants_mod,           only : r_def, i_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: calc_cell_orientation_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/                                  &
       arg_type(GH_FIELD, GH_WRITE, W3)                                &
       /)
  type(func_type) :: meta_funcs(1) = (/                                &
       func_type(W3, GH_BASIS)                                         &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::calc_cell_orientation_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface calc_cell_orientation_kernel_type
   module procedure calc_cell_orientation_kernel_constructor
end interface

public calc_cell_orientation_code

contains

type(calc_cell_orientation_kernel_type) function calc_cell_orientation_kernel_constructor() result(self)
  return
end function calc_cell_orientation_kernel_constructor

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
!>  @brief  Function which returns the index depending on orientation of cell
!!
!!  @param[in]      branch                    branch of the cross-stencil
!!  @param[in]      orientation               orientation of cell
!!  @param[return]  index_of_interest_map     returns index
!--------------------------------------------------------------------------------
function index_of_interest_map(branch, orientation)

integer :: index_of_interest_map, branch, orientation

  index_of_interest_map = mod(orientation + 2 + branch, 4) + 1

end function index_of_interest_map

!--------------------------------------------------------------------------------
!>  @brief  Function which returns the cells orientation
!!
!!  @param[in]      ii        relates to the W2 dof
!!  @param[in]      branch    relates to the branch of the cross-stencil
!!  @param[return]  orientation_of_cell    orientation of the cell
!--------------------------------------------------------------------------------
function orientation_of_cell(branch,ii)

  integer :: orientation_of_cell, branch, ii

  orientation_of_cell = real(mod(ii + 6 - branch, 4) + 1,r_def)

end function orientation_of_cell

!--------------------------------------------------------------------------------
!>  @brief  Subroutine which calculates the orientation of cells
!!
!> @details The kernel computes the orientation of cells which is dependent on
!>          the numbering of the local W2 dof values.
!>          The kernel iterates over all core cells in a partition and uses a
!>          cross-stencil inorder to calculate the orientation of cells in the halo.
!>          The orientation of cells in the cross-stencil is calculated by
!>          iterating over the 4 cells closest to the centre cell, then the
!>          cells a distance of 2 away from the centre cell and so on.
!!
!! @param[in] nlayers              Number of layers
!! @param[inout] orientation       W3 field containing orientation of cells
!! @param[in] undf_w3              Number of unique degrees of freedom
!! @param[in] ndf_w3               Number of degrees of freedom per cell
!! @param[in] map_w3               Dofmap for the cell at the base of the column
!! @param[in] ndf_w2               Number of degrees of freedom per cell
!! @param[in] size_of_stencil      Size of cross-stencil
!! @param[in] stencil_map_w2_cross Dofmap for the stencil
!! @param[in] stencil_map_w3_cross Dofmap for the stencil
!--------------------------------------------------------------------------------
subroutine calc_cell_orientation_code(  nlayers,                       &
                                        orientation,                   &
                                        undf_w3,                       &
                                        ndf_w3,                        &
                                        map_w3,                        &
                                        ndf_w2,                        &
                                        size_of_stencil,               &
                                        stencil_map_w2_cross,          &
                                        stencil_map_w3_cross )

  use log_mod, only : log_event, LOG_LEVEL_ERROR

  implicit none

  integer, intent(in)                     :: nlayers
  integer, intent(in)                     :: undf_w3
  integer, intent(in)                     :: ndf_w3
  real(kind=r_def), intent(inout)         :: orientation(1:undf_w3)
  integer, dimension(ndf_w3), intent(in)  :: map_w3
  integer, intent(in)                     :: ndf_w2
  integer, intent(in)                     :: size_of_stencil
  integer, intent(in)                     :: stencil_map_w2_cross(1:ndf_w2,1:size_of_stencil)
  integer, intent(in)                     :: stencil_map_w3_cross(1:ndf_w3,1:size_of_stencil)

  integer :: k, ii, branch, stencil ! iteration counters
  integer :: cell_inner, cell_outer
  integer :: cell_inner_index_of_interest
  integer :: shared_dof_of_interest
  integer :: halo_depth, cross_stencil_branches

  halo_depth = (size_of_stencil-1)/4
  cross_stencil_branches = 4

  do k=0, nlayers-1

    ! Kernel loops over core cells only, all core cells are assumed to have orientation 1
    orientation(stencil_map_w3_cross(1,1)) = 1_r_def

    do stencil=1,halo_depth
      do branch=1,cross_stencil_branches ! loop over stencil cross branches

        cell_inner = max(int(1),int(4*stencil-7+int(branch)))
        cell_outer = int(4*(stencil-1)+branch+1,i_def)

        if ( orientation(stencil_map_w3_cross(1,cell_inner)+k) < 0.5_r_def ) then
          call log_event( " Orientation unassigned ", LOG_LEVEL_ERROR )
        end if

        cell_inner_index_of_interest = index_of_interest_map(branch,int(orientation(stencil_map_w3_cross(1,cell_inner)+k)))
        shared_dof_of_interest = int(stencil_map_w2_cross(cell_inner_index_of_interest,cell_inner), i_def)

        do ii=1,ndf_w2
          if ( shared_dof_of_interest == int(stencil_map_w2_cross(ii,cell_outer),i_def) ) then
            orientation(stencil_map_w3_cross(1,cell_outer)+k) = orientation_of_cell(branch,ii)
          end if
        end do

      end do
    end do
  end do

end subroutine calc_cell_orientation_code

end module calc_cell_orientation_kernel_mod
