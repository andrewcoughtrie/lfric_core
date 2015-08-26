!------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!> @brief A type which holds information about the dofmap.
!> @details Types for storing the general stencil dofmaps of a general size
!> Allowable stencil types ( the size is variable ):
!> Currently on the point stencil is allowed 
!> 
!> POINT --> [1]
!>   
!> 1DX --> |4|2|1|3|5|
!>   
!>         |5|
!>         |3|
!> 1DY --> |1|
!>         |2|
!>         |4|      
!>
!>           |5|
!> CROSS-> |2|1|4|
!>           |3|
!>
module stencil_dofmap_mod

use constants_mod,     only: i_def
use master_dofmap_mod, only: master_dofmap_type
implicit none

private
type, public :: stencil_dofmap_type
  private 
  integer :: dofmap_shape
  integer :: dofmap_size
  integer :: dofmap_id
  integer, allocatable :: dofmap(:,:,:) 
contains
  procedure :: get_dofmap
  procedure :: get_id
end type stencil_dofmap_type

integer, public, parameter :: STENCIL_POINT = 1100
integer, public, parameter :: STENCIL_1DX   = 1200
integer, public, parameter :: STENCIL_1DY   = 1300
integer, public, parameter :: STENCIL_CROSS = 1400

interface stencil_dofmap_type
  module procedure stencil_dofmap_constructor
end interface

contains 

!-----------------------------------------------------------------------------
! Construct the stencil dofmap
!-----------------------------------------------------------------------------
!> Function to construct a stencil dofmap
!> @param[in] st_shape The shape of the required stencil
!> @param[in] st_size The number of cells in the stencil
!> @param[in] master_dofmap The cell dofmap to create the stencil from
!> @return The dofmap object
function stencil_dofmap_constructor( st_shape, st_size, ndf, mesh, master_dofmap) result(self)

    use log_mod,  only: log_event,         &
                        log_scratch_space, &
                        LOG_LEVEL_ERROR
    use mesh_mod, only: mesh_type

    integer,                  intent(in) :: st_shape, st_size, ndf
    type(mesh_type),          intent(in) :: mesh
    type(master_dofmap_type), intent(in) :: master_dofmap
    type(stencil_dofmap_type), target    :: self

    integer :: cell, ncells
    integer, pointer :: map(:) => null()

    self%dofmap_shape = st_shape
    self%dofmap_size  = st_size
    self%dofmap_id    = st_shape*100 + st_size

    ncells = mesh%get_ncells()
    ! Allocate the dofmap array
    allocate( self%dofmap( ndf, st_size, ncells ) )
    ! Compute the dofmap
    select case ( st_shape ) 
      case ( STENCIL_POINT )
        do cell = 1,ncells
          map => master_dofmap%get_master_dofmap(cell)
          self%dofmap(:,1,cell) = map(:)
        end do
      case default
        write( log_scratch_space, '( A, I4 )' ) &
           "Invalid stencil type: ", st_shape
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end select 

  end function stencil_dofmap_constructor

!-----------------------------------------------------------------------------
! Get the stencil dofmap for a single cell
!-----------------------------------------------------------------------------
!> Subroutine Returns a pointer to the dofmap for the cell 
!! @param[in] self The calling function_space
!! @param[in] cell Which cell
!! @return The pointer which points to a slice of the dofmap
function get_dofmap(self,cell) result(map)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self
  integer,                            intent(in) :: cell
  integer, pointer                               :: map(:,:) 

  map => self%dofmap(:,:,cell)
  return
end function get_dofmap

!-----------------------------------------------------------------------------
! Get the dofmap id
!-----------------------------------------------------------------------------
!> Subroutine Returns the unique id of the dofmap
!! @param[in] self The calling function_space
!! @return The dofmap integer id
integer function get_id(self)
  implicit none
  class(stencil_dofmap_type), target, intent(in) :: self

  get_id = self%dofmap_id
  return
end function get_id

end module stencil_dofmap_mod

