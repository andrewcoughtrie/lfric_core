!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which applies boundary conditions to a field
!> @details Code for applying constant boundary conditions to a field
module set_boundary_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_REAL, GH_INC, GH_READ,      &
                                    ANY_SPACE_1,                             &
                                    CELLS
use constants_mod,           only : r_def, i_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: set_boundary_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  ANY_SPACE_1),                     &
       arg_type(GH_REAL,    GH_READ)                                   &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::set_boundary_code
end type

!-------------------------------------------------------------------------------
! Constset_boundaryctors
!-------------------------------------------------------------------------------

! overload the default stset_boundarycture constset_boundaryctor for function space
interface set_boundary_kernel_type
   module procedure set_boundary_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public set_boundary_code
contains

type(set_boundary_kernel_type) function set_boundary_kernel_constructor() result(self)
  return
end function set_boundary_kernel_constructor

!> @brief right some values to boundary terms of a field
!! @param[in] nlayers Number of layers
!! @param[inout] field fireld to write data to
!! @param[in] ndf Number of degrees of freedom per cell
!! @param[in] undf Number unique of degrees of freedom
!! @param[in] map Dofmap for the cell at the base of the column
!! @param[in] boundary_flag Flags (= 0) for dofs that live on the
!!            vertical boundaries of the cell (=1 for other dofs)
!! @param[in] boundary_value Value to set field to on vertical boundaries
subroutine set_boundary_code(nlayers,                        &
                             field,                          &
                             ndf, undf, map, boundary_flag,  &
                             boundary_value                  &
                            )
  
  !Arguments
  integer, intent(in) :: nlayers
  integer, intent(in) :: ndf
  integer, intent(in) :: undf
  integer, dimension(ndf),   intent(in) :: map
  integer, dimension(ndf,2), intent(in) :: boundary_flag

  real(kind=r_def), dimension(undf), intent(inout) :: field
  real(kind=r_def),                  intent(in)    :: boundary_value
  ! Local variables 
  integer :: df, k

  k = 0
  do df = 1,ndf
    field(map(df) + k) = field(map(df) + k)*real(boundary_flag(df,1)) &
                         + real(1_i_def - boundary_flag(df,1))*boundary_value
  end do
  k = nlayers - 1  
  do df = 1,ndf
    field(map(df) + k) = field(map(df) + k)*real(boundary_flag(df,2)) &
                         + real(1_i_def - boundary_flag(df,2))*boundary_value
  end do

end subroutine set_boundary_code

end module set_boundary_kernel_mod
