!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel to compute the fractional level a dof lives on. This is given
!! by the layer index (k) + the nodal coordinate (hat{chi})
module compute_dof_level_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_WRITE,                      &
                                    ANY_SPACE_1, GH_BASIS,                   &
                                    CELLS
use constants_mod,           only : r_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: compute_dof_level_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/                                  &
       arg_type(GH_FIELD,    GH_WRITE, ANY_SPACE_1)                    &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::compute_dof_level_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface compute_dof_level_kernel_type
   module procedure compute_dof_level_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public compute_dof_level_code
contains

type(compute_dof_level_kernel_type) function compute_dof_level_kernel_constructor() result(self)
  return
end function compute_dof_level_kernel_constructor

!> @param[in] nlayers Number of layers
!> @param[in] ndf Number of degrees of freedom per cell for the output field
!> @param[in] undf Number of unique degrees of freedom for the output field
!> @param[in] map Dofmap for the cell at the base of the column for the output field
!> @param[out] level Fractional level of the dof's
!> @param[in] nodes Nodal coordinates of the dofs
subroutine compute_dof_level_code(nlayers,                                  &
                                  level,                                    &
                                  ndf, undf, map,                           &
                                  nodes                                     &
                                  )
                           
  !Arguments
  integer, intent(in)                             :: nlayers
  integer, intent(in)                             :: ndf
  integer, intent(in)                             :: undf
  integer, dimension(ndf),            intent(in)  :: map
  real(kind=r_def), dimension(undf),  intent(out) :: level
  real(kind=r_def), dimension(3,ndf), intent(in)  :: nodes

  !Internal variables
  integer          :: df, k

  do k = 0,nlayers - 1
    do df = 1,ndf
      level(map(df) + k) = real(k,r_def) + nodes(3,df)
    end do
  end do

end subroutine compute_dof_level_code

end module compute_dof_level_kernel_mod
