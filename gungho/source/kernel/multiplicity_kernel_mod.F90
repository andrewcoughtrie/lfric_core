!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel which computes the multiplicity of the dofs for a field
!> @details Computes how many times each dof in a field is visited when looping
!>          over cells and all dof's asscociated with that cell 
module multiplicity_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_INC,                        &
                                    ANY_SPACE_1,                             &
                                    CELLS
use constants_mod,           only : r_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: multiplicity_kernel_type
  private
  type(arg_type) :: meta_args(1) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  ANY_SPACE_1)                      &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::multiplicity_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface multiplicity_kernel_type
  module procedure multiplicity_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public multiplicity_code
contains

type(multiplicity_kernel_type) function multiplicity_kernel_constructor() result(self)
  implicit none
  return
end function multiplicity_kernel_constructor

!> @brief Compute the multiplicity of a field (number of cells each dof is shared by)
!! @param[in] nlayers Number of layers
!! @param[in] ndf Number of degrees of freedom per cell for the function space
!! @param[in] undf Number unique of degrees of freedom  for the function space
!! @param[in] map Dofmap for the cell at the base of the column for the function space
!! @param[inout] field Input/output field

subroutine multiplicity_code(nlayers,                        &
                             field,                          &
                             ndf, undf, map                  &
                            )

  implicit none

  !Arguments
  integer, intent(in) :: nlayers
  integer, intent(in) :: ndf
  integer, intent(in) :: undf
  integer, dimension(ndf),   intent(in) :: map

  real(kind=r_def), dimension(undf), intent(inout) :: field
 
  integer :: k, df
  
  do k = 0, nlayers - 1
    do df = 1,ndf
      field(map(df) + k) = field(map(df) + k) + 1.0_r_def
    end do
  end do

end subroutine multiplicity_code

end module multiplicity_kernel_mod
