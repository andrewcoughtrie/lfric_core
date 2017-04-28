!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
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
  return
end function multiplicity_kernel_constructor

!> @brief ompute the mulitplicity of a field (number of cells each dof is shared by)
!! @param[in] nlayers Number of layers
!! @param[in] ndf Number of degrees of freedom per cell for the function space
!! @param[in] undf Number unique of degrees of freedom  for the function space
!! @param[in] map Dofmap for the cell at the base of the column for the function space
!! @param[inout] field Input/ouput field

subroutine multiplicity_code(nlayers,                        &
                             field,                          &
                             ndf, undf, map                  &
                            )

  
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
