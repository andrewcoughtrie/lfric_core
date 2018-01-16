!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @brief Kernel Computes v.Jv on nodal points for normalising w2 fields
!> @details Compute the normalsation factor fro W2 fields as vJv on W2 nodes
!> For a regular orthogonal grid J = diag(dx,dy,dz) so:
!> vJv = (dx,0,0) for u components
!> vJv = (0,dy,0) for v components
!> vJv = (0,0,dz) for w components
module w2_normalisation_kernel_mod

use argument_mod,            only : arg_type, func_type,           &
                                    GH_FIELD, GH_INC, GH_READ,     &
                                    ANY_SPACE_9, W2,               &
                                    GH_BASIS, GH_DIFF_BASIS,       &
                                    CELLS, GH_EVALUATOR
use constants_mod,           only : r_def, i_def
use kernel_mod,              only : kernel_type

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: w2_normalisation_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                  &
       arg_type(GH_FIELD,   GH_INC,  W2),                              &
       ARG_TYPE(GH_FIELD*3, GH_READ, ANY_SPACE_9)                      &
       /)
  type(func_type) :: meta_funcs(2) = (/                                &
       func_type(W2,          GH_BASIS),                               &
       func_type(ANY_SPACE_9, GH_DIFF_BASIS)                           &
       /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_EVALUATOR
contains
  procedure, public, nopass :: w2_normalisation_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface w2_normalisation_kernel_type
   module procedure w2_normalisation_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

type(w2_normalisation_kernel_type) function w2_normalisation_kernel_constructor() result(self)
  return
end function w2_normalisation_kernel_constructor

!> @brief Compute the normalisation factor for W2 fields as vJv
!! @param[in] nlayers Number of layers
!! @param[inout] normalisation Normalisation field to compute
!! @param[in] chi_1 X component of the coordinate field
!! @param[in] chi_2 Y component of the coordinate field
!! @param[in] chi_3 Z component of the coordinate field
!! @param[in] ndf Number of degrees of freedom per cell
!! @param[in] undf Total number of degrees of freedom
!! @param[in] map Dofmap for the cell at the base of the column
!! @param[in] basis W2 Basis functions evaluated at W2 nodal points
!! @param[in] ndf_chi Number of dofs per cell for the coordinate field
!! @param[in] undf_chi Total number of degrees of freedom
!! @param[in] map_chi Dofmap for the coordinate field
!! @param[in] chi_diff_basis Wchi basis functions evaluated at W2 nodal points
subroutine w2_normalisation_code(nlayers,                &
                                 normalisation,          & 
                                 chi_1, chi_2, chi_3,    &
                                 ndf, undf,              &
                                 map, basis,             &
                                 ndf_chi, undf_chi,      &
                                 map_chi, chi_diff_basis &
                                 )

  use coordinate_jacobian_mod,    only : coordinate_jacobian

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers, ndf, ndf_chi
  integer(kind=i_def), intent(in) :: undf, undf_chi

  integer(kind=i_def), dimension(ndf),     intent(in) :: map
  integer(kind=i_def), dimension(ndf_chi), intent(in) :: map_chi

  real(kind=r_def), intent(in), dimension(3,ndf_chi,ndf) :: chi_diff_basis
  real(kind=r_def), intent(in), dimension(3,ndf,ndf)     :: basis

  real(kind=r_def), dimension(undf),     intent(inout) :: normalisation
  real(kind=r_def), dimension(undf_chi), intent(in)    :: chi_1, chi_2, chi_3

  ! Internal variables
  integer(kind=i_def)                    :: df, k
  real(kind=r_def), dimension(ndf,1)     :: dj
  real(kind=r_def), dimension(3,3,ndf,1) :: jacobian
  real(kind=r_def), dimension(ndf_chi)   :: chi_1_cell, chi_2_cell, chi_3_cell
  real(kind=r_def), dimension(3)         :: Jv
  real(kind=r_def), dimension(3,3)       :: JTJ

  do k = 0, nlayers-1
    do df = 1, ndf_chi
      chi_1_cell(df) = chi_1(map_chi(df) + k)
      chi_2_cell(df) = chi_2(map_chi(df) + k)
      chi_3_cell(df) = chi_3(map_chi(df) + k)
    end do
    call coordinate_jacobian(ndf_chi, &
                             ndf, &
                             1_i_def, &
                             chi_1_cell, &
                             chi_2_cell, &
                             chi_3_cell, &
                             chi_diff_basis, &
                             jacobian, &
                             dj)
    do df = 1,ndf
      JTJ =  matmul(transpose(jacobian(:,:,df,1)),jacobian(:,:,df,1))
      Jv = matmul(JTJ,basis(:,df,df))
      normalisation(map(df)+k) = normalisation(map(df)+k) &
                               + sqrt(dot_product(Jv,basis(:,df,df)))
    end do
  end do

end subroutine w2_normalisation_code

end module w2_normalisation_kernel_mod
