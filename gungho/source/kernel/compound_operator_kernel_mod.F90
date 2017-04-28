!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!> @brief Computes a compound operator A = B*C*D where A,B,C & D are all locally 
!!        assembled operators and B & C are mass matrices
module compound_operator_kernel_mod
use constants_mod,           only: r_def, i_def
use kernel_mod,              only: kernel_type
use argument_mod,            only: arg_type, func_type,            &
                                   GH_OPERATOR,                    &
                                   GH_READ, GH_WRITE,              &
                                   ANY_SPACE_1, ANY_SPACE_2,       &
                                   CELLS
implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(kernel_type) :: compound_operator_kernel_type
  private
  type(arg_type) :: meta_args(4) = (/                                  &
       arg_type(GH_OPERATOR, GH_WRITE, ANY_SPACE_1, ANY_SPACE_2),      &
       arg_type(GH_OPERATOR, GH_READ,  ANY_SPACE_1, ANY_SPACE_1),      &
       arg_type(GH_OPERATOR, GH_READ,  ANY_SPACE_1, ANY_SPACE_1),      &
       arg_type(GH_OPERATOR, GH_READ,  ANY_SPACE_1, ANY_SPACE_2)       &
       /)
  integer :: iterates_over = CELLS

contains
  procedure, nopass :: compound_operator_kernel_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface compound_operator_kernel_type
   module procedure compound_operator_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public compound_operator_kernel_code
contains

type(compound_operator_kernel_type) function compound_operator_kernel_constructor() result(self)
  return
end function compound_operator_kernel_constructor

!> @brief This subroutine computes the div operator 
!! @param[in] cell Cell number
!! @param[in] nlayers Number of layers.
!! @param[in] ncell_3d_1 Ncell*ndf
!! @param[in] ncell_3d_2 Ncell*ndf
!! @param[in] ncell_3d_3 Ncell*ndf
!! @param[in] ncell_3d_4 Ncell*ndf
!! @param[inout] compound_operator LMA operator to create
!! @param[in] mass_matrix1 First mass matrix
!! @param[in] mass_matrix2 Second mass matrix
!! @param[in] differential_matrix Third operator
!! @param[in] ndf1 Number of dofs per cell for space 1
!! @param[in] ndf2 Number of dofs per cell for space 2
subroutine compound_operator_kernel_code(cell, nlayers, &
                                         ncell_3d_1,  &
                                         compound_operator, &
                                         ncell_3d_2,  &
                                         mass_matrix1,  &
                                         ncell_3d_3,  &
                                         mass_matrix2, & 
                                         ncell_3d_4,  &
                                         differential_matrix, &
                                         ndf1, ndf2)
  implicit none
  !Arguments
  integer(kind=i_def),                     intent(in) :: cell
  integer(kind=i_def),                     intent(in) :: nlayers
  integer(kind=i_def),                     intent(in) :: ncell_3d_1, ncell_3d_2, ncell_3d_3, ncell_3d_4
  integer(kind=i_def),                     intent(in) :: ndf1, ndf2

  real(kind=r_def), dimension(ndf1,ndf2,ncell_3d_1), intent(inout) :: compound_operator
  real(kind=r_def), dimension(ndf1,ndf1,ncell_3d_2), intent(inout) :: mass_matrix1
  real(kind=r_def), dimension(ndf1,ndf1,ncell_3d_3), intent(inout) :: mass_matrix2
  real(kind=r_def), dimension(ndf1,ndf2,ncell_3d_4), intent(inout) :: differential_matrix

  !Internal variables
  integer(kind=i_def)                          :: k, ik

  do k = 0, nlayers - 1
    ik = k + 1 + (cell-1)*nlayers
    compound_operator(:,:,ik) = matmul(mass_matrix1(:,:,ik), &
                                       matmul(mass_matrix2(:,:,ik),&
                                              differential_matrix(:,:,ik)))
  end do 
end subroutine compound_operator_kernel_code

end module compound_operator_kernel_mod
