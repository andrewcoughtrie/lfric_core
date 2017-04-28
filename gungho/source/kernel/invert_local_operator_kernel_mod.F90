!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Provides access to the members of the w0_kernel class.

!> @details Accessor functions for the w0_kernel class are defined in this module.

module invert_local_operator_kernel_mod
use constants_mod,           only: r_def
use kernel_mod,              only: kernel_type
use argument_mod,            only: arg_type, func_type,                      &
                                   GH_OPERATOR, GH_FIELD, GH_READ, GH_WRITE, &
                                   W3, &
                                   CELLS
use matrix_invert_mod,       only: matrix_invert
implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
type, public, extends(kernel_type) :: invert_local_operator_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                  &
       arg_type(GH_OPERATOR, GH_WRITE, W3, W3),                        &
       arg_type(GH_OPERATOR, GH_READ,  W3, W3)                         &
       /)
  integer :: iterates_over = CELLS

contains
  procedure, nopass :: invert_local_operator_code
end type invert_local_operator_kernel_type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface invert_local_operator
   module procedure invert_local_operator_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public invert_local_operator_code
contains

type(invert_local_operator_kernel_type) function invert_local_operator_constructor() result(self)
  return
end function invert_local_operator_constructor
  
!> @brief This subroutine computes the mass matrix for the w0 space
!! @param[in] cell cell Number
!! @param[in] nlayers Number of layers.
!! @param[in] ndf Number of degrees of freedom per cell.
!! @param[in] ncell3d ncell*nlayers
!! @param[in] ncell3d_inv ncell*nlayers
!! @param[inout] matrix_inv Inverse matrix
!! @param[in] matrix Input matrix
subroutine invert_local_operator_code(cell, nlayers, ncell3d_inv,       &
                                      matrix_inv, ncell3d, matrix,          &
                                      ndf)

  !Arguments
  integer, intent(in)     :: cell
  integer, intent(in)     :: nlayers, ndf
  integer, intent(in)     :: ncell3d, ncell3d_inv


  real(kind=r_def), dimension(ndf,ndf,ncell3d),     intent(in)     :: matrix
  real(kind=r_def), dimension(ndf,ndf,ncell3d_inv), intent(inout)  :: matrix_inv

  !Internal variables
  integer                                      :: k, ik

  !loop over layers: Start from 1 as in this loop k is not an offset
  do k = 1, nlayers
    ik = k + (cell-1)*nlayers
    call matrix_invert(matrix(:,:,ik),matrix_inv(:,:,ik),ndf)
  end do

end subroutine invert_local_operator_code

end module invert_local_operator_kernel_mod
