!-----------------------------------------------------------------------------
! (c) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Scale a field by 1/detj.
!>
!> @details Scales a horizontally discontinuous field by 1/det(J) sampled
!>          at the scalar nodal points, effectively this scales the field
!>          by 1/volume of the cell.
module scale_by_detj_kernel_mod

  use argument_mod,      only : arg_type, func_type,       &
                                GH_FIELD, GH_READ, GH_INC, &
                                ANY_SPACE_1, ANY_SPACE_9,  &
                                GH_BASIS, GH_DIFF_BASIS,   &
                                CELLS, GH_EVALUATOR
  use constants_mod,     only : r_def, i_def
  use kernel_mod,        only : kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> PSy layer.
  !>@todo The metadata will become ANY_DISCONTINOUS_SPACE when #1968 is on
  !>      trunk. Additionally GH_INC will become GH_READWRITE
  type, public, extends(kernel_type) :: scale_by_detj_kernel_type
    private
    type(arg_type) :: meta_args(2) = (/             &
        arg_type(GH_FIELD,   GH_INC,  ANY_SPACE_1), &
        arg_type(GH_FIELD*3, GH_READ, ANY_SPACE_9)  &
        /)
    type(func_type) :: meta_funcs(1) = (/      &
        func_type(ANY_SPACE_9, GH_DIFF_BASIS)  &
        /)
    integer :: iterates_over = CELLS
    integer :: gh_shape = GH_EVALUATOR
  contains
    procedure, nopass :: scale_by_detj_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public scale_by_detj_code

contains

!> @brief Scales a field by 1/det J
!! @param[in] nlayers Number of layers
!! @param[in,out] field Horizontally discontinuous field to scale
!! @param[in] chi1 First component of the coordinate field
!! @param[in] chi2 Second component of the coordinate field
!! @param[in] chi3 Third component of the coordinate field
!! @param[in] ndf_ws Number of degrees of freedom per cell for field
!! @param[in] undf_ws Number of unique degrees of freedom for field
!! @param[in] map_ws Dofmap for the cell at the base of the column for field
!! @param[in] ndf_wx Number of degrees of freedom per cell for the coordinates
!! @param[in] undf_wx Number of unique degrees of freedom for the coordinates
!! @param[in] map_wx Dofmap for the cell at the base of the column for the coordinates
!! @param[in] diff_basis_wx Differential basis functions for the coordinate fields evaluated 
!!            at the nodes of field
subroutine scale_by_detj_code(nlayers,                 &
                              field,                   &
                              chi1, chi2, chi3,        &
                              ndf_ws, undf_ws, map_ws, &
                              ndf_wx, undf_wx, map_wx, &
                              diff_basis_wx            &
                             )

  use coordinate_jacobian_mod, only: pointwise_coordinate_jacobian

  implicit none
                           
  ! Arguments
  integer(kind=i_def),                    intent(in) :: nlayers
  integer(kind=i_def),                    intent(in) :: ndf_ws, undf_ws, &
                                                        ndf_wx, undf_wx
  integer(kind=i_def), dimension(ndf_ws), intent(in) :: map_ws
  integer(kind=i_def), dimension(ndf_wx), intent(in) :: map_wx

  real(kind=r_def), dimension(3,ndf_wx,ndf_ws), intent(in)    :: diff_basis_wx
  real(kind=r_def), dimension(undf_ws),         intent(inout) :: field
  real(kind=r_def), dimension(undf_wx),         intent(in)    :: chi1, chi2, chi3

  ! Internal variables
  integer(kind=i_def)                 :: df, k, loc

  real(kind=r_def), dimension(ndf_wx)    :: chi1_e, chi2_e, chi3_e
  real(kind=r_def), dimension(3,3)       :: jac
  real(kind=r_def)                       :: detj
  real(kind=r_def), dimension(0:nlayers) :: detj_av, f_av
 
  detj_av = 0.0_r_def
  f_av  = 0.0_r_def
  do k = 0, nlayers-1
    do df = 1, ndf_wx
      chi1_e(df) = chi1(map_wx(df) + k)
      chi2_e(df) = chi2(map_wx(df) + k)
      chi3_e(df) = chi3(map_wx(df) + k)
    end do
     
    do df = 1,ndf_ws
      ! Compute detj at dof points 
      call pointwise_coordinate_jacobian(ndf_wx, chi1_e, chi2_e, chi3_e,  &
                                         diff_basis_wx(:,:,df), &
                                         jac, detj)
      detj_av(k + df-1) = detj_av(k + df-1) + detj
      f_av(k + df-1)    = f_av(k + df-1) +  field(map_ws(df)+k) 
    end do
  end do

  do k = 0,nlayers-1+ (ndf_ws-1)
    field(map_ws(1)+k) = f_av(k)/detj_av(k)
  end do

end subroutine scale_by_detj_code

end module scale_by_detj_kernel_mod
