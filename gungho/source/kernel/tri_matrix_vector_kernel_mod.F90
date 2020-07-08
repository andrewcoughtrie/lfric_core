!-----------------------------------------------------------------------------
! (c) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Multiplies a vector by a tridiagonal matrix.
!> @details Takes the non-zero elements of a tridiagonal matrix T and multiples
!> a vector x, returning a vector y:
!> \f[ T * x = y \f]
!> Intended for transforming from Wtheta to the shifted W3 space
!>
module tri_matrix_vector_kernel_mod

  use argument_mod,      only : arg_type, func_type,                 &
                                GH_FIELD, GH_WRITE, GH_READ,         &
                                ANY_DISCONTINUOUS_SPACE_3,           &
                                CELLS
  use constants_mod,     only : r_def, i_def
  use fs_continuity_mod, only : Wtheta
  use kernel_mod,        only : kernel_type

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !> PSy layer.
  !>
  type, public, extends(kernel_type) :: tri_matrix_vector_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                             &
         arg_type(GH_FIELD,   GH_WRITE, ANY_DISCONTINUOUS_SPACE_3), &
         arg_type(GH_FIELD*3,  GH_READ, ANY_DISCONTINUOUS_SPACE_3), &
         arg_type(GH_FIELD,    GH_READ,  Wtheta)                    &
         /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass :: tri_matrix_vector_code
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public tri_matrix_vector_code
contains

!> @brief Compute the terms of the tridiagonal matrix for transforming from
!> mixing ratio in Wtheta to density in shifted W3.
!! @param[in] nlayers_shifted Number of layers in the shifted mesh
!! @param[out] field_sh_w3 The output field in W3 shifted.
!! @param[in] tri_below The below-diagonal elements of the tridiagonal matrix.
!! It is a field in shifted W3.
!! @param[in] tri_diag The central diagonal elements of the tridiagonal matrix.
!! It is a field in shifted W3.
!! @param[in] tri_above The above-diagonal elements of the tridiagonal matrix.
!! It is a field in shifted W3.
!! @param[in] field_wt The input field in Wtheta
!! @param[in] ndf_sh_w3 The number of degrees of freedom per cell for shifted w3
!! @param[in] undf_sh_w3 The number of unique degrees of freedom for shifted w3
!! @param[in] map_sh_w3 Dofmap for the cell at the base of the column for shifted w3
!! @param[in] ndf_wt The number of degrees of freedom per cell for wtheta
!! @param[in] undf_wt The number of unique degrees of freedom for wtheta
!! @param[in] map_wt Dofmap for the cell at the base of the column for wtheta

subroutine tri_matrix_vector_code(                                   &
                                   nlayers_shifted,                  &
                                   field_sh_w3,                      &
                                   tri_below,                        &
                                   tri_diag,                         &
                                   tri_above,                        &
                                   field_wt,                         &
                                   ndf_sh_w3, undf_sh_w3, map_sh_w3, &
                                   ndf_wt, undf_wt, map_wt           &
                                  )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in) :: nlayers_shifted
  integer(kind=i_def), intent(in) :: ndf_wt, ndf_sh_w3
  integer(kind=i_def), intent(in) :: undf_wt, undf_sh_w3
  integer(kind=i_def), dimension(ndf_wt),    intent(in) :: map_wt
  integer(kind=i_def), dimension(ndf_sh_w3), intent(in) :: map_sh_w3

  real(kind=r_def), dimension(undf_sh_w3),  intent(out) :: field_sh_w3
  real(kind=r_def), dimension(undf_sh_w3),   intent(in) :: tri_below
  real(kind=r_def), dimension(undf_sh_w3),   intent(in) :: tri_diag
  real(kind=r_def), dimension(undf_sh_w3),   intent(in) :: tri_above
  real(kind=r_def), dimension(undf_wt),      intent(in) :: field_wt

  ! Internal variables
  integer(kind=i_def) :: df, k

  ! Top and bottom layers (bottom is k=0)
  k = nlayers_shifted - 1
  do df = 1, ndf_sh_w3
    field_sh_w3(map_sh_w3(df)) = tri_above(map_sh_w3(df)) * field_wt(map_wt(df)+1) + &
                                 tri_diag(map_sh_w3(df)) * field_wt(map_wt(df))

    field_sh_w3(map_sh_w3(df)+k) = tri_below(map_sh_w3(df)+k) * field_wt(map_wt(df)+k-1) + &
                                   tri_diag(map_sh_w3(df)+k) * field_wt(map_wt(df)+k)
  end do

  do k = 1, nlayers_shifted-2
    do df = 1, ndf_sh_w3
      field_sh_w3(map_sh_w3(df)+k) = tri_below(map_sh_w3(df)+k) * field_wt(map_wt(df)+k-1) + &
                                     tri_diag(map_sh_w3(df)+k) * field_wt(map_wt(df)+k)    + &
                                     tri_above(map_sh_w3(df)+k) * field_wt(map_wt(df)+k+1)
    end do
  end do

end subroutine tri_matrix_vector_code

end module tri_matrix_vector_kernel_mod