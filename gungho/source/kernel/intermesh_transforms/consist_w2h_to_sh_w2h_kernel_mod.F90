!-----------------------------------------------------------------------------
! (c) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Maps a field from W2h to shifted W2h.
!> @details Calculates a shifted W2h field from a W2h field. The new horizontal
!!          fluxes take the average from each of the half-levels from the
!!          original field. In conjunction with the mapping from W3 to
!!          shifted W3 this provides a consistent mapping of fluxes and
!!          densities to the shifted mesh.
!!          This kernel only works for the lowest-order elements.
module consist_w2h_to_sh_w2h_kernel_mod

  use argument_mod,          only : arg_type,                  &
                                    GH_FIELD, GH_REAL,         &
                                    GH_READ, GH_INC,           &
                                    ANY_SPACE_2, CELL_COLUMN
  use constants_mod,         only : r_double, i_def, r_single, r_def
  use fs_continuity_mod,     only : W2h
  use kernel_mod,            only : kernel_type
  use reference_element_mod, only : N, E, S, W

  implicit none

  private

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  !> The type declaration for the kernel. Contains the metadata needed by the
  !! Psy layer.
  !!
  type, public, extends(kernel_type) :: consist_w2h_to_sh_w2h_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                                   &
         arg_type(GH_FIELD, GH_REAL, GH_INC,  ANY_SPACE_2),               &
         arg_type(GH_FIELD, GH_REAL, GH_READ, W2h),                       &
         arg_type(GH_FIELD, GH_REAL, GH_READ, W2h)                        &
         /)
    integer :: operates_on = CELL_COLUMN
  end type

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public :: consist_w2h_to_sh_w2h_code

  ! Generic interface for real32 and real64 types
  interface consist_w2h_to_sh_w2h_code
    module procedure  &
      consist_w2h_to_sh_w2h_code_r_single, &
      consist_w2h_to_sh_w2h_code_r_double
  end interface

contains

!> @brief Maps a field from W2h to the W2h shifted space.
!> @param[in] nlayers_sh Number of layers in the shifted mesh
!> @param[in,out] field_w2h_sh Field in the shifted W2h space to be returned.
!> @param[in] field_w2h Original field in W2h to be used.
!> @param[in] rmultiplicity_w2h Reciprocal of nodal multiplicity field for W2h
!> @param[in] ndf_w2h_sh Number of degrees of freedom per cell for W2h shifted
!> @param[in] undf_w2h_sh Number of (local) unique degrees of freedom for W2h shifted
!> @param[in] map_w2h_sh Dofmap for the cell at the base of the column for W2h shifted
!> @param[in] ndf_w2h Number of degrees of freedom per cell for W2h
!> @param[in] undf_w2h Number of (local) unique degrees of freedom for W2h
!> @param[in] map_w2h Dofmap for the cell at the base of the column for W2h

! R_SINGLE PRECISION
! ==================
subroutine consist_w2h_to_sh_w2h_code_r_single( nlayers_sh,        &
                                                field_w2h_sh,      &
                                                field_w2h,         &
                                                rmultiplicity_w2h, &
                                                ndf_w2h_sh,        &
                                                undf_w2h_sh,       &
                                                map_w2h_sh,        &
                                                ndf_w2h,           &
                                                undf_w2h,          &
                                                map_w2h            &
                                                )

  implicit none

  ! Arguments
  integer(kind=i_def),                            intent(in) :: nlayers_sh
  integer(kind=i_def),                            intent(in) :: ndf_w2h_sh, ndf_w2h
  integer(kind=i_def),                            intent(in) :: undf_w2h_sh, undf_w2h
  integer(kind=i_def), dimension(ndf_w2h_sh),     intent(in) :: map_w2h_sh
  integer(kind=i_def), dimension(ndf_w2h),        intent(in) :: map_w2h

  real(kind=r_single), dimension(undf_w2h_sh), intent(inout) :: field_w2h_sh
  real(kind=r_single), dimension(undf_w2h),       intent(in) :: field_w2h
  real(kind=r_def),    dimension(undf_w2h),       intent(in) :: rmultiplicity_w2h

  ! Internal variables
  integer(kind=i_def) :: df, k, j
  integer(kind=i_def) :: horizontal_dofs(4)


  ! We don't want to do operations twice for DoFs that are shared between cells
  ! It would be good to find a way to avoid duplicating this calculation!
  horizontal_dofs = (/ N, E, S, W /)

  ! Loop over layers of original mesh
  do k = 0, nlayers_sh - 2

    ! Loop over horizontal W2 DoFs
    ! Fluxes from the original mesh cells each contribute 1/2 to shifted fluxes
    do j = 1, size(horizontal_dofs)
      df = horizontal_dofs(j)

      field_w2h_sh(map_w2h_sh(df)+k) = field_w2h_sh(map_w2h_sh(df)+k)          &
        + 0.5_r_single*real( rmultiplicity_w2h(map_w2h(df)+k)*field_w2h(map_w2h(df)+k), r_single )

      field_w2h_sh(map_w2h_sh(df)+k+1) = field_w2h_sh(map_w2h_sh(df)+k+1)      &
        + 0.5_r_single*real( rmultiplicity_w2h(map_w2h(df)+k)*field_w2h(map_w2h(df)+k), r_single )

    end do
  end do

end subroutine consist_w2h_to_sh_w2h_code_r_single

! R_DOUBLE PRECISION
! ==================
subroutine consist_w2h_to_sh_w2h_code_r_double( nlayers_sh,        &
                                                field_w2h_sh,      &
                                                field_w2h,         &
                                                rmultiplicity_w2h, &
                                                ndf_w2h_sh,        &
                                                undf_w2h_sh,       &
                                                map_w2h_sh,        &
                                                ndf_w2h,           &
                                                undf_w2h,          &
                                                map_w2h            &
                                                )

  implicit none

  ! Arguments
  integer(kind=i_def),                            intent(in) :: nlayers_sh
  integer(kind=i_def),                            intent(in) :: ndf_w2h_sh, ndf_w2h
  integer(kind=i_def),                            intent(in) :: undf_w2h_sh, undf_w2h
  integer(kind=i_def), dimension(ndf_w2h_sh),     intent(in) :: map_w2h_sh
  integer(kind=i_def), dimension(ndf_w2h),        intent(in) :: map_w2h

  real(kind=r_double),   dimension(undf_w2h_sh), intent(inout) :: field_w2h_sh
  real(kind=r_double),   dimension(undf_w2h),       intent(in) :: field_w2h
  real(kind=r_def),    dimension(undf_w2h),       intent(in) :: rmultiplicity_w2h

  ! Internal variables
  integer(kind=i_def) :: df, k, j
  integer(kind=i_def) :: horizontal_dofs(4)


  ! We don't want to do operations twice for DoFs that are shared between cells
  ! It would be good to find a way to avoid duplicating this calculation!
  horizontal_dofs = (/ N, E, S, W /)

  ! Loop over layers of original mesh
  do k = 0, nlayers_sh - 2

    ! Loop over horizontal W2 DoFs
    ! Fluxes from the original mesh cells each contribute 1/2 to shifted fluxes
    do j = 1, size(horizontal_dofs)
      df = horizontal_dofs(j)

      field_w2h_sh(map_w2h_sh(df)+k) = field_w2h_sh(map_w2h_sh(df)+k)          &
        + 0.5_r_double*real( rmultiplicity_w2h(map_w2h(df)+k)*field_w2h(map_w2h(df)+k), r_double )

      field_w2h_sh(map_w2h_sh(df)+k+1) = field_w2h_sh(map_w2h_sh(df)+k+1)      &
        + 0.5_r_double*real( rmultiplicity_w2h(map_w2h(df)+k)*field_w2h(map_w2h(df)+k), r_double )

    end do
  end do

end subroutine consist_w2h_to_sh_w2h_code_r_double

end module consist_w2h_to_sh_w2h_kernel_mod
