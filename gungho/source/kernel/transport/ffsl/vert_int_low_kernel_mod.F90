!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Kernel which computes the integer and low order mass flux for FFSL
!!        in the z direction.
!> @details This kernel reconstructs a field using the Piecewise Constant Method (PCM),
!!          and then computes the low order fractional mass flux as
!!          flux = fractional_wind * reconstruction
!!          The PCM reconstruction is equivalent to a first order upwind scheme. The
!!          kernel also outputs the integer part of the mass flux.
!!          This kernel is designed to work in the vertical direction only.
!!
!!          Note that this kernel only works when field is a W3 field at lowest order
!!          since it is assumed that ndf_w3 = 1 with stencil_map(1,:) containing
!!          the relevant dofmaps.

module vert_int_low_kernel_mod

use argument_mod,       only : arg_type,              &
                               GH_FIELD, GH_REAL,     &
                               GH_READ, GH_WRITE,     &
                               GH_SCALAR, CELL_COLUMN
use fs_continuity_mod,  only : W3, W2v
use constants_mod,      only : r_tran, i_def, EPS_R_TRAN
use kernel_mod,         only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: vert_int_low_kernel_type
  private
  type(arg_type) :: meta_args(8) = (/                  &
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, W2v), & ! flux_low
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, W2v), & ! flux_int
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2v), & ! frac_wind
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2v), & ! dep_pts
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2v), & ! detj
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3),  & ! field
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3),  & ! dz
       arg_type(GH_SCALAR, GH_REAL,    GH_READ)        & ! dt
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: vert_int_low_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: vert_int_low_code

contains

!> @brief Compute the flux using the PCM reconstruction.
!> @param[in]     nlayers   Number of layers
!> @param[in,out] flux_low  The fractional low order mass flux
!> @param[in,out] flux_int  The integer mass flux
!> @param[in]     frac_wind The fractional vertical wind
!> @param[in]     dep_pts   The vertical departure points
!> @param[in]     detj      Det(J) at vertical W2v points
!> @param[in]     field     The field to construct the flux
!> @param[in]     dz        Vertical length of the W3 cell
!> @param[in]     ndf_w2v   Number of degrees of freedom for W2v per cell
!> @param[in]     undf_w2v  Number of unique degrees of freedom for W2v
!> @param[in]     map_w2v   The dofmap for the W2v cell at the base of the column
!> @param[in]     ndf_w3    Number of degrees of freedom for W3 per cell
!> @param[in]     undf_w3   Number of unique degrees of freedom for W3
!> @param[in]     map_w3    The dofmap for the cell at the base of the column
subroutine vert_int_low_code( nlayers,   &
                              flux_low,  &
                              flux_int,  &
                              frac_wind, &
                              dep_pts,   &
                              detj,      &
                              field,     &
                              dz,        &
                              dt,        &
                              ndf_w2v,   &
                              undf_w2v,  &
                              map_w2v,   &
                              ndf_w3,    &
                              undf_w3,   &
                              map_w3 )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)    :: nlayers
  integer(kind=i_def), intent(in)    :: undf_w2v
  integer(kind=i_def), intent(in)    :: ndf_w2v
  integer(kind=i_def), intent(in)    :: undf_w3
  integer(kind=i_def), intent(in)    :: ndf_w3
  real(kind=r_tran),   intent(inout) :: flux_low(undf_w2v)
  real(kind=r_tran),   intent(inout) :: flux_int(undf_w2v)
  real(kind=r_tran),   intent(in)    :: field(undf_w3)
  real(kind=r_tran),   intent(in)    :: frac_wind(undf_w2v)
  real(kind=r_tran),   intent(in)    :: dep_pts(undf_w2v)
  real(kind=r_tran),   intent(in)    :: detj(undf_w2v)
  real(kind=r_tran),   intent(in)    :: dz(undf_w3)
  integer(kind=i_def), intent(in)    :: map_w3(ndf_w3)
  integer(kind=i_def), intent(in)    :: map_w2v(ndf_w2v)
  real(kind=r_tran),   intent(in)    :: dt

  ! Internal variables
  integer(kind=i_def) :: k
  integer(kind=i_def) :: ii
  integer(kind=i_def) :: n_cells_to_sum
  integer(kind=i_def) :: df, nz, nc

  real(kind=r_tran)   :: departure_dist
  real(kind=r_tran)   :: fractional_distance
  real(kind=r_tran)   :: reconstruction_low
  real(kind=r_tran)   :: mass_from_whole_cells
  real(kind=r_tran)   :: field_1d(0:nlayers-1)
  real(kind=r_tran)   :: dz_1d(0:nlayers-1)

  nz = nlayers

  ! Flux boundary conditions
  k = 0
  df = 1
  ! Bottom boundary condition, zero flux
  flux_low(map_w2v(df)+k)  = 0.0_r_tran
  flux_int(map_w2v(df)+k)  = 0.0_r_tran
  k = nlayers-1
  df = 2
  ! Top boundary condition, zero flux
  flux_low(map_w2v(df)+k)  = 0.0_r_tran
  flux_int(map_w2v(df)+k)  = 0.0_r_tran

  ! Local field array
  do k=0,nlayers-1
    field_1d(k) = field(map_w3(1) + k)
    dz_1d(k) = dz(map_w3(1) + k)
  end do

  do k=0,nlayers-2

    ! Set mass from whole cells to be zero
    mass_from_whole_cells = 0.0_r_tran

    ! Do flux at top edge
    departure_dist = dep_pts(map_w2v(df)+k)

    ! Calculate number of cells of interest and fraction of a cell to add
    fractional_distance = departure_dist - int(departure_dist)
    n_cells_to_sum = abs(int(departure_dist))+1_i_def

    ! Get integer part and index
    if (departure_dist >= 0.0_r_tran) then
      nc = k-(n_cells_to_sum-1)
      do ii = 1, n_cells_to_sum-1
        mass_from_whole_cells = mass_from_whole_cells + field_1d(k-ii+1)*dz_1d(k-ii+1)/dz_1d(k)
      end do
    else
      nc = k+(n_cells_to_sum-1)+1
      do ii = 1, n_cells_to_sum-1
        mass_from_whole_cells = mass_from_whole_cells + field_1d(k+ii)*dz_1d(k+ii)/dz_1d(k+1)
      end do
    end if

    ! Low order PCM reconstruction
    reconstruction_low = field_1d(nc)

    ! Set low order fractional mass flux and integer flux
    flux_low(map_w2v(df)+k) = frac_wind(map_w2v(df)+k)*reconstruction_low
    flux_int(map_w2v(df)+k) = sign(1.0_r_tran,departure_dist)*mass_from_whole_cells/dt &
                              * detj(map_w2v(df)+k)

  end do

end subroutine vert_int_low_code

end module vert_int_low_kernel_mod
