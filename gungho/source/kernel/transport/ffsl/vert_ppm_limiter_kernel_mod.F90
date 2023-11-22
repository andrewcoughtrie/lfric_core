!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Computes the fractional vertical mass flux using the PPM reconstruction with relaxed
!!        monotonicity.
!> @details This kernel reconstructs a field using the limited PPM scheme, and
!!          then computes the fractional mass flux as
!!          flux = wind * reconstruction
!!          The PPM scheme is equivalent to fitting a quadratic to the cell such that
!!          the integral of the quadratic equals the integral of the field in the cell,
!!          and the quadratic matches the field (interpolated with fourth-order accuracy)
!!          values at cell edges. Relaxed monotonicity is applied to the edge values and
!!          the reconstruction. This kernel is designed to work in the vertical
!!          direction only and takes into account the vertical boundaries and grid spacing.
!!
!!          Note that this kernel only works when field is a W3 field at lowest order
!!          since it is assumed that ndf_w3 = 1 with stencil_map(1,:) containing
!!          the relevant dofmaps.

module vert_ppm_limiter_kernel_mod

use argument_mod,       only : arg_type,              &
                               GH_FIELD, GH_REAL,     &
                               GH_READ, GH_WRITE,     &
                               GH_SCALAR,             &
                               GH_LOGICAL, CELL_COLUMN
use fs_continuity_mod,  only : W3, W2v
use constants_mod,      only : r_tran, i_def, l_def, EPS_R_TRAN
use kernel_mod,         only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: vert_ppm_limiter_kernel_type
  private
  type(arg_type) :: meta_args(6) = (/                  &
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, W2v), & ! flux_high
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2v), & ! frac_wind
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2v), & ! dep pts
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3),  & ! field
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3),  & ! dz
       arg_type(GH_SCALAR, GH_LOGICAL, GH_READ)        & ! log_space
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: vert_ppm_limiter_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: vert_ppm_limiter_code

contains

!> @brief Compute the flux using the PPM reconstruction.
!> @param[in]     nlayers   Number of layers
!> @param[in,out] flux_high The high order fractional mass flux
!> @param[in]     frac_wind The fractional vertical wind
!> @param[in]     dep_pts   The vertical departure points
!> @param[in]     field     The field to construct the flux
!> @param[in]     dz        Vertical length of the W3 cell
!> @param[in]     log_space Switch to use natural logarithmic space
!!                          for edge interpolation
!> @param[in]     ndf_w2v   Number of degrees of freedom for W2v per cell
!> @param[in]     undf_w2v  Number of unique degrees of freedom for W2v
!> @param[in]     map_w2v   The dofmap for the W2v cell at the base of the column
!> @param[in]     ndf_w3    Number of degrees of freedom for W3 per cell
!> @param[in]     undf_w3   Number of unique degrees of freedom for W3
!> @param[in]     map_w3    The dofmap for the cell at the base of the column
subroutine vert_ppm_limiter_code( nlayers,   &
                                  flux_high, &
                                  frac_wind, &
                                  dep_pts,   &
                                  field,     &
                                  dz,        &
                                  log_space, &
                                  ndf_w2v,   &
                                  undf_w2v,  &
                                  map_w2v,   &
                                  ndf_w3,    &
                                  undf_w3,   &
                                  map_w3 )

  use cosmic_flux_mod, only: frac_and_int_vert_index
  use subgrid_rho_mod, only: vertical_ppm_relax, &
                             fourth_order_vertical_edge_relaxed

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)    :: nlayers
  integer(kind=i_def), intent(in)    :: undf_w2v
  integer(kind=i_def), intent(in)    :: ndf_w2v
  integer(kind=i_def), intent(in)    :: undf_w3
  integer(kind=i_def), intent(in)    :: ndf_w3
  real(kind=r_tran),   intent(inout) :: flux_high(undf_w2v)
  real(kind=r_tran),   intent(in)    :: field(undf_w3)
  real(kind=r_tran),   intent(in)    :: frac_wind(undf_w2v)
  real(kind=r_tran),   intent(in)    :: dep_pts(undf_w2v)
  real(kind=r_tran),   intent(in)    :: dz(undf_w3)
  integer(kind=i_def), intent(in)    :: map_w3(ndf_w3)
  integer(kind=i_def), intent(in)    :: map_w2v(ndf_w2v)
  logical(kind=l_def), intent(in)    :: log_space

  ! Internal variables
  integer(kind=i_def) :: k, edge_to_do
  integer(kind=i_def) :: ii
  integer(kind=i_def) :: n_cells_to_sum
  integer(kind=i_def) :: df, nz, nc

  real(kind=r_tran)   :: departure_dist
  real(kind=r_tran)   :: fractional_distance
  real(kind=r_tran)   :: reconstruction
  real(kind=r_tran)   :: field_local(4)
  real(kind=r_tran)   :: field_1d(0:nlayers-1)
  real(kind=r_tran)   :: edge_below(0:nlayers)
  real(kind=r_tran)   :: dz_local(1:4)
  real(kind=r_tran)   :: dz_1d(0:nlayers-1)

  ! Compute each edge separately, then use edge values to calculate flux reconstruction
  ! field_local and dz_local have index: | 1 | 2 | 3 | 4 | for fourth_order_vertical_edge_relaxed
  ! edge_to_do specifies which edge:     0   1   2   3   4

  nz = nlayers

  ! Flux boundary conditions
  k = 0
  df = 1
  ! Bottom boundary condition, zero flux
  flux_high(map_w2v(df)+k) = 0.0_r_tran
  k = nlayers-1
  df = 2
  ! Top boundary condition, zero flux
  flux_high(map_w2v(df)+k) = 0.0_r_tran

  ! Set up variables as 1D array
  do k=0,nlayers-1
    field_1d(k) = field(map_w3(1) + k)
    dz_1d(k) = dz(map_w3(1) + k)
  end do

  ! Calculate edge values for bottom layer
  k = 0
  do ii = 1,4
    field_local(ii) = field_1d(ii - 1)
    dz_local(ii) = dz(map_w3(1) + ii - 1)
  end do
  edge_to_do = 0_i_def
  call fourth_order_vertical_edge_relaxed( field_local, dz_local, edge_to_do, log_space, edge_below(k) )
  edge_to_do = 1_i_def
  call fourth_order_vertical_edge_relaxed( field_local, dz_local, edge_to_do, log_space, edge_below(k+1) )

  ! Calculate edge values for the top layer
  k = nlayers - 1
  do ii = 1,4
    field_local(ii) = field_1d(nlayers - 5 + ii)
    dz_local(ii) = dz(map_w3(1) + nlayers - 5 + ii)
  end do
  edge_to_do = 3_i_def
  call fourth_order_vertical_edge_relaxed( field_local, dz_local, edge_to_do, log_space, edge_below(k) )
  edge_to_do = 4_i_def
  call fourth_order_vertical_edge_relaxed( field_local, dz_local, edge_to_do, log_space, edge_below(k+1) )

  ! Loop over non-boundary cells to find bottom edge value of cell
  do k = 2,nlayers-2
    do ii = 1,4
      field_local(ii) = field_1d(k + ii - 3)
      dz_local(ii) = dz(map_w3(1) + k + ii - 3)
    end do
    edge_to_do = 2_i_def
    call fourth_order_vertical_edge_relaxed( field_local, dz_local, edge_to_do, log_space, edge_below(k) )
  end do

  ! Work with W2v DOF at top of cell, df = 2

  do k=0,nlayers-2

    ! Do flux at top edge
    departure_dist = dep_pts(map_w2v(df)+k)

    ! Calculate number of cells of interest, fractional departure distance,
    ! and index of upwind reconstruction cell
    call frac_and_int_vert_index(departure_dist,      &
                                 k,                   &
                                 fractional_distance, &
                                 n_cells_to_sum,      &
                                 nc)

    ! Relaxed limited PPM reconstruction
    call vertical_ppm_relax(reconstruction, fractional_distance, field_1d(nc), &
                            edge_below(nc), edge_below(nc+1))

    ! Set high order fractional mass flux
    flux_high(map_w2v(df)+k) = frac_wind(map_w2v(df)+k)*reconstruction

  end do

end subroutine vert_ppm_limiter_code

end module vert_ppm_limiter_kernel_mod
