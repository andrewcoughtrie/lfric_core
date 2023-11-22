!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Computes the fractional vertical mass flux using the reversible Nirvana reconstruction.
!> @details This kernel reconstructs a field using the reversible Nirvana scheme, and
!!          then computes the fractional mass flux as
!!          flux = wind * reconstruction
!!          The reversible Nirvana reconstruction is equivalent to fitting a quadratic
!!          to the cell such that the integral of the quadratic equals the integral of
!!          the field in the cell, and the quadratic matches the field (interpolated)
!!          values at cell edges. This kernel is designed to work in the vertical
!!          direction only and takes into account the vertical boundaries and grid spacing.
!!
!!          Note that this kernel only works when field is a W3 field at lowest order
!!          since it is assumed that ndf_w3 = 1 with stencil_map(1,:) containing
!!          the relevant dofmaps.

module vert_nirvana_reversible_kernel_mod

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
type, public, extends(kernel_type) :: vert_nirvana_reversible_kernel_type
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
  procedure, nopass :: vert_nirvana_reversible_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: vert_nirvana_reversible_code

contains

!> @brief Compute the flux using the reversible Nirvana reconstruction.
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
subroutine vert_nirvana_reversible_code( nlayers,   &
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
  use subgrid_rho_mod, only: vertical_ppm_recon, &
                             second_order_vertical_edge

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
  real(kind=r_tran)   :: field_local(2)
  real(kind=r_tran)   :: field_1d(0:nlayers-1)
  real(kind=r_tran)   :: edge_below(0:nlayers)
  real(kind=r_tran)   :: dz_local(1:2)
  real(kind=r_tran)   :: dz_1d(0:nlayers-1)

  ! field_local and dz_local have index: | 1 | 2 |

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

  ! If using log_space then convert field to log space
  if (log_space) then
    do k=0,nlayers-1
      field_1d(k) = log( max( EPS_R_TRAN, abs( field(map_w3(1) + k) ) ) )
      dz_1d(k) = dz(map_w3(1) + k)
    end do
  else
    do k=0,nlayers-1
      field_1d(k) = field(map_w3(1) + k)
      dz_1d(k) = dz(map_w3(1) + k)
    end do
  end if

  ! Calculate edge values for bottom layer
  k = 0
  do ii = 1,2
    dz_local(ii) = dz(map_w3(1) + ii - 1)
    field_local(ii) = field_1d(ii - 1)
  end do
  edge_to_do = 0_i_def
  call second_order_vertical_edge(field_local, dz_local, edge_to_do, edge_below(k))

  ! Calculate edge values for top layer
  k = nlayers-1
  do ii = 1,2
    dz_local(ii) = dz(map_w3(1) + nlayers + ii - 3)
    field_local(ii) = field_1d(nlayers + ii - 3)
  end do
  edge_to_do = 2_i_def
  call second_order_vertical_edge(field_local, dz_local, edge_to_do, edge_below(k+1))

  ! Loop over non-boundary cells to find bottom edge value of cell
  do k = 1,nlayers-1
    do ii = 1,2
      dz_local(ii) = dz(map_w3(1) + k + ii - 2)
      field_local(ii) = field_1d(k + ii - 2)
    end do
    edge_to_do = 1_i_def
    call second_order_vertical_edge(field_local, dz_local, edge_to_do, edge_below(k))
  end do

  ! If using log_space then convert back
  if (log_space) then
    do k=0,nlayers
      edge_below(k) = exp(edge_below(k))
    end do
    do k=0,nlayers-1
      field_1d(k)   = exp(field_1d(k))
    end do
  end if

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

    ! Reversible Nirvana reconstruction
    call vertical_ppm_recon(reconstruction, fractional_distance, field_1d(nc), &
                            edge_below(nc), edge_below(nc+1))

    ! Set high order fractional mass flux
    flux_high(map_w2v(df)+k) = frac_wind(map_w2v(df)+k)*reconstruction

  end do

end subroutine vert_nirvana_reversible_code

end module vert_nirvana_reversible_kernel_mod
