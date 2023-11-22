!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Computes the fractional vertical mass flux using the Nirvana reconstruction.
!> @details This kernel reconstructs a field using the Nirvana scheme, and
!!          then computes the fractional mass flux as
!!          flux = wind * reconstruction
!!          The Nirvana reconstruction is equivalent to fitting a quadratic to
!!          the cell such that the integral of the quadratic equals the integral
!!          of the field in the cell, and the gradient of the quadratic matches
!!          the gradient of the field at cell edges. This kernel is designed to
!!          work in the vertical direction only and takes into account the vertical
!!          boundaries and grid spacing.
!!
!!          Note that this kernel only works when field is a W3 field at lowest order
!!          since it is assumed that ndf_w3 = 1 with stencil_map(1,:) containing
!!          the relevant dofmaps.

module vert_nirvana_kernel_mod

use argument_mod,       only : arg_type,              &
                               GH_FIELD, GH_REAL,     &
                               GH_READ, GH_WRITE,     &
                               CELL_COLUMN
use fs_continuity_mod,  only : W3, W2v
use constants_mod,      only : r_tran, i_def, EPS_R_TRAN
use kernel_mod,         only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: vert_nirvana_kernel_type
  private
  type(arg_type) :: meta_args(5) = (/                  &
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, W2v), & ! flux_high
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2v), & ! frac_wind
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2v), & ! dep_pts
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3),  & ! field
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3)   & ! dz
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: vert_nirvana_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: vert_nirvana_code

contains

!> @brief Compute the flux using the Nirvana reconstruction.
!> @param[in]     nlayers   Number of layers
!> @param[in,out] flux_high The high order fractional mass flux
!> @param[in]     frac_wind The fractional vertical wind
!> @param[in]     dep_pts   The vertical departure points
!> @param[in]     field     The field to construct the flux
!> @param[in]     dz        Vertical length of the W3 cell
!> @param[in]     ndf_w2v   Number of degrees of freedom for W2v per cell
!> @param[in]     undf_w2v  Number of unique degrees of freedom for W2v
!> @param[in]     map_w2v   The dofmap for the W2v cell at the base of the column
!> @param[in]     ndf_w3    Number of degrees of freedom for W3 per cell
!> @param[in]     undf_w3   Number of unique degrees of freedom for W3
!> @param[in]     map_w3    The dofmap for the cell at the base of the column
subroutine vert_nirvana_code( nlayers,   &
                              flux_high, &
                              frac_wind, &
                              dep_pts,   &
                              field,     &
                              dz,        &
                              ndf_w2v,   &
                              undf_w2v,  &
                              map_w2v,   &
                              ndf_w3,    &
                              undf_w3,   &
                              map_w3 )

  use cosmic_flux_mod, only: frac_and_int_vert_index
  use subgrid_rho_mod, only: vertical_nirvana_recon, &
                             second_order_vertical_gradient

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

  ! Internal variables
  integer(kind=i_def) :: k
  integer(kind=i_def) :: ii
  integer(kind=i_def) :: n_cells_to_sum
  integer(kind=i_def) :: df, nz, nc

  real(kind=r_tran)   :: departure_dist
  real(kind=r_tran)   :: fractional_distance
  real(kind=r_tran)   :: reconstruction
  real(kind=r_tran)   :: field_local(2)
  real(kind=r_tran)   :: field_1d(0:nlayers-1)
  real(kind=r_tran)   :: dz_1d(0:nlayers-1)
  real(kind=r_tran)   :: gradient_below(0:nlayers)
  real(kind=r_tran)   :: dz_local(1:2)

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

  ! At top and bottom the edge gradients are zero
  gradient_below(0) = 0.0_r_tran
  gradient_below(nlayers) = 0.0_r_tran

  do k=0,nlayers-1
    field_1d(k) = field(map_w3(1) + k)
    dz_1d(k) = dz(map_w3(1) + k)
  end do

  ! Loop over non-boundary cells to find the gradient at bottom edge of the cell
  do k = 1,nlayers-1
    do ii = 1,2
      dz_local(ii) = dz(map_w3(1) + k + ii - 2)
      field_local(ii) = field_1d(k + ii - 2)
    end do
    call second_order_vertical_gradient(field_local, dz_local, gradient_below(k))
  end do

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

    ! Nirvana reconstruction
    call vertical_nirvana_recon(reconstruction, fractional_distance, field_1d(nc), &
                                gradient_below(nc), gradient_below(nc+1), dz_1d(nc))

    ! Set high order fractional mass flux
    flux_high(map_w2v(df)+k) = frac_wind(map_w2v(df)+k)*reconstruction

  end do

end subroutine vert_nirvana_code

end module vert_nirvana_kernel_mod
