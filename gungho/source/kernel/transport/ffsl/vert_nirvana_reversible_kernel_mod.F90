!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Calculates the coefficients for 1D reversible Nirvana subgrid representation
!!        of rho in the vertical direction.
!> @details The kernel computes the coefficients a0, a1, a2 where rho is represented
!!          in 1D by the approximation rho(x) = a0+a1*x+a2*x**2 with 0<x<1.
!!          Reversible Nirvana is used to calculate the quadratic subgrid
!!          representation of rho.
!!
!!          This kernel is designed to work in the vertical direction only and
!!          takes into account the vertical boundaries.
!!
!!          Note that this kernel only works when rho is a W3 field at lowest order
!!          since it is assumed that ndf_w3 = 1 with stencil_map(1,:) containing
!!          the relevant dofmaps.

module vert_nirvana_reversible_kernel_mod

use argument_mod,       only : arg_type,              &
                               GH_FIELD, GH_REAL,     &
                               GH_READ, GH_WRITE,     &
                               GH_SCALAR, GH_INTEGER, &
                               GH_LOGICAL, CELL_COLUMN
use fs_continuity_mod,  only : W3
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
  type(arg_type) :: meta_args(7) = (/                 &
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, W3), & ! a0 subgrid coefficient
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, W3), & ! a1 subgrid coefficient
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, W3), & ! a2 subgrid coefficient
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3), & ! rho
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3), & ! dz
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ     ), & ! monotone
       arg_type(GH_SCALAR, GH_LOGICAL, GH_READ)       & ! log_space
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

!> @brief Compute the reversible Nirvana subgrid reconstruction coefficients for a field.
!> @param[in]     nlayers   Number of layers
!> @param[in,out] a0        Coefficient a0
!> @param[in,out] a1        Coefficient a1
!> @param[in,out] a2        Coefficient a2
!> @param[in]     rho       Density
!> @param[in]     dz        Vertical length of the W3 cell
!> @param[in]     monotone  Vertical monotone option for FFSL
!> @param[in]     log_space Switch to use natural logarithmic space
!!                          for edge interpolation
!> @param[in]     ndf_w3    Number of degrees of freedom for W3 per cell
!> @param[in]     undf_w3   Number of unique degrees of freedom for W3
!> @param[in]     map_w3    The dofmap for the cell at the base of the column
subroutine vert_nirvana_reversible_code( nlayers,   &
                                         a0,        &
                                         a1,        &
                                         a2,        &
                                         rho,       &
                                         dz,        &
                                         monotone,  &
                                         log_space, &
                                         ndf_w3,    &
                                         undf_w3,   &
                                         map_w3 )

  use subgrid_rho_mod, only : second_order_vertical_edge, &
                              ppm_output

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)    :: nlayers
  integer(kind=i_def), intent(in)    :: undf_w3
  integer(kind=i_def), intent(in)    :: ndf_w3
  real(kind=r_tran),   intent(inout) :: a0(undf_w3)
  real(kind=r_tran),   intent(inout) :: a1(undf_w3)
  real(kind=r_tran),   intent(inout) :: a2(undf_w3)
  real(kind=r_tran),   intent(in)    :: rho(undf_w3)
  real(kind=r_tran),   intent(in)    :: dz(undf_w3)
  integer(kind=i_def), intent(in)    :: map_w3(ndf_w3)
  integer(kind=i_def), intent(in)    :: monotone
  logical(kind=l_def), intent(in)    :: log_space

  real(kind=r_tran)                  :: coeffs(1:3)
  real(kind=r_tran)                  :: rho_1d(0:nlayers-1)
  real(kind=r_tran)                  :: rho_local(1:2)
  real(kind=r_tran)                  :: dz_local(1:2)
  real(kind=r_tran)                  :: edge_below(0:nlayers)
  integer(kind=i_def)                :: k, ii, edge_to_do

  ! rho_local and dz_local have index: | 1 | 2 |

  ! If using log_space then convert rho to log space
  if (log_space) then
    do k=0,nlayers-1
      rho_1d(k) = log( max( EPS_R_TRAN, abs( rho(map_w3(1) + k) ) ) )
    end do
  else
    do k=0,nlayers-1
      rho_1d(k) = rho(map_w3(1) + k)
    end do
  end if

  ! Calculate edge values for bottom layer
  k = 0
  do ii = 1,2
    dz_local(ii) = dz(map_w3(1) + ii - 1)
    rho_local(ii) = rho_1d(ii - 1)
  end do
  edge_to_do = 0_i_def
  call second_order_vertical_edge(rho_local, dz_local, edge_to_do, edge_below(k))

  ! Calculate edge values for top layer
  k = nlayers-1
  do ii = 1,2
    dz_local(ii) = dz(map_w3(1) + nlayers + ii - 3)
    rho_local(ii) = rho_1d(nlayers + ii - 3)
  end do
  edge_to_do = 2_i_def
  call second_order_vertical_edge(rho_local, dz_local, edge_to_do, edge_below(k+1))

  ! Loop over non-boundary cells to find bottom edge value of cell
  do k = 1,nlayers-1
    do ii = 1,2
      dz_local(ii) = dz(map_w3(1) + k + ii - 2)
      rho_local(ii) = rho_1d(k + ii - 2)
    end do
    edge_to_do = 1_i_def
    call second_order_vertical_edge(rho_local, dz_local, edge_to_do, edge_below(k))
  end do

  ! If using log_space then convert back
  if (log_space) then
    do k=0,nlayers
      edge_below(k) = exp(edge_below(k))
    end do
  end if

  ! Compute the PPM coefficients using the second-order edge values to get
  ! reversible Nirvana
  do k = 0,nlayers-1
    call ppm_output( edge_below(k), edge_below(k+1), rho(map_w3(1)+k), monotone, coeffs )
    a0(map_w3(1)+k) = coeffs(1)
    a1(map_w3(1)+k) = coeffs(2)
    a2(map_w3(1)+k) = coeffs(3)
  end do

end subroutine vert_nirvana_reversible_code

end module vert_nirvana_reversible_kernel_mod
