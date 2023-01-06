!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!> @brief Kernel which computes vertical cell edges value based on the monotone
!!        Koren scheme.
!> @details The kernel computes advective increment at Wtheta-points, using
!!          edge-values at w3-points computed with the Koren scheme.
!!  Ref.   Koren, B. (1993), "A robust upwind discretisation method for advection,
!!         diffusion and source terms", in Vreugdenhil, C.B.; Koren, B. (eds.),
!!         Numerical Methods for Advection/Diffusion Problems, Braunschweig:
!!         Vieweg, p. 117, ISBN 3-528-07645-3.

module polyv_wtheta_koren_kernel_mod

use argument_mod,         only : arg_type, GH_FIELD,     &
                                 GH_SCALAR, GH_REAL,     &
                                 GH_INTEGER, GH_LOGICAL, &
                                 GH_READWRITE,           &
                                 GH_READ, CELL_COLUMN
use constants_mod,        only : r_tran, i_def, l_def, tiny_eps, EPS_R_TRAN
use fs_continuity_mod,    only : W2v, Wtheta
use kernel_mod,           only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the PSy layer
type, public, extends(kernel_type) :: polyv_wtheta_koren_kernel_type
  private
  type(arg_type) :: meta_args(6) = (/                                          &
       arg_type(GH_FIELD,  GH_REAL,    GH_READWRITE, Wtheta),                  &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,      W2v),                     &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,      Wtheta),                  &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                               &
       arg_type(GH_SCALAR, GH_LOGICAL, GH_READ),                               &
       arg_type(GH_SCALAR, GH_LOGICAL, GH_READ)                                &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: polyv_wtheta_koren_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: polyv_wtheta_koren_code

contains

!> @brief Computes the vertical fluxes for a tracer density.
!> @param[in]     nlayers      Number of layers
!> @param[in,out] advective    Advective update to increment
!> @param[in]     wind         Wind field
!> @param[in]     tracer       Tracer field to advect
!> @param[in]     ndata        Number of data points per dof location
!> @param[in]     logspace     Perform interpolation in log space
!> @param[in]     ndf_wt       Number of degrees of freedom per cell
!> @param[in]     undf_wt      Number of unique degrees of freedom for the tracer field
!> @param[in]     map_wt       Cell dofmaps for the tracer space
!> @param[in]     ndf_w2v      Number of degrees of freedom per cell
!> @param[in]     undf_w2v     Number of unique degrees of freedom for the flux &
!!                             wind fields
!> @param[in]     map_w2v      Dofmap for the cell at the base of the column

subroutine polyv_wtheta_koren_code( nlayers,              &
                                    advective,            &
                                    wind,                 &
                                    tracer,               &
                                    ndata,                &
                                    reversible,           &
                                    logspace,             &
                                    ndf_wt,               &
                                    undf_wt,              &
                                    map_wt,               &
                                    ndf_w2v,              &
                                    undf_w2v,             &
                                    map_w2v               )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)                     :: nlayers
  integer(kind=i_def), intent(in)                     :: ndf_wt
  integer(kind=i_def), intent(in)                     :: undf_wt
  integer(kind=i_def), intent(in)                     :: ndf_w2v
  integer(kind=i_def), intent(in)                     :: undf_w2v
  integer(kind=i_def), dimension(ndf_w2v), intent(in) :: map_w2v
  integer(kind=i_def), dimension(ndf_wt),  intent(in) :: map_wt
  integer(kind=i_def), intent(in)                     :: ndata

  real(kind=r_tran), dimension(undf_wt),  intent(inout) :: advective
  real(kind=r_tran), dimension(undf_w2v), intent(in)    :: wind
  real(kind=r_tran), dimension(undf_wt),  intent(in)    :: tracer
  logical(kind=l_def),                   intent(in)     :: reversible
  logical(kind=l_def),                   intent(in)     :: logspace

  !Internal variables
  real(kind=r_tran), dimension(nlayers+1)   :: wind_1d, dtracerdz
  real(kind=r_tran), dimension(0:nlayers+2) :: tracer_1d
  integer(kind=i_def) :: k, k1, k2, k3, km, kp
  real(kind=r_tran)   :: x, y, r, r1, r2, phi, tracer_p, tracer_m

  !Extract vertical 1d-arrays from global data
  do k=0,nlayers
      wind_1d(k+1) = wind(map_w2v(1)+k)
      tracer_1d(k+1) = tracer(map_wt(1)+k)
  end do

  ! Add 2 extra points for tracer_1d array outside the boundaries
  ! to avoid treating the boundaries differently from the rest
  ! of the column.
  !
  ! Ideally these extra points can be constructed by
  ! preserving the slope near the boundaries, but this
  ! approach can cause the data to go outside the bounds
  ! of the initial (unextended) data. Therefore we use zero
  ! slope assumption.

  tracer_1d(0) = tracer_1d(1)
  tracer_1d(nlayers+2) = tracer_1d(nlayers+1)

  ! Apply log to tracer_1d if required
  ! If using the logspace option, the tracer is forced to be positive
  if (logspace) then
    do k=0,nlayers+2
      tracer_1d(k) = log(max(EPS_R_TRAN,abs(tracer_1d(k))))
    end do
  end if

  do k = 2, nlayers
    if ( reversible ) then
      tracer_m = 0.5_r_tran*(tracer_1d(k-1) + tracer_1d(k))
      tracer_p = 0.5_r_tran*(tracer_1d(k)   + tracer_1d(k+1))
    else
      ! Cell below
      km = k - 1
      if ( wind_1d(k) > 0.0_r_tran ) then
        ! (k, k-1, k-2)
        k3 = km + 1_i_def
        k2 = km
        k1 = km - 1_i_def
      else
        ! (k-1, k, k+1)
        k3 = km
        k2 = km + 1_i_def
        k1 = km + 2_i_def
      end if
      x = tracer_1d(k2) - tracer_1d(k1)
      y = tracer_1d(k3) - tracer_1d(k2)
      r = (y + tiny_eps)/(x + tiny_eps)
      r1 = 2.0_r_tran*r
      r2 = (1.0_r_tran + r1)/3.0_r_tran
      phi = max(0.0_r_tran, min(r1,r2,2.0_r_tran))
      tracer_m = tracer_1d(k2) + 0.5_r_tran*phi*x

      ! Cell above
      kp = k
      if ( wind_1d(k) > 0.0_r_tran ) then
        ! (k+1, k, k-1)
        k3 = kp + 1_i_def
        k2 = kp
        k1 = kp - 1_i_def
      else
        ! (k, k+1, k+2)
        k3 = kp
        k2 = kp + 1_i_def
        k1 = kp + 2_i_def
      end if
      x = tracer_1d(k2) - tracer_1d(k1)
      y = tracer_1d(k3) - tracer_1d(k2)
      r = (y + tiny_eps)/(x + tiny_eps)
      r1 = 2.0_r_tran*r
      r2 = (1.0_r_tran + r1)/3.0_r_tran
      phi = max(0.0_r_tran, min(r1,r2,2.0_r_tran))
      tracer_p = tracer_1d(k2) + 0.5_r_tran*phi*x

    end if
    if ( logspace ) then
      tracer_m = exp(tracer_m)
      tracer_p = exp(tracer_p)
    end if
    dtracerdz(k) = tracer_p - tracer_m
  end do

  do k = 1, nlayers - 1
    advective(map_wt(1) + k ) = advective(map_wt(1) + k )   &
                                + wind(map_w2v(1) + k )     &
                                * dtracerdz(k+1)
  end do

end subroutine polyv_wtheta_koren_code

end module polyv_wtheta_koren_kernel_mod
