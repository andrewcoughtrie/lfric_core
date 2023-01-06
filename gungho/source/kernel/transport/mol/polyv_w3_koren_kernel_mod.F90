!-----------------------------------------------------------------------------
! (c) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!> @brief Kernel which computes vertical cell egdes value based on the monotone
!!        Koren scheme.
!> @details The kernel computes vertical cell edges at W2v-points from
!!          W3-field using the Koren scheme.
!!  Ref.   Koren, B. (1993), "A robust upwind discretisation method for advection,
!!         diffusion and source terms", in Vreugdenhil, C.B.; Koren, B. (eds.),
!!         Numerical Methods for Advection/Diffusion Problems, Braunschweig:
!!         Vieweg, p. 117, ISBN 3-528-07645-3.

module polyv_w3_koren_kernel_mod

use argument_mod,      only : arg_type, func_type,         &
                              GH_FIELD, GH_SCALAR,         &
                              GH_REAL, GH_INTEGER,         &
                              GH_LOGICAL, GH_WRITE,        &
                              ANY_DISCONTINUOUS_SPACE_1,   &
                              GH_READ, CELL_COLUMN
use fs_continuity_mod, only : W3
use constants_mod,     only : i_def, l_def, tiny_eps, EPS_R_TRAN, r_tran
use kernel_mod,        only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the PSy layer
type, public, extends(kernel_type) :: polyv_w3_koren_kernel_type
  private
  type(arg_type) :: meta_args(5) = (/                                        &
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, ANY_DISCONTINUOUS_SPACE_1), &
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W3),                        &
       arg_type(GH_SCALAR, GH_INTEGER, GH_READ),                             &
       arg_type(GH_SCALAR, GH_LOGICAL, GH_READ),                             &
       arg_type(GH_SCALAR, GH_LOGICAL, GH_READ)                              &
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: polyv_w3_koren_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: polyv_w3_koren_code

contains

!> @brief Computes the vertical reconstructions for a tracer.
!> @param[in]     nlayers        Number of layers
!> @param[in,out] reconstruction Mass reconstruction field to compute
!> @param[in]     tracer         Tracer field
!> @param[in]     ndata          Number of data points per dof location
!> @param[in]     reversible     Use the reversible scheme
!> @param[in]     logspace       Perform interpolation in log space;
!> @param[in]     ndf_md         Number of degrees of freedom per cell
!> @param[in]     undf_md        Number of unique degrees of freedom for the
!!                               reconstrution
!> @param[in]     map_md         Dofmap for the cell at the base of the column
!> @param[in]     ndf_w3         Number of degrees of freedom per cell
!> @param[in]     undf_w3        Number of unique degrees of freedom for the tracer field
!> @param[in]     map_w3         Cell dofmaps for the tracer space

subroutine polyv_w3_koren_code( nlayers,                           &
                                reconstruction,                    &
                                tracer,                            &
                                ndata,                             &
                                reversible,                        &
                                logspace,                          &
                                ndf_md,                            &
                                undf_md,                           &
                                map_md,                            &
                                ndf_w3,                            &
                                undf_w3,                           &
                                map_w3                             )

  implicit none

  ! Arguments
  integer(kind=i_def), intent(in)                     :: nlayers
  integer(kind=i_def), intent(in)                     :: ndata
  integer(kind=i_def), intent(in)                     :: ndf_w3
  integer(kind=i_def), intent(in)                     :: undf_w3
  integer(kind=i_def), intent(in)                     :: ndf_md
  integer(kind=i_def), intent(in)                     :: undf_md
  integer(kind=i_def), dimension(ndf_md),  intent(in) :: map_md
  integer(kind=i_def), dimension(ndf_w3),  intent(in) :: map_w3

  real(kind=r_tran), dimension(undf_md),  intent(inout) :: reconstruction
  real(kind=r_tran), dimension(undf_w3),  intent(in)    :: tracer
  logical(kind=l_def), intent(in)                       :: reversible
  logical(kind=l_def), intent(in)                       :: logspace

  ! Local variables
  integer(kind=i_def)                              :: k, k1, k2, k3
  real(kind=r_tran)                                :: x, y, r, r1, r2, phi
  integer(kind=i_def), parameter                   :: ext = 2
  real(kind=r_tran), dimension(2, nlayers)         :: tracer_edge
  real(kind=r_tran), dimension(1-ext:nlayers+ext)  :: tracer_1d

  ! Extract the global data into 1d-array
  do k = 0,nlayers - 1
    tracer_1d(k+1) = tracer(map_w3(1)+k)
  end do

  ! Extend the tracer_1d array to go beyond the boundaries
  ! to avoid treating the boundaries differently from the rest
  ! of the column.
  !
  ! Ideally these extra points can be constructed by
  ! preserving the slope near the boundaries, but this
  ! approach can cause the data to go outside the initial bounds.
  ! Therefore we use zero slope assumption.

  tracer_1d(1-ext:0) = tracer_1d(1)
  tracer_1d(nlayers+1:nlayers+ext) = tracer_1d(nlayers)

  ! Apply log to tracer_1d if required
  ! If using the logspace option, the tracer is forced to be positive
  if (logspace) then
      do k = 1-ext,nlayers+ext
         tracer_1d(k) = log(max(EPS_R_TRAN,abs(tracer_1d(k))))
      end do
  end if

  if (reversible) then !The reversible is the koren-scheme with phi=r
    do k = 1, nlayers
      ! Bottom edge of cell
      tracer_edge(1,k) = 0.5_r_tran*(tracer_1d(k-1) + tracer_1d(k))
      ! Top edge of cell
      tracer_edge(2,k) = 0.5_r_tran*(tracer_1d(k) + tracer_1d(k+1))
    end do
  else
    do k = 1, nlayers
      ! Bottom edge
        k3 = k - 1
        k2 = k
        k1 = k + 1
        x = tracer_1d(k2) - tracer_1d(k1)
        y = tracer_1d(k3) - tracer_1d(k2)
        r = (y + tiny_eps)/(x + tiny_eps)
        r1 = 2.0_r_tran*r
        r2 = (1.0_r_tran + r1)/3.0_r_tran
        phi = max(0.0_r_tran, min(r1,r2,2.0_r_tran))
        tracer_edge(1,k) = tracer_1d(k2) + 0.5_r_tran*phi*x

      ! Top edge
        k3 = k + 1
        k2 = k
        k1 = k - 1
        x = tracer_1d(k2) - tracer_1d(k1)
        y = tracer_1d(k3) - tracer_1d(k2)
        r = (y + tiny_eps)/(x + tiny_eps)
        r1 = 2.0_r_tran*r
        r2 = (1.0_r_tran + r1)/3.0_r_tran
        phi = max(0.0_r_tran, min(r1,r2,2.0_r_tran))
        tracer_edge(2,k) = tracer_1d(k2) + 0.5_r_tran*phi*x
    end do
  end if

  if (logspace) then
      do k=1,nlayers
         tracer_edge(1,k) = exp(tracer_edge(1,k))
         tracer_edge(2,k) = exp(tracer_edge(2,k))
      end do
  end if

  do k = 0, nlayers-1
     reconstruction(map_md(1) + k )           =  tracer_edge(1,k+1)
     reconstruction(map_md(1) + nlayers + k ) =  tracer_edge(2,k+1)
  end do

end subroutine polyv_w3_koren_code

end module polyv_w3_koren_kernel_mod
