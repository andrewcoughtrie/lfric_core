!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Kernel which converts the vertical departure distance into a fractional wind.
!> @details The Eulerian departure distance is calculated using the advecting wind
!!          divided by Det(J) multiplied by the time step. This kernels splits
!!          the vertical departure distance into integer and fractional parts, and
!!          then returns the fractional vertical advecting wind.

module fractional_vertical_wind_kernel_mod

use argument_mod,       only : arg_type,              &
                               GH_FIELD, GH_REAL,     &
                               GH_READ, GH_WRITE,     &
                               GH_SCALAR, CELL_COLUMN
use fs_continuity_mod,  only : W2v
use constants_mod,      only : r_tran, i_def, EPS_R_TRAN
use kernel_mod,         only : kernel_type

implicit none

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: fractional_vertical_wind_kernel_type
  private
  type(arg_type) :: meta_args(4) = (/                  &
       arg_type(GH_FIELD,  GH_REAL,    GH_WRITE, W2v), & ! frac_wind
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2v), & ! dep_pts
       arg_type(GH_FIELD,  GH_REAL,    GH_READ,  W2v), & ! detj
       arg_type(GH_SCALAR, GH_REAL,    GH_READ)        & ! dt
       /)
  integer :: operates_on = CELL_COLUMN
contains
  procedure, nopass :: fractional_vertical_wind_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public :: fractional_vertical_wind_code

contains

  !> @brief Compute the flux using the PCM reconstruction.
  !> @param[in]     nlayers   Number of layers
  !> @param[in,out] frac_wind The fractional vertical wind
  !> @param[in]     dep_pts   The vertical departure distance
  !> @param[in]     detj      Det(J) at vertical W2v points
  !> @param[in]     dt        The time step
  !> @param[in]     ndf_w2v   Number of degrees of freedom for W2v per cell
  !> @param[in]     undf_w2v  Number of unique degrees of freedom for W2v
  !> @param[in]     map_w2v   The dofmap for the W2v cell at the base of the column
  subroutine fractional_vertical_wind_code( nlayers,   &
                                            frac_wind, &
                                            dep_pts,   &
                                            detj,      &
                                            dt,        &
                                            ndf_w2v,   &
                                            undf_w2v,  &
                                            map_w2v )

    implicit none

    ! Arguments
    integer(kind=i_def), intent(in)    :: nlayers
    integer(kind=i_def), intent(in)    :: undf_w2v
    integer(kind=i_def), intent(in)    :: ndf_w2v
    real(kind=r_tran),   intent(inout) :: frac_wind(undf_w2v)
    real(kind=r_tran),   intent(in)    :: dep_pts(undf_w2v)
    real(kind=r_tran),   intent(in)    :: detj(undf_w2v)
    integer(kind=i_def), intent(in)    :: map_w2v(ndf_w2v)
    real(kind=r_tran),   intent(in)    :: dt

    ! Internal variables
    integer(kind=i_def) :: k, df

    real(kind=r_tran)   :: departure_dist
    real(kind=r_tran)   :: fractional_distance

    ! Wind at surface and model top should be zero
    frac_wind(map_w2v(1)) = 0.0_r_tran
    frac_wind(map_w2v(2)+nlayers-1) = 0.0_r_tran

    ! Loop over levels working with DOF at the top of the cell
    df = 2

    do k = 0, nlayers-2
      ! Get departure distance
      departure_dist = dep_pts(map_w2v(df)+k)

      ! Get fractional part
      fractional_distance = departure_dist - int(departure_dist)

      ! Fractional wind is fractional_departure_distance * Det(J) / timestep
      frac_wind(map_w2v(df)+k) = fractional_distance*detj(map_w2v(df)+k)/dt
    end do

  end subroutine fractional_vertical_wind_code

end module fractional_vertical_wind_kernel_mod
