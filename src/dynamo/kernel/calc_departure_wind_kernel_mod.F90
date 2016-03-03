!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel to compute the wind used for calculating the departure points
!! in the dimensionally split advection scheme. Input is the Piola wind and
!! output is the departure wind.

module calc_departure_wind_kernel_mod
use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_READ, GH_WRITE,             &
                                    W0, W2,                                  &
                                    GH_DIFF_BASIS, GH_BASIS,                 &
                                    CELLS
use constants_mod,           only : r_def

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: calc_departure_wind_kernel_type
  private
  type(arg_type) :: meta_args(3) = (/                                  &
       arg_type(GH_FIELD,    GH_WRITE, W2),                            &
       arg_type(GH_FIELD,    GH_READ,  W2),                            &
       arg_type(GH_FIELD*3,  GH_READ,  W0)                             &
       /)
  type(func_type) :: meta_funcs(1) = (/                                &
       func_type(W0, GH_DIFF_BASIS)                                    &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::calc_departure_wind_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! overload the default structure constructor for function space
interface calc_departure_wind_kernel_type
   module procedure calc_departure_wind_kernel_constructor
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public calc_departure_wind_code
contains

type(calc_departure_wind_kernel_type) function calc_departure_wind_kernel_constructor() result(self)
  return
end function calc_departure_wind_kernel_constructor

!> @param[in] nlayers Integer the number of layers
!> @param[in] ndf The number of degrees of freedom per cell for the output field
!> @param[in] undf The number of unique degrees of freedom for the output field
!> @param[in] map Integer array holding the dofmap for the cell at the base of the column for the output field
!> @param[inout] u_departure_wind The output field containing the departure wind used to calculate departure points
!> @param[in] u_piola The input field for the Piola wind
!> @param[in] chi1 the array of coordinates in the first direction
!> @param[in] chi2 the array of coordinates in the second direction
!> @param[in] chi3 the array of coordinates in the third direction
!> @param[in] ndf_chi The number of degrees of freedom per cell for the coordinate field
!> @param[in] undf_chi The number of unique degrees of freedom for the coordinate field
!> @param[in] map_chi Integer array holding the dofmap for the cell at the base of the column for the coordinate field
!> @param[in] diff_basis_chi the diff basis functions of the coordinate space evaluated at the nodal points
subroutine calc_departure_wind_code(nlayers,                                  &
                                    u_departure_wind,                         &
                                    u_piola,                                  &
                                    chi1, chi2, chi3,                         &
                                    ndf, undf, map,                           &
                                    ndf_chi, undf_chi, map_chi,               &
                                    diff_basis_chi                            &
                                    )
  use coordinate_jacobian_mod, only: coordinate_jacobian
  implicit none
  !Arguments
  integer,                                    intent(in)    :: nlayers
  integer,                                    intent(in)    :: ndf, undf, &
                                                               ndf_chi, undf_chi
  integer,          dimension(ndf),           intent(in)    :: map
  integer,          dimension(ndf_chi),       intent(in)    :: map_chi
  real(kind=r_def), dimension(undf),          intent(in)    :: u_piola
  real(kind=r_def), dimension(undf_chi),      intent(in)    :: chi1, chi2, chi3
  real(kind=r_def), dimension(undf),          intent(inout) :: u_departure_wind
  real(kind=r_def), dimension(3,ndf_chi,ndf), intent(in)    :: diff_basis_chi

  !Internal variables
  integer          :: df, k
  real(kind=r_def) :: jacobian(3,3,ndf,1), dj(ndf,1)
  real(kind=r_def), dimension(ndf_chi) :: chi1_e, chi2_e, chi3_e

  do k = 0, nlayers-1
    do df = 1,ndf_chi
      chi1_e(df) = chi1(map_chi(df) + k)
      chi2_e(df) = chi2(map_chi(df) + k)
      chi3_e(df) = chi3(map_chi(df) + k)
    end do
    call coordinate_jacobian(ndf_chi, ndf, 1, chi1_e, chi2_e, chi3_e,  &
                             diff_basis_chi, jacobian, dj)
    do df = 1,ndf
      u_departure_wind(map(df)+k) = u_piola(map(df)+k)/dj(df,1)
    end do
  end do

end subroutine calc_departure_wind_code

end module calc_departure_wind_kernel_mod
