!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Kernel computes the departure points in 1D

!> @details The kernel computes the departure points using the departure wind.
!>         The kernel works in 1D only.

module calc_departure_point_kernel_mod

use kernel_mod,              only : kernel_type
use argument_mod,            only : arg_type, func_type,                     &
                                    GH_FIELD, GH_READ, GH_WRITE,             &
                                    W0, W2, W3,                              &
                                    GH_BASIS, GH_DIFF_BASIS,                 &
                                    CELLS
use constants_mod,           only : r_def
use biperiodic_deppt_config_mod, only : n_dep_pt_iterations
use timestepping_config_mod, only : dt

implicit none

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
!> The type declaration for the kernel. Contains the metadata needed by the Psy layer
type, public, extends(kernel_type) :: calc_departure_point_kernel_type
  private
  type(arg_type) :: meta_args(2) = (/                                  &
       arg_type(GH_FIELD,   GH_WRITE, W3),                             &
       arg_type(GH_FIELD,   GH_READ,  W2)                              &
       /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass ::calc_departure_point_code
end type

!-------------------------------------------------------------------------------
! Constructors
!-------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface calc_departure_point_kernel_type
   module procedure calc_departure_point_kernel_constructor
end interface

public calc_departure_point_code

contains

type(calc_departure_point_kernel_type) function calc_departure_point_kernel_constructor() result(self)
  return
end function calc_departure_point_kernel_constructor

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
!> @brief Subroutine to calculate the departure point
!! @param[in]    nlayers Number of model levels
!! @param[inout] dep_pts Departure point values in W2 space
!! @param[in]    departure_pt_stencil_length  Length of stencil
!! @param[in]    undf_w2 Number of unique degrees of freedom for W2
!! @param[in]    ndf_w2 Number of degrees of freedom per cell in W2
!! @param[in]    stencil_map_w2  Stencil map in W2 space in Y direction
!! @param[in]    u_n Wind in W2 space at time n
!! @param[in]    u_np1 Wind in W2 space at time n+1
!! @param[in]    direction Direction in which to calculate departure points
!! @param[in]    dep_pt_method Enumaration of method to use
subroutine calc_departure_point_code( nlayers,                       &
                                      dep_pts,                       &
                                      departure_pt_stencil_length,   &
                                      undf_w2,                       &
                                      ndf_w2,                        &
                                      stencil_map_w2,                &
                                      u_n,                           &
                                      u_np1,                         &
                                      direction,                     &
                                      dep_pt_method )

  use biperiodic_deppts_mod, only : calc_dep_point
  use cosmic_flux_mod, only : calc_stencil_ordering
  use flux_direction_mod, only : x_direction, y_direction

  integer, intent(in)                     :: nlayers
  integer, intent(in)                     :: undf_w2
  real(kind=r_def), intent(inout)         :: dep_pts(1:undf_w2)
  integer, intent(in)                     :: departure_pt_stencil_length
  integer, intent(in)                     :: ndf_w2
  integer, intent(in)                     :: stencil_map_w2(1:ndf_w2,1:departure_pt_stencil_length)
  real(kind=r_def), intent(in)            :: u_n(1:undf_w2)
  real(kind=r_def), intent(in)            :: u_np1(1:undf_w2)
  integer, intent(in)                     :: direction
  integer, intent(in)                     :: dep_pt_method

  real(kind=r_def)     :: xArrival
  real(kind=r_def)     :: u_n_local(1:departure_pt_stencil_length+1)
  real(kind=r_def)     :: u_np1_local(1:departure_pt_stencil_length+1)

  integer              :: nCellEdges
  integer              :: k, df, df1, df2
  integer              :: stencil_order_out(1:departure_pt_stencil_length)

  nCellEdges = departure_pt_stencil_length+1

  call calc_stencil_ordering(departure_pt_stencil_length,stencil_order_out)

  if (direction .EQ. x_direction) then
    df1 = 1
    df2 = 3
  elseif (direction .EQ. y_direction) then
    df1 = 2
    df2 = 4
  end if

  do k=0, nlayers-1

    do df=1,nCellEdges-1
      u_n_local(df)   = u_n(stencil_map_w2(df1,stencil_order_out(df))+k)
      u_np1_local(df) = u_np1(stencil_map_w2(df1,stencil_order_out(df))+k)
    end do
    u_n_local(nCellEdges)   = u_n(stencil_map_w2(df2,stencil_order_out(nCellEdges-1))+k)
    u_np1_local(nCellEdges) = u_np1(stencil_map_w2(df2,stencil_order_out(nCellEdges-1))+k)

    ! xArrival = 0.0 represents the arrival point at a flux edge in a finite
    ! element cell in the local coordinates
    xArrival = 0.0_r_def

    dep_pts(stencil_map_w2(df1,1)+k) = calc_dep_point(  xArrival,             &
                                                        nCellEdges,           &
                                                        u_n_local,            &
                                                        u_np1_local,          &
                                                        dt,                   &
                                                        dep_pt_method,        &
                                                        n_dep_pt_iterations )

    ! xArrival = 1.0 represents the opposite flux edge in a finite element cell
    ! in the local coordinates
    xArrival = 1.0_r_def

    dep_pts(stencil_map_w2(df2,1)+k) = calc_dep_point(  xArrival,             &
                                                        nCellEdges,           &
                                                        u_n_local,            &
                                                        u_np1_local,          &
                                                        dt,                   &
                                                        dep_pt_method,        &
                                                        n_dep_pt_iterations )

  end do

end subroutine calc_departure_point_code

end module calc_departure_point_kernel_mod
