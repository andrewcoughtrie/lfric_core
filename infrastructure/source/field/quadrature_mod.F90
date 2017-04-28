!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Contains the routines used for (Gaussian) quadrature.

!> @details This module has a type for the (Gaussian) quadrature and a static
!> copy of the quadrature that is used throughout the model. The first
!> time the quadrature is required, it is created and a pointer to it
!> returned. Subsequent times, the pointer to the already created 
!> quadrature is returned.

module quadrature_mod
use constants_mod,       only: r_def, i_def, PI, EPS
use log_mod,             only: LOG_LEVEL_ERROR, log_event, log_scratch_space
use quadrature_rule_mod, only: quadrature_rule_type
use quadrature_rule_gaussian_mod, only: quadrature_rule_gaussian_type
use quadrature_rule_newton_cotes_mod, only: quadrature_rule_newton_cotes_type

implicit none
private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
type, public :: quadrature_type
  private
  !> Allocatable arrays which holds the values of the gaussian quadrature
  real(kind=r_def), allocatable :: xqp(:), xqp_h(:,:), wqp(:), wqp_h(:)

  !> integer Number of quadrature points in the horizontal
  integer :: nqp_h

  !> integer Number of quadrature points in the vertical
  integer :: nqp_v

contains

  !> @brief Quadrature rule functor to get 1D quadrature points and weights
  !> @param quadrature_rule_type function to be used
  !> @return List of 1D quadrature points and weights
  procedure, private :: quadrature_rule

  !> @brief Returns the 2-d array of quadrature points in the horizontal
  !> @param[in] self The calling quadrature rule
  !> @return xgp_h The array to copy the quadrature points into
  procedure :: get_xqp_h

  !> @brief Returns the 1-d array of vertical quadrature points
  !> @param[in] self The calling quadrature rule
  !> @return xqp_v The array to copy the quadrature points into
  procedure :: get_xqp_v

  !> @brief Returns the 1-d array of horizontal quadrature weights
  !> @param[in] self The calling quadrature rule
  !> @return wqp_h The pointer to the horizontal quadrature weights
  procedure :: get_wqp_h

  !> @brief Returns the 1-d array of vertical quadrature weights
  !> @param[in] self The calling quadrature rule
  !> @return wqp_v The pointer to the vertical quadrature weights
  procedure :: get_wqp_v

  !> @brief Returns the number of quadrature points in the vertical
  !> @param[in] self The calling quadrature rule
  !> @return nqp_v Number of quadrature points in the vertical
  procedure :: get_nqp_v

  !> @brief Returns the number of quadrature points in the horizontal
  !> @param[in] self The calling quadrature rule
  !> @return nqp_h Number of quadrature points in the horizontal
  procedure :: get_nqp_h

  !> @brief Routine to destroy quadrature
  final     :: quadrature_destructor
end type

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------
!> Integer that defines the type of quadrature rule required
integer, public, parameter      :: GAUSSIAN = 1001, &
                                   NEWTON   = 1002

interface quadrature_type
module procedure init_quadrature
end interface
!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
! Initialises the quadrature rule
!-------------------------------------------------------------------------------
!> @brief Initialises the quadrature rule 
!> @param[in] npoints Integer, The npoints of integration, i.e. number of points per
!!            dimension
!> @param[in] rule Integer, quadrature rule
!-------------------------------------------------------------------------------
function init_quadrature(npoints, rule) result (self)

  implicit none

  type(quadrature_type) :: self
  integer, intent(in) :: npoints
  integer, intent(in) :: rule

  real(kind=r_def), allocatable       :: points_weights(:,:)
  type(quadrature_rule_gaussian_type) :: gaussian_quadrature
  type(quadrature_rule_newton_cotes_type) :: newton_cotes_quadrature

    self%nqp_h = npoints**2
    self%nqp_v = npoints

    allocate( self%xqp(self%nqp_v) )
    allocate( self%wqp(self%nqp_v) ) 
    allocate( self%xqp_h(self%nqp_h,2) ) 
    allocate( self%wqp_h(self%nqp_h) )

    self%xqp(:) = 0.0_r_def
    self%wqp(:) = 0.0_r_def
    self%xqp_h(:,:) = 0.0_r_def
    self%wqp_h(:) = 0.0_r_def

    allocate(points_weights(self%nqp_v,2))
    select case (rule)
      case (GAUSSIAN)
        points_weights = self%quadrature_rule( gaussian_quadrature )
      case (NEWTON)
        points_weights = self%quadrature_rule( newton_cotes_quadrature )
      case default
         write(log_scratch_space,'(A,I5)') "quadrature_type:Invalid Quadrature Rule: ",rule
        call log_event(log_scratch_space,LOG_LEVEL_ERROR)
    end select

    call create_quadrature(self, points_weights)
    deallocate(points_weights)

end function init_quadrature 

!-----------------------------------------------------------------------------
! Distribute quadrature points and weights 
!-----------------------------------------------------------------------------
!> @brief Distribute quadrature points (xqp) and weights (wqp)
!> @param[in] self The calling quadrature rule
!> @param[in] points_weights quadrature Points(dim 1) and weights(dim 2)
!> @todo This code is correct for quads but will need modification for
!>       hexes/triangles)
!-----------------------------------------------------------------------------
subroutine create_quadrature(self, points_weights)

  implicit none

  class(quadrature_type) :: self
  real(kind=r_def)       :: points_weights(:,:)
  integer(kind=i_def)    :: i,j,ic

  ! This is correct for quads (will need modification for hexes/triangles)
  self%xqp = points_weights(:,1)
  self%wqp = points_weights(:,2)

  ic = 1
  do i=1,self%nqp_v
    do j=1,self%nqp_v
      self%xqp_h(ic,1) = self%xqp(i)
      self%xqp_h(ic,2) = self%xqp(j)
      self%wqp_h(ic) = self%wqp(i)*self%wqp(j)

      ic = ic + 1
    end do
  end do

  return
end subroutine create_quadrature

!-----------------------------------------------------------------------------
! Returns the quadrature points in the horizontal
!-----------------------------------------------------------------------------
function get_xqp_h(self) result(xqp_h)
  implicit none
  class(quadrature_type), target, intent(in) :: self
  real(kind=r_def), pointer :: xqp_h(:,:)

  xqp_h => self%xqp_h
  return
end function get_xqp_h

!-----------------------------------------------------------------------------
! Returns the quadrature points in the vertical
!-----------------------------------------------------------------------------
function get_xqp_v(self) result(xqp_v)
  implicit none
  class(quadrature_type), target, intent(in) :: self
  real(kind=r_def), pointer :: xqp_v(:)

  xqp_v => self%xqp
  return
end function get_xqp_v

!-----------------------------------------------------------------------------
! Returns the number of quadrature points in the vertical
!-----------------------------------------------------------------------------
function get_nqp_v(self) result(nqp_v)
  implicit none
  class(quadrature_type), intent(in) :: self
  integer :: nqp_v

  nqp_v = self%nqp_v
  return
end function get_nqp_v

!-----------------------------------------------------------------------------
! Returns the number of quadrature points in the horizontal
!-----------------------------------------------------------------------------
function get_nqp_h(self) result(nqp_h)
  implicit none
  class(quadrature_type), intent(in) :: self
  integer :: nqp_h

  nqp_h = self%nqp_h
  return
end function get_nqp_h

!-----------------------------------------------------------------------------
! Returns the quadrature weights in the horizontal
!-----------------------------------------------------------------------------
function get_wqp_h(self) result(wqp_h)
  implicit none
  class(quadrature_type), target, intent(in) :: self
  real(kind=r_def), pointer :: wqp_h(:) 

  wqp_h => self%wqp_h
  return
end function get_wqp_h 

!-----------------------------------------------------------------------------
! Returns the quadrature weights in the vertical
!-----------------------------------------------------------------------------
function get_wqp_v(self) result(wqp_v)
  implicit none
  class(quadrature_type), target, intent(in) :: self
  real(kind=r_def), pointer :: wqp_v(:) 

  wqp_v => self%wqp
  return
end function get_wqp_v 

subroutine quadrature_destructor(self)
  implicit none
  type(quadrature_type) :: self

  if(allocated(self%xqp)) then
     deallocate(self%xqp)
  end if
  if(allocated(self%xqp_h)) then
     deallocate(self%xqp_h)
  end if
  if(allocated(self%wqp)) then
     deallocate(self%wqp)
  end if
  if(allocated(self%wqp_h)) then
     deallocate(self%wqp_h)
  end if
  
  
end subroutine quadrature_destructor

function quadrature_rule(self, quadrature_rule_strategy)
  implicit none

  class(quadrature_type)      :: self
  class(quadrature_rule_type) :: quadrature_rule_strategy
  real(kind=r_def)            :: quadrature_rule(self%nqp_v,2)

  quadrature_rule = quadrature_rule_strategy%quadrature_rule(self%nqp_v)

  return
end function quadrature_rule

end module quadrature_mod
