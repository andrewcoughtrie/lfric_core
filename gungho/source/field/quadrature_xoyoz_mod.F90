!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Contains quadrature_xyoz_type and quadrature_xyoz_type

!> @details This module contains the quadrature_xoyoz_type.
!> 
!> This type contains points and weights stored in 1D for horizontal (x & y) 
!> and vertical (z). A proxy is used to access the data. A type bound procedure 
!> 'compute_evaluate' is also available. This method uses the evaluate_function 
!> defined in objects of class evaluate_function_type (e.g. function space) for 
!> the xoyoz data points.

module quadrature_xoyoz_mod
use constants_mod,           only: r_def, i_def, PI, EPS
use log_mod,                 only: LOG_LEVEL_ERROR, log_event, log_scratch_space
use quadrature_rule_mod,     only: quadrature_rule_type
use abstract_quadrature_mod, only: abstract_quadrature_type
use function_space_mod,      only: function_space_type
use evaluate_function_mod,   only: evaluate_function_type
implicit none
private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, public, extends(abstract_quadrature_type) :: quadrature_xoyoz_type
  private

  !> Allocatable arrays which holds the quadrature weights
  real(kind=r_def), allocatable :: weights_x(:), weights_y(:), weights_z(:)

  !> Allocatable arrays which holds the points (x)
  real(kind=r_def), allocatable :: points_x(:), points_y(:), points_z(:)

  !> Number of points in each direction
  integer(kind=i_def) :: np_x, np_y, np_z

contains

  ! Get a proxy with public pointers to the data in a quadrature_xoyoz type.
  procedure, public :: get_quadrature_proxy

  ! Evaluates the function provided for given set of 3d points
  procedure, public :: compute_evaluate

  ! Destroy the quadrature object
  final :: quadrature_destructor

end type quadrature_xoyoz_type

!> Psy layer representation of a quadrature_xoyoz type
!>
!> This is an accessor class that allows access to quadrature_xoyoz_type 
!> data and information with each element accessed via a public pointer.
!>
type, public :: quadrature_xoyoz_proxy_type

  private
  !> allocatable arrays which holds the values of the points
  real(kind=r_def), pointer, public :: weights_x(:) => null()
  real(kind=r_def), pointer, public :: weights_y(:) => null()
  real(kind=r_def), pointer, public :: weights_z(:) => null()
  real(kind=r_def), pointer, public :: points_x(:)  => null()
  real(kind=r_def), pointer, public :: points_y(:)  => null()
  real(kind=r_def), pointer, public :: points_z(:)  => null()

  !> Number of points
  integer, public                   :: np_x, np_y, np_z

contains
end type quadrature_xoyoz_proxy_type

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------
interface quadrature_xoyoz_type
  module procedure init_quadrature
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!--------------------------------------------------------------------------------
!> @brief Initialises the xoyoz quadrature type
!> @param[in] np_x integer, The number of points in the x-direction
!> @param[in] np_y integer, The number of points in the y-direction
!> @param[in] np_z integer, The number of points in the z-direction
!> @param[in] rule quadrature_rule_type, Quadrature rule to use
function init_quadrature(np_x, np_y, np_z, rule) result (self)

  implicit none

  type(quadrature_xoyoz_type) :: self
  integer, intent(in) :: np_x, np_y, np_z
  class(quadrature_rule_type), intent(in) :: rule

  self%np_x = np_x
  self%np_y = np_y
  self%np_z = np_z

  call create_quadrature( self, rule )

end function init_quadrature
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
!> @brief Distribute quadrature points and weights
!> @param[in] self The calling quadrature type
!> @param[in] rule quadrature_rule_type Quadrature rule to use
subroutine create_quadrature(self, rule)

  implicit none

  class(quadrature_xoyoz_type) :: self
  class(quadrature_rule_type), intent(in) :: rule

  real(kind=r_def), allocatable :: points_weights_x(:,:)
  real(kind=r_def), allocatable :: points_weights_y(:,:)
  real(kind=r_def), allocatable :: points_weights_z(:,:)

  ! Allocate space for the points of points weights in the quad type
  allocate( self%points_x(self%np_x) )
  allocate( self%weights_x(self%np_x) )
  allocate( self%points_y(self%np_y) )
  allocate( self%weights_y(self%np_y) )
  allocate( self%points_z(self%np_z) )
  allocate( self%weights_z(self%np_z) )

  ! Initilise all to zero
  self%points_x(:) = 0.0_r_def
  self%weights_x(:) = 0.0_r_def
  self%points_y(:) = 0.0_r_def
  self%weights_y(:) = 0.0_r_def
  self%points_z(:) = 0.0_r_def
  self%weights_z(:) = 0.0_r_def

  ! Allocate space for the points and weights of the 1D with dimension defined
  ! in quad type
  allocate( points_weights_x( self%np_x,2 ) )
  allocate( points_weights_y( self%np_y,2 ) )
  allocate( points_weights_z( self%np_z,2 ) )

  ! Get a copy of the 1D points and weights
  points_weights_x = rule % quadrature_rule( self%np_x )
  points_weights_y = rule % quadrature_rule( self%np_y )
  points_weights_z = rule % quadrature_rule( self%np_z )

  ! Distribute the 1D points and weights
  ! We cant uses use XoYoZ for non quads as they do not have the threefold symmetry necessary for XoYoZ rules
  self%points_x  = points_weights_x(:,1)
  self%points_y  = points_weights_y(:,1)
  self%points_z  = points_weights_z(:,1)
  self%weights_x = points_weights_x(:,2)
  self%weights_y = points_weights_y(:,2)
  self%weights_z = points_weights_z(:,2)

  deallocate( points_weights_x, points_weights_y, points_weights_z )

  return
end subroutine create_quadrature
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
!> @brief Function to create a proxy with access to the data in the
!>        quadrature_xoyoz_type.
!>
!> @return The proxy type with public pointers to the elements of
!> quadrature_xoyoz_type.
type(quadrature_xoyoz_proxy_type ) function get_quadrature_proxy(self)

  implicit none

  class(quadrature_xoyoz_type), target, intent(in)  :: self

  get_quadrature_proxy % points_x  => self % points_x
  get_quadrature_proxy % points_y  => self % points_y
  get_quadrature_proxy % points_z  => self % points_z
  get_quadrature_proxy % weights_x => self % weights_x
  get_quadrature_proxy % weights_y => self % weights_y
  get_quadrature_proxy % weights_z => self % weights_z
  get_quadrature_proxy % np_x      = self % np_x
  get_quadrature_proxy % np_y      = self % np_y
  get_quadrature_proxy % np_z      = self % np_z

end function get_quadrature_proxy
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
!> @brief Evaluates the a given function for on a set of 3d points
!> @param[in] func_to_call enumerator defining the function to call
!> @param[in] ef object containing the function to evaluate
!> @param[in] ndf integer number of dofs
!> @param[out] basis real 3 dimensional array holding the evaluated
!> function
subroutine compute_evaluate(self, func_to_call, ef, ef_dim, ndf, basis)

  implicit none

  class(quadrature_xoyoz_type), intent(in)                                           :: self
  class(evaluate_function_type), intent(in)                                          :: ef
  integer(kind=i_def), intent(in)                                                    :: func_to_call
  integer(kind=i_def), intent(in)                                                    :: ef_dim
  integer(kind=i_def), intent(in)                                                    :: ndf
  real(kind=r_def), dimension(ef_dim,ndf,self%np_x,self%np_y,self%np_z), intent(out) :: basis

  ! local variables - loop counters
  integer(kind=i_def) :: df
  real(kind=r_def)    :: xyz(3)
  integer(kind=i_def) :: qp1
  integer(kind=i_def) :: qp2
  integer(kind=i_def) :: qp3

  do qp3 = 1, self%np_z
    xyz(3)=self%points_z(qp3)
    do qp2 = 1, self%np_y
      xyz(2)=self%points_y(qp2)
      do qp1 = 1, self%np_x
        xyz(1) = self%points_x(qp1)
        do df = 1, ndf
          basis(:,df,qp1,qp2,qp3) = ef%evaluate_function(func_to_call,df,xyz)
        end do
      end do
    end do
  end do

end subroutine compute_evaluate
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
!> @brief Routine to destroy quadrature
subroutine quadrature_destructor(self)
  implicit none
  type(quadrature_xoyoz_type) :: self

  if(allocated(self%points_x)) deallocate(self%points_x)
  if(allocated(self%points_y)) deallocate(self%points_y)
  if(allocated(self%points_z)) deallocate(self%points_z)
  if(allocated(self%weights_x)) deallocate(self%weights_x)
  if(allocated(self%weights_y)) deallocate(self%weights_y)
  if(allocated(self%weights_z)) deallocate(self%weights_z)
  
end subroutine quadrature_destructor
!--------------------------------------------------------------------------------

end module quadrature_xoyoz_mod
