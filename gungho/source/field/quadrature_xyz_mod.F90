!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Contains quadrature_xyz_type and quadrature_xyz_type

!> @details This module contains the quadrature_xyz_type

!> This type contains points and weights stored in 3D (x-y-z). A proxy  
!> is used to access the data. A type bound procedure 'compute_evaluate' 
!> is also available. This method uses the evaluate_function defined in 
!> objects of class evaluate_function_type (e.g. function space) for the 
!> xyz data points.

module quadrature_xyz_mod
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

!-------------------------------------------------------------------------------
! xyz quadrature type
!-------------------------------------------------------------------------------

type, public, extends(abstract_quadrature_type) :: quadrature_xyz_type
  private

  !> Allocatable arrays which holds the quadrature weights
  real(kind=r_def), allocatable :: weights_xyz(:)

  !> Allocatable arrays which holds the points
  real(kind=r_def), allocatable :: points_xyz(:,:)

  !> Total number of points
  integer(kind=i_def) :: np_xyz

contains

  ! Get a proxy with public pointers to the data in a quadrature_xyz type.
  procedure, public :: get_quadrature_proxy

  ! Evaluates the a function for given set of 3d points
  procedure, public :: compute_evaluate

  ! Destroy the quadrature object
  final :: quadrature_destructor

end type quadrature_xyz_type

!> Psy layer representation of a quadrature_xyz type
!>
!> This is an accessor class that allows access to quadrature_xyz_type 
!> data and information with each element accessed via a public pointer.
!>
type, public :: quadrature_xyz_proxy_type

  private
  !> allocatable arrays which holds the values of the gaussian quadrature
  real(kind=r_def), pointer, public :: weights_xyz(:)  => null()
  real(kind=r_def), pointer, public :: points_xyz(:,:) => null()

  !> Number of points
  integer, public                   :: np_xyz

contains
end type quadrature_xyz_proxy_type

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------
interface quadrature_xyz_type
  module procedure init_quadrature
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

!-------------------------------------------------------------------------------
!> @brief Initialises the xyz quadrature type
!> @param[in] np_xyz integer, The total number of points in x,y and z
!> @param[in] rule quadrature_rule_type, The quadrature rule
function init_quadrature(np_xyz, rule) result (self)

  implicit none

  type(quadrature_xyz_type)               :: self
  integer, intent(in)                     :: np_xyz
  class(quadrature_rule_type), intent(in) :: rule

  self%np_xyz = np_xyz
  call create_quadrature( self, rule )

end function init_quadrature

!> @brief Distribute quadrature points and weights
!> @param[in] self The calling quadrature_type
!> @param[in] rule quadrature_rule_type Quadrature rule to use
!> @todo This code is correct for quads but will need modification for
!>       hexes/triangles)
subroutine create_quadrature(self, rule)

  implicit none

  class(quadrature_xyz_type)              :: self
  class(quadrature_rule_type), intent(in) :: rule

  integer(kind=i_def)           :: i,j,k,ic, np_1d
  real(kind=r_def), allocatable :: points_weights(:,:)

  ! Currently assume that the space is symmetric so that can use the 1-D
  ! quadrature rule
  np_1d = int(self%np_xyz ** (1.0_r_def/3.0_r_def))

  ! Allocate space for the points of points weights in the quad type
  allocate( self%points_xyz(3,self%np_xyz) )
  allocate( self%weights_xyz(self%np_xyz) )

  ! Initialise all to zero
  self%points_xyz(:,:) = 0.0_r_def
  self%weights_xyz(:) = 0.0_r_def

  ! Allocate space for the points and weights of the 1D. The dimension assumes
  ! symmetry in the space
  allocate( points_weights( np_1d,2 ) )

  ! Get a copy of the 1D points and weights
  points_weights = rule % quadrature_rule( np_1d )

  ! Distribute the 1D points and weights
  ic = 1
  do i=1,np_1d
    do j=1,np_1d
      do k=1,np_1d
        self%points_xyz(1,ic) = points_weights(i,1)
        self%points_xyz(2,ic) = points_weights(j,1)
        self%points_xyz(3,ic) = points_weights(k,1)

        self%weights_xyz(ic) = points_weights(i,2)*points_weights(j,2)*points_weights(k,2)

        ic = ic + 1
      end do
    end do
  end do

  deallocate( points_weights )

  return
end subroutine create_quadrature
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Function to create a proxy with access to the data in the
!>        quadrature_xyz_type.
!>
!> @return The proxy type with public pointers to the elements of
!> quadrature_xyz_type.
type(quadrature_xyz_proxy_type ) function get_quadrature_proxy(self)

  implicit none

  class(quadrature_xyz_type), target, intent(in) :: self

  get_quadrature_proxy % points_xyz  => self % points_xyz
  get_quadrature_proxy % weights_xyz => self % weights_xyz
  get_quadrature_proxy % np_xyz      = self % np_xyz

end function get_quadrature_proxy
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Evaluates the a given function for on a set of 3d points
!> @param[in] func_to_call enumerator defining the function to call
!> @param[in] ef object containing the function to evaluate
!> @param[in] ndf integer number of dofs
!> @param[out] basis real 3 dimensional array holding the evaluated
!> function
subroutine compute_evaluate(self, func_to_call, ef, ef_dim, ndf, basis)
  implicit none

  class(quadrature_xyz_type),                           intent(in)  :: self
  class(evaluate_function_type),                        intent(in)  :: ef
  integer(kind=i_def),                                  intent(in)  :: func_to_call
  integer(kind=i_def),                                  intent(in)  :: ef_dim
  integer(kind=i_def),                                  intent(in)  :: ndf
  real(kind=r_def), dimension(ef_dim,ndf,self%np_xyz),  intent(out) :: basis

  ! local variables - loop counters
  integer(kind=i_def) :: df
  integer(kind=i_def) :: qp1

  do qp1 = 1, self%np_xyz
    do df = 1, ndf
      basis(:,df,qp1) = ef%evaluate_function(func_to_call,df,self%points_xyz(:,qp1))
    end do
  end do

end subroutine compute_evaluate


!-------------------------------------------------------------------------------
!> @brief Routine to destroy quadrature
subroutine quadrature_destructor(self)
  implicit none
  type(quadrature_xyz_type) :: self

  if(allocated(self%points_xyz)) deallocate(self%points_xyz)
  if(allocated(self%weights_xyz)) deallocate(self%weights_xyz)
  
end subroutine quadrature_destructor
!-------------------------------------------------------------------------------

end module quadrature_xyz_mod
