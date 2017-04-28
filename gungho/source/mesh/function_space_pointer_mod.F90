!------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!------------------------------------------------------------------------------
!
!> Function space pointer

module function_space_pointer_mod

use constants_mod,        only: i_def
use linked_list_data_mod, only: linked_list_data_type
use function_space_mod,   only: function_space_type

implicit none

private

!> Linked list data object that contains a pointer to a function space object.
!> This function_space_pointer_type is required as a work through, as the
!> linked list class can only hold objects which are children of the
!> linked_list_data_type.
!> 
type, extends(linked_list_data_type), public :: function_space_pointer_type

  private

  type(function_space_type), pointer :: function_space_target => null()

  !> An unused allocatable integer that prevents an intenal compiler error
  !> with the Gnu Fortran compiler
  !>
  ! Adding an allocatable forces the compiler to accept that the object
  ! has a finaliser. It gets confused without it. This is a workaround for
  ! GCC bug id 61767 - when this bug is fixed, the integer can be removed.
  integer(i_def), allocatable :: dummy_for_gnu

contains

  !> Gets the function space that this object refers to.
  !> @return function_space_target Pointer to the function space object.
  procedure, public :: get_target

end type function_space_pointer_type

interface function_space_pointer_type
  module procedure function_space_pointer_constructor
end interface

!===============================================================================
contains ! Module procedures

!> Instantiates a function space pointer object
!> @return instance A function space pointer object
!===============================================================================
function function_space_pointer_constructor(function_space) result(instance)

  implicit none

  type(function_space_pointer_type) :: instance
  type(function_space_type), intent(in), pointer :: function_space

  instance%function_space_target => function_space

  return
end function function_space_pointer_constructor


function get_target(self) result(function_space_target)

  implicit none

  class(function_space_pointer_type), intent(in), target :: self
  type(function_space_type), pointer :: function_space_target

  function_space_target => self%function_space_target

end function get_target

!===============================================================================
end module function_space_pointer_mod
