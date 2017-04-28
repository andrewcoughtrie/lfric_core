!------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at
! https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!>
!> @brief A generic interface for objects containing evaluate_function function
!>
!> @details Abstract object containing deferred function evaluate function.
!> Functions that are called depend on the enumerator used and includes BASIS and
!> DIFF_BASIS. This object provides a generic interface for objects containing
!> the evaluate_function function.
!>
!> This type extends linked_list_data_type. This is not because we require lists
!> of evaluate_functions, it is because we require lists of fields which inherit
!> from evaluate_function_type.
module evaluate_function_mod

use constants_mod, only: i_def, r_def
use linked_list_data_mod,  only : linked_list_data_type

implicit none

!-------------------------------------------------------------------------------
! Module parameters
!-------------------------------------------------------------------------------
integer(i_def), public, parameter :: BASIS      = 100
integer(i_def), public, parameter :: DIFF_BASIS = 101

private

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------

type, extends(linked_list_data_type), abstract, public :: evaluate_function_type
  private

contains
  procedure(evaluate_function_interface), deferred :: evaluate_function
end type evaluate_function_type

abstract interface
  function evaluate_function_interface(self, func_to_call, df, xi) result(p)
    import i_def, r_def, evaluate_function_type

    class(evaluate_function_type) :: self
    integer(i_def), intent(in)  :: func_to_call
    integer(i_def), intent(in)  :: df
    real(r_def),    intent(in)  :: xi(3)    
    real(r_def), allocatable    :: p(:)
  end function
end interface

!-------------------------------------------------------------------------------
! Contained functions/subroutines 
!-------------------------------------------------------------------------------
contains

end module evaluate_function_mod
