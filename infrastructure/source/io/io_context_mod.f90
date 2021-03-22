!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Specifies the interface for all I/O context classes.
!>
module io_context_mod

  use clock_mod, only : clock_type

  implicit none

  private

  !> @brief All context classes inherit this interface.
  !>
  type, public, abstract :: io_context_type
    private
  contains
    private
    procedure(get_clock_if), public, deferred :: get_clock
  end type io_context_type

  abstract interface
    !> Gets the clock associated with this context.
    !>
    !> @return Clock object.
    !>
    function get_clock_if( this ) result(clock)
      import clock_type, io_context_type
      implicit none
      class(io_context_type), intent(in), target :: this
      class(clock_type), pointer :: clock
    end function get_clock_if
  end interface

  !> @brief Interface for initialisation callback classes.
  !>
  !> @todo Not sure this is necessary. May just be overcomplicating matters.
  !>
  type, public, abstract :: io_context_initialiser_type
    private
  contains
    private
    procedure(callback_if), public, deferred :: callback
  end type io_context_initialiser_type

  abstract interface
    !> @brief Called once context is set up but before initialisation is
    !>        complete.
    !>
    !> @param[in] context  The object under initialisation.
    !>
    subroutine callback_if( this, context )
      import io_context_type, io_context_initialiser_type
      implicit none
      class(io_context_initialiser_type), intent(inout) :: this
      class(io_context_type),             intent(in)    :: context
    end subroutine callback_if
  end interface

contains

end module io_context_mod
