!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Specifies the interface for all I/O context classes.
!>
module io_context_mod

  use clock_mod,       only : clock_type
  use constants_mod,   only : i_native, r_second
  use field_mod,       only : field_type
  use linked_list_mod, only : linked_list_type

  implicit none

  private

  !> @brief All context classes inherit this interface.
  !>
  type, public, abstract :: io_context_type
    private
  contains
    private
    procedure(get_filelist_if),    public, deferred :: get_filelist
  end type io_context_type

  abstract interface
    !> Gets the list of files associated with this context.
    !>
    !> @return Linked list of file objects.
    !>
    function get_filelist_if( this ) result(filelist)
      import linked_list_type, io_context_type
      implicit none
      class(io_context_type), intent(in), target :: this
      type(linked_list_type), pointer :: filelist
    end function get_filelist_if
  end interface

contains

end module io_context_mod
