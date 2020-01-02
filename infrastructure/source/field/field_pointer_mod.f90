!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!
!> @brief A module providing a class to hold a pointer to a field in an
!>        object that is a child of field_parent
!>
!> @details We want to be able hold pointers to fields but when these are
!>          passed around, Fortran has a habit of automatically dereferencing
!>          them for you. If we store them in an object that contains the
!>          pointer - they don't get dereferenced. Also, we want to be able to
!>          polymorphically pass both fields and pointers to fields (so they
!>          appear as children of the same abstract parent type). To do this
!>          the pointer needs to be held in an object that inherits from the
!>          field_parent_type. This module provides that functionality


module field_pointer_mod
  use field_mod,        only: field_type
  use field_parent_mod, only: field_parent_type

  implicit none

  private

! Public types

  type, extends(field_parent_type), public :: field_pointer_type
    !> A pointer to a field
    type(field_type), pointer, public :: field_ptr
  contains
    !> Finaliser for a field pointer object
    final :: field_pointer_destructor
  end type field_pointer_type

  interface field_pointer_type
    module procedure field_pointer_constructor
  end interface

contains

  !> Constructor for a field pointer
  !>
  !> @param [in] field_ptr A pointer to the field that is to be
  !>                       stored as a reference
  !> @return The field_pointer type
  function field_pointer_constructor(field_ptr) result(self)
    implicit none

    type(field_type), pointer :: field_ptr
    type(field_pointer_type) :: self

    self%field_ptr => field_ptr
  end function field_pointer_constructor

  ! Finaliser for a field pointer
  !
  ! The following finaliser doesn't do anything. Without it, the Gnu compiler
  ! tries to create its own, but only ends up producing an Internal Compiler
  ! Error, so it is included here to prevent that.
  subroutine field_pointer_destructor(self)
    implicit none
    type(field_pointer_type), intent(inout) :: self
  end subroutine field_pointer_destructor

end module field_pointer_mod
