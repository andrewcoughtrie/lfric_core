!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Simple context supporting only a simple time-step clock.
!>
module simple_io_context_mod

  use clock_mod,         only : clock_type
  use constants_mod,     only : i_native, r_second
  use field_mod,         only : field_type
  use io_context_mod,    only : io_context_type
  use linked_list_mod,   only : linked_list_type
  use log_mod,           only : log_event, log_level_error
  use model_clock_mod,   only : model_clock_type
  use step_calendar_mod, only : step_calendar_type

  implicit none

  private

  !> @brief I/O context with no external dependencies.
  !>
  !> This context may be used where simplified I/O which does not require
  !> additional libraries is desirable.
  !>
  type, public, extends(io_context_type) :: simple_io_context_type
    private
    type(linked_list_type), allocatable :: filelist
  contains
    private
    procedure, public :: initialise
    procedure, public :: get_filelist
  end type simple_io_context_type

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> @brief Set up a declared simple_io_context object.
  !>
  !> @param [in]     id                Unique identifying string.
  !> @param [in]     communicator      MPI communicator used by context.
  !> @param [in]     chi               Array of coordinate fields
  !> @param [in]     panel_id          Panel ID field
  !> @param [in]     alt_coords        Array of coordinate fields for alternative meshes
  !> @param [in]     alt_panel_ids     Panel ID fields for alternative meshes
  !>
  subroutine initialise( this,                    &
                         id, communicator,        &
                         chi, panel_id,           &
                         alt_coords, alt_panel_ids )

    implicit none

    class(simple_io_context_type), intent(inout) :: this
    character(*),                  intent(in)    :: id
    integer(i_native),             intent(in)    :: communicator
    class(field_type),             intent(in)    :: chi(:)
    class(field_type),             intent(in)    :: panel_id
    type(field_type),    optional, intent(in)    :: alt_coords(:,:)
    type(field_type),    optional, intent(in)    :: alt_panel_ids(:)

  end subroutine initialise

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Gets the context's file list
  !>
  !> @return  The list of file objects used by the model
  !>
  function get_filelist( this ) result(filelist)

    implicit none

    class(simple_io_context_type), intent(in), target :: this
    type(linked_list_type), pointer :: filelist

    filelist => this%filelist

  end function get_filelist
end module simple_io_context_mod
