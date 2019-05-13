!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!>
!> @brief Holds and manages function spaces created during a model run.
!>
!> @details A container which holds type definition of a collection of
!>          function spaces. The collection holds function spaces as 
!>          singletons. It will handle the creation and storing of 
!>          requested function spaces.
!
module function_space_collection_mod

  use constants_mod,      only: i_def, l_def
  use function_space_mod, only: function_space_type, generate_function_space_id
  use fs_continuity_mod,  only: W0, W1, W2, W2V, W2H, W2broken, W2trace, W3, &
                                Wtheta, Wchi, name_from_functionspace
  use log_mod,            only: log_event, log_scratch_space, &
                                LOG_LEVEL_ERROR, LOG_LEVEL_INFO, LOG_LEVEL_TRACE
  use linked_list_mod,    only: linked_list_type, &
                                linked_list_item_type
  implicit none

  private

  !-----------------------------------------------------------------------------
  ! Type which is is a collection of function spaces held in a linked list
  !-----------------------------------------------------------------------------
  type, public :: function_space_collection_type
    private
    type(linked_list_type) :: fs_list

    !> An unused allocatable integer that prevents an intenal compiler error
    !> with the Gnu Fortran compiler. Adding an allocatable forces the compiler
    !> to accept that the object has a finaliser. It gets confused without it.
    !> This is a workaround for GCC bug id 61767 - when this bug is fixed, the
    !> integer can be removed.
    integer(i_def), allocatable :: dummy_for_gnu

  contains
    procedure, public :: get_fs
    procedure, public :: get_fs_by_id

    procedure, public :: get_fs_collection_size
    procedure, public :: clear
    final             :: function_space_collection_destructor
  end type function_space_collection_type
  !-----------------------------------------------------------------------------

  interface function_space_collection_type
    module procedure function_space_collection_constructor
  end interface

  ! Module level variable to make the function space collection
  ! globally available
  type(function_space_collection_type), public, allocatable :: &
      function_space_collection

contains
!-----------------------------------------------------------------------------
! Construct the function space collection
!-----------------------------------------------------------------------------
!> Function to construct a function space collection

function function_space_collection_constructor() result(self)

  implicit none

  type(function_space_collection_type) :: self

  self%fs_list = linked_list_type()

end function function_space_collection_constructor


!-----------------------------------------------------------------------------
! Get or create a function space
!-----------------------------------------------------------------------------
!> Function to get an instance of a function space from the linked list
!> or create it if it doesn't exist
function get_fs(self, mesh_id, element_order, lfric_fs) &
                result(fs)

  implicit none

  class(function_space_collection_type), intent(inout) :: self
  integer(i_def), intent(in)                           :: mesh_id
  integer(i_def), intent(in)                           :: element_order
  integer(i_def), intent(in)                           :: lfric_fs

  type(function_space_type),   pointer :: fs

  integer(i_def) :: fs_id

  nullify(fs)

  select case (lfric_fs)

  case (W0, W1, W2, W2V, W2H, W2broken, W2trace, W3, WTHETA, WCHI)
  case default
    write(log_scratch_space, '(A,I0,A)')                   &
        'Function space type continuity type (', lfric_fs, &
        ') not defined for LFRic.'
    call log_event(log_scratch_space, LOG_LEVEL_INFO)
    write(log_scratch_space, '(A)') &
        'Available integer ids are: 100-107, corresponding to:'
    call log_event(log_scratch_space, LOG_LEVEL_INFO)
    write(log_scratch_space, '(A)') &
        '[ W0, W1, W2, W2V, W2H, W2broken, W2trace, W3, WTHETA, WCHI ]'
    call log_event(log_scratch_space, LOG_LEVEL_ERROR)
  end select

  if (element_order < 0) then
    write(log_scratch_space, '(A,I0)') &
      'Function space element order must be >= 0   ',element_order
    call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    return
  end if

  ! Generate id for requested function space
  ! can use the passed mesh_id
  fs_id = generate_function_space_id( mesh_id, element_order, lfric_fs )

  fs => self%get_fs_by_id(fs_id)

  if (.not. associated(fs)) then

    call self%fs_list%insert_item( function_space_type( mesh_id,       &
                                                        element_order, &
                                                        lfric_fs) )

    write(log_scratch_space, '(A,2(I0,A))')          &
      'Generated order-',element_order,              &
      ' '//trim(name_from_functionspace(lfric_fs))// &
      '-function space singleton (id:', fs_id,')'
    call log_event(log_scratch_space, LOG_LEVEL_TRACE)

    fs => self%get_fs_by_id(fs_id)

  end if

  return
end function get_fs

!------------------------------------------------------------------------------
!> Function to scan the function space collection for
!> function space with a given id and return a pointer
!> to it. A null pointer is returned if the requested
!> function space does not exist.
!>
!> @param  [in] fs_id <integer> Id of requested function space
!> @return <pointer> Pointer to function space object or null()
function get_fs_by_id(self, fs_id) result(instance)

  implicit none

  class(function_space_collection_type) :: self
  integer(i_def)                        :: fs_id
  type(function_space_type),   pointer  :: instance

  type(linked_list_item_type), pointer  :: loop

  ! Point to head of the function space linked list
  loop => self%fs_list%get_head()

  ! Loop through the linked list
  do
    if ( .not. associated(loop) ) then
      ! Have reach the end of the list so either
      ! the list is empty or at the end of list.
      instance => null()

      loop => self%fs_list%get_tail()
      exit
    end if

    ! Check the id of the payload in the current item
    ! to see if its the one requested.
    if ( fs_id == loop%payload%get_id() ) then
      ! Need to 'cast' the payload as the specific
      ! linked list data type, i.e. function_space_type,
      ! before we can use it.
      select type(v => loop%payload)
      type is (function_space_type)
        instance => v
      end select
      exit
    end if

    loop => loop%next
  end do

  nullify(loop)
  return
end function get_fs_by_id


!----------------------------------------------------------------------------
! Get the size of the function space collection
! (only really used in unit tests)
!-----------------------------------------------------------------------------
!> Function to return the number of function spaces currently
!> held in the collection

function get_fs_collection_size(self) result(fs_list_length)

  implicit none

  class(function_space_collection_type), intent(in)   :: self

  integer(i_def) :: fs_list_length

  fs_list_length = self%fs_list%get_length()
  
  return

end function get_fs_collection_size


!-----------------------------------------------------------------------------
! Clear the function space collection
!-----------------------------------------------------------------------------
!> Function to clear all items from the function space collection
!> linked list
subroutine clear(self)

  implicit none

  class(function_space_collection_type), intent(inout) :: self

  call self%fs_list%clear()
  if (allocated(self%dummy_for_gnu)) deallocate(self%dummy_for_gnu)

  return
end subroutine clear

!-----------------------------------------------------------------------------
! Function space collection destructor
!-----------------------------------------------------------------------------

subroutine function_space_collection_destructor(self)

  implicit none

  type (function_space_collection_type), intent(inout) :: self

  call self%clear()

  return
end subroutine function_space_collection_destructor


end module function_space_collection_mod
