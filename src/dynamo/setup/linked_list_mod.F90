!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @brief Provides linked list functionality

!> @details A collection of simple routines to provide linked list
!> functionality. The list data consists of a single integer value
!> for every entry. This is a doubly-linked list, so it can be
!> traversed in both forwards and backwards directions
module linked_list_mod

use constants_mod, only: i_def

integer(i_def), parameter :: BEFORE = -1
integer(i_def), parameter :: AFTER = 1

 type linked_list_type
   !> The single integer data value for every entry in the list
   integer(i_def) :: dat
   !> Pointer to the next entry in the linked list
   type(linked_list_type), pointer :: prev
   !> Pointer to the previous entry in the linked list
   type(linked_list_type), pointer :: next
 end type linked_list_type

 contains

!> Insert a new item to the linked list
!> @param curr The current position, before or after which the item will be
!>             inserted to the list. On return, curr will point the the newly
!>             inserted item.
!> @param data_value The single integer data value to be inserted into the list.
!> @param placement Optional flag that if set to -1 (minus one) leads the
!>                  routine inserting the item before curr. Omitting
!>                  "placement", or setting it to a value other than -1 (minus
!>                  one) leads to the routine inserting the item after curr.
 subroutine insert_item(curr, data_value, placement)
 type(linked_list_type), pointer, intent (inout) :: curr
 integer(i_def),                  intent (in)    :: data_value
 integer(i_def), optional,        intent (in)    :: placement

 type(linked_list_type), pointer :: new

 allocate(new)
 new%dat=data_value
 if(associated(curr))then
   if ( present(placement) .and. placement == BEFORE ) then
   ! If placement set to BEFORE, insert new list item before curr
     new%prev=>curr%prev
     new%next=>curr
     if(associated(curr%prev))curr%prev%next=>new
     curr%prev=>new
   else
   ! Else, insert the new list item after curr
     new%prev=>curr
     new%next=>curr%next
     if(associated(curr%next))curr%next%prev=>new
     curr%next=>new
   end if
 else
   ! If curr is null, create a new item in a new list
   new%prev=>null()
   new%next=>null()
 end if
 curr=>new
 end subroutine insert_item


!> Insert a new item to the linked list - only if the item does not duplicate
!> any item already on the list.
!> @param start The starting position in the list from which to check for
!>              duplicate entries.
!> @param curr The current position, before or after which items will be
!>             added to the list.
!> @param data_value The single integer data value to be added to the list.
!> @param num_added The number of entries added to the list by this call.
!> @param finish Optional finish point in the list at which to stop searching
!>               for duplicates. If omitted, the search will go on to the end
!>               of the list.
!> @param placement Optional flag that if set to -1 (minus one) leads the
!>                  routine inserting the item before curr. Omitting
!>                  "placement", or setting it to a value other than -1 (minus
!>                  one) leads to the routine inserting the item after curr.
!> @param exclude An optional second list of data values. If the data value
!>                to be added appears in this list, it will not be added
 subroutine insert_unique_item(start, curr, data_value, num_added, &
                            finish, placement, exclude)
 type(linked_list_type),           pointer, intent (in)    :: start
 type(linked_list_type),           pointer, intent (inout) :: curr
 integer(i_def),                            intent (in)    :: data_value
 integer(i_def),                            intent (out)   :: num_added
 integer(i_def),         optional,          intent (in)    :: placement
 type(linked_list_type), optional, pointer, intent (in)    :: finish
 type(linked_list_type), optional, pointer, intent (in)    :: exclude

 type(linked_list_type), pointer :: loop ! the item in the list that is currently being looped over

 ! Check list from start to end of list (or finish if specified) to see if
 ! this item already exists - if not, then add it either before or after curr
 num_added=1
 loop=>start
 do
   if ( .not. associated(loop) .or. &
        (present(finish) .and. associated(loop, finish)) )exit
   if (data_value == loop%dat )then
     num_added=0
     exit
   end if
   loop=>loop%next
 end do 
 if (present(exclude))then
   if (num_added == 1) then
   loop=>exclude
     do
       if ( .not. associated(loop) )exit
       if (data_value == loop%dat )then
         num_added=0
         exit
       end if
       loop=>loop%next
     end do 
   end if
 end if
 if (num_added == 1) then
   call insert_item(curr, data_value, placement)
 end if
 end subroutine insert_unique_item


!> Clear the list and return the memory used
!> @param start The start of the linked list that is to be cleared
 subroutine clear_list(start)
 type(linked_list_type), pointer, intent(inout) :: start

! Temporary space used to clear a list item whilst still allowing access to
! the next item in the list
 type(linked_list_type), pointer :: tmp
 do
   if ( .not. associated(start) )exit
   tmp=>start
   start=>start%next
   deallocate(tmp)
 end do
 end subroutine clear_list

end module linked_list_mod
