!-----------------------------------------------------------------------------
! (c) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief A simple module to remove duplicate entries from array inputs.
!>
module remove_duplicates_mod

  use constants_mod, only: i_def, imdi, str_def

  implicit none

  private

  interface remove_duplicates
    module procedure get_unique_int_array
    module procedure get_unique_char_array
  end interface remove_duplicates

  !> @brief Function call for integer and character arrays
  !> @param[in] array_in  Integer/Character array to remove duplicates from
  !> @returns   array_out Pointer to processed array with duplicate removed
  public :: remove_duplicates

contains

!-----------------------------------------------------------------------------
! Private function to remove duplicates from integer arrays
!-----------------------------------------------------------------------------
function get_unique_int_array(array_in) result(array_out)

  implicit none

  integer(i_def), intent(in)  :: array_in(:)
  integer(i_def), allocatable :: array_out(:)
  integer(i_def) :: n_entries, i, j, n_uniques

  integer(i_def), allocatable :: unique_list(:)

  n_entries = size(array_in)
  allocate(unique_list(n_entries))
  unique_list = imdi

  unique_list(1) = array_in(1)
  n_uniques = 1

  do i=1, n_entries
    if (array_in(i) == imdi) exit
    do j=1, n_uniques
      if (array_in(i) == unique_list(j)) then
        exit
      else
        if (j == n_uniques) then
          unique_list(j+1) = array_in(i)
          n_uniques = n_uniques+1
        end if
      end if
    end do
  end do

  if (allocated(array_out)) deallocate(array_out)
  allocate(array_out(n_uniques))
  array_out(:) = unique_list(1:n_uniques)

  if (allocated(unique_list)) deallocate (unique_list)
  return
end function get_unique_int_array

!-----------------------------------------------------------------------------
! Private function to remove duplicates from character arrays
!-----------------------------------------------------------------------------
function get_unique_char_array(array_in) result(array_out)

  implicit none

  character(str_def), intent(in)  :: array_in(:)
  character(str_def), allocatable :: array_out(:)

  integer(i_def) :: n_entries, i, j, n_uniques

  character(str_def), allocatable :: unique_list(:)

  n_entries = size(array_in)
  allocate(unique_list(n_entries))
  unique_list = ''

  unique_list(1) = array_in(1)
  n_uniques = 1

  do i=1, n_entries
    if (array_in(i) == '') exit
    do j=1, n_uniques
      if (array_in(i) == unique_list(j)) then
        exit
      else
        if (j == n_uniques) then
          unique_list(j+1) = array_in(i)
          n_uniques = n_uniques+1
        end if
      end if
    end do
  end do

  if (allocated(array_out)) deallocate(array_out)
  allocate(array_out(n_uniques))
  array_out(:) = unique_list(1:n_uniques)

  if (allocated(unique_list)) deallocate (unique_list)
  return
end function get_unique_char_array

end module remove_duplicates_mod

