!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Collection of small utility procedures for lfric-xios
!>
module lfric_xios_utils_mod

    use xios,           only : xios_date
    use constants_mod,  only : i_def, i_native
    use mesh_mod,       only : mesh_type

    implicit none
    private
    public :: parse_date_as_xios, set_prime_io_mesh, prime_io_mesh_is

    integer(kind=i_def), private, allocatable :: prime_io_mesh_ids(:)

    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> Interpret a string as an XIOS date object.
    !> Expected format is yyyy-mm-dd hh:mm:ss
    !>
    !> @param [in] date_str The string representation of the date
    !> @result     date_obj The xios_date object represented by the input string
    !>
    function parse_date_as_xios( date_str ) result( date_obj )
      implicit none
      character(*), intent(in)      :: date_str
      type(xios_date)               :: date_obj
      integer(i_native)             :: y, mo, d, h, mi, s, size

      size = len(date_str)

      ! Indexing from end to support arbitrarily long year
      read( date_str(1      :size-15), * ) y
      read( date_str(size-13:size-12), * ) mo
      read( date_str(size-10:size-9 ), * ) d
      read( date_str(size-7 :size-6 ), * ) h
      read( date_str(size-4 :size-3 ), * ) mi
      read( date_str(size-1 :size   ), * ) s

      date_obj = xios_date( y, mo, d, h, mi, s )

    end function parse_date_as_xios

    !> @brief Registers a mesh to be used as the primary I/O mesh
    !> @param[in] mesh  The mesh object to be registered
    subroutine set_prime_io_mesh( mesh )

      implicit none

      type(mesh_type), intent(in) :: mesh

      integer(i_def), allocatable :: mesh_id_list(:)

      ! Set up array of ints to hold mesh ids, bring in previous mesh IDs if
      ! already present and add the new mesh ID to the new array
      if (allocated(prime_io_mesh_ids)) then
        allocate(mesh_id_list(size(prime_io_mesh_ids) + 1))
        mesh_id_list(1:size(prime_io_mesh_ids)) = prime_io_mesh_ids
        mesh_id_list(size(prime_io_mesh_ids)+1) = mesh%get_id()
      else
        allocate(mesh_id_list(1))
        mesh_id_list(1) = mesh%get_id()
      end if

      ! Make the new array the main array
      call move_alloc(mesh_id_list, prime_io_mesh_ids)

    end subroutine set_prime_io_mesh

    function prime_io_mesh_is( mesh ) result( mesh_is_prime_io )

      implicit none

      type(mesh_type), intent(in) :: mesh

      logical :: mesh_is_prime_io

      if (.not. allocated(prime_io_mesh_ids) ) then
        mesh_is_prime_io = .false.
      else
        mesh_is_prime_io = (any(prime_io_mesh_ids == mesh%get_id()))
      end if

    end function prime_io_mesh_is

  end module lfric_xios_utils_mod