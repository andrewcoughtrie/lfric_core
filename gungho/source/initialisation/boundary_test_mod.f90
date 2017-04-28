!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> Boundary test program support functions.
!>
!> Originally these were "block" constructs within the program but neither
!> GNU or Intel Fortran where properly able to cope with that.
!>
module boundary_test_mod

  use log_mod, only : log_event,         &
                      log_scratch_space, &
                      LOG_LEVEL_ERROR,   &
                      LOG_LEVEL_TRACE,   &
                      LOG_LEVEL_DEBUG

  implicit none

  private
  public :: load_configuration

contains

  !> Loads run-time configuration and ensures everything is ship-shape.
  !>
  subroutine load_configuration( filename )

    use gungho_configuration_mod, only : read_configuration, &
                                         ensure_configuration

    implicit none

    character(*), intent(in) :: filename

    character(*), parameter :: &
                            required_configuration(5) = ['finite_element   ', &
                                                         'base_mesh        ', &
                                                         'planet           ', &
                                                         'restart          ', &
                                                         'extrusion        ']

    logical              :: okay
    logical, allocatable :: success_map(:)
    integer              :: i

    allocate( success_map(size(required_configuration)) )

    call read_configuration( filename )

    okay = ensure_configuration( required_configuration, success_map )
    if (.not. okay) then
      write( log_scratch_space, '(A)' ) &
                             'The following required namelists were not loaded:'
      do i = 1,size(required_configuration)
        if (.not. success_map(i)) &
          log_scratch_space = trim(log_scratch_space) // ' ' &
                              // required_configuration(i)
      end do
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    deallocate( success_map )

  end subroutine load_configuration

end module boundary_test_mod
