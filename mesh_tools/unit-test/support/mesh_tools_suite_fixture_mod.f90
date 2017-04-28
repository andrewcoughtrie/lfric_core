!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> Set up and tear down for whole-suite fixtures.
!>
module mesh_tools_suite_fixture_mod

  use ESMF

  implicit none
  private
  public :: get_esmf_handle, set_up_suite, tear_down_suite

  type(ESMF_VM) :: vm

contains

  !> Get the ESMF virtual machine handle.
  !>
  type(ESMF_VM) function get_esmf_handle()

    implicit none

    get_esmf_handle = vm

  end function get_esmf_handle

  !> Set up any whole-suite fixtures.
  !> This is called by pFUnit, not developer code.
  !>
  subroutine set_up_suite()

    implicit none

    integer :: rc

    call ESMF_Initialize( vm=vm, &
                          defaultlogfilename="dynamo.Log", &
                          logkindflag=ESMF_LOGKIND_MULTI, &
                          rc=rc )
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize( endflag=ESMF_END_ABORT )

  end subroutine set_up_suite

  !> Tear down any whole-suite fixtures.
  !> This is called by pFUnit, not developer code.
  !>
  subroutine tear_down_suite()

    implicit none

    integer :: rc

    ! MPI must be kept open so that subsequent finalisation can close it.
    !
    call ESMF_Finalize( endflag=ESMF_END_KEEPMPI, rc=rc )

  end subroutine tear_down_suite

end module mesh_tools_suite_fixture_mod
