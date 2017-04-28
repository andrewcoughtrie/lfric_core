!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @details boundary_test
!> Test program for the automatic generation of boundary condition enforcement
!> by psyclone

!> @brief Main program used to test the automatic enforcement of boundary
!!        conditions.

program boundary_test

  use boundary_test_alg_mod,          only : boundary_test_alg_init, &
                                             boundary_test_alg
  use boundary_test_mod,              only : load_configuration
  use cli_mod,                        only : get_initial_filename
  use constants_mod,                  only : i_def
  use derived_config_mod,             only : set_derived_config
  use ESMF
  use field_io_mod,                   only : write_state_netcdf
  use field_mod,                      only : field_type
  use init_gungho_mod,                only : init_gungho
  use init_boundary_test_mod,         only : init_boundary_test
  use log_mod,                        only : log_event,         &
                                             log_set_level,     &
                                             log_scratch_space, &
                                             LOG_LEVEL_ERROR,   &
                                             LOG_LEVEL_INFO,    &
                                             LOG_LEVEL_DEBUG,   &
                                             LOG_LEVEL_TRACE,   &
                                             log_scratch_space
  use operator_mod,                   only : operator_type
  use restart_config_mod,             only : restart_filename => filename
  use restart_control_mod,            only : restart_type

  implicit none

  character(:), allocatable :: filename

  type(ESMF_VM)      :: vm
  integer            :: rc
  integer            :: total_ranks, local_rank
  integer            :: petCount, localPET

  type(restart_type) :: restart

  integer            :: mesh_id

  ! prognostic fields
  type( field_type ) :: u, xi

  !-----------------------------------------------------------------------------
  ! Driver layer init
  !-----------------------------------------------------------------------------

  ! Initialise ESMF and get the rank information from the virtual machine
  CALL ESMF_Initialize(vm=vm, defaultlogfilename="boundary_test.Log", &
                  logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to initialise ESMF.', LOG_LEVEL_ERROR )

  call ESMF_VMGet(vm, localPet=localPET, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to get the ESMF virtual machine.', LOG_LEVEL_ERROR )

  total_ranks = petCount
  local_rank  = localPET

  call log_event( 'Boundary test running...', LOG_LEVEL_INFO )

  call get_initial_filename( filename )
  call load_configuration( filename )
  call set_derived_config()
  deallocate( filename )

  restart = restart_type( restart_filename, local_rank, total_ranks )

  !-----------------------------------------------------------------------------
  ! model init
  !-----------------------------------------------------------------------------


  ! Create the mesh and function space collection
  call init_gungho(mesh_id, local_rank, total_ranks)

  ! Create and initialise prognostic fields
  call init_boundary_test(mesh_id, u, xi, restart)

  !-----------------------------------------------------------------------------
  ! model step 
  !-----------------------------------------------------------------------------
  call boundary_test_alg_init(u, xi)
  call boundary_test_alg(u, xi)

  !-----------------------------------------------------------------------------
  ! model finalise
  !-----------------------------------------------------------------------------

  ! Write checksums to file
  open( 9, file="boundary_test-checksums.txt", status="replace", iostat=rc)
  if (rc /= 0) then
    write( log_scratch_space, '("Unable to open checksum file")' )
    call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end if
  call u%write_checksum( 9, 'u' )
  call xi%write_checksum( 9, 'xi' )
  close( 9 )

  call log_event( 'Boundary test completed', LOG_LEVEL_INFO )

  !-----------------------------------------------------------------------------
  ! Driver layer finalise
  !-----------------------------------------------------------------------------

  ! Close down ESMF
  call ESMF_Finalize(rc=rc)

end program boundary_test
