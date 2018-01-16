!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @page io_miniapp IO Miniapp
!> Test program for the XIOS IO implementation. 

!> @brief Main program used to test XIOS setup and output of a field.

program io_miniapp

  use constants_mod,                  only : i_def
  use cli_mod,                        only : get_initial_filename
  use io_miniapp_mod,                 only : load_configuration
  use init_mesh_mod,                  only : init_mesh
  use init_fem_mod,                   only : init_fem
  use init_io_miniapp_mod,            only : init_io_miniapp
  use ESMF
  use field_mod,                      only : field_type
  use finite_element_config_mod,      only : element_order
  use global_mesh_collection_mod,    only: global_mesh_collection, &
                                           global_mesh_collection_type

  use log_mod,                        only : log_event,         &
                                             log_set_level,     &
                                             log_scratch_space, &
                                             LOG_LEVEL_ERROR,   &
                                             LOG_LEVEL_INFO,    &
                                             LOG_LEVEL_DEBUG,   &
                                             LOG_LEVEL_TRACE,   &
                                             log_scratch_space
  use derived_config_mod,             only : set_derived_config
  use io_mod,                         only : output_xios_nodal, &
                                             xios_domain_init
  use timestepping_config_mod,        only : dt

  use xios
  use mpi
  use mod_wait

  implicit none

  character(:), allocatable :: filename

  type(ESMF_VM)      :: vm
  integer(i_def)     :: rc
  integer(i_def)     :: total_ranks, local_rank
  integer(i_def)     :: petCount, localPET, ierr
  integer(i_def)     :: comm = -999

  character(len=*), parameter   :: xios_id   = "lfric_client"
  character(len=*), parameter   :: xios_ctx  = "io_mini"


  integer(i_def)     :: mesh_id, twod_mesh_id

  type( field_type ) :: test_field

  ! Coordinate field
  type(field_type), target, dimension(3) :: chi

  integer(i_def)     :: dtime
  !-----------------------------------------------------------------------------
  ! Driver layer init
  !-----------------------------------------------------------------------------

  ! Initialise MPI
 
  call mpi_init(ierr)

  ! Initialise XIOS and get back the split mpi communicator
  call init_wait()
  call xios_initialize(xios_id, return_comm = comm)

  ! Initialise ESMF using mpi communicator initialised by XIOS
  ! and get the rank information from the virtual machine
  call ESMF_Initialize(vm=vm, &
                       defaultlogfilename="io_mini.Log", &
                       logkindflag=ESMF_LOGKIND_MULTI, &
                       mpiCommunicator=comm, &
                       rc=rc)

  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to initialise ESMF.', LOG_LEVEL_ERROR )

  call ESMF_VMGet(vm, localPet=localPET, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to get the ESMF virtual machine.', LOG_LEVEL_ERROR )

  total_ranks = petCount
  local_rank  = localPET

  call log_event( 'IO mini app running...', LOG_LEVEL_INFO )

  call get_initial_filename( filename )
  call load_configuration( filename )
  call set_derived_config( .true. )
  deallocate( filename )


  !-----------------------------------------------------------------------------
  ! Top-level init
  !-----------------------------------------------------------------------------
  allocate( global_mesh_collection, &
            source = global_mesh_collection_type() )

  ! Create the mesh and function space collection
  call init_mesh(local_rank, total_ranks, mesh_id, twod_mesh_id)

  ! Full global meshes no longer required, so reclaim
  ! the memory from global_mesh_collection
  write(log_scratch_space,'(A)') &
      "Purging global mesh collection."
  call log_event( log_scratch_space, LOG_LEVEL_INFO )
  deallocate(global_mesh_collection)

  ! Create FEM specifics (function spaces and chi field)
  call init_fem(mesh_id, chi)


  !-----------------------------------------------------------------------------
  ! IO init
  !-----------------------------------------------------------------------------

  ! Set up XIOS domain and context

  dtime = int(dt)

  call xios_domain_init(xios_ctx, comm, dtime, mesh_id, chi, vm, local_rank, total_ranks)

  !-----------------------------------------------------------------------------
  ! Model init
  !-----------------------------------------------------------------------------

  ! Create and initialise field
  call init_io_miniapp(mesh_id, chi, test_field)

  !-----------------------------------------------------------------------------
  ! Model step 
  !-----------------------------------------------------------------------------

  ! Update XIOS calendar
  call log_event( "IO Mini App: Updating XIOS timestep", LOG_LEVEL_INFO )
  call xios_update_calendar(1)

  call log_event("IO Mini App: writing xios output", LOG_LEVEL_INFO)
  call output_xios_nodal('test_field', test_field, mesh_id)


  !-----------------------------------------------------------------------------
  ! Driver layer finalise
  !-----------------------------------------------------------------------------

  ! Finalise XIOS contet
  call xios_context_finalize()

  ! Finalise XIOS
  call xios_finalize()

  ! Finalise ESMF
  call ESMF_Finalize(endflag=ESMF_END_KEEPMPI,rc=rc)

  ! Finalise mpi
  call mpi_finalize(ierr)

end program io_miniapp
