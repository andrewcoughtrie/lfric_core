!-----------------------------------------------------------------------------
! (c) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @mainpage miniapp_skeleton
!> Barebones miniapp that can be taken and adapted for new science development

program miniapp_skeleton

  use constants_mod,                  only : i_def
  use cli_mod,                        only : get_initial_filename
  use miniapp_skeleton_mod,           only : load_configuration
  use init_mesh_mod,                  only : init_mesh
  use init_fem_mod,                   only : init_fem
  use init_miniapp_skeleton_mod,      only : init_miniapp_skeleton
  use ESMF
  use global_mesh_collection_mod,     only : global_mesh_collection, &
                                             global_mesh_collection_type
  use field_mod,                      only : field_type
  use miniapp_skeleton_alg_mod,       only : miniapp_skeleton_alg
  use log_mod,                        only : log_event,         &
                                             log_set_level,     &
                                             log_scratch_space, &
                                             LOG_LEVEL_ERROR,   &
                                             LOG_LEVEL_INFO
  use output_config_mod,              only : write_nodal_output, &
                                             write_xios_output
  use io_mod,                         only : output_nodal, &
                                             output_xios_nodal, &
                                             xios_domain_init
  use checksum_alg_mod,               only : checksum_alg

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
  character(len=*), parameter   :: xios_ctx  = "skeleton_mini"

  integer(i_def)     :: mesh_id

  integer(i_def)     :: dtime

  ! Prognostic fields
  type( field_type ) :: field_1

  ! Coordinate field
  type(field_type), target, dimension(3) :: chi

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
                       defaultlogfilename="miniapp_skeleton.Log", &
                       logkindflag=ESMF_LOGKIND_MULTI, &
                       mpiCommunicator=comm, &
                       rc=rc)



  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to initialise ESMF.', LOG_LEVEL_ERROR )

  call ESMF_VMGet(vm, localPet=localPET, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to get the ESMF virtual machine.', LOG_LEVEL_ERROR )

  total_ranks = petCount
  local_rank  = localPET

  call log_event( 'skeleton miniapp running...', LOG_LEVEL_INFO )

  call get_initial_filename( filename )
  call load_configuration( filename )
  deallocate( filename )

  !-----------------------------------------------------------------------------
  ! Model init
  !-----------------------------------------------------------------------------

  ! Create the mesh and function space collection
  allocate( global_mesh_collection, &
            source = global_mesh_collection_type() )
  call init_mesh(local_rank, total_ranks, mesh_id)
  ! Full global meshes no longer required, so reclaim
  ! the memory from global_mesh_collection
  write(log_scratch_space,'(A)') &
      "Purging global mesh collection."
  call log_event( log_scratch_space, LOG_LEVEL_INFO )
  deallocate(global_mesh_collection)

  ! Create FEM specifics (function spaces and chi field)
  call init_fem(mesh_id, chi)


  ! If xios output then set up XIOS domain and context
  if (write_xios_output) then

    dtime = 1

    call xios_domain_init(xios_ctx, comm, dtime, mesh_id, chi, vm, local_rank, total_ranks)

    ! Make sure XIOS calendar is set to timestep 1 as it starts there
    ! not timestep 0.
    call xios_update_calendar(1)

  end if

  ! Create and initialise prognostic fields
  call init_miniapp_skeleton(mesh_id, chi, field_1)

  ! Call an algorithm
  call miniapp_skeleton_alg(field_1)
  

  ! Write out output file
  call log_event("skeleton miniapp: writing diagnostic output", LOG_LEVEL_INFO)

  ! Original nodal output
  if ( write_nodal_output)  then
    call output_nodal('skeleton_field', 0, field_1, mesh_id)
  end if

  ! XIOS output
  if (write_xios_output) then
    call output_xios_nodal("skeleton_field", field_1, mesh_id)
  end if
  
  !-----------------------------------------------------------------------------
  ! Model finalise
  !-----------------------------------------------------------------------------

  ! Write checksums to file
  call checksum_alg('miniapp_skeleton', field_1, 'skeleton_field_1')

  call log_event( 'Skeleton miniapp completed', LOG_LEVEL_INFO )

  !-----------------------------------------------------------------------------
  ! Driver layer finalise
  !-----------------------------------------------------------------------------

  ! Finalise XIOS context if we used it for IO
  if (write_xios_output) then
    call xios_context_finalize()
  end if

  ! Finalise XIOS
  call xios_finalize()

  ! Finalise ESMF
  call ESMF_Finalize(endflag=ESMF_END_KEEPMPI,rc=rc)

  ! Finalise mpi
  call mpi_finalize(ierr)

end program miniapp_skeleton
