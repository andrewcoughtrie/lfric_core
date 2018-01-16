!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

!> @page gravity_wave_minapp Gravity Wave Miniapp
!> Test program for the automatic generation of boundary condition enforcement
!> by PSyclone.

!> @brief Main program used to simulate the linear gravity waves equations.

program gravity_wave

  use constants_mod,                  only : i_def
  use cli_mod,                        only : get_initial_filename
  use gravity_wave_mod,               only : load_configuration
  use init_mesh_mod,                  only : init_mesh
  use init_fem_mod,                   only : init_fem
  use init_gravity_wave_mod,          only : init_gravity_wave
  use ESMF
  use field_mod,                      only : field_type
  use finite_element_config_mod,      only : element_order
  use operator_mod,                   only : operator_type
  use gravity_wave_alg_mod,           only : gravity_wave_alg_init, &
                                             gravity_wave_alg_step
  use log_mod,                        only : log_event,         &
                                             log_set_level,     &
                                             log_scratch_space, &
                                             LOG_LEVEL_ERROR,   &
                                             LOG_LEVEL_INFO,    &
                                             LOG_LEVEL_DEBUG,   &
                                             LOG_LEVEL_TRACE,   &
                                             log_scratch_space
  use restart_config_mod,             only : restart_filename => filename
  use restart_control_mod,            only : restart_type
  use derived_config_mod,             only : set_derived_config
  use output_config_mod,              only : diagnostic_frequency, &
                                             subroutine_timers, &
                                             write_nodal_output, &
                                             write_xios_output
  use timestepping_config_mod,        only : dt
  use checksum_alg_mod,               only : checksum_alg
  use timer_mod,                      only : timer, output_timer

  use global_mesh_collection_mod,    only: global_mesh_collection, &
                                           global_mesh_collection_type
  use function_space_chain_mod,      only: function_space_chain_type

  use io_mod,                         only : output_nodal, &
                                             output_xios_nodal, &
                                             xios_domain_init

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
  character(len=*), parameter   :: xios_ctx  = "gungho_gw"

  type(restart_type) :: restart

  integer(i_def)     :: mesh_id, twod_mesh_id

  ! Prognostic fields
  type( field_type ) :: wind, buoyancy, pressure

  ! Coordinate field
  type(field_type), target, dimension(3) :: chi

  integer(i_def)     :: timestep, ts_init, dtime

  ! Function space chains
  type(function_space_chain_type) :: multigrid_function_space_chain

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
                       defaultlogfilename="gravity_wave.Log", &
                       logkindflag=ESMF_LOGKIND_MULTI, &
                       mpiCommunicator=comm, &
                       rc=rc)

  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to initialise ESMF.', LOG_LEVEL_ERROR )

  call ESMF_VMGet(vm, localPet=localPET, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to get the ESMF virtual machine.', LOG_LEVEL_ERROR )

  total_ranks = petCount
  local_rank  = localPET

  call log_event( 'Gravity wave simulation running...', LOG_LEVEL_INFO )

  call get_initial_filename( filename )
  call load_configuration( filename )
  call set_derived_config( .false. )
  deallocate( filename )

  restart = restart_type( restart_filename, local_rank, total_ranks )

  !-----------------------------------------------------------------------------
  ! Model init
  !-----------------------------------------------------------------------------
  if ( subroutine_timers ) call timer('gravity wave')

  allocate( global_mesh_collection, &
            source = global_mesh_collection_type() )

  ! Create the mesh
  call init_mesh(local_rank, total_ranks, mesh_id, twod_mesh_id)

  ! Create FEM specifics (function spaces and chi field)
  call init_fem(mesh_id, chi)

  !-----------------------------------------------------------------------------
  ! IO init
  !-----------------------------------------------------------------------------

  ! If xios output then set up XIOS domain and context
  if (write_xios_output) then

    dtime = int(dt)

    call xios_domain_init(xios_ctx, comm, dtime, mesh_id, chi, vm, &
                          local_rank, total_ranks)

  end if


  multigrid_function_space_chain = function_space_chain_type()

  ! Create function space collection and initialise prognostic fields
  call init_gravity_wave( mesh_id, chi, multigrid_function_space_chain, &
                          wind, pressure, buoyancy, restart )

  ! Full global meshes no longer required, so reclaim
  ! the memory from global_mesh_collection
  write(log_scratch_space,'(A)') &
      "Purging global mesh collection."
  call log_event( log_scratch_space, LOG_LEVEL_INFO )
  deallocate(global_mesh_collection)


  ! Output initial conditions
  ! We only want these once at the beginning of a run 
  ts_init = max( (restart%ts_start() - 1), 0 ) ! 0 or t previous.

  if (ts_init == 0) then 

      if (write_nodal_output) then
        call output_nodal('wind',     ts_init, wind,     mesh_id)
        call output_nodal('pressure', ts_init, pressure, mesh_id)
        call output_nodal('buoyancy', ts_init, buoyancy, mesh_id)
      end if

    ! XIOS output
    if (write_xios_output) then

      ! Make sure XIOS calendar is set to first timestep to be computed
      call xios_update_calendar(ts_init+1)
 
      ! Output scalar fields
      call output_xios_nodal("init_wind", wind, mesh_id)
      call output_xios_nodal("init_pressure", pressure, mesh_id)
      call output_xios_nodal("init_buoyancy", buoyancy, mesh_id)

    end if

  end if

  !-----------------------------------------------------------------------------
  ! Model step 
  !-----------------------------------------------------------------------------
  do timestep = restart%ts_start(),restart%ts_end()

    ! Update XIOS calendar
    if (write_xios_output) then
      call log_event( "Gravity Wave: Updating XIOS timestep", LOG_LEVEL_INFO )
      call xios_update_calendar(timestep)
    end if

    call log_event( &
    "/****************************************************************************\ ", &
     LOG_LEVEL_TRACE )
     write( log_scratch_space, '(A,I0)' ) 'Start of timestep ', timestep
     call log_event( log_scratch_space, LOG_LEVEL_INFO )
     if (timestep == restart%ts_start()) then
       call gravity_wave_alg_init(mesh_id, wind, pressure, buoyancy)
     end if

    call gravity_wave_alg_step(wind, pressure, buoyancy)
    write( log_scratch_space, '(A,I0)' ) 'End of timestep ', timestep
    call log_event( log_scratch_space, LOG_LEVEL_INFO )
    call log_event( &
    '\****************************************************************************/ ', &
     LOG_LEVEL_INFO)
    if ( mod(timestep, diagnostic_frequency) == 0 ) then

      if (write_nodal_output) then
        call log_event("Gravity Wave: writing nodal diag output", LOG_LEVEL_INFO)
        call output_nodal('wind',     timestep, wind,     mesh_id)
        call output_nodal('pressure', timestep, pressure, mesh_id)
        call output_nodal('buoyancy', timestep, buoyancy, mesh_id)
      end if
      ! XIOS output
      if (write_xios_output) then
 
        call log_event("Gravity Wave: writing xios output", LOG_LEVEL_INFO)
        call output_xios_nodal('wind',     wind,     mesh_id)
        call output_xios_nodal('pressure', pressure, mesh_id)
        call output_xios_nodal('buoyancy', buoyancy, mesh_id)
     end if

    end if

  end do
  !-----------------------------------------------------------------------------
  ! Model finalise
  !-----------------------------------------------------------------------------

  ! Write checksums to file
  call checksum_alg('gravity_wave', wind, 'wind', buoyancy, 'buoyancy', pressure, 'pressure')

  call log_event( 'Gravity wave simulation completed', LOG_LEVEL_INFO )
  if ( subroutine_timers ) then
    call timer('gravity wave')
    call output_timer()
  end if

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

end program gravity_wave
