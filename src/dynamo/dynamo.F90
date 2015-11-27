!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!> @mainpage Dynamo
!> Illustration of the PSyKAl (Parallel-system/Kernel/Algorithm) architecture
!> for Gung Ho. Whilst the computational and optimisation infrastructure is
!> being developed, the science code is being developed using
!> a hand-rolled PSy layer, PSy-lite. A PSyKAl-lite needs a dynamo!
!> Eventually, PSyKAl-lite will be replaced with the real PSy and Dynamo
!> will be the implementation of the Gung Ho dynamical core.

!> @brief Main program used to illustrate dynamo functionality.

!> @details Calls Creates function spaces, then fields on those
!> function spaces, before passing the fields to the algorithm layer

program dynamo

  use ESMF
  use constants_mod,           only : i_def, str_max_filename
  use configuration_mod,       only : configure_dynamo,        &
                                      l_nonlinear,             &
                                      ITIMESTEP_SEMI_IMPLICIT, &
                                      ITIMESTEP_RK_SSP3,       &     
                                      itimestep_option   
  use init_prognostic_fields_alg_mod, &
                               only : init_prognostic_fields_alg
  use iter_timestep_alg_mod,   only : iter_timestep_alg
  use rk_alg_timestep_mod,     only : rk_alg_timestep
  use lin_rk_alg_timestep_mod, only : lin_rk_alg_timestep
  use field_mod,               only : field_type
  use function_space_mod,      only : function_space_type, W0, W1, W2, W3,    &
                                      Wtheta, W2V, W2H
  use set_up_mod,              only : set_up
  use assign_coordinate_field_mod, only : assign_coordinate_field
  use field_io_mod,            only : write_state_netcdf                      &
                                    , write_state_plain_text                  &
                                    , read_state_netcdf
  use restart_control_mod,     only : restart_type
  
  use log_mod,                 only : log_event,         &
                                      log_set_level,     &
                                      log_scratch_space, &
                                      LOG_LEVEL_ERROR,   &
                                      LOG_LEVEL_INFO,    &
                                      LOG_LEVEL_DEBUG,   &
                                      LOG_LEVEL_TRACE
  use mesh_mod,                only : mesh_type

  implicit none

  type( function_space_type )      :: function_space

  ! coordinate fields
  type( field_type ) :: chi(3)

  ! prognostic fields    
  type( field_type ) :: u, rho, theta, xi
                  
  type(mesh_type)                  :: mesh
  integer                          :: coord
  type( field_type ), allocatable  :: state(:)
  integer                          :: n_fields
  type(ESMF_VM) :: vm
  integer :: rc
  integer :: total_ranks, local_rank
  integer :: petCount, localPET  
  integer( i_def )                 :: argument_index,  &
                                      argument_length, &
                                      argument_status
  character( 6 )                   :: argument
  type(restart_type)               :: restart

  ! Set defaults for the rank information to be for a serial run
  total_ranks = 1
  local_rank  = 0

  ! Initialise ESMF and get the true rank information from the virtual machine
  ! (although, for now, don't use it - only serial runs are allowed)
  CALL ESMF_Initialize(vm=vm, defaultlogfilename="dynamo.Log", &
                  logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to initialise ESMF.', LOG_LEVEL_ERROR )

  call ESMF_VMGet(vm, localPet=localPET, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call log_event( 'Failed to get the ESMF virtual machine.', LOG_LEVEL_ERROR )

  if (petCount /= 1 .OR. localPet /= 0)then
    call log_event( 'Currently, Dynamo can only run on a single process.', LOG_LEVEL_ERROR )
  endif

  call log_event( 'Dynamo running...', LOG_LEVEL_INFO )

  ! Process command line arguments
  cli_argument_loop: do argument_index = 1, command_argument_count()

    call get_command_argument( argument_index,  &
                               argument,        &
                               argument_length, &
                               argument_status )

    if ( argument_status > 0 ) then
      call log_event( 'Unable to retrieve command line argument', &
                      LOG_LEVEL_ERROR )
    else if ( argument_status < 0 ) then
      write( log_scratch_space, '( A, A, A )' ) "Argument starting >", &
                                                argument,              &
                                                "< is too long"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    if ( argument == '-debug' ) then
      call log_set_level( LOG_LEVEL_TRACE )
      call log_event( 'Switching to full debug output', LOG_LEVEL_DEBUG )
    else
      write( log_scratch_space, '( A, A, A )' ) "Unrecognised argument >", &
                                                trim( argument ), &
                                                "<"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

  end do cli_argument_loop 

  ! Configure Dynamo
  call log_event( "Dynamo: Calling configure_dynamo(restart)", LOG_LEVEL_INFO )
  call configure_dynamo(restart)

  ! Set up mesh and element order
  call set_up(mesh, local_rank, total_ranks)

  ! Calculate coordinates
  do coord = 1,3
    chi(coord) = field_type                                                    &
                 (vector_space = function_space%get_instance(mesh, W0))
  end do
  ! Assign coordinate field
  call log_event( "Dynamo: Computing W0 coordinate fields", LOG_LEVEL_INFO )
  call assign_coordinate_field(mesh, chi)

  ! Create prognostic fields
  theta = field_type(vector_space = function_space%get_instance(mesh, W0))
  xi    = field_type(vector_space = function_space%get_instance(mesh, W1))
  u     = field_type(vector_space = function_space%get_instance(mesh, W2))
  rho   = field_type(vector_space = function_space%get_instance(mesh, W3))

  ! Initialise prognostic fields: Theta, U, Xi, Rho are initialised in 
  ! separate algorithm/subroutine
  call init_prognostic_fields_alg( mesh, chi, u, rho, theta, xi, restart)

  ! Run timestepping algorithms
  if ( l_nonlinear ) then    ! Nonlinear timestepping options

    select case( itimestep_option )
      case( ITIMESTEP_SEMI_IMPLICIT )  ! Semi-Implicit 
        call iter_timestep_alg( mesh, chi, u, rho, theta, xi, restart)
      case( ITIMESTEP_RK_SSP3 )        ! RK SSP3
        call rk_alg_timestep( mesh, chi, u, rho, theta, xi, restart)
      case default
        call log_event("Dynamo: Incorrect time stepping option chosen, "// &
                       "stopping program! ",LOG_LEVEL_ERROR)
        stop
    end select

  else                       ! Linear timestepping options

    select case( itimestep_option )
      case( ITIMESTEP_RK_SSP3 )        ! RK SSP3
        call lin_rk_alg_timestep( mesh, chi, u, rho, theta, restart)
      case default
        call log_event("Dynamo: Only RK SSP3 available for linear equations. ", &
                        LOG_LEVEL_INFO )
        call log_event("Dynamo: Incorrect time stepping option chosen, "// &
                       "stopping program! ",LOG_LEVEL_ERROR)
        stop
    end select
        
  end if

  ! Do some i/o
  call rho%log_field(   LOG_LEVEL_DEBUG, LOG_LEVEL_INFO, 'rho' )
  call theta%log_field( LOG_LEVEL_DEBUG, LOG_LEVEL_INFO, 'theta' )
  call u%log_field(     LOG_LEVEL_DEBUG, LOG_LEVEL_INFO, 'u' )

  if( restart%write_file() ) then 
     n_fields = 1
     allocate(state(4))
     write(log_scratch_space,'(A,A)') "writing file:",  &
          trim(restart%endfname("rho"))
     call log_event(log_scratch_space,LOG_LEVEL_INFO)
     state(1) = rho
     call write_state_netcdf( n_fields, state, trim(restart%endfname("rho")) )

     write(log_scratch_space,'(A,A)') "writing file:",  &
          trim(restart%endfname("u"))
     call log_event(log_scratch_space,LOG_LEVEL_INFO)
     state(2) = u
     call write_state_netcdf( n_fields, state(2), trim(restart%endfname("u")) )

     write(log_scratch_space,'(A,A)') "writing file:",  &
          trim(restart%endfname("theta"))
     call log_event(log_scratch_space,LOG_LEVEL_INFO)
     state(3) = theta
     call write_state_netcdf( n_fields, state(3), trim(restart%endfname("theta")) )

     write(log_scratch_space,'(A,A)') "writing file:",  &
          trim(restart%endfname("xi"))
     call log_event(log_scratch_space,LOG_LEVEL_INFO)
     state(4) = xi
     call write_state_netcdf( n_fields, state(4), trim(restart%endfname("xi")) )
  end if
  n_fields = 3

  call log_event( 'Dynamo completed', LOG_LEVEL_INFO )

  ! Close down ESMF
  call ESMF_Finalize(rc=rc)

end program dynamo
