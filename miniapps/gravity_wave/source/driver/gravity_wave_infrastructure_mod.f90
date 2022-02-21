!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Controls infrastructure related information used by the model

module gravity_wave_infrastructure_mod

  use check_configuration_mod,    only : get_required_stencil_depth
  use clock_mod,                  only : clock_type
  use configuration_mod,          only : final_configuration
  use constants_mod,              only : i_def, i_native, PRECISION_REAL, r_def
  use convert_to_upper_mod,       only : convert_to_upper
  use derived_config_mod,         only : set_derived_config
  use gravity_wave_mod,           only : load_configuration
  use log_mod,                    only : log_event,          &
                                         log_set_level,      &
                                         log_scratch_space,  &
                                         initialise_logging, &
                                         finalise_logging,   &
                                         LOG_LEVEL_ALWAYS,   &
                                         LOG_LEVEL_ERROR,    &
                                         LOG_LEVEL_WARNING,  &
                                         LOG_LEVEL_INFO,     &
                                         LOG_LEVEL_DEBUG,    &
                                         LOG_LEVEL_TRACE
  use mesh_mod,                   only : mesh_type
  use mpi_mod,                    only : store_comm, &
                                         get_comm_size, get_comm_rank
  use yaxt,                       only : xt_initialize, xt_finalize
  use io_context_mod,             only : io_context_type
  use field_mod,                  only : field_type
  use mesh_collection_mod,        only : mesh_collection, &
                                         mesh_collection_type
  use local_mesh_collection_mod,  only : local_mesh_collection, &
                                         local_mesh_collection_type
  use driver_fem_mod,             only : init_fem
  use driver_mesh_mod,            only : init_mesh
  use runtime_constants_mod,      only : create_runtime_constants
  use gravity_wave_io_mod,        only : initialise_io
  use formulation_config_mod,     only : l_multigrid

  implicit none

  private
  public initialise_infrastructure, finalise_infrastructure

  character(*), public, parameter   :: xios_ctx = 'gravity_wave'

contains

  !> @brief Initialises the infrastructure used by the model
  !> @param [inout] comm The MPI communicator for use within the model
  !>                     (not XIOS' communicator)
  !> @param [in] filename The name of the configuration namelist file
  !> @param [in] program_name An identifier given to the model begin run
  !> @param [in] xios_id XIOS client identifier
  subroutine initialise_infrastructure(comm,         &
                                       filename,     &
                                       program_name, &
                                       io_context,   &
                                       mesh,         &
                                       twod_mesh)

    use logging_config_mod, only: run_log_level,          &
                                  key_from_run_log_level, &
                                  RUN_LOG_LEVEL_ERROR,    &
                                  RUN_LOG_LEVEL_INFO,     &
                                  RUN_LOG_LEVEL_DEBUG,    &
                                  RUN_LOG_LEVEL_TRACE,    &
                                  RUN_LOG_LEVEL_WARNING
    implicit none

    integer(i_native), intent(in) :: comm
    character(*),      intent(in) :: filename
    character(*),      intent(in) :: program_name
    class(io_context_type), intent(out), allocatable :: io_context
    type(mesh_type),   intent(inout), pointer :: mesh
    type(mesh_type),   intent(inout), pointer :: twod_mesh

    type(field_type), target :: chi(3)
    type(field_type), target :: panel_id

    integer(i_def),   allocatable :: multigrid_mesh_ids(:)
    integer(i_def),   allocatable :: multigrid_2d_mesh_ids(:)
    type(field_type), allocatable :: chi_mg(:,:)
    type(field_type), allocatable :: panel_id_mg(:)

    integer(i_def) :: total_ranks, local_rank, stencil_depth

    integer(i_native) :: log_level

    type(clock_type), pointer :: clock
    real(r_def)               :: dt_model

    ! Initialise YAXT
    call xt_initialize(comm)

    !Store the MPI communicator for later use
    call store_comm(comm)

    ! Get the rank information
    total_ranks = get_comm_size()
    local_rank  = get_comm_rank()

    call initialise_logging(local_rank, total_ranks, program_name)

    call load_configuration( filename )

    select case (run_log_level)
    case( RUN_LOG_LEVEL_ERROR )
      log_level = LOG_LEVEL_ERROR
    case( RUN_LOG_LEVEL_WARNING )
      log_level = LOG_LEVEL_WARNING
    case( RUN_LOG_LEVEL_INFO )
      log_level = LOG_LEVEL_INFO
    case( RUN_LOG_LEVEL_DEBUG )
      log_level = LOG_LEVEL_DEBUG
    case( RUN_LOG_LEVEL_TRACE )
      log_level = LOG_LEVEL_TRACE
    end select

    call log_set_level( log_level )

    write(log_scratch_space,'(A)')                              &
        'Runtime message logging severity set to log level: '// &
        convert_to_upper(key_from_run_log_level(run_log_level))
    call log_event( log_scratch_space, LOG_LEVEL_ALWAYS )

    write(log_scratch_space,'(A)')                        &
        'Application built with '//trim(PRECISION_REAL)// &
        '-bit real numbers'
    call log_event( log_scratch_space, LOG_LEVEL_ALWAYS )

    call set_derived_config( .false. )


    !-------------------------------------------------------------------------
    ! Initialise aspects of the grid
    !-------------------------------------------------------------------------
    allocate( mesh_collection, &
              source=mesh_collection_type() )
    allocate( local_mesh_collection, &
              source = local_mesh_collection_type() )

    ! Get the rank information from the virtual machine
    total_ranks = get_comm_size()
    local_rank  = get_comm_rank()

    stencil_depth = get_required_stencil_depth()

    ! Create the mesh
    call init_mesh( local_rank, total_ranks, stencil_depth, mesh,  &
                    twod_mesh             = twod_mesh,             &
                    multigrid_mesh_ids    = multigrid_mesh_ids,    &
                    multigrid_2D_mesh_ids = multigrid_2D_mesh_ids, &
                    use_multigrid         = l_multigrid )

    ! Create FEM specifics (function spaces and chi field)
    call init_fem( mesh, chi, panel_id,                           &
                   multigrid_mesh_ids    = multigrid_mesh_ids,    &
                   multigrid_2D_mesh_ids = multigrid_2D_mesh_ids, &
                   chi_mg                = chi_mg,                &
                   panel_id_mg           = panel_id_mg,           &
                   use_multigrid         = l_multigrid )

    !-------------------------------------------------------------------------
    ! Initialise aspects of output
    !-------------------------------------------------------------------------

    call initialise_io( comm,      &
                        mesh,      &
                        twod_mesh, &
                        chi,       &
                        panel_id,  &
                        xios_ctx,  &
                        io_context )

    !-------------------------------------------------------------------------
    ! Setup constants
    !-------------------------------------------------------------------------
    clock => io_context%get_clock()
    dt_model = real(clock%get_seconds_per_step(), r_def)

    ! Create runtime_constants object. This in turn creates various things
    ! needed by the timestepping algorithms such as mass matrix operators, mass
    ! matrix diagonal fields and the geopotential field and limited area masks.
    call create_runtime_constants( mesh, twod_mesh,                        &
                                   chi, panel_id, dt_model,                &
                                   mg_mesh_ids    = multigrid_mesh_ids,    &
                                   mg_2D_mesh_ids = multigrid_2D_mesh_ids, &
                                   chi_mg         = chi_mg,                &
                                   panel_id_mg    = panel_id_mg )


  end subroutine initialise_infrastructure


  !> @brief Finalises infrastructure used by the model
  subroutine finalise_infrastructure()

    implicit none

    ! Finalise namelist configurations
    call final_configuration()

    ! Finalise YAXT
    call xt_finalize()

    ! Finalise the logging system
    call finalise_logging()

  end subroutine finalise_infrastructure

end module gravity_wave_infrastructure_mod
