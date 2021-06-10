!-----------------------------------------------------------------------------
! (c) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Drives the execution of the multires_coupling miniapp.
!>
!> This a base structure from which further physics-dynamics coupling development can
!> be done. Constant wind, rho and theta fields are intialised on a dynamics mesh, and
!> are then mapped to a physics mesh and an output mesh. A test algorithm can be called
!> in order to test the scalar mapping and setting up fields on different meshes
!>
module multires_coupling_driver_mod

  use multires_coupling_config_mod,             only : output_mesh_name,   &
                                                       dynamics_mesh_name, &
                                                       physics_mesh_name,  &
                                                       multires_coupling_mode,      &
                                                       multires_coupling_mode_test
  use geometric_constants_mod,                  only : get_coordinates,     &
                                                       get_coordinates_xyz, &
                                                       get_panel_id
  use checksum_alg_mod,                         only : checksum_alg
  use clock_mod,                                only : clock_type
  use constants_mod,                            only : i_def, i_native, &
                                                       str_def
  use lfric_xios_io_mod,                        only : initialise_xios
  use io_config_mod,                            only : write_diag,     &
                                                       use_xios_io,    &
                                                       nodal_output_on_w3
  use io_context_mod,                           only : io_context_type
  use mesh_collection_mod,                      only : mesh_collection
  use log_mod,                                  only : log_event,        &
                                                       LOG_LEVEL_ALWAYS, &
                                                       LOG_LEVEL_INFO
  use simple_io_mod,                            only : initialise_simple_io
  use multires_coupling_mod,                    only : program_name
  use coupling_test_alg_mod,                    only : coupling_test_alg
  use time_config_mod,                          only : timestep_start, &
                                                       timestep_end
  use timestepping_config_mod,                  only : dt, &
                                                       spinup_period
  use xios,                                     only : xios_context_finalize
  use init_multires_coupling_model_mod,         only : initialise_multires_coupling_model, &
                                                       final_multires_coupling_model
  use multires_coupling_prognostics_mod,        only : init_multires_coupling_prognostics
  use multires_coupling_infrastructure_mod,     only : initialise_infrastructure, &
                                                       finalise_infrastructure
  use multires_coupling_diagnostics_driver_mod, only : multires_coupling_diagnostics_driver
  use field_collection_mod,                     only : field_collection_type
  use field_mod,                                only : field_type

  implicit none

  private
  public initialise, run, finalise

  class(io_context_type), allocatable :: io_context

  ! Prognostic fields
  type( field_collection_type ) :: output_prognostic_fields
  type( field_collection_type ) :: dynamics_prognostic_fields
  type( field_collection_type ) :: physics_prognostic_fields

  ! Primary coordinate field
  type(field_type), target, dimension(3) :: prime_chi_xyz
  type(field_type), target, dimension(3) :: prime_chi_sph
  type(field_type), target               :: prime_panel_id
  type(field_type), target, dimension(3) :: prime_shifted_chi_xyz
  type(field_type), target, dimension(3) :: prime_shifted_chi_sph
  type(field_type), target, dimension(3) :: prime_double_level_chi_xyz
  type(field_type), target, dimension(3) :: prime_double_level_chi_sph

  ! Output Coordiante Fields
  type(field_type), pointer :: output_chi_xyz(:)
  type(field_type), pointer :: output_chi_sph(:)
  type(field_type), pointer :: output_panel_id

  ! Multires Coupling coordinate fields
  type(field_type), target, allocatable  :: chi_xyz_fields(:,:)
  type(field_type), target, allocatable  :: chi_sph_fields(:,:)
  type(field_type), target, allocatable  :: panel_id_fields(:)

  ! Multigrid coordinate fields
  type(field_type), allocatable :: chi_mg_sph(:,:)
  type(field_type), allocatable :: panel_id_mg(:)

  ! Primary mesh ids
  integer(kind=i_def) :: prime_mesh_id
  integer(kind=i_def) :: prime_2D_mesh_id
  integer(kind=i_def) :: prime_shifted_mesh_id
  integer(kind=i_def) :: prime_double_level_mesh_id

  ! Output mesh ids
  integer(kind=i_def) :: output_mesh_id
  integer(kind=i_def) :: output_2D_mesh_id

  ! Output 2d mesh name
  character(str_def) :: output_2D_mesh_name

  ! Dynamics mesh ids
  integer(kind=i_def) :: dynamics_mesh_id

  ! Physics mesh ids
  integer(kind=i_def) :: physics_mesh_id

  ! Multigrid mesh ids
  integer(kind=i_def), allocatable :: multigrid_mesh_ids(:)
  integer(kind=i_def), allocatable :: multigrid_2D_mesh_ids(:)

  ! Multires Coupling mesh ids
  integer(kind=i_def), allocatable :: multires_coupling_mesh_ids(:)
  integer(kind=i_def), allocatable :: multires_coupling_2D_mesh_ids(:)

  character(*), public, parameter   :: xios_ctx = 'multires_coupling'

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Sets up required state in preparation for run.
  !>
  subroutine initialise( filename, model_communicator )

    implicit none

    character(:),      intent(in), allocatable :: filename
    integer(i_native), intent(in)              :: model_communicator

    call log_event( 'Initialising Infrastructure...', LOG_LEVEL_ALWAYS )
    call initialise_infrastructure( model_communicator, filename, program_name )

    !-------------------------------------------------------------------------
    ! Model init
    !-------------------------------------------------------------------------
    call log_event( 'Initialising '//program_name//' ...', LOG_LEVEL_ALWAYS )

    !-------------------------------------------------------------------------
    ! Initialise meshes, FEM and runtime constants
    !-------------------------------------------------------------------------

    call initialise_multires_coupling_model( prime_mesh_id, prime_2D_mesh_id,           &
                                             prime_shifted_mesh_id,                     &
                                             prime_double_level_mesh_id,                &
                                             multires_coupling_mesh_ids,                &
                                             multires_coupling_2D_mesh_ids,             &
                                             multigrid_mesh_ids, multigrid_2D_mesh_ids, &
                                             prime_chi_xyz, prime_chi_sph,              &
                                             prime_panel_id,                            &
                                             prime_shifted_chi_xyz,                     &
                                             prime_shifted_chi_sph,                     &
                                             prime_double_level_chi_xyz,                &
                                             prime_double_level_chi_sph,                &
                                             chi_xyz_fields, chi_sph_fields,            &
                                             panel_id_fields,                           &
                                             chi_mg_sph, panel_id_mg )

    ! Assign mesh ids panel id fields and coordinate fields
    output_2D_mesh_name = trim(output_mesh_name)//'_2d'
    output_mesh_id = mesh_collection%get_mesh_id(output_mesh_name)
    output_2D_mesh_id = mesh_collection%get_mesh_id(output_2D_mesh_name)
    dynamics_mesh_id = mesh_collection%get_mesh_id(dynamics_mesh_name)
    physics_mesh_id = mesh_collection%get_mesh_id(physics_mesh_name)

    output_chi_sph => get_coordinates(output_mesh_id)
    output_chi_xyz => get_coordinates_xyz(output_mesh_id)
    output_panel_id => get_panel_id(output_mesh_id)

    !-------------------------------------------------------------------------
    ! IO init
    !-------------------------------------------------------------------------
    call log_event( 'Initialising IO...', LOG_LEVEL_ALWAYS )

    if (use_xios_io) then
      call initialise_xios( io_context,          &
                            xios_ctx,            &
                            model_communicator,  &
                            output_mesh_id,      &
                            output_2D_mesh_id,   &
                            output_chi_xyz,      &
                            output_panel_id,     &
                            timestep_start,      &
                            timestep_end,        &
                            spinup_period,       &
                            dt )
    else
      call initialise_simple_io( io_context,     &
                                 timestep_start, &
                                 timestep_end,   &
                                 spinup_period,  &
                                 dt )
    end if

    !-------------------------------------------------------------------------
    ! Create and initialise prognostic fields
    !-------------------------------------------------------------------------
    call log_event( 'Creating and Initialising Prognostic Fields...', LOG_LEVEL_ALWAYS )

    output_prognostic_fields=field_collection_type(name='output_prognostic_fields')
    dynamics_prognostic_fields=field_collection_type(name='dynamics_prognostic_fields')
    physics_prognostic_fields=field_collection_type(name='physics_prognostic_fields')

    ! Creates and initialises prognostic fields for all model components
    call init_multires_coupling_prognostics( output_mesh_id,             &
                                             dynamics_mesh_id,           &
                                             physics_mesh_id,            &
                                             output_prognostic_fields,   &
                                             dynamics_prognostic_fields, &
                                             physics_prognostic_fields )

  end subroutine initialise

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Performs time steps.
  !>
  subroutine run()

    implicit none

    class(clock_type), pointer :: clock
    logical                    :: running

    clock => io_context%get_clock()
    running = clock%tick()

    ! Call coupling test algorithm
    if ( multires_coupling_mode == multires_coupling_mode_test ) then
      call coupling_test_alg()
    end if

    ! Write out output file
    call log_event(program_name//": Writing depository output", LOG_LEVEL_INFO)

    if ( write_diag ) then
      call multires_coupling_diagnostics_driver( output_mesh_id, dynamics_mesh_id, &
                                                 output_prognostic_fields,         &
                                                 dynamics_prognostic_fields,       &
                                                 clock, nodal_output_on_w3 )
    end if

  end subroutine run

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Finalise after run
  !>
  subroutine finalise()

    implicit none

    type(field_type), pointer :: output_u => null()
    type(field_type), pointer :: output_rho => null()
    type(field_type), pointer :: output_theta => null()

    !-----------------------------------------------------------------------------
    ! Model finalise
    !-----------------------------------------------------------------------------
    call log_event( 'Finalising '//program_name//' ...', LOG_LEVEL_ALWAYS )

    output_u => output_prognostic_fields%get_field('u')
    output_rho => output_prognostic_fields%get_field('rho')
    output_theta => output_prognostic_fields%get_field('theta')

    ! Write checksums to file
    call checksum_alg( program_name, output_rho, 'rho',     &
                                     output_u , 'u',        &
                                     output_theta , 'theta' )
    call log_event( program_name//': Miniapp completed', LOG_LEVEL_INFO )

    !-------------------------------------------------------------------------
    ! Driver layer finalise
    !-------------------------------------------------------------------------

   ! Finalise XIOS context if we used it for diagnostic output or checkpointing
    if ( use_xios_io ) then
      call xios_context_finalize()
    end if

    call final_multires_coupling_model()
    call finalise_infrastructure( program_name )

  end subroutine finalise

end module multires_coupling_driver_mod
