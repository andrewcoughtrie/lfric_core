!-----------------------------------------------------------------------------
! (c) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @mainpage solver_miniapp
!> The solver API is abstract so cannot be tested unless with a particular implementation.
!! The mini-app makes an example linear operator, preconditioner and krylov subspace solver.
!! This can then be executed to test the solver api against different compilers.

program solver_miniapp

  use constants_mod,           only: i_def, PRECISION_REAL, str_def
  use convert_to_upper_mod,    only: convert_to_upper
  use cli_mod,                 only: get_initial_filename
  use driver_collections_mod,  only: init_collections, final_collections
  use driver_config_mod,       only: init_config, final_config
  use driver_mesh_mod,         only: init_mesh
  use driver_fem_mod,          only: init_fem
  use driver_log_mod,          only: init_logger, final_logger
  use halo_comms_mod,          only: initialise_halo_comms, &
                                     finalise_halo_comms
  use init_solver_miniapp_mod, only: init_solver_miniapp
  use inventory_by_mesh_mod,   only: inventory_by_mesh_type
  use mpi_mod,                 only: global_mpi, create_comm, destroy_comm
  use field_mod,               only: field_type
  use field_vector_mod,        only: field_vector_type
  use solver_miniapp_alg_mod,  only: solver_miniapp_alg
  use configuration_mod,       only: final_configuration
  use solver_miniapp_mod,      only: solver_required_namelists
  use log_mod,                 only: log_event,            &
                                     log_scratch_space,    &
                                     LOG_LEVEL_ALWAYS,     &
                                     LOG_LEVEL_INFO
  use mesh_mod,                only: mesh_type
  use mesh_collection_mod,     only: mesh_collection
  use namelist_collection_mod, only: namelist_collection_type
  use checksum_alg_mod,        only: checksum_alg

  use base_mesh_config_mod,    only: prime_mesh_name

  implicit none

  character(*), parameter :: program_name = 'solver_miniapp'

  character(:), allocatable :: filename
  type(namelist_collection_type), SAVE :: configuration

  integer(i_def) :: total_ranks, local_rank
  integer(i_def) :: comm = -999

  ! prognostic fields
  type(field_type),    pointer :: chi(:) => null()
  type(field_type),    pointer :: panel_id => null()
  type(field_type)             :: field_1, field_2
  type(field_vector_type)      :: fv_1

  type(mesh_type),     pointer :: mesh => null()
  type(inventory_by_mesh_type) :: chi_inventory
  type(inventory_by_mesh_type) :: panel_id_inventory

  character(str_def)           :: base_mesh_names(1)


  !-----------------------------------------------------------------------------
  ! Driver layer init
  !-----------------------------------------------------------------------------

  ! Initialise MPI communicatios and get a valid communicator
  call create_comm(comm)

  ! Initialise halo functionality
  call initialise_halo_comms( comm )

  ! Save the commmunicator for later use
  call global_mpi%initialise(comm)

  total_ranks = global_mpi%get_comm_size()
  local_rank  = global_mpi%get_comm_rank()

  call get_initial_filename( filename )
  call configuration%initialise( program_name, table_len=10 )
  call init_config( filename, solver_required_namelists, &
                    configuration )
  call init_logger( comm, program_name )
  call init_collections()

  deallocate( filename )

  write(log_scratch_space,'(A)')                        &
      'Application built with '//trim(PRECISION_REAL)// &
      '-bit real numbers'
  call log_event( log_scratch_space, LOG_LEVEL_ALWAYS )

  !-----------------------------------------------------------------------------
  ! model init
  !-----------------------------------------------------------------------------
  call log_event( 'Initialising '//program_name//' ...', LOG_LEVEL_ALWAYS )

  base_mesh_names(1) = prime_mesh_name
  call init_mesh( local_rank, total_ranks, base_mesh_names )

  call init_fem( mesh_collection, chi_inventory, panel_id_inventory )

  ! Create and initialise prognostic fields
  mesh => mesh_collection%get_mesh(prime_mesh_name)
  call init_solver_miniapp( mesh, fv_1 )

  ! Call an algorithm
  call chi_inventory%get_field_array(mesh, chi)
  call panel_id_inventory%get_field(mesh, panel_id)
  call solver_miniapp_alg( fv_1, chi, panel_id )

  ! Write out output file
  call log_event(program_name//": writing diagnostic output", LOG_LEVEL_INFO)

  ! pull the fields from the vector
  call fv_1%export_field( field_1, 1 )
  call fv_1%export_field( field_2, 2 )

  !-----------------------------------------------------------------------------
  ! model finalise
  !-----------------------------------------------------------------------------
  call log_event( 'Finalising '//program_name//' ...', LOG_LEVEL_ALWAYS )

  ! Write checksums to file
  call checksum_alg(program_name, field_1, 'solver_field_1',field_2, 'solver_field_2')

  call log_event( program_name//': Miniapp completed', LOG_LEVEL_INFO )

  !-----------------------------------------------------------------------------
  ! Driver layer finalise
  !-----------------------------------------------------------------------------

  nullify(chi, panel_id, mesh)

  ! Finalise global collections
  call final_collections()

  ! Finalise namelist configurations
  call final_configuration()

  ! Finalise halo functionality
  call finalise_halo_comms()

  call final_logger( program_name )
  call final_config()

  ! Finalise MPI communications
  call global_mpi%finalise()
  call destroy_comm()

end program solver_miniapp