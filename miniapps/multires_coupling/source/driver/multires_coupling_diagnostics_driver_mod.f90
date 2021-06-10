!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Outputs the diagnostics from the multires_coupling miniapp

!> @details Calls the routine that generates diagnostic output for all
!>          fields used by the multires_coupling miniapp.

module multires_coupling_diagnostics_driver_mod

  use clock_mod,                          only : clock_type
  use constants_mod,                      only : i_def
  use field_mod,                          only : field_type
  use field_collection_mod,               only : field_collection_type
  use multires_coupling_mappings_alg_mod, only : map_w2_vectors_alg, &
                                                 map_scalars_alg
  use diagnostics_io_mod,                 only : write_scalar_diagnostic, &
                                                 write_vector_diagnostic
  implicit none

  private
  public multires_coupling_diagnostics_driver

contains

  !> @brief Outputs the diagnostics from the multires_coupling miniapp by mapping across
  !>        the state from the dynamics mesh to the output mesh. Note in future
  !>        some fields may also be mapped across from the physics mesh
  !> @param [in] output_mesh_id The identifier of the output mesh
  !> @param [in] dynamics_mesh_id The identifier of the dynamics mesh
  !> @param [in,out] output_state A collection containing the fields that will
  !>                be written to diagnostic output
  !> @param [in,out] dynamics_state A collection containing the fields on the
  !>                dynamics mesh
  !> @param [in] clock Model time.
  !> @param [in] W3_project Flag that determines if vector fields should be
  !>                        projected to W3
  subroutine multires_coupling_diagnostics_driver( output_mesh_id, dynamics_mesh_id, &
                                                   output_state, dynamics_state,     &
                                                   clock, W3_project )

    implicit none

    type(field_collection_type), intent(inout) :: output_state
    type(field_collection_type), intent(inout) :: dynamics_state
    integer(kind=i_def),         intent(in)    :: output_mesh_id
    integer(kind=i_def),         intent(in)    :: dynamics_mesh_id
    class(clock_type),           intent(in)    :: clock
    logical,                     intent(in)    :: W3_project

    type(field_type), pointer :: output_u => null()
    type(field_type), pointer :: output_rho => null()
    type(field_type), pointer :: output_theta => null()

    type(field_type), pointer :: dynamics_u => null()
    type(field_type), pointer :: dynamics_rho => null()
    type(field_type), pointer :: dynamics_theta => null()

    ! Can't just iterate through the collection as some fields are scalars
    ! and some fields are vectors, so explicitly extract all fields from
    ! the collection and output each of them
    output_u => output_state%get_field('u')
    output_rho => output_state%get_field('rho')
    output_theta => output_state%get_field('theta')
    dynamics_u => dynamics_state%get_field('u')
    dynamics_rho => dynamics_state%get_field('rho')
    dynamics_theta => dynamics_state%get_field('theta')

    ! Map dynamics state to output state on output mesh
    call map_w2_vectors_alg( dynamics_u, output_u )
    call map_scalars_alg( dynamics_rho, output_rho )
    call map_scalars_alg( dynamics_theta, output_theta )

    ! Calculation and output of diagnostics
    call write_vector_diagnostic( 'u', output_u, &
                                  clock, output_mesh_id, W3_project )
    call write_scalar_diagnostic( 'rho', output_rho, &
                                  clock, output_mesh_id, W3_project )
    call write_scalar_diagnostic( 'theta', output_theta, &
                                  clock, output_mesh_id, W3_project )

  end subroutine multires_coupling_diagnostics_driver

end module multires_coupling_diagnostics_driver_mod
