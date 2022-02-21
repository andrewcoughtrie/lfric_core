!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Outputs diagnostics from linear model

module linear_diagnostics_driver_mod

  use clock_mod,                 only : clock_type
  use constants_mod,             only : i_def, str_def
  use diagnostics_io_mod,        only : write_scalar_diagnostic, &
                                        write_vector_diagnostic
  use field_collection_mod,      only : field_collection_type
  use gungho_model_data_mod,     only : model_data_type
  use field_mod,                 only : field_type
  use formulation_config_mod,    only : use_moisture
  use mesh_mod,                  only : mesh_type
  use mr_indices_mod,            only : nummr, mr_names
  use initialization_config_mod, only : ls_option, &
                                        ls_option_file
  use log_mod,                   only : log_event, &
                                        LOG_LEVEL_INFO

  implicit none

  private
  public linear_diagnostics_driver

contains

  !> @brief Outputs simple diagnostics from Linear model
  !> @param[in] mesh       The primary mesh
  !> @param[in] model_data The working data set for the model run
  !> @param[in] timestep   The timestep at which the fields are valid
  !> @param[in] nodal_output_on_w3 Flag that determines if vector fields
  !>                  should be projected to W3 for nodal output
  subroutine linear_diagnostics_driver( mesh,       &
                                        model_data, &
                                        clock,      &
                                        nodal_output_on_w3 )

    implicit none

    type(mesh_type),       intent(in), pointer :: mesh
    type(model_data_type), intent(in), target  :: model_data
    class(clock_type),     intent(in)          :: clock
    logical,               intent(in)          :: nodal_output_on_w3

    type( field_collection_type ), pointer :: ls_fields => null()
    type( field_type ),            pointer :: ls_mr(:) => null()

    type( field_type), pointer :: ls_theta => null()
    type( field_type), pointer :: ls_u => null()
    type( field_type), pointer :: ls_rho => null()
    type( field_type), pointer :: ls_exner => null()
    type( field_type), pointer :: ls_v_u => null()
    type( field_type), pointer :: ls_h_u => null()

    integer :: i

    call log_event("Linear: writing diagnostic output", LOG_LEVEL_INFO)

    ls_fields => model_data%ls_fields
    ls_mr => model_data%ls_mr

    ls_theta => ls_fields%get_field('ls_theta')
    ls_u => ls_fields%get_field('ls_u')
    ls_rho => ls_fields%get_field('ls_rho')
    ls_exner => ls_fields%get_field('ls_exner')

    ! Scalar fields
    call write_scalar_diagnostic('ls_rho', ls_rho, &
                                 clock, mesh, nodal_output_on_w3)
    call write_scalar_diagnostic('ls_theta', ls_theta, &
                                 clock, mesh, nodal_output_on_w3)
    call write_scalar_diagnostic('ls_exner', ls_exner, &
                                 clock, mesh, nodal_output_on_w3)

    ! Vector fields
    call write_vector_diagnostic('ls_u', ls_u, &
                                 clock, mesh, nodal_output_on_w3)


    ! Fluxes - horizontal and vertical (if reading linearisation
    ! state from file)
    if (ls_option == ls_option_file) then
      ls_v_u => ls_fields%get_field('ls_v_u')
      ls_h_u => ls_fields%get_field('ls_h_u')
      call write_scalar_diagnostic('readls_v_u', ls_v_u, &
                                   clock, mesh, nodal_output_on_w3)
      call write_vector_diagnostic('readls_h_u', ls_h_u, &
                                   clock, mesh, nodal_output_on_w3)
    end if

    ! Moisture fields
    if (use_moisture) then
      do i=1,nummr
        call write_scalar_diagnostic( 'ls_'//trim(mr_names(i)), ls_mr(i), &
                                      clock, mesh, nodal_output_on_w3 )
      end do
    end if

  end subroutine linear_diagnostics_driver

end module linear_diagnostics_driver_mod
