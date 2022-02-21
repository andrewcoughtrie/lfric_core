!-----------------------------------------------------------------------------
! (C) Crown copyright 2021 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Initialises and finalises the linear model numerical schemes.
module linear_model_mod

  use constants_mod,              only : i_def
  use clock_mod,                  only : clock_type
  use field_mod,                  only : field_type
  use field_collection_mod,       only : field_collection_type
  use gungho_model_data_mod,      only : model_data_type
  use tl_rk_alg_timestep_mod,     only : tl_rk_alg_init, &
                                         tl_rk_alg_final
  use tl_si_timestep_alg_mod,     only : tl_semi_implicit_alg_init, &
                                         tl_semi_implicit_alg_final
  use si_operators_alg_mod,       only : final_si_operators
  use semi_implicit_solver_alg_mod, &
                                  only : semi_implicit_solver_alg_final
  use timestepping_config_mod,    only : method,               &
                                         method_semi_implicit, &
                                         method_rk
  use log_mod,                    only : log_event, &
                                         log_scratch_space, &
                                         LOG_LEVEL_INFO,    &
                                         LOG_LEVEL_TRACE,   &
                                         LOG_LEVEL_ERROR
  use mesh_mod,                    only: mesh_type

  implicit none

  private
  public initialise_linear_model, &
         finalise_linear_model

contains

  !> @brief Completes the initialisation of the tangent linear model
  !> @param[in] clock Model time
  !> @param[in] mesh The primary mesh
  !> @param[in,out] model_data The working data set for the model run
  subroutine initialise_linear_model( clock, &
                                      mesh,  &
                                      model_data )
    implicit none

    class(clock_type), intent(in), pointer :: clock
    type(mesh_type),   intent(in), pointer :: mesh

    type( model_data_type ), intent(inout), target :: model_data

    type( field_collection_type ), pointer :: prognostic_fields => null()
    type( field_type ),            pointer :: mr(:) => null()
    type( field_collection_type ), pointer :: ls_fields => null()
    type( field_type ),            pointer :: ls_mr(:) => null()
    type( field_type ),            pointer :: ls_moist_dyn(:) => null()

    type( field_type), pointer :: theta => null()
    type( field_type), pointer :: u => null()
    type( field_type), pointer :: rho => null()
    type( field_type), pointer :: exner => null()
    type( field_type), pointer :: ls_theta => null()
    type( field_type), pointer :: ls_u => null()
    type( field_type), pointer :: ls_rho => null()
    type( field_type), pointer :: ls_exner => null()

    ! Get pointers to field collections for use downstream
    prognostic_fields => model_data%prognostic_fields
    mr => model_data%mr
    ls_fields => model_data%ls_fields
    ls_mr => model_data%ls_mr
    ls_moist_dyn => model_data%ls_moist_dyn

    ! Get pointers to fields in the prognostic/diagnostic field collections
    ! for use downstream
    theta => prognostic_fields%get_field('theta')
    u => prognostic_fields%get_field('u')
    rho => prognostic_fields%get_field('rho')
    exner => prognostic_fields%get_field('exner')
    ls_theta => ls_fields%get_field('ls_theta')
    ls_u => ls_fields%get_field('ls_u')
    ls_rho => ls_fields%get_field('ls_rho')
    ls_exner => ls_fields%get_field('ls_exner')

    select case( method )
      case( method_semi_implicit )  ! Semi-Implicit
        call semi_implicit_solver_alg_final()
        call final_si_operators()
        call tl_semi_implicit_alg_init(mesh, u, rho, theta, exner, &
                                       mr, ls_u, ls_rho, ls_theta, ls_exner, &
                                       ls_mr, ls_moist_dyn)

      case( method_rk )             ! RK
        ! Initialise and output initial conditions for first timestep

        call tl_rk_alg_init(mesh, u, rho, theta, exner, &
                            ls_u, ls_rho, ls_theta, ls_exner)

      case default
        call log_event("TL: Incorrect time stepping option chosen, "// &
                        "stopping program! ",LOG_LEVEL_ERROR)
    end select

  end subroutine initialise_linear_model

  !> @brief Finalises the remaining infrastructure and constants used by the linear model
  subroutine finalise_linear_model

    implicit none

    if ( method == method_rk )            call tl_rk_alg_final()
    if ( method == method_semi_implicit ) call tl_semi_implicit_alg_final()

  end subroutine finalise_linear_model

end module linear_model_mod
