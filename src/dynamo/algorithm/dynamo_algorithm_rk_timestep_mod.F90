!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!>@brief A Runge-Kutta time-discretisation of the 3D equations, currently using 3-stage SSP.
!>@details An algorithm for timstepping the 3D linear no advection equations
!>         using a 3-stage SSP Runge-Kutta algortihm. The algorithm
!>         initialialises all fields, sets up tempoary fields and perform the
!>         timestepping and checks to see if it should dump output
module dynamo_algorithm_rk_timestep_mod

  use log_mod,       only: log_event,         &
                           log_scratch_space, &
                           LOG_LEVEL_INFO,    &
                           LOG_LEVEL_TRACE
  use psy,           only: invoke_initial_theta_kernel,                      &
                           invoke_calc_exner_kernel,                         &
                           invoke_rtheta_kernel,                             &
                           invoke_ru_kernel,                                 &
                           invoke_rrho_kernel,                               &
                           invoke_compute_mass_matrix_w0,                    &
                           invoke_compute_mass_matrix_w2,                    &
                           invoke_copy_field_data,                           &
                           invoke_set_field_scalar,                          &
                           invoke_axpy,                                      &                                     
                           invoke_matrix_vector_mm
  use field_mod,     only: field_type
  use function_space_mod, &
                     only: function_space_type, W0, W1, W2, W3
  use solver_mod,    only: solver_algorithm
  use constants_mod, only: r_def, SOLVER_OPTION
  use quadrature_mod, only : quadrature_type, QR3
  use galerkin_projection_algorithm_mod, &
                     only : galerkin_projection_algorithm
  use driver_layer,  only : interpolated_output
  use operator_mod,  only : operator_type

  implicit none

  private
  public :: dynamo_algorithm_rk_timestep

contains

  subroutine dynamo_algorithm_rk_timestep( chi, u, rho, theta, exner, xi)

    implicit none

    ! coordinate fields
    type( field_type ), intent( inout ) :: chi(3)
    ! prognostic fields
    type( field_type ), intent( inout ) :: u, rho, theta, exner, xi
    ! temporary fields
    type( field_type ) :: u_n, rho_n, theta_n,       &
                          r_u, r_rho, r_theta,       &
                          u_inc, rho_inc, theta_inc

    type( field_type ), allocatable :: rt_prediction(:),  &
                                       ru_prediction(:),  &
                                       rr_prediction(:)
    type( field_type ) :: projected_field(3)

    integer :: theta_fs, u_fs, rho_fs
    integer :: n, nt, output_freq
    integer :: stage, num_rk_stage, st
    real(kind=r_def), allocatable :: ak(:,:)
    real(kind=r_def) :: dt = 1.0_r_def
    type(function_space_type) :: fs
    type( quadrature_type), pointer :: qr => null()
    integer, parameter :: VECTOR_FIELD = 3, &
                          SCALAR_FIELD = 1

    type(operator_type) :: mm_w2, mm_w0

    ! SSP3 weights
    num_rk_stage = 3
    allocate ( ak (num_rk_stage,num_rk_stage) )
    ak(1,:) = (/ 1.0_r_def,  0.0_r_def,  0.0_r_def /)
    ak(2,:) = (/ 0.25_r_def, 0.25_r_def, 0.0_r_def /)
    ak(3,:) = (/ 1.0_r_def,  1.0_r_def,  4.0_r_def /)/6.0_r_def

    allocate ( rt_prediction(num_rk_stage), &
               ru_prediction(num_rk_stage), &
               rr_prediction(num_rk_stage) )

    ! Local fields
    theta_fs  = theta%which_function_space()
    u_fs      = u%which_function_space()
    rho_fs    = rho%which_function_space()

    qr => qr%get_instance(QR3,9,3)

    theta_n   = field_type( vector_space = fs%get_instance(theta_fs) )
    u_n       = field_type( vector_space = fs%get_instance(u_fs) )
    rho_n     = field_type( vector_space = fs%get_instance(rho_fs) )
    r_theta   = field_type( vector_space = fs%get_instance(theta_fs) )
    r_u       = field_type( vector_space = fs%get_instance(u_fs) )
    r_rho     = field_type( vector_space = fs%get_instance(rho_fs) )
    theta_inc = field_type( vector_space = fs%get_instance(theta_fs) )
    u_inc     = field_type( vector_space = fs%get_instance(u_fs) )
    rho_inc   = field_type( vector_space = fs%get_instance(rho_fs) )

    do stage = 1,num_rk_stage
      rt_prediction(stage) = field_type( vector_space = fs%get_instance(theta_fs) )
      ru_prediction(stage) = field_type( vector_space = fs%get_instance(u_fs) )
      rr_prediction(stage) = field_type( vector_space = fs%get_instance(rho_fs) )
    end do

    mm_w0 = operator_type(fs%get_instance(W0),fs%get_instance(W0))
    mm_w2 = operator_type(fs%get_instance(W2),fs%get_instance(W2))

    do stage = 1,3
      projected_field(stage) = field_type( vector_space = fs%get_instance(theta_fs) )
    end do

    !Construct PSy layer given a list of kernels. This is the line the code
    !generator may parse and do its stuff.

    ! initialise

    ! Construct initial conditions
    call log_event( "Dynamo: computing initial fields", LOG_LEVEL_INFO )
    call invoke_initial_theta_kernel( theta, chi )
    call invoke_set_field_scalar(0.0_r_def, u)
    call invoke_set_field_scalar(0.0_r_def, rho)
    call invoke_set_field_scalar(0.0_r_def, xi)

    call invoke_calc_exner_kernel   ( exner, rho, theta, chi, qr)

    call invoke_compute_mass_matrix_w0(mm_w0, chi, qr)
    call invoke_compute_mass_matrix_w2(mm_w2, chi, qr)

    call galerkin_projection_algorithm(projected_field(1),                     &
         theta, chi, SCALAR_FIELD, qr, mm=mm_w0)
    call interpolated_output(0, SCALAR_FIELD, projected_field(1), chi, 'theta_')
    call invoke_set_field_scalar(0.0_r_def, projected_field(1))
    call galerkin_projection_algorithm(projected_field(1),                     &
         rho,   chi,  SCALAR_FIELD, qr, mm=mm_w0)
    call interpolated_output(0, SCALAR_FIELD, projected_field(1), chi, 'rho___')
    call galerkin_projection_algorithm(projected_field(:),                     &
         u,     chi, VECTOR_FIELD, qr, mm=mm_w0)
    call interpolated_output(0, VECTOR_FIELD, projected_field(:), chi, 'u_____')

    !==========================================================================
    ! Timestep
    nt = 30 ! number of timesteps
    output_freq = nt
    do n = 1,nt
      call log_event( "/****************************************************************************\ ", &
                      LOG_LEVEL_TRACE )
      write( log_scratch_space, '(A,I0)' ) 'Start of timestep ', n
      call log_event( log_scratch_space, LOG_LEVEL_INFO )

      !PSY call invoke ( copy_field_data(theta,theta_n))
      call invoke_copy_field_data(theta,theta_n)
      !PSY call invoke ( copy_field_data(u,u_n))
      call invoke_copy_field_data(u,u_n)
      !PSY call invoke ( copy_field_data(rho,rho_n))
      call invoke_copy_field_data(rho,rho_n)

      ! Runge-Kutta algorithm
      do stage = 1,num_rk_stage
        ! Compute new rhs
        !PSY call invoke ( set_field_scalar(0.0_r_def, rt_prediction(stage)))
        call invoke_set_field_scalar(0.0_r_def, rt_prediction(stage))
        call invoke_rtheta_kernel( rt_prediction(stage), u, chi, qr)
        !PSY call invoke ( set_field_scalar(0.0_r_def, ru_prediction(stage)))
        call invoke_set_field_scalar(0.0_r_def, ru_prediction(stage))
        call invoke_ru_kernel    ( ru_prediction(stage), u, rho, theta, chi, qr )
        call invoke_rrho_kernel  ( rr_prediction(stage), u, chi, qr )
        !PSY call invoke ( set_field_scalar(0.0_r_def, r_theta))
        call invoke_set_field_scalar(0.0_r_def, r_theta)
        !PSY call invoke ( set_field_scalar(0.0_r_def, r_u))
        call invoke_set_field_scalar(0.0_r_def, r_u)
        !PSY call invoke ( set_field_scalar(0.0_r_def, r_rho))
        call invoke_set_field_scalar(0.0_r_def, r_rho)

        do st = 1, stage
          !PSY call invoke ( axpy(ak(stage,st), rt_prediction(st),r_theta, r_theta))
          call invoke_axpy(ak(stage,st), rt_prediction(st),r_theta, r_theta)
          !PSY call invoke ( axpy(ak(stage,st), ru_prediction(st),r_u, r_u))
          call invoke_axpy(ak(stage,st), ru_prediction(st),r_u, r_u)
          !PSY call invoke ( axpy(ak(stage,st), rr_prediction(st),r_rho, r_rho))
          call invoke_axpy(ak(stage,st), rr_prediction(st),r_rho, r_rho)
        end do

        ! Invert mass matrices
        call solver_algorithm( theta_inc, r_theta, chi, SOLVER_OPTION, mm=mm_w0)
        call solver_algorithm( u_inc,     r_u,     chi, SOLVER_OPTION, mm=mm_w2)
        call solver_algorithm( rho_inc,   r_rho,   chi, SOLVER_OPTION, qr=qr)

        ! add increments
        !PSY call invoke ( axpy(dt, theta_inc, theta_n, theta))
        call invoke_axpy(dt, theta_inc, theta_n, theta)
        !PSY call invoke ( axpy(dt, u_inc, u_n, u))
        call invoke_axpy(dt, u_inc, u_n, u)
        !PSY call invoke ( axpy(dt, rho_inc, rho_n, rho))
        call invoke_axpy(dt, rho_inc, rho_n, rho)

        ! recompute latest exner value
        call invoke_calc_exner_kernel( exner, rho, theta, chi, qr )

        ! diagnostics
        call theta_inc%log_minmax(LOG_LEVEL_TRACE, 'theta_inc');
        call u_inc%log_minmax(LOG_LEVEL_TRACE, 'u_inc');
        call rho_inc%log_minmax(LOG_LEVEL_TRACE, 'rho_inc');

      end do

      if ( mod(n, output_freq) == 0 ) then
        call galerkin_projection_algorithm(projected_field(1),                  &
             theta, chi, SCALAR_FIELD, qr, mm=mm_w0)
        call interpolated_output(n, SCALAR_FIELD, projected_field(1), chi, 'theta_')
        call galerkin_projection_algorithm(projected_field(1),                  &
             rho,   chi, SCALAR_FIELD, qr, mm=mm_w0)
        call interpolated_output(n, SCALAR_FIELD, projected_field(1), chi, 'rho___')
        call galerkin_projection_algorithm(projected_field(:),                  &
             u,     chi, VECTOR_FIELD, qr, mm=mm_w0)
        call interpolated_output(n, VECTOR_FIELD, projected_field(:), chi, 'u_____')
      end if

      write( log_scratch_space, '(A,I0)' ) 'End of timestep ', n
      call log_event( log_scratch_space, LOG_LEVEL_TRACE )
      call log_event( '\****************************************************************************/ ', &
                      LOG_LEVEL_TRACE )
    end do
    call log_event( "Dynamo: finished timestep", LOG_LEVEL_INFO )
    !==========================================================================

  end subroutine dynamo_algorithm_rk_timestep

end module dynamo_algorithm_rk_timestep_mod
