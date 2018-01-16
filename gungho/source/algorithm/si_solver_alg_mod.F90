!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!>@brief Routines for solving the semi-implicit equation set

! Note: PSyclone 1.5.0 fails to correctly parse "use, intrinsic :: ieee_arithmetic" 
!       so this algorithm is not using PSyclone built-ins or other invokes.
!       However, commented PSyclone invokes are left as placeholders for when 
!       the support becomes available.
module si_solver_alg_mod

  use, intrinsic :: ieee_arithmetic

  use constants_mod,             only: r_def, str_def, i_def
  use field_bundle_mod,          only: clone_bundle, &
                                       set_bundle_scalar, &
                                       bundle_axpy, &
                                       copy_bundle, &
                                       minus_bundle, &
                                       bundle_ax, &
                                       bundle_divide, &
                                       bundle_minmax, &
                                       bundle_inner_product
  use finite_element_config_mod, only: wtheta_on
  use runtime_constants_mod,     only: get_mass_matrix_diagonal, &
                                       w2_id, theta_space_id, eye_id
  use field_mod,                 only: field_type
  use lhs_alg_mod,               only: lhs_alg
  use log_mod,                   only: log_event,         &
                                       log_scratch_space, &
                                       LOG_LEVEL_ERROR,   &
                                       LOG_LEVEL_DEBUG,   &
                                       LOG_LEVEL_TRACE,   &
                                       LOG_LEVEL_INFO
  use operator_mod,              only: operator_type
  use solver_config_mod,         only: si_maximum_iterations, &
                                       si_tolerance, &
                                       si_preconditioner, &
                                       si_postconditioner, &
                                       solver_si_preconditioner_none, &
                                       solver_si_preconditioner_diagonal, &
                                       solver_si_preconditioner_pressure, &
                                       solver_si_postconditioner_none, &
                                       solver_si_postconditioner_diagonal, &
                                       solver_si_postconditioner_pressure, &
                                       solver_si_method_gmres, &
                                       solver_si_method_gcr, &
                                       solver_si_method_jacobi, &
                                       si_method, &
                                       gcrk

  use timestepping_config_mod,   only: dt, tau_u, tau_t, tau_r
  use derived_config_mod,        only: bundle_size
  use field_indices_mod,         only: igh_u, igh_t, igh_d, igh_p
  use output_config_mod,         only: subroutine_timers 
  use timer_mod,                 only: timer

  implicit none
  type(field_type), allocatable, private :: mm_diagonal(:)
  type(field_type), allocatable, private :: dx(:), Ax(:), residual(:), s(:), &
                                            w(:)
  type(field_type), allocatable, private :: v(:,:), Pv(:,:)
  real(r_def),      parameter,   private :: sc_err_min = 1.0e-8_r_def

  private

  public  :: si_solver_alg
  public  :: si_solver_init
  private :: mixed_gmres_alg
  private :: mixed_gcr_alg
  private :: mixed_jacobi_alg
 
contains

  !>@brief Setup for the semi-implicit solver, extracts mass matrix diagonals and 
  !!         sets up terms for the Newton-Krylov method if needed
  !>@details Control routine to solve the system L*dx = rhs using a Krlyov method
  !!         sets up terms needed either for a prescribed L
  !>@param[inout] x0 The state array to solve for the increment of 
  !>@param[in]    rhs0 Fixed rhs forcing for the solver
  !>@param[in]    x_ref A reference state used for computing a proscribed L
  subroutine si_solver_alg(x0, rhs0, x_ref)
    use psykal_lite_mod,           only: invoke_set_field_scalar
    use psykal_lite_mod,           only: invoke_copy_field_data

    implicit none

    type(field_type), intent(inout)          :: x0(bundle_size)
    type(field_type), intent(in)             :: rhs0(bundle_size), &
                                                x_ref(bundle_size)
 
    real(kind=r_def)                         :: tau_u_dt, tau_t_dt, tau_r_dt

    if ( subroutine_timers ) call timer('si_solver_alg')
    ! Set up tau_{u,t,r}_dt: to be used here and in subsequent algorithms
    tau_u_dt = tau_u*dt
    tau_t_dt = tau_t*dt
    tau_r_dt = tau_r*dt

    call rhs0(igh_u)%log_minmax(LOG_LEVEL_DEBUG,'max/min r_u = ')
    call rhs0(igh_t)%log_minmax(LOG_LEVEL_DEBUG,'max/min r_t = ')
    call rhs0(igh_d)%log_minmax(LOG_LEVEL_DEBUG,'max/min r_r = ')
    call rhs0(igh_p)%log_minmax(LOG_LEVEL_DEBUG,'max/min r_p = ')

    select case (si_method)
      case (solver_si_method_gmres)
        call mixed_gmres_alg(x0, rhs0, x_ref, tau_u_dt, tau_t_dt, tau_r_dt)
      case (solver_si_method_gcr)
        call mixed_gcr_alg(x0, rhs0, x_ref, tau_u_dt, tau_t_dt, tau_r_dt)
      case (solver_si_method_jacobi)
        call mixed_jacobi_alg(x0, rhs0, x_ref, tau_u_dt, tau_t_dt, tau_r_dt)
    end select

    if ( subroutine_timers ) call timer('si_solver_alg')

  end subroutine si_solver_alg

!=============================================================================!
  !>@brief Initialisation routine for the semi-implicit solver, creates all the 
  !>       field objects needed for the solver
  !>@param[in] x0 field bundle used as template for the solver fields
  subroutine si_solver_init(x0)
    use lhs_alg_mod, only: lhs_init

    implicit none

    type(field_type), intent(in) :: x0(bundle_size)
    integer(i_def)               :: iter

    allocate( dx         (bundle_size), &
              Ax         (bundle_size), &
              residual   (bundle_size), &
              s          (bundle_size), &
              w          (bundle_size), &
              mm_diagonal(bundle_size), &
              v          (bundle_size,gcrk), &
              Pv         (bundle_size,gcrk) )

    mm_diagonal(igh_u) = get_mass_matrix_diagonal(w2_id)
    mm_diagonal(igh_t) = get_mass_matrix_diagonal(theta_space_id)
    mm_diagonal(igh_d) = get_mass_matrix_diagonal(eye_id)
    mm_diagonal(igh_p) = get_mass_matrix_diagonal(eye_id)
   
    call clone_bundle(x0, dx,       bundle_size)
    call clone_bundle(x0, Ax,       bundle_size)
    call clone_bundle(x0, s,        bundle_size)
    call clone_bundle(x0, w,        bundle_size)
    call clone_bundle(x0, residual, bundle_size)   
    do iter = 1,gcrk
      call clone_bundle(x0,  v(:,iter), bundle_size)
      call clone_bundle(x0, Pv(:,iter), bundle_size)
    end do
    ! Initialise lhs fields
    call lhs_init(x0)

  end subroutine si_solver_init

!=============================================================================!
  !>@brief GMRES solver adapted for solving the semi-implicit equations
  !>@details Standard GMRES algorithm from "Iterative methods for sparse linear
  !! systems" by Y Saad, SIAM 2003
  !>@param[inout] x0 State to increment 
  !>@param[in]    rhs0 Fixed rhs so solve for
  !>@param[in]    x_ref Reference state
  !>@param[in]    tau_{u,t,r}_dt The offcentering parameter times the timestep
  subroutine mixed_gmres_alg(x0, rhs0, x_ref, tau_u_dt, tau_t_dt, tau_r_dt)

    implicit none

    type(field_type),             intent(inout) :: x0(bundle_size)
    type(field_type),             intent(in)    :: rhs0(bundle_size), &
                                                   x_ref(bundle_size)
    real(kind=r_def),             intent(in)    :: tau_u_dt, tau_t_dt, tau_r_dt

    ! The scalars
    real(kind=r_def)         :: h(gcrk+1, gcrk), u(gcrk), g(gcrk+1)
    real(kind=r_def)         :: beta,si, ci, nrm, h1, h2, p, q
    ! Others
    real(kind=r_def)               :: err, sc_err, init_err
    integer(kind=i_def)            :: iter, i, j, k, m
    integer(kind=i_def)            :: max_gmres_iter


    max_gmres_iter = si_maximum_iterations

    err = bundle_inner_product(rhs0, rhs0, bundle_size)
    sc_err = max( sqrt(err), sc_err_min )
    init_err = sc_err

    if (err < si_tolerance) then
      write( log_scratch_space, '(A,I3,A,E12.4,A,E15.8)' ) &
           "gmres solver_algorithm:converged in ", 0,      &
           " iters, init=", init_err,                      &
           " final=", err
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
      return
    else
      write( log_scratch_space, '(A,I3,A,2E15.8)' )        &
             "solver_algorithm[", 0, "]: residual = ", init_err
      call log_event(log_scratch_space, LOG_LEVEL_DEBUG)
    end if

    ! Initial guess
    call set_bundle_scalar(0.0_r_def, dx, bundle_size)

    call set_bundle_scalar(0.0_r_def, Ax, bundle_size)

    call minus_bundle( rhs0, Ax, residual, bundle_size )

    call bundle_preconditioner(s, residual, si_preconditioner, mm_diagonal, bundle_size )

    beta = sqrt(bundle_inner_product(s, s, bundle_size)) 

    call bundle_ax( 1.0_r_def/beta, s, v(:,1), bundle_size )

    h(:,:) = 0.0_r_def
    g(:)   = 0.0_r_def
    g(1)   = beta

    do iter = 1, max_gmres_iter

      do j = 1, gcrk

        call bundle_preconditioner(Pv(:,j), v(:,j), si_postconditioner, mm_diagonal, bundle_size)

        call lhs_alg(s, Pv(:,j), x_ref, tau_u_dt, tau_t_dt, tau_r_dt)
        call bundle_preconditioner(w, s, si_preconditioner, mm_diagonal, bundle_size )
        do k = 1, j
          h(k,j) =  bundle_inner_product( v(:,k), w, bundle_size )
          call bundle_axpy( -h(k,j), v(:,k), w, w, bundle_size )
        end do        
        h(j+1,j) = sqrt( bundle_inner_product( w, w, bundle_size ))
        if( j < gcrk ) then
          call bundle_ax(1.0_r_def/h(j+1,j), w, v(:,j+1), bundle_size)
        end if
      end do

      ! Solve (7.23) of Wesseling (see Saad's book)
      do m = 1, gcrk
        nrm    = sqrt( h(m,m)*h(m,m) + h(m+1,m)*h(m+1,m) )
        si     = h(m+1,m)/nrm
        ci     = h(m,m)/nrm
        p      = ci*g(m) + si*g(m+1)
        q      = -si*g(m) + ci*g(m+1)
        g(m)   = p
        g(m+1) = q
        do j = m, gcrk
          h1       = ci*h(m,j)   + si*h(m+1,j)
          h2       =-si*h(m,j)   + ci*h(m+1,j)
          h(m,j)   = h1
          h(m+1,j) = h2
        end do
      end do

      u(gcrk) = g(gcrk)/h(gcrk,gcrk)
      do i = gcrk-1, 1, -1
        u(i) = g(i)
        do j = i+1, gcrk
          u(i) = u(i) - h(i,j)*u(j)
        end do
        u(i) = u(i)/h(i,i)
      end do

      do i = 1, gcrk
        call bundle_axpy( u(i), Pv(:,i), dx, dx, bundle_size )
      end do

      ! Check for convergence
      call lhs_alg(Ax, dx, x_ref, tau_u_dt, tau_t_dt, tau_r_dt)

      call minus_bundle( rhs0, Ax, residual, bundle_size )

      beta = sqrt(bundle_inner_product(residual, residual, bundle_size))

      err = beta/sc_err
      write( log_scratch_space, '(A,I3,A,E15.8)' ) &
             "solver_algorithm[", iter, "]: residual = ", err
      call log_event(log_scratch_space, LOG_LEVEL_INFO)

      if( err <  si_tolerance ) then
        write( log_scratch_space, '(A,I3,A,E12.4,A,E15.8)' )      &
             "GMRES solver_algorithm:converged in ", iter,        &
             " iters, init=", init_err, " final=", err
        call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
        exit
      end if

      call bundle_preconditioner(s, residual, si_preconditioner, mm_diagonal, bundle_size)
      call bundle_ax(1.0_r_def/beta, s, v(:,1), bundle_size)

      g(:) = 0.0_r_def
      g(1) = beta

    end do

    if ((iter >= max_gmres_iter .and. err >  si_tolerance) &
        .or. ieee_is_nan(err)) then
      write( log_scratch_space, '(A, I3, A, E15.8)') &
           "GMRES solver_algorithm: NOT converged in ", max_gmres_iter, &
           " iters, Res=", err
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    ! Add increments to field
    call bundle_axpy(1.0_r_def, dx, x0, x0, bundle_size)  

  end subroutine mixed_gmres_alg

!=============================================================================!
  !>@brief GCR(k) solver adapted for solving the semi-implicit equations
  !> @details Solves A.lhs = rhs where the operation A.lhs is encoded in 
  !> the lhs algorithm. Uses the Preconditioned GCR(k) algorithm from Wesseling.
  !>@param[inout] x0 State to increment 
  !>@param[in]    rhs0 Fixed rhs so solve for
  !>@param[in]    x_ref Reference state
  !>@param[in]    tau_{u,t,r}_dt The offcentering parameter times the timestep
  subroutine mixed_gcr_alg(x0, rhs0, x_ref, tau_u_dt, tau_t_dt, tau_r_dt)

    implicit none

    type(field_type),             intent(inout) :: x0(bundle_size)
    type(field_type),             intent(in)    :: rhs0(bundle_size), &
                                                   x_ref(bundle_size)
    real(kind=r_def),             intent(in)    :: tau_u_dt, tau_t_dt, tau_r_dt

    ! The scalars
    real(kind=r_def)               :: alpha, const1
    ! Others
    real(kind=r_def)               :: err, sc_err, init_err
    integer(kind=i_def)            :: iter, n, m, m_final
    integer(kind=i_def)            :: max_gmres_iter


    max_gmres_iter = si_maximum_iterations

    err = bundle_inner_product(rhs0, rhs0, bundle_size)
    sc_err = max( sqrt(err), sc_err_min )
    init_err = sc_err

    ! Initial guess
    call set_bundle_scalar(0.0_r_def, dx, bundle_size)
    if ( .true. ) then
      call bundle_preconditioner(dx, rhs0, si_postconditioner, mm_diagonal, bundle_size )
      call lhs_alg(Ax, dx, x_ref, tau_u_dt, tau_t_dt, tau_r_dt)
      call minus_bundle( rhs0, Ax, residual, bundle_size )
    else
      call copy_bundle( rhs0, residual, bundle_size )
    end if


    if (err < si_tolerance) then
      write( log_scratch_space, '(A,I3,A,E12.4,A,E15.8)' ) &
           "gcr solver_algorithm:converged in ", 0,      &
           " iters, init=", init_err,                      &
           " final=", err
      call log_event( log_scratch_space, LOG_LEVEL_DEBUG )
      return
    else
      write( log_scratch_space, '(A,I3,A,2E15.8)' )        &
             "solver_algorithm[", 0, "]: residual = ", init_err
      call log_event(log_scratch_space, LOG_LEVEL_DEBUG)
    end if


    do iter = 1, max_gmres_iter
      do m = 1, gcrk        
        call bundle_preconditioner(Pv(:,m), residual, si_postconditioner, mm_diagonal, bundle_size)
        call lhs_alg(v(:,m), Pv(:,m), x_ref, tau_u_dt, tau_t_dt, tau_r_dt)

        do n = 1, m-1
          alpha = bundle_inner_product( v(:,m), v(:,n), bundle_size )
          call bundle_axpy( -alpha, v(:,n), v(:,m), v(:,m), bundle_size )
          call bundle_axpy( -alpha, Pv(:,n), Pv(:,m), Pv(:,m), bundle_size )
        end do
        err = bundle_inner_product( v(:,m), v(:,m), bundle_size )
        alpha = sqrt(err)
        const1 = 1.0_r_def/alpha
        call bundle_ax(const1, v(:,m), v(:,m), bundle_size)
        call bundle_ax(const1, Pv(:,m), Pv(:,m), bundle_size)
        alpha = bundle_inner_product( residual, v(:,m), bundle_size )
        call bundle_axpy( alpha, Pv(:,m), dx, dx, bundle_size )
        call bundle_axpy(-alpha, v(:,m), residual, residual, bundle_size )

        err = bundle_inner_product( residual, residual, bundle_size )
        err = sqrt( err )/sc_err
        if ( err <  si_tolerance ) then 
          m_final = m
          exit
        end if
      end do
      write( log_scratch_space, '(A,I3,A,I3,A,E12.4,A,E15.8)' )      &
           "Mixed solver: [", iter,',',m_final,    &
           "], init=", init_err, " error=", err
      call log_event( log_scratch_space, LOG_LEVEL_INFO )
      if ( err <  si_tolerance ) exit
    end do

    if ((iter >= max_gmres_iter .and. err >  si_tolerance) &
        .or. ieee_is_nan(err)) then
      write( log_scratch_space, '(A, I3, A, E15.8)') &
           "GCR solver_algorithm: NOT converged in ", max_gmres_iter, &
           " iters, Res=", err
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    ! Add increments to field
    call bundle_axpy(1.0_r_def, dx, x0, x0, bundle_size)  

  end subroutine mixed_gcr_alg

!=============================================================================!
  !>@brief Jacobi iterative solver adapted for solving the semi-implicit equations
  !>@param[inout] x0 State to increment 
  !>@param[in]    rhs0 Fixed rhs so solve for
  !>@param[in]    x_ref Reference state
  !>@param[in]    tau_{u,t,r}_dt The offcentering parameter times the timestep
  subroutine mixed_jacobi_alg(x0, rhs0, x_ref, tau_u_dt, tau_t_dt, tau_r_dt) 
    implicit none

    type(field_type),             intent(inout) :: x0(bundle_size)
    type(field_type),             intent(in)    :: rhs0(bundle_size), &
                                                   x_ref(bundle_size)
    real(kind=r_def),             intent(in)    :: tau_u_dt, tau_t_dt, tau_r_dt

    real(r_def),    parameter :: omega = 3.0/3.0
    real(r_def)               :: err, err0
    integer(i_def)            :: iter

    err0 = sqrt(bundle_inner_product(rhs0, rhs0, bundle_size))

    call set_bundle_scalar(0.0_r_def, dx, bundle_size)
    do iter = 1,si_maximum_iterations 
      call lhs_alg(Ax, dx, x_ref, tau_u_dt, tau_t_dt, tau_r_dt)
      call minus_bundle( rhs0, Ax, residual, bundle_size )
     
      ! Compute error
      err = bundle_inner_product(residual, residual, bundle_size)
      write( log_scratch_space, '(A,I2,2E12.4)' ) &
        'Jacobi residual = ',iter,sqrt(err), sqrt(err)/err0
      call log_event( log_scratch_space, LOG_LEVEL_INFO )
      if ( sqrt(err)/err0 < si_tolerance ) exit

      call bundle_preconditioner(s, residual, si_preconditioner, mm_diagonal, bundle_size )
      call bundle_ax( omega, s, dx, bundle_size )     
    end do
    call bundle_axpy(1.0_r_def, dx, x0, x0, bundle_size)  

  end subroutine mixed_jacobi_alg

!=============================================================================!
  !>@brief Applies a choosen preconditioner to a state x to produce state y
  !>@param[inout] y Preconditioned state
  !>@param[in]    x Original state
  !>@param[in]    option Choice of which preconditioner to use
  !>@param[in]    mm Arrays containing diagonal approximation to mass matrices
  !>@param[in]    bundle_size Number of fields the state arrays
  subroutine bundle_preconditioner(y, x, option, mm, bundle_size)
    use psykal_lite_mod,          only: invoke_copy_field_data
    use helmholtz_solver_alg_mod, only: helmholtz_solver_alg

    implicit none
    integer(kind=i_def), intent(in)    :: bundle_size
    type(field_type),    intent(inout) :: y(bundle_size)
    type(field_type),    intent(in)    :: x(bundle_size)
    type(field_type),    optional      :: mm(bundle_size)
    integer(kind=i_def), intent(in)    :: option
    integer(kind=i_def)                :: i

    select case (option)
      case (solver_si_preconditioner_none, &
            solver_si_postconditioner_none)
        do i = 1,bundle_size
          call invoke_copy_field_data( x(i), y(i) )
          ! call invoke( setval_X(y(i), x(i)) )
        end do
      case (solver_si_preconditioner_diagonal, &
            solver_si_postconditioner_diagonal)
        do i = 1,bundle_size
          call invoke_copy_field_data( x(i), y(i) )
          ! call invoke( setval_X(y(i), x(i)) )
        end do
        call bundle_divide(y, mm, bundle_size)
      case (solver_si_preconditioner_pressure, &
            solver_si_postconditioner_pressure)
        call set_bundle_scalar(0.0_r_def, y, bundle_size)
        call helmholtz_solver_alg(y, x)
      case default
        call log_event( 'Invalid si pre/postconditioner', LOG_LEVEL_ERROR )
    end select

  end subroutine bundle_preconditioner

!=============================================================================!
end module si_solver_alg_mod
