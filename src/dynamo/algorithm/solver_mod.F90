!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!> @brief Solver algorithm - Krylov subspace iterative solver for A x = b
!! @details Only BiCGstab so far but can contain other solvers so not typing at the momoent
!! Simply contains a single subroutine which implements BiCGStab for a hard-code matrix-vector
!! (the operation Ax) routine
!! @parameter lhs field_type the solution, x
!! @parameter rhs field_type the right hand side, b

module solver_mod
  use constants_mod,   only : dp, max_iter
  use log_mod,         only : log_event, LOG_LEVEL_INFO, LOG_LEVEL_ERROR, &
                              LOG_LEVEL_DEBUG
  use field_mod,       only : field_type
  use function_space_mod, only : function_space_type
  use mesh_mod, only : num_layers
  use gaussian_quadrature_mod, only : gaussian_quadrature_type
                                   
  use psy,             only : inner_prod, invoke_matrix_vector

  implicit none
  private

  public :: solver_algorithm

contains
!> @brief BiCGStab solver with no preconditioning. 
!! @details solves A.x = b where the operation A.x is encoded in a kernel
!! @param rhs_field_p The input b
!! @param lhs_field_p The answser, x

  subroutine solver_algorithm(lhs, rhs)

    type(field_type), intent(inout)    :: lhs
    type(field_type), intent(in)       :: rhs

    character(len=80)                  :: cmessage
    ! The temporary fields
    type(field_type)                   :: res, cr, p, v, s, t

    ! the scalars 
    ! the greeks - standard BiCGStab
    real(kind=dp)                      :: rho,alpha,omega,beta, norm
    real(kind=dp)                      :: ts,tt
    ! others
    real(kind=dp)                      :: err,sc_err, tol, init_err
    integer                            :: iter
    integer                            :: rhs_fs
    type(function_space_type)          :: fs
    type( gaussian_quadrature_type )   :: gq 
    type( gaussian_quadrature_type ), pointer :: null_gq => null()

    tol = 1.0e-8
    ! compute the residual this is a global sum to the PSy ---
    sc_err = inner_prod(rhs,rhs)
    sc_err = sqrt(sc_err)
    write(cmessage,'("solver_algorithm: starting ... ||b|| = ",E15.8)') sc_err
    call log_event(trim(cmessage), LOG_LEVEL_INFO)
    call lhs%set_field_scalar(0.0_dp)
    
    rhs_fs = rhs%which_function_space()
    v = field_type(fs%get_instance(rhs_fs),                                &
         gq%get_instance(), num_layers = num_layers)
    call v%set_field_scalar(0.0_dp)

    call invoke_matrix_vector(v,lhs)
    err = inner_prod(v,v)
    
    res = field_type(fs%get_instance(rhs_fs),                              &
         null_gq, num_layers=num_layers)   ! don't need a gq here
    
    call res%minus_field_data(rhs,v)
    
    err = inner_prod(res,res)
    err = sqrt(err)/sc_err
    init_err=err
    
    alpha  = 1.0
    omega  = 1.0
    norm   = 1.0
    
    cr = field_type(fs%get_instance(rhs_fs),                               &
         null_gq, num_layers = num_layers)
    call cr%copy_field_data(res)
    
    p = field_type(fs%get_instance(rhs_fs),                                &
         null_gq, num_layers = num_layers)
    call p%set_field_scalar(0.0_dp)
    
    t = field_type(fs%get_instance(rhs_fs),                                &
         gq%get_instance(), num_layers = num_layers)
    s = field_type(fs%get_instance(rhs_fs),                                &
         null_gq, num_layers = num_layers)
    call v%set_field_scalar(0.0_dp)
    
    do iter = 1, max_iter
       
       rho = inner_prod(cr,res)
       beta = (rho/norm) * (alpha/omega)
       call t%axpy((-beta*omega),v,res)
       
       !      ! this is where the preconitioner would go
       call s%copy_field_data(t)
       call p%axpy( beta,p,s)
       call v%set_field_scalar(0.0_dp)
       call invoke_matrix_vector(v,p)
      
       norm = inner_prod(cr,v)
       alpha = rho/norm
       call s%axpy(-alpha,v,res)
       
       !precon cs, s - again no preconditioner
       ! either use a cs or zero t first as its an inc field!
       call t%set_field_scalar(0.0_dp)
       call invoke_matrix_vector(t,s)
       
       tt = inner_prod(t,t)
       ts = inner_prod(t,s)
       
       omega = ts/tt
       
       !      lhs = lhs + omega * s + alpha * p
       call lhs%axpy(omega,s,lhs)
       call lhs%axpy(alpha,p,lhs)
       call res%axpy(-omega,t,s)
       norm = rho
       
       ! check for convergence
       err = inner_prod(res,res)
       err = sqrt(err)/sc_err
       
       write(cmessage,'("solver_algorithm[",I2,"]: res = ", E15.8)')        &
            iter, err
       call log_event(trim(cmessage), LOG_LEVEL_DEBUG)
       
       if (err < tol) then 
          write(cmessage,'("solver_algorithm:converged in ", I2," iters, init=",E12.4," final=",E15.8)') iter,init_err,err
          call log_event(trim(cmessage),LOG_LEVEL_INFO)
          exit
       end if
    end do
    
    if(iter.ge.max_iter) then
       write(cmessage,'("solver_algortihm: NOT converged in", I3," iters, Res=",E15.8)') &
            iter, err
       call log_event(trim(cmessage),LOG_LEVEL_ERROR)
       call log_event(" ... time to flee, bye.",LOG_LEVEL_ERROR)
       stop
    end if
    
  end subroutine solver_algorithm
  
end module solver_mod
