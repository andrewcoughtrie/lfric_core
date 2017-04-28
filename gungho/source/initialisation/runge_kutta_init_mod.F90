!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------

!>@brief Intialisation module for the Runge-Kutta method, sets the
!!number of stages and coefficients 
module runge_kutta_init_mod

  use constants_mod,     only: r_def, i_def

implicit none
  real(kind=r_def),    public, protected, allocatable :: ak(:,:)
  integer(kind=i_def), public, protected              :: num_rk_stage
contains

subroutine runge_kutta_init

  use constants_mod,           only: r_def, i_def
  use timestepping_config_mod, only: runge_kutta_method, &
                                     timestepping_runge_kutta_method_ssp2, &
                                     timestepping_runge_kutta_method_ssp3, &
                                     timestepping_runge_kutta_method_ssp4, &
                                     timestepping_runge_kutta_method_ssp5
  use log_mod,                 only: log_event,         &
                                     log_scratch_space, &
                                     LOG_LEVEL_ERROR
implicit none

  select case(runge_kutta_method)

    case(timestepping_runge_kutta_method_ssp2)
      num_rk_stage = 2
      allocate ( ak (num_rk_stage,num_rk_stage) )
      ak(1,:) = (/ 1.0_r_def, 0.0_r_def /)
      ak(2,:) = (/ 0.5_r_def, 0.5_r_def /)

    case(timestepping_runge_kutta_method_ssp3)
      num_rk_stage = 3
      allocate ( ak (num_rk_stage,num_rk_stage) )
      ak(1,:) = (/ 1.0_r_def,  0.0_r_def,  0.0_r_def /)
      ak(2,:) = (/ 0.25_r_def, 0.25_r_def, 0.0_r_def /)
      ak(3,:) = (/ 1.0_r_def,  1.0_r_def,  4.0_r_def /)/6.0_r_def  
 
    case(timestepping_runge_kutta_method_ssp4)
      num_rk_stage = 4
      allocate ( ak (num_rk_stage,num_rk_stage) )
      ak(1,:) = (/ 0.5_r_def, 0.0_r_def, 0.0_r_def, 0.0_r_def /)
      ak(2,:) = (/ 0.5_r_def, 0.5_r_def, 0.0_r_def, 0.0_r_def /)
      ak(3,:) = (/ 1.0_r_def, 1.0_r_def, 1.0_r_def, 0.0_r_def /)/6.0_r_def
      ak(4,:) = (/ 1.0_r_def, 1.0_r_def, 1.0_r_def, 3.0_r_def /)/6.0_r_def

    case(timestepping_runge_kutta_method_ssp5)
      num_rk_stage = 5
      allocate ( ak (num_rk_stage,num_rk_stage) )
      ak(1,:) = (/ 0.37727_r_def, 0.0_r_def,     0.0_r_def,     0.0_r_def,     0.0_r_def     /)
      ak(2,:) = (/ 0.37727_r_def, 0.37727_r_def, 0.0_r_def,     0.0_r_def,     0.0_r_def     /)
      ak(3,:) = (/ 0.24300_r_def, 0.24300_r_def, 0.24300_r_def, 0.0_r_def,     0.0_r_def     /)
      ak(4,:) = (/ 0.15359_r_def, 0.15359_r_def, 0.15359_r_def, 0.23846_r_def, 0.0_r_def     /)
      ak(5,:) = (/ 0.20673_r_def, 0.20673_r_def, 0.11710_r_def, 0.18180_r_def, 0.28763_r_def /)

    case default
      write( log_scratch_space, '(A,I3)' )  &
        'Invalid Runge Kutta method, stopping', runge_kutta_method
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
  end select
end subroutine runge_kutta_init

end module runge_kutta_init_mod
