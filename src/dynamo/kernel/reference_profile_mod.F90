!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!> @brief Module for computing a linear hydrostatially balanced reference state
module reference_profile_mod
use constants_mod,                 only: r_def, i_def, GRAVITY, Cp, Rd, &
                                         KAPPA, P_ZERO  
use configuration_mod,             only: earth_radius, l_spherical          
use initialisation_mod,            only: ITEST_GRAVITY_WAVE, &
                                         ITEST_COLD_BUBBLE, &
                                         ITEST_WARM_BUBBLE, &
                                         n_sq
use coord_transform_mod,           only: xyz2llr 
use generate_global_gw_fields_mod, only: generate_global_gw_fields

implicit none

contains
!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
!> Subroutine Computes the analytic reference profile at a single point
!! @param[out] exner_s   Real Holds the exner reference profile
!! @param[out] rho_s     Real Holds the rho reference profile
!! @param[out] theta_s   Real Holds the theta reference profile
!! @param[in] x          Real Holds the (x,y,z) coordinate field
!! @param[in] itest_option Integer Choice of idealised profile
subroutine reference_profile(exner_s, rho_s, theta_s, x, itest_option)

real(kind=r_def), intent(in)     :: x(3)
integer(kind=i_def), intent(in)  :: itest_option
real(kind=r_def), intent(out)    :: exner_s, rho_s, theta_s

real(kind=r_def), parameter :: THETA_SURF = 300.0_r_def
real(kind=r_def), parameter :: THETA_SURF_HOT = 303.05_r_def
real(kind=r_def), parameter :: EXNER_SURF = 1.0_r_def
real(kind=r_def)            :: nsq_over_g, z, u_s(3), lat, lon, r


if ( l_spherical ) then  ! SPHERICAL DOMAIN  

  ! Gravity wave test only for now
  call xyz2llr(x(1),x(2),x(3),lon,lat,r)
  z = r - earth_radius
  call generate_global_gw_fields (lat, z, exner_s, u_s, theta_s, rho_s)

else                     ! BIPERIODIC PLANE DOMAIN

  z = x(3)
  ! Calculate theta and exner for each biperiodic test
  select case( itest_option )
    case( ITEST_GRAVITY_WAVE )  ! Gravity wave test
      nsq_over_g = n_sq/GRAVITY
      theta_s = THETA_SURF * exp ( nsq_over_g * z )
      exner_s = EXNER_SURF - GRAVITY**2/(Cp*THETA_SURF*n_sq)   &
                * (1.0_r_def - exp ( - nsq_over_g * z ))
    case( ITEST_COLD_BUBBLE )   ! Density current test
      theta_s = theta_surf
      exner_s = exner_surf - GRAVITY/(Cp*THETA_SURF)*z
    case( ITEST_WARM_BUBBLE )   ! Warm bubble test
      theta_s = theta_surf
      exner_s = exner_surf - GRAVITY/(Cp*THETA_SURF_HOT)*z
  end select
  ! Calculate rho for all biperiodic tests
  rho_s   = P_ZERO/(Rd*theta_s) * exner_s ** ((1.0_r_def - KAPPA)/KAPPA)

end if

end subroutine reference_profile

end module reference_profile_mod
