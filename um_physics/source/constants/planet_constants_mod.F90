!-----------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!-----------------------------------------------------------------------
!> @brief LFRic interface module for UM code (planet_constants_mod)
!-----------------------------------------------------------------------

! Planet constants

module planet_constants_mod

  ! Universal constants
  use constants_mod, only: l_def, i_um, r_um, real32, pi, rmdi, imdi
  use lfric_atm_water_constants_mod, only: gas_constant_h2o

  implicit none

  private
  public :: set_planet_constants
  public :: c_virtual, cp, cv, etar, g, grcp, kappa, lcrcp, lfrcp, ls, lsrcp, &
            one_minus_epsilon, one_minus_epsilon_32b, p_zero, planet_radius,  &
            pref, r, recip_a2, recip_kappa, repsilon, repsilon_32b, rv, vkman

  ! The following variables have been hidden as they are not currently
  ! required to build the extracted UM code. They have been left in
  ! in case they are required as more UM code is drawn into the lfric_atm
  ! build. Should they be required at a later date, they should simply be
  ! added to the public statement above.

  ! Disabled variables:
  !   l_planet_orbit,          l_set_planet_rotation,
  !   l_planet_g,              l_planet_grey_surface,
  !   l_planet_intrinsic_flux, l_planet_aerosol,
  !   l_fix_solang
  !
  !   planet_t_intrinsic, planet_albedo,  planet_emissivity,
  !   planet_aerosol_mmr, planet_epoch,   planet_e,
  !   planet_de,          planet_lph,     planet_dlph,
  !   planet_oblq,        planet_doblq,   planet_a,
  !   planet_da,          planet_M,       planet_dM,
  !   planet_ha,          planet_dha,     planet_obs_lon,
  !   planet_obs_lat,     stellar_radius, solar_zenith_angle,
  !   solar_azimuth_angle, sc, sclht, lapse, omega, i_eqt,
  !   ip_smart, ip_mueller, two_omega, s2r, recip_p_zero,
  !   recip_epsilon, g_over_r



!----------------------------------------------------------------------
! Primary planet constants
!----------------------------------------------------------------------

  ! Use planet orbital parameters
  logical(l_def), protected :: l_planet_orbit = .false.

  ! Set planet rotation from namelist
  logical(l_def), protected :: l_set_planet_rotation = .false.

  ! Use g on model levels for radiative heating
  logical(l_def), protected :: l_planet_g = .false.

  ! Set a grey surface for radiation
  logical(l_def), protected :: l_planet_grey_surface = .false.

  ! Use an intrinsic thermal flux at the lower boundary, set
  ! with an effective intrinsic temperature planet_t_intrinsic.
  logical(l_def), protected :: l_planet_intrinsic_flux = .false.

  ! Use a constant aerosol mixing ratio
  logical(l_def), protected :: l_planet_aerosol = .false.

  ! Fix the sun at a particular zenith and azimuth angle
  logical(l_def), protected :: l_fix_solang = .false.

  ! Intrinsic planet temperature used to set thermal flux at lower
  ! boundary if l_planet_intrinsic_flux is .true.
  real(r_um), protected :: planet_t_intrinsic = real(rmdi, r_um)

  ! Effective surface albedo for broadband shortwave radiation
  ! if l_planet_grey_surface is .true.
  real(r_um), protected :: planet_albedo = real(rmdi, r_um)

  ! Effective surface emissivity for broadband longwave radiation
  ! if l_planet_grey_surface is .true.
  real(r_um), protected :: planet_emissivity = real(rmdi, r_um)

  ! Constant aerosol mixing ratio
  real(r_um), protected :: planet_aerosol_mmr = real(rmdi, r_um)

  ! Fixed solar zenith angle in radians
  real(r_um), protected :: solar_zenith_angle = real(rmdi, r_um)

  ! Fixed solar azimuth angle measured clockwise from grid north (radians)
  real(r_um), protected :: solar_azimuth_angle = real(rmdi, r_um)

  ! Planet radius in metres
  real(r_um), protected :: planet_radius = real(rmdi, r_um)

  ! Epoch in Julian Days
  real(r_um), protected :: planet_epoch = real(rmdi, r_um)

  ! Eccentricity of the orbit
  real(r_um), protected :: planet_e = real(rmdi, r_um)

  ! Increment to eccentricity per day number from epoch
  real(r_um), protected :: planet_de = real(rmdi, r_um)

  ! Longitude of perihelion in radians
  real(r_um), protected :: planet_lph = real(rmdi, r_um)

  ! Increment to longitude of perihelion per day number from epoch
  real(r_um), protected :: planet_dlph = real(rmdi, r_um)

  ! Obliquity of the orbit in radians
  real(r_um), protected :: planet_oblq = real(rmdi, r_um)
  
  ! Increment to obliquity of the orbit per day number from epoch
  real(r_um), protected :: planet_doblq = real(rmdi, r_um)

  ! Semi-major axis in AU
  real(r_um), protected :: planet_a = real(rmdi, r_um)

  ! Increment to semi-major axis per day number from epoch
  real(r_um), protected :: planet_da = real(rmdi, r_um)

  ! Mean anomaly at epoch in radians
  real(r_um), protected :: planet_M = real(rmdi, r_um)

  ! Increment to mean anomaly per day number from epoch
  real(r_um), protected :: planet_dM = real(rmdi, r_um)

  ! Planet hour angle at epoch in radians
  real(r_um), protected :: planet_ha = real(rmdi, r_um)

  ! Increment to planet hour angle per day number from epoch
  real(r_um), protected :: planet_dha = real(rmdi, r_um)

  ! Orbital longitude of observer
  real(r_um), protected :: planet_obs_lon = real(rmdi, r_um)

  ! Orbital latitude of observer
  real(r_um), protected :: planet_obs_lat = real(rmdi, r_um)

  ! Stellar radius in metres
  real(r_um), protected :: stellar_radius = real(rmdi, r_um)

  ! Stellar irradiance at 1 astronomical unit (AU) in W/m2
  real(r_um), protected :: sc = real(rmdi, r_um)

  ! Mean acceleration due to gravity at the planet surface
  real(r_um), protected :: g = real(rmdi, r_um)

  ! Gas constant for dry air
  real(r_um), protected :: r = real(rmdi, r_um)

  ! Specific heat of dry air at constant pressure
  real(r_um), protected :: cp = real(rmdi, r_um)

  ! Reference surface pressure
  real(r_um), protected :: pref = real(rmdi, r_um)

  ! Mean scale height for pressure
  real(r_um), protected :: sclht = real(rmdi, r_um)

  ! Near surface environmental lapse rate
  real(r_um), protected :: lapse = real(rmdi, r_um)

  ! Angular speed of planet rotation
  real(r_um), protected :: omega = real(rmdi, r_um)

  ! Selector for formulation of equation of time
  integer(i_um), protected :: i_eqt = int(imdi, i_um)

  ! Identifiers for equation of time formulation:
  integer(i_um), parameter :: ip_smart   = 1_i_um
  !   Equation 29 on page 149 of Smart (1944).
  integer(i_um), parameter :: ip_mueller = 2_i_um 
  !   M. Mueller, Acta Physica Polonica A 88 Supplement, S-49 (1995)


  ! Gas constant for water vapour
  real(r_um), parameter :: rv = real(gas_constant_h2o, r_um )

  ! Von Karman's constant
  real(r_um),  parameter :: vkman = 0.4_r_um

!----------------------------------------------------------------------
! Derived planet constants
!----------------------------------------------------------------------

  ! Angular speed of planet rotation x2
  real(r_um), protected :: two_omega

  ! Seconds-to-radians converter
  real(r_um), protected :: s2r                 ! planet_dha/rsec_per_day

  ! Ratio of molecular weights of water and dry air
  real(r_um), protected :: repsilon            ! r/rv

  real(r_um), protected :: p_zero              ! pref
  real(r_um), protected :: recip_p_zero        ! 1.0/pref
  real(r_um), protected :: kappa               ! r/cp
  real(r_um), protected :: recip_kappa         ! 1.0/kappa
  real(r_um), protected :: recip_epsilon       ! 1.0/repsilon
  real(r_um), protected :: c_virtual           ! 1.0/repsilon-1.0
  real(r_um), protected :: one_minus_epsilon   ! 1.0-repsilon
  real(r_um), protected :: etar                ! 1.0/(1.0-repsilon)
  real(r_um), protected :: grcp                ! g/cp
  real(r_um), protected :: lcrcp               ! lc/cp
  real(r_um), protected :: lfrcp               ! lf/cp
  real(r_um), protected :: ls                  ! lc+lf
  real(r_um), protected :: lsrcp               ! (lc+lf)/cp
  real(r_um), protected :: cv                  ! cp-r
  real(r_um), protected :: recip_a2            ! 1.0/(planet_radius*planet_radius)
  real(r_um), protected :: g_over_r            ! g/r

  ! 32-bit versions of variables
  real(real32), protected :: repsilon_32b
  real(real32), protected :: one_minus_epsilon_32b

contains

subroutine set_planet_constants()

  use constants_mod,     only: r_um, real32
  use planet_config_mod, only: gravity, radius, rd,      &
                               lfric_omega => omega,     &
                               lfric_cp => cp,           &
                               lfric_p_zero => p_zero

  use lfric_atm_water_constants_mod, only: latent_heat_h2o_condensation, &
                                           latent_heat_h2o_fusion,       &
                                           gas_constant_h2o

  implicit none

  omega  = real(lfric_omega, r_um)
  r      = real(rd, r_um)
  cp     = real(lfric_cp, r_um)
  g      = real(gravity, r_um)
  planet_radius = real(radius, r_um)
  pref   = real(lfric_p_zero, r_um)


  ! These variables left in hardwired to earth values as LFRic does not
  ! currently read in any data for these variables.
  sclht          = 6.8e+03_r_um
  lapse          = 0.0065_r_um
  planet_dha     = 2.0_r_um * real(pi, r_um)
  stellar_radius = 6.957e+08_r_um


  ! Set derived constants
  two_omega         = real( 2.0*omega, r_um )
  repsilon          = real( r/gas_constant_h2o, r_um )
  p_zero            = real( pref, r_um)
  recip_p_zero      = real( 1.0/pref, r_um )
  kappa             = real( r/cp, r_um )
  recip_kappa       = real( 1.0/kappa, r_um )
  recip_epsilon     = real( 1.0/repsilon, r_um )
  one_minus_epsilon = real( 1.0-repsilon, r_um )
  c_virtual         = real( 1.0/repsilon-1.0, r_um )

  etar     = real( 1.0/(1.0-repsilon), r_um )
  grcp     = real( gravity/cp, r_um )
  lcrcp    = real( latent_heat_h2o_condensation / cp, r_um )
  lfrcp    = real( latent_heat_h2o_fusion / cp, r_um )
  ls       = real( latent_heat_h2o_condensation + latent_heat_h2o_fusion, r_um )
  lsrcp    = real( (latent_heat_h2o_condensation + latent_heat_h2o_fusion) / &
                   cp, r_um )

  cv       = real( cp - r, r_um )

  recip_a2 = real( 1.0/(planet_radius**2), r_um )
  g_over_r = real( g/r, r_um )


  ! Set 32-bit versions as required, eg in qsat_mod
  repsilon_32b          = real( repsilon, real32 )
  one_minus_epsilon_32b = real( one_minus_epsilon, real32 )

end subroutine set_planet_constants

end module planet_constants_mod
