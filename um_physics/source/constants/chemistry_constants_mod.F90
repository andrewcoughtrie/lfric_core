!-----------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!-----------------------------------------------------------------------
!> @brief LFRic inteface module for UM code (chemistry_constants_mod)
!-----------------------------------------------------------------------

module chemistry_constants_mod

  use constants_mod, only: r_um
  use lfric_atm_chemistry_constants_mod, only:                                 &
                                         lfric_avogadro => avogadro,           &
                                         lfric_boltzmann => boltzmann,         &
                                         density_so4,                          &
                                         mean_free_path_reference,             &
                                         temperature_mean_free_path_reference, &
                                         pressure_mean_free_path_reference

  implicit none

  private
  public :: boltzmann, mfp_ref, pref_mfp, tref_mfp

  ! The following variables have been hidden as they are not currently
  ! required to build the extracted UM code. They have been left in
  ! in case they are required as more UM code is drawn into the lfric_atm
  ! build. Should they be required at a later date, they should simply be
  ! added to the public statement above.

  ! Disabled variables:
  !   avogadro, rho_so4

  ! Number of molecules in 1 mole
  real(r_um), parameter :: avogadro = real(lfric_avogadro, r_um)

  ! Boltzmanns constant [J/K]
  real(r_um), parameter :: boltzmann = real(lfric_boltzmann, r_um)

  ! Density of so4 particle [kg/m^3]
  real(r_um), parameter :: rho_so4 = real(density_so4, r_um)

  ! Mean free path
  ! Ref value [m]
  real(r_um), parameter :: mfp_ref =  real(mean_free_path_reference, r_um)
  ! Ref temperature [K]
  real(r_um), parameter :: tref_mfp = real(temperature_mean_free_path_reference, r_um)
  ! Ref pressure [Pa]
  real(r_um), parameter :: pref_mfp = real(pressure_mean_free_path_reference, r_um)

end module chemistry_constants_mod

