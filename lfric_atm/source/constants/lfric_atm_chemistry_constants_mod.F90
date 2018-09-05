!-----------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!-----------------------------------------------------------------------
!> @brief LFRic chemistry constants module
!-----------------------------------------------------------------------
!
module lfric_atm_chemistry_constants_mod

  use constants_mod, only: r_def

  implicit none

  private
  public :: avogadro, boltzmann, density_so4,     &
            mean_free_path_reference,             &
            temperature_mean_free_path_reference, &
            pressure_mean_free_path_reference

  !> Number of molecules in 1 mole
  real(r_def), parameter :: avogadro = 6.022e23_r_def

  !> Boltzmanns constant (J/K)
  real(r_def), parameter :: boltzmann = 1.3804e-23_r_def

  !> Density of SO4 particle (kg/m^3)
  real(r_def), parameter :: density_so4 = 1769.0_r_def

  !> @name Mean Free Path constants
  !> @{
  real(r_def), parameter :: mean_free_path_reference = 6.6e-8_r_def
  !< [m]
  real(r_def), parameter :: temperature_mean_free_path_reference = 293.15_r_def
  !< [K]
  real(r_def), parameter :: pressure_mean_free_path_reference = 1.01325e5_r_def
  !< [Pa]
  !> @}

end module lfric_atm_chemistry_constants_mod

