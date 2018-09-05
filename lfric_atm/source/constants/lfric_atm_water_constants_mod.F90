!-----------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!-----------------------------------------------------------------------
!> @brief LFRic water constants module
!-----------------------------------------------------------------------

module lfric_atm_water_constants_mod

  use constants_mod, only : r_def

  implicit none

  private
  public :: T_freeze_h2o_sea, T_freeze_h2o, density_h2o,                &
            density_h2o_sea, density_ice, latent_heat_h2o_condensation, &
            latent_heat_h2o_fusion, heat_capacity_h2o_vapour,           &
            heat_capacity_h2o, heat_capacity_ice, gas_constant_h2o,     &
            jules_dpsidt

  !> @name Temperature, [K]
  !> @{
  real(r_def), parameter :: T_freeze_h2o_sea =271.35_r_def
  !< Temperature at which sea water freezes, [K]
  real(r_def), parameter :: T_freeze_h2o = 273.15_r_def
  !< Temperature at which fresh water freezes and ice melts, [K]
  !> @}

  !> @name Density, [kg/m^3]
  !> @{
  real(r_def), parameter :: density_h2o = 1000.0_r_def
  !< Density of water [kg/m^3]
  real(r_def), parameter :: density_h2o_sea = 1026.0_r_def
  !< Density of sea water [kg/m^3]
  real(r_def), parameter :: density_ice = 917.0_r_def
  !< Density of ice [kg/m^3]
  !> @}

  !> @name Specific Latent Heat at 0 degC, [J/kg]
  !> @{
  real(r_def), parameter :: latent_heat_h2o_condensation = 2.501e6_r_def
  !< Specific Latent heat of condensation of water at 0 degC [J/kg]

  real(r_def), parameter :: latent_heat_h2o_fusion = 0.334e6_r_def
  !< Specific Latent heat of fusion of water at 0 degC [J/kg]
  !> @}

  !> @name Specific Heat Capacity, [J/kg]
  !> @{
  real(r_def), parameter :: heat_capacity_h2o_vapour = 1850.0_r_def
  !< Specific heat capacity of water vapour, [J/(kg K)]

  real(r_def), parameter :: heat_capacity_h2o = 4180.0_r_def
  !< Specific heat capacity of water, [J/(kg K)]

  real(r_def), parameter :: heat_capacity_ice = 2100.0_r_def
  !< Specific heat capacity of ice [J/(kg K)]
  !> @}

  real(r_def), parameter :: gas_constant_h2o = 461.51_r_def
  !< Gas constant of water [J/(mol K)]

  real(r_def), parameter :: jules_dpsidt = 114.3_r_def
  !< Rate of change of soil matrix potential with temperature at
  !< equilibrium between water and ice in partially frozen soil [m/K].

end module lfric_atm_water_constants_mod
