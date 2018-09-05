!-----------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
!-----------------------------------------------------------------------
!> @brief LFRic Relative Molecular Masses module
!-----------------------------------------------------------------------

module lfric_atm_rel_mol_mass_mod

  use constants_mod, only : r_def

  implicit none

  private
  public :: relative_molecular_mass_s,    &
            relative_molecular_mass_h2o2, &
            relative_molecular_mass_o3,   &
            relative_molecular_mass_h2o,  &
            relative_molecular_mass_dry_air

  !> @name Relative Molecular Mass (kg/mole)
  !> @{
  real(r_def), parameter :: relative_molecular_mass_s = 3.20e-2_r_def
  !< Sulphur
  real(r_def), parameter :: relative_molecular_mass_h2o2 = 3.40e-2_r_def
  !< Hydrogen Peroxide
  real(r_def), parameter :: relative_molecular_mass_o3 = 4.8e-2_r_def
  !< Ozone
  real(r_def), parameter :: relative_molecular_mass_h2o = 1.8e-2_r_def
  !< Water
  real(r_def), parameter :: relative_molecular_mass_dry_air = 2.896e-2_r_def
  !< Dry Air: This should be planet dependent!
  !> @}

end module lfric_atm_rel_mol_mass_mod
