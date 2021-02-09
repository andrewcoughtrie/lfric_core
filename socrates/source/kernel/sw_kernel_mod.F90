!-----------------------------------------------------------------------------
! (c) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! @brief Interface to Socrates for shortwave fluxes (external illumination)

module sw_kernel_mod

use argument_mod,      only : arg_type,                  &
                              GH_FIELD, GH_INTEGER,      &
                              GH_READ, GH_WRITE,         &
                              GH_READWRITE, CELLS,       &
                              ANY_DISCONTINUOUS_SPACE_1, &
                              ANY_DISCONTINUOUS_SPACE_2, &
                              ANY_DISCONTINUOUS_SPACE_3, &
                              ANY_DISCONTINUOUS_SPACE_4, &
                              ANY_DISCONTINUOUS_SPACE_5
use fs_continuity_mod, only:  W3, Wtheta
use constants_mod,     only : r_def, i_def
use kernel_mod,        only : kernel_type

implicit none

private

public :: sw_kernel_type
public :: sw_code

!-------------------------------------------------------------------------------
! Public types
!-------------------------------------------------------------------------------
! The type declaration for the kernel.
! Contains the metadata needed by the PSy layer.
type, extends(kernel_type) :: sw_kernel_type
  private
  type(arg_type) :: meta_args(47) = (/                             &
    arg_type(GH_FIELD,   GH_WRITE,     Wtheta),                    & ! sw_heating_rate
    arg_type(GH_FIELD,   GH_WRITE,     ANY_DISCONTINUOUS_SPACE_1), & ! sw_down_surf
    arg_type(GH_FIELD,   GH_WRITE,     ANY_DISCONTINUOUS_SPACE_1), & ! sw_direct_surf
    arg_type(GH_FIELD,   GH_WRITE,     ANY_DISCONTINUOUS_SPACE_1), & ! sw_down_blue_surf
    arg_type(GH_FIELD,   GH_WRITE,     ANY_DISCONTINUOUS_SPACE_1), & ! sw_direct_blue_surf
    arg_type(GH_FIELD,   GH_WRITE,     ANY_DISCONTINUOUS_SPACE_2), & ! sw_up_tile
    arg_type(GH_FIELD,   GH_WRITE,     ANY_DISCONTINUOUS_SPACE_2), & ! sw_up_blue_tile
    arg_type(GH_FIELD,   GH_READWRITE, Wtheta),                    & ! sw_heating_rate_rts
    arg_type(GH_FIELD,   GH_READWRITE, ANY_DISCONTINUOUS_SPACE_1), & ! sw_down_surf_rts
    arg_type(GH_FIELD,   GH_READWRITE, ANY_DISCONTINUOUS_SPACE_1), & ! sw_direct_surf_rts
    arg_type(GH_FIELD,   GH_READWRITE, ANY_DISCONTINUOUS_SPACE_1), & ! sw_direct_toa_rts
    arg_type(GH_FIELD,   GH_READWRITE, ANY_DISCONTINUOUS_SPACE_1), & ! sw_down_blue_surf_rts
    arg_type(GH_FIELD,   GH_READWRITE, ANY_DISCONTINUOUS_SPACE_1), & ! sw_direct_blue_surf_rts
    arg_type(GH_FIELD,   GH_READWRITE, ANY_DISCONTINUOUS_SPACE_2), & ! sw_up_tile_rts
    arg_type(GH_FIELD,   GH_READWRITE, ANY_DISCONTINUOUS_SPACE_2), & ! sw_up_blue_tile_rts
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! theta
    arg_type(GH_FIELD,   GH_READ,      W3),                        & ! exner
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! exner_in_wth
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! rho_in_wth
    arg_type(GH_FIELD,   GH_READ,      W3),                        & ! height_w3
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! height_wth
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_1), & ! cos_zenith_angle
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_1), & ! lit_fraction
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_1), & ! cos_zenith_angle_rts
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_1), & ! lit_fraction_rts
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_1), & ! stellar_irradiance_rts
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! ozone
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! mv
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! mcl
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! mci
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! area_fraction
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! liquid_fraction
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! ice_fraction
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! cca
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! ccw
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! cloud_drop_no_conc
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_2), & ! tile_fraction
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_3), & ! tile_sw_direct_albedo
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_3), & ! tile_sw_diffuse_albedo
    arg_type(GH_FIELD,   GH_READ,      Wtheta),                    & ! sulphuric
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_4), & ! aer_mix_ratio
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_5), & ! aer_sw_absorption
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_5), & ! aer_sw_scattering
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_5), & ! aer_sw_asymmetry
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_1), & ! latitude
    arg_type(GH_FIELD,   GH_READ,      ANY_DISCONTINUOUS_SPACE_1), & ! longitude
    arg_type(GH_INTEGER, GH_READ                                )  & ! timestep
    /)
  integer :: iterates_over = CELLS
contains
  procedure, nopass :: sw_code
end type

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
contains

! @param[in]    nlayers                 Number of layers
! @param[out]   sw_heating_rate         SW heating rate
! @param[out]   sw_down_surf            SW downward surface flux
! @param[out]   sw_direct_surf          SW unscattered surface flux
! @param[out]   sw_down_blue_surf       SW blue downward surface flux
! @param[out]   sw_direct_blue_surf     SW blue unscattered surface flux
! @param[out]   sw_up_tile              SW upward tiled surface flux
! @param[out]   sw_up_blue_tile         SW blue upward tiled surface flux
! @param[inout] sw_heating_rate_rts     SW heating rate
! @param[inout] sw_down_surf_rts        SW downward surface flux
! @param[inout] sw_direct_surf_rts      SW unscattered surface flux
! @param[inout] sw_direct_toa_rts       SW unscattered top-of-atmosphere flux
! @param[inout] sw_down_blue_surf_rts   SW blue downward surface flux
! @param[inout] sw_direct_blue_surf_rts SW blue unscattered surface flux
! @param[inout] sw_up_tile_rts          SW upward tiled surface flux
! @param[inout] sw_up_blue_tile_rts     SW blue upward tiled surface flux
! @param[in]    theta                   Potential temperature field
! @param[in]    exner                   Exner pressure in density space
! @param[in]    exner_in_wth            Exner pressure in wth space
! @param[in]    rho_in_wth              Density in wth space
! @param[in]    height_w3               Height of w3 levels above surface
! @param[in]    height_wth              Height of wth levels above surface
! @param[in]    cos_zenith_angle        Cosine of the stellar zenith angle
! @param[in]    lit_fraction            Lit fraction of the timestep
! @param[in]    cos_zenith_angle_rts    Cosine of the stellar zenith angle
! @param[in]    lit_fraction_rts        Lit fraction of the timestep
! @param[in]    stellar_irradiance_rts  Stellar irradaince at the planet
! @param[in]    ozone                   Ozone field
! @param[in]    mv                      Water vapour field
! @param[in]    mcl                     Cloud liquid field
! @param[in]    mci                     Cloud ice field
! @param[in]    area_fraction           Total cloud area fraction field
! @param[in]    liquid_fraction         Liquid cloud fraction field
! @param[in]    ice_fraction            Ice cloud fraction field
! @param[in]    cca                     Convective cloud amount (fraction)
! @param[in]    ccw                     Convective cloud water (kg/kg) (can be ice or liquid)
! @param[in]    cloud_drop_no_conc      Cloud Droplet Number Concentration
! @param[in]    tile_fraction           Surface tile fractions
! @param[in]    tile_sw_direct_albedo   SW direct tile albedos
! @param[in]    tile_sw_diffuse_albedo  SW diffuse tile albedos
! @param[in]    sulphuric               Sulphuric acid aerosol
! @param[in]    aer_mix_ratio           MODE aerosol mixing ratios
! @param[in]    aer_sw_absorption       MODE aerosol SW absorption
! @param[in]    aer_sw_scattering       MODE aerosol SW scattering
! @param[in]    aer_sw_asymmetry        MODE aerosol SW asymmetry
! @param[in]    latitude                Latitude field
! @param[in]    longitude               Longitude field
! @param[in]    timestep                Timestep number
! @param[in]    ndf_wth                 No. DOFs per cell for wth space
! @param[in]    undf_wth                No. unique of DOFs for wth space
! @param[in]    map_wth                 Dofmap for wth space column base cell
! @param[in]    ndf_2d                  No. of DOFs per cell for 2D space
! @param[in]    undf_2d                 No. unique of DOFs for 2D space
! @param[in]    map_2d                  Dofmap for 2D space column base cell
! @param[in]    ndf_tile                Number of DOFs per cell for tiles
! @param[in]    undf_tile               Number of total DOFs for tiles
! @param[in]    map_tile                Dofmap for tile space column base cell
! @param[in]    ndf_w3                  No. of DOFs per cell for w3 space
! @param[in]    undf_w3                 No. unique of DOFs for w3 space
! @param[in]    map_w3                  Dofmap for w3 space column base cell
! @param[in]    ndf_rtile               No. of DOFs per cell for rtile space
! @param[in]    undf_rtile              No. unique of DOFs for rtile space
! @param[in]    map_rtile               Dofmap for rtile space column base cell
! @param[in]    ndf_mode                No. of DOFs per cell for mode space
! @param[in]    undf_mode               No. unique of DOFs for mode space
! @param[in]    map_mode                Dofmap for mode space column base cell
! @param[in]    ndf_rmode               No. of DOFs per cell for rmode space
! @param[in]    undf_rmode              No. unique of DOFs for rmode space
! @param[in]    map_rmode               Dofmap for rmode space column base cell
subroutine sw_code(nlayers,                          &
                   sw_heating_rate,                  &
                   sw_down_surf,                     &
                   sw_direct_surf,                   &
                   sw_down_blue_surf,                &
                   sw_direct_blue_surf,              &
                   sw_up_tile,                       &
                   sw_up_blue_tile,                  &
                   sw_heating_rate_rts,              &
                   sw_down_surf_rts,                 &
                   sw_direct_surf_rts,               &
                   sw_direct_toa_rts,                &
                   sw_down_blue_surf_rts,            &
                   sw_direct_blue_surf_rts,          &
                   sw_up_tile_rts,                   &
                   sw_up_blue_tile_rts,              &
                   theta,                            &
                   exner,                            &
                   exner_in_wth,                     &
                   rho_in_wth,                       &
                   height_w3,                        &
                   height_wth,                       &
                   cos_zenith_angle,                 &
                   lit_fraction,                     &
                   cos_zenith_angle_rts,             &
                   lit_fraction_rts,                 &
                   stellar_irradiance_rts,           &
                   ozone,                            &
                   mv,                               &
                   mcl,                              &
                   mci,                              &
                   area_fraction,                    &
                   liquid_fraction,                  &
                   ice_fraction,                     &
                   cca, ccw,                         &
                   cloud_drop_no_conc,               &
                   tile_fraction,                    &
                   tile_sw_direct_albedo,            &
                   tile_sw_diffuse_albedo,           &
                   sulphuric,                        &
                   aer_mix_ratio,                    &
                   aer_sw_absorption,                &
                   aer_sw_scattering,                &
                   aer_sw_asymmetry,                 &
                   latitude, longitude,              &
                   timestep,                         &
                   ndf_wth, undf_wth, map_wth,       &
                   ndf_2d, undf_2d, map_2d,          &
                   ndf_tile, undf_tile, map_tile,    &
                   ndf_w3, undf_w3, map_w3,          &
                   ndf_rtile, undf_rtile, map_rtile, &
                   ndf_mode, undf_mode, map_mode,    &
                   ndf_rmode, undf_rmode, map_rmode)

  use well_mixed_gases_config_mod, only: &
    co2_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, o2_mix_ratio
  use radiation_config_mod, only: n_radstep,                  &
    l_planet_grey_surface, planet_albedo, l_rayleigh_sw,      &
    i_cloud_ice_type_sw, i_cloud_liq_type_sw,                 &
    cloud_horizontal_rsd, cloud_vertical_decorr,              &
    l_trans_zen_correction
  use set_thermodynamic_mod, only: set_thermodynamic
  use set_cloud_field_mod, only: set_cloud_field
  use jules_control_init_mod, only: n_surf_tile, sw_band_tile
  use init_aerosol_fields_alg_mod, only: l_radaer, l_sulphuric, &
    n_aer_mode, mode_dimen, sw_band_mode
  use socrates_runes, only: runes, ip_source_illuminate
  use socrates_bones, only: bones

  implicit none

  ! Arguments
  integer(i_def), intent(in) :: nlayers, timestep
  integer(i_def), intent(in) :: ndf_wth, ndf_w3, ndf_2d
  integer(i_def), intent(in) :: ndf_tile, ndf_rtile, ndf_mode, ndf_rmode
  integer(i_def), intent(in) :: undf_wth, undf_w3, undf_2d
  integer(i_def), intent(in) :: undf_tile, undf_rtile, undf_mode, undf_rmode

  integer(i_def), dimension(ndf_wth),   intent(in) :: map_wth
  integer(i_def), dimension(ndf_w3),    intent(in) :: map_w3
  integer(i_def), dimension(ndf_2d),    intent(in) :: map_2d
  integer(i_def), dimension(ndf_tile),  intent(in) :: map_tile
  integer(i_def), dimension(ndf_rtile), intent(in) :: map_rtile
  integer(i_def), dimension(ndf_mode),  intent(in) :: map_mode
  integer(i_def), dimension(ndf_rmode), intent(in) :: map_rmode

  real(r_def), dimension(undf_wth),  intent(out) :: sw_heating_rate
  real(r_def), dimension(undf_2d),   intent(out) :: sw_down_surf, &
    sw_direct_surf, sw_down_blue_surf, sw_direct_blue_surf
  real(r_def), dimension(undf_tile), intent(out) :: sw_up_tile, sw_up_blue_tile

  real(r_def), dimension(undf_wth),  intent(inout) :: sw_heating_rate_rts
  real(r_def), dimension(undf_2d),   intent(inout) :: sw_down_surf_rts, &
    sw_direct_surf_rts, sw_direct_toa_rts, &
    sw_down_blue_surf_rts, sw_direct_blue_surf_rts
  real(r_def), dimension(undf_tile), intent(inout) :: sw_up_tile_rts, &
    sw_up_blue_tile_rts

  real(r_def), dimension(undf_w3),   intent(in) :: exner, height_w3
  real(r_def), dimension(undf_wth),  intent(in) :: theta, exner_in_wth, &
    rho_in_wth, height_wth, ozone, mv, mcl, mci, &
    area_fraction, liquid_fraction, ice_fraction, cca, ccw, cloud_drop_no_conc
  real(r_def), dimension(undf_2d), intent(in) :: &
    cos_zenith_angle, lit_fraction, &
    cos_zenith_angle_rts, lit_fraction_rts, stellar_irradiance_rts
  real(r_def), dimension(undf_tile),  intent(in) :: tile_fraction
  real(r_def), dimension(undf_rtile), intent(in) :: &
    tile_sw_direct_albedo, tile_sw_diffuse_albedo
  real(r_def), dimension(undf_wth),   intent(in) :: sulphuric
  real(r_def), dimension(undf_mode),  intent(in) :: aer_mix_ratio
  real(r_def), dimension(undf_rmode), intent(in) :: &
    aer_sw_absorption, aer_sw_scattering, aer_sw_asymmetry
  real(r_def), dimension(undf_2d), intent(in) :: latitude, longitude

  ! Local variables for the kernel
  integer(i_def), parameter :: n_profile = 1
  integer(i_def) :: i_cloud_representation, i_overlap, i_inhom, i_drop_re
  integer(i_def) :: rand_seed(n_profile)
  integer(i_def) :: n_cloud_layer
  integer(i_def) :: i_tile, rtile_1, rtile_last
  integer(i_def) :: wth_1, wth_nlayers, mode_1, mode_last, rmode_1, rmode_last
  real(r_def), dimension(nlayers) :: layer_heat_capacity
    ! Heat capacity for each layer
  real(r_def), dimension(nlayers) :: p_layer, t_layer
    ! Layer pressure and temperature
  real(r_def), dimension(nlayers) :: d_mass
    ! Mass of layer per square metre
  real(r_def), dimension(nlayers) :: cloud_frac, liq_dim
    ! Large-scale cloud fields
  real(r_def), dimension(nlayers) :: conv_frac, &
    liq_conv_frac, ice_conv_frac, liq_conv_mmr, ice_conv_mmr, liq_conv_dim
    ! Convective cloud fields
  real(r_def), dimension(0:nlayers) :: sw_direct, sw_down, sw_up

  ! Tiled surface fields
  real(r_def), dimension(n_surf_tile) :: frac_tile, &
    flux_up_tile_rts, flux_up_blue_tile_rts, &
    flux_up_tile, flux_up_blue_tile


  ! Set indexing
  wth_1 = map_wth(1)+1
  wth_nlayers = map_wth(1)+nlayers
  rtile_1 = map_rtile(1)
  rtile_last = map_rtile(1)+sw_band_tile-1
  mode_1 = map_mode(1)+1
  mode_last = map_mode(1)+(nlayers+1)*mode_dimen-1
  rmode_1 = map_rmode(1)+1
  rmode_last = map_rmode(1)+(nlayers+1)*sw_band_mode-1

  if (mod(timestep-1_i_def, n_radstep) == 0) then
    ! Radiation time-step: full calculation of radiative fluxes

    ! Set up pressures, temperatures, masses and heat capacities
    call set_thermodynamic(nlayers,                &
      exner(map_w3(1):map_w3(1)+nlayers-1),        &
      exner_in_wth(map_wth(1):map_wth(1)+nlayers), &
      theta(map_wth(1):map_wth(1)+nlayers),        &
      rho_in_wth(map_wth(1):map_wth(1)+nlayers),   &
      height_w3(map_w3(1):map_w3(1)+nlayers-1),    &
      height_wth(map_wth(1):map_wth(1)+nlayers),   &
      p_layer, t_layer, d_mass, layer_heat_capacity)

    ! Set up cloud fields for radiation
    call set_cloud_field(nlayers, n_profile, &
      area_fraction(wth_1:wth_nlayers), &
      cca(wth_1:wth_nlayers), ccw(wth_1:wth_nlayers), t_layer, &
      latitude(map_2d(1):map_2d(1)), longitude(map_2d(1):map_2d(1)), &
      i_cloud_representation, i_overlap, i_inhom, i_drop_re, &
      rand_seed, n_cloud_layer, cloud_frac, liq_dim, conv_frac, &
      liq_conv_frac, ice_conv_frac, liq_conv_mmr, ice_conv_mmr, liq_conv_dim)

    ! Tile fractions
    do i_tile = 1, n_surf_tile
      frac_tile(i_tile) = tile_fraction(map_tile(i_tile))
    end do

    ! Calculate the SW fluxes (RUN the Edwards-Slingo two-stream solver)
    call runes(n_profile, nlayers,                                             &
      spectrum_name          = 'sw',                                           &
      i_source               = ip_source_illuminate,                           &
      n_cloud_layer          = n_cloud_layer,                                  &
      p_layer_1d             = p_layer,                                        &
      t_layer_1d             = t_layer,                                        &
      mass_1d                = d_mass,                                         &
      density_1d             = rho_in_wth(wth_1:wth_nlayers),                  &
      h2o_1d                 = mv(wth_1:wth_nlayers),                          &
      o3_1d                  = ozone(wth_1:wth_nlayers),                       &
      co2_mix_ratio          = co2_mix_ratio,                                  &
      n2o_mix_ratio          = n2o_mix_ratio,                                  &
      ch4_mix_ratio          = ch4_mix_ratio,                                  &
      o2_mix_ratio           = o2_mix_ratio,                                   &
      cos_zenith_angle       = cos_zenith_angle_rts(map_2d(1):map_2d(1)),      &
      solar_irrad            = stellar_irradiance_rts(map_2d(1):map_2d(1)),    &
      l_grey_albedo          = l_planet_grey_surface,                          &
      grey_albedo            = planet_albedo,                                  &
      l_tile                 = .not.l_planet_grey_surface,                     &
      n_tile                 = n_surf_tile,                                    &
      frac_tile_1d           = frac_tile,                                      &
      albedo_diff_tile_1d    = tile_sw_diffuse_albedo(rtile_1:rtile_last),     &
      albedo_dir_tile_1d     = tile_sw_direct_albedo(rtile_1:rtile_last),      &
      cloud_frac_1d          = cloud_frac,                                     &
      liq_frac_1d            = liquid_fraction(wth_1:wth_nlayers),             &
      ice_frac_1d            = ice_fraction(wth_1:wth_nlayers),                &
      liq_mmr_1d             = mcl(wth_1:wth_nlayers),                         &
      ice_mmr_1d             = mci(wth_1:wth_nlayers),                         &
      liq_dim_1d             = liq_dim,                                        &
      liq_nc_1d              = cloud_drop_no_conc(wth_1:wth_nlayers),          &
      conv_frac_1d           = conv_frac,                                      &
      liq_conv_frac_1d       = liq_conv_frac,                                  &
      ice_conv_frac_1d       = ice_conv_frac,                                  &
      liq_conv_mmr_1d        = liq_conv_mmr,                                   &
      ice_conv_mmr_1d        = ice_conv_mmr,                                   &
      liq_conv_dim_1d        = liq_conv_dim,                                   &
      liq_conv_nc_1d         = cloud_drop_no_conc(wth_1:wth_nlayers),          &
      cloud_horizontal_rsd   = cloud_horizontal_rsd,                           &
      cloud_vertical_decorr  = cloud_vertical_decorr,                          &
      conv_vertical_decorr   = cloud_vertical_decorr,                          &
      rand_seed              = rand_seed,                                      &
      layer_heat_capacity_1d = layer_heat_capacity,                            &
      l_rayleigh             = l_rayleigh_sw,                                  &
      l_mixing_ratio         = .true.,                                         &
      i_cloud_representation = i_cloud_representation,                         &
      i_overlap              = i_overlap,                                      &
      i_inhom                = i_inhom,                                        &
      i_drop_re              = i_drop_re,                                      &
      i_st_water             = i_cloud_liq_type_sw,                            &
      i_st_ice               = i_cloud_ice_type_sw,                            &
      i_cnv_water            = i_cloud_liq_type_sw,                            &
      i_cnv_ice              = i_cloud_ice_type_sw,                            &
      l_sulphuric            = l_sulphuric,                                    &
      sulphuric_1d           = sulphuric(wth_1:wth_nlayers),                   &
      l_aerosol_mode         = l_radaer,                                       &
      n_aer_mode             = n_aer_mode,                                     &
      n_aer_layer            = nlayers+1,                                      &
      aer_mix_ratio_1d       = aer_mix_ratio(mode_1:mode_last),                &
      aer_absorption_1d      = aer_sw_absorption(rmode_1:rmode_last),          &
      aer_scattering_1d      = aer_sw_scattering(rmode_1:rmode_last),          &
      aer_asymmetry_1d       = aer_sw_asymmetry(rmode_1:rmode_last),           &
      l_invert               = .true.,                                         &
      flux_direct_1d         = sw_direct,                                      &
      flux_down_1d           = sw_down,                                        &
      flux_up_1d             = sw_up,                                          &
      flux_up_tile_1d        = flux_up_tile_rts,                               &
      flux_up_blue_tile_1d   = flux_up_blue_tile_rts,                          &
      flux_direct_blue_surf  = sw_direct_blue_surf_rts(map_2d(1):map_2d(1)),   &
      flux_down_blue_surf    = sw_down_blue_surf_rts(map_2d(1):map_2d(1)),     &
      heating_rate_1d        = sw_heating_rate_rts(wth_1:wth_nlayers))

    ! Set level 0 increment such that theta increment will equal level 1
    sw_heating_rate_rts(map_wth(1)) = sw_heating_rate_rts(map_wth(1) + 1) &
                                    * exner_in_wth(map_wth(1))            &
                                    / exner_in_wth(map_wth(1) + 1)

    ! Set surface and top-of-atmosphere fluxes
    sw_down_surf_rts(map_2d(1)) = sw_down(0)
    sw_direct_surf_rts(map_2d(1)) = sw_direct(0)
    sw_direct_toa_rts(map_2d(1)) = sw_direct(nlayers)

    ! Set tiled fluxes
    do i_tile = 1, n_surf_tile
      sw_up_tile_rts(map_tile(i_tile)) = flux_up_tile_rts(i_tile)
      sw_up_blue_tile_rts(map_tile(i_tile)) = flux_up_blue_tile_rts(i_tile)
    end do
  else
    ! Not a radiation time-step

    ! Set tiled fluxes
    do i_tile = 1, n_surf_tile
      flux_up_tile_rts(i_tile) = sw_up_tile_rts(map_tile(i_tile))
      flux_up_blue_tile_rts(i_tile) = sw_up_blue_tile_rts(map_tile(i_tile))
    end do
  end if

  if (n_radstep == 1) then
    ! Radiation timestep = model timestep
    sw_heating_rate(map_wth(1):wth_nlayers) &
      = sw_heating_rate_rts(map_wth(1):wth_nlayers)

    sw_down_surf(map_2d(1))        = sw_down_surf_rts(map_2d(1))
    sw_direct_surf(map_2d(1))      = sw_direct_surf_rts(map_2d(1))
    sw_down_blue_surf(map_2d(1))   = sw_down_blue_surf_rts(map_2d(1))
    sw_direct_blue_surf(map_2d(1)) = sw_direct_blue_surf_rts(map_2d(1))

    do i_tile = 1, n_surf_tile
      sw_up_tile(map_tile(i_tile))      = sw_up_tile_rts(map_tile(i_tile))
      sw_up_blue_tile(map_tile(i_tile)) = sw_up_blue_tile_rts(map_tile(i_tile))
    end do
  else
    ! Corrections to model timestep. The radiative fluxes have been calculated
    ! for a mean sun angle over the radiation timestep and must be converted
    ! to fluxes appropriate for the mean sun angle over the shorter model
    ! timestep.

    ! The bare "bones" of a simple radiative transfer calculation.
    ! Apply the solar zenith angle correction.
    call bones(n_profile, nlayers,                                             &
      n_tile                    = n_surf_tile,                                 &
      l_cos_zen_correction      = .true.,                                      &
      cos_zen_rts               = cos_zenith_angle_rts(map_2d(1):map_2d(1)),   &
      lit_frac_rts              = lit_fraction_rts(map_2d(1):map_2d(1)),       &
      cos_zen_mts               = cos_zenith_angle(map_2d(1):map_2d(1)),       &
      lit_frac_mts              = lit_fraction(map_2d(1):map_2d(1)),           &
      l_trans_zen_correction    = l_trans_zen_correction,                      &
      flux_direct_toa_rts       = sw_direct_toa_rts(map_2d(1):map_2d(1)),      &
      heating_rate_1d_rts       = sw_heating_rate_rts(wth_1:wth_nlayers),      &
      flux_up_tile_1d_rts       = flux_up_tile_rts,                            &
      flux_up_blue_tile_1d_rts  = flux_up_blue_tile_rts,                       &
      flux_direct_surf_rts      = sw_direct_surf_rts(map_2d(1):map_2d(1)),     &
      flux_down_surf_rts        = sw_down_surf_rts(map_2d(1):map_2d(1)),       &
      flux_direct_blue_surf_rts = sw_direct_blue_surf_rts(map_2d(1):map_2d(1)),&
      flux_down_blue_surf_rts   = sw_down_blue_surf_rts(map_2d(1):map_2d(1)),  &
      heating_rate_1d_mts       = sw_heating_rate(wth_1:wth_nlayers),          &
      flux_up_tile_1d_mts       = flux_up_tile,                                &
      flux_up_blue_tile_1d_mts  = flux_up_blue_tile,                           &
      flux_direct_surf_mts      = sw_direct_surf(map_2d(1):map_2d(1)),         &
      flux_down_surf_mts        = sw_down_surf(map_2d(1):map_2d(1)),           &
      flux_direct_blue_surf_mts = sw_direct_blue_surf(map_2d(1):map_2d(1)),    &
      flux_down_blue_surf_mts   = sw_down_blue_surf(map_2d(1):map_2d(1)))

    ! Set level 0 increment such that theta increment will equal level 1
    sw_heating_rate(map_wth(1)) = sw_heating_rate(map_wth(1) + 1) &
                                * exner_in_wth(map_wth(1))        &
                                / exner_in_wth(map_wth(1) + 1)

    ! Set tiled fluxes
    do i_tile = 1, n_surf_tile
      sw_up_tile(map_tile(i_tile)) = flux_up_tile(i_tile)
      sw_up_blue_tile(map_tile(i_tile)) = flux_up_blue_tile(i_tile)
    end do
  end if

end subroutine sw_code

end module sw_kernel_mod
