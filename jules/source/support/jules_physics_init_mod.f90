!----------------------------------------------------------------------------
! (c) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!----------------------------------------------------------------------------
!> @brief Controls the setting of variables for Jules physics schemes, which
!>         are either fixed in LFRic or derived from LFRic inputs

module jules_physics_init_mod

  ! Other LFRic modules used
  use constants_mod,          only : r_um, i_def
  use jules_control_init_mod, only : n_sea_ice_tile, n_land_tile
  use surface_config_mod,     only : use_hydrology,                            &
                                     fixed_sea_albedo_in => fixed_sea_albedo,  &
                                     non_iso_scatter, sea_alb_method,          &
                                     sea_alb_method_barker, sea_alb_method_jin,&
                                     sea_alb_method_fixed, sea_alb_var_chl,    &
                                     blue_sky_alb, sea_surf_alg, albedo_obs,   &
                                     sea_surf_alg_coare,                       &
                                     sea_surf_alg_surf_div,                    &
                                     alb_sice_melt, dt_ice_albedo,             &
                                     emis_sea_in => emis_sea,                  &
                                     emis_sice_in => emis_sice,                &
                                     therm_cond_sice, therm_cond_sice_snow,    &
                                     therm_cond_sea,                           &
                                     iceformdrag_lupkes, stability_lupkes,     &
                                     sice_heatflux,                            &
                                     basal_melting, basal_melting_none,        &
                                     basal_melting_instant, grain_growth,      &
                                     grain_growth_marshall,                    &
                                     grain_growth_taillandier, relayer_opt,    &
                                     relayer_opt_original, relayer_opt_inverse,&
                                     rho_snow_fresh_in => rho_snow_fresh,      &
                                     dpsids_dsdz, soil_sat_down,               &
                                     cor_mo_iter_in => cor_mo_iter,            &
                                     cor_mo_iter_lim_oblen,                    &
                                     cor_mo_iter_improved, srf_ex_cnv_gust,    &
                                     formdrag_in => formdrag, formdrag_none,   &
                                     formdrag_eff_z0, formdrag_dist_drag,      &
                                     fd_stability_dep, fd_stability_dep_none,  &
                                     fd_stability_dep_surf_ri,                 &
                                     alb_snocov_nvg, alb_snofree_nvg,          &
                                     can_cap_nvg, heat_cap_nvg,                &
                                     alb_snocov_max,                           &
                                     alb_leaf_nir, alb_leaf_vis,               &
                                     light_extinct, scat_coef_vis,             &
                                     scat_coef_nir, z0hm_ratio_pft,            &
                                     use_variable_sst, heat_cap_sea,           &
                                     evap_scale_sea

  ! UM modules used
  use jules_surface_types_mod, only : npft, nnvg
  use nlsizes_namelist_mod,    only : sm_levels, land_field, ntiles

  implicit none

  ! Decrease in saturated hydraulic conductivity with depth (m-1)
  ! This is a 2D field in the UM/Jules, but is spatially and temporally
  ! invariant, so we instead declare it as a parameter here
  real(kind=r_um), parameter :: decrease_sath_cond = 1.0_r_um

  ! The total size of snow arrays on snow levels (nsmax) and land tiles
  ! (n_land_tile)
  integer(kind=i_def), protected :: snow_lev_tile

  private
  public :: jules_physics_init, decrease_sath_cond, snow_lev_tile

contains

  !>@brief Initialise Jules physics variables which are either fixed in LFRic
  !>        or derived from LFRic inputs
  !>@details This file sets many parameters and switches which are currently
  !>          in the Jules namelists. Many of these will never be promoted to
  !>          the LFRic namelist as they are legacy options not fit for future
  !>          use. Hence we set them here until such time as we can retire them
  !>          from the Jules code.
  !>        Other parameters and switches which are genuinely input variables,
  !>         via the LFRic namelists, are also set here for the Jules code.
  !>       Where possible, all values are taken from GL7 science settings.
  subroutine jules_physics_init()

    ! Jules modules containing things that need setting
    use allocate_jules_arrays_mod, only: allocate_jules_arrays
    use bl_option_mod, only: on
    use c_kappai, only: kappai, kappai_snow, kappa_seasurf
    use c_z0h_z0m, only: z0h_z0m
    use jules_hydrology_mod, only: l_hydrology, check_jules_hydrology,      &
         l_top, nfita
    use jules_radiation_mod, only: i_sea_alb_method,                        &
         l_embedded_snow, l_mask_snow_orog,                                 &
         l_spec_alb_bs, l_spec_albedo, l_spec_sea_alb, fixed_sea_albedo,    &
         check_jules_radiation, l_niso_direct, l_sea_alb_var_chl,           &
         l_albedo_obs
    use jules_science_fixes_mod, only: l_dtcanfix, l_fix_alb_ice_thick,     &
         l_fix_albsnow_ts, l_fix_ctile_orog, l_fix_wind_snow
    use jules_sea_seaice_mod, only: nice_use, iseasurfalg, emis_sea,        &
         seasalinityfactor, nice, ip_ss_surf_div, z0sice,                   &
         z0h_z0m_sice, emis_sice, l_ctile, l_tstar_sice_new,                &
         l_sice_heatflux, check_jules_sea_seaice, z0h_z0m_miz,              &
         ip_ss_coare_mq, a_chrn_coare, b_chrn_coare, u10_max_coare,         &
         l_10m_neut, alpham, dtice, l_iceformdrag_lupkes,                   &
         l_stability_lupkes, l_use_dtstar_sea, hcap_sea, beta_evap,         &
         buddy_sea
    use jules_snow_mod, only: cansnowpft, check_jules_snow, nsmax,          &
         a_snow_et, b_snow_et, c_snow_et, can_clump, dzsnow,                &
         frac_snow_subl_melt, i_snow_cond_parm, l_et_metamorph,             &
         l_snow_infilt, l_snow_nocan_hc, l_snowdep_surf, lai_alb_lim_sn,    &
         n_lai_exposed, rho_snow_et_crit, rho_snow_fresh, snow_hcon,        &
         unload_rate_u, i_basal_melting_opt, i_grain_growth_opt,            &
         i_relayer_opt
    use jules_soil_mod, only: dzsoil_io, l_dpsids_dsdz, l_soil_sat_down,    &
         l_vg_soil, soilhc_method, check_jules_soil
    use jules_soil_biogeochem_mod, only: const_ch4_cs,                      &
         check_jules_soil_biogeochem
    use jules_surface_mod, only: l_epot_corr, cor_mo_iter, iscrntdiag,      &
         isrfexcnvgust, Limit_ObukhovL, ip_scrndecpl2, IP_SrfExWithCnv,     &
         diff_frac, fd_stab_dep, orog_drag_param, check_jules_surface,      &
         Improve_Initial_Guess, formdrag, beta_cnv_bl, fd_hill_option,      &
         i_modiscopt, l_land_ice_imp, no_drag, effective_z0,                &
         capped_lowhill, explicit_stress
    use jules_vegetation_mod, only: can_rad_mod, ilayers, l_vegcan_soilfx,  &
         photo_model, photo_collatz, check_jules_vegetation
    use nvegparm, only:                                                     &
         albsnc_nvg, albsnf_nvgu, albsnf_nvg, albsnf_nvgl, catch_nvg,       &
         ch_nvg, emis_nvg, gs_nvg, infil_nvg, vf_nvg, z0_nvg
    use pftparm, only:                                                      &
         a_wl, a_ws, albsnc_max, albsnc_min, albsnf_maxu, albsnf_maxl,      &
         alniru, alnir, alnirl, alparu, alpar, alparl, alpha, b_wl, c3,     &
         can_struct_a, catch0, dcatch_dlai, dgl_dm, dgl_dt, dqcrit,         &
         dz0v_dh, emis_pft, eta_sl, f0, fd, fsmc_of, fsmc_p0,               &
         g_leaf_0, glmin, hw_sw, infil_f, kext, kn, knl, kpar, lai_alb_lim, &
         lma, neff, nl0, nmass, nr, nr_nl, ns_nl, nsw, omega, omegal,       &
         omegau, omnir, omnirl, omniru, orient, q10_leaf, r_grow, rootd_ft, &
         sigl, tleaf_of, tlow, tupp, vint, vsl

    implicit none

    ! ----------------------------------------------------------------
    ! Jules hydrology settings - contained in module jules_hydrology
    ! ----------------------------------------------------------------
    l_hydrology = use_hydrology
    l_top       = .true.
    ! l_var_rainfrac = .true. should be set here but needs coding
    nfita       = 30

    ! Check the contents of the hydrology parameters module
    call check_jules_hydrology()

    ! ----------------------------------------------------------------
    ! Jules radiation settings - contained in module jules_radiation
    ! ----------------------------------------------------------------
    fixed_sea_albedo = real(fixed_sea_albedo_in, r_um)
    select case (sea_alb_method)
      case(sea_alb_method_barker)
        i_sea_alb_method = 2
      case(sea_alb_method_jin)
        i_sea_alb_method = 3
      case(sea_alb_method_fixed)
        i_sea_alb_method = 4
    end select
    l_albedo_obs = albedo_obs
    l_embedded_snow  = .true.
    l_mask_snow_orog = .true.
    l_niso_direct    = non_iso_scatter
    l_sea_alb_var_chl = sea_alb_var_chl
    l_spec_alb_bs    = blue_sky_alb
    l_spec_albedo    = .true.
    l_spec_sea_alb   = .true.

    ! Check the contents of the radiation parameters module
    call check_jules_radiation()

    ! ----------------------------------------------------------------
    ! Jules sea and sea-ice settings - contained in module jules_sea_seaice
    !                                   and c_kappai
    ! ----------------------------------------------------------------
    kappai        = real(therm_cond_sice, r_um)
    kappai_snow   = real(therm_cond_sice_snow, r_um)
    kappa_seasurf = real(therm_cond_sea, r_um)

    a_chrn_coare         = 0.0016_r_um
    alpham               = real(alb_sice_melt, r_um)
    b_chrn_coare         = -0.0035_r_um
    beta_evap            = real(evap_scale_sea, r_um)
    buddy_sea            = on
    dtice                = real(dt_ice_albedo, r_um)
    emis_sea             = real(emis_sea_in, r_um)
    emis_sice            = real(emis_sice_in, r_um)
    select case (sea_surf_alg)
      case(sea_surf_alg_surf_div)
        iseasurfalg = ip_ss_surf_div
      case(sea_surf_alg_coare)
        iseasurfalg = ip_ss_coare_mq
    end select
    l_10m_neut           = .false.
    ! l_ctile is implicitly true by design of LFRic and should not be changed
    l_ctile              = .true.
    l_iceformdrag_lupkes = iceformdrag_lupkes
    l_stability_lupkes   = stability_lupkes
    l_sice_heatflux      = sice_heatflux
    ! Code has not been included to support this being false as configurations
    ! should be moving to the new code
    l_tstar_sice_new     = .true.
    l_use_dtstar_sea     = use_variable_sst
    if (use_variable_sst) hcap_sea = real(heat_cap_sea, r_um)
    nice                 = n_sea_ice_tile
    nice_use             = n_sea_ice_tile
    seasalinityfactor    = 0.98_r_um
    u10_max_coare        = 22.0_r_um
    z0h_z0m_miz          = 0.2_r_um
    z0h_z0m_sice         = 0.2_r_um
    z0sice               = 5.0e-4_r_um

    ! Check the contents of the sea_seaice parameters module
    call check_jules_sea_seaice()

    ! ----------------------------------------------------------------
    ! Jules snow settings - contained in module jules_snow
    ! ----------------------------------------------------------------
    nsmax                  = 3
    a_snow_et              = 2.8e-6_r_um
    b_snow_et              = 0.042_r_um
    c_snow_et              = 0.046_r_um
    can_clump(1:npft)      = (/ 1.0,4.0,1.0,1.0,1.0 /)
    cansnowpft(1:npft)     = (/ .false.,.true.,.false.,.false.,.false. /)
    dzsnow(1:nsmax)        = (/ 0.04,0.12,0.34 /)
    frac_snow_subl_melt    = 1
    select case (basal_melting)
      case(basal_melting_none)
        i_basal_melting_opt = 0
      case(basal_melting_instant)
        i_basal_melting_opt = 1
    end select
    select case (grain_growth)
      case(grain_growth_marshall)
        i_grain_growth_opt = 0
      case(grain_growth_taillandier)
        i_grain_growth_opt = 1
    end select
    select case (relayer_opt)
      case(relayer_opt_original)
        i_relayer_opt = 0
      case(relayer_opt_inverse)
        i_relayer_opt = 1
    end select
    i_snow_cond_parm       = 1
    l_et_metamorph         = .true.
    l_snow_infilt          = .true.
    l_snow_nocan_hc        = .true.
    l_snowdep_surf         = .true.
    lai_alb_lim_sn(1:npft) = (/ 1.0,1.0,0.1,0.1,0.1 /)
    n_lai_exposed(1:npft)  = (/ 1.0,1.0,3.0,3.0,2.0 /)
    rho_snow_et_crit       = 150.0_r_um
    rho_snow_fresh         = real(rho_snow_fresh_in, r_um)
    snow_hcon              = 0.1495_r_um
    unload_rate_u(1:npft)  = (/ 0.0,2.31e-6,0.0,0.0,0.0 /)

    ! Set the LFRic dimension
    snow_lev_tile = nsmax * n_land_tile

    ! Check the contents of the Jules snow parameters module
    ! This module sets some derived parameters
    call check_jules_snow()

    ! ----------------------------------------------------------------
    ! Jules soil settings - contained in modules jules_soil
    ! ----------------------------------------------------------------
    ! The number of levels specified here needs to be consistent with
    ! sm_levels from jules_control_init
    dzsoil_io(1:sm_levels)    = (/ 0.1_r_um, 0.25_r_um, 0.65_r_um, 2.0_r_um /)
    l_dpsids_dsdz   = dpsids_dsdz
    l_soil_sat_down = soil_sat_down
    l_vg_soil       = .true.
    soilhc_method   = 2

    ! Check the contents of the Jules soil parameters module
    ! This module sets some derived parameters
    call check_jules_soil(sm_levels)

    ! ----------------------------------------------------------------
    ! Jules Biogeochemisty settings - contained in module jules_soil_biogeochem
    ! ----------------------------------------------------------------
    const_ch4_cs = 5.41e-12_r_um

    ! Check the contents of the Jules biogeochemistry parameters module
    call check_jules_soil_biogeochem()

    ! ----------------------------------------------------------------
    ! Jules surface settings - contained in module jules_surface
    ! ----------------------------------------------------------------
    beta_cnv_bl     = 0.04_r_um
    select case (cor_mo_iter_in)
      case(cor_mo_iter_lim_oblen)
        cor_mo_iter = Limit_ObukhovL
      case(cor_mo_iter_improved)
        cor_mo_iter = Improve_Initial_Guess
    end select
    fd_hill_option  = capped_lowhill
    select case (fd_stability_dep)
      case(fd_stability_dep_none)
        fd_stab_dep = 0
      case(fd_stability_dep_surf_ri)
        fd_stab_dep = 1
    end select
    select case (formdrag_in)
      case(formdrag_none)
        formdrag = no_drag
      case(formdrag_eff_z0)
        formdrag = effective_z0
      case(formdrag_dist_drag)
        formdrag = explicit_stress
    end select
    i_modiscopt     = 1
    iscrntdiag      = ip_scrndecpl2
    if (srf_ex_cnv_gust) isrfexcnvgust = IP_SrfExWithCnv
    l_epot_corr     = .true.
    l_land_ice_imp  = .true.
    orog_drag_param = 0.15_r_um

    ! Check the contents of the Jules surface parameters module
    call check_jules_surface()

    ! ----------------------------------------------------------------
    ! Jules vegatation settings - contained in module jules_vegetation
    ! ----------------------------------------------------------------
    ilayers         = 10
    l_vegcan_soilfx = .true.
    photo_model     = photo_collatz

    ! Check the contents of the vegetation parameters module
    call check_jules_vegetation()

    ! ----------------------------------------------------------------
    ! Temporary logicals used to fix bugs in Jules
    !  - contained in jules_science_fixes
    ! ----------------------------------------------------------------
    l_dtcanfix = .true.
    ! Not strictly GA7 but sensible to set them
    l_fix_alb_ice_thick = .true.
    l_fix_albsnow_ts    = .true.
    l_fix_ctile_orog    = .true.
    l_fix_wind_snow     = .true.

    ! The following routine initialises 3D arrays which are used direct
    ! from modules throughout the Jules code base.
    ! We must initialise them here so that they are always available
    ! But they must be set to appropriate values for the current column
    ! in any kernel whos external code uses the variables
    ! Ideally the Jules code will be changed so that they are passed in
    ! through the argument list
    ! It also must be called after the above Jules namelists are set
    ! as some arrays are conditional upon the switches, but it also
    ! needs calling before the below parameters are set, because
    ! their arrays are allocated in here.

    call allocate_jules_arrays()

    ! ----------------------------------------------------------------
    ! Jules non-vegetated tile settings - contained in module nvegparm
    ! ----------------------------------------------------------------
    albsnc_nvg = real(alb_snocov_nvg, r_um)
    albsnf_nvg = real(alb_snofree_nvg, r_um)
    albsnf_nvgl=(/ 0.05,0.06,0.03,0.75 /)
    albsnf_nvgu=(/ 0.20,0.15,0.80,0.75 /)
    catch_nvg = real(can_cap_nvg, r_um)
    ch_nvg = real(heat_cap_nvg, r_um)
    emis_nvg=(/ 0.970,0.985,0.900,0.990 /)
    gs_nvg=(/ 0.00,0.00,1.00e-2,1.00e+6 /)
    infil_nvg=(/ 0.1,0.0,0.5,0.0 /)
    vf_nvg=(/ 1.0,1.0,0.0,0.0 /)
    z0_nvg=(/ 1.00,1.00e-4,1.00e-3,5.00e-4 /)

    ! ----------------------------------------------------------------
    ! Jules vegetation tile settigs - contained in module pftparm
    ! ----------------------------------------------------------------
    a_wl=(/ 0.65,0.65,0.005,0.005,0.10 /)
    a_ws=(/ 10.00,10.00,1.00,1.00,10.00 /)
    albsnc_max = real(alb_snocov_max, r_um)
    albsnc_min=(/ 3.00000e-1,3.00000e-1,8.00000e-1,8.00000e-1,8.00000e-1 /)
    albsnf_maxl=(/ 0.095,0.059,0.128,0.106,0.077 /)
    albsnf_maxu=(/ 0.215,0.132,0.288,0.239,0.173 /)
    alnir = real(alb_leaf_nir, r_um)
    alnirl=(/ 0.30,0.23,0.39,0.39,0.39 /)
    alniru=(/ 0.75,0.65,0.95,0.95,0.87 /)
    alpar = real(alb_leaf_vis, r_um)
    alparl=(/ 0.06,0.04,0.06,0.06,0.06 /)
    alparu=(/ 0.15,0.11,0.25,0.25,0.25 /)
    alpha=(/ 0.08,0.08,0.08,0.040,0.08 /)
    b_wl=(/ 1.667,1.667,1.667,1.667,1.667 /)
    c3=(/ 1,1,1,0,1 /)
    can_struct_a=(/ 1.0,1.0,1.0,1.0,1.0 /)
    catch0=(/ 0.5,0.5,0.5,0.5,0.5 /)
    dcatch_dlai=(/ 0.05,0.05,0.05,0.05,0.05 /)
    dgl_dm=(/ 0.0,0.0,0.0,0.0,0.0 /)
    dgl_dt=(/ 9.0,9.0,0.0,0.0,9.0 /)
    dqcrit=(/ 0.090,0.060,0.100,0.075,0.100 /)
    dz0v_dh=(/ 5.00e-2,5.00e-2,1.00e-1,1.00e-1,1.00e-1 /)
    emis_pft=(/ 0.98,0.99,0.98,0.98,0.98 /)
    eta_sl=(/ 0.01,0.01,0.01,0.01,0.01 /)
    f0=(/ 0.875,0.875,0.900,0.800,0.900 /)
    fd=(/ 0.015,0.015,0.015,0.025,0.015 /)
    fsmc_of=(/ 0.0,0.0,0.0,0.0,0.0 /)
    fsmc_p0=(/ 0.0,0.0,0.0,0.0,0.0 /)
    g_leaf_0=(/ 0.25,0.25,0.25,0.25,0.25 /)
    glmin=(/ 1.0e-6,1.0e-6,1.0e-6,1.0e-6,1.0e-6 /)
    hw_sw=(/ 0.5,0.5,0.5,0.5,0.5 /)
    infil_f=(/ 4.0,4.0,2.0,2.0,2.0 /)
    kext = real(light_extinct, r_um)
    kn=(/ 0.78,0.78,0.78,0.78,0.78 /)
    knl=(/ 0.20,0.20,0.20,0.20,0.20 /)
    kpar=(/ 0.5,0.5,0.5,0.5,0.5 /)
    lai_alb_lim=(/ 0.005,0.005,0.005,0.005,0.005 /)
    lma=(/ 0.0824,0.2263,0.0498,0.1370,0.0695 /)
    neff=(/ 0.8e-3,0.8e-3,0.8e-3,0.4e-3,0.8e-3 /)
    nl0=(/ 0.040,0.030,0.060,0.030,0.030 /)
    nmass=(/ 0.0210,0.0115,0.0219,0.0131,0.0219 /)
    nr=(/ 0.01726,0.00784,0.0162,0.0084,0.01726 /)
    nr_nl=(/ 1.0,1.0,1.0,1.0,1.0 /)
    ns_nl=(/ 0.10,0.10,1.00,1.00,0.10 /)
    nsw=(/ 0.0072,0.0083,0.01604,0.0202,0.0072 /)
    omega = real(scat_coef_vis, r_um)
    omegal=(/ 0.10,0.10,0.10,0.12,0.10 /)
    omegau=(/ 0.23,0.23,0.35,0.35,0.35 /)
    omnir = real(scat_coef_nir, r_um)
    omnirl=(/ 0.50,0.30,0.53,0.53,0.53 /)
    omniru=(/ 0.90,0.65,0.98,0.98,0.98 /)
    orient=(/ 0,0,0,0,0 /)
    q10_leaf=(/ 2.0,2.0,2.0,2.0,2.0 /)
    r_grow=(/ 0.25,0.25,0.25,0.25,0.25 /)
    rootd_ft=(/ 3.0,1.0,0.5,0.5,0.5 /)
    sigl=(/ 0.0375,0.1000,0.0250,0.0500,0.0500 /)
    tleaf_of=(/ 273.15,243.15,258.15,258.15,243.15 /)
    tlow=(/ 0.0,-5.0,0.0,13.0,0.0 /)
    tupp=(/ 36.0,31.0,36.0,45.0,36.0 /)
    vint=(/ 5.73,6.32,6.42,0.00,14.71 /)
    vsl=(/ 29.81,18.15,40.96,10.24,23.15 /)

    ! ----------------------------------------------------------------
    ! Settings which are specified on all surface tiles at once
    ! - contained in module c_z0h_z0m
    ! ----------------------------------------------------------------
    z0h_z0m(1:npft) = real(z0hm_ratio_pft, r_um)
    z0h_z0m(npft+1:npft+nnvg) = (/ 1.0e-7,2.5e-1,2.0e-2,2.0e-1 /)

    ! ----------------------------------------------------------------
    ! Jules surface settings - contained in module jules_surface
    ! ----------------------------------------------------------------
    ! The following depends on a previously set variable from jules_vegetation
    ! and the array is allocated in allocate_jules_arrays
    if (can_rad_mod == 6) then
      diff_frac = 0.4_r_um
    else
      diff_frac = 0.0_r_um
    end if

  end subroutine jules_physics_init

end module jules_physics_init_mod
