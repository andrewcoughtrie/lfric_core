!----------------------------------------------------------------------------
! (c) Crown copyright 2020 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!----------------------------------------------------------------------------
!> @brief Controls the setting of variables for UM physics schemes, which
!>         are either fixed in LFRic or derived from LFRic inputs

module um_physics_init_mod

  ! LFRic namelists which have been read
  use aerosol_config_mod,        only : c_aerosol,                        &
                                        c_aerosol_glomap_mode_climatology

  use blayer_config_mod,         only : flux_bc_opt_in => flux_bc_opt

  use cloud_config_mod,          only : scheme,       &
                                        scheme_smith, &
                                        scheme_pc2,   &
                                        rh_crit

  use convection_config_mod,     only : cv_scheme,                    &
                                        cv_scheme_gregory_rowntree,   &
                                        cv_scheme_lambert_lewis,      &
                                        number_of_convection_substeps

  use extrusion_config_mod,      only : domain_top, number_of_layers

  use formulation_config_mod,    only : use_moisture

  use mixing_config_mod,         only : smagorinsky,                &
                                        mixing_method => method,    &
                                        method_3d_smag,             &
                                        method_2d_smag,             &
                                        method_blending,            &
                                        mix_factor_in => mix_factor

  use section_choice_config_mod, only : aerosol,           &
                                        aerosol_um,        &
                                        boundary_layer,    &
                                        boundary_layer_um, &
                                        convection,        &
                                        convection_um,     &
                                        cloud,             &
                                        cloud_um,          &
                                        microphysics,      &
                                        microphysics_um,   &
                                        spectral_gwd,      &
                                        spectral_gwd_um,   &
                                        surface,           &
                                        surface_jules

  use spectral_gwd_config_mod,   only :                                       &
                                 ussp_launch_factor_in => ussp_launch_factor, &
                                 wavelstar_in => wavelstar,                   &
                                 add_cgw_in => add_cgw,                       &
                                 cgw_scale_factor_in => cgw_scale_factor

  ! Other LFRic modules used
  use constants_mod,        only : r_um, rmdi
  use log_mod,              only : log_event,         &
                                   log_scratch_space, &
                                   LOG_LEVEL_ERROR

  ! UM modules used
  use cderived_mod,         only : delta_lambda, delta_phi
  use nlsizes_namelist_mod, only : bl_levels, row_length, rows

  implicit none

  private
  public :: um_physics_init

contains

  !>@brief Initialise UM physics variables which are either fixed in LFRic
  !>        or derived from LFRic inputs
  !>@details This file sets many parameters and switches which are currently
  !>          in the UM namelists. Many of these will never be promoted to the
  !>          LFRic namelist as they are legacy options not fit for future use.
  !>          Hence we set them here until such time as we can retire them
  !>          from the UM code.
  !>        Other parameters and switches which are genuinely input variables,
  !>         via the LFRic namelists, are also set here for the UM code.
  !>       Where possible, all values are taken from GA7 science settings.
  !>        Some are incorrect and that will be addressed in #2100
  subroutine um_physics_init()

    ! UM modules containing things that need setting
    use bl_option_mod, only: i_bl_vn, sbl_op, ritrans,                     &
         cbl_op, lambda_min_nml, local_fa, keep_ri_fa,                     &
         sg_orog_mixing, fric_heating, idyndiag,                           &
         zhloc_depth_fac, flux_grad, entr_smooth_dec,                      &
         relax_sc_over_cu, bl_res_inv, blending_option,                    &
         a_ent_shr_nml, alpha_cd, puns, pstb, nl_bl_levels, kprof_cu,      &
         non_local_bl, flux_bc_opt, i_bl_vn_9c, sharp_sea_mes_land,        &
         lem_conven, to_sharp_across_1km, off, on, DynDiag_Ribased,        &
         blend_allpoints, ng_stress
    use cloud_inputs_mod, only: i_cld_vn, forced_cu, i_rhcpt, i_cld_area,  &
         rhcrit, ice_fraction_method,falliceshear_method, cff_spread_rate, &
         l_subgrid_qv, ice_width, smith_orig, i_eacf,                      &
         i_pc2_checks_cld_frac_method, l_ensure_min_in_cloud_qcf,          &
         l_simplify_pc2_init_logic, dbsdtbs_turb_0,                        &
         i_pc2_erosion_method, check_run_cloud, forced_cu_fac,             &
         i_pc2_conv_coupling, allicetdegc, starticetkelvin
    use cv_run_mod, only: icvdiag, cvdiag_inv, cvdiag_sh_wtest,            &
         limit_pert_opt, tv1_sd_opt, iconv_congestus, iconv_deep,          &
         ent_fac_dp, cldbase_opt_dp, cldbase_opt_sh, w_cape_limit,         &
         l_param_conv, i_convection_vn, l_ccrad, l_mom, adapt, amdet_fac,  &
         bl_cnv_mix, anvil_factor, ccw_for_precip_opt, cldbase_opt_md,     &
         cnv_wat_load_opt,dd_opt, deep_cmt_opt, eff_dcff, eff_dcfl,        &
         ent_dp_power, ent_fac_md, ent_opt_dp, ent_opt_md, fac_qsat,       &
         ent_fac_md, iconv_deep, iconv_mid, iconv_shallow,l_3d_cca,        &
         l_anvil, l_cmt_heating, l_cv_conserve_check, l_safe_conv,         &
         mdet_opt_dp, mid_cnv_pmin, mparwtr, qlmin, n_conv_calls,          &
         qstice, r_det, sh_pert_opt,t_melt_snow, termconv, tice,           &
         tower_factor,ud_factor, fdet_opt, anv_opt, cape_timescale,        &
         cca2d_dp_opt,cca2d_md_opt,cca2d_sh_opt,                           &
         cca_dp_knob,cca_md_knob,cca_sh_knob,                              &
         ccw_dp_knob,ccw_md_knob,ccw_sh_knob,                              &
         cnv_cold_pools,dil_plume_water_load,l_cloud_deep, mid_cmt_opt,    &
         plume_water_load, rad_cloud_decay_opt, cape_bottom, cape_top,     &
         cape_min, i_convection_vn_6a, i_cv_llcs, midtrig_opt,             &
         llcs_cloud_precip, llcs_opt_all_rain, llcs_rhcrit, llcs_timescale,&
         check_run_convection, llcs_opt_crit_condens, llcs_detrain_coef
    use cv_param_mod, only: mtrig_ntmlplus2
    use cv_stash_flg_mod, only: set_convection_output_flags
    use cv_set_dependent_switches_mod, only: cv_set_dependent_switches
    use dust_parameters_mod, only: i_dust, i_dust_off,                     &
         dust_parameters_load
    use fsd_parameters_mod, only: fsd_eff_lam, fsd_eff_phi
    use glomap_clim_option_mod, only: i_glomap_clim_setup,                 &
         i_gc_sussocbc_5mode, l_glomap_clim_aie2
    use g_wave_input_mod, only: ussp_launch_factor, wavelstar, l_add_cgw,  &
         cgw_scale_factor
    use mphys_bypass_mod, only: mphys_mod_top
    use mphys_inputs_mod, only: ai, ar, bi, c_r_correl, ci_input, cic_input, &
        di_input, dic_input, i_mcr_iter, l_diff_icevt,                       &
        l_mcr_qrain, l_psd, l_rain, l_warm_new, timestep_mp_in, x1r, x2r,    &
        l_mcr_qcf2, sediment_loc, i_mcr_iter_tstep, all_sed_start,           &
        check_run_precip, graupel_option, no_graupel
    use pc2_constants_mod, only: i_cld_off, i_cld_smith, i_cld_pc2,        &
         rhcpt_off, acf_off, real_shear,                                   &
         pc2eros_exp_rh,pc2eros_hybrid_allfaces,pc2eros_hybrid_sidesonly
    use rad_input_mod, only: two_d_fsd_factor
    use science_fixes_mod, only: l_fix_mphys_diags_iter
    use tuning_segments_mod, only: bl_segment_size, precip_segment_size, &
         ussp_seg_size
    use turb_diff_ctl_mod, only: visc_m, visc_h, max_diff, delta_smag,   &
         rneutml_sq
    use turb_diff_mod, only: l_subfilter_horiz, l_subfilter_vert,        &
         mix_factor, turb_startlev_vert, turb_endlev_vert
    use ukca_mode_setup, only: ukca_mode_sussbcoc_5mode

    implicit none

    ! ----------------------------------------------------------------
    ! UKCA aerosol scheme settings - contained in glomap_clim_option_mod
    ! ----------------------------------------------------------------
    if ( aerosol == aerosol_um ) then

      ! Options which are bespoke to the aerosol scheme chosen
      select case (c_aerosol)

      case(c_aerosol_glomap_mode_climatology)
        ! Only the 2nd indirect effect is currently implemented in LFRic
        l_glomap_clim_aie2 = .true.
        ! Set up the correct mode and components for GLOMAP-mode:
        ! 5 mode with SU SS OC BC components
        i_glomap_clim_setup = i_gc_sussocbc_5mode
        call ukca_mode_sussbcoc_5mode()

      case default
        write( log_scratch_space, '(A,I0)' )                                   &
             'Invalid aerosol option, stopping', c_aerosol
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )

      end select
    end if

    ! ----------------------------------------------------------------
    ! UM boundary layer scheme settings - contained in UM module bl_option_mod
    ! ----------------------------------------------------------------
    if ( boundary_layer == boundary_layer_um ) then

      if ( surface /= surface_jules ) then
        write( log_scratch_space, '(A)' )                                   &
            'Jules surface is required for UM boundary layer - please switch on'
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      a_ent_shr_nml    = 5.0_r_um
      allocate(alpha_cd(bl_levels))
      alpha_cd         = 1.5_r_um
      alpha_cd(1)      = 2.0_r_um
      ! Not GA7 - should be on - needs testing
      bl_res_inv       = off
      cbl_op           = lem_conven
      ! Not GA7 - should be on - needs testing
      entr_smooth_dec  = off
      flux_bc_opt      = flux_bc_opt_in
      flux_grad        = off
      fric_heating     = on
      i_bl_vn          = i_bl_vn_9c
      idyndiag         = DynDiag_Ribased
      keep_ri_fa       = on
      ! Not GA7 - should be buoy_integ - needs testing
      kprof_cu         = off
      ! l_bl_mix_qcf = .true should be set here, but code doesn't exist
      ! l_reset_dec_thres=.true. should be set here - needs testing
      lambda_min_nml   = 40.0_r_um
      local_fa         = to_sharp_across_1km
      pstb             = 2.0_r_um
      puns             = 0.5_r_um
      relax_sc_over_cu = off
      ritrans          = 0.1_r_um
      sbl_op           = sharp_sea_mes_land
      sg_orog_mixing   = off
      ! Not GA7 - should be 6km
      nl_bl_levels     = bl_levels
      ! Not GA7 - should be 0.4 - needs testing
      zhloc_depth_fac  = 0.3_r_um

    end if

    ! ----------------------------------------------------------------
    ! UM convection scheme settings - contained in UM module cv_run_mod
    ! ----------------------------------------------------------------

    ! The following are needed by conv_diag regardless of whether
    ! convection is actually called or not
    ! GA7 but possibly these should vary with vertical level set??
    cape_bottom          = 5
    cape_top             = 50
    cldbase_opt_dp       = 8
    cldbase_opt_sh       = 0
    cvdiag_inv           = 0
    cvdiag_sh_wtest      = 0.02_r_um
    dil_plume_water_load = 0
    ent_fac_dp           = 1.13_r_um
    iconv_congestus      = 0
    iconv_deep           = 1
    icvdiag              = 1
    limit_pert_opt       = 2
    plume_water_load     = 0
    tv1_sd_opt           = 2
    w_cape_limit         = 0.4_r_um

    if ( convection == convection_um ) then

      ! Options needed by all convection schemes
      l_param_conv = .true.
      fac_qsat     = 0.500_r_um
      mparwtr      = 1.5000e-3_r_um
      qlmin        = 3.0000e-4_r_um

      ! Options which are bespoke to the choice of scheme
      select case (cv_scheme)

      case(cv_scheme_gregory_rowntree)

      if ( boundary_layer /= boundary_layer_um ) then
        write( log_scratch_space, '(A)' )                                   &
            'UM boundary layer is required for GR convection - please switch on'
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

        i_convection_vn     = i_convection_vn_6a
        adapt               = 7
        amdet_fac           = 3.0_r_um
        anv_opt             = 0
        anvil_factor        = 1.0000_r_um
        bl_cnv_mix          = 1
        cape_min            = 0.5_r_um
        cape_timescale      = 3600
        cca2d_dp_opt        = 1
        cca2d_md_opt        = 1
        cca2d_sh_opt        = 2
        cca_dp_knob         = 0.10_r_um
        cca_md_knob         = 0.10_r_um
        cca_sh_knob         = 0.20_r_um
        ccw_dp_knob         = 1.00_r_um
        ccw_for_precip_opt  = 4
        ccw_md_knob         = 1.00_r_um
        ccw_sh_knob         = 1.00_r_um
        cldbase_opt_md      = 7
        cnv_cold_pools      = 0
        cnv_wat_load_opt    = 0
        dd_opt              = 1
        deep_cmt_opt        = 5
        eff_dcff            = 1.0_r_um
        eff_dcfl            = 1.0_r_um
        ent_dp_power        = 1.00_r_um
        ent_fac_md          = 0.90_r_um
        ent_opt_dp          = 3
        ent_opt_md          = 0
        fdet_opt            = 0
        iconv_mid           = 1
        iconv_shallow       = 1
        l_cloud_deep        = .true.
        l_3d_cca            = .true.
        l_anvil             = .true.
        l_ccrad             = .true.
        l_cmt_heating       = .true.
        l_cv_conserve_check = .true.
        l_mom               = .true.
        l_safe_conv         = .true.
        mdet_opt_dp         = 1
        mid_cmt_opt         = 0
        mid_cnv_pmin        = 10000.00_r_um
        midtrig_opt         = mtrig_ntmlplus2
        n_conv_calls        = number_of_convection_substeps
        qstice              = 3.5000e-3_r_um
        r_det               = 0.8000_r_um
        rad_cloud_decay_opt = 0
        sh_pert_opt         = 1
        t_melt_snow         = 274.15_r_um
        termconv            = 1
        tice                = 263.1500_r_um
        tower_factor        = 1.0000_r_um
        ud_factor           = 1.0000_r_um

        ! Deried switches and parameters are set here based on the options
        ! above
        call cv_set_dependent_switches( )
        ! Flags for diagnostic output are set here
        call set_convection_output_flags( )

      case(cv_scheme_lambert_lewis)
        i_convection_vn   = i_cv_llcs
        non_local_bl      = off
        if ( scheme == scheme_pc2 ) then
          ! If pc2, we detrain some cloud
          llcs_cloud_precip = llcs_opt_crit_condens
          llcs_detrain_coef = 0.6_r_um
        else
          ! We just rain everything out
          llcs_cloud_precip = llcs_opt_all_rain
        end if
        llcs_rhcrit       = 0.8_r_um
        llcs_timescale    = 3600.0_r_um

      case default
        write( log_scratch_space, '(A,I5)' )  &
             'Invalid convection scheme option, stopping', cv_scheme
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )

      end select
    else ! convection /= convection_um
      ! Need to set the version of the convection diagnosis that we want to use
      i_convection_vn = i_convection_vn_6a
    end if

    ! Check the contents of the convection parameters module
    call check_run_convection()

    ! ----------------------------------------------------------------
    ! UM cloud scheme settings - contained in UM module cloud_inputs_mod
    ! ----------------------------------------------------------------

    ! Options needed regardless of whether the cloud scheme is called
    i_cld_area = acf_off
    ! Not GA7 - should be 2 - needs testing
    forced_cu = off

    if ( cloud == cloud_um ) then

      if ( .not. use_moisture ) then
        write( log_scratch_space, '(A)' )                                   &
            'use_moisture is required for UM cloud - please switch on'
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      ! Options needed by all cloud schemes
      ! Not GA7 - should be 1e-5 - needs testing
      cff_spread_rate            = 1.0e-3_r_um
      falliceshear_method        = real_shear
      i_eacf                     = off
      ! Not GA7 - should be 2 - needs coding
      i_rhcpt                    = rhcpt_off
      ice_fraction_method        = smith_orig
      ice_width                  = 0.02_r_um
      ! l_add_cca_to_mcica=.true. needs adding - not currently coded
      ! l_od_cld_filter=.true. should be here - not currently coded
      ! ...tau_thresh=0.01 should be set here if so
      l_subgrid_qv               = .true.
      rhcrit(1:number_of_layers) = real(rh_crit, r_um)

      ! Options which are bespoke to the choice of scheme
      select case (scheme)

      case(scheme_smith)
        i_cld_vn = i_cld_smith

      case(scheme_pc2)
        i_cld_vn = i_cld_pc2
        allicetdegc                  = -20.0_r_um
        dbsdtbs_turb_0               = 1.50e-4_r_um
        forced_cu_fac                = 0.5_r_um
        i_pc2_checks_cld_frac_method = 2
        i_pc2_conv_coupling          = 3
        i_pc2_erosion_method         = pc2eros_hybrid_sidesonly
        l_ensure_min_in_cloud_qcf    = .true.
        ! Not GA7, should be false
        l_simplify_pc2_init_logic    = .true.
        starticetkelvin              = 263.15_r_um

      case default
        write( log_scratch_space, '(A,I3)' )  &
             'Invalid cloud scheme option, stopping', scheme
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )

      end select

      ! Check the contents of the cloud parameters module
      call check_run_cloud()

    else ! cloud /= cloud_um
      ! Set switch for no cloud scheme in UM
      i_cld_vn = i_cld_off
    end if

    ! ----------------------------------------------------------------
    ! Classic dust scheme - contained in dust_parameters_mod
    ! ----------------------------------------------------------------
    ! This is not used in LFRic but potentially called from UM code.
    !  Hence its inputs and options need setting according to the
    ! scheme being off
    i_dust = i_dust_off
    call dust_parameters_load()

    ! ----------------------------------------------------------------
    ! UM microphysics settings - contained in UM module mphys_inputs_mod
    ! ----------------------------------------------------------------
    if ( microphysics == microphysics_um ) then

      if ( cloud /= cloud_um ) then
        write( log_scratch_space, '(A)' )                                   &
            'UM cloud is required for UM microphysics - please switch on'
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      ai             = 2.5700e-2_r_um
      ar             = 1.00_r_um
      bi             = 2.00_r_um
      c_r_correl     = 0.9_r_um
      ci_input       = 14.3_r_um
      cic_input      = 1024.0_r_um
      di_input       = 0.416_r_um
      dic_input      = 1.0_r_um
      graupel_option = no_graupel
      i_mcr_iter     = i_mcr_iter_tstep
      l_diff_icevt   = .true.
      ! l_fsd_generator should be set here - code doesn't yet exist
      l_mcr_qrain    = .true.
      l_psd          = .true.
      l_rain         = .true.
      ! l_subgrid_qcl_mp should be set here - needs coding??
      l_warm_new     = .true.
      sediment_loc   = all_sed_start
      timestep_mp_in = 120
      x1r            = 2.2000e-1_r_um
      x2r            = 2.2000_r_um

      ! Domain top used in microphysics - contained in mphys_bypass_mod
      mphys_mod_top  = real(domain_top, r_um)

      ! Options for the subgrid cloud variability parametrization used
      ! in microphysics but living elsewhere in the UM
      ! ... contained in rad_input_mod
      ! Not GA7 - should be 1.5
      two_d_fsd_factor = 1.414_r_um
      ! ... contained in fsd_parameters_mod
      fsd_eff_lam      = delta_lambda
      fsd_eff_phi      = delta_phi

    end if

    ! Check the contents of the microphysics parameters module
    call check_run_precip()

    ! ----------------------------------------------------------------
    ! UM spectral gravity wave drag options - contained in g_wave_input_mod
    ! ----------------------------------------------------------------
    if ( spectral_gwd == spectral_gwd_um ) then

      ! Namelist value 1.2 is not GA7 - should be 1.3
      cgw_scale_factor = real(cgw_scale_factor_in, r_um)
      l_add_cgw = add_cgw_in
      ussp_launch_factor = real(ussp_launch_factor_in, r_um)
      wavelstar = real(wavelstar_in, r_um)

    end if

    ! ----------------------------------------------------------------
    ! Temporary logicals used to fix bugs in the UM - contained in science_fixes
    ! ----------------------------------------------------------------
    ! l_fix_drop_settle=.true. should be set here - needs testing
    l_fix_mphys_diags_iter = .true.
    ! l_pc2_homog_turb_q_neg=.true. should be set here - needs testing

    ! ----------------------------------------------------------------
    ! Segment sizes for UM physics - contained in tuning_segments_mod
    ! ----------------------------------------------------------------
    ! These are set to 1 currently because only 1 grid-cell is passed to
    ! a kernel. However, multiple columns are passed to a kernel,
    ! these values will need to be set depending on how many columns
    ! a kernel is passed.
    bl_segment_size     = 1
    precip_segment_size = 1
    ussp_seg_size       = 1

    !-----------------------------------------------------------------------
    ! Smagorinsky mixing options - contained in turb_diff_mod and
    !                              turb_diff_ctl_mod
    !-----------------------------------------------------------------------
    if ( smagorinsky ) then

      ! The following 3D arrays are used direct from turb_diff_ctl_mod
      ! in the UM code.
      ! We must initialise them here so that they are available.
      ! But they must be set to appropriate values for the current column
      ! in any kernel whos external code uses the variables.
      ! Ideally the UM code will be changed so that they are passed in
      ! through the argument list.
      allocate ( visc_h(row_length, rows, number_of_layers), source=rmdi )
      allocate ( visc_m(row_length, rows, number_of_layers), source=rmdi )
      allocate ( rneutml_sq(row_length, rows, number_of_layers), source=rmdi )
      allocate ( max_diff  (row_length, rows), source=rmdi )
      allocate ( delta_smag(row_length, rows), source=rmdi )

      ! The following are needed regardless of which mixing option is used
      mix_factor = real(mix_factor_in, r_um)
      turb_startlev_vert  = 1
      turb_endlev_vert    = bl_levels

      ! Options which are bespoke to the choice of scheme
      select case ( mixing_method )

      case( method_3d_smag )
        l_subfilter_horiz = .true.
        l_subfilter_vert  = .true.
        blending_option   = off
        non_local_bl      = off
        ng_stress         = off
      case( method_2d_smag )
        l_subfilter_horiz = .true.
        l_subfilter_vert  = .false.
        blending_option   = off
      case( method_blending )
        l_subfilter_horiz = .true.
        l_subfilter_vert  = .true.
        blending_option   = blend_allpoints
      end select

    else ! not Smagorinsky

      ! Allocate these to small size to avoid compiler issues
      allocate ( visc_h(1,1,1), source=rmdi  )
      allocate ( visc_m(1,1,1), source=rmdi  )
      allocate ( rneutml_sq(1,1,1), source=rmdi  )
      allocate ( max_diff(1,1), source=rmdi  )
      allocate ( delta_smag(1,1), source=rmdi  )

      ! Switches for Smagorinsky being off
      blending_option   = off
      l_subfilter_horiz = .false.
      l_subfilter_vert  = .false.

    end if

  end subroutine um_physics_init

end module um_physics_init_mod
