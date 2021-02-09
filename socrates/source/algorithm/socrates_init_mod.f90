!------------------------------------------------------------------------------
! (c) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!------------------------------------------------------------------------------
! @brief Initialisation for the Socrates radiation code

module socrates_init_mod

use constants_mod, only: r_def, i_def

implicit none

integer(i_def) :: n_sw_band, n_lw_band
integer(i_def), allocatable :: sw_n_band_exclude(:), sw_index_exclude(:, :)
integer(i_def), allocatable :: lw_n_band_exclude(:), lw_index_exclude(:, :)
real(r_def), allocatable :: sw_wavelength_short(:), sw_wavelength_long(:)
real(r_def), allocatable :: lw_wavelength_short(:), lw_wavelength_long(:)
real(r_def), allocatable :: sw_weight_blue(:)

private
public :: socrates_init, &
  n_sw_band, sw_n_band_exclude, sw_index_exclude, &
  sw_wavelength_short, sw_wavelength_long, sw_weight_blue, &
  n_lw_band, lw_n_band_exclude, lw_index_exclude, &
  lw_wavelength_short, lw_wavelength_long

contains

subroutine socrates_init()

  use radiation_config_mod,  only:                                     &
    spectral_file_sw, spectral_file_lw, mcica_data_file,               &
    l_h2o_sw, l_co2_sw, l_o3_sw, l_n2o_sw, l_ch4_sw, l_o2_sw,          &
    l_h2o_lw, l_co2_lw, l_o3_lw, l_n2o_lw, l_ch4_lw,                   &
    cloud_representation, cloud_representation_no_cloud,               &
    cloud_inhomogeneity, cloud_inhomogeneity_mcica
  use rad_ccf, only: set_socrates_constants
  use socrates_set_spectrum, only: set_spectrum, get_spectrum, set_mcica
  use log_mod, only: log_event, log_scratch_space, LOG_LEVEL_INFO

  implicit none

  integer(i_def) :: i_band


  ! Set constants in the socrates modules
  call set_socrates_constants()

  call set_spectrum(                  &
    spectrum_name = 'sw',             &
    spectral_file = spectral_file_sw, &
    l_h2o         = l_h2o_sw,         &
    l_co2         = l_co2_sw,         &
    l_o3          = l_o3_sw,          &
    l_n2o         = l_n2o_sw,         &
    l_ch4         = l_ch4_sw,         &
    l_o2          = l_o2_sw )

  call get_spectrum(                        &
    spectrum_name    = 'sw',                &
    n_band           = n_sw_band,           &
    n_band_exclude   = sw_n_band_exclude,   &
    index_exclude    = sw_index_exclude,    &
    wavelength_short = sw_wavelength_short, &
    wavelength_long  = sw_wavelength_long,  &
    weight_blue      = sw_weight_blue )

  call log_event( 'SW bands:', LOG_LEVEL_INFO )
  do i_band=1, n_sw_band
    write( log_scratch_space, '(I3,2E16.8)' ) &
      i_band, sw_wavelength_short(i_band), sw_wavelength_long(i_band)
    call log_event( log_scratch_space, LOG_LEVEL_INFO )
  end do

  call set_spectrum(                  &
    spectrum_name = 'lw',             &
    spectral_file = spectral_file_lw, &
    l_h2o         = l_h2o_lw,         &
    l_co2         = l_co2_lw,         &
    l_o3          = l_o3_lw,          &
    l_n2o         = l_n2o_lw,         &
    l_ch4         = l_ch4_lw )

  call get_spectrum(                        &
    spectrum_name    = 'lw',                &
    n_band           = n_lw_band,           &
    n_band_exclude   = lw_n_band_exclude,   &
    index_exclude    = lw_index_exclude,    &
    wavelength_short = lw_wavelength_short, &
    wavelength_long  = lw_wavelength_long )

  call log_event( 'LW bands:', LOG_LEVEL_INFO )
  do i_band=1, n_lw_band
    write( log_scratch_space, '(I3,2E16.8)' ) &
      i_band, lw_wavelength_short(i_band), lw_wavelength_long(i_band)
    call log_event( log_scratch_space, LOG_LEVEL_INFO )
  end do

  if ( (cloud_representation /= cloud_representation_no_cloud) .and. &
       (cloud_inhomogeneity == cloud_inhomogeneity_mcica) ) then
    call set_mcica(mcica_data_file, 'sw', 'lw')
    call log_event( 'Read MCICA data file.', LOG_LEVEL_INFO )
  end if

end subroutine socrates_init
end module socrates_init_mod
