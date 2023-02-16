!-----------------------------------------------------------------------------
! (C) Crown copyright 2022 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @page Miniapp da_dev program

!> @brief Main program for running da_dev independently with jedi objects.

!> @details Setup and run a pseudo model forecast using the mock jedi objects
!>          The jedi objects are constructed via an initialiser call and the
!>          forecast is handled by the model object.

program jedi_forecast_pseudo

  !
  use constants_mod,          only : i_def
  !! temp data types and methods to get/store configurations for geometry, state and model
  use jedi_state_config_mod, only : jedi_state_config_type
  use jedi_pseudo_model_config_mod, only : jedi_pseudo_model_config_type
  use cli_mod,               only: get_initial_filename

  ! Mock jedi objects
  use jedi_run_mod,          only : jedi_run_type
  use jedi_geometry_mod,     only : jedi_geometry_type
  use jedi_state_mod,        only : jedi_state_type
  use jedi_pseudo_model_mod, only : jedi_pseudo_model_type

  !
  use da_dev_driver_mod,     only : finalise_model

  implicit none

  ! jedi objects
  type(jedi_geometry_type)     :: jedi_geometry
  type(jedi_state_type)        :: jedi_state
  type(jedi_pseudo_model_type) :: jedi_model
  type(jedi_run_type)          :: jedi_run

  ! faux configs
  type(jedi_state_config_type)        :: jedi_state_config
  type(jedi_pseudo_model_config_type) :: jedi_pseudo_model_config
  integer( kind=i_def )               :: date_time_duration
  character(:), allocatable           :: filename

  ! config for the mock jedi objects
  ! Infrastructure config
  call get_initial_filename( filename )
  ! State config
  call jedi_state_config%initialise(use_full_model=.false.)
  ! Model Config
  call jedi_pseudo_model_config%initialise()
  ! Forecast config
  date_time_duration = 5

  ! Run object
  ! handles initialization and finalization of required infrastructure
  call jedi_run%initialise( filename )

  ! Geometry
  ! possibly pass in a comm?
  call jedi_geometry%initialise()

  ! State
  call jedi_state%initialise( jedi_geometry, jedi_state_config )

  ! Model
  call jedi_model%initialise(jedi_pseudo_model_config)

  ! run app via model class
  call jedi_model%forecast(jedi_state, date_time_duration)

  ! to provide KGO
  call finalise_model(jedi_state%model_data)

end program jedi_forecast_pseudo
