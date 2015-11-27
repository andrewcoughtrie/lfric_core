!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!> @brief Defines various initialisation options for the model.
!>
!> @details Initialisation options for prognostic fields and test cases defined
!>          in this module include: 
!>          Idealised test options and choice; 
!>          Wind initialisation (wind profiles options and choice, also setup of 
!>          wind components); 
!>          Temperature initialisation (Brunt-Vaisala frequency). 
module initialisation_mod

  use constants_mod, only: r_def, i_def 

  implicit none


  !=========================== Idealised test options =========================!

  !> @name Idealised test options
  !> @{
  integer, parameter :: ITEST_GRAVITY_WAVE = 1  !< Gravity wave test (either planar or spherical).   
  integer, parameter :: ITEST_COLD_BUBBLE  = 2  !< Straka density cold current test (planar domain only).
  integer, parameter :: ITEST_WARM_BUBBLE  = 3  !< Warm bubble test (planar domain only).

  !> @}

  !> @name Idealised test choice
  !> @{
  integer(kind=i_def) :: itest_option = ITEST_GRAVITY_WAVE   !< Choice of which idealised test to run.
  !> @}

  !=========================== Wind initialisation  ===========================!

  !> @name Wind profiles options
  !> @{
  integer, parameter :: ZERO_WIND                = 0   !< Prescribed background wind.
  integer, parameter :: SOLID_BODY_ROTATION_WIND = 1   !< Analytic wind profile from the Solid Body 
                                                       !! Rotation test case.
  integer, parameter :: CONSTANT_UV_WIND         = 2   !< Constant horizontal wind.
  integer, parameter :: CONSTANT_SHEAR_UV_WIND   = 3   !< Constant horizontal wind shear.
  !> @}

  !> @name Wind components setup
  !> @{
  real(kind=r_def), parameter :: U0 = 0.0_r_def    !< Prescribed background U horizontal wind component.
  real(kind=r_def), parameter :: V0 = 0.0_r_def    !< Prescribed background V horizontal wind component.
  !> @}

  !> @name Wind initialisation choice
  !> @{
  integer(kind=i_def) :: initial_u_profile = ZERO_WIND   !< Choice of which wind profile to use.
  real(kind=r_def)    :: rotation_angle = 0.0_r_def      !< Rotated wind profile.
  !> @}

  !=========================== Temperature (related) initialisation  ==========!

  !> @name Temperature initialisation parameters
  !> @{
  real(kind=r_def) :: n_sq = 0.0001_r_def   !< The square of Brunt-Vaisala frequency [1/s^2].  
  !> @}

contains

!> @brief Subroutine which initialises Dynamo.
!> @details The routine reads in namelist file with a number of options to 
!>          initialise Dynamo, such as choice of idealised test (itest_option), 
!>          setting of initial wind profile etc.
subroutine read_initialisation_namelist()

  use constants_mod, only: str_max_filename, str_long
  use log_mod,       only: log_event, log_scratch_space, &
                           LOG_LEVEL_INFO, LOG_LEVEL_ERROR 

  implicit none


  integer, parameter                :: funit = 888
  integer                           :: ierr
  character(len = str_max_filename) :: init_fname
  character(len = str_long)         :: ioerrmsg = ''

  namelist /idealised_test_nml/ itest_option
  namelist /wind_nml/ initial_u_profile, rotation_angle
  namelist /temperature_nml/ n_sq

  ! Name of initialisation file
  init_fname = 'dynamo_initfile.nml' 
  ! Open initialisation file
  open(funit, file = trim(init_fname), iostat = ierr, status = 'old', &
       iomsg = ioerrmsg)
  if (ierr /= 0) then
    write(log_scratch_space,'(A,A)') "Problems opening file: ",trim(init_fname)
    call log_event(log_scratch_space,LOG_LEVEL_INFO)
    call log_event(ioerrmsg,LOG_LEVEL_ERROR)
  end if
  ! Read initialisation file
  read(funit, nml = idealised_test_nml, iostat = ierr, iomsg = ioerrmsg)
  if (ierr /= 0) then
    write(log_scratch_space,'(A,A)') "Problems reading idealised_test_nml in ", &
          trim(init_fname)
    call log_event(log_scratch_space,LOG_LEVEL_INFO)
    call log_event(ioerrmsg,LOG_LEVEL_ERROR)
  end if
  read(funit, nml = wind_nml, iostat = ierr, iomsg = ioerrmsg)
  if (ierr /= 0) then
    write(log_scratch_space,'(A,A)') "Problems reading wind_nml in ", &
          trim(init_fname)
    call log_event(log_scratch_space,LOG_LEVEL_INFO)
    call log_event(ioerrmsg,LOG_LEVEL_ERROR)
  end if
  read(funit, nml = temperature_nml, iostat = ierr, iomsg = ioerrmsg)
  if (ierr /= 0) then
    write(log_scratch_space,'(A,A)') "Problems reading temperature_nml in ", &
          trim(init_fname)
    call log_event(log_scratch_space,LOG_LEVEL_INFO)
    call log_event(ioerrmsg,LOG_LEVEL_ERROR)
  end if

  ! Close initialisation file
  close(funit, iostat = ierr, iomsg = ioerrmsg)
  if (ierr /= 0) then
       write(log_scratch_space,'(A,A)') "Closing file: ", trim(init_fname)
       call log_event(log_scratch_space,LOG_LEVEL_INFO)
       call log_event(ioerrmsg,LOG_LEVEL_ERROR)
    end if

  ! Notify the user
  call log_event( "initialise_dynamo: Read initialisation namelists ", LOG_LEVEL_INFO )

end subroutine read_initialisation_namelist


end module initialisation_mod
