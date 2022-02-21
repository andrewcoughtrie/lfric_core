! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
PROGRAM skeleton

! lfricinputs modules
USE lfricinp_finalise_lfric_mod, ONLY: lfricinp_finalise_lfric
USE lfricinp_read_command_line_args_mod, ONLY: lfricinp_read_command_line_args
USE lfricinp_create_lfric_fields_mod,  ONLY: lfricinp_create_lfric_fields
USE lfricinp_lfric_driver_mod, ONLY: lfricinp_initialise_lfric, mesh, &
     twod_mesh, lfric_fields
USE lfricinp_initialise_um_mod, ONLY: lfricinp_initialise_um, um_input_file
USE lfricinp_um_grid_mod, ONLY: um_grid
USE lfricinp_regrid_options_mod, ONLY: lfricinp_init_regrid_options
USE lfricinp_datetime_mod, ONLY : datetime_type

! skeleton modules
USE skeleton_namelists_mod, ONLY: skeleton_nl_fname, lfric_nl_fname, &
     skeleton_config, required_lfric_namelists
USE skeleton_initialise_skeleton_mod, ONLY: skeleton_initialise_skeleton


IMPLICIT NONE

TYPE(datetime_type) :: datetime

! Read command line args
CALL lfricinp_read_command_line_args(skeleton_nl_fname, lfric_nl_fname)

! Read in global regrid options
CALL lfricinp_init_regrid_options(skeleton_nl_fname)

! Load date and time information
CALL datetime % initialise()

! Initialise LFRic Infrastructure
CALL lfricinp_initialise_lfric(program_name_arg="skeleton",                    &
     lfric_nl_fname=lfric_nl_fname,                                            &
     required_lfric_namelists = required_lfric_namelists,                      &
     calendar = datetime % calendar,                                           &
     start_date = datetime % first_validity_time,                              &
     time_origin = datetime % first_validity_time,                             &
     first_step = datetime % first_step,                                       &
     last_step = datetime % last_step,                                         &
     spinup_period = datetime % spinup_period,                                 &
     seconds_per_step = datetime % seconds_per_step)

! Read skeleton namelist
CALL skeleton_config%load_namelists(skeleton_nl_fname)

! Initialise skeleton
CALL skeleton_initialise_skeleton()

! Initialise UM Infrastructure
!CALL lfricinp_initialise_um("filename")

! Create LFRic field collection based on list of stashcodes
CALL lfricinp_create_lfric_fields( mesh, twod_mesh, lfric_fields,       &
                                   skeleton_config%stash_list, um_grid, &
                                   um_input_file)

!! Main loop

! Finalise LFRic infrastructure
CALL lfricinp_finalise_lfric()

END PROGRAM skeleton
