!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> Drives the stepping of the diagnostics miniapp
!>
module diagnostics_step_mod

    use clock_mod,             only : clock_type
    use constants_mod,         only : i_def
    use diagnostics_alg_mod,   only : diagnostics_alg
    use field_mod,             only : field_type
    use gungho_model_data_mod, only : model_data_type

    implicit none

    private
    public diagnostics_step


contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !> Performs time steps.
    !>
    subroutine diagnostics_step( mesh_id,      &
                                 twod_mesh_id, &
                                 model_data,   &
                                 clock )

        implicit none

        ! Model run working data set
        integer(i_def),                  intent(in)    :: mesh_id ! included for consistency with gungho
        integer(i_def),                  intent(in)    :: twod_mesh_id ! included for consistency with gungho
        type( model_data_type ), target, intent(inout) :: model_data
        class(clock_type),               intent(in)    :: clock ! included for consistency with gungho

        type(field_type), pointer :: red => null(), green => null(), blue => null(), hex => null()


        red => model_data%prognostic_fields%get_field("red")
        green => model_data%prognostic_fields%get_field("green")
        blue => model_data%prognostic_fields%get_field("blue")
        hex => model_data%diagnostic_fields%get_field("hex")


        ! Call an algorithm
        call diagnostics_alg(&
                red, &
                green, &
                blue, &
                hex &
                )

    end subroutine diagnostics_step

end module diagnostics_step_mod
