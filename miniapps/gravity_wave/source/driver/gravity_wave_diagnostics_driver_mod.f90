!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Outputs the diagnostics from the gravity-wave miniapp

!> @details Calls the routine that generates diagnostic output for all
!>          fields used by the gravity-wave miniapp. This is only a temporary
!>          hard-coded solution in lieu of a proper dianostic system

module gravity_wave_diagnostics_driver_mod

  use clock_mod,            only : clock_type
  use constants_mod,        only : i_def
  use field_mod,            only : field_type
  use field_collection_mod, only : field_collection_type
  use diagnostics_io_mod,   only : write_scalar_diagnostic, &
                                   write_vector_diagnostic
  implicit none

  private
  public gravity_wave_diagnostics_driver

contains

  !> @brief Outputs the diagnostics from the gravity-wave miniapp
  !> @param [in] mesh_id The identifier of the primary mesh
  !> @param [inout] state A collection containing the fields that will
  !>                   be written to diagnostic output
  !> @param [in] clock Model time.
  !> @param [in] W3_project Flag that determines if vector fields should be
  !>                        projected to W3
  subroutine gravity_wave_diagnostics_driver( mesh_id, &
                                              state,   &
                                              clock,   &
                                              W3_project )

    implicit none

    type(field_collection_type), intent(inout) :: state
    integer(i_def),              intent(in)    :: mesh_id
    class(clock_type),           intent(in)    :: clock
    logical,                     intent(in)    :: W3_project

    type( field_type), pointer :: wind => null()
    type( field_type), pointer :: buoyancy => null()
    type( field_type), pointer :: pressure => null()

    ! Can't just iterate through the collection as some fields are scalars
    ! and some fields are vectors, so explicitly extract all fields from
    ! the collection and output each of them
    wind => state%get_field('wind')
    buoyancy => state%get_field('buoyancy')
    pressure => state%get_field('pressure')

    ! Calculation and output of diagnostics
    call write_vector_diagnostic( 'wind', wind, &
                                  clock, mesh_id, W3_project )
    call write_scalar_diagnostic( 'pressure', pressure, &
                                  clock, mesh_id, W3_project )
    call write_scalar_diagnostic( 'buoyancy', buoyancy, &
                                  clock, mesh_id, W3_project )

  end subroutine gravity_wave_diagnostics_driver

end module gravity_wave_diagnostics_driver_mod
