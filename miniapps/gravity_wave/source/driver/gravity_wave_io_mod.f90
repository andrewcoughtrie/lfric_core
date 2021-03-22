!-----------------------------------------------------------------------------
! (C) Crown copyright 2019 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief Controls output (diags/checkpointing) related information used by
!>        the model

module gravity_wave_io_mod

  use constants_mod,           only : i_def, i_native
  use field_mod,               only : field_type
  use io_config_mod,           only : use_xios_io
  use io_context_mod,          only : io_context_type
  use log_mod,                 only : log_event, log_level_error
  use simple_io_mod,           only : initialise_simple_io
  use time_config_mod,         only : timestep_end, timestep_start
  use timestepping_config_mod, only : dt, spinup_period
  use lfric_xios_clock_mod,    only : lfric_xios_clock_type
  use lfric_xios_io_mod,       only : initialise_xios

  implicit none

  private
  public initialise_io, finalise_io

contains

  !> @brief Initialises output (diags/checkpointing) used by the model.
  !>
  !> @param [in]  comm         The MPI communicator for use within the model.
  !> @param [in]  mesh_id      The identifier of the primary mesh.
  !> @param [in]  twod_mesh_id The identifier of the primary 2d mesh.
  !> @param [in]  chi_xyz      A size 3 array of fields holding the (X,Y,Z)
  !>                           coordinates of the mesh.
  !> @param [in]  context_name I/O context identifier.
  !> @param [out] io_context   Context in which I/O operations are performed.
  !>
  subroutine initialise_io( comm,         &
                            mesh_id,      &
                            twod_mesh_id, &
                            chi_xyz,      &
                            context_name, &
                            io_context )

    implicit none

    integer(i_native),      intent(in)  :: comm
    integer(i_def),         intent(in)  :: mesh_id
    integer(i_def),         intent(in)  :: twod_mesh_id
    type(field_type),       intent(in)  :: chi_xyz(3)
    character(len=*),       intent(in)  :: context_name
    class(io_context_type), intent(out), &
                            allocatable :: io_context

    !--------------------------------------------------------------------------
    ! IO init
    !--------------------------------------------------------------------------

    if (use_xios_io) then
      call initialise_xios( io_context,     &
                            context_name,   &
                            comm,           &
                            mesh_id,        &
                            twod_mesh_id,   &
                            chi_xyz,        &
                            timestep_start, &
                            timestep_end,   &
                            spinup_period,  &
                            dt )
    else
      call initialise_simple_io( io_context,     &
                                 timestep_start, &
                                 timestep_end,   &
                                 spinup_period,  &
                                 dt )
    end if

  end subroutine initialise_io

  !> @brief Finalises output related functions used by the model.
  !>
  subroutine finalise_io()

    implicit none

  end subroutine finalise_io

end module gravity_wave_io_mod
