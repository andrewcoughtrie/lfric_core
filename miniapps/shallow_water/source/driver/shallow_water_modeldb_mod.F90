!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Container for everything that makes up the shallow water model state
!!
!> @details This module holds all the information required to run an instance
!>          of the shallow water model (such as an ensemble member). That
!>          includes all the scientific and technical state. Nothing
!>          should be held in global space, unless it is truely global
!!
module shallow_water_modeldb_mod

  use shallow_water_model_data_mod, only: model_data_type
  use model_clock_mod,              only: model_clock_type
  use mpi_mod,                      only: mpi_type
  use namelist_collection_mod,      only: namelist_collection_type

  implicit none

  private

  !> Holds the technical and scientific model state for the shallow water model
  !>
  type :: modeldb_type

    private

    !> Stores all the fields used by the model
    type( model_data_type ), public :: model_data

    !> Stores all the configuration namelists used by the model
    !> from a namelist input file.
    type( namelist_collection_type ), public :: configuration

    !> Manages time in the model instance
    type(model_clock_type), public, allocatable :: clock

    !> MPI object that contains all the functionality to perform MPI tasks
    !> on the MPI communicator for this model instance.
    !> @todo  Currently, this is juat a pointer to the global MPI object, as
    !>        this is the only MPI object that PSyclone can use at the moment
    !>        When PSyclone is updated, this can be the actual MPI object.
    type(mpi_type), pointer, public :: mpi

    contains

  end type modeldb_type

  public modeldb_type

contains

end module shallow_water_modeldb_mod
