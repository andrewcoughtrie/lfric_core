!-----------------------------------------------------------------------------
! (C) Crown copyright 2023 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
!> @brief Container that holds all the model state.
!> @details The modeldb_type for io_dev is an extension of the modeldb_type
!> from the driver component layer.
!> @todo Move linked_list_type to be held in driver_modeldb_type, see #4183
module io_dev_modeldb_mod

  use driver_modeldb_mod, only: driver_modeldb_type => modeldb_type
  use linked_list_mod,    only: linked_list_type

  implicit none

  private
  public :: modeldb_type

  !> Holds the state of the model.
  !> It extends the base modeldb type in order to hold an io_dev specific linked list.
  type, extends(driver_modeldb_type) :: modeldb_type
    private

    !> linked_list_type of time_axis objects for the variable fields
    type(linked_list_type), public :: variable_field_times

  contains

  end type modeldb_type

contains

end module io_dev_modeldb_mod