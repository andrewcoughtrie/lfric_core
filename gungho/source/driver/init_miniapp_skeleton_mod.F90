!-----------------------------------------------------------------------------
! (c) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

!> @brief init functionality for the miniapp skeleton

!> @details Handles init of prognostic fields and through the call to 
!>          runtime_csontants the coordinate fields and fem operators

module init_miniapp_skeleton_mod

  use constants_mod,                  only : i_def
  use field_mod,                      only : field_type, write_interface
  use finite_element_config_mod,      only : element_order
  use function_space_collection_mod,  only : function_space_collection
  use fs_continuity_mod,              only : W3
  use log_mod,                        only : log_event,         &
                                             LOG_LEVEL_INFO, &
                                             LOG_LEVEL_ERROR
  use runtime_constants_mod,          only : create_runtime_constants
  use output_config_mod,              only : write_xios_output
  use io_mod,                         only : xios_write_field_face
  implicit none


  contains

  subroutine init_miniapp_skeleton(mesh_id, chi, field_1)

    integer(i_def), intent(in)               :: mesh_id
    ! Prognostic fields
    type( field_type ), intent(inout)        :: field_1
    ! Coordinate field
    type( field_type ), intent(inout)        :: chi(:)

    procedure(write_interface), pointer      :: tmp_ptr

    call log_event( 'miniapp skeleton: initialisation...', LOG_LEVEL_INFO )


    ! Create prognostic fields
    ! Creates a field in the W3 function space (fully discontinuous field)
    field_1 = field_type( vector_space = &
                      function_space_collection%get_fs(mesh_id, element_order, W3) )

    ! Set up field with an IO behaviour (XIOS only at present)

    if (write_xios_output) then

       tmp_ptr => xios_write_field_face

       call field_1%set_write_field_behaviour(tmp_ptr)
       
    end if

    ! Create runtime_constants object. This in turn creates various things
    ! needed by the fem algorithms such as mass matrix operators, mass
    ! matrix diagonal fields and the geopotential field
    call create_runtime_constants(mesh_id, chi)

    call log_event( 'miniapp skeleton initialised', LOG_LEVEL_INFO )

  end subroutine init_miniapp_skeleton

end module init_miniapp_skeleton_mod
