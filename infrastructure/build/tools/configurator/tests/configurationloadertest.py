#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################

import unittest
import StringIO

import configurator.configurationloader as loader

###############################################################################
class LoaderTest( unittest.TestCase ):
    def setUp( self ):
        self.maxDiff = None

    ###########################################################################
    def testEmpty( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! Handles the loading of namelists.
!
module empty_mod

  use constants_mod, only : i_native, l_def, str_def, str_max_filename
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMGet, ESMF_SUCCESS, &
                            ESMF_VMGetCurrent, ESMF_VMBroadcast

  implicit none

  private
  public :: read_configuration, ensure_configuration

contains

  ! Reads configuration namelists from a file.
  !
  ! [in] filename File holding the namelists.
  !
  ! TODO: Assumes namelist tags come at the start of lines.
  ! TODO: Support "namelist file" namelists which recursively call this
  !       procedure to load other namelist files.
  !
  subroutine read_configuration( filename )

    use io_utility_mod, only : open_file, close_file

    implicit none

    character(*), intent(in) :: filename

    integer(i_native) :: local_rank
    type(ESMF_VM)     :: vm

    character(str_def), allocatable :: namelists(:)
    integer(i_native) :: unit = -1
    integer(i_native) :: condition

    call ESMF_VMGetCurrent( vm=vm, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to get VM when trying to read configuration, file: "//filename
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    call ESMF_VMGet( vm, localPet=local_rank, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to query VM when trying to read configuration, file: "//filename
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    if (local_rank == 0) unit = open_file( filename )

    call get_namelist_names( unit, vm, local_rank, namelists )

    call read_configuration_namelists( unit, vm, local_rank, &
                                       namelists, filename )

    if (local_rank == 0) call close_file( unit )

  end subroutine read_configuration

  ! Finds names of all namelists present in file.
  !
  ! [in] unit File holding namelists.
  ! [out] names of namelist in file (in order).
  !
  ! TODO: Assumes namelist tags are at the start of lines.
  !
  subroutine get_namelist_names( unit, vm, local_rank, names )

    use io_utility_mod, only : read_line

    implicit none

    integer(i_native),  intent(in)                 :: unit
    type(esmf_vm),      intent(in)                 :: vm
    integer(i_native),  intent(in)                 :: local_rank
    character(str_def), intent(inout), allocatable :: names(:)

    character(str_def), allocatable :: names_temp(:)
    integer(i_native)  :: condition
    character(str_def) :: buffer
    logical(l_def)     :: continue_read
    ! Number of names - technically a scalar but must be defined as a
    ! single element array to be broadcast-able
    integer(i_native)  :: namecount(1)

    namecount = 0
    if (local_rank == 0) then
      text_line_loop: do

        continue_read = read_line( unit, buffer )
        if ( .not. continue_read ) exit text_line_loop

        if (buffer(1:1) == '&') then
          namecount = namecount + 1
          allocate(names_temp(namecount(1)))
          if (namecount(1) > 1) then
            names_temp(1:namecount(1)-1) = names
          end if
          names_temp(namecount(1)) = trim(buffer(2:))
          call move_alloc(names_temp, names)
        end if
      end do text_line_loop
      rewind(unit)
    end if

    call ESMF_VMBroadcast( vm, namecount, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast number of namelists"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    if (local_rank /= 0) then
      allocate(names(namecount(1)))
    end if

    call ESMF_VMBroadcast( vm, names, namecount(1)*str_def, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast list of namelist names"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

  end subroutine get_namelist_names

  ! Checks that the requested namelists have been loaded.
  !
  ! [in]  names List of namelists.
  ! [out] success_mask Marks corresponding namelists as having failed.
  !
  ! [return] Overall success.
  !
  function ensure_configuration( names, success_mask )

    implicit none

    character(*),             intent(in)  :: names(:)
    logical(l_def), optional, intent(out) :: success_mask(:)
    logical(l_def)                        :: ensure_configuration

    integer(i_native) :: i
    logical           :: configuration_found = .True.

    if (present(success_mask) &
        .and. (size(success_mask, 1) /= size(names, 1))) then
      call log_event( 'Arguments "names" and "success_mask" to function' &
                      // '"ensure_configuration" are different shapes',  &
                      LOG_LEVEL_ERROR )
    end if

    ensure_configuration = .True.

    name_loop: do i = 1, size(names)
      select case(trim( names(i) ))
        case default
          write( log_scratch_space, '(A, A, A)' )          &
               "Tried to ensure unrecognised namelist """, &
               trim(names(i)),                             &
               """ was loaded"
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end select

      ensure_configuration = ensure_configuration .and. configuration_found

      if (present(success_mask)) success_mask(i) = configuration_found

    end do name_loop

  end function ensure_configuration

  subroutine read_configuration_namelists( unit, vm, local_rank, &
                                           namelists, filename )
    implicit none

    integer(i_native),  intent(in) :: unit
    type(ESMF_VM),      intent(in) :: vm
    integer(i_native),  intent(in) :: local_rank
    character(str_def), intent(in) :: namelists(:)
    character(*),       intent(in) :: filename

    integer(i_native) :: i

    ! Read the namelists
    do i = 1, size(namelists)
      select case (trim(namelists(i)))
        case default
          write( log_scratch_space, '(A)' )        &
                "Unrecognised namelist """//        &
                trim(namelists(i))//                &
                """ found in file "//               &
                trim(filename)
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end select
    end do

    ! Perform post load actions
    do i = 1, size(namelists)
      select case (trim(namelists(i)))
        case default
          write( log_scratch_space, '(A)' )        &
                "Unrecognised namelist """//        &
                trim(namelists(i))//                &
                """ found in file "//               &
                trim(filename)
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end select
    end do

  end subroutine read_configuration_namelists

end module empty_mod
        '''.strip()

        outputFile = StringIO.StringIO()
        uut = loader.ConfigurationLoader( 'empty_mod')
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

    ###########################################################################
    def testWithContent( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! Handles the loading of namelists.
!
module content_mod

  use constants_mod, only : i_native, l_def, str_def, str_max_filename
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMGet, ESMF_SUCCESS, &
                            ESMF_VMGetCurrent, ESMF_VMBroadcast

  use foo_config_mod, only : read_foo_namelist, &
                             postprocess_foo_namelist, &
                             foo_is_loadable, &
                             foo_is_loaded

  implicit none

  private
  public :: read_configuration, ensure_configuration

contains

  ! Reads configuration namelists from a file.
  !
  ! [in] filename File holding the namelists.
  !
  ! TODO: Assumes namelist tags come at the start of lines.
  ! TODO: Support "namelist file" namelists which recursively call this
  !       procedure to load other namelist files.
  !
  subroutine read_configuration( filename )

    use io_utility_mod, only : open_file, close_file

    implicit none

    character(*), intent(in) :: filename

    integer(i_native) :: local_rank
    type(ESMF_VM)     :: vm

    character(str_def), allocatable :: namelists(:)
    integer(i_native) :: unit = -1
    integer(i_native) :: condition

    call ESMF_VMGetCurrent( vm=vm, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to get VM when trying to read configuration, file: "//filename
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    call ESMF_VMGet( vm, localPet=local_rank, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to query VM when trying to read configuration, file: "//filename
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    if (local_rank == 0) unit = open_file( filename )

    call get_namelist_names( unit, vm, local_rank, namelists )

    call read_configuration_namelists( unit, vm, local_rank, &
                                       namelists, filename )

    if (local_rank == 0) call close_file( unit )

  end subroutine read_configuration

  ! Finds names of all namelists present in file.
  !
  ! [in] unit File holding namelists.
  ! [out] names of namelist in file (in order).
  !
  ! TODO: Assumes namelist tags are at the start of lines.
  !
  subroutine get_namelist_names( unit, vm, local_rank, names )

    use io_utility_mod, only : read_line

    implicit none

    integer(i_native),  intent(in)                 :: unit
    type(esmf_vm),      intent(in)                 :: vm
    integer(i_native),  intent(in)                 :: local_rank
    character(str_def), intent(inout), allocatable :: names(:)

    character(str_def), allocatable :: names_temp(:)
    integer(i_native)  :: condition
    character(str_def) :: buffer
    logical(l_def)     :: continue_read
    ! Number of names - technically a scalar but must be defined as a
    ! single element array to be broadcast-able
    integer(i_native)  :: namecount(1)

    namecount = 0
    if (local_rank == 0) then
      text_line_loop: do

        continue_read = read_line( unit, buffer )
        if ( .not. continue_read ) exit text_line_loop

        if (buffer(1:1) == '&') then
          namecount = namecount + 1
          allocate(names_temp(namecount(1)))
          if (namecount(1) > 1) then
            names_temp(1:namecount(1)-1) = names
          end if
          names_temp(namecount(1)) = trim(buffer(2:))
          call move_alloc(names_temp, names)
        end if
      end do text_line_loop
      rewind(unit)
    end if

    call ESMF_VMBroadcast( vm, namecount, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast number of namelists"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    if (local_rank /= 0) then
      allocate(names(namecount(1)))
    end if

    call ESMF_VMBroadcast( vm, names, namecount(1)*str_def, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast list of namelist names"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

  end subroutine get_namelist_names

  ! Checks that the requested namelists have been loaded.
  !
  ! [in]  names List of namelists.
  ! [out] success_mask Marks corresponding namelists as having failed.
  !
  ! [return] Overall success.
  !
  function ensure_configuration( names, success_mask )

    implicit none

    character(*),             intent(in)  :: names(:)
    logical(l_def), optional, intent(out) :: success_mask(:)
    logical(l_def)                        :: ensure_configuration

    integer(i_native) :: i
    logical           :: configuration_found = .True.

    if (present(success_mask) &
        .and. (size(success_mask, 1) /= size(names, 1))) then
      call log_event( 'Arguments "names" and "success_mask" to function' &
                      // '"ensure_configuration" are different shapes',  &
                      LOG_LEVEL_ERROR )
    end if

    ensure_configuration = .True.

    name_loop: do i = 1, size(names)
      select case(trim( names(i) ))
        case ('foo')
          configuration_found = foo_is_loaded()
        case default
          write( log_scratch_space, '(A, A, A)' )          &
               "Tried to ensure unrecognised namelist """, &
               trim(names(i)),                             &
               """ was loaded"
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end select

      ensure_configuration = ensure_configuration .and. configuration_found

      if (present(success_mask)) success_mask(i) = configuration_found

    end do name_loop

  end function ensure_configuration

  subroutine read_configuration_namelists( unit, vm, local_rank, &
                                           namelists, filename )
    implicit none

    integer(i_native),  intent(in) :: unit
    type(ESMF_VM),      intent(in) :: vm
    integer(i_native),  intent(in) :: local_rank
    character(str_def), intent(in) :: namelists(:)
    character(*),       intent(in) :: filename

    integer(i_native) :: i

    ! Read the namelists
    do i = 1, size(namelists)
      select case (trim(namelists(i)))
        case ('foo')
          if (foo_is_loadable()) then
            call read_foo_namelist( unit, vm, local_rank )
          else
            write( log_scratch_space, '(A)' )      &
                  "Namelist """//                   &
                  trim(namelists(i))//              &
                  """ can not be read. Too many instances?"
            call log_event( log_scratch_space, LOG_LEVEL_ERROR )
          end if
        case default
          write( log_scratch_space, '(A)' )        &
                "Unrecognised namelist """//        &
                trim(namelists(i))//                &
                """ found in file "//               &
                trim(filename)
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end select
    end do

    ! Perform post load actions
    do i = 1, size(namelists)
      select case (trim(namelists(i)))
        case ('foo')
          call postprocess_foo_namelist()
        case default
          write( log_scratch_space, '(A)' )        &
                "Unrecognised namelist """//        &
                trim(namelists(i))//                &
                """ found in file "//               &
                trim(filename)
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end select
    end do

  end subroutine read_configuration_namelists

end module content_mod
        '''.strip()

        outputFile = StringIO.StringIO()
        uut = loader.ConfigurationLoader( 'content_mod' )
        uut.addNamelist( 'foo' )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n',
                                   outputFile.getvalue() )
