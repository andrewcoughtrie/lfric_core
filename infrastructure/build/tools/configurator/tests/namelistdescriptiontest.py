#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################

import unittest
import StringIO

import configurator.namelistdescription as description

###############################################################################
class NamelistDescriptionTest( unittest.TestCase ):
    ###########################################################################
    def setUp( self ):
        self.maxDiff = None

    ###########################################################################
    def tearDown( self ):
        pass

    ###########################################################################
    def testModuleWriteEmpty( self ):
        outputFile = StringIO.StringIO()

        uut = description.NamelistDescription( 'test' )
        self.assertRaises( description.NamelistDescriptionException, \
                           uut.writeModule, outputFile )

    ###########################################################################
    def testModuleWriteOneOfEach( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the test namelist.
!>
module test_config_mod

  use constants_mod, only : i_def, &
                            i_long, &
                            i_native, &
                            i_short, &
                            l_def, &
                            r_def, &
                            r_double, &
                            r_single, &
                            str_def, &
                            str_max_filename
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMBroadcast, ESMF_SUCCESS

  implicit none

  private
  public :: enum_from_key, key_from_enum, &
            read_test_namelist, postprocess_test_namelist, &
            test_is_loadable, test_is_loaded

  integer(i_native), public, parameter :: test_enum_one = 100
  integer(i_native), public, parameter :: test_enum_two = 101
  integer(i_native), public, parameter :: test_enum_three = 102

  integer(i_def), public, protected :: dint
  logical(l_def), public, protected :: dlog
  real(r_def), public, protected :: dreal
  character(str_def), public, protected :: dstr
  integer(i_native), public, protected :: enum
  character(str_max_filename), public, protected :: fstr
  integer(i_long), public, protected :: lint
  real(r_double), public, protected :: lreal
  integer(i_short), public, protected :: sint
  real(r_single), public, protected :: sreal
  integer(i_def), public, protected :: vint
  real(r_def), public, protected :: vreal
  character(str_def), public, protected :: vstr

  logical :: namelist_loaded = .false.

  character(str_def), parameter :: enum_key(3) &
          = [character(len=str_def) :: 'one', &
                                       'two', &
                                       'three']

contains

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_native) function enum_from_key( key )

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    key_index = 1
    do
      if (trim(enum_key(key_index)) == trim(key)) then
        enum_from_key = key_index + test_enum_one - 1
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(enum_key, 1)) then
          write( log_scratch_space, &
                 '("Key ''", A, "'' not recognised for test enum")' ) key
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function enum_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_enum( value )

    implicit none

    integer(i_native), intent(in) :: value

    integer(i_native) :: key_index

    key_index = value - test_enum_one + 1
    if (key_index < lbound(enum_key, 1) &
        .or. key_index > ubound(enum_key, 1)) then
      write( log_scratch_space, &
             '("Value ", I0, " is not in test enum")' ) value
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    key_from_enum = enum_key( key_index )

  end function key_from_enum

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] vm ESMF VM object of current run.
  !> @param [in] local_rank Rank of current ESMF process.
  !>
  subroutine read_test_namelist( file_unit, vm, local_rank )
    implicit none
    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    call read_namelist( file_unit, vm, local_rank, enum )
  end subroutine read_test_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, vm, local_rank, dummy_enum )

    use constants_mod, only : imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    integer(i_native), intent(out) :: dummy_enum

    character(str_def) :: buffer_character_str_def(2)
    character(str_max_filename) :: buffer_character_str_max_filename(1)
    integer(i_def) :: buffer_integer_i_def(2)
    integer(i_long) :: buffer_integer_i_long(1)
    integer(i_native) :: buffer_integer_i_native(1)
    integer(i_short) :: buffer_integer_i_short(1)
    integer(i_native) :: buffer_logical_l_def(1)
    real(r_def) :: buffer_real_r_def(2)
    real(r_double) :: buffer_real_r_double(1)
    real(r_single) :: buffer_real_r_single(1)

    character(str_def) :: enum

    namelist /test/ dint, &
                    dlog, &
                    dreal, &
                    dstr, &
                    enum, &
                    fstr, &
                    lint, &
                    lreal, &
                    sint, &
                    sreal, &
                    vint, &
                    vreal, &
                    vstr

    integer(i_native) :: condition

    dint = imdi
    dlog = .false.
    dreal = rmdi
    dstr = ""
    enum = ""
    fstr = ""
    lint = imdi
    lreal = rmdi
    sint = imdi
    sreal = rmdi
    vint = imdi
    vreal = rmdi
    vstr = ""

    if (local_rank == 0) then

      read( file_unit, nml=test, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      dummy_enum = enum_from_key( enum )

    end if

    buffer_integer_i_def(2) = dint
    buffer_logical_l_def(1) = merge( 1, 0, dlog )
    buffer_real_r_def(2) = dreal
    buffer_character_str_def(2) = dstr
    buffer_integer_i_native(1) = dummy_enum
    buffer_character_str_max_filename(1) = fstr
    buffer_integer_i_long(1) = lint
    buffer_real_r_double(1) = lreal
    buffer_integer_i_short(1) = sint
    buffer_real_r_single(1) = sreal
    buffer_integer_i_def(1) = vint
    buffer_real_r_def(1) = vreal
    buffer_character_str_def(1) = vstr

    call ESMF_VMBroadcast( vm, buffer_character_str_def, 2*str_def, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_character_str_def'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_character_str_max_filename, 1*str_max_filename, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_character_str_max_filename'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_integer_i_def, 2, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_integer_i_def'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_integer_i_long, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_integer_i_long'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_integer_i_native, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_integer_i_native'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_integer_i_short, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_integer_i_short'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_logical_l_def, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_logical_l_def'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_real_r_def, 2, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_real_r_def'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_real_r_double, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_real_r_double'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_real_r_single, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_real_r_single'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    dint = buffer_integer_i_def(2)
    dlog = buffer_logical_l_def(1) /= 0
    dreal = buffer_real_r_def(2)
    dstr = buffer_character_str_def(2)
    dummy_enum = buffer_integer_i_native(1)
    fstr = buffer_character_str_max_filename(1)
    lint = buffer_integer_i_long(1)
    lreal = buffer_real_r_double(1)
    sint = buffer_integer_i_short(1)
    sreal = buffer_real_r_single(1)
    vint = buffer_integer_i_def(1)
    vreal = buffer_real_r_def(1)
    vstr = buffer_character_str_def(1)



    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_test_namelist()

    use constants_mod, only : imdi, rmdi

    implicit none


  end subroutine postprocess_test_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function test_is_loadable()

    implicit none

    logical :: test_is_loadable

    test_is_loadable = .not. namelist_loaded

  end function test_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function test_is_loaded()

    implicit none

    logical :: test_is_loaded

    test_is_loaded = namelist_loaded

  end function test_is_loaded

end module test_config_mod
        '''.strip()

        outputFile = StringIO.StringIO()

        uut = description.NamelistDescription( 'test' )
        uut.addParameter( 'vint', 'integer'             )
        uut.addParameter( 'dint', 'integer', 'default'  )
        uut.addParameter( 'sint', 'integer', 'short'    )
        uut.addParameter( 'lint', 'integer', 'long'     )
        uut.addParameter( 'dlog', 'logical', 'default'  )
        uut.addParameter( 'vreal', 'real'               )
        uut.addParameter( 'dreal', 'real',   'default'  )
        uut.addParameter( 'sreal', 'real',   'single'   )
        uut.addParameter( 'lreal', 'real',   'double'   )
        uut.addParameter( 'vstr', 'string'              )
        uut.addParameter( 'dstr', 'string',  'default'  )
        uut.addParameter( 'fstr', 'string',  'filename' )
        uut.addParameter( 'enum', 'enumeration', None,
                          enumerators=['one', 'two', 'three'] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

    ###########################################################################
    def testModuleWriteGrowing( self ):
        firstExpectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the test namelist.
!>
module test_config_mod

  use constants_mod, only : i_def, &
                            i_native
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMBroadcast, ESMF_SUCCESS

  implicit none

  private
  public :: read_test_namelist, postprocess_test_namelist, &
            test_is_loadable, test_is_loaded

  integer(i_def), public, protected :: foo

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] vm ESMF VM object of current run.
  !> @param [in] local_rank Rank of current ESMF process.
  !>
  subroutine read_test_namelist( file_unit, vm, local_rank )
    implicit none
    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    call read_namelist( file_unit, vm, local_rank )
  end subroutine read_test_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, vm, local_rank )

    use constants_mod, only : imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank

    integer(i_def) :: buffer_integer_i_def(1)

    namelist /test/ foo

    integer(i_native) :: condition

    foo = imdi

    if (local_rank == 0) then

      read( file_unit, nml=test, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_integer_i_def(1) = foo

    call ESMF_VMBroadcast( vm, buffer_integer_i_def, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_integer_i_def'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    foo = buffer_integer_i_def(1)



    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_test_namelist()

    use constants_mod, only : imdi, rmdi

    implicit none


  end subroutine postprocess_test_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function test_is_loadable()

    implicit none

    logical :: test_is_loadable

    test_is_loadable = .not. namelist_loaded

  end function test_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function test_is_loaded()

    implicit none

    logical :: test_is_loaded

    test_is_loaded = namelist_loaded

  end function test_is_loaded

end module test_config_mod
        '''.strip()

        secondExpectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the test namelist.
!>
module test_config_mod

  use constants_mod, only : i_def, &
                            i_native, &
                            r_def
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMBroadcast, ESMF_SUCCESS

  implicit none

  private
  public :: read_test_namelist, postprocess_test_namelist, &
            test_is_loadable, test_is_loaded

  real(r_def), public, protected :: bar
  integer(i_def), public, protected :: foo

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] vm ESMF VM object of current run.
  !> @param [in] local_rank Rank of current ESMF process.
  !>
  subroutine read_test_namelist( file_unit, vm, local_rank )
    implicit none
    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    call read_namelist( file_unit, vm, local_rank )
  end subroutine read_test_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, vm, local_rank )

    use constants_mod, only : imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank

    integer(i_def) :: buffer_integer_i_def(1)
    real(r_def) :: buffer_real_r_def(1)

    namelist /test/ bar, &
                    foo

    integer(i_native) :: condition

    bar = rmdi
    foo = imdi

    if (local_rank == 0) then

      read( file_unit, nml=test, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_real_r_def(1) = bar
    buffer_integer_i_def(1) = foo

    call ESMF_VMBroadcast( vm, buffer_integer_i_def, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_integer_i_def'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, buffer_real_r_def, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_real_r_def'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    bar = buffer_real_r_def(1)
    foo = buffer_integer_i_def(1)



    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_test_namelist()

    use constants_mod, only : imdi, rmdi

    implicit none


  end subroutine postprocess_test_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function test_is_loadable()

    implicit none

    logical :: test_is_loadable

    test_is_loadable = .not. namelist_loaded

  end function test_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function test_is_loaded()

    implicit none

    logical :: test_is_loaded

    test_is_loaded = namelist_loaded

  end function test_is_loaded

end module test_config_mod
        '''.strip()

        outputFile = StringIO.StringIO()

        uut = description.NamelistDescription( 'test' )
        uut.addParameter( 'foo', 'integer' )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( firstExpectedSource + '\n', \
                                   outputFile.getvalue() )

        outputFile = StringIO.StringIO()
        uut.addParameter( 'bar', 'real', 'default' )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( secondExpectedSource + '\n', \
                                   outputFile.getvalue() )

    ##########################################################################
    def testEnumerationOnly( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the enum namelist.
!>
module enum_config_mod

  use constants_mod, only : i_native, &
                            str_def
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMBroadcast, ESMF_SUCCESS

  implicit none

  private
  public :: value_from_key, key_from_value, &
            read_enum_namelist, postprocess_enum_namelist, &
            enum_is_loadable, enum_is_loaded

  integer(i_native), public, parameter :: enum_value_one = 100
  integer(i_native), public, parameter :: enum_value_two = 101
  integer(i_native), public, parameter :: enum_value_three = 102

  integer(i_native), public, protected :: value

  logical :: namelist_loaded = .false.

  character(str_def), parameter :: value_key(3) &
          = [character(len=str_def) :: 'one', &
                                       'two', &
                                       'three']

contains

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_native) function value_from_key( key )

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    key_index = 1
    do
      if (trim(value_key(key_index)) == trim(key)) then
        value_from_key = key_index + enum_value_one - 1
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(value_key, 1)) then
          write( log_scratch_space, &
                 '("Key ''", A, "'' not recognised for enum value")' ) key
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function value_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_value( value )

    implicit none

    integer(i_native), intent(in) :: value

    integer(i_native) :: key_index

    key_index = value - enum_value_one + 1
    if (key_index < lbound(value_key, 1) &
        .or. key_index > ubound(value_key, 1)) then
      write( log_scratch_space, &
             '("Value ", I0, " is not in enum value")' ) value
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    key_from_value = value_key( key_index )

  end function key_from_value

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] vm ESMF VM object of current run.
  !> @param [in] local_rank Rank of current ESMF process.
  !>
  subroutine read_enum_namelist( file_unit, vm, local_rank )
    implicit none
    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    call read_namelist( file_unit, vm, local_rank, value )
  end subroutine read_enum_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, vm, local_rank, dummy_value )

    use constants_mod, only : imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    integer(i_native), intent(out) :: dummy_value

    integer(i_native) :: buffer_integer_i_native(1)

    character(str_def) :: value

    namelist /enum/ value

    integer(i_native) :: condition

    value = ""

    if (local_rank == 0) then

      read( file_unit, nml=enum, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      dummy_value = value_from_key( value )

    end if

    buffer_integer_i_native(1) = dummy_value

    call ESMF_VMBroadcast( vm, buffer_integer_i_native, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_integer_i_native'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    dummy_value = buffer_integer_i_native(1)



    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_enum_namelist()

    use constants_mod, only : imdi, rmdi

    implicit none


  end subroutine postprocess_enum_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function enum_is_loadable()

    implicit none

    logical :: enum_is_loadable

    enum_is_loadable = .not. namelist_loaded

  end function enum_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function enum_is_loaded()

    implicit none

    logical :: enum_is_loaded

    enum_is_loaded = namelist_loaded

  end function enum_is_loaded

end module enum_config_mod
        '''.strip()

        outputFile = StringIO.StringIO()

        uut = description.NamelistDescription( 'enum' )
        uut.addParameter( 'value',
                          'enumeration',
                          None,
                          enumerators=['one', 'two', 'three'] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n',
                                   outputFile.getvalue() )

    ###########################################################################
    def testMoreThanOneEnumeration( self ):
      expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the twoenum namelist.
!>
module twoenum_config_mod

  use constants_mod, only : i_native, &
                            str_def
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMBroadcast, ESMF_SUCCESS

  implicit none

  private
  public :: first_from_key, key_from_first, &
            second_from_key, key_from_second, &
            read_twoenum_namelist, postprocess_twoenum_namelist, &
            twoenum_is_loadable, twoenum_is_loaded

  integer(i_native), public, parameter :: twoenum_first_one = 100
  integer(i_native), public, parameter :: twoenum_first_two = 101
  integer(i_native), public, parameter :: twoenum_first_three = 102
  integer(i_native), public, parameter :: twoenum_second_ay = 103
  integer(i_native), public, parameter :: twoenum_second_bee = 104
  integer(i_native), public, parameter :: twoenum_second_see = 105

  integer(i_native), public, protected :: first
  integer(i_native), public, protected :: second

  logical :: namelist_loaded = .false.

  character(str_def), parameter :: first_key(3) &
          = [character(len=str_def) :: 'one', &
                                       'two', &
                                       'three']
  character(str_def), parameter :: second_key(3) &
          = [character(len=str_def) :: 'ay', &
                                       'bee', &
                                       'see']

contains

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_native) function first_from_key( key )

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    key_index = 1
    do
      if (trim(first_key(key_index)) == trim(key)) then
        first_from_key = key_index + twoenum_first_one - 1
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(first_key, 1)) then
          write( log_scratch_space, &
                 '("Key ''", A, "'' not recognised for twoenum first")' ) key
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function first_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_first( value )

    implicit none

    integer(i_native), intent(in) :: value

    integer(i_native) :: key_index

    key_index = value - twoenum_first_one + 1
    if (key_index < lbound(first_key, 1) &
        .or. key_index > ubound(first_key, 1)) then
      write( log_scratch_space, &
             '("Value ", I0, " is not in twoenum first")' ) value
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    key_from_first = first_key( key_index )

  end function key_from_first

  !> Gets the enumeration value from the key string.
  !>
  !> An error is reported if the key is not actually a key.
  !>
  !> @param[in] key Enumeration key.
  !>
  integer(i_native) function second_from_key( key )

    implicit none

    character(*), intent(in) :: key

    integer(i_native) :: key_index

    key_index = 1
    do
      if (trim(second_key(key_index)) == trim(key)) then
        second_from_key = key_index + twoenum_second_ay - 1
        return
      else
        key_index = key_index + 1
        if (key_index > ubound(second_key, 1)) then
          write( log_scratch_space, &
                 '("Key ''", A, "'' not recognised for twoenum second")' ) key
          call log_event( log_scratch_space, LOG_LEVEL_ERROR )
        end if
      end if
    end do

  end function second_from_key

  !> Gets the enumeration key corresponding to a particular value.
  !>
  !> An error is reported if the value is not within range.
  !>
  !> @param[in] value Enumeration value.
  !>
  character(str_def) function key_from_second( value )

    implicit none

    integer(i_native), intent(in) :: value

    integer(i_native) :: key_index

    key_index = value - twoenum_second_ay + 1
    if (key_index < lbound(second_key, 1) &
        .or. key_index > ubound(second_key, 1)) then
      write( log_scratch_space, &
             '("Value ", I0, " is not in twoenum second")' ) value
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    key_from_second = second_key( key_index )

  end function key_from_second

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] vm ESMF VM object of current run.
  !> @param [in] local_rank Rank of current ESMF process.
  !>
  subroutine read_twoenum_namelist( file_unit, vm, local_rank )
    implicit none
    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    call read_namelist( file_unit, vm, local_rank, first, second )
  end subroutine read_twoenum_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, vm, local_rank, dummy_first, dummy_second )

    use constants_mod, only : imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    integer(i_native), intent(out) :: dummy_first
    integer(i_native), intent(out) :: dummy_second

    integer(i_native) :: buffer_integer_i_native(2)

    character(str_def) :: first
    character(str_def) :: second

    namelist /twoenum/ first, &
                       second

    integer(i_native) :: condition

    first = ""
    second = ""

    if (local_rank == 0) then

      read( file_unit, nml=twoenum, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      dummy_first = first_from_key( first )
      dummy_second = second_from_key( second )

    end if

    buffer_integer_i_native(1) = dummy_first
    buffer_integer_i_native(2) = dummy_second

    call ESMF_VMBroadcast( vm, buffer_integer_i_native, 2, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_integer_i_native'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    dummy_first = buffer_integer_i_native(1)
    dummy_second = buffer_integer_i_native(2)



    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_twoenum_namelist()

    use constants_mod, only : imdi, rmdi

    implicit none


  end subroutine postprocess_twoenum_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function twoenum_is_loadable()

    implicit none

    logical :: twoenum_is_loadable

    twoenum_is_loadable = .not. namelist_loaded

  end function twoenum_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function twoenum_is_loaded()

    implicit none

    logical :: twoenum_is_loaded

    twoenum_is_loaded = namelist_loaded

  end function twoenum_is_loaded

end module twoenum_config_mod
                       '''.strip()

      outputFile = StringIO.StringIO()

      uut = description.NamelistDescription( 'twoenum' )
      uut.addParameter( 'first', 'enumeration', None,
                        enumerators=['one', 'two', 'three'] )
      uut.addParameter( 'second', 'enumeration', None,
                        enumerators=['ay', 'bee', 'see'] )
      uut.writeModule( outputFile )

      self.assertMultiLineEqual( expectedSource + '\n',
                                 outputFile.getvalue() )

    ###########################################################################
    def testModuleWriteComputed( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the teapot namelist.
!>
module teapot_config_mod

  use constants_mod, only : i_native, &
                            r_def
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMBroadcast, ESMF_SUCCESS

  implicit none

  private
  public :: read_teapot_namelist, postprocess_teapot_namelist, &
            teapot_is_loadable, teapot_is_loaded

  real(r_def), public, protected :: bar
  real(r_def), public, protected :: foo

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] vm ESMF VM object of current run.
  !> @param [in] local_rank Rank of current ESMF process.
  !>
  subroutine read_teapot_namelist( file_unit, vm, local_rank )
    implicit none
    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    call read_namelist( file_unit, vm, local_rank )
  end subroutine read_teapot_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, vm, local_rank )

    use constants_mod, only : imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank

    real(r_def) :: buffer_real_r_def(1)

    namelist /teapot/ foo

    integer(i_native) :: condition

    bar = rmdi
    foo = rmdi

    if (local_rank == 0) then

      read( file_unit, nml=teapot, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_real_r_def(1) = foo

    call ESMF_VMBroadcast( vm, buffer_real_r_def, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_real_r_def'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    foo = buffer_real_r_def(1)


    bar = foo ** 2

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_teapot_namelist()

    use constants_mod, only : imdi, rmdi

    implicit none


  end subroutine postprocess_teapot_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function teapot_is_loadable()

    implicit none

    logical :: teapot_is_loadable

    teapot_is_loadable = .not. namelist_loaded

  end function teapot_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function teapot_is_loaded()

    implicit none

    logical :: teapot_is_loaded

    teapot_is_loaded = namelist_loaded

  end function teapot_is_loaded

end module teapot_config_mod
        '''.strip()

        outputFile = StringIO.StringIO()

        uut = description.NamelistDescription( 'teapot' )
        uut.addParameter( 'foo', 'real', 'default' )
        uut.addParameter( 'bar', 'real', 'default', calculation=['foo ** 2'] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

    ###########################################################################
    def testModuleWriteConstant( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the cheese namelist.
!>
module cheese_config_mod

  use constants_mod, only : i_native, &
                            r_def
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMBroadcast, ESMF_SUCCESS

  implicit none

  private
  public :: read_cheese_namelist, postprocess_cheese_namelist, &
            cheese_is_loadable, cheese_is_loaded

  real(r_def), public, protected :: fred
  real(r_def), public, protected :: wilma

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] vm ESMF VM object of current run.
  !> @param [in] local_rank Rank of current ESMF process.
  !>
  subroutine read_cheese_namelist( file_unit, vm, local_rank )
    implicit none
    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    call read_namelist( file_unit, vm, local_rank )
  end subroutine read_cheese_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, vm, local_rank )

    use constants_mod, only : fudge, imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank

    real(r_def) :: buffer_real_r_def(1)

    namelist /cheese/ fred

    integer(i_native) :: condition

    fred = rmdi
    wilma = rmdi

    if (local_rank == 0) then

      read( file_unit, nml=cheese, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_real_r_def(1) = fred

    call ESMF_VMBroadcast( vm, buffer_real_r_def, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_real_r_def'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    fred = buffer_real_r_def(1)


    wilma = fred * FUDGE

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_cheese_namelist()

    use constants_mod, only : fudge, imdi, rmdi

    implicit none


  end subroutine postprocess_cheese_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function cheese_is_loadable()

    implicit none

    logical :: cheese_is_loadable

    cheese_is_loadable = .not. namelist_loaded

  end function cheese_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function cheese_is_loaded()

    implicit none

    logical :: cheese_is_loaded

    cheese_is_loaded = namelist_loaded

  end function cheese_is_loaded

end module cheese_config_mod
        '''.strip()

        outputFile = StringIO.StringIO()

        uut = description.NamelistDescription( 'cheese' )
        uut.addParameter( 'FUDGE', 'constant' )
        uut.addParameter( 'fred', 'real', 'default' )
        uut.addParameter( 'wilma', 'real', 'default', calculation=['fred * FUDGE'] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

    ###########################################################################
    def testModuleWriteArray( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> Manages the aerial namelist.
!>
module aerial_config_mod

  use constants_mod, only : i_def, &
                            i_native, &
                            r_def, &
                            str_def
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMBroadcast, ESMF_SUCCESS

  implicit none

  private
  public :: read_aerial_namelist, postprocess_aerial_namelist, &
            aerial_is_loadable, aerial_is_loaded

  character(str_def), public, protected :: absolute(5)
  integer(i_def), public, protected, allocatable :: inlist(:)
  integer(i_native), public, protected :: lsize
  real(r_def), public, protected, allocatable :: outlist(:)
  integer(i_def), public, protected, allocatable :: unknown(:)

  logical :: namelist_loaded = .false.

contains

  !> Populates this module from a namelist file.
  !>
  !> An error is reported if the namelist could not be read.
  !>
  !> @param [in] file_unit Unit number of the file to read from.
  !> @param [in] vm ESMF VM object of current run.
  !> @param [in] local_rank Rank of current ESMF process.
  !>
  subroutine read_aerial_namelist( file_unit, vm, local_rank )
    implicit none
    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank
    call read_namelist( file_unit, vm, local_rank )
  end subroutine read_aerial_namelist

  ! Reads the namelist file.
  !
  subroutine read_namelist( file_unit, vm, local_rank )

    use constants_mod, only : imdi, rmdi

    implicit none

    integer(i_native), intent(in) :: file_unit
    type(ESMF_VM),     intent(in) :: vm
    integer(i_native), intent(in) :: local_rank

    integer(i_native), parameter :: initial_size = 100

    integer(i_native) :: buffer_integer_i_native(1)

    namelist /aerial/ absolute, &
                      inlist, &
                      lsize, &
                      outlist, &
                      unknown

    integer(i_native) :: condition

    allocate( inlist(initial_size), stat=condition )
    if (condition /= 0) then
      write( log_scratch_space, '(A)' ) &
            'Unable to allocate temporary array for "inlist"'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    allocate( outlist(initial_size), stat=condition )
    if (condition /= 0) then
      write( log_scratch_space, '(A)' ) &
            'Unable to allocate temporary array for "outlist"'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    allocate( unknown(initial_size), stat=condition )
    if (condition /= 0) then
      write( log_scratch_space, '(A)' ) &
            'Unable to allocate temporary array for "unknown"'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    absolute = ""
    inlist = imdi
    lsize = imdi
    outlist = rmdi
    unknown = imdi

    if (local_rank == 0) then

      read( file_unit, nml=aerial, iostat=condition, iomsg=log_scratch_space )
      if (condition /= 0) then
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

    end if

    buffer_integer_i_native(1) = lsize

    call ESMF_VMBroadcast( vm, buffer_integer_i_native, 1, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'buffer_integer_i_native'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    lsize = buffer_integer_i_native(1)



    call ESMF_VMBroadcast( vm, absolute, size(absolute, 1)*str_def, 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'absolute'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, inlist, size(inlist, 1), 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'inlist'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, outlist, size(outlist, 1), 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'outlist'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    call ESMF_VMBroadcast( vm, unknown, size(unknown, 1), 0, rc=condition )
    if (condition /= ESMF_SUCCESS) then
      write(log_scratch_space, "(A)") &
          "Failed to broadcast 'unknown'"
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if

    namelist_loaded = .true.

  end subroutine read_namelist

  !> Performs any processing to be done once all namelists are loaded
  !>
  subroutine postprocess_aerial_namelist()

    use constants_mod, only : imdi, rmdi
    use wibble_mod, only : esize

    implicit none

    integer(i_native) :: condition
    integer(i_native) :: index
    integer(i_native) :: size
    integer(i_def) :: new_inlist(:)
    real(r_def) :: new_outlist(:)
    integer(i_def) :: new_unknown(:)

    size = lsize
    allocate( new_inlist(size), stat=condition )
    if (condition /= 0) then
      write(log_scratch_space, '(A)') 'Unable to allocate "inlist"'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    new_inlist(:size) = inlist(:size)
    call move_alloc( new_inlist, inlist )
    size = esize
    allocate( new_outlist(size), stat=condition )
    if (condition /= 0) then
      write(log_scratch_space, '(A)') 'Unable to allocate "outlist"'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    new_outlist(:size) = outlist(:size)
    call move_alloc( new_outlist, outlist )
    do index=ubound(unknown, 1), 1, -1
      if unknown(index) /= imdi exit
    end do
    size = index + 1
    allocate( new_unknown(size), stat=condition )
    if (condition /= 0) then
      write(log_scratch_space, '(A)') 'Unable to allocate "unknown"'
      call log_event( log_scratch_space, LOG_LEVEL_ERROR )
    end if
    new_unknown(:size) = unknown(:size)
    call move_alloc( new_unknown, unknown )

  end subroutine postprocess_aerial_namelist

  !> Can this namelist be loaded?
  !>
  !> @return True if it is possible to load the namelist.
  !>
  function aerial_is_loadable()

    implicit none

    logical :: aerial_is_loadable

    aerial_is_loadable = .not. namelist_loaded

  end function aerial_is_loadable

  !> Has this namelist been loaded?
  !>
  !> @return True if the namelist has been loaded.
  !>
  function aerial_is_loaded()

    implicit none

    logical :: aerial_is_loaded

    aerial_is_loaded = namelist_loaded

  end function aerial_is_loaded

end module aerial_config_mod
        '''.strip()

        outputFile = StringIO.StringIO()

        uut = description.NamelistDescription( 'aerial' )
        uut.addParameter( 'esize',     'use', module='wibble_mod' )
        uut.addParameter( 'lsize',     'integer', 'native' )
        uut.addParameter( 'absolute',  'string',  bounds=['5'] )
        uut.addParameter( 'inlist',    'integer', bounds=['lsize'] )
        uut.addParameter( 'outlist',   'real',    bounds=['esize'] )
        uut.addParameter( 'unknown',   'integer', bounds=[':'] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

##############################################################################
class NamelistDescriptionParserTest( unittest.TestCase ):
    ##########################################################################
    def setUp( self ):
        pass

    ##########################################################################
    def tearDown( self ):
        pass

    ##########################################################################
    def _compileDictionary( self, namelists ):
      dictionary = {}

      for namelist in namelists:
        parameters = {}
        for parameter in namelist.getParameters():
          parameters[parameter.name] = [parameter.fortranType.intrinsicType,
                                        parameter.fortranType.kind]
          if parameter.getConfigureType() == 'enumeration':
            parameters[parameter.name].extend( parameter.mapping.keys() )
          elif parameter.getConfigureType() == 'computed':
            parameters[parameter.name].append( parameter.computation )
          elif parameter.getConfigureType() == 'array':
            if isinstance( parameter.bounds[0], list ):
              bounds = parameter.bounds[0][0]
            else:
              bounds = parameter.bounds[0]
            parameters[parameter.name].append( '(' + bounds + ')' )
        dictionary[namelist.getNamelistName()] = parameters

      return dictionary

    ##########################################################################
    def testParserGoodFile( self ):
        inputFile = StringIO.StringIO('''
        ! Initial comment followed by blank line

        namelist fred
            first_thing : string ! Trailing comment
            second      : integer
            filename    : string(filename)
            choices     : enumeration[ foo, bar, baz, qux ]
        end namelist fred
        ''')

        uut = description.NamelistDescriptionParser()
        result = self._compileDictionary( uut.parseFile( inputFile ) )

        self.assertEqual( {'fred'  :
                     {'choices' : ['integer', 'i_native',
                                                  'foo', 'bar', 'baz', 'qux'],
                      'filename' : ['character', 'str_max_filename'],
                      'first_thing' : ['character', 'str_def'],
                      'second' : ['integer', 'i_def']}},
                          result )

    ##########################################################################
    def testOnlyEnumeration( self ):
        inputFile = StringIO.StringIO('''
        namelist barney
            stuff : enumeration[ one, two, three ]
        end namelist barney
        ''')

        uut = description.NamelistDescriptionParser()
        result =self._compileDictionary( uut.parseFile( inputFile ) )

        self.assertEqual( {'barney' : {'stuff' : ['integer', 'i_native', \
                                                  'one', 'two', 'three']}}, \
                          result )

    ##########################################################################
    def testMismatchedNames( self ):
        inputFile = StringIO.StringIO('''
        namelist wilma
            junk : integer
        end namelist betty
        ''')

        uut = description.NamelistDescriptionParser()
        self.assertRaises( description.NamelistDescriptionException, \
                           uut.parseFile, inputFile )

    ##########################################################################
    def testComputedFields( self ):
        inputFile = StringIO.StringIO('''
        namelist teapot
           foo : real
           bar : real(default)['foo ** 2']
           baz : real['PI * foo']
        end namelist teapot
        ''')

        uut = description.NamelistDescriptionParser()
        result =self._compileDictionary( uut.parseFile( inputFile ) )

        self.assertEqual({'teapot' : {'foo' : ['real', 'r_def'], \
                                      'bar' : ['real', 'r_def', 'foo ** 2'], \
                                      'baz' : ['real', 'r_def', 'PI * foo']}}, \
                         result)

    ##########################################################################
    def testComputedFieldsWithConstant( self ):
        inputFile = StringIO.StringIO('''
        namelist cheese
           FUDGE : constant
           fred  : real
           wilma : real['fred * FUDGE']
        end namelist cheese
        ''')

        uut = description.NamelistDescriptionParser()
        result =self._compileDictionary( uut.parseFile( inputFile ) )

        self.assertEqual({'cheese' : {'fred' : ['real', 'r_def'], \
                                      'wilma' : ['real', 'r_def', 'fred * FUDGE']}}, \
                         result)

    ##########################################################################
    def testArrayFields( self ):
        inputFile = StringIO.StringIO('''
        namelist aerial
           TABLET : use[sugar_mod]
           FUDGE : constant
           fred  : real
           wilma(FUDGE) : real
           betty(fred)  : logical
           bambam(:)    : integer
        end namelist aerial
        ''')

        uut = description.NamelistDescriptionParser()
        result =self._compileDictionary( uut.parseFile( inputFile ) )

        self.assertEqual({'aerial' : {'bambam' : ['integer', 'i_def', '(:)'],
                                      'betty' : ['logical', 'l_def', '(fred)'],
                                      'fred' : ['real', 'r_def'],
                                      'wilma' : ['real', 'r_def', '(FUDGE)']}},
                         result)

##############################################################################
if __name__ == '__main__':
    unittest.main()
