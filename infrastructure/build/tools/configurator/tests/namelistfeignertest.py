#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################

import unittest
import StringIO

import configurator.namelistdescription as namelist
import configurator.namelistfeigner     as feigner

###############################################################################
class FeignerTest( unittest.TestCase ):
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
! Handles the feigning of namelists.
!
module empty_mod

  use constants_mod, only : i_native
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMGet, ESMF_SUCCESS

  implicit none

  private
  public :: 

  integer(i_native) :: local_rank = -1
  type(ESMF_VM)     :: vm
  integer(i_native), parameter :: temporary_unit = 3

contains

end module empty_mod
        '''.strip()

        outputFile = StringIO.StringIO()
        uut = feigner.NamelistFeigner( 'empty_mod' )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

    ###########################################################################
    def testSimple( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! Handles the feigning of namelists.
!
module simple_mod

  use constants_mod, only : i_def, i_native, l_def, r_double, str_def
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMGet, ESMF_SUCCESS

  implicit none

  private
  public :: feign_simple_config

  integer(i_native) :: local_rank = -1
  type(ESMF_VM)     :: vm
  integer(i_native), parameter :: temporary_unit = 3

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine feign_simple_config( foo, &
                                  bar, &
                                  baz, &
                                  fred )

    use simple_config_mod, only : read_simple_namelist

    implicit none

    integer(i_def), intent(in) :: foo
    real(r_double), intent(in) :: bar
    character(*), intent(in) :: baz
    logical(l_def), intent(in) :: fred

    integer(i_native) :: condition

    if (local_rank == -1) then
      call ESMF_VMGetCurrent( vm=vm, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to get VM when trying to feign feign_simple_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      call ESMF_VMGet( vm, localPet=local_rank, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to query VM when trying to feign feign_simple_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if
    end if

    open( temporary_unit, status='scratch', action='readwrite', &
          iostat=condition )
    if (condition /= 0) then
      write( 6, '("feign_simple_config: ", I0)' ) condition
      stop
    end if

    write( temporary_unit, '("&simple")' )
    write( temporary_unit, '("foo = ", I0)' ) foo
    write( temporary_unit, '("bar = ", E14.7)' ) bar
    write( temporary_unit, '("baz = ''", A, "''")' ) baz
    write( temporary_unit, '("fred = ", L2)' ) fred
    write( temporary_unit, '("/")' )

    rewind(temporary_unit)
    call read_simple_namelist( temporary_unit, vm, local_rank )

    close(temporary_unit, iostat=condition )
    if (condition /= 0) stop 'feign_simple_config: Unable to close temporary file'

  end subroutine feign_simple_config

end module simple_mod
        '''.strip()

        simple = namelist.NamelistDescription( 'simple' )
        simple.addParameter( 'foo', 'integer', 'default' )
        simple.addParameter( 'bar', 'real', 'double' )
        simple.addParameter( 'baz', 'string' )
        simple.addParameter( 'qux', 'constant' )
        simple.addParameter( 'fred', 'logical' )

        outputFile = StringIO.StringIO()
        uut = feigner.NamelistFeigner( 'simple_mod' )
        uut.addNamelist( [simple] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

    ###########################################################################
    def testEnumeration( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! Handles the feigning of namelists.
!
module enumeration_mod

  use constants_mod, only : i_native
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMGet, ESMF_SUCCESS

  implicit none

  private
  public :: feign_enum_config

  integer(i_native) :: local_rank = -1
  type(ESMF_VM)     :: vm
  integer(i_native), parameter :: temporary_unit = 3

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine feign_enum_config( thing )

    use enum_config_mod, only : read_enum_namelist, &
                                key_from_thing, &
                                thing_from_key

    implicit none

    integer(i_native), intent(in) :: thing

    integer(i_native) :: condition

    if (local_rank == -1) then
      call ESMF_VMGetCurrent( vm=vm, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to get VM when trying to feign feign_enum_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      call ESMF_VMGet( vm, localPet=local_rank, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to query VM when trying to feign feign_enum_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if
    end if

    open( temporary_unit, status='scratch', action='readwrite', &
          iostat=condition )
    if (condition /= 0) then
      write( 6, '("feign_enum_config: ", I0)' ) condition
      stop
    end if

    write( temporary_unit, '("&enum")' )
    write( temporary_unit, '("thing = ''", A, "''")' ) key_from_thing( thing )
    write( temporary_unit, '("/")' )

    rewind(temporary_unit)
    call read_enum_namelist( temporary_unit, vm, local_rank )

    close(temporary_unit, iostat=condition )
    if (condition /= 0) stop 'feign_enum_config: Unable to close temporary file'

  end subroutine feign_enum_config

end module enumeration_mod
        '''.strip()

        enumable = namelist.NamelistDescription( 'enum' )
        enumable.addParameter( 'thing', 'enumeration',
                               enumerators=['one', 'two'] )

        outputFile = StringIO.StringIO()
        uut = feigner.NamelistFeigner( 'enumeration_mod' )
        uut.addNamelist( [enumable] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

    ###########################################################################
    def testComputed( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! Handles the feigning of namelists.
!
module computed_mod

  use constants_mod, only : i_def, i_native
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMGet, ESMF_SUCCESS

  implicit none

  private
  public :: feign_computed_config

  integer(i_native) :: local_rank = -1
  type(ESMF_VM)     :: vm
  integer(i_native), parameter :: temporary_unit = 3

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine feign_computed_config( teapot, &
                                    cheese )

    use computed_config_mod, only : read_computed_namelist

    implicit none

    integer(i_def), intent(in) :: teapot
    integer(i_def), intent(in) :: cheese

    integer(i_native) :: condition

    if (local_rank == -1) then
      call ESMF_VMGetCurrent( vm=vm, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to get VM when trying to feign feign_computed_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      call ESMF_VMGet( vm, localPet=local_rank, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to query VM when trying to feign feign_computed_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if
    end if

    open( temporary_unit, status='scratch', action='readwrite', &
          iostat=condition )
    if (condition /= 0) then
      write( 6, '("feign_computed_config: ", I0)' ) condition
      stop
    end if

    write( temporary_unit, '("&computed")' )
    write( temporary_unit, '("teapot = ", I0)' ) teapot
    write( temporary_unit, '("cheese = ", I0)' ) cheese
    write( temporary_unit, '("/")' )

    rewind(temporary_unit)
    call read_computed_namelist( temporary_unit, vm, local_rank )

    close(temporary_unit, iostat=condition )
    if (condition /= 0) stop 'feign_computed_config: Unable to close temporary file'

  end subroutine feign_computed_config

end module computed_mod
        '''.strip()

        simple = namelist.NamelistDescription( 'computed' )
        simple.addParameter( 'teapot', 'integer', 'default' )
        simple.addParameter( 'cheese', 'integer', 'default' )
        simple.addParameter( 'biscuits', 'integer', 'default',
                             calculation=['teapot + cheese'] )

        outputFile = StringIO.StringIO()
        uut = feigner.NamelistFeigner( 'computed_mod' )
        uut.addNamelist( [simple] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

    ###########################################################################
    def testEverything( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! Handles the feigning of namelists.
!
module everything_mod

  use constants_mod, only : i_native, l_def, r_def, str_max_filename
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMGet, ESMF_SUCCESS

  implicit none

  private
  public :: feign_everything_config

  integer(i_native) :: local_rank = -1
  type(ESMF_VM)     :: vm
  integer(i_native), parameter :: temporary_unit = 3

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine feign_everything_config( cake, &
                                      teapot, &
                                      cheese, &
                                      fish, &
                                      tail )

    use everything_config_mod, only : read_everything_namelist, &
                                      key_from_teapot, &
                                      teapot_from_key

    implicit none

    character(*), intent(in) :: cake
    integer(i_native), intent(in) :: teapot
    logical(l_def), intent(in) :: cheese
    real(r_def), intent(in) :: fish
    integer(i_native), intent(in) :: tail

    integer(i_native) :: condition

    if (local_rank == -1) then
      call ESMF_VMGetCurrent( vm=vm, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to get VM when trying to feign feign_everything_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      call ESMF_VMGet( vm, localPet=local_rank, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to query VM when trying to feign feign_everything_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if
    end if

    open( temporary_unit, status='scratch', action='readwrite', &
          iostat=condition )
    if (condition /= 0) then
      write( 6, '("feign_everything_config: ", I0)' ) condition
      stop
    end if

    write( temporary_unit, '("&everything")' )
    write( temporary_unit, '("cake = ''", A, "''")' ) cake
    write( temporary_unit, '("teapot = ''", A, "''")' ) key_from_teapot( teapot )
    write( temporary_unit, '("cheese = ", L2)' ) cheese
    write( temporary_unit, '("fish = ", E14.7)' ) fish
    write( temporary_unit, '("tail = ", I0)' ) tail
    write( temporary_unit, '("/")' )

    rewind(temporary_unit)
    call read_everything_namelist( temporary_unit, vm, local_rank )

    close(temporary_unit, iostat=condition )
    if (condition /= 0) stop 'feign_everything_config: Unable to close temporary file'

  end subroutine feign_everything_config

end module everything_mod
        '''.strip()

        everything = namelist.NamelistDescription( 'everything' )
        everything.addParameter( 'cake', 'string', 'filename' )
        everything.addParameter( 'teapot', 'enumeration',
                                 enumerators=['brown', 'steel'] )
        everything.addParameter( 'cheese', 'logical' )
        everything.addParameter( 'fish', 'real' )
        everything.addParameter( 'wibble', 'constant' )
        everything.addParameter( 'yarn', 'real', 'default', \
                                 calculation=['fish * wibble / 180.0_r_def'] )
        everything.addParameter( 'tail', 'integer', 'native' )

        outputFile = StringIO.StringIO()
        uut = feigner.NamelistFeigner( 'everything_mod' )
        uut.addNamelist( [everything] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )

    ###########################################################################
    def testMultiFile( self ):
        expectedSource = '''
!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
! Handles the feigning of namelists.
!
module multifile_mod

  use constants_mod, only : i_native, l_def, r_def, str_max_filename
  use log_mod,       only : log_scratch_space, log_event, LOG_LEVEL_ERROR
  use ESMF,          only : ESMF_VM, ESMF_VMGetCurrent, ESMF_VMGet, ESMF_SUCCESS

  implicit none

  private
  public :: feign_first_config, &
            feign_second_config

  integer(i_native) :: local_rank = -1
  type(ESMF_VM)     :: vm
  integer(i_native), parameter :: temporary_unit = 3

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine feign_first_config( cake, &
                                 teapot, &
                                 cheese )

    use first_config_mod, only : read_first_namelist, &
                                 key_from_teapot, &
                                 teapot_from_key

    implicit none

    character(*), intent(in) :: cake
    integer(i_native), intent(in) :: teapot
    logical(l_def), intent(in) :: cheese

    integer(i_native) :: condition

    if (local_rank == -1) then
      call ESMF_VMGetCurrent( vm=vm, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to get VM when trying to feign feign_first_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      call ESMF_VMGet( vm, localPet=local_rank, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to query VM when trying to feign feign_first_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if
    end if

    open( temporary_unit, status='scratch', action='readwrite', &
          iostat=condition )
    if (condition /= 0) then
      write( 6, '("feign_first_config: ", I0)' ) condition
      stop
    end if

    write( temporary_unit, '("&first")' )
    write( temporary_unit, '("cake = ''", A, "''")' ) cake
    write( temporary_unit, '("teapot = ''", A, "''")' ) key_from_teapot( teapot )
    write( temporary_unit, '("cheese = ", L2)' ) cheese
    write( temporary_unit, '("/")' )

    rewind(temporary_unit)
    call read_first_namelist( temporary_unit, vm, local_rank )

    close(temporary_unit, iostat=condition )
    if (condition /= 0) stop 'feign_first_config: Unable to close temporary file'

  end subroutine feign_first_config

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine feign_second_config( fish, &
                                  yarn, &
                                  tail )

    use second_config_mod, only : read_second_namelist, &
                                  key_from_yarn, &
                                  yarn_from_key

    implicit none

    real(r_def), intent(in) :: fish
    integer(i_native), intent(in) :: yarn
    integer(i_native), intent(in) :: tail

    integer(i_native) :: condition

    if (local_rank == -1) then
      call ESMF_VMGetCurrent( vm=vm, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to get VM when trying to feign feign_second_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if

      call ESMF_VMGet( vm, localPet=local_rank, rc=condition )
      if (condition /= ESMF_SUCCESS) then
        write(log_scratch_space, "(A)") &
            "Failed to query VM when trying to feign feign_second_config"
        call log_event( log_scratch_space, LOG_LEVEL_ERROR )
      end if
    end if

    open( temporary_unit, status='scratch', action='readwrite', &
          iostat=condition )
    if (condition /= 0) then
      write( 6, '("feign_second_config: ", I0)' ) condition
      stop
    end if

    write( temporary_unit, '("&second")' )
    write( temporary_unit, '("fish = ", E14.7)' ) fish
    write( temporary_unit, '("yarn = ''", A, "''")' ) key_from_yarn( yarn )
    write( temporary_unit, '("tail = ", I0)' ) tail
    write( temporary_unit, '("/")' )

    rewind(temporary_unit)
    call read_second_namelist( temporary_unit, vm, local_rank )

    close(temporary_unit, iostat=condition )
    if (condition /= 0) stop 'feign_second_config: Unable to close temporary file'

  end subroutine feign_second_config

end module multifile_mod
        '''.strip()

        firstfile = namelist.NamelistDescription( 'first' )
        firstfile.addParameter( 'cake', 'string', 'filename' )
        firstfile.addParameter( 'teapot', 'enumeration',
                                enumerators=['brown', 'steel'] )
        firstfile.addParameter( 'cheese', 'logical' )

        secondfile = namelist.NamelistDescription( 'second' )
        secondfile.addParameter( 'fish', 'real' )
        secondfile.addParameter( 'yarn', 'enumeration',
                                 enumerators=['fuzzy', 'colourful'] )
        secondfile.addParameter( 'tail', 'integer', 'native' )

        outputFile = StringIO.StringIO()
        uut = feigner.NamelistFeigner( 'multifile_mod' )
        uut.addNamelist( [firstfile, secondfile] )
        uut.writeModule( outputFile )

        self.assertMultiLineEqual( expectedSource + '\n', \
                                   outputFile.getvalue() )
