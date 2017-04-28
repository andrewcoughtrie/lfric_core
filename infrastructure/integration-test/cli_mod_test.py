#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

from __future__ import print_function

import sys

from testframework import Test, TestEngine, TestFailed

class cli_mod_normal_test(Test):
  def __init__( self ):
    self._INJECT = 'onwards/waffles.nml'
    super(cli_mod_normal_test, self).__init__( [sys.argv[1], self._INJECT] )

  def test( self, process ):
    out, err = process.communicate( )

    if process.returncode != 0:
      raise TestFailed( 'Unexpected failure of test executable: {code}' \
                        .format( code=process.returncode ) )

    if out.strip() != self._INJECT:
      raise TestFailed( 'Expected filename "{expected}" but found "{found}"'\
                        .format( expected=self._INJECT, found=out.strip() ) )

    return 'Filename extracted from command line'

class cli_mod_too_few_test(Test):
  def test( self, process ):
    out, err = process.communicate()

    if process.returncode == 0:
      raise TestFailed( 'Unexpected success with no arguments' )

    return 'Command line with no arguments returned with error'

class cli_mod_too_many_test(Test):
  def __init__( self ):
    super(cli_mod_too_many_test, self).__init__( [sys.argv[1],
                                                  'onwards/waffles.nml', '2'] )

  def test( self, process ):
    out, err = process.communicate()

    if process.returncode == 0:
      raise TestFailed( 'Unexpected success with 2 arguments' )

    return 'Command line with 2 arguments returned with error'

class cli_mod_help_test(Test):
  def __init__( self ):
    super(cli_mod_help_test, self).__init__( [sys.argv[1], '-help'] )

  def test( self, process ):
    out, err = process.communicate()

    if process.returncode == 0:
      raise TestFailed( 'Unexpected success with "-help"' )

    return 'Command line with "-help" returned an error'

class cli_mod_h_test(Test):
  def __init__( self ):
    super(cli_mod_h_test, self).__init__( [sys.argv[1], '-h'] )

  def test( self, process ):
    out, err = process.communicate()

    if process.returncode == 0:
      raise TestFailed( 'Unexpected success with "-h"' )

    return 'Command line with "-h" returned an error'

class cli_mod_other_help_test(Test):
  def __init__( self ):
    super(cli_mod_other_help_test, self).__init__( [sys.argv[1], '--help'] )

  def test( self, process ):
    out, err = process.communicate()

    if process.returncode == 0:
      raise TestFailed( 'Unexpected success with "--help"' )

    return 'Command line with "--help" returned an error'

if __name__ == '__main__':
  TestEngine.run( cli_mod_normal_test() )
  TestEngine.run( cli_mod_too_few_test() )
  TestEngine.run( cli_mod_too_many_test() )
  TestEngine.run( cli_mod_help_test() )
  TestEngine.run( cli_mod_h_test() )
  TestEngine.run( cli_mod_other_help_test() )
