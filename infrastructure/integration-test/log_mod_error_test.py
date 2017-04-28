#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

from __future__ import print_function

import datetime

from testframework import Test, TestEngine, TestFailed

class log_mod_error_test( Test ):
  def test( self, process ):
    minimumTimestamp = datetime.datetime.utcnow()
    expectedLevel = 'ERROR'
    expectedMessage = ' An error was logged.'

    out, err = process.communicate()

    if process.returncode == 0:
      raise TestFailed( 'Logging an error did not cause termination to end' )

    if out != '':
      message = 'Expected no output on standard out but found: {out}' \
                + '\nStandard error: {err}'
      raise TestFailed( message.format( out=out, err=err ) )

    try:
      timestampString, level, report = err.split( ':', 3 )
      timestampWithoutTimezone = timestampString[:-5]

      timestamp = datetime.datetime.strptime( timestampWithoutTimezone, \
                                              '%Y%m%d%H%M%S.%f' )
    except Exception as ex:
      raise TestFailed( 'Unexpected log message: {}'.format( err ) )

    if timestamp < minimumTimestamp:
      message = 'Expected a timestamp after {} but read {}'
      raise TestFailed( message.format( minimumTimestamp, timestamp ) )

    if level != expectedLevel:
      message = 'Expected "{}" but read "{}"'
      raise TestFailed( message.format( expectedLevel, level ) )

    # We only check the first line as compilers tend to print the return code
    # as well. This will remain true until we can use Fortran 2008 and
    # "stop error".
    #
    first, newline, rest = report.partition( '\n' )
    if first != expectedMessage:
      message = 'Expected "{}" but read "{}"'
      raise TestFailed( message.format( expectedMessage, first ) )

    message = 'Logging an error caused exit as expected with code {code}'
    return message.format( code=process.returncode )

if __name__ == '__main__':
  TestEngine.run( log_mod_error_test() )
