#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

from abc import ABCMeta, abstractmethod
import subprocess
import sys

class Test():
  __metaclass__ = ABCMeta

  def __init__( self, command=sys.argv[1] ):
    if type(command) is not list:
      command = [command]

    self.process = subprocess.Popen( command,
                                     stdin=subprocess.PIPE,
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE )

  def performTest( self ):
    return self.test( self.process )

  @abstractmethod
  def test( self, process ):
    pass

class MpiTest(Test):
  __metaclass__ = ABCMeta

  def __init__( self, command=sys.argv[1], cores=4 ):
    mpiCommand = ['mpiexec', '-n', str(cores)]
    if type(command) is list:
      mpiCommand.extend( command )
    else:
      mpiCommand.append( command )

    super(MpiTest, self).__init__( mpiCommand )
