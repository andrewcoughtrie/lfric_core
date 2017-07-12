#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################
# Generate a make file snippet holding build information about a source file.
#
# This snippet consists of a dependency list built up from which modules a
# file "use"s.
#
# Additionally a separate snippet file is created for each "program" unit found
# listing all the modules which go to make that program.
#
# These snippets may then be "include"ed into other make files.

from __future__ import print_function;

from abc import ABCMeta, abstractmethod
import os
import os.path
import re
import shutil
import subprocess

'''
Examine Fortran source and build dependency information for use by "make".
'''

###############################################################################
# Interface for analysers
#
class Analyser(object):
    __metaclass__ = ABCMeta

    ###########################################################################
    # Examine a source file and store dependency information in the database.
    #
    # Arguments:
    #   sourceFilename - The name of the object to scan.
    #
    @abstractmethod
    def analyse( self, sourceFilename ):
        pass

###############################################################################
# Examine a namelist description file for dependencies.
#
class NamelistDescriptionAnalyser(Analyser):
    ###########################################################################
    # Constructor
    # Arguments:
    #   logger   - A logger object to write output to.
    #   database - FileDependencies object to hold details.
    #
    def __init__( self, logger, database ):
        self._logger   = logger
        self._database = database

    ###########################################################################
    # Scan a namelist description file and harvest dependency information.
    #
    # Arguments:
    #   sourceFilename - File object to scan.
    #
    def analyse( self, sourceFilename ):
        if not sourceFilename.endswith( '.nld' ):
            raise Exception( 'File doesn''t look like a namelist description' \
                             + ' file: ' + sourceFilename )

        self._logger.logEvent( '  Scanning ' + sourceFilename )
        self._database.removeFile( sourceFilename )

        with open( sourceFilename, 'r' ) as sourceFile:
            for line in sourceFile:
                match = re.match( r'^\s*namelist\s+(\S+)', line, \
                                  flags=re.IGNORECASE )
                if match is not None:
                    fortranFilename = '{}_configuration_mod.f90' \
                                      .format( match.group( 1 ) )
                    self._database.addFileDependency( fortranFilename, \
                                                      sourceFilename )

###############################################################################
# Examine Fortran source code for module dependencies.
#
class FortranAnalyser(Analyser):
  ###########################################################################
  # Constructor
  #
  # Arguments:
  #   logger        - A Logger object to write output to.
  #   ignoreModules - A list of module names to ignore.
  #   database      - FortranDatabase object to hold details.
  #
  def __init__( self, logger, ignoreModules, database ):
    self._logger          = logger
    self._ignoreModules   = map(str.lower, ignoreModules)
    self._database        = database

    self._ignoreModules.append( 'iso_fortran_env' )
    self._ignoreModules.append( 'omp_lib' )

    self._fpp = os.getenv( 'FPP', None )
    if not self._fpp:
        raise Exception( 'No Fortran preprocessor provided in $FPP' )
    self._fpp = self._fpp.split()

    self._programPattern = re.compile( r'^\s*PROGRAM\s+(\S+)',
                                       flags=re.IGNORECASE )
    self._modulePattern \
  = re.compile( r'^\s*MODULE\s+(?!(?:PROCEDURE|SUBROUTINE|FUNCTION)\s+)(\S+)',
                flags=re.IGNORECASE )
    self._submodulePattern \
            = re.compile( r'^\s*SUBMODULE\s+\((?:([^:]+):)?([^)]+)\)\s+(\S+)',
                          flags=re.IGNORECASE )
    self._usePattern = re.compile( r'^\s*USE\s+([^,\s]+)',
                                   flags=re.IGNORECASE )
    self._pFUnitPattern = re.compile( r'^\s*#\s+\d+\s+".*testSuites.inc"',
                                      flags=re.IGNORECASE )
    self._suitePattern  = re.compile( r'^\s*ADD_TEST_SUITE\(\s*(\S+)\)' )

    self._subroutinePattern = re.compile(r'^\s*SUBROUTINE\s+([^\(\s]+)', \
                                             flags=re.IGNORECASE)
    self._dependsPattern = re.compile( r'^\s*!\s*DEPENDS ON:\s+([^,\s]+)',  \
                                         flags=re.IGNORECASE)
    self._interfacePattern = re.compile( r'^\s*INTERFACE\s*', flags=re.IGNORECASE)
    self._containsPattern = re.compile(r'^\s*CONTAINS\s*', flags=re.IGNORECASE)
  ###########################################################################
  # Scan a Fortran source file and harvest dependency information.
  #
  # Arguments:
  #   sourceFilename(str) - The name of the object to scan.
  #   preprocessMacros(dict) - Macro name is the key. Value may be None for
  #                            empty macros.
  #
  def analyse( self, sourceFilename, preprocessMacros=None ):
    if sourceFilename.endswith( '.F90' ):
      self._logger.logEvent( '  Preprocessing ' + sourceFilename )
      preprocessCommand = self._fpp
      if preprocessMacros:
        for name, macro in preprocessMacros.iteritems():
          if macro:
            preprocessCommand.append( '-D{}={}'.format( name, macro ) )
          else:
            preprocessCommand.append( '-D{}'.format( name ) )
      preprocessCommand.append( sourceFilename )
      preprocessor = subprocess.Popen( preprocessCommand, \
                                       stdout=subprocess.PIPE, \
                                       stderr=subprocess.PIPE )
      processedSource, errors = preprocessor.communicate()
      if preprocessor.returncode:
        raise subprocess.CalledProcessError( preprocessor.returncode,
                                             " ".join( preprocessCommand ) )
    elif sourceFilename.endswith( '.f90' ):
      with open( sourceFilename, 'r' ) as sourceFile:
        processedSource = sourceFile.read()
    else:
      message = 'File doesn''t look like a Fortran file: {}'
      raise Exception( message.format( sourceFilename ) )

    self._logger.logEvent( '  Scanning ' + sourceFilename )
    self._database.removeFile( sourceFilename )

    def addDependency( programUnit, prerequisiteUnit, reverseLink=False ):
      prerequisiteUnit = prerequisiteUnit.lower()
      if prerequisiteUnit in self._ignoreModules:
        self._logger.logEvent( '      - Ignored 3rd party prerequisite' )
      elif prerequisiteUnit in modules:
        self._logger.logEvent( '      - Ignored self' )
      elif prerequisiteUnit in dependencies:
        self._logger.logEvent( '      - Ignore duplicate prerequisite' )
      else:
        dependencies.append( prerequisiteUnit )
        self._database.addModuleCompileDependency( programUnit, \
                                                   prerequisiteUnit )
        if reverseLink:
          self._database.addModuleLinkDependency( prerequisiteUnit, \
                                                  programUnit )
        else:
          self._database.addModuleLinkDependency( programUnit, \
                                                  prerequisiteUnit )

    programUnit = None
    modules = []
    dependencies = []
    lookForSubroutines=True
    pFUnitDriver = False
    for line in processedSource.splitlines():
      match = self._programPattern.match( line )
      if match is not None:
        programUnit = match.group(1).lower()
        self._logger.logEvent( '    Contains program: ' + programUnit )
        self._database.addProgram( programUnit, sourceFilename )
        continue

      match = self._modulePattern.match( line )
      if match is not None:
        programUnit = match.group( 1 ).lower()
        self._logger.logEvent( '    Contains module ' + programUnit )
        modules.append( programUnit )
        self._database.addModule( programUnit, sourceFilename )
        continue

      if lookForSubroutines:
          match = self._subroutinePattern.match( line )
          if match is not None:
              programUnit = match.group( 1 ).lower()
              self._logger.logEvent( '    Contains subroutine ' + programUnit )
              modules.append( programUnit )
              self._database.addModule( programUnit, sourceFilename )
              continue

      match = self._submodulePattern.match( line )
      if match is not None:
        ancestorUnit = match.group(1)
        if ancestorUnit:
          ancestorUnit = ancestorUnit.lower()
        parentUnit  = match.group(2).lower()
        programUnit = match.group(3).lower()

        message = '{}Contains submodule {} of {}'.format( ' ' * 4,     \
                                                          programUnit, \
                                                          parentUnit )
        if ancestorUnit:
          message = message + '({})'.format( ancestorUnit )
        self._logger.logEvent( message )

        self._database.addModule( programUnit, sourceFilename )
        # I don't think it's necessary to append this to "modules".
        # It's really just a dependency.
        addDependency( programUnit, parentUnit, True )
        continue

      match = self._usePattern.match( line )
      if match is not None:
        moduleName = match.group( 1 ).lower()
        self._logger.logEvent( '    Depends on module ' + moduleName )
        addDependency( programUnit, moduleName )
        continue

      match = self._pFUnitPattern.match( line )
      if not pFUnitDriver and match is not None:
        self._logger.logEvent( '    Is driver' )
        pFUnitDriver = True

        includeFilename = os.path.join( os.path.dirname( sourceFilename ),
                                        'testSuites.inc' )
        with open( includeFilename, 'r' ) as includeFile:
          for line in includeFile:
            match = self._suitePattern.match( line )
            if match is not None:
              testGeneratorFunction = match.group( 1 )
              testModule = testGeneratorFunction.replace( '_suite', '' )
              self._logger.logEvent( '      Depends on module ' + testModule )
              self._database.addModuleCompileDependency( programUnit, testModule )
              self._database.addModuleLinkDependency( programUnit, testModule )
        continue

      match = self._dependsPattern.match(line)
      if match is not None:
          name = match.group( 1 ).lower()
          if name is not None:
              self._logger.logEvent( '    %s depends on call to %s ' % (programUnit, name) )
          addDependency( programUnit, name )
          continue

      # Are we entering an interface block (in which case we may subsequently want to 
      # ignore matches of subroutine above)
      match = self._interfacePattern.match(line)
      if match is not None:
          lookForSubroutines=False
          continue

      # Are we entering module contains, (in which case we may subsequently want to 
      # ignore matches of subroutine above)
      match = self._containsPattern.match(line)
      if match is not None:
          lookForSubroutines=False
          continue
