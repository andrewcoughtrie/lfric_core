#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
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

import os
import os.path
import re
import shutil
import subprocess
import sys
import tempfile

'''
Examine Fortran source and build dependency information for use by "make".
'''

###############################################################################
# Examine Fortran source code for module dependencies.
#
class Analyser():
    ###########################################################################
    # Constructor.
    #
    # Arguments:
    #   logger        - A Logger object to write output to.
    #   ignoreModules - A list of module names to ignore.
    #   database      - The database object holding dependency information.
    #
    def __init__( self, logger, ignoreModules, database ):
        self._logger          = logger
        self._ignoreModules   = ignoreModules
        self._database        = database

        self._fpp = os.getenv( 'FPP', None )
        if not self._fpp:
            raise Exception( 'No Fortran preprocessor provided in $FPP' )
        self._fpp = self._fpp.split()

        self._temporaryDirectory = tempfile.mkdtemp()

    ###########################################################################
    # Destructor.
    #
    def __del__( self ):
        if self._temporaryDirectory:
            shutil.rmtree( self._temporaryDirectory )

    ###########################################################################
    # Scan the source tree.
    #
    # Arguments:
    #   sourceFilename - The name of the object to scan.
    #
    def processSource( self, sourceFilename ):
        if not re.match( '.+\.[Ff]90$', sourceFilename ):
            raise Exception( 'File doesn''t look like a Fortran file: ' \
                            + sourceFilename )

        temporaryDirectory = None
        if sourceFilename.endswith( '.F90' ):
            self._logger.logEvent( '  Preprocessing ' + sourceFilename )
            processedSourceFile = os.path.join( self._temporaryDirectory, \
                                                'processed.f90' )
            commandLine = list(self._fpp)
            commandLine.extend( [sourceFilename, processedSourceFile] )
            subprocess.check_call( commandLine )
        else:
            processedSourceFile = sourceFilename

        self._logger.logEvent( '  Scanning ' + sourceFilename )

        self._database.removeSourceFile( sourceFilename )

        programUnit = None
        modules = []
        dependencies = []
        with open( processedSourceFile, 'r' ) as sourceFile:
            for line in sourceFile:
                match = re.match( r'^\s*PROGRAM\s+(\S+)', line, \
                                  flags=re.IGNORECASE)
                if match is not None:
                    programUnit = match.group( 1 )
                    self._logger.logEvent( '    Contains program: ' + programUnit )
                    self._database.addProgram( programUnit, sourceFilename )

                match = re.match( r'^\s*MODULE\s+(?!PROCEDURE)(\S+)', line, \
                                  flags=re.IGNORECASE)
                if match is not None:
                    programUnit = match.group( 1 )
                    self._logger.logEvent( '    Contains module ' + programUnit )
                    modules.append( programUnit )
                    self._database.addModule( programUnit, sourceFilename )

                match = re.match( r'^\s*USE\s+([^,\s]+)', line, \
                                  flags=re.IGNORECASE)
                if match is not None:
                    moduleName = match.group( 1 )
                    self._logger.logEvent( '    Depends on module ' + moduleName )
                    if moduleName in self._ignoreModules :
                        self._logger.logEvent( '      - Ignored 3rd party module' )
                    elif moduleName in modules:
                        self._logger.logEvent( '      - Ignored self' )
                    else :
                        if moduleName in dependencies:
                            self._logger.logEvent( '      - Ignoring duplicate module' )
                        else:
                            dependencies.append( moduleName )
                            self._database.addDependency( programUnit, moduleName )
