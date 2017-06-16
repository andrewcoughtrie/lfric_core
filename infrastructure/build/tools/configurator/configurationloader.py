#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################

from __future__ import print_function

import jinja2    as jinja

##############################################################################
class ConfigurationLoader():
    def __init__( self, moduleName ):
        self._engine = jinja.Environment( \
                   loader=jinja.PackageLoader( 'configurator', 'templates') )

        self._moduleName = moduleName
        self._namelists  = []

    def addNamelist( self, namelist ):
        self._namelists.append( namelist )

    def writeModule( self, moduleFile ):
        inserts = { 'moduleName': self._moduleName, \
                    'namelists' : self._namelists }

        template = self._engine.get_template( 'loader.f90.jinja' )
        print( template.render( inserts ), file=moduleFile )
