#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
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

        template = self._engine.get_template( 'loader.f90' )
        print( template.render( inserts ), file=moduleFile )
