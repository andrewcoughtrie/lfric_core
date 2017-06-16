#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################

from __future__ import print_function

import collections
import jinja2 as jinja

import jinjamacros

##############################################################################
class NamelistFeigner():
  def __init__( self, moduleName ):
    self._moduleName = moduleName

    self._engine = jinja.Environment( \
                loader=jinja.PackageLoader( 'configurator', 'templates'), \
                extensions=['jinja2.ext.do'] )
    self._engine.filters['decorate'] = jinjamacros.decorateMacro

    self._namelists = collections.OrderedDict()

  def addNamelist( self, namelists ):
    for item in namelists:
      self._namelists[item.getNamelistName()] = item

  def writeModule( self, moduleFile ):
    enumerations = collections.defaultdict( list )
    kinds = set( ['i_native'] )
    namelists = []
    parameters = {}
    for namelist in self._namelists.values():
      namelists.append( namelist.getNamelistName() )
      parameters[namelist.getNamelistName()] = []
      for param in namelist.getParameters():
        if param.getConfigureType() == 'enumeration':
          enumerations[namelist.getNamelistName()].append( param.name )
        if param.getConfigureType() != 'computed':
          parameters[namelist.getNamelistName()].append(param)
          kinds.add( param.fortranType.kind )

    inserts = { 'enumerations'  : enumerations,
                'kinds'         : kinds,
                'modulename'    : self._moduleName,
                'namelists'     : namelists,
                'parameters'    : parameters }

    template = self._engine.get_template( 'feign_config.f90.jinja' )
    print( template.render( inserts ), file=moduleFile )
