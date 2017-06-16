#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################
'''
Turns namelist descriptions into namelist modules.
'''

from __future__ import print_function

from abc import ABCMeta, abstractmethod
import collections
import jinja2    as jinja
import pyparsing as parsing

import jinjamacros

##############################################################################
class NamelistDescriptionException(Exception):
  pass

##############################################################################
class FortranType(object):
  _singletonMap = {}

  @classmethod
  def instance( cls, intrinsicType, kind, writeFormat ):
    if intrinsicType not in cls._singletonMap:
      cls._singletonMap[intrinsicType] = {}

    if kind not in cls._singletonMap[intrinsicType]:
      cls._singletonMap[intrinsicType][kind] = {}

    if writeFormat not in cls._singletonMap[intrinsicType][kind]:
      cls._singletonMap[intrinsicType][kind][writeFormat] \
                                     = cls( intrinsicType, kind, writeFormat )

    return cls._singletonMap[intrinsicType][kind][writeFormat]

  def __init__( self, intrinsicType, kind, writeFormat ):
    self.intrinsicType = intrinsicType
    self.kind          = kind
    self.writeFormat   = writeFormat

  def declaration( self ):
    return self.intrinsicType + '(' + self.kind + ')'

  def label( self ):
    return self.intrinsicType + '_' + self.kind

  def __lt__( self, other ):
    return self.declaration() < other.declaration()

  def __eq__(self, other ):
    self.declaration() == other.declaration()

##############################################################################
class _Property(object):
  __metaclass__ = ABCMeta

  def __init__( self, name, fortranType ):
    self.name = name
    self.fortranType = fortranType

  def requiredKinds( self ):
    return [self.fortranType.kind]

  @abstractmethod
  def getConfigureType( self ):
    pass

##############################################################################
class _String(_Property):
  _fortranStringMap = { 'default'  : 'str_def',
                        'filename' : 'str_max_filename'}

  def __init__( self, name, length ):
    super(_String, self).__init__( name,
                                   FortranType.instance( 'character',
                                               self._fortranStringMap[length],
                                                         'A' ) )

  def getConfigureType( self ):
    return 'string'

##############################################################################
class _Enumeration(_Property):

  def __init__( self, name, keyDictionary ):
    super(_Enumeration, self).__init__( name,
                                        FortranType.instance( 'integer',
                                                              'i_native',
                                                              'I0') )

    self.mapping = keyDictionary
    self.inverseMapping = {value: key for key, value in self.mapping.iteritems()}
    self.firstKey = self.inverseMapping[min(self.inverseMapping.keys())]

  def requiredKinds( self ):
    return [self.fortranType.kind, 'str_def']

  def getConfigureType( self ):
    return 'enumeration'

##############################################################################
class _Scalar(_Property):
  _fortranKindMap = { 'logical' : {'default' : 'l_def',
                                   'native'  : 'l_native'},
                      'integer' : {'default' : 'i_def',
                                   'native'  : 'i_native',
                                   'short'   : 'i_short',
                                   'long'    : 'i_long'},
                      'real'    : {'default' : 'r_def',
                                   'native'  : 'r_native',
                                   'single'  : 'r_single',
                                   'double'  : 'r_double'} }

  _fortranFormatMap = { 'logical' : 'L2',
                        'integer' : 'I0',
                        'real'    : 'E14.7' }

  _fortranMissingDataIndicator = { 'logical' : '.false.',
                                   'integer' : 'imdi',
                                   'real'    : 'rmdi' }

  ############################################################################
  def __init__( self, name, configureType, configureKind ):
    super(_Scalar, self).__init__( name,
                                   FortranType.instance( configureType,
                          self._fortranKindMap[configureType][configureKind],
                          self._fortranFormatMap[configureType] ) )
    self._missingDataIndicator = self._fortranMissingDataIndicator[configureType]

  def getConfigureType( self ):
    return 'scalar'

  def getMissingDataIndicator( self ):
    return self._missingDataIndicator

##############################################################################
class _Computed(_Scalar):
  def __init__( self, name, configureType, configureKind, computation ):
    super(_Computed, self).__init__( name, configureType, configureKind )
    self.computation = computation[0]

  def getConfigureType( self ):
    return 'computed'

##############################################################################
class _Array(_Property):
  def __init__( self, name, contentProperty, bounds ):
    super(_Array, self).__init__( name, contentProperty.fortranType )
    self.content = contentProperty

    if len(bounds) == 1:
      if ':' in bounds[0]:
        lower, upper = bounds[0].split( ':' )
        if lower.strip() != '1' and lower.strip() != '':
          message = 'Only lower bound of 1 is allowed in configuration: {}'
          raise NamelistDescriptionException( message.format(bounds) )
      self.bounds = bounds
    else:
      message = 'Only 1D arrays allowed in configuration: {}'
      raise NamelistDescriptionException( message.format(bounds) )

  ############################################################################
  def getConfigureType( self ):
    return 'array'

  ############################################################################
  def isImmediateSize( self ):
    if self.bounds[0].isdigit():
      return True
    else:
      return False

  ############################################################################
  def isDeferredSize( self ):
    if not self.bounds[0].isdigit() and self.bounds[0] != ':':
      return True
    else:
      return False

  ############################################################################
  def isArbitrarySize( self ):
    if self.bounds[0] == ':':
      return True
    else:
      return False

##############################################################################
class NamelistDescription():
  def __init__( self, listname ):
    self._listname = listname

    self._engine = jinja.Environment(
                                   loader=jinja.PackageLoader( 'configurator',
                                                               'templates'),
                                      extensions=['jinja2.ext.do'])
    self._engine.filters['decorate'] = jinjamacros.decorateMacro

    self._parameters   = collections.OrderedDict()
    self._module_usage = collections.defaultdict( set )
    self._module_usage['constants_mod'] = set( ['imdi', 'rmdi'] )
    self._enumCounter = 100

  ##########################################################################
  def getNamelistName( self ):
    return self._listname

  ##########################################################################
  def getModuleName ( self ):
    return self._listname + '_config_mod'

  ##########################################################################
  def addParameter( self, name, configureType, configureKind='default',
                    bounds=None, calculation=None, enumerators=None,
                    module=None ):

    if configureType == 'constant':
      self._module_usage['constants_mod'].add( name.lower() )
      return

    if configureType == 'use':
      self._module_usage[module].add( name )
      return

    if configureType == 'enumeration':
      keyDict = collections.OrderedDict()
      for key in enumerators:
        keyDict[key] = self._enumCounter
        self._enumCounter += 1
      self._parameters[name] = _Enumeration( name, keyDict )
      return

    if configureKind == '':
      configureKind = 'default'

    newParameter = None
    if calculation:
      newParameter = _Computed( name,
                                configureType,
                                configureKind,
                                calculation )
    else:
      if configureType == 'string':
        newParameter = _String( name, configureKind )
      else:
        newParameter = _Scalar( name, configureType, configureKind )

    if bounds :
      self._parameters[name] = _Array( name, newParameter, bounds )
    else:
      self._parameters[name] = newParameter

  ##########################################################################
  def getParameters( self ):
    return self._parameters.values()

  ##########################################################################
  def writeModule( self, fileObject ):
    if len(self._parameters) == 0:
      message = 'Cannot write a module to load an empty namelist'
      raise NamelistDescriptionException( message )

    allKinds      = set( ['i_native'] )
    loneKindIndex = {}
    loneKindTally = collections.defaultdict( int )
    namelist      = []
    for name, parameter in self._parameters.iteritems():
      allKinds.update( parameter.requiredKinds() )
      if isinstance( parameter, _Array ):
        arraysPresent = True
      if not isinstance( parameter, _Computed ) \
          and not isinstance( parameter, _Array ):
        loneKindTally[parameter.fortranType] += 1
        loneKindIndex[name] = loneKindTally[parameter.fortranType]
      if not isinstance( parameter, _Computed ):
        namelist.append( parameter.name )

    inserts = { 'all_kinds'     : allKinds,
                'arrays'        : [parameter.name
                                    for parameter in self._parameters.values()
                                    if isinstance(parameter,_Array)],
                'enumerations'  : [parameter.name
                                    for parameter in self._parameters.values()
                                    if isinstance(parameter,_Enumeration)],
                'listname'      : self._listname,
                'lonekindindex' : loneKindIndex,
                'lonekindtally' : loneKindTally,
                'namelist'      : namelist,
                'parameters'    : self._parameters,
                'use_from'      : self._module_usage }

    template = self._engine.get_template( 'namelist.f90.jinja' )
    print( template.render( inserts ), file=fileObject )

###############################################################################
class NamelistDescriptionParser():
  '''
  Syntax of namelist description file:

  label ::= alpha[alphanum]*

  bound ::= label | number

  bounds ::= bound ^ (bound? ":" bound?)

  dimensions ::= "(" bounds ("," bounds)* ")"

  kind_keyword ::= "default" | "native"
                    | "short" | "long" | "single" | "double"

  string_keyword ::= "default" | "filename"

  kind ::= "(" kind_keyword | string_keyword ")"

  type_keyword ::= "logical" | "integer" | "real" | "string" | "enumeration"
                    | "constant" | "dimension" | "use"

  typedef ::= typekeyword kind?

  argument ::= label | """ anychar """

  argumentlist ::= "[" argument ["," argument]* "]"

  parameter ::= label dimensions? ":" typedef argumentlist?

  namelistname ::= alpha[alphanum]*

  file ::= "namelist" namelistname parameter* "end" "namelist" namelistname
  '''
  ###########################################################################
  def __init__( self ):
    self._argumentOrder = {}
    def catchNamelist( tokens ):
      name = tokens[0][0]
      self._argumentOrder[name] = []
      arguments = tokens[0][1:]
      for blob in arguments:
        self._argumentOrder[name].append( blob[0] )

    name = parsing.Forward()
    def catchName( tokens ):
      name << parsing.oneOf( tokens.asList() )

    exclam      = parsing.Literal('!')
    colon       = parsing.Literal(':')
    openParen   = parsing.Literal('(').suppress()
    closeParen  = parsing.Literal(')').suppress()
    openSquare  = parsing.Literal('[').suppress()
    closeSquare = parsing.Literal(']').suppress()
    comma       = parsing.Literal(',').suppress()

    label = parsing.Word( parsing.alphas+"_", parsing.alphanums+"_" )
    number = parsing.Word( parsing.nums )

    bound = label ^ number
    bounds = bound ^ parsing.Combine( parsing.Optional( bound )
                                      + colon
                                      + parsing.Optional( bound ) )
    dimensions =  openParen \
                  + parsing.Group( bounds
                                    + parsing.ZeroOrMore( comma
                                                          + bounds )
                                  ).setResultsName('xbounds' ) \
                  + closeParen

    typeKeywords = parsing.oneOf(
          'logical integer real string enumeration constant dimension use',
                                  caseless=True )
    kindKeywords = parsing.oneOf(
                                'default native short long single double',
                                  caseless=True )
    stringKeywords = parsing.oneOf( 'default, filename', caseless=True )
    kinddef = kindKeywords ^ stringKeywords
    typedef = typeKeywords('xtype') + parsing.Optional( openParen
                                                        - kinddef('xkind')
                                                        - closeParen )

    computation = parsing.quotedString
    computation.setParseAction(parsing.removeQuotes)
    argument = label ^ computation
    argumentList = parsing.Group( openSquare + argument \
                                  + parsing.ZeroOrMore( comma + argument)
                                  + closeSquare ).setResultsName('xargs')

    parameter = parsing.Group( label
                              + parsing.Optional( dimensions )
                              + colon
                              + typedef
                              + parsing.Optional( argumentList ) )

    nameLabel = parsing.Word( parsing.alphas+"_", parsing.alphanums+"_()" )
    nameLabel.setParseAction( catchName )

    namelistKeyword = parsing.Literal('namelist').suppress()
    endKeyword      = parsing.Literal('end').suppress()

    definitionStart = namelistKeyword + nameLabel
    definitionEnd   = endKeyword + namelistKeyword - name.suppress()

    definition = parsing.Group( definitionStart
                          + parsing.Dict( parsing.OneOrMore( parameter ) )
                                + definitionEnd )
    definition.setParseAction( catchNamelist )

    self._parser = parsing.Dict( definition )

    comment = exclam - parsing.restOfLine
    self._parser.ignore( comment )

  ###########################################################################
  def parseFile ( self, fileObject ):
    try:
      parseTree = self._parser.parseFile( fileObject )
    except (parsing.ParseException, parsing.ParseSyntaxException) as err:
      message = '\n{}\n{}\n{}'.format( err.line,
                                       " "*(err.column-1) + "^",
                                       err )
      raise NamelistDescriptionException( message )

    result = []
    for listname, variables in parseTree.items():
      description = NamelistDescription( listname )

      for name in self._argumentOrder[listname]:
        value = variables[name]

        if isinstance(value, parsing.ParseResults) :
          if value.xtype == 'enumeration':
            description.addParameter( name,
                                      value.xtype,
                                      value.xkind,
                                      bounds=value.xbounds,
                                      enumerators=value.xargs )
          elif value.xtype == 'use':
            description.addParameter( name,
                                      value.xtype,
                                      value.xkind,
                                      module=value.xargs[0] )
          else:
            if value.xbounds:
              bounds = value.xbounds.asList()
            else:
              bounds = None
            description.addParameter( name,
                                      value.xtype,
                                      value.xkind,
                                      bounds=bounds,
                                      calculation=value.xargs )
        else:
          description.addParameter( name, value )

      result.append( description )

    return result
