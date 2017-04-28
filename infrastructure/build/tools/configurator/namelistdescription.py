#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
'''
Turns namelist descriptions into namelist modules.
'''

from __future__ import print_function

import collections
import jinja2    as jinja
import pyparsing as parsing

import jinjamacros

###############################################################################
class NamelistDescriptionException(Exception):
    pass

###############################################################################
class _FortranType:
    def __init__( self, typex, kind ):
        self.typex = typex
        self.kind  = kind

###############################################################################
class _Enumeration(dict):
    def __init__( self, key, value ):
        self['key'] = key
        self['value'] = value

###############################################################################
class NamelistDescription():
    _enumerationType = 'integer'
    _enumerationKind = 'native'
    class TypeDetail:
        def __init__( self, xtype, kindMap ):
            self.xtype = xtype
            self.kindMap = kindMap

    _fortranTypeMap = { 'logical' : TypeDetail( 'logical', \
                                                {'default' : 'l_def', \
                                                 'native'  : 'l_native'} ), \
                        'integer' : TypeDetail( 'integer', \
                                                {'default' : 'i_def',    \
                                                 'native'  : 'i_native', \
                                                 'short'   : 'i_short',  \
                                                 'long'    : 'i_long'} ), \
                        'real'    : TypeDetail( 'real',    \
                                                {'default' : 'r_def',    \
                                                 'native'  : 'r_native', \
                                                 'single'  : 'r_single', \
                                                 'double'  : 'r_double'} ), \
                        'string'  : TypeDetail( 'character', \
                                                {'default'  : 'str_def',
                                           'filename' : 'str_max_filename'} ) }

    ###########################################################################
    def __init__( self, name ):
        self._engine = jinja.Environment( \
                   loader=jinja.PackageLoader( 'configurator', 'templates') )
        self._engine.filters['decorate'] = jinjamacros.decorateMacro

        self._name         = name
        self._parameters   = collections.OrderedDict()
        self._enumerations = {}
        self._logicals     = {}
        self._computed     = {}
        self._constants    = set()

    ###########################################################################
    def getName( self ):
        return self._name

    ###########################################################################
    def getModuleName ( self ):
        return self._name + '_config_mod'

    ###########################################################################
    def addParameter( self, name, xtype, kind='default', args=[] ):
        if xtype == 'constant':
            self._constants.add( name )
            return

        if kind == '':
            kind = 'default'

        if xtype == 'enumeration':
            self._enumerations[name] = args
            xtype = 'integer'
            kind  = 'native'
        elif xtype == 'logical':
            self._logicals[name] = args
        elif args:
            self._computed[name] = args

        self._parameters[name] = _FortranType( self._fortranTypeMap[xtype].xtype, \
                                               self._fortranTypeMap[xtype].kindMap[kind] )

    ###########################################################################
    def getParameters( self ):
        return self._parameters

    ###########################################################################
    def getEnumerations( self ):
        return self._enumerations

    ###########################################################################
    def getLogicals( self ):
        return self._logicals

    ###########################################################################
    def getComputations( self ):
        return self._computed

    ###########################################################################
    def writeModule( self, fileObject ):
        if len(self._parameters) + len(self._enumerations) == 0:
            message = 'Namelist description contains no variables.'
            raise NamelistDescriptionException( message )

        kindset = set(['i_native'])
        if self._enumerations:
            kindset.add( 'str_def' )
        if self._logicals:
            kindset.add( 'l_def' )

        evalue = 100
        enumerations = {}
        for name, keys in self._enumerations.iteritems():
            enumerations[name] = []
            for key in keys:
                enumerations[name].append( _Enumeration( key, evalue) )
                evalue += 1

        variables = {}
        kindcounts = collections.defaultdict(int)
        for name, fType in self._parameters.iteritems():
            kindset.add( fType.kind )
            kindcounts [ fType.kind ] += 1
            if name not in self._computed.keys():
                variables[name] = fType

        inserts = { 'listname'       : self._name,        \
                    'kindlist'       : sorted( kindset ), \
                    'kindcounts'     : kindcounts,        \
                    'enumerations'   : enumerations,      \
                    'logicals'       : self._logicals,    \
                    'parameters'     : self._parameters,  \
                    'constants'      : self._constants,   \
                    'variables'      : variables,         \
                    'initialisation' : self._computed }

        template = self._engine.get_template( 'namelist.f90' )
        print( template.render( inserts ), file=fileObject )

    ###########################################################################
    def asDict( self ):
        if len(self._parameters) + len(self._enumerations) == 0:
            return {}
        else:
            representation = {}

            for name, fType in self._parameters.iteritems():
                representation[name] = [fType.typex, fType.kind]
                if name in self._computed:
                    representation[name].extend( self._computed[name] )

            for name, identifiers in self._enumerations.iteritems():
                representation[name] = ['enumeration', None]
                representation[name].extend( identifiers )

            for name in self._constants:
                representation[name] = ['constant']

            return {self._name : representation}

###############################################################################
class NamelistDescriptionParser():
    '''
    Syntax of namelist description file:

    namelistname ::= alpha[alphanum]*
    file ::= "namelist" namelistname "end" "namelist" namelistname
    '''

    ###########################################################################
    def __init__( self ):
        exclam      = parsing.Literal('!')
        colon       = parsing.Literal(':').suppress()
        openParen   = parsing.Literal('(').suppress()
        closeParen  = parsing.Literal(')').suppress()
        openSquare  = parsing.Literal('[').suppress()
        closeSquare = parsing.Literal(']').suppress()
        comma       = parsing.Literal(',').suppress()

        namelistKeyword = parsing.Literal('namelist').suppress()
        endKeyword      = parsing.Literal('end').suppress()
        typeKeywords \
                  = parsing.oneOf( 'logical integer real string enumeration constant', \
                                   caseless=True )
        kindKeywords \
                   = parsing.oneOf('default native short long single double', \
                                   caseless=True)
        stringKeywords = parsing.oneOf('default, filename', caseless=True)

        name = parsing.Forward()
        def catchName( tokens ):
            name << parsing.oneOf(tokens.asList())
        nameLabel = parsing.Word( parsing.alphas+"_", parsing.alphanums+"_" )
        nameLabel.setParseAction( catchName )
        definitionStart = namelistKeyword + nameLabel
        definitionEnd = endKeyword + namelistKeyword - name.suppress()

        kinddef = kindKeywords ^ stringKeywords
        typedef = typeKeywords('xtype') \
                  + parsing.Optional( openParen \
                                      - kinddef('xkind') \
                                      - closeParen )

        label = parsing.Word( parsing.alphas+"_", parsing.alphanums+"_" )
        argument = label ^ parsing.quotedString.addParseAction(parsing.removeQuotes)
        argumentList = parsing.Group( openSquare + argument \
                                      + parsing.ZeroOrMore( comma + argument)
                                      + closeSquare ).setResultsName('xargs')

        variable = parsing.Group( label + colon + typedef \
                                  + parsing.Optional( argumentList ) )

        self._argumentOrder = {}
        def catchNamelist( tokens ):
            name = tokens[0][0]
            self._argumentOrder[name] = []
            arguments = tokens[0][1:]
            for blob in arguments:
                self._argumentOrder[name].append( blob[0] )
        definition = parsing.Group( definitionStart                           \
                              + parsing.Dict( parsing.OneOrMore( variable ) ) \
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
            message = '\n{}\n{}\n{}'.format( err.line, \
                                           " "*(err.column-1) + "^", \
                                           err )
            raise NamelistDescriptionException( message )

        result = []
        for name, variables in parseTree.items():
            description = NamelistDescription( name )

            for key in self._argumentOrder[name]:
                value = variables[key]
                if isinstance(value, parsing.ParseResults) :
                    description.addParameter( key,         \
                                              value.xtype, \
                                              value.xkind, \
                                              value.xargs )
                else:
                    description.addParameter( key, value )

            result.append( description )

        return result
