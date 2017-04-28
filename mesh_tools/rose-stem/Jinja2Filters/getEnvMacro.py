#!/usr/bin/env python
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
'''
Implements a Jinja2 filter to run a macro specified by a string.
'''
from jinja2 import contextfilter
import re
import ast

@contextfilter
def getEnvMacro(context, call):
    '''
    Takes a string and parses any instances of an env dictionary. 
    @param [inout] context Jinja2 instance to run macro against.
    @param [in]    call    Invokation string.
    @return String resulting from setting the environment.
    '''
    if call.find('(') == -1:
        macroName = call
        arguments = ''
    else:
        macroName = call[:call.index('(')]
        arguments = re.split(', *', call[call.index('(')+1:call.rindex(')')])

    normalArguments  = [argument for argument in arguments \
                        if argument.find('=') == -1]
    keywordArguments = [argument for argument in arguments \
                        if argument.find('=') != -1]

    argumentList = []
    for argument in normalArguments:
        if argument[0] == '"':
            argumentList.append( argument[1:-1] )
        else:
            argumentList.append( argument )

    argumentDictionary = {}
    for argument in keywordArguments:
        key, value = re.split(' *= *', argument)
        argumentDictionary[key] = value

    # We only do work on the 'env' dictionary
    if 'env' in argumentDictionary.keys():
        envDict=ast.literal_eval(argumentDictionary['env'])
    else:
        envDict={}

    envVariables=[]
    for key, value in envDict.items():
        envVariables.append('%s = %s' % (key, value) )
            
    values='\n'.join(envVariables)

    if arguments == '':
        return_value = None, None
    else:
        return_value = arguments[0], values

    return return_value
