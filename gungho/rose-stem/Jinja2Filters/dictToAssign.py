#!/usr/bin/env python
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################
'''
Implements a Jinja2 filter to run a macro specified by a string.
'''
from jinja2 import contextfilter

def dictToAssign(inDict, indent):
    '''
    Takes a dictionary and returns a string of assigments k=v 
    @param [in]    inDict    Dictionary
    @return String resulting from setting the environment.
    '''
    envVariables=[]
    for key, value in inDict.items():
        if not isinstance(value, dict):
            envVariables.append('%s = %s' % (key, value) )

    joining_str = '\n' + indent
    return joining_str.join(envVariables)
