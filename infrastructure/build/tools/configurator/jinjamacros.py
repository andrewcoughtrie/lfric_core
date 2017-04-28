#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

###############################################################################
def decorateMacro( subject, prefix=None, postfix=None ):
    result = [value for value in subject]

    if prefix:
        result = [prefix+value for value in result]

    if postfix:
        result = [value+postfix for value in result]

    return result
