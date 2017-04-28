#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

from __future__ import print_function
from sys import exit

from exception import TestFailed

class TestEngine:
    @staticmethod
    def run( testcase ):
        try:
            success = testcase.performTest()
            print( '[PASS] {message}'.format( message=success ) )
        except TestFailed as ex:
            exit( '[FAIL] {message}'.format( message=ex ) )
