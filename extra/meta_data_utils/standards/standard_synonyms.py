###############################################################################
# (c) Crown copyright 2020 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
###############################################################################
""" A list of standard synonyms for fields"""
from enum import Enum


class StandardSynonyms(str, Enum):
    """ A list of standard synonyms for fields"""
    # validated standards
    CF = "CF"
    CMIP6 = "CMIP6"
    # non validated Synonyms
    AMIP = "AMIP"
    GRIB = "GRIB"
    STASH = "STASH"
