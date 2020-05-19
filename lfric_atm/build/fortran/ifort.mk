##############################################################################
# (c) Crown copyright 2017 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
# Various things specific to the Intel Fortran compiler.
##############################################################################

$(info Project specials for Intel compiler)

export FFLAGS_UM_PHYSICS = -r8

$(info LFRic compile options required for files with OpenMP - see Ticket 1490)
%psy.o %psy.mod:   export FFLAGS += $(FFLAGS_INTEL_FIX_ARG)
psy/%.o psy/%.mod: export FFLAGS += $(FFLAGS_INTEL_FIX_ARG)
