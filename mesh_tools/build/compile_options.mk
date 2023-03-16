##############################################################################
# (c) Crown copyright 2018 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################

$(info Disable warnings-turned-error caused by undeclared external functions - see ifort.mk)
%mpi_mod.o %mpi_mod.mod: private FFLAGS_EXTRA = $(FFLAGS_INTEL_EXTERNALS)
