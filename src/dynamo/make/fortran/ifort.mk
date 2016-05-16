##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
# Compiler specific details: Intel Fortran with GCC C/C++
#
$(info ** Chosen Intel Fortran compiler)

FFLAGS_COMPILER           = -qopenmp
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -O2 -fp-model precise
FFLAGS_RISKY_OPTIMISATION = -O3 -xhost
FFLAGS_DEBUG              = -g -traceback
FFLAGS_WARNINGS           = -warn all -warn errors
FFLAGS_INIT               = -ftrapuv
FFLAGS_RUNTIME            = -check all -fpe0

LDFLAGS_COMPILER = -qopenmp

ifdef CRAY_ENVIRONMENT
LD_COMPILER_LIBRARIES = stdc++
endif
