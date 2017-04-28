##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
# Various things specific to the GNU Fortran compiler.
##############################################################################
#
# This macro is evaluated now (:= syntax) so it may be used as many times as
# desired without wasting time rerunning it.
#
GFORTRAN_VERSION := $(shell $(FC) -dumpversion 2>&1 \
                    | awk -F . '{ printf "%02i%02i%02i", $$1, $$2, $$3 }')
$(info ** Chosen GNU Fortran compiler version $(GFORTRAN_VERSION))

ifeq ($(shell test $(GFORTRAN_VERSION) -lt 040900; echo $$?), 0)
  $(error GFortran is too old to build dynamo. Must be at least 4.9.0)
endif

F_MOD_DESTINATION_ARG     = -J
OPENMP_ARG = -fopenmp

FFLAGS_COMPILER           = -ffree-line-length-none
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -Og
FFLAGS_RISKY_OPTIMISATION = -Ofast
FFLAGS_DEBUG              = -g
FFLAGS_WARNINGS           = -Wall
FFLAGS_INIT               = -finit-integer=31173 -finit-real=snan \
                            -finit-logical=true -finit-character=85
FFLAGS_RUNTIME            = -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow

LDFLAGS_COMPILER =
