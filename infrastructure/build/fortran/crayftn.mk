##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
# Various things specific to the Cray Fortran compiler.
##############################################################################
#
# This macro is evaluated now (:= syntax) so it may be used as many times as
# desired without wasting time rerunning it.
#
CRAYFTN_VERSION := $(shell ftn -V 2>&1 \
                     | awk -F "[. ]" '/[0-9]\.[0-9]\.[0-9]/ { printf "%03i%03i%03i", $$5,$$6,$$7}' )

$(info ** Chosen Cray Fortran compiler version $(CRAYFTN_VERSION))

ifeq ($(shell test $(CRAYFTN_VERSION) -lt 008003004; echo $$?), 0)
  $(error CrayFTN is too old. It must be at least 8.3.4)
endif

OPENMP_ARG            = -h omp

FFLAGS_COMPILER           =
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -O2
FFLAGS_RISKY_OPTIMISATION = -O3
FFLAGS_DEBUG              = -Gfast
FFLAGS_WARNINGS           = -m 0

LDFLAGS_COMPILER =

DEPRULE_FLAGS = -moduleobjects
