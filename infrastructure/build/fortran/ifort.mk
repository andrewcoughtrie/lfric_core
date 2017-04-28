##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
# Various things specific to the Intel Fortran compiler.
##############################################################################
#
# This macro is evaluated now (:= syntax) so it may be used as many times as
# desired without wasting time rerunning it.
#
IFORT_VERSION := $(shell ifort -v 2>&1 \
                         | awk -F "[. ]" '/[0-9]\.[0-9]\.[0-9]/ { printf "%03i%02i%02i", $$(NF-2),$$(NF-1),$$NF}' )

$(info ** Chosen Intel Fortran compiler version $(IFORT_VERSION))

ifeq ($(shell test $(IFORT_VERSION) -lt 0150001; echo $$?), 0)
  $(error IFort is too old to build dynamo. Must be at least 15.0.1)
endif

F_MOD_DESTINATION_ARG = -module$(SPACE)
OPENMP_ARG            = -qopenmp

FFLAGS_COMPILER           =
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -O2 -fp-model precise
FFLAGS_RISKY_OPTIMISATION = -O3 -xhost
FFLAGS_DEBUG              = -g -traceback
FFLAGS_WARNINGS           = -warn all -warn errors
FFLAGS_INIT               = -ftrapuv
FFLAGS_RUNTIME            = -check all -fpe0

# The "-assume realloc-lhs" switch causes Intel Fortran prior to v17 to
# actually implement the Fortran2003 standard. At version 17 it becomes the
# default behaviour.
ifeq ($(shell test $(IFORT_VERSION) -lt 0170000; echo $$?), 0)
  $(info ** Activating Intel "Make it work" switch for version earlier than 17)
  FFLAGS_COMPILER += -assume realloc-lhs
endif

LDFLAGS_COMPILER =
