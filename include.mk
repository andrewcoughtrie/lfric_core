##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
# Variables defined here will will be included in all Makefiles for this
# project.
# Expects ROOT to point to the root of the project tree.

ROOT ?= .
BUILD_DIR = $(ROOT)/build
TOOL_DIR = $(ROOT)/tools

$(info Make version $(MAKE_VERSION))
ifeq ($(filter else-if,$(value .FEATURES)),)
  $(error The build system requires else-if support from GMake)
endif

ifdef PE_ENV
    # This may not be valid but we are going to assume that this variable
    # indicates a Cray XC-30.
    ifeq '$(PE_ENV)' 'CRAY'
        FC = crayftn
    else ifeq '$(PE_ENV)' 'INTEL'
        FC = ifort
    else ifeq '$(PE_ENV)' 'GNU'
        FC = gfortran
    else
        $(error Unrecognised Cray programming environment)
    endif
endif

ifndef CPP
	CPP = cpp
endif

ifndef FC
  $(info No compiler specified in $$FC)
  OS = $(shell uname -s)
  MACHINE = $(shell uname -m)

  ifeq '$(OS)' 'Linux'
    ifeq '$(MACHINE)' 'x86_64'
      FC = ifort
    else
      $(error Unrecognised processor: $(MACHINE))
    endif
  else ifeq '$(OS)' 'AIX'
    FC = xlf2003_r
  else ifeq '$(OS)' 'Darwin'
    FC = gfortran
  else
    $(error Unrecognised operating system: $(OS))
  endif
endif

EMPTY :=
SPACE := $(EMPTY) # Comment to highlight space character.

ifdef VERBOSE
	Q :=
else
	Q = @
endif

-include $(ESMFMKFILE)

FLDFLAGS = $(LDFLAGS)

COMPILER_NAME = $(shell basename $(FC))
ifeq '$(COMPILER_NAME)' 'ifort'
  $(info ** Chosen Intel Fortran compiler)

  FFLAGS_COMPILER       = -xhost $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS)
  FFLAGS_OPTIMISATION   = -O0
  FFLAGS_DEBUG          = -g -traceback
  FFLAGS_WARNINGS       = -warn all  -warn errors
  FFLAGS_INIT           = -ftrapuv
  FFLAGS_RUNTIME        = -check all -fpe0
  FFLAGS_TESTS          = -assume realloc_lhs
  F_MOD_DESTINATION_ARG = -module$(SPACE)

  FLINK = mpif90
  FLDFLAGS += $(FFLAGS_COMPILER) $(FFLAGS_DEBUG)

  IFORT_VERSION        := $(shell ifort -v 2>&1 \
                          | awk -F "[. ]" '/[0-9]\.[0-9]\.[0-9]/ { printf "%03i%02i%02i", $$(NF-2),$$(NF-1),$$NF}' )
  $(info ** Version $(IFORT_VERSION))

else ifeq ($(findstring xlf,$(COMPILER_NAME) ), xlf)
  $(info ** Chosen IBM XL Fortran compiler)

  FFLAGS_OPTIMISATION   = -O0
  FFLAGS_DEBUG          = -g
  FFLAGS_WARNINGS       = -qinfo=all -qhalt=w
  FFLAGS_INIT           = -qinitalloc=ffffffff -qinitauto=ffffffff
  FFLAGS_RUNTIME        = -qddim -qstackprotect=all -qfloat=nans -qflttrap=enable:invalid:nanq:overflow:underflow:zerodivide
  F_MOD_DESTINATION_ARG = -qmoddir=

else ifeq '$(COMPILER_NAME)' 'gfortran'
  $(info ** Chosen GNU Fortran compiler)

  FFLAGS_COMPILER       = $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS)
  FFLAGS_OPTIMISATION   = -O0 
  FFLAGS_DEBUG          = -g
  FFLAGS_WARNINGS       = -Wall  -Werror
  FFLAGS_INIT           = -finit-integer=31173 -finit-real=snan \
                          -finit-logical=true -finit-character=85
  FFLAGS_RUNTIME        = -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow
  F_MOD_DESTINATION_ARG = -J

  FLINK = mpif90
  FLDFLAGS += $(FFLAGS_DEBUG)

  GFORTRAN_VERSION     := $(shell gfortran -dumpversion 2>&1 \
                          | awk -F . '{ printf "%02i%02i%02i", $$1, $$2, $$3 }')
  $(info ** Version $(GFORTRAN_VERSION))

else ifeq '$(COMPILER_NAME)' 'nagfor'
  $(info ** Chosen NAG Fortran compiler)

  FFLAGS_OPTIMISATION   = -O0
  FFLAGS_DEBUG          = -g -gline -colour
  FFLAGS_INIT           = -nan
  FFLAGS_RUNTIME        = -C=all -C=undefined -mtrace=all -nan
  F_MOD_DESTINATION_ARG = -mdir$(SPACE)

else ifeq '$(COMPILER_NAME)' 'pgfortran'
  $(info ** Chosen Portland Fortran compiler)

  FFLAGS_OPTIMISATION   = -O0
  FFLAGS_DEBUG          = -g -traceback
  FFLAGS_RUNTIME        = -Mbounds -Mchkptr -Mchkstk
  F_MOD_DESTINATION_ARG = -module$(SPACE)

else ifeq '$(COMPILER_NAME)' 'crayftn'
  $(info ** Chosen Cray Fortran compiler)

  FFLAGS_COMPILER       = -em $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS)
  FFLAGS_OPTIMISATION   = -O0
  FFLAGS_DEBUG          = -g
  FFLAGS_WARNINGS       = -m 0
  F_MOD_DESTINATION_ARG = -J$(SPACE)

  FLINK = CC
  FLDFLAGS += $(FFLAGS_DEBUG)

else
  $(error Unrecognised Fortran compiler $(FC))

endif

# Once we've set everything up we need to repair FC on Cray systems
ifdef PE_ENV
  FC = ftn
endif

FFLAGS += $(FFLAGS_COMPILER) $(FFLAGS_OPTIMISATION) $(FFLAGS_DEBUG) \
          $(FFLAGS_WARNINGS) $(FFLAGS_INIT) $(FFLAGS_RUNTIME)
