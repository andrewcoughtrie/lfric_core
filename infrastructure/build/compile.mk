##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
#
# Locate and compile all Fortran source in the current directory.
#
# This is done as a standalone make file so that your base makefile does not
# require the dependency analysis. This is annoying if you issue a
# "make clean" on an already clean working copy. In that case the analysis
# will be performed in order that it then be deleted.
#
# The following variables may be specified to modify the compile process...
#
# ROOT: Project directory
# BIN_DIR: Path to directory for resulting executables.
#          Default: $(ROOT)/bin
# EXTERNAL_LIBRARIES: Libraries required by link stage. These should be
#                     specified in static link order, even if you are linking
#                     dynamically.
# LINK_TYPE: 'static' or 'dynamic' linking.
#            Default: dynamic, except on Crays where it's static
# PROGRAMS: Names of programs to compile.
#           Default: Everything listed in programs.mk
# PRE_PROCESS_MACROS: Macro definitions in the form NAME[=MACRO] to be passed
#                     to the compiler.
#
##############################################################################

.SECONDEXPANSION:

BIN_DIR   ?= $(ROOT)/bin
PROGRAMS  ?= $(basename $(notdir $(PROG_OBJS)))

# If the compiler produces module files, tell it where to put them
#
ifdef F_MOD_DESTINATION_ARG
  MODULE_DESTINATION_ARGUMENT = $(F_MOD_DESTINATION_ARG)$(dir $@)
endif

# Build a set of "-I" arguments to seach the whole object tree:
INCLUDE_ARGS := $(subst ./,-I,$(shell find . -mindepth 1 -type d -print))

# Build a set of "-D" argument for any pre-processor macros
#
MACRO_ARGS := $(addprefix -D,$(PRE_PROCESS_MACROS))

include programs.mk

# Convert the program names to all caps, append "_OBJS" to the end and
# dereference that to get a list of all objects needed by all programs:
#
ALL_OBJECTS = $(foreach proj, $(shell echo $(PROGRAMS) | tr a-z A-Z), $($(proj)_OBJS))

##############################################################################
.PHONY: applications
applications: $(addprefix $(BIN_DIR)/,$(PROGRAMS))

$(BIN_DIR)/%: %.x | $(BIN_DIR)
	$(call MESSAGE,Installing,$@)
	$(Q)cp $< $@

.PRECIOUS: %.x
%.x: $$($$(shell echo $$* | tr a-z A-Z)_OBJS)
	$(call MESSAGE,Linking,$@)
	$(Q)$(LDMPI) $(LDFLAGS) -o $@ \
	             $(patsubst %,-l%,$(EXTERNAL_DYNAMIC_LIBRARIES)) \
	             $^ \
	             $(patsubst %,-l%,$(EXTERNAL_STATIC_LIBRARIES))

.PRECIOUS: %.o %.mod
%.o %.mod: %.f90
	$(call MESSAGE,Compile,$<)
	$(Q)$(FC) $(FFLAGS) \
	          $(MODULE_DESTINATION_ARGUMENT) \
	          $(INCLUDE_ARGS) -c -o $(basename $@).o $<

%.o %.mod: %.F90
	$(call MESSAGE,Pre-process and compile,$<)
	$(Q)$(FC) $(FPPFLAGS) $(FFLAGS) \
	          $(MODULE_DESTINATION_ARGUMENT) \
	          $(INCLUDE_ARGS) $(MACRO_ARGS) -c -o $(basename $@).o $<

#############################################################################
# Directories
#
$(BIN_DIR):
	$(call MESSAGE,Creating,$@)
	$(Q)mkdir -p $@

#############################################################################
# Dependencies
#
include dependencies.mk

##############################################################################
include $(LFRIC_BUILD)/lfric.mk

ifdef CRAY_ENVIRONMENT
  $(warning Running on a Cray, selecting static linking)
  LINK_TYPE ?= static
else
  LINK_TYPE ?= dynamic
endif

# Work out what to do with external libraries.
#
ifeq "$(LINK_TYPE)" "static"
  EXTERNAL_STATIC_LIBRARIES := $(EXTERNAL_DYNAMIC_LIBRARIES) $(EXTERNAL_STATIC_LIBRARIES)
else ifeq "$(LINK_TYPE)" "dynamic"
  # Nothing further needs to be done.
else
  $(error Unrecognised LINK_TYPE. Must be either "static" or "dynamic")
endif
