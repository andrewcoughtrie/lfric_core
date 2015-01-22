##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

include ../../include.mk

AR ?= ar

BIN_DIR  = $(ROOT)/bin
DATABASE = $(OBJ_DIR)/dependencies.db

include $(OBJ_DIR)/programs.mk $(OBJ_DIR)/dependencies.mk

# Extract the program names from the program objects:
PROGRAMS = $(patsubst %.o,%,$(notdir $(PROG_OBJS)))

# Convert the program names to all caps, append "_OBJS" to the end and
# dereference that to get a list of all objects needed by all programs:
ALL_OBJS = $(foreach proj, $(shell echo $(PROGRAMS) | tr a-z A-Z), $($(proj)_OBJS))

# Compile a list of all possible objects by substituting .[Ff]90 with .o on all
# source files:
ALL_POSSIBLE_OBJS = $(patsubst ./%, $(OBJ_DIR)/%.o, $(basename $(shell find . -name "[^.]*.[Ff]90")))

# Find any objects which could be built but aren't used:
UNUSED_OBJS = $(filter-out $(ALL_OBJS), $(ALL_POSSIBLE_OBJS))

# If no programs objects were found then the dependency analysis probably
# hasn't happened yet
ifneq ($(words $(ALL_OBJS)), 0)
  ifneq ($(words $(UNUSED_OBJS)), 0)
    $(error Found the following unused source files: $(patsubst $(OBJ_DIR)/%.o, %.[Ff]90, $(UNUSED_OBJS)))
  endif
endif

.SECONDEXPANSION:

.PHONY: applications
applications: $(patsubst %,$(BIN_DIR)/%,$(PROGRAMS))

.PHONY: modules
modules: $(OBJ_DIR)/modules.a

$(BIN_DIR)/%: $(OBJ_DIR)/%.x | $(BIN_DIR)
	@echo "Installing $@"
	$(Q)cp $(OBJ_DIR)/$(notdir $@).x $@

# Directories

# Find all the subdirectories within the source directory:
SUBDIRS = $(shell find * -type d -prune)

# Mirror the source directory tree in the object directory:
OBJ_SUBDIRS = $(patsubst %,$(OBJ_DIR)/%/,$(SUBDIRS))

$(BIN_DIR):
	@echo "Creating $@"
	$(Q)mkdir -p $@

$(OBJ_DIR) $(OBJ_SUBDIRS):
	@echo "Creating $@"
	$(Q)mkdir -p $@

# Build Rules

# Build a set of "-I" arguments to seach the whole object tree:
INCLUDE_ARGS = -I$(OBJ_DIR) $(patsubst %, -I$(OBJ_DIR)/%, $(SUBDIRS))

# Build set of "-ignore" arguments for each 3rd party depenendnecy:
IGNORE_DEPENDENCIES := $(patsubst %,-ignore %,$(IGNORE_DEPENDENCIES))

$(OBJ_DIR)/%.o $(OBJ_DIR)/%.mod: %.F90 | $(OBJ_DIR)
	@echo "Compile $<"
	$(Q)$(FC) $(CPPFLAGS) $(FFLAGS) \
	          $(F_MOD_DESTINATION_ARG)$(OBJ_DIR)/$(dir $@) \
	          $(INCLUDE_ARGS) -c -o $(basename $@).o $<

$(OBJ_DIR)/%.o $(OBJ_DIR)/%.mod: %.f90 | $(OBJ_DIR)
	@echo "Compile $<"
	$(Q)$(FC) $(CPPFLAGS) $(FFLAGS) \
	          $(F_MOD_DESTINATION_ARG)$(OBJ_DIR)/$(dir $@) \
	          $(INCLUDE_ARGS) -c -o $(basename $@).o $<

$(OBJ_DIR)/modules.a: $(ALL_OBJS)
	$(Q)$(AR) -r $@ $^

$(OBJ_DIR)/%.x: $$($$(shell echo $$* | tr a-z A-Z)_OBJS)
	@echo "Linking $@"
	$(Q)$(FLINK) $(FLDFLAGS) -o $@ \
	             $(patsubst %,-l%,$(EXTERNAL_DYNAMIC_LIBRARIES)) \
	             $^ \
	             $(patsubst %,-l%,$(EXTERNAL_STATIC_LIBRARIES))

# Dependencies

# Create a list of dependency analysis flag files by substituting .o with .t:
TOUCH_FILES = $(addsuffix .t, $(basename $(ALL_POSSIBLE_OBJS)))

$(OBJ_DIR)/programs.mk: | $(OBJ_DIR)
	$(TOOL_DIR)/ProgramObjects -database $(DATABASE) $@

$(OBJ_DIR)/dependencies.mk: $(TOUCH_FILES) | $(OBJ_DIR) $(OBJ_SUBDIRS)
	$(Q)$(TOOL_DIR)/DependencyRules -database $(DATABASE) $@

$(OBJ_DIR)/%.t: %.F90 | $(OBJ_SUBDIRS)
	@echo Analysing $<
	$(Q)$(TOOL_DIR)/DependencyAnalyser $(IGNORE_ARGUMENTS) \
	                               $(DATABASE) $< && touch $@

$(OBJ_DIR)/%.t: %.f90 | $(OBJ_SUBDIRS)
	@echo Analysing $<
	$(Q)$(TOOL_DIR)/DependencyAnalyser $(IGNORE_ARGUMENTS) \
	                                   $(DATABASE) $< && touch $@

# Special Rules

.PHONY: clean
clean:
	-rm -rf $(OBJ_DIR)
	-rm -f $(BIN_DIR)/*
