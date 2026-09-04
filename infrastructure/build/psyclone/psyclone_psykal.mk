##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE which you
# should have received as part of this distribution.
##############################################################################
#
# Run this make file to generate PSyKAl source in WORKING_DIR from algorithms
# and kernels in SOURCE_DIR. Transformation scripts are sought in
# OPTIMISATION_PATH.
#
# Set the DSL Method in use to collect the correct transformation files.
DSL = psykal
#
# This makefile's own path, so that the phases below can recurse into it. It
# must be captured here, before any include adds to MAKEFILE_LIST, and cannot
# be built from LFRIC_BUILD because that is not defined until lfric.mk has been
# included further down.
PSYKAL_MAKEFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
#

# Set default psyclone command additional options
PSYCLONE_PSYKAL_EXTRAS ?= -l all
#
# The command used to invoke PSyclone for an individual file.
PSYCLONE ?= psyclone
#
# Running "psyclone" once per algorithm file means importing PSyclone, fparser
# and sympy every time - seven to ten seconds and some sixteen thousand
# filesystem operations per file, on a site install where those libraries sit
# on shared NFS. A build with hundreds of algorithm files spends most of its
# time doing nothing else.
#
# psyclone_batch.py removes that cost from the bulk of the work: it imports
# PSyclone once and forks a child per file. It runs as a pre-pass, before the
# per-file rules below, which then find their targets already up to date. It is
# purely an optimisation - anything it skips or fails to produce is still built
# by the ordinary rules - so a build cannot be broken by it.
#
# Set PSYCLONE_NO_BATCH=1 to skip the pre-pass and transform every file
# individually, which is the simplest way to isolate a problem.
PSYCLONE_BATCH ?= $(LFRIC_BUILD)/psyclone/psyclone_batch.py
#
# Number of algorithm files transformed at once by the pre-pass. Sized to the
# build parallelism where known (MAKE_THREADS), otherwise to the number of
# available processors, and capped so a many-core node cannot run away with
# memory - each concurrent transformation needs roughly 100MB.
#
# Note ":=" rather than "?=": a recursively expanded variable would re-run
# "nproc" on every expansion.
PSYCLONE_MAX_WORKERS ?= 8
ifdef MAKE_THREADS
  PSYCLONE_WORKERS_WANTED := $(MAKE_THREADS)
else
  PSYCLONE_WORKERS_WANTED := $(shell nproc 2>/dev/null || echo 4)
endif
PSYCLONE_WORKERS := $(shell \
    if [ $(PSYCLONE_WORKERS_WANTED) -gt $(PSYCLONE_MAX_WORKERS) ]; \
    then echo $(PSYCLONE_MAX_WORKERS); \
    else echo $(PSYCLONE_WORKERS_WANTED); fi)
#

ALGORITHM_F_FILES := $(patsubst $(SOURCE_DIR)/%.X90, \
                                $(WORKING_DIR)/%.f90, \
                                $(shell find $(SOURCE_DIR) -name '*.X90' -print))

ALGORITHM_f_FILES := $(patsubst $(SOURCE_DIR)/%.x90, \
                                $(WORKING_DIR)/%.f90, \
                                $(shell find $(SOURCE_DIR) -name '*.x90' -print))

# The preprocessed algorithms in the workspace, which are what both the batch
# pre-pass and the per-file rules consume.
ALGORITHM_X90_FILES := \
    $(patsubst $(SOURCE_DIR)/%.X90,$(WORKING_DIR)/%.x90, \
               $(shell find $(SOURCE_DIR) -name '*.X90' -print)) \
    $(patsubst $(SOURCE_DIR)/%.x90,$(WORKING_DIR)/%.x90, \
               $(shell find $(SOURCE_DIR) -name '*.x90' -print))

DIRECTORIES := $(patsubst $(SOURCE_DIR)%,$(WORKING_DIR)%, \
                          $(shell find $(SOURCE_DIR) -type d -printf '%p/\n'))
PSYCLONE_CONFIG_FILE ?= $(CORE_ROOT_DIR)/etc/psyclone.cfg

# Three phases, because make decides whether a target is out of date before it
# runs any recipe. The algorithms must all exist in the workspace before the
# batch can see which are stale, and the batch must have finished before make
# tests the generated files. Recursion is how the rest of this build system
# sequences such phases.
#
.PHONY: psyclone
psyclone:
	$Q$(MAKE) $(QUIET_ARG) -f $(PSYKAL_MAKEFILE) psyclone-preprocess
	$Q$(MAKE) $(QUIET_ARG) -f $(PSYKAL_MAKEFILE) psyclone-batch
	$Q$(MAKE) $(QUIET_ARG) -f $(PSYKAL_MAKEFILE) psyclone-generate

.PHONY: psyclone-preprocess
psyclone-preprocess: $(ALGORITHM_X90_FILES) | $(WORKING_DIR)/kernel

# PSyclone will not create its own kernel output directory, and neither the
# batch pre-pass nor the per-file rules can run without it.
$(WORKING_DIR)/kernel:
	$(call MESSAGE,Creating,$@)
	$Qmkdir -p $@

.PHONY: psyclone-generate
psyclone-generate: $(ALGORITHM_F_FILES) $(ALGORITHM_f_FILES)

.PHONY: psyclone-batch
psyclone-batch:
ifneq ($(PSYCLONE_NO_BATCH),1)
	$QPYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH $(PSYCLONE_BATCH) \
	           --source-dir '$(SOURCE_DIR)' \
	           --working-dir '$(WORKING_DIR)' \
	           --optimisation-path '$(OPTIMISATION_PATH)' \
	           --dsl '$(DSL)' \
	           --config '$(PSYCLONE_CONFIG_FILE)' \
	           --workers '$(PSYCLONE_WORKERS)' \
	           --extra '$(PSYCLONE_PSYKAL_EXTRAS)'
endif

include $(LFRIC_BUILD)/lfric.mk
include $(LFRIC_BUILD)/fortran.mk

MACRO_ARGS := $(addprefix -D,$(PRE_PROCESS_MACROS))

# Where an override file exists in the "psy" directory we invoke PSyclone, then
# delete the resulting PSy source. The override has been copied as part of the
# rest of the source.
#
$(WORKING_DIR)/%.f90: \
$$(SOURCE_DIR)/psy/$$(notdir $$*)_psy.f90 $(WORKING_DIR)/%_psy.f90
	$(call MESSAGE,Removing,$*_psy.f90)
	$Qrm $(WORKING_DIR)/$*_psy.f90

# Where an optimisation script exists for a specific file, use it.
#
$(WORKING_DIR)/%.f90 $(WORKING_DIR)/%_psy.f90: \
$(WORKING_DIR)/%.x90 $$(OPTIMISATION_PATH)/$(DSL)/$$*.py | $$(dir $$@)
	$(call MESSAGE,PSyclone - local optimisation,$(subst $(SOURCE_DIR)/,,$<))
	$QPYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH $(PSYCLONE) -api lfric \
	           -d $(WORKING_DIR) \
	           --config $(PSYCLONE_CONFIG_FILE) \
	           -s $(OPTIMISATION_PATH)/$(DSL)/$*.py \
	           -okern $(WORKING_DIR)/kernel \
	           -oalg $(WORKING_DIR)/$*.f90 \
	           -opsy $(WORKING_DIR)/$*_psy.f90 \
	           $(PSYCLONE_PSYKAL_EXTRAS) \
	           $<

# Where a global optimisation script exists, use it.
#
$(WORKING_DIR)/%.f90 $(WORKING_DIR)/%_psy.f90: \
$(WORKING_DIR)/%.x90 $(OPTIMISATION_PATH)/$(DSL)/global.py | $$(dir $$@)
	$(call MESSAGE,PSyclone - global optimisation,$(subst $(SOURCE_DIR)/,,$<))
	$QPYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH $(PSYCLONE) -api lfric \
	           -d $(WORKING_DIR) \
	           --config $(PSYCLONE_CONFIG_FILE) \
	           -s $(OPTIMISATION_PATH)/$(DSL)/global.py \
	           -okern $(WORKING_DIR)/kernel \
	           -oalg  $(WORKING_DIR)/$*.f90 \
	           -opsy $(WORKING_DIR)/$*_psy.f90 \
	           $(PSYCLONE_PSYKAL_EXTRAS) \
	           $<

# Where no optimisation script exists, don't use it.
#
$(WORKING_DIR)/%.f90 $(WORKING_DIR)/%_psy.f90: \
$(WORKING_DIR)/%.x90 | $$(dir $$@)
	$(call MESSAGE,PSyclone,$(subst $(SOURCE_DIR)/,,$<))
	$QPYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH $(PSYCLONE) -api lfric \
	           -l all -d $(WORKING_DIR) \
	           --config $(PSYCLONE_CONFIG_FILE) \
	           -okern $(WORKING_DIR)/kernel \
	           -oalg  $(WORKING_DIR)/$*.f90 \
	           -opsy $(WORKING_DIR)/$*_psy.f90 \
	           $(PSYCLONE_PSYKAL_EXTRAS) \
	           $<

.PRECIOUS: $(WORKING_DIR)/%.x90
# Perform preprocessing for big X90 files.
#
ifeq ("$(FORTRAN_COMPILER)", "nvfortran")
$(WORKING_DIR)/%.x90: $(SOURCE_DIR)/%.X90 | $$(dir $$@)
	$(call MESSAGE,Preprocessing, $(subst $(SOURCE_DIR)/,,$<))
	$Q$(FPP) $(FPPFLAGS) $(MACRO_ARGS) -o $@ $<
else
$(WORKING_DIR)/%.x90: $(SOURCE_DIR)/%.X90 | $$(dir $$@)
	$(call MESSAGE,Preprocessing, $(subst $(SOURCE_DIR)/,,$<))
	$Q$(FPP) $(FPPFLAGS) $(MACRO_ARGS) $< $@
endif

# Little x90 files are just copied to the workspace.
#
$(WORKING_DIR)/%.x90: $(SOURCE_DIR)/%.x90 | $$(dir $$@)
	$(call MESSAGE,Copying, $(subst $(SOURCE_DIR)/,,$<))
	$Qcp $< $@

# Create directories in the workspace as needed.
#
$(DIRECTORIES):
	$(call MESSAGE,Creating,$@)
	$Qmkdir -p $@
