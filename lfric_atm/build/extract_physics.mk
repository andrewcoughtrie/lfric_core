##############################################################################
# (c) Crown copyright 2020 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
#
# Run this file to extract source code from the UM repository.
#
# The following environment variables are used for input:
#   LFRIC_TARGET_PLATFORM : Target identifier used to get the correct source.
#   PROFILE : Build profile used to determine optimisation level.
#   PROJECT_DIR : Full path to the current project's root directory.
#   SCRATCH_DIR : Temporary space for extracted source.
#   WORKING_DIR : Directory to hold working copies of source.
#
###############################################################################

.PHONY: skip
skip: extract

include $(LFRIC_BUILD)/lfric.mk
include $(LFRIC_BUILD)/cxx.mk
include $(LFRIC_BUILD)/fortran.mk

export platform_config_dir=$(strip $(shell grep -E "$(LFRIC_TARGET_PLATFORM)\s*:\s*$(FORTRAN_COMPILER)\s*:" $(PROJECT_DIR)/fcm-make/target-map.txt | cut -d : -f 3))
export optimisation_level=$(strip $(shell grep -E "$(PROFILE)\s*:\s*" $(PROJECT_DIR)/fcm-make/optimisation-map.txt | cut -d : -f 2))

.PHONY: extract
extract:
	$Qif [ x$(platform_config_dir) = x ] ; then echo Unable to convert $(LFRIC_TARGET_PLATFORM):$(FORTRAN_COMPILER) to a UM target; false; fi
	$(info Using UM target $(platform_config_dir))
	# Retrieve and preprocess the UM, Jules and Socrates code
	# The UM_ENV file contains the appropriate locations and UM side
	# environment variables
	$Qsource $(PROJECT_DIR)/fcm-make/parameters.sh \
	   && fcm make -C $(SCRATCH_DIR) -f $(PROJECT_DIR)/fcm-make/extract.cfg
	# Note that if wanting to modify UM source this should be done via the
	# UM repository either through a working copy or branch
	$Qrsync -acvz $(SCRATCH_DIR)/preprocess-atmos/ $(WORKING_DIR)/science/
