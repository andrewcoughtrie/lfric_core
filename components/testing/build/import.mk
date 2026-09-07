##############################################################################
# (c) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
export PROJECT_SOURCE = $(CORE_ROOT_DIR)/components/testing/source

.PHONY: import-testing
import-testing:
	$Q$(MAKE) $(QUIET_ARG) -f $(LFRIC_BUILD)/extract.mk SOURCE_DIR=$(PROJECT_SOURCE)
	$Q$(MAKE) $(QUIET_ARG) -f $(LFRIC_BUILD)/psyclone/psyclone_psykal.mk \
            SOURCE_DIR=$(PROJECT_SOURCE) \
            OPTIMISATION_PATH=$(OPTIMISATION_PATH)
