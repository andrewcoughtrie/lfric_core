##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
#
# Run this make file to generate pFUnit source in WORKING_DIR from test
# descriptions in SOURCE_DIR.
#
PF_FILES = $(shell find $(SOURCE_DIR) -name '*.pf' -printf '%P\n')

.PHONY: prepare-pfunit
prepare-pfunit: $(patsubst %.pf,$(WORKING_DIR)/%.F90,$(PF_FILES)) \
        $(WORKING_DIR)/$(PROJECT)_unit_test.F90
	@echo >/dev/null

include $(LFRIC_BUILD)/lfric.mk

.PRECIOUS: $(WORKING_DIR)/$(PROJECT)_unit_test.F90
$(WORKING_DIR)/$(PROJECT)_unit_test.F90: $(PFUNIT)/include/driver.F90 \
                                         $(WORKING_DIR)/testSuites.inc
	$(call MESSAGE,Processing,pFUnit driver source)
	$(Q)sed "s/program main/program $(basename $(notdir $@))/" <$< >$@

.PRECIOUS: $(WORKING_DIR)/testSuites.inc
$(WORKING_DIR)/testSuites.inc:
	$(call MESSAGE,Collating, $@)
	$(Q)mkdir -p $(dir $@)
	$(Q)echo ! Tests to run >$@
	$(Q)for test in $(basename $(notdir $(shell find $(SOURCE_DIR) -name '*.pf'))); do echo ADD_TEST_SUITE\($${test}_suite\) >> $@; done

$(WORKING_DIR)/%.F90: $(SOURCE_DIR)/%.pf
	$(call MESSAGE,Generating unit test,$@)
	$(Q)mkdir -p $(dir $@)
ifdef VERBOSE
	$(Q)$(PFUNIT)/bin/pFUnitParser.py $< $@
else
	$(Q)$(PFUNIT)/bin/pFUnitParser.py $< $@ >/dev/null
endif
