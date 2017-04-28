##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
#
# Run this make file to generate configuration found in SOURCE_DIR
# to WORKING_DIR. Uses PROJECT to know what to call master files.
#
.PHONY: configuration_files
configuration_files: $(WORKING_DIR)/$(PROJECT)_configuration_mod.f90 \
                     $(WORKING_DIR)/$(PROJECT)_feign_config_mod.f90
	$(Q)echo >/dev/null

NAMELIST_DESCRIPTIONS = $(shell find $(SOURCE_DIR) -name '*.nld' -print)
NAMELIST_LOADERS = $(patsubst $(SOURCE_DIR)/%.nld,$(WORKING_DIR)/%_config_mod.f90,$(NAMELIST_DESCRIPTIONS))

.PRECIOUS: $(WORKING_DIR)/%_configuration_mod.f90
$(WORKING_DIR)/%_configuration_mod.f90: $(NAMELIST_LOADERS)
	$(call MESSAGE,Generating configuration loader,$@)
	$(Q)$(LFRIC_BUILD)/tools/GenerateLoader $(VERBOSE_ARG) $@ $(patsubst %_config_mod.f90,%,$(notdir $^))

.PRECIOUS: $(WORKING_DIR)/%_config_mod.f90
$(WORKING_DIR)/%_config_mod.f90: $(SOURCE_DIR)/%.nld
	$(call MESSAGE,Generating namelist loader,$@)
	$(Q)mkdir -p $(dir $@)
	$(Q)$(LFRIC_BUILD)/tools/GenerateNamelist $(VERBOSE_ARG) \
                                              -directory $(dir $@) $<

.PRECIOUS: $(WORKING_DIR)/%_feign_config_mod.f90
$(WORKING_DIR)/%_feign_config_mod.f90: $(NAMELIST_DESCRIPTIONS)
	$(call MESSAGE,Generating feign module,$@)
	$(Q)$(LFRIC_BUILD)/tools/GenerateFeigns -output $@ $^

include $(LFRIC_BUILD)/lfric.mk
