##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
#
# Run this make file to copy a source tree from SOURCE_DIR to WORKING_DIR
#
.PHONY: files-to-extract
files-to-extract: $(addprefix $(WORKING_DIR)/,$(shell find $(SOURCE_DIR) -name '*.[Ff]90' -printf '%P\n'))
	$(Q)echo >/dev/null

.PRECIOUS: $(WORKING_DIR)/%.F90
$(WORKING_DIR)/%.F90: $(SOURCE_DIR)/%.F90
	$(call MESSAGE,Copying source,$<)
	$(Q)mkdir -p $(dir $@)
	$(Q)cp $< $@

.PRECIOUS: $(WORKING_DIR)/%.f90
$(WORKING_DIR)/%.f90: $(SOURCE_DIR)/%.f90
	$(call MESSAGE,Copying source,$<)
	$(Q)mkdir -p $(dir $@)
	$(Q)cp $< $@

include $(LFRIC_BUILD)/lfric.mk
