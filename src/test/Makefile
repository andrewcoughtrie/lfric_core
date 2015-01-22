##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

IGNORE_DEPENDENCIES = netcdf
EXTERNAL_LIBRARIES = netcdff

export ROOT = ../..
include $(ROOT)/include.mk
export PFUNIT_INSTALL = $(BUILD_DIR)/pfunit-install

SPECIAL_FFLAGS := $(filter-out -Werror,$(FFLAGS)) # pFUnit isn't that well put together

OBJ_DIR = $(BUILD_DIR)/tests
OBJ_SUBDIRS = $(patsubst %,$(OBJ_DIR)/%,$(shell find * -type d))
export DRIVER_OBJ=$(OBJ_DIR)/driver.o

TEST_SOURCES = $(shell find -name "*.pf")
TEST_OBJECTS = $(patsubst %.pf,$(OBJ_DIR)/%.o,$(TEST_SOURCES))

SUPPORT_SOURCE  = $(shell find -name "*.[Ff]90")
SUPPORT_OBJECTS = $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(basename $(SUPPORT_SOURCE))))

SUBJECT_DIR     = $(BUILD_DIR)/dynamo
SUBJECT_SUBDIRS = $(shell find $(SUBJECT_DIR) -type d)
SUBJECT_MODINC  = $(patsubst %,-I %,$(SUBJECT_SUBDIRS))

.PHONY: test
test: $(OBJ_DIR)/test | pfunit
	$(Q)$(OBJ_DIR)/test -robust

.PRECIOUS: $(OBJ_DIR)/%.F90
$(OBJ_DIR)/%.F90: %.pf | $(OBJ_DIR) $(OBJ_SUBDIRS)
	@echo Generating $@
	$(Q)$(PFUNIT_INSTALL)/bin/pFUnitParser.py $< $@

$(OBJ_DIR)/%.o: $(OBJ_DIR)/%.F90
	@echo Compiling $<
	$(Q)$(FC) $(FPPFLAGS) $(SPECIAL_FFLAGS) $(FFLAGS_TESTS) \
	          $(F_MOD_DESTINATION_ARG)$(OBJ_DIR)/$(dir $@) \
	          $(SUBJECT_MODINC) -I$(PFUNIT_INSTALL)/mod \
	          -c -o $@ $<

$(OBJ_DIR)/test: $(DRIVER_OBJ) $(SUPPORT_OBJECTS) $(TEST_OBJECTS)
	@echo Linking $@
	$(Q)$(FLINK) $(FLDFLAGS) -o $@ \
	             $^ \
	             $(SUBJECT_DIR)/modules.a \
	             $(PFUNIT_INSTALL)/lib/libpfunit.a \
	             $(patsubst %,-l%,$(EXTERNAL_LIBRARIES))

# Replace OBJ_SUBDIR with mangling of % ?
$(OBJ_DIR)/%.o: %.F90 | $(OBJ_SUBDIRS)
	$(FC) $(FPPFLAGS) $(F_MOD_DESTINATION_ARG)$(OBJ_DIR)/$(dir $@) \
	      $(SUBJECT_MODINC) -c -o $@ $<

$(OBJ_DIR)/%.o: %.f90 | $(OBJ_SUBDIRS)
	$(FC) $(FPPFLAGS) $(F_MOD_DESTINATION_ARG)$(OBJ_DIR)/$(dir $@) \
	      $(SUBJECT_MODINC) -c -o $@ $<

$(DRIVER_OBJ):
	$(MAKE) -f pfunit.mk $(DRIVER_OBJ)

$(OBJ_DIR) $(OBJ_SUBDIRS):
	@echo "Creating $@"
	$(Q)mkdir -p $@

.PHONY: pfunit
pfunit:
	$(MAKE) -f pfunit.mk pfunit

.PHONY: clean
clean:
	-rm -rf $(OBJ_DIR)
ifdef ALL
	$(MAKE) -f pfunit.mk clean
endif
