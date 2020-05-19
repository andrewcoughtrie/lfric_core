##############################################################################
# (c) Crown copyright 2017 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################

include $(LFRIC_BUILD)/compile_options.mk

$(info UM physics specific compile options)

include $(PROJECT_DIR)/build/fortran/$(FORTRAN_COMPILER).mk

science/%.o science/%.mod: export FFLAGS += $(FFLAGS_UM_PHYSICS)
