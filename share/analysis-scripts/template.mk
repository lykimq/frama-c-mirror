##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2025                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

# Makefile template for Frama-C/Eva case studies.
# For details and usage information, see the Frama-C User Manual.

### Prologue. Do not modify this block. #######################################
-include path.mk # path.mk contains variables specific to each user
                 # (e.g. FRAMAC, FRAMAC_GUI) and should not be versioned. It is
                 # an optional include, unnecessary if frama-c is in the PATH.
FRAMAC ?= frama-c # FRAMAC is defined in path.mk when it is included, but the
                  # user can override it in the command-line.
ifeq ($(FRAMAC_LIB),)
  FRAMAC_LIB := $(shell $(FRAMAC)-config -print-lib-path)
endif
include $(FRAMAC_LIB)/analysis-scripts/prologue.mk
###############################################################################

# Edit below as needed. Suggested flags are optional.

MACHDEP = x86_64

## Preprocessing flags (for -cpp-extra-args)
CPPFLAGS    += \

## Other preprocessing and parsing flags (e.g. -cpp-extra-args-per-file)
PARSEFLAGS    += \

## General flags
FCFLAGS     += \
  -add-symbolic-path=..:. \
  -kernel-warn-key annot:missing-spec=abort \
  -kernel-warn-key typing:implicit-function-declaration=abort \

## Eva-specific flags
EVAFLAGS    += \
  -eva-warn-key builtins:missing-spec=abort \
  -eva-warn-key libc:unsupported-spec=abort \
  -eva-warn-key recursion=abort \

# Note: if the code has recursive calls, manually review them, add assigns as
# needed, and remove the line '-eva-warn-key recursion=abort' above

## WP-specific flags
WPFLAGS    += \

## GUI-only flags
FCGUIFLAGS += \

## Analysis targets (suffixed with .eva)
TARGETS = main.eva

### Each target <t>.eva needs a rule <t>.parse with source files as prerequisites
main.parse: \
  main.c \

### Epilogue. Do not modify this block. #######################################
include $(FRAMAC_LIB)/analysis-scripts/epilogue.mk
###############################################################################
