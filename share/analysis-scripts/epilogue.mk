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

# Makefile template epilogue for analyses with Frama-C/Eva.
# For details and usage information, see the Frama-C User Manual.

# Some targets provided for convenience
# Note: they all depend on TARGETS having been properly set by the user
eva: $(TARGETS)
parse: $(TARGETS:%.eva=%.parse)
# Opening one GUI for each target is cumbersome; we open only the first target
gui: $(firstword $(TARGETS)).gui
ivette: $(firstword $(TARGETS)).ivette

# Default target
all: eva
ifeq ($(TARGETS),)
	@echo "error: TARGETS is empty"
endif

display-targets:
	@echo "$(addprefix .frama-c/,$(TARGETS))"
