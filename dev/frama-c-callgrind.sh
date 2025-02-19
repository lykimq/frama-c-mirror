#!/bin/bash -eu
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

# Script for profiling Frama-C with callgrind (a valgrind tool).
# Note: execution time with valgrind is about 15x-20x slower.
#
# For more focused results, you can activate the profiling only after entering
# a specific function. For instance, to only profile Eva, add
#
#   --toggle-collect='*Eva__Analysis__force_compute*'
#
# to the command line below.
#
# Example of invocation :
#
#   devel_tools/frama-c-callgrind tests/idct/*.c -eva -float-normal -no-warn-signed-overflow
#
# This creates a 'callgrind.out' file (Callgrind format), which can be viewed
# with a tool such as kcachegrind:
#
#   kcachegrind callgrind.out

dune exec -- valgrind \
  --tool=callgrind \
  --callgrind-out-file=callgrind.out \
  --dump-instr=yes \
  --separate-callers=1 \
  --collect-jumps=yes \
  --fn-skip='caml_*' --fn-skip='caml_Stdlib' \
  frama-c "$@"
