# -*- coding: utf-8 -*-
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

"""Contains functions to perform logging-related configuration common to
   several scripts."""

import logging
import sys

# extra log level used in messages
VERBOSE = 15


def init(debug, verbose):
    """Initializes the logging mechanism. 'debug' is the filename to redirect to
    (or special values 'stdout'/'stderr'). If None, disable debug.
    If 'verbose' is true, output VERBOSE messages. If both are set, the level
    is 'debug'."""

    logging.addLevelName(VERBOSE, "")
    logging.addLevelName(logging.ERROR, "ERROR: ")
    logging.addLevelName(logging.WARN, "WARNING: ")
    logging.addLevelName(logging.INFO, "")
    log_level = logging.DEBUG if debug else VERBOSE if verbose else logging.INFO
    log_format = "%(levelname)s%(message)s"

    # special values for debug filename
    if debug == "stdout" or not debug:
        logging.basicConfig(stream=sys.stdout, level=log_level, format=log_format)
        # This very ugly hack seems necessary to avoid BrokenPipeErrors when the
        # output of the calling script is truncated (via head, grep -q, etc).
        # Note that, on Windows, there is an OSError with "22 Invalid argument"
        # instead of a BrokenPipeError.
        original_flush = sys.stdout.flush

        def new_flush():
            try:
                original_flush()
            except BrokenPipeError:
                sys.stdout = None
                sys.exit(0)
            except OSError as e:
                if sys.platform == "win32" and e.errno == 22:
                    sys.stdout = None
                    sys.exit(0)
                raise e

        sys.stdout.flush = new_flush
    elif debug == "stderr":
        logging.basicConfig(stream=sys.stderr, level=log_level, format=log_format)
    else:
        logging.basicConfig(
            filename=debug,
            level=log_level,
            filemode="w",
            format=log_format,
        )
