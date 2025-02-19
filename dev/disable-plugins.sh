#!/usr/bin/env bash
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

case "$1" in
  "-h"|"--help")
    echo "Usage: $0 <plugin directories>"
    echo "  - directories are given without prefix src/plugins"
    echo "  - if no directory is given all plugins are enabled (alternative: none)"
    exit 0
esac

if [ ! -f VERSION ]; then
  echo "This script is meant to be run from the root directory of Frama-C"
  exit 2
fi

if [[ "$#" == "0" || ( "$#" == "1" && "$1" == "none" ) ]]; then
  rm -f src/plugins/dune
  echo "All plugins enabled"
  echo "Make sure to \"dune clean\" the Frama-C directory before rebuilding"
  exit 0
fi

IFS='\, ' read -a DISABLED_PLUGINS <<< "$@"

PLUGINS=
for PLUGIN in "${DISABLED_PLUGINS[@]}" ; do
  if [ -d "src/plugins/$PLUGIN" ]; then
    PLUGINS="$PLUGINS $PLUGIN"
  else
    echo "Error: src/plugins/$PLUGIN is not a directory"
    exit 2
  fi
done

cat > src/plugins/dune <<EOL
;; File generated by ./dev/disable-plugins.sh <PLUGINS>
(include_subdirs no)
;; Disabled plugin list:
(data_only_dirs ${PLUGINS})
(rule
 (alias "frama-c-configure")
 (deps (universe))
 (action (progn
  (echo "Disabled plug-in(s):\n")
EOL

for PLUGIN in "${DISABLED_PLUGINS[@]}" ; do
  echo "  (echo \"- src/plugins/$PLUGIN\n\")" >> src/plugins/dune
done

cat >> src/plugins/dune <<EOL
)))
;; Test
(alias (name ptests) (deps (alias ptests_config)))
(rule
 (alias "ptests_config")
 (deps (universe))
 (action (progn
  (echo "Testing with disabled plug-in(s):\n")
EOL
for PLUGIN in "${DISABLED_PLUGINS[@]}" ; do
  echo "  (echo \"- src/plugins/$PLUGIN\n\")" >> src/plugins/dune
done
echo ")))" >> src/plugins/dune

echo "Disabled plug-ins: $PLUGINS"
echo "Make sure to clean the current directory before rebuilding"
