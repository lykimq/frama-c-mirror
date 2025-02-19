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

commit="$(git ls-remote git@git.frama-c.com:frama-c/wp-cache.git HEAD)"
if [ $? != 0 ]; then exit 1; fi

commit=$(echo "$commit" | cut -f1)

cat >./nix/wp-cache.nix << EOL
{ lib, stdenv } :
stdenv.mkDerivation rec {
  name = "frama-c-wp-cache";
  src = fetchGit {
           url = "git@git.frama-c.com:frama-c/wp-cache.git" ;
           rev = "$commit" ;
           shallow = true ;
         };
  installPhase = "touch \$out";
}
EOL
