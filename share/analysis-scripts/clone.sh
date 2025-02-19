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

git_hash="master"
clone_dir="frama-c-clones"
repository_path="git@git.frama-c.com:frama-c/frama-c"
show_usage=""

while [[ $# > 0 ]]
do
    case $1 in
        -d|--clone-dir)
            clone_dir="$2"
            shift
            ;;

        -p|--repository-path)
            repository_path="$2"
            shift
            ;;

        -h|--help)
            show_usage="yes"
            ;;

        *)
            git_hash="$1"
            ;;
    esac
    shift
done

if [ -n "$show_usage" ]
then
    echo "Usage: $0 HASH"
    echo "Provides a working tree of Frama-C."
    echo ""
    echo "The following arguments can be given:"
    echo "  -d, --clone-dir             path to the directory where frama-c versions are"
    echo "                                cloned"
    echo "  -p, --repository-path PATH  do not clone from frama-c gitlab, use this path instead"
    echo "  -h, --help                  prints this help and quits"
    exit 1
fi


bare="$clone_dir/frama-c.git"

# Check if bench clone exists
if [ ! -d "$bare" ]
then
    git clone --bare --quiet $repository_path "$bare"
    sed --in-place '/bare = true/d' $bare/config
fi

# Fetch all refs
git -C $bare fetch origin '+refs/heads/*:refs/heads/*' --prune

# Resolve branch name if given
git_hash=`git --git-dir="$bare" rev-parse "$git_hash"`

# target_path must be an absolute path
target_path="$(readlink -f "$clone_dir/$git_hash")"

# Checkout
if [ ! -e "$target_path" ]
then
    git clone "$bare" "$target_path" --quiet
    (cd "$target_path" && git checkout "$git_hash" --quiet)
fi

# Build Frama-C
if [ ! -e "$target_path/build/bin/frama-c" ]
then
    (
        cd "$target_path";
        make -j --quiet > /dev/null;
        make install PREFIX=$(pwd)/build > /dev/null;
    )
fi

# Output repository path
echo "$target_path"
