#! /bin/sh
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

#
# convert a Frama-C plugin from Frama-C Magnesium to Frama-C Aluminium
# as most as possible (no guarantee that the result is fully compatible)
#
# known miss features: don't work if a directory name contains some spaces

ARGS=$@

DIR=

# verbosing on by default
VERBOSE="v"

sedi ()
{
  if [ -n "`sed --help 2> /dev/null | grep \"\\-i\" 2> /dev/null`" ]; then
    eval sed -i "$@"
  else
      # option '-i' is not recognized by sed: use a tmp file
    new_temp=`mktemp /tmp/frama-c.XXXXXXX` || exit 1
    eval sed "$@" > $new_temp
    eval last=\${$#}
    mv $new_temp $last
  fi
}

dirs ()
{
  if [ -z "$DIR" ]; then
    DIR=.
  fi
}

safe_goto ()
{
  dir=$1
  cd $dir
  $3
  cd $2
}

goto ()
{
  if [ -d $1 ]; then
    safe_goto $1 $2 $3
  else
    echo "Directory '$1' does not exist. Omitted."
  fi
}

process_file ()
{
  file=$1
  if [ "$VERBOSE" ]; then
    echo "Processing file $file"
  fi
  sedi \
   -e "\"s/filter_le_ge_lt_gt/backward_comp/g\"" \
    $file
}

apply_one_dir ()
{
  if [ "$VERBOSE" ]; then
    echo "Processing directory `pwd`"
  fi
  for f in `ls -p1 *.ml* 2> /dev/null`; do
    process_file $f
  done
}

apply_recursively ()
{
  apply_one_dir
  for d in `ls -p1 | grep \/`; do
    safe_goto $d .. apply_recursively
  done
}

applying_to_list ()
{
  dirs
  tmpdir=`pwd`
  for d in $DIR; do
    goto $d $tmpdir $1
  done
}

help ()
{
  echo "Usage: $0 [options | directories]

Options are:
  -r | --recursive       Check subdirectories recursively
  -h | --help            Display help message
  -q | --quiet           Quiet mode (i.e. non-verbose mode)
  -v | --verbose         Verbose mode (default)"
  exit 0
}

error ()
{
  echo "$1.
Do \"$0 -h\" for help."
  exit 1
}

FN="apply_one_dir"

parse_arg ()
{
  case $1 in
    -r | --recursive)     FN="apply_recursively";;
    -h | -help      )     help; exit 0;;
    -q | --quiet    )     VERBOSE=;;
    -v | --verbose  )     VERBOSE="v";;
    -* )                  error "Invalid option $1";;
    * )                   DIR="$DIR $1";;
  esac
}

cmd_line ()
{
  for s in $ARGS; do
    parse_arg $s
  done
  applying_to_list $FN
}

cmd_line
exit 0
