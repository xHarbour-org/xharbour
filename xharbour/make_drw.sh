#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Template to initialize the environment before starting
# the GNU make system for Harbour
#
# For further information about the GNU make system please
# check doc/gmake.txt
#
# Copyright 1999-2001 Viktor Szakats (viktor.szakats@syenar.hu)
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

# Autodetect old Darwin versions and set appropriate build options
if [ `uname -r | sed "s/\..*//g"` -lt 6 ]; then
    export HB_NCURSES_FINK=yes
fi

. `dirname $0`/make_gnu.sh $*
