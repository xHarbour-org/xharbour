#!/bin/sh
#
# $Id: make_drw.sh,v 1.8 2004/11/01 05:38:04 likewolf Exp $
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

export HB_ARCHITECTURE=darwin
export HB_COMPILER=gcc
export HB_GPM_MOUSE=no
export HB_MT=MT
export HB_MULTI_GT=yes

if [ -z "$PREFIX" ]; then export PREFIX=/usr/local; fi

if [ -z "$HB_GT_LIB" ]; then export HB_GT_LIB=gtsln; fi

# Set to constant value to be consistent with the non-GNU make files.

if [ -z "$HB_BIN_INSTALL" ]; then export HB_BIN_INSTALL=$PREFIX/bin/; fi
if [ -z "$HB_LIB_INSTALL" ]; then export HB_LIB_INSTALL=$PREFIX/lib/xharbour/; fi
if [ -z "$HB_INC_INSTALL" ]; then export HB_INC_INSTALL=$PREFIX/include/xharbour/; fi

# Autodetect old Darwin versions and set appropriate build options
if [ `uname -r | sed "s/\..*//g"` -lt 6 ]; then
    export HB_NCURSES_FINK=yes
fi

. `dirname $0`/make_gnu.sh $*
