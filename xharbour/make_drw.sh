#!/bin/bash
#
# $Id: make_drw.sh,v 1.1 2002/12/04 23:06:28 likewolf Exp $
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
export HB_GPM_MOUSE=yes
export HB_GT_LIB=gtstd

# Set to constant value to be consistent with the non-GNU make files.

if [ -z "$HB_BIN_INSTALL" ]; then export HB_BIN_INSTALL=bin/; fi
if [ -z "$HB_LIB_INSTALL" ]; then export HB_LIB_INSTALL=lib/; fi
if [ -z "$HB_INC_INSTALL" ]; then export HB_INC_INSTALL=include/; fi

. `dirname $0`/make_gnu.sh $*
