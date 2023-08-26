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

# NOTE: You will need to install latest versions of the following
# brew update
# brew install bison
# brew install zlib
# brew install openssl
# brew install tcl-tk
# brew install --cask xquartz

export C_USR="-Weverything -Wno-cast-qual -Wno-format-nonliteral -Wno-declaration-after-statement -Wno-empty-translation-unit -Wno-newline-eof -Wno-date-time -Wno-double-promotion -Wno-shorten-64-to-32 -Wno-shadow -Wno-implicit-int-conversion -Wno-sign-conversion -Wno-poison-system-directories -Wno-padded -Wno-deprecated-declarations -Wno-cast-align -Wno-undef -Wno-unused-macros -Wno-missing-prototypes -Wno-invalid-source-encoding"
export L_USR=""

export C_USR="$C_USR -I/usr/local/opt/zlib/include"
export L_USR="$L_USR -L/usr/local/opt/zlib/lib"

export C_USR="$C_USR -I/usr/local/opt/openssl/include"
export L_USR="$L_USR -L/usr/local/opt/openssl/lib"

export C_USR="$C_USR -I/opt/X11/include"
export L_USR="$L_USR -L/opt/X11/lib"

#if you do not want DEBUG build, comment the following line
export C_USR="$C_USR -g -O0 -fsanitize=address"
export L_USR="$L_USR -g -O0 -fsanitize=address"

. `dirname $0`/make_gnu.sh $*
