#!/bin/bash

export C_USR="-I$(pwd) -m64"
export L_USR="-Wl,--noinhibit-exec"
export PRG_USR=-w0

if [ -z "$HB_ARCHITECTURE" ]; then export HB_ARCHITECTURE=linux; fi
if [ -z "$HB_COMPILER" ]; then export HB_COMPILER=gcc; fi
if [ -z "$HB_GPM_MOUSE" ]; then export HB_GPM_MOUSE=yes; fi
if [ -z "$HB_GT_LIB" ]; then export HB_GT_LIB=gtsln; fi
if [ -z "$HB_MULTI_GT" ]; then export HB_MULTI_GT=yes; fi
if [ -z "$HB_MT" ]; then export HB_MT=MT; fi

# Set to constant value to be consistent with the non-GNU make files.

if [ -z "$HB_BIN_INSTALL" ]; then export HB_BIN_INSTALL=$(pwd)/bin/; fi
if [ -z "$HB_LIB_INSTALL" ]; then export HB_LIB_INSTALL=$(pwd)/lib/; fi
if [ -z "$HB_INC_INSTALL" ]; then export HB_INC_INSTALL=$(pwd)/include/; fi

make $*
