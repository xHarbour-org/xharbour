#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id: postinst.sh,v 1.6 2004/05/28 18:51:21 likewolf Exp $
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script run after xHarbour make install to finish install
# process
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

if [ -z "$HB_ARCHITECTURE" ] || [ -z "$HB_COMPILER" ] || \
   [ -z "$HB_BIN_INSTALL" ] || \
   [ -z "$HB_INC_INSTALL" ] || \
   [ -z "$HB_LIB_INSTALL" ]
then
    echo "The following envvars must be set:"
    echo "   HB_ARCHITECTURE"
    echo "   HB_COMPILER"
    echo "   HB_BIN_INSTALL"
    echo "   HB_INC_INSTALL"
    echo "   HB_LIB_INSTALL"
    exit 1
fi

hb_root=`dirname "$0"`
if [ "${hb_root}" = "." ]
then
    hb_root=".."
else
    hb_root=`dirname "${hb_root}"`
fi
. ${hb_root}/bin/hb-func.sh

if [ "$HB_COMPILER" = "gcc" ] || [ "$HB_COMPILER" = "mingw32" ] || [ "$HB_COMPILER" = "djgpp" ]
then
    if [ "${HB_ARCHITECTURE}" == "bsd" ]; then
        MAKE=gmake
    else
        MAKE=make
    fi
    if [ "${HB_ARCHITECTURE}" != "dos" ]; then
        install -m755 "${hb_root}/bin/hb-mkslib.sh" "${HB_BIN_INSTALL}/hb-mkslib"
    fi
    mk_hbtools "${HB_BIN_INSTALL}"
    [ "$HB_COMPILER" = "gcc" ] && mk_hblibso "${hb_root}"

    # build fm lib with memory statistic
    (cd ${hb_root}/source/vm
    C_USR=${C_USR//-DHB_FM_STATISTICS_OFF/}
    rm -f fm.o
    ${MAKE} -r fm.o
    ar -r ${HB_LIB_INSTALL}/libfm.a fm.o
    rm -f fm.o
    if [ "${HB_MT}" = "MT" ]; then
        ${MAKE} -r fm.o 'HB_LIBCOMP_MT=YES'
        ar -r ${HB_LIB_INSTALL}/libfmmt.a fm.o
        rm -f fm.o
    fi
    )
fi
