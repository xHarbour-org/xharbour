#!/bin/sh
#
# $Id: hb-mkslib.sh,v 1.3 2003/06/27 20:58:02 druzus Exp $
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build shared libraries from static ones and
# object files
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

if [ $# -lt 2 ]
then
    echo "usage: `basename $0` <target[.so]> src1.a .. srcN.a [obj1.o .. objN.o]"
    exit 1
fi

OTMPDIR="/tmp/hb-mkslib-$$"
HB_SO_LIB="$1"
shift
case "${HB_SO_LIB}" in
    *.so)
	;;
    *)
	HB_SO_LIB="${HB_SO_LIB}.so"
	;;
esac
dir=`pwd`

cleanup()
{
    rm -fR "${OTMPDIR}"
}

trap cleanup EXIT &>/dev/null

rm -fR "${OTMPDIR}"
mkdir -p "${OTMPDIR}"
cd "${OTMPDIR}"

for f in $*
do
    if [ ! -r "${dir}/${f}" ]
    then
	echo "cannot read file: ${f}"
	exit 1
    fi
    case "${f}" in
	*.o)
	    cp "${dir}/${f}" "${OTMPDIR}" || exit 1
	    ;;
	*.a)
	    d="${f%.a}"
	    d="${f##*/}"
	    mkdir $d
	    cd $d
	    ar -x "${dir}/${f}" || exit 1
	    cd ..
	    ;;
	*)
	    echo "unrecognized file: ${f}"
	    exit 1
	    ;;
    esac
done
OBJLST=`find . -name \*.o`
cd "${dir}"
rm -f "${HB_SO_LIB}"
cd "${OTMPDIR}"

base=`basename "${HB_SO_LIB}"`
if [ `uname` = "Darwin" ]; then
    short=`echo ${base} | sed "s/\(.*\)-${version}.*/\1/g"`
    version=`echo ${base} | sed "s/[^0-9]*\([0-9.]*\)/\1/g;s/\.$//g"`
    major=`echo ${version} | sed "s/^\([0-9]*\).*/\1/g"`
    minor=`echo ${version} | sed "s/^[0-9]*\.\([0-9]*\).*/\1/g"`
    gcc -dynamiclib -install_name "${dir}/${short}.${major}.dylib" \
        -compatibility_version ${major}.${minor} -current_version ${version} \
        -fno-common -o "${short}.${version}.dylib" $OBJLST && \
        cd "${dir}" && \
        mv -f "${OTMPDIR}/${short}.${version}.dylib" `dirname "${HB_SO_LIB}"`/${short}.${version}.dylib && \
        rm -f "${short}.${major}.dylib" "${short}.dylib" && \
        ln -s "${short}.${version}.dylib" "${short}.${major}.dylib" && \
        ln -s "${short}.${version}.dylib" "${short}.dylib"
else
    gcc -shared -o "${base}" $OBJLST && \
        cd "${dir}" && \
        mv -f "${OTMPDIR}/${base}" "${HB_SO_LIB}"
fi

stat="$?"
cleanup
exit "${stat}"
