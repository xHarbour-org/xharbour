#!/bin/sh
#
# $Id: make_rpm.sh,v 1.6 2003/06/15 13:11:56 druzus Exp $
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build RPMs from xHarbour sources
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

test_reqrpm()
{
    dpkg -s "$1" &> /dev/null
}

TOINST_LST=""
for i in gcc binutils bash bison libncurses5 libncurses5-dev debmake libgpmg1-dev
do
    test_reqrpm "$i" || TOINST_LST="${TOINST_LST} $i"
done

if [ -z "${TOINST_LST}" ] || [ "$1" = "--force" ]
then
    . ./bin/\!pack_src.sh
    stat="$?"
    if [ -z "${hb_filename}" ]
    then
	echo "The script ./bin/!pack_src.sh doesn't set archive name to \${hb_filename}"
	exit 1
    elif [ "${stat}" != 0 ]
    then
	echo "Error during packing the sources in ./bin/\!pack_src.sh"
	exit 1
    elif [ -f ${hb_filename} ]
    then
    install -m777 debian/xhbrules /usr/bin
    install -m777 debian/xhbrules1 /usr/bin
    dpkg-buildpackage -b
    else
	echo "Cannot find archive file: ${hb_filename}"
	exit 1
    fi
else
    echo "If you want to build xHarbour compiler"
    echo "you have to install the folowing RPM files:"
    echo "${TOINST_LST}"
    exit 1
fi
