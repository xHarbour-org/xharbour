#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build RPMs from xHarbour sources
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

test_reqrpm()
{
    rpm -q "$1" &> /dev/null
}

TOINST_LST=""
for i in gcc binutils bash flex bison ncurses ncurses-devel slang-devel gpm-devel
do
    test_reqrpm "$i" || TOINST_LST="${TOINST_LST} $i"
done

if [ -z "${TOINST_LST}" ]
then
    . ./bin/\!pack_src.sh
    if [ -f ${hb_filename} ]
    then
	if [ `id -u` != 0 ] && [ ! -f ${HOME}/.rpmmacros ]
	then
	    RPMDIR="${HOME}/RPM"
	    mkdir -p ${RPMDIR}/SOURCES ${RPMDIR}/RPMS ${RPMDIR}/SRPMS \
		     ${RPMDIR}/BUILD ${RPMDIR}/SPECS
	    echo "%_topdir ${RPMDIR}" > ${HOME}/.rpmmacros
	fi
	rpm -ta ${hb_filename} --rmsource
    else
	echo "Cannot find archive file: ${hb_filename}"
    fi
else
    echo "If you want to build xHarbour compilers"
    echo "you have to install the folowing RPM files:"
    echo "${TOINST_LST}"
fi
