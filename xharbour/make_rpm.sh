#!/bin/sh
#
# $Id: make_rpm.sh,v 1.8 2003/09/11 17:59:01 druzus Exp $
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

BUGGY_RPM=""
if [ -f /etc/conectiva-release ]; then
    BUGGY_RPM="yes"
fi

TOINST_LST=""
for i in gcc binutils bash bison ncurses ncurses-devel gpm-devel 
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
	if [ `id -u` != 0 ] && [ ! -f ${HOME}/.rpmmacros ]
	then
	    RPMDIR="${HOME}/RPM"
	    mkdir -p ${RPMDIR}/SOURCES ${RPMDIR}/RPMS ${RPMDIR}/SRPMS \
		     ${RPMDIR}/BUILD ${RPMDIR}/SPECS
	    echo "%_topdir ${RPMDIR}" > ${HOME}/.rpmmacros
	    if [ "${BUGGY_RPM}" = "yes" ]
		then
		    cp ${hb_filename} ${RPMDIR}/SOURCES
		    cp xharbour.spec ${RPMDIR}/SPECS	
		fi
	fi	
	if [ "${BUGGY_RPM}" = "yes" ]
	then
	    rpm -ba xharbour.spec
	
	elif which rpmbuild &>/dev/null	    
	then
	    rpmbuild -ta ${hb_filename} --rmsource
	else
	    rpm -ta ${hb_filename} --rmsource
	fi
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
