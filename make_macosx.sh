#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Template to initialize the environment before starting
# the GNU make system, so xHarbour can be built in MacOS X
#
# For further information about the GNU make system please
# check doc/gmake.txt
#
# ---------------------------------------------------------------

export HB_WITHOUT_GTSLN=yes
#export HB_WITHOUT_X11=yes

. `dirname $0`/make_gnu.sh $*
