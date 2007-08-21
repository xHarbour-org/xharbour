#!/bin/sh
#
# $Id: make_drw.sh,v 1.11 2005/03/13 12:44:13 likewolf Exp $
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
export HB_WITHOUT_X11=yes

. `dirname $0`/make_gnu.sh $*
