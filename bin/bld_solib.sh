#!/bin/sh
#
# Shared object file creators
# By Giancarlo Niccolai
#
# $Id$
#
# Based on the work of Przemyslav Czerpak
#

#
# usage!
#

usage()
{
	echo "usage: $(basename $0) [parameters]"
	echo "  -s 		create single threaded library (default to HB_MT)"
	echo "  -n 		Don't strip debug symobl fron target"
	echo "  -t 		create thread-aware library (default to HB_MT)"
	echo "  -g gtlib	Use gtlib as HB_GT_LIB"
	echo "  -i, --install	install also the library (def /usr/local/lib)"
	echo "  -I instdir	Install into instdir"		
	echo "  -?, --usage	this screen"
	echo ""
}



#
# Real call to Przemyslav hb-mkslib command
#

call_mkslib()
{
	HB_GT=${HB_GT_LIB#gt}
	OUTPUT=libharbour$HB_MT-$HB_GT.so
	echo "$(basename $0): Making $HB_LIB_INSTALL/$OUTPUT"

	$HB_BIN_INSTALL/hb-mkslib.sh $OUTPUT \
		libcommon.a libdbfdbt.a libdbffpt.a \
		libdbfcdx$HB_MT.a libdbfntx$HB_MT.a libdebug.a \
		liblang.a libmacro$HB_MT.a libpp$HB_MT.a librdd$HB_MT.a \
		lib$HB_GT_LIB.a \
		librtl$HB_MT.a libvm$HB_MT.a  \
		libcodepage.a    
		
	if [ "$?" = "0" -a "$dont_strip" = "false" ]; then
		strip -S $OUTPUT
	fi
}


#
# Initialization
#

INSTALL_DIR=/usr/local/lib
VERSION="0.8.2"
VERBASE="0"
OLD_PWD=$(pwd)

#check for environment to be complete
if [ -z "$HB_BIN_INSTALL" ]; then
	echo "$(basename $0): Set HB_BIN_INSTALL variable to where 'harbour'"
	echo "binary resides."
	exit
fi

if [ -z "$HB_LIB_INSTALL" ]; then
	echo "$(basename $0): Set HB_LIB_INSTALL variable to where xharbour "
	echo "libraries are installed"
	exit
fi

if [ -z $HB_GT_LIB ]; then 
	HB_GT_LIB=gtsln
fi

test "x$HB_MT" = "xMT" && HB_MT="mt"


#
# Step 0: parameter parsing
#
#
	
do_install='false'
mt_suffix=''
dont_strip='false'

while [ -n "$*" ]; do
	
	case $1 in
		--usage|"-?") 
			usage
			exit
		;;

		-g)
			shift
			HB_GT_LIB=$1
		;;
		
		-s)
			HB_MT=
		;;
		
		-t)
			HB_MT=mt
		;;

		-n)
			dont_strip='true'
		;;
			
		
		-i|--install) 
			do_install='true'
		;;	
		
		-I) 
			shift
			INSTALL_DIR=$1
		;;	
		
		*)
			echo "Unrecognized parameter $1"
			echo ""
			usage
			exit
			;;
	esac
	
	shift
done

#Step 1: making 

cd $HB_LIB_INSTALL
call_mkslib

#Step 2: installing
if test "x$do_install" = "xtrue"; then
	echo "$(basename $0): Installing on $INSTALL_DIR"
	mv $OUTPUT $INSTALL_DIR/$OUTPUT.$VERSION
	ln -sf $INSTALL_DIR/$OUTPUT.$VERSION $INSTALL_DIR/$OUTPUT.$VERBASE
	ln -sf $INSTALL_DIR/$OUTPUT.$VERSION $INSTALL_DIR/$OUTPUT
	ldconfig
fi

#Step 3: terminating

cd $OLD_PWD

echo "$(basename $0): done"
#
# End of make_so.sh
#

