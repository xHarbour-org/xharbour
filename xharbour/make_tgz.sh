#!/bin/sh
#
# $Id: make_tgz.sh,v 1.36 2005/01/09 20:39:46 likewolf Exp $
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build binaries .tgz from xHarbour sources
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

cd `dirname $0`
. bin/hb-func.sh

name="xharbour"
hb_ver=`get_hbver`
hb_platform=`get_hbplatform`
[ "${hb_platform}" = "" ] || hb_platform="-${hb_platform}"
hb_archfile="${name}-${hb_ver}${hb_platform}.bin.tar.gz"
hb_instfile="${name}-${hb_ver}${hb_platform}.inst.sh"
hb_lnkso="yes"
hb_pref="xhb"
hb_contrib=""
hb_sysdir="yes"
export C_USR="-DHB_FM_STATISTICS_OFF -O3"

if [ -z "$HB_ARCHITECTURE" ]; then
    if [ "$OSTYPE" = "msdosdjgpp" ]; then
        hb_arch="dos"
    else
        hb_arch=`uname -s | tr -d "[-]" | tr '[A-Z]' '[a-z]' 2>/dev/null`
        case "$hb_arch" in
            *windows*) hb_arch="w32" ;;
            *dos)      hb_arch="dos" ;;
            *bsd)      hb_arch="bsd" ;;
        esac
        export HB_ARCHITECTURE="$hb_arch"
    fi
fi

if [ -z "$HB_COMPILER" ]; then
    case "$HB_ARCHITECTURE" in
        w32) HB_COMPILER="mingw32" ;;
        dos) HB_COMPILER="djgpp" ;;
        *)   HB_COMPILER="gcc" ;;
    esac
    export HB_COMPILER
fi

if [ -z "$HB_GT_LIB" ]; then
    case "$HB_ARCHITECTURE" in
        w32) HB_GT_LIB="gtwin" ;;
        dos) HB_GT_LIB="gtdos" ;;
        *)   HB_GT_LIB="gtcrs" ;;
    esac
    export HB_GT_LIB
fi

if [ -z "$HB_MULTI_GT" ]; then export HB_MULTI_GT=yes; fi
if [ -z "$HB_MT" ]; then export HB_MT=MT; fi
if [ -z "$HB_COMMERCE" ]; then export HB_COMMERCE=no; fi

# default lib dir name
HB_LIBDIRNAME="lib"

# Select the platform-specific installation prefix and ownership
HB_INSTALL_OWNER=root
case "$HB_ARCHITECTURE" in
    linux)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr"
        [ -d "$HB_INSTALL_PREFIX/lib64" ] && HB_LIBDIRNAME="lib64"
        HB_INSTALL_GROUP=root
        ;;
    dos)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/${name}"
        HB_INSTALL_GROUP=root
        hb_sysdir="no"
        ;;
    *)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr/local"
        HB_INSTALL_GROUP=wheel
        ;;
esac

# Select the platform-specific command names
INSTALL=install
MAKE=make
TAR=tar
case "$HB_ARCHITECTURE" in
    darwin) TAR=gtar; INSTALL="install -c" ;;
    bsd)    MAKE=gmake ;;
esac

# Select the contribs to build
case "$HB_ARCHITECTURE" in
    linux)  hb_contrib="htmllib libmisc libnf rdd_ads" ;;
    bsd)    hb_contrib="htmllib libmisc libnf" ;;
    darwin) hb_contrib="htmllib libmisc libnf" ;;
esac

if [ -z "$HB_GPM_MOUSE" ]; then
    if [ "$HB_ARCHITECTURE" = "linux" ] && \
       ( [ -f /usr/include/gpm.h ] || [ -f /usr/local/include/gpm.h ]); then
        HB_GPM_MOUSE=yes
    else
        HB_GPM_MOUSE=no
    fi
    export HB_GPM_MOUSE
fi

# Set other platform-specific build options
case "$HB_ARCHITECTURE" in
    linux)
        ;;
    dos)
        hb_lnkso="no"
        ;;
    darwin)
        # Autodetect old Darwin versions and set appropriate build options
        if [ `uname -r | sed "s/\..*//g"` -lt 6 ]; then
            export HB_NCURSES_FINK=yes
        fi
        [ -z "$HB_WITHOUT_X11" ] && export HB_WITHOUT_X11=yes
        ;;
    *)
        [ -z "$HB_WITHOUT_X11" ] && export HB_WITHOUT_X11=yes
        ;;
esac

if [ "$HB_COMMERCE" = yes ]
then
   export HB_GPM_MOUSE=no
   export HB_WITHOUT_GTSLN=yes
fi

if [ "${hb_sysdir}" = "yes" ]; then
    export HB_BIN_INSTALL="$HB_INSTALL_PREFIX/bin"
    export HB_INC_INSTALL="$HB_INSTALL_PREFIX/include/${name}"
    export HB_LIB_INSTALL="$HB_INSTALL_PREFIX/$HB_LIBDIRNAME/${name}"
else
    export HB_BIN_INSTALL="$HB_INSTALL_PREFIX/bin"
    export HB_INC_INSTALL="$HB_INSTALL_PREFIX/include"
    export HB_LIB_INSTALL="$HB_INSTALL_PREFIX/$HB_LIBDIRNAME"
fi

# build
umask 022
$MAKE -r clean
$MAKE -r
for l in ${hb_contrib}
do
    (cd "contrib/$l"
     $MAKE -r clean
     $MAKE -r)
done

# install
if [ -z "$TMPDIR" ]; then TMPDIR="/tmp"; fi
HB_INST_PREF="$TMPDIR/$name.bin.$USER.$$"
rm -fR "${HB_INST_PREF}"

export _DEFAULT_BIN_DIR=$HB_BIN_INSTALL
export _DEFAULT_INC_DIR=$HB_INC_INSTALL
export _DEFAULT_LIB_DIR=$HB_LIB_INSTALL
export HB_BIN_INSTALL="$HB_INST_PREF/$HB_BIN_INSTALL"
export HB_INC_INSTALL="$HB_INST_PREF/$HB_INC_INSTALL"
export HB_LIB_INSTALL="$HB_INST_PREF/$HB_LIB_INSTALL"

mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL
$MAKE -r -i install
for l in ${hb_contrib}
do
    (cd "contrib/$l"
     $MAKE -r -i install)
done

# Keep the size of the binaries to a minimim.
strip $HB_BIN_INSTALL/harbour
# Keep the size of the libraries to a minimim, but don't try to strip symlinks.
strip -S `find $HB_LIB_INSTALL -type f`

if [ "${hb_sysdir}" = "yes" ]; then

mkdir -p $HB_INST_PREF/etc/harbour
$INSTALL -m644 source/rtl/gtcrs/hb-charmap.def $HB_INST_PREF/etc/harbour/hb-charmap.def

cat > $HB_INST_PREF/etc/harbour.cfg <<EOF
CC=gcc
CFLAGS=-c -I$_DEFAULT_INC_DIR -O3
VERBOSE=YES
DELTMP=YES
EOF

fi

# check if we should rebuild tools with shared libs
if [ "${hb_lnkso}" = yes ]
then
    ADD_LIBS=""
    case $HB_ARCHITECTURE in
        linux)  [ "${HB_GPM_MOUSE}" = yes ] && ADD_LIBS="$ADD_LIBS -lgpm" ;;
        darwin) ADD_LIBS="$ADD_LIBS -L/sw/lib" ;;
    esac 
    [ "${HB_WITHOUT_GTSLN}" != yes ] && ADD_LIBS="$ADD_LIBS -lslang"
    [ "${HB_WITHOUT_X11}" != yes ] && ADD_LIBS="$ADD_LIBS -L/usr/X11R6/$HB_LIBDIRNAME -lX11"

    export L_USR="-L${HB_LIB_INSTALL} -l${name} -lncurses ${ADD_LIBS}"
    export PRG_USR="\"-D_DEFAULT_INC_DIR='${_DEFAULT_INC_DIR}'\""

    for utl in hbmake hbrun hbpp hbdoc hbtest hbdict xbscript
    do
        (cd "utils/${utl}"
         rm -fR "./${HB_ARCHITECTURE}"
         $MAKE -r install
         strip "${HB_BIN_INSTALL}/${utl}")
    done
fi
ln -s xbscript ${HB_BIN_INSTALL}/pprun
ln -s xbscript ${HB_BIN_INSTALL}/xprompt


$TAR -czvf "${hb_archfile}" --owner=${HB_INSTALL_OWNER} --group=${HB_INSTALL_GROUP} -C "${HB_INST_PREF}" .
rm -fR "${HB_INST_PREF}"

if [ "${HB_ARCHITECTURE}" = darwin ]; then
  DO_LDCONFIG=""
else
  DO_LDCONFIG="&& ldconfig"
fi
cat > "${hb_instfile}" <<EOF
#!/bin/sh
if [ "\$1" == "--extract" ]; then
    sed -e '1,/^HB_INST_EOF\$/ d' \$0 > "${hb_archfile}"
    exit
fi
if [ \`id -u\` != 0 ]; then
    echo "This package has to be installed from root account."
    exit 1
fi
echo "Do you want to install ${name} (y/n)"
read ASK
if [ "\${ASK}" != "y" ] && [ "\${ASK}" != "Y" ]; then
    exit 1
fi
(sed -e '1,/^HB_INST_EOF\$/ d' \$0 | gzip -cd | $TAR xvf - -C /) ${DO_LDCONFIG}
exit \$?
HB_INST_EOF
EOF
cat "${hb_archfile}" >> "${hb_instfile}"
chmod +x "${hb_instfile}"
rm -f "${hb_archfile}"
