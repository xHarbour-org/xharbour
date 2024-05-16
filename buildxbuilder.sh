#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build binaries .tgz from xHarbour sources
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

cd `dirname $0`
export HB_WITHOUT_X11=yes
. bin/hb-func.sh
export HB_PREFIX_INSTALL=/usr
export PREFIX=/usr 

export HB_WITHOUT_GTCRS=yes

export HB_COMMERCE=no
export HB_WITHOUT_GTSLN=yes


#packagesNeeded='curl jq'
if [ -x "$(command -v apk)" ];       then sudo apk add --no-cache make git bison g++ gcc ncurses-dev libc-dev unixodbc-dev libncurses5 libncurses5-dev libgpm-dev libslang2-dev libx11-dev libcurl-dev libssl-dev
elif [ -x "$(command -v apt-get)" ]; then sudo apt-get install build-essential make git bison g++ gcc ncurses-dev libc-dev unixodbc-dev libncurses5 libncurses5-dev libgpm-dev libslang2-dev libx11-dev libcurl-dev libssl-dev
elif [ -x "$(command -v dnf)" ];     then sudo dnf install make git bison cpp gcc  glibc-devel unixODBC-devel  ncurses-devel slang-devel curl-devel openssl-devel
elif [ -x "$(command -v zypper)" ];  then sudo zypper install make git bison cpp gcc  glibc-devel unixODBC-devel  ncurses-devel slang-devel curl-devel openssl-devel
elif [ -x "$(command -v yum)" ];     then sudo yum -y install make git bison cpp gcc  glibc-devel unixODBC-devel  ncurses-devel slang-devel curl-devel openssl-devel libnsl2-devel libnsl
elif [ -x "$(command -v brew)" ];    then brew install bison zlib openssl tcl-tk unixodbc && brew install --cask xquartz
else echo "FAILED TO INSTALL PACKAGE: Package manager not found. You must manually install: $packagesNeeded">&2; fi


name="xharbour"
hb_ver=`get_hbver`
hb_platform=`get_hbplatform`
[ "${hb_platform}" = "" ] || hb_platform="-${hb_platform}${HB_BUILDSUF}"
[ "${HB_XBUILD}" = "" ] || hb_platform="-${HB_XBUILD}"
hb_archfile="${name}-${hb_ver}${hb_platform}.bin.tar.gz"
hb_instfile="${name}-${hb_ver}${hb_platform}.inst.sh"
hb_lnkso="yes"
hb_pref="xhb"
hb_contrib=""
hb_sysdir="yes"
hb_exesuf=""
export C_USR="$C_USR -DHB_FM_STATISTICS_OFF -O3"

[ -z "$HB_INSTALL_PREFIX" ] && [ -n "$PREFIX" ] && export HB_INSTALL_PREFIX="$PREFIX"

if [ -z "$TMPDIR" ]; then TMPDIR="/tmp"; fi
HB_INST_PREF="$HB_INSTALL_PREFIX"

if [ -z "$HB_ARCHITECTURE" ]; then
    if [ "$OSTYPE" = "msdosdjgpp" ]; then
        hb_arch="dos"
    else
        hb_arch=`uname -s | tr -d "[-]" | tr '[A-Z]' '[a-z]' 2>/dev/null`
        case "$hb_arch" in
            *windows*|*mingw32*|msys*)    hb_arch="w32" ;;
            *dos)   hb_arch="dos" ;;
            *bsd)   hb_arch="bsd" ;;
        esac
    fi
    export HB_ARCHITECTURE="$hb_arch"
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
        os2) HB_GT_LIB="gtos2" ;;
        *)   HB_GT_LIB="gttrm" ;;
    esac
    export HB_GT_LIB
fi

if [ -z "$HB_MT" ]; then
    case "$HB_ARCHITECTURE" in
        dos) HB_MT="" ;;
        *)   HB_MT="MT" ;;
    esac
    export HB_MT
fi

if [ -z "$HB_COMMERCE" ]; then export HB_COMMERCE=no; fi

# default lib dir name
HB_LIBDIRNAME="lib"

ETC="/etc"

HB_ARCH64=""
if [ "$HB_ARCHITECTURE" = "linux" ]
then
    HB_CPU=`uname -m`
    case "$HB_CPU" in
        *[_@]64)
            export C_USR="$C_USR -fPIC"
            HB_ARCH64="yes"
            ;;
        *)
            ;;
    esac
elif [ "$HB_ARCHITECTURE" = "hpux" ]
then
    export C_USR="$C_USR -fPIC"
fi

# Select the platform-specific installation prefix and ownership
HB_INSTALL_OWNER=root
case "$HB_ARCHITECTURE" in
    darwin)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr/local"
        HB_INSTALL_GROUP=wheel
        ETC="/private/etc"
        LIB_NAMES=("zlib" "openssl" "X11" "unixodbc")
        LIB_PREFIXES=($(brew --prefix zlib) $(brew --prefix openssl) "/opt/X11" $(brew --prefix unixodbc))
        for index in ${!LIB_NAMES[@]}; do
            export C_USR="$C_USR -I${LIB_PREFIXES[$index]}/include"
            export L_USR="$L_USR -L${LIB_PREFIXES[$index]}/lib"
        done
        ;;
    linux)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr/local"
        [ -d "$HB_INSTALL_PREFIX/lib64" ] && [ "${HB_ARCH64}" = yes ] && HB_LIBDIRNAME="lib64"
        HB_INSTALL_GROUP=root
        ;;
    w32)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr/local"
        HB_INSTALL_GROUP=0
        hb_sysdir="no"
        hb_exesuf=".exe"
        hb_instfile=""
        ;;
    dos)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/${name}"
        HB_INSTALL_GROUP=root
        hb_lnkso="no"
        hb_sysdir="no"
        hb_exesuf=".exe"
        hb_instfile=""
        hb_archfile="${name}.tgz"
        HB_INST_PREF="$TMPDIR/hb-$$"
        ;;
    *)
        [ -z "$HB_INSTALL_PREFIX" ] && HB_INSTALL_PREFIX="/usr/local"
        HB_INSTALL_GROUP=wheel
        ;;
esac

# Select the platform-specific command names
MAKE=make
TAR=tar
hb_gnutar=yes
if gtar --version >/dev/null 2>&1; then
   TAR=gtar
elif ! tar --version >/dev/null 2>&1; then
   hb_gnutar=no
   echo "Warning!!! Cannot find GNU TAR"
fi
if gmake --version >/dev/null 2>&1; then
   MAKE=gmake
elif ! make --version >/dev/null 2>&1; then
   echo "Warning!!! Cannot find GNU MAKE"
fi

# Set other platform-specific build options
if [ -z "$HB_GPM_MOUSE" ]; then
    if [ "$HB_ARCHITECTURE" = "linux" ] && \
       ( [ -f /usr/include/gpm.h ] || [ -f /usr/local/include/gpm.h ]); then
        HB_GPM_MOUSE=yes
    else
        HB_GPM_MOUSE=no
    fi
    export HB_GPM_MOUSE
fi

if [ -z "${HB_WITHOUT_GTSLN}" ]; then
    HB_WITHOUT_GTSLN=yes
    case "$HB_ARCHITECTURE" in
        linux|bsd|darwin|hpux|sunos)
            for dir in /usr /usr/local /sw /opt/local
            do
                if [ -f ${dir}/include/slang.h ] || \
                   [ -f ${dir}/include/slang/slang.h ]; then
                    HB_WITHOUT_GTSLN=no
                fi
            done
            ;;
    esac
    export HB_WITHOUT_GTSLN
fi

case "$HB_ARCHITECTURE" in
    linux)
        ;;
    darwin)
        # Autodetect old Darwin versions and set appropriate build options
        if [ `uname -r | sed "s/\..*//g"` -lt 6 ]; then
            export HB_NCURSES_FINK=yes
        fi
        [ -z "$HB_WITHOUT_X11" ] && export HB_WITHOUT_X11=yes
        ;;
    dos|w32)
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
#$MAKE -r clean
$MAKE -r >2 2>&1
cd contrib
     $MAKE -r clean
     $MAKE -r >>../2 2>&1
cd ../xHarbourBuilder
     $MAKE -r clean
     $MAKE -r >>../2 2>&1
cd ..
# install
#rm -fR "${HB_INST_PREF}"

#export _DEFAULT_BIN_DIR=$HB_BIN_INSTALL
#export _DEFAULT_INC_DIR=$HB_INC_INSTALL
#export _DEFAULT_LIB_DIR=$HB_LIB_INSTALL
#export HB_BIN_INSTALL="$HB_INST_PREF$HB_BIN_INSTALL"
#export HB_INC_INSTALL="$HB_INST_PREF$HB_INC_INSTALL"
#export HB_LIB_INSTALL="$HB_INST_PREF$HB_LIB_INSTALL"

#mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL
$MAKE -r -i install
cd contrib
     $MAKE -r -i install

cd ../xHarbourBuilder
     $MAKE -r -i install

cd ..

