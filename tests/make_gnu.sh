#!/bin/sh

name="xharbour"

if [ -z "$HB_ARCHITECTURE" ]; then
   if [ "$OSTYPE" = "msdosdjgpp" ]; then
      hb_arch="dos"
   else
      hb_arch=`uname -s | tr -d "[-]" | tr '[A-Z]' '[a-z]' 2>/dev/null`
      case "$hb_arch" in
         *windows*|*mingw32*|msys*) hb_arch="w32" ;;
         *dos)                      hb_arch="dos" ;;
         *bsd)                      hb_arch="bsd" ;;
        esac
   fi
   export HB_ARCHITECTURE="$hb_arch"
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

if [ -z "$HB_GPM_MOUSE" ]; then
    if [ "$HB_ARCHITECTURE" = "linux" ] && \
       ( [ -f /usr/include/gpm.h ] || [ -f /usr/local/include/gpm.h ]); then
        HB_GPM_MOUSE=yes
    else
        HB_GPM_MOUSE=no
    fi
    export HB_GPM_MOUSE
fi

if [ -z "$HB_MT" ]; then
    case "$HB_ARCHITECTURE" in
        dos) HB_MT="" ;;
        *)   HB_MT="MT" ;;
    esac
    export HB_MT
fi

[ -z "$HB_INSTALL_PREFIX" ] && [ -n "$PREFIX" ] && export HB_INSTALL_PREFIX="$PREFIX"
[ -z "$HB_INSTALL_PREFIX" ] && export HB_INSTALL_PREFIX="/usr/local"

# Set to constant value to be consistent with the non-GNU make files.

case "$HB_INSTALL_PREFIX" in
    /usr|/usr/local|/opt)
        hb_instsubdir="/$name"
        ;;
    *)
        hb_instsubdir=""
        ;;
esac

if [ -z "$HB_BIN_INSTALL" ]; then export HB_BIN_INSTALL="$HB_INSTALL_PREFIX/bin"; fi
if [ -z "$HB_LIB_INSTALL" ]; then export HB_LIB_INSTALL="$HB_INSTALL_PREFIX/lib$hb_instsubdir"; fi
if [ -z "$HB_INC_INSTALL" ]; then export HB_INC_INSTALL="$HB_INSTALL_PREFIX/include$hb_instsubdir"; fi

make -f Makefile $*