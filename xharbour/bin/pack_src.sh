#!/bin/sh
#
# $Id: pack_src.sh,v 1.2 2003/12/21 21:31:52 druzus Exp $
#
# This script requires "TAR" utilities for compression.

hb_archbin="tar"
hb_archopt="-cz --ignore-failed-read -f"
hb_ext=".tar.gz"
if [ -f bin/hb-func.sh ]; then
  hb_rootdir="."
else
  hb_rootdir=`dirname $0`
  hb_rootdir="${hb_rootdir}/.."
  hb_archopt="-C $hb_rootdir $hb_archopt"
fi
. ${hb_rootdir}/bin/hb-func.sh

hb_ver=`get_hbver ${hb_rootdir}`
hb_filename="xharbour-${hb_ver}.src${hb_ext}"
[ -f $hb_filename ] && rm -f $hb_filename

#[ -z "$TZ" ] && export TZ=PST8PDT

hb_collect_all()
{

hb_collect="echo"

# README.TXT

# ROOT
$hb_collect *.bat
$hb_collect *.sh
$hb_collect *.cmd
$hb_collect *.spec
$hb_collect [Mm]akefile*
$hb_collect ChangeLog

# BIN
$hb_collect bin/*.bat
$hb_collect bin/*.sh

# CONFIG
$hb_collect config/*.cf
$hb_collect config/bsd/*.cf
$hb_collect config/dos/*.cf
$hb_collect config/linux/*.cf
$hb_collect config/os2/*.cf
$hb_collect config/w32/*.cf

# DOC
$hb_collect doc/*.txt
$hb_collect doc/en/*.txt
$hb_collect doc/es/*.txt

# INCLUDE
$hb_collect include/Makefile
$hb_collect include/*.c
$hb_collect include/*.api
$hb_collect include/*.h
$hb_collect include/*.ch

# SOURCE\COMMON
$hb_collect source/common/Makefile
$hb_collect source/common/*.c

# SOURCE
$hb_collect source/Makefile

# SOURCE\COMPILER
$hb_collect source/compiler/Makefile
$hb_collect source/compiler/*.c
$hb_collect source/compiler/*.slx
$hb_collect source/compiler/*.sly

# SOURCE\DEBUG
$hb_collect source/debug/Makefile
$hb_collect source/debug/*.prg

# SOURCE\LANG
$hb_collect source/lang/Makefile
$hb_collect source/lang/*.c

# SOURCE\CODEPAGE
$hb_collect source/codepage/Makefile
$hb_collect source/codepage/*.c

# SOURCE\MACRO
$hb_collect source/macro/Makefile
$hb_collect source/macro/*.c
$hb_collect source/macro/*.slx
$hb_collect source/macro/*.y

# SOURCE\PP
$hb_collect source/pp/Makefile
$hb_collect source/pp/*.c

# SOURCE\RDD
$hb_collect source/rdd/Makefile
$hb_collect source/rdd/*.c
$hb_collect source/rdd/*.prg

# SOURCE\RDD\DBFDBT
$hb_collect source/rdd/dbfdbt/Makefile
$hb_collect source/rdd/dbfdbt/*.[ch]
$hb_collect source/rdd/dbfdbt/*.prg

# SOURCE\RDD\DBFFPT
$hb_collect source/rdd/dbffpt/Makefile
$hb_collect source/rdd/dbffpt/*.[ch]
$hb_collect source/rdd/dbffpt/*.prg

# SOURCE\RDD\DBFCDX
$hb_collect source/rdd/dbfcdx/Makefile
$hb_collect source/rdd/dbfcdx/*.[ch]
$hb_collect source/rdd/dbfcdx/*.prg

# SOURCE\RDD\DBFNTX
$hb_collect source/rdd/dbfntx/Makefile
$hb_collect source/rdd/dbfntx/*.[ch]
$hb_collect source/rdd/dbfntx/*.prg

# SOURCE\RDD\NULSYS
$hb_collect source/rdd/nulsys/Makefile
$hb_collect source/rdd/nulsys/*.prg

# SOURCE\RTL
$hb_collect source/rtl/Makefile
$hb_collect source/rtl/*.c
$hb_collect source/rtl/*.prg

# SOURCE\RTL\GT_TPL
$hb_collect source/rtl/gt_tpl/Makefile
$hb_collect source/rtl/gt_tpl/*.c
$hb_collect source/rtl/gt_tpl/*.h

# SOURCE\RTL\GTNUL
$hb_collect source/rtl/gtnul/Makefile
$hb_collect source/rtl/gtnul/*.c
$hb_collect source/rtl/gtnul/*.h

# SOURCE\RTL\GTCGI
$hb_collect source/rtl/gtcgi/Makefile
$hb_collect source/rtl/gtcgi/*.c
$hb_collect source/rtl/gtcgi/*.h

# SOURCE\RTL\GTCRS
$hb_collect source/rtl/gtcrs/Makefile
$hb_collect source/rtl/gtcrs/*.c
$hb_collect source/rtl/gtcrs/*.h
$hb_collect source/rtl/gtcrs/*.def

# SOURCE\RTL\GTDOS
$hb_collect source/rtl/gtdos/Makefile
$hb_collect source/rtl/gtdos/*.c
$hb_collect source/rtl/gtdos/*.h

# SOURCE\RTL\GTOS2
$hb_collect source/rtl/gtos2/Makefile
$hb_collect source/rtl/gtos2/*.c
$hb_collect source/rtl/gtos2/*.h
$hb_collect source/rtl/gtos2/*.gcc

# SOURCE\RTL\GTPCA
$hb_collect source/rtl/gtpca/Makefile
$hb_collect source/rtl/gtpca/*.c
$hb_collect source/rtl/gtpca/*.h

# SOURCE\RTL\GTSLN
$hb_collect source/rtl/gtsln/Makefile
$hb_collect source/rtl/gtsln/*.c
$hb_collect source/rtl/gtsln/*.h

# SOURCE\RTL\GTSTD
$hb_collect source/rtl/gtstd/Makefile
$hb_collect source/rtl/gtstd/*.c
$hb_collect source/rtl/gtstd/*.h

# SOURCE\RTL\GTWIN
$hb_collect source/rtl/gtwin/Makefile
$hb_collect source/rtl/gtwin/*.c
$hb_collect source/rtl/gtwin/*.h

# SOURCE\RTL\GTWVT
$hb_collect source/rtl/gtwvt/Makefile
$hb_collect source/rtl/gtwvt/*.c
$hb_collect source/rtl/gtwvt/*.h

# SOURCE\RTL\GTXVT
$hb_collect source/rtl/gtxvt/Makefile
$hb_collect source/rtl/gtxvt/*.c
$hb_collect source/rtl/gtxvt/*.h

# SOURCE\VM
$hb_collect source/vm/Makefile
$hb_collect source/vm/*.c
$hb_collect source/vm/*.prg

# SOURCE\samples
$hb_collect source/clipsamp/Makefile
$hb_collect source/clipsamp/*.c
$hb_collect source/clipsamp/*.ch
$hb_collect source/clipsamp/*.prg

# SOURCE\odbc
$hb_collect source/odbc/Makefile
$hb_collect source/odbc/*.c
$hb_collect source/odbc/*.ch
$hb_collect source/odbc/*.prg

# TESTS
$hb_collect tests/*.bat
$hb_collect tests/*.ch
$hb_collect tests/*.dbf
$hb_collect tests/*.fpt
$hb_collect tests/*.prg
$hb_collect tests/*.src
$hb_collect tests/*.txt

# UTILS
$hb_collect utils/Makefile

# UTILS\HBDOC
$hb_collect utils/hbdoc/Makefile
$hb_collect utils/hbdoc/*.ch
$hb_collect utils/hbdoc/*.prg

# UTILS\HBEXTERN
$hb_collect utils/hbextern/Makefile
$hb_collect utils/hbextern/*.bat
$hb_collect utils/hbextern/*.prg

# UTILS\HBMAKE
$hb_collect utils/hbmake/Makefile
$hb_collect utils/hbmake/*.ch
$hb_collect utils/hbmake/*.prg
$hb_collect utils/hbmake/*.c

# UTILS\HBPP
$hb_collect utils/hbpp/Makefile
$hb_collect utils/hbpp/*.c

# UTILS\HBRUN
$hb_collect utils/hbrun/Makefile
$hb_collect utils/hbrun/*.prg

# UTILS\HBTEST
$hb_collect utils/hbtest/Makefile
$hb_collect utils/hbtest/*.ch
$hb_collect utils/hbtest/*.cmd
$hb_collect utils/hbtest/*.prg

# CONTRIB\LIBCT
$hb_collect contrib/libct/Makefile
$hb_collect contrib/libct/*.[ch]
$hb_collect contrib/libct/*.prg
$hb_collect contrib/libct/*.ch
$hb_collect contrib/libct/include/*.h
$hb_collect contrib/libct/include/*.ch

$hb_collect contrib/rdd_ads/Makefile
$hb_collect contrib/rdd_ads/*.c
$hb_collect contrib/rdd_ads/*.prg
$hb_collect contrib/rdd_ads/*.h
$hb_collect contrib/rdd_ads/*.ch

$hb_collect contrib/mysql/Makefile
$hb_collect contrib/mysql/*.c
$hb_collect contrib/mysql/*.prg
$hb_collect contrib/mysql/*.h
$hb_collect contrib/mysql/*.ch

$hb_collect contrib/libnf/Makefile
$hb_collect contrib/libnf/*.[ch]
$hb_collect contrib/libnf/*.prg
$hb_collect contrib/libnf/*.ch
$hb_collect contrib/libnf/include/*.h
$hb_collect contrib/libnf/include/*.ch

}

hb_flst=`cd "$hb_rootdir";hb_collect_all|grep -v "[*?[]"`

$hb_archbin $hb_archopt $hb_filename $hb_flst
