#!/bin/sh
#
# $Id: pack_src.sh,v 1.15 2005/09/02 18:25:02 druzus Exp $
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
$hb_collect Change[Ll]og*
$hb_collect COPYING

# BIN
$hb_collect bin/*.bat
$hb_collect bin/*.sh

# CONFIG
$hb_collect config/*.cf
$hb_collect config/dos/*.cf
$hb_collect config/linux/*.cf
$hb_collect config/bsd/*.cf
$hb_collect config/darwin/*.cf
$hb_collect config/sunos/*.cf
$hb_collect config/os2/*.cf
$hb_collect config/w32/*.cf

# DOC
$hb_collect doc/[Mm]akefile*
$hb_collect doc/*.txt
$hb_collect doc/en/[Mm]akefile*
$hb_collect doc/en/*.txt
$hb_collect doc/es/[Mm]akefile*
$hb_collect doc/es/*.txt

# INCLUDE
$hb_collect include/Makefile
$hb_collect include/*.[ch]
$hb_collect include/*.api
$hb_collect include/*.ch

# SOURCE\COMMON
$hb_collect source/common/Makefile
$hb_collect source/common/*.[ch]

# SOURCE
$hb_collect source/Makefile

# SOURCE\COMPILER
$hb_collect source/compiler/Makefile
$hb_collect source/compiler/*.[cylh]
$hb_collect source/compiler/*.sl[xy]
$hb_collect source/compiler/*.simple

# SOURCE\DEBUG
$hb_collect source/debug/Makefile
$hb_collect source/debug/*.prg

# SOURCE\LANG
$hb_collect source/lang/Makefile
$hb_collect source/lang/*.[ch]

# SOURCE\CODEPAGE
$hb_collect source/codepage/Makefile
$hb_collect source/codepage/*.[ch]

# SOURCE\MACRO
$hb_collect source/macro/Makefile
$hb_collect source/macro/*.[cylh]
$hb_collect source/macro/*.slx

# SOURCE\PP
$hb_collect source/pp/Makefile
$hb_collect source/pp/*.[ch]

# SOURCE\RDD
$hb_collect source/rdd/Makefile
$hb_collect source/rdd/*.[ch]
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

# SOURCE\RDD\HBSIX
$hb_collect source/rdd/hbsix/Makefile
$hb_collect source/rdd/hbsix/*.[ch]
$hb_collect source/rdd/hbsix/*.ch
$hb_collect source/rdd/hbsix/*.prg

# SOURCE\RDD\HSX
$hb_collect source/rdd/hsx/Makefile
$hb_collect source/rdd/hsx/*.[ch]
$hb_collect source/rdd/hsx/*.ch
$hb_collect source/rdd/hsx/*.prg

# SOURCE\RDD\NULSYS
$hb_collect source/rdd/nulsys/Makefile
$hb_collect source/rdd/nulsys/*.prg

for d in ${HB_DB_DRVEXT}
do
  $hb_collect source/rdd/$d/Makefile
  $hb_collect source/rdd/$d/*.[ch]
  $hb_collect source/rdd/$d/*.ch
  $hb_collect source/rdd/$d/*.prg
done

# SOURCE\RTL
$hb_collect source/rtl/Makefile
$hb_collect source/rtl/*.[ch]
$hb_collect source/rtl/*.prg

# SOURCE\RTL\PCRE
$hb_collect source/rtl/pcre/Makefile
$hb_collect source/rtl/pcre/*.[ch]

# SOURCE\RTL\GT_TPL
$hb_collect source/rtl/gt_tpl/Makefile
$hb_collect source/rtl/gt_tpl/*.[ch]

# SOURCE\RTL\GTNUL
$hb_collect source/rtl/gtnul/Makefile
$hb_collect source/rtl/gtnul/*.[ch]

# SOURCE\RTL\GTCGI
$hb_collect source/rtl/gtcgi/Makefile
$hb_collect source/rtl/gtcgi/*.[ch]

# SOURCE\RTL\GTCRS
$hb_collect source/rtl/gtcrs/Makefile
$hb_collect source/rtl/gtcrs/*.[ch]
$hb_collect source/rtl/gtcrs/*.def

# SOURCE\RTL\GTDOS
$hb_collect source/rtl/gtdos/Makefile
$hb_collect source/rtl/gtdos/*.[ch]

# SOURCE\RTL\GTOS2
$hb_collect source/rtl/gtos2/Makefile
$hb_collect source/rtl/gtos2/*.[ch]
$hb_collect source/rtl/gtos2/*.gcc

# SOURCE\RTL\GTPCA
$hb_collect source/rtl/gtpca/Makefile
$hb_collect source/rtl/gtpca/*.[ch]

# SOURCE\RTL\GTSLN
$hb_collect source/rtl/gtsln/Makefile
$hb_collect source/rtl/gtsln/*.[ch]

# SOURCE\RTL\GTSTD
$hb_collect source/rtl/gtstd/Makefile
$hb_collect source/rtl/gtstd/*.[ch]

# SOURCE\RTL\GTWIN
$hb_collect source/rtl/gtwin/Makefile
$hb_collect source/rtl/gtwin/*.[ch]

# SOURCE\RTL\GTWVT
$hb_collect source/rtl/gtwvt/Makefile
$hb_collect source/rtl/gtwvt/*.[ch]

# SOURCE\RTL\GTXVT
$hb_collect source/rtl/gtxvt/Makefile
$hb_collect source/rtl/gtxvt/*.[ch]

# SOURCE\RTL\GTXWC
$hb_collect source/rtl/gtxwc/Makefile
$hb_collect source/rtl/gtxwc/*.[ch]

# SOURCE\RTL\GTALLEG
$hb_collect source/rtl/gtalleg/Makefile
$hb_collect source/rtl/gtalleg/*.[ch]
$hb_collect source/rtl/gtalleg/*.sfc

# SOURCE\VM
$hb_collect source/vm/Makefile
$hb_collect source/vm/*.[ch]
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

# SOURCE\tip
$hb_collect source/tip/Makefile
$hb_collect source/tip/*.[ch]
$hb_collect source/tip/*.prg

# SOURCE\ct
$hb_collect source/ct/Makefile
$hb_collect source/ct/*.[ch]
$hb_collect source/ct/*.prg

# TESTS
$hb_collect tests/*.bat
$hb_collect tests/*.ch
$hb_collect tests/*.dbf
$hb_collect tests/*.fpt
$hb_collect tests/*.prg
$hb_collect tests/*.src
$hb_collect tests/*.txt

# TESTS\BLDTEST
$hb_collect tests/bldtest/Makefile
$hb_collect tests/bldtest/*.[ch]

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
$hb_collect utils/hbmake/*.[ch]

# UTILS\XBSCRIPT
$hb_collect utils/xbscript/Makefile
$hb_collect utils/xbscript/*.ch
$hb_collect utils/xbscript/*.prg
$hb_collect utils/xbscript/*.src
$hb_collect utils/xbscript/*.txt

# UTILS\XPROMPT
$hb_collect utils/xprompt/*.txt

# UTILS\HBPP
$hb_collect utils/hbpp/Makefile
$hb_collect utils/hbpp/*.[ch]

# UTILS\HBRUN
$hb_collect utils/hbrun/Makefile
$hb_collect utils/hbrun/*.prg

# UTILS\HBTEST
$hb_collect utils/hbtest/Makefile
$hb_collect utils/hbtest/*.ch
$hb_collect utils/hbtest/*.cmd
$hb_collect utils/hbtest/*.prg

# CONTRIB
$hb_collect contrib/Makefile

# CONTRIB\RDD_ADS
$hb_collect contrib/rdd_ads/Makefile
$hb_collect contrib/rdd_ads/*.[ch]
$hb_collect contrib/rdd_ads/*.prg
$hb_collect contrib/rdd_ads/*.ch

# CONTRIB\MYSQL
$hb_collect contrib/mysql/Makefile
$hb_collect contrib/mysql/*.[ch]
$hb_collect contrib/mysql/*.prg
$hb_collect contrib/mysql/*.ch

# CONTRIB\PGSQL
$hb_collect contrib/pgsql/Makefile
$hb_collect contrib/pgsql/*.[ch]
$hb_collect contrib/pgsql/*.prg
$hb_collect contrib/pgsql/*.ch

# CONTRIB\LIBNF
$hb_collect contrib/libnf/Makefile
$hb_collect contrib/libnf/*.[ch]
$hb_collect contrib/libnf/*.prg
$hb_collect contrib/libnf/*.ch
$hb_collect contrib/libnf/include/*.h
$hb_collect contrib/libnf/include/*.ch

# CONTRIB\LIBMISC
$hb_collect contrib/libmisc/Makefile
$hb_collect contrib/libmisc/*.[ch]
$hb_collect contrib/libmisc/*.prg
$hb_collect contrib/libmisc/*.ch

# CONTRIB\HTMLLIB
$hb_collect contrib/htmllib/Makefile
$hb_collect contrib/htmllib/*.prg
$hb_collect contrib/htmllib/example/*.*
$hb_collect contrib/htmllib/help/*.*
$hb_collect contrib/htmllib/include/Makefile
$hb_collect contrib/htmllib/include/*.ch

}

hb_flst=`cd "$hb_rootdir";hb_collect_all|grep -v "[*?[]"`

$hb_archbin $hb_archopt $hb_filename $hb_flst
