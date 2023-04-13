#!/bin/sh
#
# $Id$
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
#$hb_collect source/rdd/dbfdbt/Makefile
#$hb_collect source/rdd/dbfdbt/*.[ch]
#$hb_collect source/rdd/dbfdbt/*.prg

# SOURCE\RDD\DBFFPT
$hb_collect source/rdd/dbffpt/Makefile
$hb_collect source/rdd/dbffpt/*.[ch]
$hb_collect source/rdd/dbffpt/*.prg

# SOURCE\RDD\DBFCDX
$hb_collect source/rdd/dbfcdx/Makefile
$hb_collect source/rdd/dbfcdx/*.[ch]
$hb_collect source/rdd/dbfcdx/*.prg

# SOURCE\RDD\BMDBFCDX
$hb_collect source/rdd/bmdbfcdx/Makefile
$hb_collect source/rdd/bmdbfcdx/*.[ch]
$hb_collect source/rdd/bmdbfcdx/*.prg


# SOURCE\RDD\DBFNSX
$hb_collect source/rdd/dbfnsx/Makefile
$hb_collect source/rdd/dbfnsx/*.[ch]
$hb_collect source/rdd/dbfnsx/*.prg

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

# SOURCE\RDD\USRRDD
$hb_collect source/rdd/usrrdd/Makefile
$hb_collect source/rdd/usrrdd/*.c
$hb_collect source/rdd/usrrdd/example/*.prg
$hb_collect source/rdd/usrrdd/rdds/Makefile
$hb_collect source/rdd/usrrdd/rdds/*.prg

# SOURCE\RDD\NULSYS
$hb_collect source/rdd/nulsys/Makefile
$hb_collect source/rdd/nulsys/*.c

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
#source\xhb\zlib
$hb_collect source/rtl/zlib/Makefile
$hb_collect source/rtl/zlib/*.[ch]
$hb_collect source/rtl/zlib/*.h

# SOURCE\RTL\PCRE
$hb_collect source/rtl/pcre/Makefile
$hb_collect source/rtl/pcre/*.[ch]
$hb_collect source/rtl/pcre/*.[ch].generic
$hb_collect source/rtl/pcre/*.[ch].dist

# SOURCE\RTL\GT_TPL
$hb_collect source/rtl/gt_tpl/Makefile
$hb_collect source/rtl/gt_tpl/*.[ch]

# SOURCE\RTL\GTNUL
#$hb_collect source/rtl/gtnul/Makefile
#$hb_collect source/rtl/gtnul/*.[ch]

# SOURCE\RTL\GTCGI
$hb_collect source/rtl/gtcgi/Makefile
$hb_collect source/rtl/gtcgi/*.[ch]

# SOURCE\RTL\GTCRS
$hb_collect source/rtl/gtcrs/Makefile
$hb_collect source/rtl/gtcrs/*.[ch]
$hb_collect source/rtl/gtcrs/*.def

# SOURCE\RTL\GTTRM
$hb_collect source/rtl/gttrm/Makefile
$hb_collect source/rtl/gttrm/*.[ch]
$hb_collect source/rtl/gttrm/*.def

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

# SOURCE\VM\FMSTAT
$hb_collect source/vm/fmstat/Makefile
$hb_collect source/vm/fmstat/*.[ch]
$hb_collect source/vm/fmstat/*.prg

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

# SOURCE\cgi
$hb_collect source/cgi/Makefile
$hb_collect source/cgi/*.ch
$hb_collect source/cgi/*.prg
$hb_collect source/cgi/include/Makefile
$hb_collect source/cgi/include/*.ch

# contrib\misc
$hb_collect contrib/misc/Makefile
$hb_collect contrib/misc/*.c
$hb_collect contrib/misc/*.ch
$hb_collect contrib/misc/*.prg

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

#contrib\sqlite3 
$hb_collect contrib/hbsqlit3/Makefile
$hb_collect contrib/hbsqlit3/config.h
$hb_collect contrib/hbsqlit3/sqlite3.c
$hb_collect contrib/hbsqlit3/sqlite3.h
$hb_collect contrib/hbsqlit3/sqlite3ext.h
$hb_collect contrib/hbsqlit3/xhbsqlite3.c
$hb_collect contrib/hbsqlit3/tests
$hb_collect contrib/hbsqlit3/tests/authorizer.prg
$hb_collect contrib/hbsqlit3/tests/blob.prg
$hb_collect contrib/hbsqlit3/tests/pngtest.png
$hb_collect contrib/hbsqlit3/tests/sqlite3.ch
$hb_collect contrib/hbsqlit3/tests/sqlite3_test.prg
  
#contrib\hbcairo
$hb_collect contrib/hbcairo/Makefile
$hb_collect contrib/hbcairo/tests
$hb_collect contrib/hbcairo/context.c
$hb_collect contrib/hbcairo/core.c
$hb_collect contrib/hbcairo/hbcairo.ch
$hb_collect contrib/hbcairo/hbcairo.h
$hb_collect contrib/hbcairo/image.c
$hb_collect contrib/hbcairo/paths.c
$hb_collect contrib/hbcairo/pdf.c
$hb_collect contrib/hbcairo/png.c
$hb_collect contrib/hbcairo/ps.c
$hb_collect contrib/hbcairo/surface.c
$hb_collect contrib/hbcairo/text.c
$hb_collect contrib/hbcairo/transfor.c
$hb_collect contrib/hbcairo/util.c
$hb_collect contrib/hbcairo/tests/fancytxt.prg
$hb_collect contrib/hbcairo/tests/glyphdbg.prg
$hb_collect contrib/hbcairo/tests/hellow.prg
$hb_collect contrib/hbcairo/tests/lightning.prg
$hb_collect contrib/hbcairo/tests/table.prg
#contrib\hbzebra
$hb_collect contrib/hbzebra/Makefile
$hb_collect contrib/hbzebra/tests
$hb_collect contrib/hbzebra/codabar.c
$hb_collect contrib/hbzebra/code11.c
$hb_collect contrib/hbzebra/code128.c
$hb_collect contrib/hbzebra/code39.c
$hb_collect contrib/hbzebra/code93.c
$hb_collect contrib/hbzebra/core.c
$hb_collect contrib/hbzebra/coredraw.c
$hb_collect contrib/hbzebra/datamtrx.c
$hb_collect contrib/hbzebra/eanupc.c
$hb_collect contrib/hbzebra/hbzebra.ch
$hb_collect contrib/hbzebra/hbzebra.h
$hb_collect contrib/hbzebra/itf.c
$hb_collect contrib/hbzebra/msi.c
$hb_collect contrib/hbzebra/pdf417.c
$hb_collect contrib/hbzebra/qrcode.c
$hb_collect contrib/hbzebra/tests/testcair.prg
$hb_collect contrib/hbzebra/tests/testhpdf.prg

#contrib/hbbz2
$hb_collect contrib/hbbz2/Makefile
$hb_collect contrib/hbbz2/tests
$hb_collect contrib/hbbz2/blocksor.c
$hb_collect contrib/hbbz2/bzlib.c
$hb_collect contrib/hbbz2/bzlib.h
$hb_collect contrib/hbbz2/bzlib_pr.h
$hb_collect contrib/hbbz2/compress.c
$hb_collect contrib/hbbz2/core.c
$hb_collect contrib/hbbz2/crctable.c
$hb_collect contrib/hbbz2/decompre.c
$hb_collect contrib/hbbz2/hbbz2.ch
$hb_collect contrib/hbbz2/huffman.c
$hb_collect contrib/hbbz2/randtabl.c
$hb_collect contrib/hbbz2/tests/test.prg


#contrib/hblzf
$hb_collect contrib/hblzf/Makefile
$hb_collect contrib/hblzf/tests
$hb_collect contrib/hblzf/core.c
$hb_collect contrib/hblzf/crc32.h
$hb_collect contrib/hblzf/hblzf.ch
$hb_collect contrib/hblzf/lzf.h
$hb_collect contrib/hblzf/lzfP.h
$hb_collect contrib/hblzf/lzf_c.c
$hb_collect contrib/hblzf/lzf_d.c
$hb_collect contrib/hblzf/tests/test.prg
$hb_collect contrib/hblzf/tests/test2.prg
 

#contrib/hbmlzo
$hb_collect contrib/hbmlzo/Makefile
$hb_collect contrib/hbmlzo/tests
$hb_collect contrib/hbmlzo/core.c
$hb_collect contrib/hbmlzo/hbmlzo.ch
$hb_collect contrib/hbmlzo/lzoconf.h
$hb_collect contrib/hbmlzo/lzodefs.h
$hb_collect contrib/hbmlzo/minilzo.c
$hb_collect contrib/hbmlzo/minilzo.h
$hb_collect contrib/hbmlzo/tests/test.prg

#contrib/hbexpat
$hb_collect contrib/hbexpat/Makefile
$hb_collect contrib/hbexpat/amigacon.h
$hb_collect contrib/hbexpat/ascii.h
$hb_collect contrib/hbexpat/asciitab.h
$hb_collect contrib/hbexpat/core.c
$hb_collect contrib/hbexpat/expat.h
$hb_collect contrib/hbexpat/expat_ex.h
$hb_collect contrib/hbexpat/hbexpat.ch
$hb_collect contrib/hbexpat/iasciita.h
$hb_collect contrib/hbexpat/internal.c
$hb_collect contrib/hbexpat/internal.h
$hb_collect contrib/hbexpat/latin1ta.h
$hb_collect contrib/hbexpat/macconfi.h
$hb_collect contrib/hbexpat/nametab.h
$hb_collect contrib/hbexpat/unitable.prg
$hb_collect contrib/hbexpat/utf8tab.h
$hb_collect contrib/hbexpat/watcomconfig.h
$hb_collect contrib/hbexpat/winconfi.h
$hb_collect contrib/hbexpat/xmlparse.c
$hb_collect contrib/hbexpat/xmlrole.c
$hb_collect contrib/hbexpat/xmlrole.h
$hb_collect contrib/hbexpat/xmltok.c
$hb_collect contrib/hbexpat/xmltok.h
$hb_collect contrib/hbexpat/xmltok_i.c
$hb_collect contrib/hbexpat/xmltok_i.h
$hb_collect contrib/hbexpat/xmltok_n.c
$hb_collect contrib/hbexpat/_hbconf.h
$hb_collect contrib/hbexpat/tests
$hb_collect contrib/hbexpat/tests/test.prg
$hb_collect contrib/hbexpat/tests/test.xml

#contrib/hbmagic
$hb_collect contrib/hbmagic/Makefile
$hb_collect contrib/hbmagic/core.c
$hb_collect contrib/hbmagic/hbmagic.ch
$hb_collect contrib/hbmagic/hbmagis.prg
$hb_collect contrib/hbmagic/tests
$hb_collect contrib/hbmagic/tests/hbmagit.prg
#contrib/curl
$hb_collect contrib/hbcurl/*.[ch]
$hb_collect contrib/hbcurl/*.ch
$hb_collect contrib/hbcurl/Makefile

#config/tiff
$hb_collect contrib/tiff/*.[ch]
$hb_collect contrib/tiff/*.c
$hb_collect contrib/tiff/*.h
$hb_collect contrib/tiff/Makefile
#jpeg
$hb_collect contrib/jpeg/*.[ch]
$hb_collect contrib/jpeg/*.c
$hb_collect contrib/jpeg/*.h
$hb_collect contrib/jpeg/Makefile
#png
$hb_collect contrib/png/*.[ch]
$hb_collect contrib/png/*.c
$hb_collect contrib/png/*.h
$hb_collect contrib/png/Makefile
#rddsql
$hb_collect contrib/rddsql/hbrddsql.ch
$hb_collect contrib/rddsql/*.c
$hb_collect contrib/rddsql/hbrddsql.h
$hb_collect contrib/rddsql/Makefile
#hbhpdf
$hb_collect contrib/hbhpdf/*.[ch]
$hb_collect contrib/hbhpdf/*.c
$hb_collect contrib/hbhpdf/*.h
$hb_collect contrib/hbhpdf/Makefile
#sddodbc
$hb_collect contrib/sddodbc/*.[ch]
$hb_collect contrib/sddodbc/*.c
$hb_collect contrib/sddodbc/*.h
$hb_collect contrib/sddodbc/Makefile

}

hb_flst=`cd "$hb_rootdir";hb_collect_all|grep -v "[*?[]"`

$hb_archbin $hb_archopt $hb_filename $hb_flst
