@echo off
rem
rem $Id: pack_src.bat,v 1.11 2005/09/15 12:55:34 druzus Exp $
rem

rem This batch requires "Free ZIP" and/or "TAR" utilities for compression.

set hb_ver=0.99.51

if '%TZ%' == '' set TZ=PST8PDT

if not "%hb_architecture%" == "linux"   set hb_archbin=zip
if not "%hb_architecture%" == "linux"   set hb_archopt=-D -X
if not "%hb_architecture%" == "linux"   set hb_archoptr=-D -X -r
if not "%hb_architecture%" == "linux"   set hb_ext=.zip
if     "%hb_architecture%" == "linux"   set hb_archbin=tar
if     "%hb_architecture%" == "linux"   set hb_archopt=-czf --no-recursion
if     "%hb_architecture%" == "linux"   set hb_archoptr=-czf
if     "%hb_architecture%" == "linux"   set hb_ext=.tar.gz

set hb_filename=xharbour-%hb_ver%.src%hb_ext%

if exist %hb_filename% del %hb_filename%

rem README.TXT

rem ROOT
%hb_archbin% %hb_archopt%  %hb_filename% *.bat
%hb_archbin% %hb_archopt%  %hb_filename% ?akefile.*
%hb_archbin% %hb_archopt%  %hb_filename% make_gnu.*

rem BIN
%hb_archbin% %hb_archopt%  %hb_filename% bin/*.bat

rem CONFIG
%hb_archbin% %hb_archopt%  %hb_filename% config/*.cf
%hb_archbin% %hb_archopt%  %hb_filename% config/dos/*.cf
%hb_archbin% %hb_archopt%  %hb_filename% config/linux/*.cf
%hb_archbin% %hb_archopt%  %hb_filename% config/bsd/*.cf
%hb_archbin% %hb_archopt%  %hb_filename% config/darwin/*.cf
%hb_archbin% %hb_archopt%  %hb_filename% config/sunos/*.cf
%hb_archbin% %hb_archopt%  %hb_filename% config/os2/*.cf
%hb_archbin% %hb_archopt%  %hb_filename% config/w32/*.cf

rem DOC
%hb_archbin% %hb_archoptr% %hb_filename% doc/*.txt
%hb_archbin% %hb_archoptr% %hb_filename% doc/en/*.txt
%hb_archbin% %hb_archoptr% %hb_filename% doc/es/*.txt

rem INCLUDE
%hb_archbin% %hb_archopt%  %hb_filename% include/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% include/*.c
%hb_archbin% %hb_archopt%  %hb_filename% include/*.api
%hb_archbin% %hb_archopt%  %hb_filename% include/*.h
%hb_archbin% %hb_archopt%  %hb_filename% include/*.ch

rem SOURCE\COMMON
%hb_archbin% %hb_archopt%  %hb_filename% source/common/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/common/*.c

rem SOURCE
%hb_archbin% %hb_archopt%  %hb_filename% source/Makefile

rem SOURCE\COMPILER
%hb_archbin% %hb_archopt%  %hb_filename% source/compiler/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/compiler/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/compiler/*.slx
%hb_archbin% %hb_archopt%  %hb_filename% source/compiler/*.sly

rem SOURCE\DEBUG
%hb_archbin% %hb_archopt%  %hb_filename% source/debug/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/debug/*.prg

rem SOURCE\LANG
%hb_archbin% %hb_archopt%  %hb_filename% source/lang/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/lang/*.c

rem SOURCE\CODEPAGE
%hb_archbin% %hb_archopt%  %hb_filename% source/codepage/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/codepage/*.c

rem SOURCE\MACRO
%hb_archbin% %hb_archopt%  %hb_filename% source/macro/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/macro/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/macro/*.slx
%hb_archbin% %hb_archopt%  %hb_filename% source/macro/*.y

rem SOURCE\PP
%hb_archbin% %hb_archopt%  %hb_filename% source/pp/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/pp/*.c

rem SOURCE\RDD
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/*.prg

rem SOURCE\RDD\DBFDBT
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfdbt/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfdbt/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfdbt/*.prg

rem SOURCE\RDD\DBFFPT
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbffpt/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbffpt/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbffpt/*.prg

rem SOURCE\RDD\DBFCDX
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfcdx/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfcdx/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfcdx/*.h
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfcdx/*.prg

rem SOURCE\RDD\DBFNTX
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfntx/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfntx/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/dbfntx/*.prg

rem SOURCE\RDD\HSX
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/hsx/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/hsx/*.c

rem SOURCE\RDD\HBSIX
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/hbsix/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/hbsix/*.c

rem SOURCE\RDD\NULSYS
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/nulsys/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rdd/nulsys/*.prg

rem SOURCE\RTL
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/*.prg

rem SOURCE\RTL\PCRE
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/pcre/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/pcre/ChangeLog
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/pcre/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/pcre/*.h

rem SOURCE\RTL\GT_TPL
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gt_tpl/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gt_tpl/*.c

rem SOURCE\RTL\GTNUL
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtnul/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtnul/*.c

rem SOURCE\RTL\GTCGI
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtcgi/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtcgi/*.c

rem SOURCE\RTL\GTCRS
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtcrs/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtcrs/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtcrs/*.h

rem SOURCE\RTL\GTDOS
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtdos/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtdos/*.c

rem SOURCE\RTL\GTOS2
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtos2/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtos2/*.c

rem SOURCE\RTL\GTPCA
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtpca/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtpca/*.c

rem SOURCE\RTL\GTSLN
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtsln/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtsln/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtcrs/*.h

rem SOURCE\RTL\GTSTD
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtstd/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtstd/*.c

rem SOURCE\RTL\GTWIN
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtwin/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtwin/*.c

rem SOURCE\RTL\GTWVT
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtwvt/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtwvt/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtwvt/*.h

rem SOURCE\RTL\GTXWC
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtxwc/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtxwc/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtxwc/*.h

rem SOURCE\RTL\GTALLEG
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtalleg/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtalleg/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtalleg/*.h
%hb_archbin% %hb_archopt%  %hb_filename% source/rtl/gtalleg/*.sfc

rem SOURCE\VM
%hb_archbin% %hb_archopt%  %hb_filename% source/vm/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/vm/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/vm/*.prg

rem SOURCE\odbc
%hb_archbin% %hb_archopt%  %hb_filename% source/odbc/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/odbc/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/odbc/*.ch
%hb_archbin% %hb_archopt%  %hb_filename% source/odbc/*.prg

rem SOURCE\tip
%hb_archbin% %hb_archopt%  %hb_filename% source/tip/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/tip/*.[ch]
%hb_archbin% %hb_archopt%  %hb_filename% source/tip/*.prg

rem SOURCE\ct
%hb_archbin% %hb_archopt%  %hb_filename% source/ct/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/ct/*.[ch]
%hb_archbin% %hb_archopt%  %hb_filename% source/ct/*.prg

rem SOURCE\cgi
%hb_archbin% %hb_archopt%  %hb_filename% source/cgi/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/cgi/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/cgi/*.ch
%hb_archbin% %hb_archopt%  %hb_filename% source/cgi/*.prg

rem SOURCE\misc
%hb_archbin% %hb_archopt%  %hb_filename% source/misc/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% source/misc/*.c
%hb_archbin% %hb_archopt%  %hb_filename% source/misc/*.ch
%hb_archbin% %hb_archopt%  %hb_filename% source/misc/*.prg

rem TESTS
REM %hb_archbin% %hb_archopt%  %hb_filename% tests/*.bat
REM %hb_archbin% %hb_archopt%  %hb_filename% tests/*.ch
REM %hb_archbin% %hb_archopt%  %hb_filename% tests/*.dbf
REM %hb_archbin% %hb_archopt%  %hb_filename% tests/*.fpt
REM %hb_archbin% %hb_archopt%  %hb_filename% tests/*.prg

rem UTILS
%hb_archbin% %hb_archopt%  %hb_filename% utils/Makefile

rem UTILS\HBDOC
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbdoc/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbdoc/*.ch
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbdoc/*.prg

rem UTILS\HBEXTERN
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbextern/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbextern/*.bat
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbextern/*.prg

rem UTILS\HBMAKE
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbmake/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbmake/*.ch
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbmake/*.prg
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbmake/*.c

rem UTILS\XBSCRIPT
%hb_archbin% %hb_archopt%  %hb_filename% utils/xbscript/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% utils/xbscript/*.ch
%hb_archbin% %hb_archopt%  %hb_filename% utils/xbscript/*.prg
%hb_archbin% %hb_archopt%  %hb_filename% utils/xbscript/*.src
%hb_archbin% %hb_archopt%  %hb_filename% utils/xbscript/*.txt

rem UTILS\XPROMPT
%hb_archbin% %hb_archopt%  %hb_filename% utils/xprompt/*.txt

rem UTILS\HBPP
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbpp/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbpp/*.c

rem UTILS\HBRUN
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbrun/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbrun/*.prg

rem UTILS\HBTEST
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbtest/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbtest/*.ch
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbtest/*.cmd
%hb_archbin% %hb_archopt%  %hb_filename% utils/hbtest/*.prg

rem CONTRIB\RDD_ADS
%hb_archbin% %hb_archopt%  %hb_filename% contrib/rdd_ads/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% contrib/rdd_ads/*.c
%hb_archbin% %hb_archopt%  %hb_filename% contrib/rdd_ads/*.prg
%hb_archbin% %hb_archopt%  %hb_filename% contrib/rdd_ads/*.h
%hb_archbin% %hb_archopt%  %hb_filename% contrib/rdd_ads/*.ch

rem CONTRIB\MYSQL
%hb_archbin% %hb_archopt%  %hb_filename% contrib/mysql/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% contrib/mysql/*.c
%hb_archbin% %hb_archopt%  %hb_filename% contrib/mysql/*.prg
%hb_archbin% %hb_archopt%  %hb_filename% contrib/mysql/*.h
%hb_archbin% %hb_archopt%  %hb_filename% contrib/mysql/*.ch

rem CONTRIB\PGSQL
%hb_archbin% %hb_archopt%  %hb_filename% contrib/pgsql/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% contrib/pgsql/*.c
%hb_archbin% %hb_archopt%  %hb_filename% contrib/pgsql/*.prg
%hb_archbin% %hb_archopt%  %hb_filename% contrib/pgsql/*.h
%hb_archbin% %hb_archopt%  %hb_filename% contrib/pgsql/*.ch

rem CONTRIB\LIBNF
%hb_archbin% %hb_archopt%  %hb_filename% contrib/libnf/Makefile
%hb_archbin% %hb_archopt%  %hb_filename% contrib/libnf/*.[ch]
%hb_archbin% %hb_archopt%  %hb_filename% contrib/libnf/*.prg
%hb_archbin% %hb_archopt%  %hb_filename% contrib/libnf/*.ch
%hb_archbin% %hb_archopt%  %hb_filename% contrib/libnf/include/*.h
%hb_archbin% %hb_archopt%  %hb_filename% contrib/libnf/include/*.ch
