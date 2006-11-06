@echo off
rem
rem $Id: make_gc.bat,v 1.6 2005/03/31 19:51:26 andijahja Exp $
rem
rem Batch File For Building xHarbour with MinGW32 under Windows
rem
rem What we have to do is to set the following 3 envars, ONE, TWO and THREE

rem ENVAR ONE
rem Our MinGW32 Root Directory
@set MINGWDIR=F:/MinGW

rem ENVAR TWO
rem Our BISON BIN Directory
@set BISONDIR=F:/Bison/bin

rem ENVAR THREE
rem SET xHarbour Working Root Directory Here
@set HARBOUR_DIR=C:/xharbour

rem Set up our environment for output files here
rem Let them be like that

rem Set up our BIN paths
@set _PATH=%PATH%
@set PATH=%MINGWDIR%\bin;%BISONDIR%

@set BIN_DIR=bin\gcc
@set LIB_DIR=lib\gcc
@set OBJ_DIR=obj\gcc

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:BUILD

if not exist %LIB_DIR%                 md %LIB_DIR%
if not exist %BIN_DIR%                 md %BIN_DIR%
if not exist %OBJ_DIR%                 md %OBJ_DIR%
if not exist %OBJ_DIR%\bin             md %OBJ_DIR%\bin
if not exist %OBJ_DIR%\libmisc         md %OBJ_DIR%\libmisc
if not exist %OBJ_DIR%\rddads          md %OBJ_DIR%\rddads
if not exist %OBJ_DIR%\unicode         md %OBJ_DIR%\unicode
if not exist %OBJ_DIR%\nanfor          md %OBJ_DIR%\nanfor
if not exist %OBJ_DIR%\design          md %OBJ_DIR%\design
if not exist %OBJ_DIR%\html            md %OBJ_DIR%\html
if not exist %OBJ_DIR%\ct              md %OBJ_DIR%\ct
if not exist %OBJ_DIR%\mt              md %OBJ_DIR%\mt
if not exist %OBJ_DIR%\mt\ct           md %OBJ_DIR%\mt\ct
if not exist %OBJ_DIR%\opt             md %OBJ_DIR%\opt
if not exist %OBJ_DIR%\opt\console     md %OBJ_DIR%\opt\console
if not exist %OBJ_DIR%\opt\gui         md %OBJ_DIR%\opt\gui
if not exist %OBJ_DIR%\mt\opt\gui      md %OBJ_DIR%\mt\opt\gui
if not exist %OBJ_DIR%\mt\opt\console  md %OBJ_DIR%\mt\opt\console
if not exist %OBJ_DIR%\mt\opt\gui      md %OBJ_DIR%\mt\opt\gui
if not exist %OBJ_DIR%\mt\rddads       md %OBJ_DIR%\mt\rddads
if not exist %OBJ_DIR%\mt\unicode      md %OBJ_DIR%\mt\unicode
if not exist %OBJ_DIR%\mt\nanfor       md %OBJ_DIR%\mt\nanfor
if not exist %OBJ_DIR%\mt\libmisc      md %OBJ_DIR%\mt\libmisc

   @set THREAD_MODE=
   mingw32-make.exe -f makefile.gc
   if errorlevel 1 goto BUILD_ERR
   @set THREAD_MODE=mt
   mingw32-make.exe -f makefile.gc
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   goto EXIT

:BUILD_ERR

   goto EXIT

:CLEAN

if exist %BIN_DIR%\xharbour.exe       del %BIN_DIR%\xharbour.exe
if exist %BIN_DIR%\xharbour.dll       del %BIN_DIR%\xharbour.dll
if exist %BIN_DIR%\harbour.exe        del %BIN_DIR%\harbour.exe
if exist %BIN_DIR%\hbdoc.exe          del %BIN_DIR%\hbdoc.exe
if exist %BIN_DIR%\hbmake.exe         del %BIN_DIR%\hbmake.exe
if exist %BIN_DIR%\hbpp.exe           del %BIN_DIR%\hbpp.exe
if exist %BIN_DIR%\hbrun.exe          del %BIN_DIR%\hbrun.exe
if exist %BIN_DIR%\hbrunmt.exe        del %BIN_DIR%\hbrunmt.exe
if exist %BIN_DIR%\hbtest.exe         del %BIN_DIR%\hbtest.exe
if exist %BIN_DIR%\xbscript.exe       del %BIN_DIR%\xbscript.exe

if exist %LIB_DIR%\libcodepage.a      del %LIB_DIR%\libcodepage.a
if exist %LIB_DIR%\libxharbour.a      del %LIB_DIR%\libxharbour.a
if exist %LIB_DIR%\libxharbour.def    del %LIB_DIR%\libxharbour.def
if exist %LIB_DIR%\libcommon.a        del %LIB_DIR%\libcommon.a
if exist %LIB_DIR%\libct.a            del %LIB_DIR%\libct.a
if exist %LIB_DIR%\libctmt.a          del %LIB_DIR%\libctmt.a
if exist %LIB_DIR%\libdbfcdx.a        del %LIB_DIR%\libdbfcdx.a
if exist %LIB_DIR%\libdbfcdxmt.a      del %LIB_DIR%\libdbfcdxmt.a
if exist %LIB_DIR%\libbmdbfcdx.a      del %LIB_DIR%\libbmdbfcdx.a
if exist %LIB_DIR%\libbmdbfcdxmt.a    del %LIB_DIR%\libbmdbfcdxmt.a
if exist %LIB_DIR%\libdbfdbt.a        del %LIB_DIR%\libdbfdbt.a
if exist %LIB_DIR%\libdbfdbtmt.a      del %LIB_DIR%\libdbfdbtmt.a
if exist %LIB_DIR%\libdbffpt.a        del %LIB_DIR%\libdbffpt.a
if exist %LIB_DIR%\libdbffptmt.a      del %LIB_DIR%\libdbffptmt.a
if exist %LIB_DIR%\libdbfntx.a        del %LIB_DIR%\libdbfntx.a
if exist %LIB_DIR%\libdbfntxmt.a      del %LIB_DIR%\libdbfntxmt.a
if exist %LIB_DIR%\libdebug.a         del %LIB_DIR%\libdebug.a
if exist %LIB_DIR%\libdesign.a        del %LIB_DIR%\libdesign.a
if exist %LIB_DIR%\libgtnul.a         del %LIB_DIR%\libgtnul.a
if exist %LIB_DIR%\libgtstd.a         del %LIB_DIR%\libgtstd.a
if exist %LIB_DIR%\libgtwin.a         del %LIB_DIR%\libgtwin.a
if exist %LIB_DIR%\libgtwvt.a         del %LIB_DIR%\libgtwvt.a
if exist %LIB_DIR%\libgtwvtmt.a       del %LIB_DIR%\libgtwvtmt.a
if exist %LIB_DIR%\libgtwvw.a         del %LIB_DIR%\libgtwvw.a
if exist %LIB_DIR%\libgtwvwmt.a       del %LIB_DIR%\libgtwvwmt.a
if exist %LIB_DIR%\libhtml.a          del %LIB_DIR%\libhtml.a
if exist %LIB_DIR%\liblang.a          del %LIB_DIR%\liblang.a
if exist %LIB_DIR%\libmacro.a         del %LIB_DIR%\libmacro.a
if exist %LIB_DIR%\libmacromt.a       del %LIB_DIR%\libmacromt.a
if exist %LIB_DIR%\libmisc.a          del %LIB_DIR%\libmisc.a
if exist %LIB_DIR%\libmiscmt.a        del %LIB_DIR%\libmiscmt.a
if exist %LIB_DIR%\libnf.a            del %LIB_DIR%\libnf.a
if exist %LIB_DIR%\libnfmt.a          del %LIB_DIR%\libnfmt.a
if exist %LIB_DIR%\libnulsys.a        del %LIB_DIR%\libnulsys.a
if exist %LIB_DIR%\liboptcon.a        del %LIB_DIR%\liboptcon.a
if exist %LIB_DIR%\liboptconmt.a      del %LIB_DIR%\liboptconmt.a
if exist %LIB_DIR%\liboptgui.a        del %LIB_DIR%\liboptgui.a
if exist %LIB_DIR%\liboptguimt.a      del %LIB_DIR%\liboptguimt.a
if exist %LIB_DIR%\libpp.a            del %LIB_DIR%\libpp.a
if exist %LIB_DIR%\libppmt.a          del %LIB_DIR%\libppmt.a
if exist %LIB_DIR%\librdd.a           del %LIB_DIR%\librdd.a
if exist %LIB_DIR%\librddads.a        del %LIB_DIR%\librddads.a
if exist %LIB_DIR%\librddadsmt.a      del %LIB_DIR%\librddadsmt.a
if exist %LIB_DIR%\librddmt.a         del %LIB_DIR%\librddmt.a
if exist %LIB_DIR%\librtl.a           del %LIB_DIR%\librtl.a
if exist %LIB_DIR%\librtlmt.a         del %LIB_DIR%\librtlmt.a
if exist %LIB_DIR%\libhbodbc.a        del %LIB_DIR%\libhbodbc.a
if exist %LIB_DIR%\libhbodbcmt.a      del %LIB_DIR%\libhbodbcmt.a
if exist %LIB_DIR%\libsamples.a       del %LIB_DIR%\libsamples.a
if exist %LIB_DIR%\libsamplesmt.a     del %LIB_DIR%\libsamplesmt.a
if exist %LIB_DIR%\libtip.a           del %LIB_DIR%\libtip.a
if exist %LIB_DIR%\libtipmt.a         del %LIB_DIR%\libtipmt.a
if exist %LIB_DIR%\libunicode.a       del %LIB_DIR%\libunicode.a
if exist %LIB_DIR%\libunicodemt.a     del %LIB_DIR%\libunicodemt.a
if exist %LIB_DIR%\libvm.a            del %LIB_DIR%\libvm.a
if exist %LIB_DIR%\libvmmt.a          del %LIB_DIR%\libvmmt.a

if exist %OBJ_DIR%\libmisc\*.c        del %OBJ_DIR%\libmisc\*.c
if exist %OBJ_DIR%\libmisc\*.o        del %OBJ_DIR%\libmisc\*.o

if exist %OBJ_DIR%\rddads\*.c         del %OBJ_DIR%\rddads\*.c
if exist %OBJ_DIR%\rddads\*.o         del %OBJ_DIR%\rddads\*.o

if exist %OBJ_DIR%\unicode\*.c        del %OBJ_DIR%\unicode\*.c
if exist %OBJ_DIR%\unicode\*.o        del %OBJ_DIR%\unicode\*.o

if exist %OBJ_DIR%\nanfor\*.c         del %OBJ_DIR%\nanfor\*.c
if exist %OBJ_DIR%\nanfor\*.o         del %OBJ_DIR%\nanfor\*.o

if exist %OBJ_DIR%\design\*.c         del %OBJ_DIR%\design\*.c
if exist %OBJ_DIR%\design\*.o         del %OBJ_DIR%\design\*.o

if exist %OBJ_DIR%\html\*.c           del %OBJ_DIR%\html\*.c
if exist %OBJ_DIR%\html\*.o           del %OBJ_DIR%\html\*.o

if exist %OBJ_DIR%\*.c                del %OBJ_DIR%\*.c
if exist %OBJ_DIR%\*.o                del %OBJ_DIR%\*.o
if exist %OBJ_DIR%\*.h                del %OBJ_DIR%\*.h
if exist %OBJ_DIR%\*.output           del %OBJ_DIR%\*.output

if exist %OBJ_DIR%\ct\*.c             del %OBJ_DIR%\ct\*.c
if exist %OBJ_DIR%\ct\*.o             del %OBJ_DIR%\ct\*.o

if exist %OBJ_DIR%\mt\*.c             del %OBJ_DIR%\mt\*.c
if exist %OBJ_DIR%\mt\*.o             del %OBJ_DIR%\mt\*.o
if exist %OBJ_DIR%\mt\*.h             del %OBJ_DIR%\mt\*.h
if exist %OBJ_DIR%\mt\*.output        del %OBJ_DIR%\mt\*.output

if exist %OBJ_DIR%\bin\*.c            del %OBJ_DIR%\bin\*.c
if exist %OBJ_DIR%\bin\*.o            del %OBJ_DIR%\bin\*.o
if exist %OBJ_DIR%\bin\*.h            del %OBJ_DIR%\bin\*.h
if exist %OBJ_DIR%\bin\*.output       del %OBJ_DIR%\bin\*.output

if exist %OBJ_DIR%\mt\ct\*.c          del %OBJ_DIR%\mt\ct\*.c
if exist %OBJ_DIR%\mt\ct\*.o          del %OBJ_DIR%\mt\ct\*.o

if exist %OBJ_DIR%\opt\*.c            del %OBJ_DIR%\opt\*.c
if exist %OBJ_DIR%\opt\*.o            del %OBJ_DIR%\opt\*.o

if exist %OBJ_DIR%\opt\console\*.c    del %OBJ_DIR%\opt\console\*.c
if exist %OBJ_DIR%\opt\console\*.o    del %OBJ_DIR%\opt\console\*.o

if exist %OBJ_DIR%\opt\gui\*.c        del %OBJ_DIR%\opt\gui\*.c
if exist %OBJ_DIR%\opt\gui\*.o        del %OBJ_DIR%\opt\gui\*.o

if exist %OBJ_DIR%\mt\opt\gui\*.c     del %OBJ_DIR%\mt\opt\gui\*.c
if exist %OBJ_DIR%\mt\opt\gui\*.o     del %OBJ_DIR%\mt\opt\gui\*.o

if exist %OBJ_DIR%\mt\opt\console\*.c del %OBJ_DIR%\mt\opt\console\*.c
if exist %OBJ_DIR%\mt\opt\console\*.o del %OBJ_DIR%\mt\opt\console\*.o

if exist %OBJ_DIR%\mt\opt\gui\*.c     del %OBJ_DIR%\mt\opt\gui\*.c
if exist %OBJ_DIR%\mt\opt\gui\*.o     del %OBJ_DIR%\mt\opt\gui\*.o

if exist %OBJ_DIR%\mt\rddads\*.c      del %OBJ_DIR%\mt\rddads\*.c
if exist %OBJ_DIR%\mt\rddads\*.o      del %OBJ_DIR%\mt\rddads\*.o

if exist %OBJ_DIR%\mt\unicode\*.c     del %OBJ_DIR%\mt\unicode\*.c
if exist %OBJ_DIR%\mt\unicode\*.o     del %OBJ_DIR%\mt\unicode\*.o

if exist %OBJ_DIR%\mt\nanfor\*.c      del %OBJ_DIR%\mt\nanfor\*.c
if exist %OBJ_DIR%\mt\nanfor\*.o      del %OBJ_DIR%\mt\nanfor\*.o

if exist %OBJ_DIR%\mt\libmisc\*.c     del %OBJ_DIR%\mt\libmisc\*.c
if exist %OBJ_DIR%\mt\libmisc\*.o     del %OBJ_DIR%\mt\libmisc\*.o

   goto EXIT

:EXIT
rem Clean up and restore environment
@set PATH=%_PATH%
@set _PATH=
@set BIN_DIR=
@set OBJ_DIR=
@set LIB_DIR=
@set MINGWDIR=
@set BISONDIR=
@set HARBOUR_DIR=
@set THREAD_MODE=
