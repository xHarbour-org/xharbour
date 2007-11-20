@echo off
rem ***********************************************************
rem * bldtest.bat
rem *
rem * $Id: bldtest.bat,v 1.9 2006/07/10 14:28:29 fsgiudice Exp $
rem *
rem * Batch file to build test programs in ST or MT environment
rem *
rem * (C) 2003 Francesco Saverio Giudice <info@fsgiudice.com>
rem *
rem ***********************************************************
rem *
rem * This is a generic batch file, if it doesn't fit your own needs
rem * please DON'T MODIFY IT.
rem *
rem * Instead, make a local copy and modify that one, or make a call to
rem * this batch file from your customized one.
rem *
rem ***********************************************************

rem Saving current HB_MT state
set OLDENVMT=%HB_MT%
set OLDENVGT=%HB_GT_LIB%
set OLDENVC=%CFLAGS%
set OLDENVHB=%HARBOURFLAGS%
set OLD_HB_ARCHITECTURE=%HB_ARCHITECTURE%
set OLD_HB_COMPILER=%HB_COMPILER%
set OLD_HB_USER_LIBS=%HB_USER_LIBS%

set HB_INSTALL=..
if %HB_ARCHITECTURE%.==. set HB_ARCHITECTURE=w32
if %HB_COMPILER%.==.     set HB_COMPILER=bcc32
SET HB_BIN_INSTALL=%HB_INSTALL%\bin
set HB_INC_INSTALL=%HB_INSTALL%\include
set HB_LIB_INSTALL=%HB_INSTALL%\lib
set HB_USER_LIBS=

echo.
echo.BldTest.bat - /? or /h to display options
echo.

:ARGUMENTS
rem Check parameters
IF %1.==.     GOTO SHOWHELP
IF %1.==/?.   GOTO SHOWHELP
IF %1.==/H.   GOTO SHOWHELP
IF %1.==/h.   GOTO SHOWHELP
IF %1.==/MT.  GOTO SETMT
IF %1.==/mt.  GOTO SETMT
IF %1.==/cgi. GOTO SETCGI
IF %1.==/CGI. GOTO SETCGI
IF %1.==/WVT. GOTO SETWVT
IF %1.==/wvt. GOTO SETWVT
IF %1.==/WVW. GOTO SETWVW
IF %1.==/wvw. GOTO SETWVW
IF %1.==/gui. GOTO SETGUI
IF %1.==/GUI. GOTO SETGUI
IF %1.==/nul. GOTO SETNUL
IF %1.==/NUL. GOTO SETNUL
IF %1.==/dbg. GOTO SETDBG
IF %1.==/DBG. GOTO SETDBG
IF %1.==/fwh. GOTO SETFWH
IF %1.==/FWH. GOTO SETFWH

IF %BLDDEFAULT%.==N. GOTO CALLBLD
GOTO SETDEFAULT

:SETMT
echo.Setting MultiThread (MT) mode
echo.
SET HB_MT=mt
SHIFT
SET BLDDEFAULT=N
GOTO ARGUMENTS

:SETWVT
echo.Setting Windows Virtual Terminal (WVT) mode
echo.
SET HB_GT_LIB=gtwvt
IF %HB_COMPILER%.==bcc32. SET CFLAGS=-W
SHIFT
SET BLDDEFAULT=N
GOTO ARGUMENTS

:SETWVW
echo.Setting Windows Virtual Windows (WVW) mode
echo.
SET HB_GT_LIB=gtwvw
IF %HB_COMPILER%.==bcc32. SET CFLAGS=-W
SHIFT
SET BLDDEFAULT=N
GOTO ARGUMENTS

:SETGUI
echo.Setting GUI (WVW) mode
echo.
SET HB_GT_LIB=gtgui
IF %HB_COMPILER%.==bcc32. SET CFLAGS=-W
SHIFT
SET BLDDEFAULT=N
GOTO ARGUMENTS

:SETNUL
echo.Setting GTNUL mode
echo.
SET HB_GT_LIB=gtnul
IF %HB_COMPILER%.==bcc32. SET CFLAGS=-W
SHIFT
SET BLDDEFAULT=N
GOTO ARGUMENTS

:SETCGI
echo.Setting CGI Terminal (CGI) mode
echo.
SET HB_GT_LIB=gtcgi
SET CFLAGS=
SHIFT
SET BLDDEFAULT=N
GOTO ARGUMENTS

:SETDBG
echo.Setting DEBUG mode
echo.
set HARBOURFLAGS=-b -es2
SHIFT
GOTO ARGUMENTS

:SETFWH
echo. Setting FWH mode
echo.
set HARBOURFLAGS=-i\fwh\include
IF %HB_COMPILER%.==msvc. SET LFLAGS=-NODEFAULTLIB:libc.lib
IF %HB_COMPILER%.==msvc. set HB_USER_LIBS=\FWH\LIB\FiveHCM.lib \FWH\LIB\FiveHMX.lib
IF %HB_COMPILER%.==bcc32. set HB_USER_LIBS=\FWH\LIB\FiveHC.lib \FWH\LIB\FiveHX.lib
GOTO :SETNUL

:SETDEFAULT
echo.Setting Default Settings (ST/GTWIN) mode
echo.
SET HB_MT=
SET HB_GT_LIB=

:CALLBLD
echo.Running %HB_BIN_INSTALL%\bld.bat %1 %2 %3 %4 %5
echo.
echo.please wait ...
echo.

if exist %1.c   del %1.c
if exist %1.obj del %1.obj
if exist %1.tds del %1.tds

if exist %2.c   del %2.c
if exist %2.obj del %2.obj
if exist %2.tds del %2.tds

if exist %3.c   del %3.c
if exist %3.obj del %3.obj
if exist %3.tds del %3.tds

if exist %1.exe   del %1.exe

call %HB_BIN_INSTALL%\bld.bat %1 %2 %3 %4 %5 > bldtest.log
IF not exist %1.exe GOTO SHOWERROR

GOTO COMPILEOK

:SHOWERROR
echo.
echo.Error on compiling ...
echo.
echo.Running notepad, please close to end this batch file ...
echo.
notepad bldtest.log
echo.
echo.Notepad closed, exiting ...
echo.
GOTO ENDSET

:COMPILEOK
echo.
echo.Compiled successfully
echo.
if exist bldtest.log del bldtest.log
GOTO ENDSET

:SHOWHELP
echo.
echo."bldtest [/MT|/mt|/CGI|/cgi|WVT|/wvt|/GUI/gui/NUL/nul/?|/H|/h] prgname"
echo.
echo. /MT       = Set MT envinronment to build test program in MultiThread mode
echo.             otherwise program will be compiled in SingleThread mode
echo. /CGI      = Uses GTCGI instead of GTWIN to build test program.
echo. /WVT      = Uses GTWVT instead of GTWIN to build test program.
echo. /WVW      = Uses GTWVW instead of GTWIN to build test program.
echo. /GUI      = Uses GTGUI instead of GTWIN to build test program.
echo. /NUL      = Uses GTNUL instead of GTWIN to build test program.
echo. /FWH      = Uses GTNUL and link with FiveWin
echo. /? or /H  = Show this help
echo. prgname   = Name of prg file to compile without extension [.prg]
echo.

:ENDSET
rem Restore Old Settings
set HB_MT=%OLDENVMT%
set HB_GT_LIB=%OLDENVGT%
set CFLAGS=%OLDENVC%
set HARBOURFLAGS=%OLDENVHB%
set HB_ARCHITECTURE=%OLD_HB_ARCHITECTURE%
set HB_COMPILER=%OLD_HB_COMPILER%
set HB_USER_LIBS=%OLD_HB_USER_LIBS%

set OLDENVHB=
set OLDENVGT=
set OLDENVC=
set OLDENVMT=
set BLDDEFAULT=
set OLD_HB_ARCHITECTURE=
set OLD_HB_COMPILER=
set OLD_HB_USER_LIBS=