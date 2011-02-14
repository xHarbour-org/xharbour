@echo off
rem ***********************************************************
rem * bldvc.bat
rem *
rem * $Id$
rem *
rem * Batch file to build test programs in ST or MT environment
rem *
rem * (C) 2005 Laverson Espíndola <laverson.espindola@gmail.com>
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
set OLD_HB_ARCHITECTURE=%HB_ARCHITECTURE%
set OLD_HB_COMPILER=%HB_COMPILER%
set OLD_HB_USER_LIBS=%HB_USER_LIBS%

set HB_INSTALL=..\..\..\..
if %HB_ARCHITECTURE%.==. set HB_ARCHITECTURE=w32
if %HB_COMPILER%.==.     	set HB_COMPILER=msvc
SET HB_BIN_INSTALL=%HB_INSTALL%\bin
set HB_INC_INSTALL=..\..\include;%HB_INSTALL%\include
set HB_LIB_INSTALL=%HB_INSTALL%\lib
set HB_USER_LIBS=..\..\lib\gdlib.lib ..\..\lib\libbgd.lib
set HB_OBJ_VM=%HB_INSTALL%\obj\vc

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
SET CFLAGS=-W
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

:SETDEFAULT
echo.Setting Default Settings (ST/GTWIN) mode
echo.
SET HB_MT=
SET HB_GT_LIB=gtwin

:CALLBLD
echo.Running %HB_BIN_INSTALL%\
echo.
echo Compiling...
if not "%1" == "" GOTO COMPILE
GOTO SHOWHELP

:COMPILE
echo %HB_BIN_INSTALL%\harbour %1.prg -n -q0 -gc -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
%HB_BIN_INSTALL%\harbour %1.prg -n -q0 -gc -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
IF NOT ERRORLEVEL 1 GOTO LINK
GOTO ENDSET

:LINK

if not "%HB_GT_LIB%" == "" set _HB_GT_LIB=%HB_GT_LIB%
if "%HB_MT%"      == "" set LDFLAGS=/NODEFAULTLIB:LIBCMT
if not "%HB_MT%"  == "" set LDFLAGS=/NODEFAULTLIB:LIBC /NODEFAULTLIB:LIBCP

if "%HB_DLL%"     == "" set HB_LIBLIST=%HB_LIB_INSTALL%\debug.lib %HB_LIB_INSTALL%\vm%HB_MT%.lib %HB_LIB_INSTALL%\rtl%HB_MT%.lib %HB_LIB_INSTALL%\pcrepos.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\lang.lib %HB_LIB_INSTALL%\rdd%HB_MT%.lib %HB_LIB_INSTALL%\macro%HB_MT%.lib %HB_LIB_INSTALL%\pp%HB_MT%.lib %HB_LIB_INSTALL%\dbffpt%HB_MT%.lib %HB_LIB_INSTALL%\dbfntx%HB_MT%.lib %HB_LIB_INSTALL%\dbfcdx%HB_MT%.lib %HB_LIB_INSTALL%\hsx.lib %HB_LIB_INSTALL%\hbsix%HB_MT%.lib %HB_LIB_INSTALL%\common.lib %HB_LIB_INSTALL%\ct%HB_MT%.lib %HB_LIB_INSTALL%\tip%HB_MT%.lib %ADS_LIBS% %HB_USER_LIBS%
if not "%HB_DLL%" == "" set HB_LIBLIST=%HB_LIB_INSTALL%\harbour.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib msvcrt.lib %ADS_LIBS% %HB_USER_LIBS%

if exist %HB_LIB_INSTALL%\hbzip.lib set HB_LIBLIST=%HB_LIBLIST% %HB_LIB_INSTALL%\hbzip.lib

if not "%HB_DLL%" == "" if "%HB_MT%" == "" set LDFLAGS=%LDFLAGS% /NODEFAULTLIB:LIBC

set _cons=CONSOLE
set _main=std

if "%HB_GT_LIB%"=="gtwvt" set _cons=WINDOWS
if "%HB_GT_LIB%"=="gtwvt" set _main=win

echo cl -TP -W3 %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% /link %LFLAGS% %HB_OBJ_VM%\main%_main%.obj /subsystem:%_cons% /FORCE:MULTIPLE %LDFLAGS% %HB_LIBLIST% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib kernel32.lib gdi32.lib comctl32.lib comdlg32.lib> msvc.log
     cl -TP -W3 %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% /link %LFLAGS% %HB_OBJ_VM%\main%_main%.obj /subsystem:%_cons% /FORCE:MULTIPLE %LDFLAGS% %HB_LIBLIST% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib kernel32.lib gdi32.lib comctl32.lib comdlg32.lib>>msvc.log

cl -TP -W3 %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% /link %LFLAGS% %HB_OBJ_VM%\main%_main%.obj /subsystem:%_cons% /FORCE:MULTIPLE %LDFLAGS% %HB_LIBLIST% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib kernel32.lib gdi32.lib comctl32.lib comdlg32.lib> msvc.log
cl -TP -W3 %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% /link %LFLAGS% %HB_OBJ_VM%\main%_main%.obj /subsystem:%_cons% /FORCE:MULTIPLE %LDFLAGS% %HB_LIBLIST% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib kernel32.lib gdi32.lib comctl32.lib comdlg32.lib>>msvc.log

set _cons=
set _main=
@type msvc.log
@echo Ignore LNK4033 warning
set LDFLAGS=

GOTO ENDSET

IF ERRORLEVEL 1 GOTO SHOWERROR

if exist %1.c   del %1.c
if exist %1.obj del %1.obj
if exist %1.tds del %1.tds

if exist %2.c   del %2.c
if exist %2.obj del %2.obj
if exist %2.tds del %2.tds

if exist %3.c   del %3.c
if exist %3.obj del %3.obj
if exist %3.tds del %3.tds

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
echo."bldvc [/MT|/mt|/CGI|/cgi|WVT|/wvt|/?|/H|/h] prgname"
echo.
echo. /MT       = Set MT envinronment to build test program in MultiThread mode
echo.             otherwise program will be compiled in SingleThread mode
echo. /CGI      = Uses GTCGI instead of GTWIN to build test program.
echo. /WVT      = Uses GTWVT instead of GTWIN to build test program.
echo. /? or /H  = Show this help
echo. prgname   = Name of prg file to compile without extension [.prg]
echo.

:ENDSET
rem Restore Old Settings
set HB_MT=%OLDENVMT%
set HB_GT_LIB=%OLDENVGT%
set CFLAGS=%OLDENVC%
set HB_ARCHITECTURE=%OLD_HB_ARCHITECTURE%
set HB_COMPILER=%OLD_HB_COMPILER%
set HB_USER_LIBS=%OLD_HB_USER_LIBS%


set OLDENVGT=
set OLDENVC=
set OLDENVMT=
set BLDDEFAULT=
set OLD_HB_ARCHITECTURE=
set OLD_HB_COMPILER=
set OLD_HB_USER_LIBS=
set HB_OBJ_VM=
