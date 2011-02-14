@echo off
rem
rem $Id$
rem
SET CC_DIR=C:\BORLAND\BCC58
SET _MYLIB=common.lib dbffpt.lib dbfcdx.lib dbfntx.lib hbsix.lib debug.lib gtwin.lib macro.lib pp.lib rdd.lib rtl.lib vm.lib lang.lib pcrepos.lib ct.lib zlib.lib png.lib hbhpdf.lib cw32.lib import32.lib
SET HARBOURDIR=..\..\..
SET HARBOURLIB=%HARBOURDIR%\lib\b32
SET HARBOUREXE=%HARBOURDIR%\bin\b32\harbour.exe
SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%PATH%

if not exist harupdf.prg goto noexist
if not exist %HARBOUREXE% goto noharbour
if exist harupdf.exe del harupdf.exe
if exist harupdf.c del harupdf.c

%HARBOUREXE% harupdf /n /i%HARBOURDIR%\include /gc2 /es2 /w /p
if not exist harupdf.c goto end
BCC32 -D__WIN32__ -DWIN32 -I%HARBOURDIR%\include -5 -6 -a8 -d -OS -O2 -c -oharupdf.obj harupdf.c
if not exist harupdf.obj goto end
ILINK32 -ap -Tpe -x -Gn -L%HARBOURLIB% -L%%CC_DIR%%\LIB harupdf.obj,harupdf.exe,,%_MYLIB% c0x32.obj

if exist harupdf.exe dir harupdf.exe
if exist harupdf.exe echo.
if exist harupdf.exe echo harupdf.exe succesfully built
if exist harupdf.exe echo.
goto end

:noexist
echo.
echo Cannot find harupdf.prg file
echo.
goto end

:noharbour
echo.
echo Cannot find Harbour.exe
echo.
goto end

:end
set PATH=%_PATH%
set _PATH=
set HARBOUREXE=
set HARBOURLIB=
set HARBOURDIR=
set _MYLIB=
SET CC_DIR=
