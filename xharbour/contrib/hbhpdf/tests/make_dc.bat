@echo off
rem
rem $Id: crc32.h,v 1.1 2008/04/14 06:06:22 andijahja Exp $
rem
SET CC_DIR=C:\DM
SET HARBOURDIR=..\..\..
SET _MYLIB=%HARBOURDIR%\lib\dc\common.lib %HARBOURDIR%\lib\dc\dbffpt.lib %HARBOURDIR%\lib\dc\dbfcdx.lib %HARBOURDIR%\lib\dc\dbfntx.lib %HARBOURDIR%\lib\dc\hbsix.lib %HARBOURDIR%\lib\dc\debug.lib %HARBOURDIR%\lib\dc\gtwin.lib %HARBOURDIR%\lib\dc\macro.lib %HARBOURDIR%\lib\dc\pp.lib %HARBOURDIR%\lib\dc\rdd.lib %HARBOURDIR%\lib\dc\rtl.lib %HARBOURDIR%\lib\dc\vm.lib %HARBOURDIR%\lib\dc\lang.lib %HARBOURDIR%\lib\dc\pcrepos.lib %HARBOURDIR%\lib\dc\ct.lib %HARBOURDIR%\lib\dc\zlib.lib %HARBOURDIR%\lib\dc\hbhpdf.lib %HARBOURDIR%\lib\dc\png.lib %CC_DIR%\LIB\snn.lib %CC_DIR%\LIB\kernel32.lib %CC_DIR%\LIB\winspool.lib %CC_DIR%\LIB\user32.lib %CC_DIR%\LIB\advapi32.lib %CC_DIR%\LIB\ole32.lib %CC_DIR%\LIB\uuid.lib %CC_DIR%\LIB\mpr.lib %CC_DIR%\LIB\winmm.lib %CC_DIR%\LIB\oleaut32.lib
SET HARBOURLIB=%HARBOURDIR%\lib\DC
SET HARBOUREXE=%HARBOURDIR%\bin\DC\harbour.exe
SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%PATH%

if not exist harupdf.prg goto noexist
if not exist %HARBOUREXE% goto noharbour
if exist harupdf.exe del harupdf.exe
if exist harupdf.c del harupdf.c

%HARBOUREXE% harupdf /q0 /n /i%HARBOURDIR%\include /gc2 /es2 /w
if not exist harupdf.c goto end
%CC_DIR%\BIN\SC harupdf.c -o -Jm -cpp -mn -I%HARBOURDIR%\INCLUDE -I%CC_DIR%\INCLUDE -c -6 -WA -D__WIN32__
if not exist harupdf.obj goto end
%CC_DIR%\BIN\LINK %HARBOURDIR%\obj\dc\mainstd.obj harupdf.obj,harupdf.exe,,%_MYLIB%

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
set CC_DIR=
