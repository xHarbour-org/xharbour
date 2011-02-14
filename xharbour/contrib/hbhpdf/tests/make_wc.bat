@echo off
rem
rem $Id$
rem
SET _MYLIB=common.lib dbffpt.lib dbfcdx.lib dbfntx.lib hbsix.lib debug.lib gtwin.lib macro.lib pp.lib rdd.lib rtl.lib vm.lib lang.lib pcrepos.lib ct.lib hbhpdf.lib png.lib zlib.lib
SET CC_DIR=C:\WATCOM
SET HARBOURDIR=..\..\..
SET HARBOURLIB=%HARBOURDIR%\lib\w32
SET HARBOUREXE=%HARBOURDIR%\bin\w32\harbour.exe
SET _LIB=%LIB%
SET _PATH=%PATH%
SET PATH=%CC_DIR%\BINNT;%CC_DIR%\BINW;C:\BISON\BIN;%_PATH%
SET WATCOM=%CC_DIR%
SET EDPATH=%CC_DIR%\EDDAT
SET LIB=%CC_DIR%\lib386;%CC_DIR%\lib386\nt;%HARBOURDIR%\LIB\w32;%_LIB%
SET __COMPILERLIBS=user32.lib winspool.lib ole32.lib oleaut32.lib advapi32.lib winmm.lib mpr.lib
IF NOT EXIST harupdf.prg GOTO noexist
IF NOT EXIST %HARBOUREXE% GOTO noharbour
IF EXIST harupdf.exe DEL harupdf.exe
IF EXIST harupdf.c DEL harupdf.c
IF EXIST harupdf.obj DEL harupdf.obj

%HARBOUREXE% harupdf /n /i%HARBOURDIR%\include /gc2 /es2 /w /q /i"c:\program files" /d"__FOO__"
IF NOT EXIST harupdf.c GOTO end
WPP386 -i=%HARBOURDIR%\include -i=%CC_DIR%\H -i=%CC_DIR%\H\NT -j -w3 -d0 -6s -fp6 -s -onaehtzr -oi+ -ei -zp8 -zq -zt0 -mf -bt=NT -D__WIN32__ -D__WINDOWS__ -DWIN32 harupdf.c
IF NOT EXIST harupdf.obj GOTO end
LINK386 /OUT:harupdf.exe /SUBSYSTEM:CONSOLE harupdf.obj %_MYLIB% %__COMPILERLIBS%

IF EXIST harupdf.exe dir harupdf.exe
IF EXIST harupdf.exe ECHO.
IF EXIST harupdf.exe ECHO harupdf.exe succesfully built
IF EXIST harupdf.exe ECHO.
GOTO end

:noexist
ECHO.
ECHO Cannot find harupdf.prg file
ECHO.
GOTO end

:noharbour
ECHO.
ECHO Cannot find Harbour.exe
ECHO.
GOTO end

:end
SET PATH=%_PATH%
SET _PATH=
SET HARBOUREXE=
SET HARBOURLIB=
SET HARBOURDIR=
SET LIB=%_LIB%
SET _LIB=
SET __COMPILERLIBS=
SET _MYLIB=
SET CC_DIR=
