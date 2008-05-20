@echo off
rem
rem $Id: crc32.h,v 1.1 2008/04/14 06:06:22 andijahja Exp $
rem
SET CC_DIR=C:\PELLESC
SET _MYLIB=common.lib dbffpt.lib dbfcdx.lib dbfntx.lib hbsix.lib debug.lib gtwin.lib macro.lib pp.lib rdd.lib rtl.lib vm.lib lang.lib pcrepos.lib ct.lib hbhpdf.lib png.lib zlib.lib
SET HARBOURDIR=..\..\..
SET HARBOURLIB=%HARBOURDIR%\lib\PC
SET HARBOUREXE=%HARBOURDIR%\bin\PC\harbour.exe
SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%PATH%
SET __COMPILERLIBS=crt.lib kernel32.lib winspool.lib user32.lib advapi32.lib ole32.lib uuid.lib oleaut32.lib winmm.lib mpr.lib
IF NOT EXIST harupdf.prg GOTO noexist
IF NOT EXIST %HARBOUREXE% GOTO noharbour
IF EXIST harupdf.exe DEL harupdf.exe
IF EXIST harupdf.c DEL harupdf.c
IF EXIST harupdf.obj DEL harupdf.obj

%HARBOUREXE% harupdf /n /i%HARBOURDIR%\include /gc2 /es2 /w /q
IF NOT EXIST harupdf.c GOTO end
POCC /c /Ze /Zx /Go /Tx86-coff /I%HARBOURDIR%\INCLUDE /I%CC_DIR%\INCLUDE /D"__WIN32__" /D"NULL=0"  harupdf.c
IF NOT EXIST harupdf.obj GOTO end
POLINK /OUT:harupdf.exe /FORCE:MULTIPLE /LIBPATH:%HARBOURDIR%\LIB\PC /LIBPATH:%CC_DIR%\LIB /LIBPATH:%CC_DIR%\LIB\WIN /MACHINE:IX86 /SUBSYSTEM:CONSOLE harupdf.obj %_MYLIB% %__COMPILERLIBS%

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
SET __COMPILERLIBS=
SET _MYLIB=
SET CC_DIR=
