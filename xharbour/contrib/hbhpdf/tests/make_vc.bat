@echo off
rem
rem $Id$
rem
SET _MYLIB=png.lib hbhpdf.lib zlib.lib common.lib dbffpt.lib dbfcdx.lib dbfntx.lib hbsix.lib debug.lib gtwin.lib macro.lib pp.lib rdd.lib rtl.lib vm.lib lang.lib pcrepos.lib ct.lib
SET HARBOURDIR=..\..\..
SET CC_DIR=C:\VC
SET HARBOURLIB=%HARBOURDIR%\lib\vc
SET HARBOUREXE=%HARBOURDIR%\bin\vc\harbour.exe
SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%PATH%
SET __COMPILERLIBS=winmm.lib kernel32.lib comdlg32.lib comctl32.lib user32.lib winspool.lib ole32.lib oleaut32.lib odbc32.lib shell32.lib gdi32.lib uuid.lib wsock32.lib ws2_32.lib wininet.lib advapi32.lib rasapi32.lib version.lib

IF NOT EXIST harupdf.prg GOTO noexist
IF NOT EXIST %HARBOUREXE% GOTO noharbour
IF EXIST harupdf.exe DEL harupdf.exe
IF EXIST harupdf.c DEL harupdf.c
IF EXIST harupdf.obj DEL harupdf.obj

%HARBOUREXE% harupdf /n /i%HARBOURDIR%\include /gc2 /es2 /w /q
IF NOT EXIST harupdf.c GOTO end
CL /c /TP /I%HARBOURDIR%\INCLUDE /I%CC_DIR%\INCLUDE /Ogt2yb1p /FD /GB /GA /Gs /W3 /nologo harupdf.c
IF NOT EXIST harupdf.obj GOTO end
LINK /LIBPATH:%HARBOURDIR%\LIB\VC /LIBPATH:%CC_DIR%\LIB /NODEFAULTLIB:LIBCMT.LIB /OUT:harupdf.exe /FORCE:MULTIPLE /INCLUDE:__matherr /SUBSYSTEM:CONSOLE harupdf.obj %_MYLIB% %__COMPILERLIBS%

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
