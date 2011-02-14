@echo off
rem
rem $Id$
rem
SET CC_DIR=C:\MINGW
SET _MYLIB=-lhbhpdf -lpng -lhbzip -lzlib -lvm -lrtl -lrdd -lvm -lct -lvm -lrtl -lgtwin -lmacro -ldbfcdx -lbmdbfcdx -lsixcdx -ldbfntx -lrtl -ldbffpt -lhbsix -lhsx -lpcrepos -lcommon -lpp -llang -luser32 -lwinspool -lcomctl32 -lcomdlg32 -lgdi32 -lwinmm -lmpr -lole32 -luuid -loleaut32
SET HARBOURDIR=..\..\..
SET HARBOURLIB=%HARBOURDIR%\lib\gc
SET HARBOUREXE=%HARBOURDIR%\bin\gc\harbour.exe
SET _PATH=%PATH%
SET PATH=%CC_DIR%\BIN;%HARBOURDIR%\BIN\GC;%_PATH%
SET __COMPILERLIBS=user32.lib winspool.lib ole32.lib oleaut32.lib advapi32.lib winmm.lib mpr.lib

IF NOT EXIST harupdf.prg GOTO noexist
IF NOT EXIST %HARBOUREXE% GOTO noharbour
IF EXIST harupdf.exe DEL harupdf.exe
IF EXIST harupdf.c DEL harupdf.c
IF EXIST harupdf.o DEL harupdf.o

%HARBOUREXE% harupdf.prg /d__GCC__ /n /i%HARBOURDIR%\include /gc3 /es2 /w
IF NOT EXIST harupdf.c GOTO end
GCC harupdf.c -D__WIN32__ -Wall -fno-strict-aliasing -mno-cygwin -O3 -mtune=pentium -c -I%HARBOURDIR%/INCLUDE -I%CC_DIR%/INCLUDE
IF NOT EXIST harupdf.o GOTO end
GCC -WAll -oharupdf.exe harupdf.o -mno-cygwin %_MYLIB% -L%CC_DIR%/LIB -L%HARBOURDIR%/LIB/GC

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
