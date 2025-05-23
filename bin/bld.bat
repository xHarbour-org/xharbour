@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

rem ---------------------------------------------------------------
rem Template to build a final Harbour executable, using Harbour
rem with the C code generation feature, then calling the proper C
rem linker/compiler.
rem
rem Copyright 1999-2001 Viktor Szakats (viktor.szakats@syenar.hu)
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

rem if "%HB_ARCH%" == "" set HB_ARCH=dos
rem if "%CC%" == "" set CC=djgpp
rem if "%HB_GT_LIB%" == "" set HB_GT_LIB=

if "%HB_BIN_INSTALL%" == "" if "%HB_INSTALL%" == "" (
   set "HB_BIN_INSTALL=%~dp0"
   set "HB_INSTALL=%~dp0..\"
   set "HB_LIB_INSTALL=%HB_INSTALL%lib\%SUB_DIR%"
   set "HB_INC_INSTALL=%HB_INSTALL%include"

   echo WARNING: HB_INSTALL is not set. Deducing from batch file location!
)

echo HB_BIN_INSTALL=%HB_BIN_INSTALL%
echo HB_LIB_INSTALL=%HB_LIB_INSTALL%
echo HB_INC_INSTALL=%HB_INC_INSTALL%
echo HB_LIBLIST=%HB_LIBLIST%

:START

   if "%HB_ARCH%" == "" goto NO_ARCH
   if "%CC%" == "" goto NO_COMP

   if not "%1" == "" goto COMPILE

:HELP

   echo.
   echo Usage: bld filename
   echo.
   echo Notes:
   echo.
   echo   - 'filename' is the .prg filename *without* extension.
   echo   - Don't forget to make a MAIN() function for you application.
   echo   - This batch file assumes you are in some directory off the main
   echo     harbour directory.
   echo   - Environment variables HB_ARCH, CC, HB_GT_LIB
   echo     should be set. Setting HB_GT_LIB is optional.
   echo     The following values are currently supported:
   echo.
   echo     HB_ARCH:
   echo       - dos   (HB_GT_LIB=gtdos by default)
   echo       - w32   (gtwin for console or gtgui for windows applications)
   echo       - w64   (gtwin for console or gtgui for windows applications)
   echo       - linux (HB_GT_LIB=gtstd by default)
   echo       - os2   (HB_GT_LIB=gtos2 by default)
   echo.
   pause
   echo     CC:
   echo       - When HB_ARCH=dos
   echo         - bcc16   (Borland C++ 3.x, 4.x, 5.0x, DOS 16-bit)
   echo         - djgpp   (Delorie GNU C, DOS 32-bit)
   echo         - rxs32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)
   echo         - watcom  (OpenWatcom, DOS 32-bit)
   echo       - When HB_ARCH=w32/w64
   echo         - bcc32[c] (Borland C++ 4.x, 5.x, Windows 32/64-bit)
   echo         - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)
   echo         - mingw32 (Cygnus/MinGW GNU C, Windows 32-bit)
   echo         - rxsnt   (EMX/RSXNT/Win32 GNU C, Windows 32-bit)
   echo         - icc     (IBM Visual Age C++, Windows 32-bit)
   echo         - msc     (Microsoft C++, Windows 32/64-bit)
   echo         - watcom  (OpenWatcom, Windows 32-bit)
   echo         - clang   (MingW-Clang, Windows 32/64-bit)
   echo       - When HB_ARCH=linux
   echo         - gcc     (GNU C, 32-bit)
   echo       - When HB_ARCH=os2
   echo         - gcc     (EMX GNU C, OS/2 32-bit)
   echo         - icc     (IBM Visual Age C++ 3.0, OS/2 32-bit)
   echo.
   pause
   echo     HB_GT_LIB:
   echo       - gtstd (Standard streaming) (for all architectures)
   echo       - gtcgi (Cgi streaming)      (for all architectures)
   echo       - gtdos (DOS console)        (for dos architecture)
   echo       - gtgui (Win32 NON console)  (for w32 architecture)
   echo       - gtwin (Win32 console)      (for w32 architecture)
   echo       - gtwvt (Win32 win console)  (for w32 architecture)
   echo       - gtos2 (OS/2 console)       (for os2 architecture)
   echo       - gtpca (PC ANSI console)    (for all architectures)
   echo       - gtcrs (Curses console)     (for linux, w32 architectures)
   echo       - gtsln (Slang console)      (for linux, w32 architectures)
   goto END

:NO_ARCH

   echo Error: HB_ARCH is not set.
   goto HELP

:NO_COMP

   echo Error: CC is not set.
   goto HELP

:BAD_ARCH

   echo Error: HB_ARCH value is unsupported.
   goto HELP

:BAD_COMP

   echo Error: CC value is unsupported.
   goto HELP

:COMPILE

   echo Compiling...
   echo %HB_BIN_INSTALL%\harbour %1.prg -n -q0 -gc -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
   %HB_BIN_INSTALL%\harbour %1.prg -n -q0 -gc -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
   IF ERRORLEVEL 1 GOTO END
   IF NOT '%2'=='' %HB_BIN_INSTALL%\harbour %2.prg -n -q0 -gc -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
   IF ERRORLEVEL 1 GOTO END
   IF NOT '%2'=='' SET HB_2nd_prg=%2.c
   IF '%2'=='' SET HB_2nd_prg=
   IF NOT '%3'=='' %HB_BIN_INSTALL%\harbour %3.prg -n -q0 -gc -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
   IF ERRORLEVEL 1 GOTO END
   IF NOT '%3'=='' SET HB_3rd_prg=%3.c
   IF '%3'=='' SET HB_3rd_prg=

:A_DOS

   if not "%HB_GT_LIB%" == "" set _HB_GT_LIB=%HB_GT_LIB%

   if not "%HB_ARCH%" == "dos" goto A_W32_64

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtdos

   if not "%CC%" == "bcc16" goto A_DOS_BCC16_NOT

      echo -O2 -d -mh %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% > build.tmp
      echo -e%1.exe >> build.tmp
      echo %1.c >> build.tmp
      echo debug.lib >> build.tmp
      echo vm.lib >> build.tmp
      echo rtl.lib >> build.tmp
      echo zlib.lib >> build.tmp
      echo pcrepos.lib >> build.tmp
      echo %_HB_GT_LIB%.lib >> build.tmp
      echo lang.lib >> build.tmp
      echo rdd.lib >> build.tmp
      echo macro.lib >> build.tmp
      echo pp.lib >> build.tmp
      echo dbffpt.lib >> build.tmp
      echo dbfntx.lib >> build.tmp
      echo dbfcdx.lib >> build.tmp
      echo bmdbfcdx.lib >> build.tmp
      echo redbfcdx.lib >> build.tmp
      echo hsx.lib >> build.tmp
      echo hbsix.lib >> build.tmp
      echo common.lib >> build.tmp
      bcc @build.tmp
      del build.tmp
      goto END

:A_DOS_BCC16_NOT

   if not "%CC%" == "djgpp" goto A_DOS_DJGPP_NOT

      echo %1.c > build.tmp
      echo -o%1.exe %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% >> build.tmp
      echo -ldebug >> build.tmp
      echo -lvm >> build.tmp
      echo -lrtl >> build.tmp
      echo -lzlib >> build.tmp
      echo -lpcrepos >> build.tmp
      echo -l%_HB_GT_LIB% >> build.tmp
      echo -llang >> build.tmp
      echo -lrdd >> build.tmp
      echo -lrtl >> build.tmp
      echo -lvm >> build.tmp
      echo -lmacro >> build.tmp
      echo -lpp >> build.tmp
      echo -ldbffpt >> build.tmp
      echo -ldbfntx >> build.tmp
      echo -ldbfcdx >> build.tmp
      echo -lbmdbfcdx >> build.tmp
      echo -lredbfcdx >> build.tmp
      echo -lhsx >> build.tmp
      echo -lhbsix >> build.tmp
      echo -lcommon >> build.tmp
      echo -lm >> build.tmp
      gcc @build.tmp
      del build.tmp
      goto END

:A_DOS_DJGPP_NOT

   if not "%CC%" == "rsx32" GOTO A_DOS_RSX32_NOT

      gcc %1.c -Zrsx32 %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbffpt -ldbfntx -ldbfcdx -lbmdbfcdx -lredbfcdx -lcommon
      goto END

:A_DOS_RSX32_NOT

   if not "%CC%" == "watcom" goto END

      wpp386 -j -w2 -d1 -zq -bt=DOS -5 -fp5 -onaehtzr -oi+ -ei -zp8 -s -zt0 %1.c -fo=%1.obj
      echo debug all OP osn=DOS OP stack=65536 OP CASEEXACT OP stub=cwstub.exe NAME %1.exe > build.tmp
      echo FILE %1.obj >> build.tmp
      echo LIB debug.lib >> build.tmp
      echo LIB vm.lib >> build.tmp
      echo LIB rtl.lib >> build.tmp
      echo LIB zlib.lib >> build.tmp
      echo LIB pcrepos.lib >> build.tmp
      echo LIB %_HB_GT_LIB%.lib >> build.tmp
      echo LIB codepage.lib >> build.tmp
      echo LIB lang.lib >> build.tmp
      echo LIB rdd.lib >> build.tmp
      echo LIB macro.lib >> build.tmp
      echo LIB pp.lib >> build.tmp
      echo LIB dbfntx.lib >> build.tmp
      echo LIB dbfcdx.lib >> build.tmp
      echo LIB bmdbfcdx.lib >> build.tmp
      echo LIB redbfcdx.lib >> build.tmp
      echo LIB dbffpt.lib >> build.tmp
      echo LIB hsx.lib >> build.tmp
      echo LIB hbsix.lib >> build.tmp
      echo LIB common.lib >> build.tmp
      echo LIB ct.lib >> build.tmp
      wlink @build.tmp
      del build.tmp
      goto END

:A_W32_64

   if not "%HB_ARCH%" == "w32" if not "%HB_ARCH%" == "w64" goto A_OS2
   if "%CC%" == "cl"      goto C_MSVC
   if "%CC%" == "watcom"  goto C_WATCOM
   if "%CC%" == "mingw32" goto C_MINGW32
   if "%CC%" == "clang"   goto C_CLANG
   if "%HB_GTALLEG%" == "yes" set HB_ALGLIB=alleg.lib

   if "%_HB_GT_LIB%" == "" set _HB_GT_LIB=gtwin

   if "%HB_DLL%" == "" set HB_LIBLIST=%HB_FIRST_LIBS% common.lib debug.lib vm%HB_MT%.lib rtl.lib zlib.lib pcrepos.lib %_HB_GT_LIB%.lib lang.lib rdd.lib macro.lib pp.lib dbffpt.lib dbfntx.lib dbfcdx.lib bmdbfcdx.lib redbfcdx.lib hsx.lib hbsix.lib ct.lib tip.lib %ADS_LIBS% %HB_USER_LIBS% %HB_ALGLIB%
   if not "%HB_DLL%" == "" set HB_LIBLIST=%HB_FIRST_LIBS% harbour.lib %_HB_GT_LIB%.lib vm.lib %ADS_LIBS% %HB_USER_LIBS% %HB_ALGLIB%

rem   if exist %HB_LIB_INSTALL%\hbzip.lib set HB_LIBLIST=%HB_LIBLIST% hbzip.lib

   if not "%HB_MT%" == "" SET BC_MT_FLAG=-tWM
   if "%HB_MT%" == "" SET BC_MT_FLAG=
   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtwin

   if "%CC%" == "bcc32c"  if     exist %HB_LIB_INSTALL%\bcc640%HB_MT%.lib bcc32c %BC_MT_FLAG% -O2 -d %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% bcc640%HB_MT%.lib %HB_LIBLIST%
   if "%CC%" == "bcc32c"  if not exist %HB_LIB_INSTALL%\bcc640.lib bcc32c %BC_MT_FLAG% -O2 -d %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% %HB_LIBLIST%
   if "%CC%" == "bcc32"   if     exist %HB_LIB_INSTALL%\bcc640%HB_MT%.lib bcc32 %BC_MT_FLAG% -O2 -d %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% bcc640%HB_MT%.lib %HB_LIBLIST%
   if "%CC%" == "bcc32"   if not exist %HB_LIB_INSTALL%\bcc640.lib bcc32 %BC_MT_FLAG% -O2 -d %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% %HB_LIBLIST%
   if "%CC%" == "gcc"     gcc %1.c -o%1.exe %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbffpt -ldbfntx -ldbfcdx -lbmdbfcdx -lredbfcdx -lcommon -lct -ltip


   if "%CC%" == "rsxnt"   gcc %1.c -Zwin32 %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbffpt -ldbfntx -ldbfcdx -lbmdbfcdx -lredbfcdx -lcommon
   goto end

:C_MINGW32
   if not "%CC%" == "mingw32" goto C_MSVC
   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtwin
   set _HB_GT_LIB= -l%_HB_GT_LIB% -lgtwvt
   gcc %1.c -o%1.exe %HB_TMP_OBJ% %CFLAGS% -mno-cygwin %HB_TMP_INCLUDE% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %HB_TMP_INSTALL% -Wl,--start-group -ldebug -lvm%HB_MT% -lrtl -l%_HB_GT_LIB% -llang -lcodepage -lrdd -lmacro -lpp -ldbffpt -ldbfntx -ldbfcdx -lbmdbfcdx -lredbfcdx -lhsx -lhbsix -lcommon -lct -lhbodbc -ltip -lzlib -lpcrepos %HB_TMP_LIB% -Wl,--end-group -luser32 -lwinspool -lole32 -loleaut32 -luuid -lgdi32 -lcomctl32 -lcomdlg32 -lodbc32 -lmapi32 -lws2_32
   goto end

:C_MSVC
   if not "%CC%" == "cl"  goto C_WATCOM
   

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtwin
   if "%HB_DLL%" == "" set HB_LIBLIST=%HB_FIRST_LIBS% debug.lib vm%HB_MT%.lib rtl.lib zlib.lib pcrepos.lib %_HB_GT_LIB%.lib lang.lib rdd.lib macro.lib pp.lib dbffpt.lib dbfntx.lib dbfcdx.lib bmdbfcdx.lib redbfcdx.lib hsx.lib hbsix.lib sixcdx.lib common.lib ct.lib tip.lib usrrdd.lib rdds.lib %ADS_LIBS% %HB_USER_LIBS%
   if not "%HB_DLL%" == "" set HB_LIBLIST=%HB_FIRST_LIBS% %HB_LIB_INSTALL%\harbour.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib msvcrt.lib %ADS_LIBS% %HB_USER_LIBS%

   if exist %HB_LIB_INSTALL%\hbzip.lib set HB_LIBLIST=%HB_LIBLIST% hbzip.lib
   if exist %HB_LIB_INSTALL%\libharu.lib if exist %HB_LIB_INSTALL%\hbhpdf.lib set HB_LIBLIST=%HB_LIBLIST% libharu.lib hbhpdf.lib
   if exist %HB_LIB_INSTALL%\png.lib set HB_LIBLIST=%HB_LIBLIST% png.lib

rem   if "%HB_MT%" == "" set LDFLAGS=/NODEFAULTLIB:LIBCMT
   if not "%HB_MT%" == "" set LDFLAGS=/NODEFAULTLIB:LIBC /NODEFAULTLIB:LIBCP

   if not "%HB_DLL%" == "" if "%HB_MT%" == "" set LDFLAGS=%LDFLAGS% /NODEFAULTLIB:LIBC

   set LDFLAGS=%LDFLAGS% /LIBPATH:"%HB_LIB_INSTALL%"

   set _cons=CONSOLE
   set _main=std
   if "%HB_GT_LIB%"=="gtwvt" set _cons=WINDOWS
   if "%HB_GT_LIB%"=="gtwvt" set _main=win
   if "%HB_GT_LIB%"=="gtwvw" set _cons=WINDOWS
   if "%HB_GT_LIB%"=="gtwvw" set _main=win
   if "%HB_GT_LIB%"=="gtgui" set _cons=WINDOWS
   if "%HB_GT_LIB%"=="gtgui" set _main=win
   if "%HB_GT_LIB%"=="gtnul" set _cons=WINDOWS
   if "%HB_GT_LIB%"=="gtnul" set _main=win
   echo cl -TP -W3 %CFLAGS% -I"%HB_INC_INSTALL%" %1.c %HB_2nd_prg% %HB_3rd_prg% /link %LFLAGS% %HB_INSTALL%\obj\%SUB_DIR%\main%_main%.obj /subsystem:%_cons% /FORCE:MULTIPLE %LDFLAGS% %HB_LIBLIST% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib kernel32.lib gdi32.lib comctl32.lib comdlg32.lib advapi32.lib> msvc.log
        cl -TP -W3 %CFLAGS% -I"%HB_INC_INSTALL%" %1.c %HB_2nd_prg% %HB_3rd_prg% /link %LFLAGS% %HB_INSTALL%\obj\%SUB_DIR%\main%_main%.obj /subsystem:%_cons% /FORCE:MULTIPLE %LDFLAGS% %HB_LIBLIST% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib kernel32.lib gdi32.lib comctl32.lib comdlg32.lib advapi32.lib>>msvc.log
   set _cons=
   set _main=
   @type msvc.log
   @echo Ignore LNK4033 and LNK4254 warnings
   set LDFLAGS=
   goto END

:C_WATCOM
   if not "%CC%" == "watcom"  goto end
   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtwin

   wpp386 -j -w2 -d1 -5 -fp5 -s -onaehtzr -oi+ -ei -zp8 -zq -zt0 -mf -bt=NT %1.c -fo=%1.obj
   echo debug all OP stack=65536 OP CASEEXACT NAME %1.exe > build.tmp
   echo system nt >> build.tmp
   echo FILE %1.obj >> build.tmp
   echo LIB debug.lib >> build.tmp
   echo LIB vm%HB_MT%.lib >> build.tmp
   echo LIB rtl.lib >> build.tmp
   echo LIB zlib.lib >> build.tmp
   echo LIB pcrepos.lib >> build.tmp
   echo LIB %_HB_GT_LIB%.lib >> build.tmp
   echo LIB gtwvt.lib >> build.tmp
   echo LIB codepage.lib >> build.tmp
   echo LIB lang.lib >> build.tmp
   echo LIB macro.lib >> build.tmp
   echo LIB pp.lib >> build.tmp
   echo LIB dbfntx.lib >> build.tmp
   echo LIB dbfcdx.lib >> build.tmp
   echo LIB bmdbfcdx.lib >> build.tmp
   echo LIB redbfcdx.lib >> build.tmp
   echo LIB dbffpt.lib >> build.tmp
   echo LIB rdd.lib >> build.tmp
   echo LIB hsx.lib >> build.tmp
   echo LIB hbsix.lib >> build.tmp
   echo LIB common.lib >> build.tmp
   echo LIB hbodbc.lib >> build.tmp
   echo LIB ct.lib >> build.tmp
   echo LIB tip.lib >> build.tmp
   echo LIB kernel32.lib >> build.tmp
   echo LIB user32.lib >> build.tmp
   echo LIB winspool.lib >> build.tmp
   echo LIB oleaut32.lib >> build.tmp
   echo LIB uuid.lib >> build.tmp
   echo LIB comctl32.lib >> build.tmp
   echo LIB odbc32.lib >> build.tmp
   echo LIB mapi32.lib >> build.tmp
   wlink @build.tmp
   del build.tmp
   goto END

:C_CLANG
   if not "%CC%" == "clang"  goto end

   if "%HB_ARCH%"  == "" set HB_ARCH=w32
   if "%HB_ARCH%%" == "w32" set ARCH_FLAG=-m32
   if "%HB_ARCH%%" == "w64" set ARCH_FLAG=-m64

   if "%HB_ARCH%%" == "w64" set "HB_TARGET=-target x86_64-w64-mingw32-ucrt"
   if "%HB_ARCH%%" == "w32" set "HB_TARGET=-target i686-w64-mingw32-ucrt"


   if "%HB_GT_LIB%" == "" set HB_GT_LIB=gtwin

   if "%HB_GT_LIB%" == "gtwin" set _cons=-mconsole & set _main=std
   if "%HB_GT_LIB%" == "gtwvt" set _cons=-mwindows & set _main=win
   if "%HB_GT_LIB%" == "gtwvw" set _cons=-mwindows & set _main=win
   if "%HB_GT_LIB%" == "gtgui" set _cons=-mwindows & set _main=win
   if "%HB_GT_LIB%" == "gtnul" set _cons=-mwindows & set _main=win

   echo clang -c %ARCH_FLAG% -o %1.o %1.c %CFLAGS% -I%HB_INC_INSTALL% 
        clang -c %ARCH_FLAG% -o %1.o %1.c %CFLAGS% -I%HB_INC_INSTALL% 

   echo Clang %ARCH_FLAG% %_cons% -Wall -o %1.exe ../obj/%SUB_DIR%/main%_main%.o %1.o -L../lib/%SUB_DIR% -Wl,--start-group -lvm -lrtl -lrdd -lct -lpng -lzlib -ltiff -ljpeg -lpdflite -lgtwin -llibmisc -lcodepage -lmacro -ldbfcdx -lbmdbfcdx -lsixcdx -ldbfntx -ldbfnsx -ldbffpt -lhbsix -lhsx -lpcrepos -lcommon -lpp -llang -Wl,--end-group -L%CC_DIR%/lib -luser32 -lwinspool -lcomctl32 -lcomdlg32 -lgdi32 -lwinmm -lmpr -lole32 -luuid -loleaut32 -liphlpapi -lws2_32
        Clang %ARCH_FLAG% %_cons% -Wall -o %1.exe ../obj/%SUB_DIR%/main%_main%.o %1.o -L../lib/%SUB_DIR% -Wl,--start-group -lvm -lrtl -lrdd -lct -lpng -lzlib -ltiff -ljpeg -lpdflite -lgtwin -llibmisc -lcodepage -lmacro -ldbfcdx -lbmdbfcdx -lsixcdx -ldbfntx -ldbfnsx -ldbffpt -lhbsix -lhsx -lpcrepos -lcommon -lpp -llang -Wl,--end-group -L%CC_DIR%/lib -luser32 -lwinspool -lcomctl32 -lcomdlg32 -lgdi32 -lwinmm -lmpr -lole32 -luuid -loleaut32 -liphlpapi -lws2_32

   set _cons=
   set _main=
   goto END

:A_OS2

   if not "%HB_ARCH%" == "os2" goto A_LINUX

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtos2

   if "%CC%" == "gcc" gcc %1.c %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp  -ldbffpt -ldbfntx -ldbfcdx -lbmdbfcdx -lredbfcdx -lcommon -lct -ltip
   if "%CC%" == "icc" icc /Gs+ /W2 /Se /Sd+ /Ti+ /C- /Tp %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_LIB_INSTALL%\debug.lib %HB_LIB_INSTALL%\vm.lib %HB_LIB_INSTALL%\rtl.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\lang.lib %HB_LIB_INSTALL%\rdd.lib %HB_LIB_INSTALL%\rtl.lib %HB_LIB_INSTALL%\vm.lib %HB_LIB_INSTALL%\macro.lib %HB_LIB_INSTALL%\pp.lib %HB_LIB_INSTALL%\dbffpt.lib %HB_LIB_INSTALL%\dbfntx.lib %HB_LIB_INSTALL%\dbfcdx.lib %HB_LIB_INSTALL%\bmdbfcdx.lib %HB_LIB_INSTALL%\redbfcdx.lib %HB_LIB_INSTALL%\common.lib %HB_LIB_INSTALL%\tip.lib
   goto END

:A_LINUX

   if not "%HB_ARCH%" == "linux" goto BAD_ARCH

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtstd

   if "%CC%" == "gcc" gcc %1.c %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbffpt -ldbffpt -ldbfntx -ldbfcdx -lbmdbfcdx -lredbfcdx -lcommon -lct -ltip
   goto END

:CLEANUP

   del %1.c

:END
