@echo off
rem
rem $Id: bld.bat,v 1.36 2004/01/06 21:17:32 paultucker Exp $
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

rem if "%HB_ARCHITECTURE%" == "" set HB_ARCHITECTURE=dos
rem if "%HB_COMPILER%" == "" set HB_COMPILER=djgpp
rem if "%HB_GT_LIB%" == "" set HB_GT_LIB=

if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=..\bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=..\lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=..\include

:START

   if "%HB_ARCHITECTURE%" == "" goto NO_ARCH
   if "%HB_COMPILER%" == "" goto NO_COMP

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
   echo   - Environment variables HB_ARCHITECTURE, HB_COMPILER, HB_GT_LIB
   echo     should be set. Setting HB_GT_LIB is optional.
   echo     The following values are currently supported:
   echo.
   echo     HB_ARCHITECTURE:
   echo       - dos   (HB_GT_LIB=gtdos by default)
   echo       - w32   (HB_GT_LIB=gtwin by default)
   echo       - linux (HB_GT_LIB=gtstd by default)
   echo       - os2   (HB_GT_LIB=gtos2 by default)
   echo.
   pause
   echo     HB_COMPILER:
   echo       - When HB_ARCHITECTURE=dos
   echo         - bcc16   (Borland C++ 3.x, 4.x, 5.0x, DOS 16-bit)
   echo         - djgpp   (Delorie GNU C, DOS 32-bit)
   echo         - rxs32   (EMX/RSXNT/DOS GNU C, DOS 32-bit)
   echo         - watcom  (OpenWatcom, DOS 32-bit)
   echo       - When HB_ARCHITECTURE=w32
   echo         - bcc32   (Borland C++ 4.x, 5.x, Windows 32-bit)
   echo         - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)
   echo         - mingw32 (Cygnus/MinGW GNU C, Windows 32-bit)
   echo         - rxsnt   (EMX/RSXNT/Win32 GNU C, Windows 32-bit)
   echo         - icc     (IBM Visual Age C++, Windows 32-bit)
   echo         - msvc    (Microsoft Visual C++, Windows 32-bit)
   echo         - watcom  (OpenWatcom, Windows 32-bit)
   echo       - When HB_ARCHITECTURE=linux
   echo         - gcc     (GNU C, 32-bit)
   echo       - When HB_ARCHITECTURE=os2
   echo         - gcc     (EMX GNU C, OS/2 32-bit)
   echo         - icc     (IBM Visual Age C++ 3.0, OS/2 32-bit)
   echo.
   pause
   echo     HB_GT_LIB:
   echo       - gtstd (Standard streaming) (for all architectures)
   echo       - gtdos (DOS console)        (for dos architecture)
   echo       - gtwin (Win32 console)      (for w32 architecture)
   echo       - gtwvt (Win32 win console)  (for w32 architecture)
   echo       - gtos2 (OS/2 console)       (for os2 architecture)
   echo       - gtpca (PC ANSI console)    (for all architectures)
   echo       - gtcrs (Curses console)     (for linux, w32 architectures)
   echo       - gtsln (Slang console)      (for linux, w32 architectures)
   goto END

:NO_ARCH

   echo Error: HB_ARCHITECTURE is not set.
   goto HELP

:NO_COMP

   echo Error: HB_COMPILER is not set.
   goto HELP

:BAD_ARCH

   echo Error: HB_ARCHITECTURE value is unsupported.
   goto HELP

:BAD_COMP

   echo Error: HB_COMPILER value is unsupported.
   goto HELP

:COMPILE

   %HB_BIN_INSTALL%\harbour %1.prg -n -q0 -gc -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
   IF NOT '%2'=='' %HB_BIN_INSTALL%\harbour %2.prg -n -q0 -gc -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
   IF NOT '%2'=='' SET HB_2nd_prg=%2.c
   IF '%2'=='' SET HB_2nd_prg=
   IF NOT '%3'=='' %HB_BIN_INSTALL%\harbour %3.prg -n -q0 -gc -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
   IF NOT '%3'=='' SET HB_3rd_prg=%3.c
   IF '%3'=='' SET HB_3rd_prg=

:A_DOS

   if not "%HB_GT_LIB%" == "" set _HB_GT_LIB=%HB_GT_LIB%

   if not "%HB_ARCHITECTURE%" == "dos" goto A_W32

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtdos

   if not "%HB_COMPILER%" == "bcc16" goto A_DOS_BCC16_NOT

      echo -O2 -d -mh %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% > build.tmp
      echo -e%1.exe >> build.tmp
      echo %1.c >> build.tmp
      echo debug.lib >> build.tmp
      echo vm.lib >> build.tmp
      echo rtl.lib >> build.tmp
      echo %_HB_GT_LIB%.lib >> build.tmp
      echo lang.lib >> build.tmp
      echo rdd.lib >> build.tmp
      echo macro.lib >> build.tmp
      echo pp.lib >> build.tmp
      echo dbfdbt.lib >> build.tmp
      echo dbffpt.lib >> build.tmp
      echo dbfntx.lib >> build.tmp
      echo dbfcdx.lib >> build.tmp
      echo common.lib >> build.tmp
      bcc @build.tmp
      del build.tmp
      goto END

:A_DOS_BCC16_NOT

   if not "%HB_COMPILER%" == "djgpp" goto A_DOS_DJGPP_NOT

      echo %1.c > build.tmp
      echo -o%1.exe %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% >> build.tmp
      echo -ldebug >> build.tmp
      echo -lvm >> build.tmp
      echo -lrtl >> build.tmp
      echo -l%_HB_GT_LIB% >> build.tmp
      echo -llang >> build.tmp
      echo -lrdd >> build.tmp
      echo -lrtl >> build.tmp
      echo -lvm >> build.tmp
      echo -lmacro >> build.tmp
      echo -lpp >> build.tmp
      echo -ldbfdbt >> build.tmp
      echo -ldbffpt >> build.tmp
      echo -ldbfntx >> build.tmp
      echo -ldbfcdx >> build.tmp
      echo -lcommon >> build.tmp
      echo -lm >> build.tmp
      gcc @build.tmp
      del build.tmp
      goto END

:A_DOS_DJGPP_NOT

   if not "%HB_COMPILER%" == "rsx32" GOTO A_DOS_RSX32_NOT

      gcc %1.c -Zrsx32 %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfdbt -ldbffpt -ldbfntx -ldbfcdx -lcommon
      goto END

:A_DOS_RSX32_NOT

   if not "%HB_COMPILER%" == "watcom" goto END

      wpp386 -j -w3 -d2 -5s -5r -fp5 -oehtz -s -zq -zt0 -bt=DOS %1.c -fo=%1.obj
      echo debug all OP osn=DOS OP stack=65536 OP CASEEXACT OP stub=cwstub.exe NAME %1.exe > build.tmp
      echo FILE %1.obj >> build.tmp
      echo LIB debug.lib >> build.tmp
      echo LIB vm.lib >> build.tmp
      echo LIB rtl.lib >> build.tmp
      echo LIB %_HB_GT_LIB%.lib >> build.tmp
      echo LIB lang.lib >> build.tmp
      echo LIB rdd.lib >> build.tmp
      echo LIB macro.lib >> build.tmp
      echo LIB pp.lib >> build.tmp
      echo LIB dbfntx.lib >> build.tmp
      echo LIB dbfcdx.lib >> build.tmp
      echo LIB dbfdbt.lib >> build.tmp
      echo LIB dbffpt.lib >> build.tmp
      echo LIB common.lib >> build.tmp
      wlink @build.tmp
      del build.tmp
      goto END

:A_W32

   if not "%HB_ARCHITECTURE%" == "w32" goto A_OS2

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtwin
   if "%HB_COMPILER%" == "msvc"    goto C_MSVC

   if "%HB_DLL%" == "" set HB_LIBLIST=common.lib debug.lib vm%HB_MT%.lib rtl%HB_MT%.lib %_HB_GT_LIB%.lib lang.lib rdd%HB_MT%.lib macro%HB_MT%.lib pp%HB_MT%.lib dbfdbt%HB_MT%.lib dbffpt%HB_MT%.lib dbfntx%HB_MT%.lib dbfcdx%HB_MT%.lib samples.lib hbzip.lib %ADS_LIBS% %HB_USER_LIBS%
   if not "%HB_DLL%" == "" set HB_LIBLIST=harbour.lib %_HB_GT_LIB%.lib samples.lib hbzip.lib vm.lib %ADS_LIBS% %HB_USER_LIBS%
   
   if not "%HB_MT%" == "" SET BC_MT_FLAG=-tWM   
   if "%HB_MT%" == "" SET BC_MT_FLAG=

   if "%HB_COMPILER%" == "bcc32"   if     exist ..\lib\bcc640%HB_MT%.lib bcc32 %BC_MT_FLAG% -O2 -d %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% bcc640%HB_MT%.lib %HB_LIBLIST%
   if "%HB_COMPILER%" == "bcc32"   if not exist ..\lib\bcc640.lib bcc32 %BC_MT_FLAG% -O2 -d %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% %HB_LIBLIST%
   if "%HB_COMPILER%" == "gcc"     gcc %1.c -o%1.exe %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfdbt -ldbffpt -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "mingw32" gcc %1.c -o%1.exe %CFLAGS% -mno-cygwin %HB_INC_TEMP% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %HB_LIB_TEMP% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd%HB_MT% -lrtl%HB_MT% -lvm%HB_MT% -lmacro -lpp%HB_MT% -ldbfdbt%HB_MT% -ldbffpt%HB_MT% -ldbfntx%HB_MT% -ldbfcdx%HB_MT% -lcommon -luser32 -lwinspool -lole32 -loleaut32 -luuid -lgdi32 -lcomctl32
   if "%HB_COMPILER%" == "rsxnt"   gcc %1.c -Zwin32 %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfdbt -ldbffpt -ldbfntx -ldbfcdx -lcommon

:C_MSVC
   if not "%HB_COMPILER%" == "msvc"  goto C_WATCOM

   if "%HB_DLL%" == "" set HB_LIBLIST=%HB_LIB_INSTALL%\debug.lib %HB_LIB_INSTALL%\vm%HB_MT%.lib %HB_LIB_INSTALL%\rtl%HB_MT%.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\lang.lib %HB_LIB_INSTALL%\rdd%HB_MT%.lib %HB_LIB_INSTALL%\macro%HB_MT%.lib %HB_LIB_INSTALL%\pp%HB_MT%.lib %HB_LIB_INSTALL%\dbfdbt%HB_MT%.lib %HB_LIB_INSTALL%\dbffpt%HB_MT%.lib %HB_LIB_INSTALL%\dbfntx%HB_MT%.lib %HB_LIB_INSTALL%\dbfcdx%HB_MT%.lib %HB_LIB_INSTALL%\common.lib %HB_LIB_INSTALL%\samples.lib %HB_LIB_INSTALL%\hbzip.lib %ADS_LIBS% %HB_USER_LIBS%
   if not "%HB_DLL%" == "" set HB_LIBLIST=%HB_LIB_INSTALL%\harbour.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\samples.lib %HB_LIB_INSTALL%\hbzip.lib msvcrt.lib %ADS_LIBS% %HB_USER_LIBS%

   if "%HB_MT%" == "" set LDFLAGS=/NODEFAULTLIB:LIBCMT
   if not "%HB_MT%" == "" set LDFLAGS=/NODEFAULTLIB:LIBC /NODEFAULTLIB:LIBCP

   if not "%HB_DLL%" == "" if "%HB_MT%" == "" set LDFLAGS=%LDFLAGS% /NODEFAULTLIB:LIBC 

   set _cons=CONSOLE
   if "%HB_GT_LIB%"=="gtwvt" set _cons=WINDOWS
   echo cl -TP -W3 %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% /link ..\obj\vc\mainstd.obj /subsystem:%_cons% /FORCE:MULTIPLE %LDFLAGS% %HB_LIBLIST% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib kernel32.lib gdi32.lib comctl32.lib> msvc.log
        cl -TP -W3 %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% /link ..\obj\vc\mainstd.obj /subsystem:%_cons% /FORCE:MULTIPLE %LDFLAGS% %HB_LIBLIST% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib kernel32.lib gdi32.lib comctl32.lib>>msvc.log
   set _cons=
   @type msvc.log
   @echo Ignore LNK4033 warning
   set LDFLAGS=

:C_WATCOM
   if not "%HB_COMPILER%" == "watcom"  goto end

   wpp386 -j -w3 -d2 -5s -5r -fp5 -oehtz -s -zq -zt0 -mf -bt=NT %1.c -fo=%1.obj
   echo debug all OP osn=NT OP stack=65536 OP CASEEXACT NAME %1.exe > build.tmp
   echo FILE %1.obj >> build.tmp
   echo LIB debug.lib >> build.tmp
   echo LIB vm.lib >> build.tmp
   echo LIB rtl.lib >> build.tmp
   echo LIB %_HB_GT_LIB%.lib >> build.tmp
   echo LIB lang.lib >> build.tmp
   echo LIB macro.lib >> build.tmp
   echo LIB pp.lib >> build.tmp
   echo LIB dbfntx.lib >> build.tmp
   echo LIB dbfcdx.lib >> build.tmp
   echo LIB dbfdbt.lib >> build.tmp
   echo LIB dbffpt.lib >> build.tmp
   echo LIB rdd.lib >> build.tmp
   echo LIB common.lib >> build.tmp
   echo LIB kernel32.lib >> build.tmp
   echo LIB user32.lib >> build.tmp
   echo LIB winspool.lib >> build.tmp
   echo LIB oleaut32.lib >> build.tmp
   echo LIB uuid.lib >> build.tmp
   echo LIB comctl32.lib >> build.tmp
   wlink @build.tmp
   del build.tmp
   goto END

:A_OS2

   if not "%HB_ARCHITECTURE%" == "os2" goto A_LINUX

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtos2

   if "%HB_COMPILER%" == "gcc"     gcc %1.c %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp  -ldbfdbt -ldbffpt -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "icc"     icc /Gs+ /W2 /Se /Sd+ /Ti+ /C- /Tp %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_LIB_INSTALL%\debug.lib %HB_LIB_INSTALL%\vm.lib %HB_LIB_INSTALL%\rtl.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\lang.lib %HB_LIB_INSTALL%\rdd.lib %HB_LIB_INSTALL%\rtl.lib %HB_LIB_INSTALL%\vm.lib %HB_LIB_INSTALL%\macro%HB_MT%.lib %HB_LIB_INSTALL%\pp.lib %HB_LIB_INSTALL%\dbfdbt.lib %HB_LIB_INSTALL%\dbffpt.lib %HB_LIB_INSTALL%\dbfntx.lib %HB_LIB_INSTALL%\dbfcdx.lib %HB_LIB_INSTALL%\common.lib
   goto END

:A_LINUX

   if not "%HB_ARCHITECTURE%" == "linux" goto BAD_ARCH

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtstd

   if "%HB_COMPILER%" == "gcc"     gcc %1.c %CFLAGS% -I%HB_INC_INSTALL% -L../lib -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfdbt -ldbffpt -ldbfdbt -ldbffpt -ldbfntx -ldbfcdx -lcommon
   goto END

:CLEANUP

   del %1.c

:END
