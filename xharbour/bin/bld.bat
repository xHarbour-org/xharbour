@echo off
rem
rem $Id: bld.bat,v 1.21 2003/05/28 04:10:17 ronpinkas Exp $
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
   echo         - watcom  (Watcom C++ 9.x, 10.x, 11.x, DOS 32-bit)
   echo       - When HB_ARCHITECTURE=w32
   echo         - bcc32   (Borland C++ 4.x, 5.x, Windows 32-bit)
   echo         - gcc     (Cygnus/Cygwin GNU C, Windows 32-bit)
   echo         - mingw32 (Cygnus/MinGW GNU C, Windows 32-bit)
   echo         - rxsnt   (EMX/RSXNT/Win32 GNU C, Windows 32-bit)
   echo         - icc     (IBM Visual Age C++, Windows 32-bit)
   echo         - msvc    (Microsoft Visual C++, Windows 32-bit)
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

   %HB_BIN_INSTALL%\harbour %1.prg -n -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
   IF NOT '%2'=='' %HB_BIN_INSTALL%\harbour %2.prg -n -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
   IF NOT '%2'=='' SET HB_2nd_prg=%2.c
   IF '%2'=='' SET HB_2nd_prg=
   IF NOT '%3'=='' %HB_BIN_INSTALL%\harbour %3.prg -n -i%HB_INC_INSTALL% %HARBOURFLAGS% -p -w
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
      echo -ldbfntx >> build.tmp
      echo -ldbfcdx >> build.tmp
      echo -lcommon >> build.tmp
      echo -lm >> build.tmp
      gcc @build.tmp
      del build.tmp
      goto END

:A_DOS_DJGPP_NOT

   if "%HB_COMPILER%" == "rsx32"   gcc %1.c -Zrsx32 %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   goto END

:A_W32

   if not "%HB_ARCHITECTURE%" == "w32" goto A_OS2

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtwin

   if "%HB_COMPILER%" == "bcc32"   if exist ..\lib\bcc640.lib bcc32 -O2 -d %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% bcc640.lib common.lib  debug.lib vm%HB_MT%.lib rtl%HB_MT%.lib %_HB_GT_LIB%.lib lang.lib rdd%HB_MT%.lib macro%HB_MT%.lib pp%HB_MT%.lib dbfntx%HB_MT%.lib dbfcdx%HB_MT%.lib %ADS_LIBS%
   if "%HB_COMPILER%" == "bcc32"   if not exist ..\lib\bcc640.lib bcc32 -O2 -d %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% common.lib  debug.lib vm%HB_MT%.lib rtl%HB_MT%.lib %_HB_GT_LIB%.lib lang.lib rdd%HB_MT%.lib macro%HB_MT%.lib pp%HB_MT%.lib dbfntx%HB_MT%.lib dbfcdx%HB_MT%.lib %ADS_LIBS%
   if "%HB_COMPILER%" == "gcc"     gcc %1.c -o%1.exe %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "mingw32" gcc %1.c -o%1.exe %CFLAGS% -mno-cygwin -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd%HB_MT% -lrtl%HB_MT% -lvm%HB_MT% -lmacro -lpp%HB_MT% -ldbfntx%HB_MT% -ldbfcdx%HB_MT% -lcommon -luser32 -lwinspool -lole32 -loleaut32 -luuid
   if "%HB_COMPILER%" == "rsxnt"   gcc %1.c -Zwin32 %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon

   if not "%HB_COMPILER%" == "msvc"  goto END

   if "%HB_MT%"=="" set LDFLAGS=/NODEFAULTLIB:LIBCMT
   if not "%HB_MT%"=="" set LDFLAGS=/NODEFAULTLIB:LIBC

   echo cl -TP -W3 %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% /link /subsystem:CONSOLE /FORCE:MULTIPLE %LDFLAGS% %HB_LIB_INSTALL%\debug.lib %HB_LIB_INSTALL%\vm%HB_MT%.lib %HB_LIB_INSTALL%\rtl%HB_MT%.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\lang.lib %HB_LIB_INSTALL%\rdd%HB_MT%.lib %HB_LIB_INSTALL%\macro%HB_MT%.lib %HB_LIB_INSTALL%\pp%HB_MT%.lib %HB_LIB_INSTALL%\dbfntx%HB_MT%.lib %HB_LIB_INSTALL%\dbfcdx%HB_MT%.lib %HB_LIB_INSTALL%\common.lib %ADS_LIBS% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib >msvc.log
   cl -TP -W3 %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_2nd_prg% %HB_3rd_prg% /link /subsystem:CONSOLE /FORCE:MULTIPLE %LDFLAGS% %HB_LIB_INSTALL%\debug.lib %HB_LIB_INSTALL%\vm%HB_MT%.lib %HB_LIB_INSTALL%\rtl%HB_MT%.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\lang.lib %HB_LIB_INSTALL%\rdd%HB_MT%.lib %HB_LIB_INSTALL%\macro%HB_MT%.lib %HB_LIB_INSTALL%\pp%HB_MT%.lib %HB_LIB_INSTALL%\dbfntx%HB_MT%.lib %HB_LIB_INSTALL%\dbfcdx%HB_MT%.lib %HB_LIB_INSTALL%\common.lib %ADS_LIBS% shell32.lib user32.lib winspool.lib ole32.lib oleaut32.lib ws2_32.lib >> msvc.log
   @type msvc.log
   @echo Ignore LNK4033 warning
   set LDFLAGS=

   goto END

:A_OS2

   if not "%HB_ARCHITECTURE%" == "os2" goto A_LINUX

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtos2

   if "%HB_COMPILER%" == "gcc"     gcc %1.c %CFLAGS% -I%HB_INC_INSTALL% -L%HB_LIB_INSTALL% -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   if "%HB_COMPILER%" == "icc"     icc /Gs+ /W2 /Se /Sd+ /Ti+ /C- /Tp %CFLAGS% -I%HB_INC_INSTALL% %1.c %HB_LIB_INSTALL%\debug.lib %HB_LIB_INSTALL%\vm.lib %HB_LIB_INSTALL%\rtl.lib %HB_LIB_INSTALL%\%_HB_GT_LIB%.lib %HB_LIB_INSTALL%\lang.lib %HB_LIB_INSTALL%\rdd.lib %HB_LIB_INSTALL%\rtl.lib %HB_LIB_INSTALL%\vm.lib %HB_LIB_INSTALL%\macro%HB_MT%.lib %HB_LIB_INSTALL%\pp.lib %HB_LIB_INSTALL%\dbfntx.lib %HB_LIB_INSTALL%\dbfcdx.lib %HB_LIB_INSTALL%\common.lib
   goto END

:A_LINUX

   if not "%HB_ARCHITECTURE%" == "linux" goto BAD_ARCH

   if "%HB_GT_LIB%" == "" set _HB_GT_LIB=gtstd

   if "%HB_COMPILER%" == "gcc"     gcc %1.c %CFLAGS% -I%HB_INC_INSTALL% -L../lib -ldebug -lvm -lrtl -l%_HB_GT_LIB% -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon
   goto END

:CLEANUP

   del %1.c

:END
