@ECHO OFF
rem $Id: make_pc.bat,v 1.0 2005/10/26 00:00:00 modalsist Exp $
rem
rem Batch file for building xHarbour under MS-Windows with Pelles C compiler.
rem
rem NOTE: Pelles C is a free C/C++ compliler for MS-Windows platform, for more
rem       info visit: http://www.smorgasbordet.com/pellesc/index.htm
rem
rem 

REM *************************************
REM *** ADJUST ONLY THE 3 SETS BELLOW ***
REM *************************************

SET HB_DIR=c:\xmycvs
SET POCC_DIR=c:\pellesc
SET BISON_DIR=c:\bison

REM *******************************************
REM *** DON'T CHANGE THESE VARIABLES BELLOW ***
REM *******************************************

SET _PATH=%PATH%
SET _LIB=%LIB%
SET _INCLUDE=%INCLUDE%

SET LIB=%POCC_DIR\lib;%POCC_DIR\lib\win;%HB_DIR\lib;%LIB%
SET INCLUDE=%POCC_DIR\include;%POCC_DIR\include\win;%HB_DIR%\include;%INCLUDE%

SET PATH=%POCC_DIR%\bin;%BISON_DIR%\bin;%HB_DIR%\bin;%PATH%

SET TARGET_OBJ_DIR=%HB_DIR%\obj\pocc
SET TARGET_LIB_DIR=%HB_DIR%\lib\pocc
SET TARGET_BIN_DIR=%HB_DIR%\bin\pocc

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN


  if not exist obj                              md obj
  if not exist %TARGET_LIB_DIR%                 md %TARGET_LIB_DIR%
  if not exist %TARGET_BIN_DIR%                 md %TARGET_BIN_DIR%
  if not exist %TARGET_OBJ_DIR%                 md %TARGET_OBJ_DIR%

  if not exist %TARGET_OBJ_DIR%\ct              md %TARGET_OBJ_DIR%\ct
  if not exist %TARGET_OBJ_DIR%\opt             md %TARGET_OBJ_DIR%\opt
  if not exist %TARGET_OBJ_DIR%\opt\console     md %TARGET_OBJ_DIR%\opt\console
  if not exist %TARGET_OBJ_DIR%\opt\gui         md %TARGET_OBJ_DIR%\opt\gui

  if not exist %TARGET_OBJ_DIR%\mt              md %TARGET_OBJ_DIR%\mt
  if not exist %TARGET_OBJ_DIR%\mt\ct           md %TARGET_OBJ_DIR%\mt\ct
  if not exist %TARGET_OBJ_DIR%\mt\opt          md %TARGET_OBJ_DIR%\mt\opt
  if not exist %TARGET_OBJ_DIR%\mt\opt\console  md %TARGET_OBJ_DIR%\mt\opt\console
  if not exist %TARGET_OBJ_DIR%\mt\opt\gui      md %TARGET_OBJ_DIR%\mt\opt\gui


  echo Compiling binaries and core libs

  SET HB_MT=
  pomake /f makefile.pc > make_pc.log
  if errorlevel 1 goto BUILD_ERR
  SET HB_MT=mt
  pomake /f makefile.pc >> make_pc.log
  if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   copy bin\pocc\*.exe bin\*.* > nul
   copy lib\pocc\*.lib lib\*.* > nul
   if exist make_pc.log del make_pc.log
   goto EXIT

:BUILD_ERR

   edit make_pc.log
   goto EXIT

:CLEAN

   echo Cleaning binaries and core libs

   if exist %TARGET_BIN_DIR%\harbour.exe          del %TARGET_BIN_DIR%\harbour.exe
   if exist %TARGET_BIN_DIR%\harbour.exp          del %TARGET_BIN_DIR%\harbour.exp

   if exist %TARGET_BIN_DIR%\xharbour.exp         del %TARGET_BIN_DIR%\xharbour.exp
   if exist %TARGET_BIN_DIR%\xharbour.exe         del %TARGET_BIN_DIR%\xharbour.exe
   if exist %TARGET_BIN_DIR%\xharbour.dll         del %TARGET_BIN_DIR%\xharbour.dll

   if exist %TARGET_BIN_DIR%\hbdoc.exe            del %TARGET_BIN_DIR%\hbdoc.exe
   if exist %TARGET_BIN_DIR%\hbdocdll.exe         del %TARGET_BIN_DIR%\hbdocdll.exe

   if exist %TARGET_BIN_DIR%\hbmake.exe           del %TARGET_BIN_DIR%\hbmake.exe
   if exist %TARGET_BIN_DIR%\hbmakedll.exe        del %TARGET_BIN_DIR%\hbmakedll.exe

   if exist %TARGET_BIN_DIR%\hbpp.exe             del %TARGET_BIN_DIR%\hbpp.exe

   if exist %TARGET_BIN_DIR%\hbrun.exe            del %TARGET_BIN_DIR%\hbrun.exe
   if exist %TARGET_BIN_DIR%\hbrundll.exe         del %TARGET_BIN_DIR%\hbrundll.exe
   if exist %TARGET_BIN_DIR%\hbrunmt.exe          del %TARGET_BIN_DIR%\hbrunmt.exe

   if exist %TARGET_BIN_DIR%\hbtest.exe           del %TARGET_BIN_DIR%\hbtest.exe
   if exist %TARGET_BIN_DIR%\hbtestdll.exe        del %TARGET_BIN_DIR%\hbtestdll.exe
   if exist %TARGET_BIN_DIR%\hbtestmt.exe         del %TARGET_BIN_DIR%\hbtestmt.exe

   if exist %TARGET_BIN_DIR%\xbscript.exe         del %TARGET_BIN_DIR%\xbscript.exe
   if exist %TARGET_BIN_DIR%\xbscriptdll.exe      del %TARGET_BIN_DIR%\xbscriptdll.exe


   if exist %TARGET_LIB_DIR%\codepage.lib         del %TARGET_LIB_DIR%\codepage.lib
   if exist %TARGET_LIB_DIR%\common.lib           del %TARGET_LIB_DIR%\common.lib
   if exist %TARGET_LIB_DIR%\ct.lib               del %TARGET_LIB_DIR%\ct.lib
   if exist %TARGET_LIB_DIR%\ctmt.lib             del %TARGET_LIB_DIR%\ctmt.lib
   if exist %TARGET_LIB_DIR%\dbfcdx.lib           del %TARGET_LIB_DIR%\dbfcdx.lib
   if exist %TARGET_LIB_DIR%\dbfcdxmt.lib         del %TARGET_LIB_DIR%\dbfcdxmt.lib
   if exist %TARGET_LIB_DIR%\dbfdbt.lib           del %TARGET_LIB_DIR%\dbfdbt.lib
   if exist %TARGET_LIB_DIR%\dbfdbtmt.lib         del %TARGET_LIB_DIR%\dbfdbtmt.lib
   if exist %TARGET_LIB_DIR%\dbffpt.lib           del %TARGET_LIB_DIR%\dbffpt.lib
   if exist %TARGET_LIB_DIR%\dbffptmt.lib         del %TARGET_LIB_DIR%\dbffptmt.lib
   if exist %TARGET_LIB_DIR%\dbfntx.lib           del %TARGET_LIB_DIR%\dbfntx.lib
   if exist %TARGET_LIB_DIR%\dbfntxmt.lib         del %TARGET_LIB_DIR%\dbfntxmt.lib
   if exist %TARGET_LIB_DIR%\debug.lib            del %TARGET_LIB_DIR%\debug.lib
   if exist %TARGET_LIB_DIR%\dllmain.lib          del %TARGET_LIB_DIR%\dllmain.lib
   if exist %TARGET_LIB_DIR%\harbour.lib          del %TARGET_LIB_DIR%\harbour.lib
   if exist %TARGET_LIB_DIR%\hbodbc.lib           del %TARGET_LIB_DIR%\hbodbc.lib
   if exist %TARGET_LIB_DIR%\hbodbcmt.lib         del %TARGET_LIB_DIR%\hbodbcmt.lib
   if exist %TARGET_LIB_DIR%\hbsix.lib            del %TARGET_LIB_DIR%\hbsix.lib
   if exist %TARGET_LIB_DIR%\hbsixmt.lib          del %TARGET_LIB_DIR%\hbsixmt.lib
   if exist %TARGET_LIB_DIR%\hsx.lib              del %TARGET_LIB_DIR%\hsx.lib
   if exist %TARGET_LIB_DIR%\hsxmt.lib            del %TARGET_LIB_DIR%\hsxmt.lib
   if exist %TARGET_LIB_DIR%\gtnul.lib            del %TARGET_LIB_DIR%\gtnul.lib
   if exist %TARGET_LIB_DIR%\gtstd.lib            del %TARGET_LIB_DIR%\gtstd.lib
   if exist %TARGET_LIB_DIR%\gtwin.lib            del %TARGET_LIB_DIR%\gtwin.lib
   if exist %TARGET_LIB_DIR%\gtcgi.lib            del %TARGET_LIB_DIR%\gtcgi.lib
   if exist %TARGET_LIB_DIR%\gtpca.lib            del %TARGET_LIB_DIR%\gtpca.lib
   if exist %TARGET_LIB_DIR%\gtwvt.lib            del %TARGET_LIB_DIR%\gtwvt.lib
   if exist %TARGET_LIB_DIR%\lang.lib             del %TARGET_LIB_DIR%\lang.lib
   if exist %TARGET_LIB_DIR%\macro.lib            del %TARGET_LIB_DIR%\macro.lib
   if exist %TARGET_LIB_DIR%\macromt.lib          del %TARGET_LIB_DIR%\macromt.lib
   if exist %TARGET_LIB_DIR%\nulsys.lib           del %TARGET_LIB_DIR%\nulsys.lib
   if exist %TARGET_LIB_DIR%\optcon.lib           del %TARGET_LIB_DIR%\optcon.lib
   if exist %TARGET_LIB_DIR%\optconmt.lib         del %TARGET_LIB_DIR%\optconmt.lib
   if exist %TARGET_LIB_DIR%\optgui.lib           del %TARGET_LIB_DIR%\optgui.lib
   if exist %TARGET_LIB_DIR%\optguimt.lib         del %TARGET_LIB_DIR%\optguimt.lib
   if exist %TARGET_LIB_DIR%\pcrepos.lib          del %TARGET_LIB_DIR%\pcrepos.lib
   if exist %TARGET_LIB_DIR%\pcreposmt.lib        del %TARGET_LIB_DIR%\pcreposmt.lib
   if exist %TARGET_LIB_DIR%\pp.lib               del %TARGET_LIB_DIR%\pp.lib
   if exist %TARGET_LIB_DIR%\ppmt.lib             del %TARGET_LIB_DIR%\ppmt.lib
   if exist %TARGET_LIB_DIR%\rdd.lib              del %TARGET_LIB_DIR%\rdd.lib
   if exist %TARGET_LIB_DIR%\rddmt.lib            del %TARGET_LIB_DIR%\rddmt.lib
   if exist %TARGET_LIB_DIR%\rtl.lib              del %TARGET_LIB_DIR%\rtl.lib
   if exist %TARGET_LIB_DIR%\rtlmt.lib            del %TARGET_LIB_DIR%\rtlmt.lib
   if exist %TARGET_LIB_DIR%\tip.lib              del %TARGET_LIB_DIR%\tip.lib
   if exist %TARGET_LIB_DIR%\tipmt.lib            del %TARGET_LIB_DIR%\tipmt.lib
   if exist %TARGET_LIB_DIR%\vm.lib               del %TARGET_LIB_DIR%\vm.lib
   if exist %TARGET_LIB_DIR%\vmmt.lib             del %TARGET_LIB_DIR%\vmmt.lib

   if exist %TARGET_OBJ_DIR%\*.c                  del %TARGET_OBJ_DIR%\*.c
   if exist %TARGET_OBJ_DIR%\*.h                  del %TARGET_OBJ_DIR%\*.h
   if exist %TARGET_OBJ_DIR%\*.obj                del %TARGET_OBJ_DIR%\*.obj
   if exist %TARGET_OBJ_DIR%\*.output             del %TARGET_OBJ_DIR%\*.output

   if exist %TARGET_OBJ_DIR%\ct\*.c               del %TARGET_OBJ_DIR%\ct\*.c
   if exist %TARGET_OBJ_DIR%\ct\*.obj             del %TARGET_OBJ_DIR%\ct\*.obj

   if exist %TARGET_OBJ_DIR%\opt\*.c              del %TARGET_OBJ_DIR%\opt\*.c
   if exist %TARGET_OBJ_DIR%\opt\*.obj            del %TARGET_OBJ_DIR%\opt\*.obj

   if exist %TARGET_OBJ_DIR%\opt\console\*.c      del %TARGET_OBJ_DIR%\opt\console\*.c
   if exist %TARGET_OBJ_DIR%\opt\console\*.obj    del %TARGET_OBJ_DIR%\opt\console\*.obj

   if exist %TARGET_OBJ_DIR%\opt\gui\*.c          del %TARGET_OBJ_DIR%\opt\gui\*.c
   if exist %TARGET_OBJ_DIR%\opt\gui\*.obj        del %TARGET_OBJ_DIR%\opt\gui\*.obj

   if exist %TARGET_OBJ_DIR%\mt\*.c               del %TARGET_OBJ_DIR%\mt\*.c
   if exist %TARGET_OBJ_DIR%\mt\*.h               del %TARGET_OBJ_DIR%\mt\*.h
   if exist %TARGET_OBJ_DIR%\mt\*.obj             del %TARGET_OBJ_DIR%\mt\*.obj

   if exist %TARGET_OBJ_DIR%\mt\opt\gui\*.c       del %TARGET_OBJ_DIR%\mt\opt\gui\*.c
   if exist %TARGET_OBJ_DIR%\mt\opt\gui\*.obj     del %TARGET_OBJ_DIR%\mt\opt\gui\*.obj

   if exist %TARGET_OBJ_DIR%\mt\opt\console\*.c   del %TARGET_OBJ_DIR%\mt\opt\console\*.c
   if exist %TARGET_OBJ_DIR%\mt\opt\console\*.obj del %TARGET_OBJ_DIR%\mt\opt\console\*.obj

   if exist %TARGET_OBJ_DIR%\mt\opt\gui\*.c       del %TARGET_OBJ_DIR%\mt\opt\gui\*.c
   if exist %TARGET_OBJ_DIR%\mt\opt\gui\*.obj     del %TARGET_OBJ_DIR%\mt\opt\gui\*.obj

   if exist %TARGET_OBJ_DIR%\mt\ct\*.c            del %TARGET_OBJ_DIR%\mt\ct\*.c
   if exist %TARGET_OBJ_DIR%\mt\ct\*.obj          del %TARGET_OBJ_DIR%\mt\ct\*.obj

   goto EXIT


:EXIT

SET POCC_DIR=
SET HB_DIR=
SET BISON_DIR=
SET TARGET_OBJ_DIR=
SET TARGET_LIB_DIR=
SET TARGET_BIN_DIR=
SET HB_MT=

SET PATH=%_PATH%
SET LIB=%_LIB%
SET INCLUDE=%_INCLUDE%

SET _PATH=
SET _LIB=
SET _INCLUDE=

