@ECHO OFF
rem $Id: make_pc.bat,v 1.6 2005/11/16 00:00:00 modalsist Exp $
rem
rem Batch file Pelles C compiler.
rem
rem NOTE: Pelles C is a free C/C++ compliler for MS-Windows platform.
rem       http://www.smorgasbordet.com/pellesc/index.htm
rem
rem 

REM ******************************************
REM *** If necessary, change only 3 sets below
REM ******************************************

SET HB_DIR=.\
SET POCC_DIR=c:\pellesc
SET BISON_DIR=c:\bison


SET _PATH=%PATH%
SET _LIB=%LIB%
SET _INCLUDE=%INCLUDE%

SET LIB=%POCC_DIR\lib;%POCC_DIR\lib\win;%HB_DIR\lib;%LIB%
SET INCLUDE=%POCC_DIR\include;%POCC_DIR\include\win;%HB_DIR%\include;%INCLUDE%

SET PATH=%POCC_DIR%\bin;%BISON_DIR%\bin;%HB_DIR%\bin;%PATH%

SET OBJ_DIR=%HB_DIR%\obj\pocc
SET LIB_DIR=%HB_DIR%\lib\pocc
SET BIN_DIR=%HB_DIR%\bin\pocc

SET BISON_SIMPLE=%BISON_DIR%\share\bison\bison.simple


if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN


  if not exist obj                       md obj
  if not exist %LIB_DIR%                 md %LIB_DIR%
  if not exist %BIN_DIR%                 md %BIN_DIR%
  if not exist %OBJ_DIR%                 md %OBJ_DIR%

  if not exist %OBJ_DIR%\ct              md %OBJ_DIR%\ct
  if not exist %OBJ_DIR%\opt             md %OBJ_DIR%\opt
  if not exist %OBJ_DIR%\opt\console     md %OBJ_DIR%\opt\console
  if not exist %OBJ_DIR%\opt\gui         md %OBJ_DIR%\opt\gui

  if not exist %OBJ_DIR%\mt              md %OBJ_DIR%\mt
  if not exist %OBJ_DIR%\mt\ct           md %OBJ_DIR%\mt\ct
  if not exist %OBJ_DIR%\mt\opt          md %OBJ_DIR%\mt\opt
  if not exist %OBJ_DIR%\mt\opt\console  md %OBJ_DIR%\mt\opt\console
  if not exist %OBJ_DIR%\mt\opt\gui      md %OBJ_DIR%\mt\opt\gui


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

   if exist %HB_DIR%\bin\harbour.exe              del %HB_DIR%\bin\harbour.exe
   if exist %HB_DIR%\bin\hbdoc.exe                del %HB_DIR%\bin\hbdoc.exe
   if exist %HB_DIR%\bin\hbmake.exe               del %HB_DIR%\bin\hbmake.exe
   if exist %HB_DIR%\bin\hbtest.exe               del %HB_DIR%\bin\hbtest.exe
   if exist %HB_DIR%\bin\hbtestmt.exe             del %HB_DIR%\bin\hbtestmt.exe
   if exist %HB_DIR%\bin\hbrun.exe                del %HB_DIR%\bin\hbrun.exe
   if exist %HB_DIR%\bin\hbrunmt.exe              del %HB_DIR%\bin\hbrunmt.exe
   if exist %HB_DIR%\bin\hbpp.exe                 del %HB_DIR%\bin\hbpp.exe
   if exist %HB_DIR%\bin\xbscript.exe             del %HB_DIR%\bin\xbscript.exe

   if exist %HB_DIR%\lib\codepage.lib         del %HB_DIR%\lib\codepage.lib
   if exist %HB_DIR%\lib\common.lib           del %HB_DIR%\lib\common.lib
   if exist %HB_DIR%\lib\ct.lib               del %HB_DIR%\lib\ct.lib
   if exist %HB_DIR%\lib\ctmt.lib             del %HB_DIR%\lib\ctmt.lib
   if exist %HB_DIR%\lib\dbfcdx.lib           del %HB_DIR%\lib\dbfcdx.lib
   if exist %HB_DIR%\lib\dbfcdxmt.lib         del %HB_DIR%\lib\dbfcdxmt.lib
   if exist %HB_DIR%\lib\dbfdbt.lib           del %HB_DIR%\lib\dbfdbt.lib
   if exist %HB_DIR%\lib\dbfdbtmt.lib         del %HB_DIR%\lib\dbfdbtmt.lib
   if exist %HB_DIR%\lib\dbffpt.lib           del %HB_DIR%\lib\dbffpt.lib
   if exist %HB_DIR%\lib\dbffptmt.lib         del %HB_DIR%\lib\dbffptmt.lib
   if exist %HB_DIR%\lib\dbfntx.lib           del %HB_DIR%\lib\dbfntx.lib
   if exist %HB_DIR%\lib\dbfntxmt.lib         del %HB_DIR%\lib\dbfntxmt.lib
   if exist %HB_DIR%\lib\debug.lib            del %HB_DIR%\lib\debug.lib
   if exist %HB_DIR%\lib\dllmain.lib          del %HB_DIR%\lib\dllmain.lib
   if exist %HB_DIR%\lib\harbour.lib          del %HB_DIR%\lib\harbour.lib
   if exist %HB_DIR%\lib\hbodbc.lib           del %HB_DIR%\lib\hbodbc.lib
   if exist %HB_DIR%\lib\hbodbcmt.lib         del %HB_DIR%\lib\hbodbcmt.lib
   if exist %HB_DIR%\lib\hbsix.lib            del %HB_DIR%\lib\hbsix.lib
   if exist %HB_DIR%\lib\hbsixmt.lib          del %HB_DIR%\lib\hbsixmt.lib
   if exist %HB_DIR%\lib\hsx.lib              del %HB_DIR%\lib\hsx.lib
   if exist %HB_DIR%\lib\hsxmt.lib            del %HB_DIR%\lib\hsxmt.lib
   if exist %HB_DIR%\lib\gtnul.lib            del %HB_DIR%\lib\gtnul.lib
   if exist %HB_DIR%\lib\gtstd.lib            del %HB_DIR%\lib\gtstd.lib
   if exist %HB_DIR%\lib\gtwin.lib            del %HB_DIR%\lib\gtwin.lib
   if exist %HB_DIR%\lib\gtcgi.lib            del %HB_DIR%\lib\gtcgi.lib
   if exist %HB_DIR%\lib\gtpca.lib            del %HB_DIR%\lib\gtpca.lib
   if exist %HB_DIR%\lib\gtwvt.lib            del %HB_DIR%\lib\gtwvt.lib
   if exist %HB_DIR%\lib\lang.lib             del %HB_DIR%\lib\lang.lib
   if exist %HB_DIR%\lib\macro.lib            del %HB_DIR%\lib\macro.lib
   if exist %HB_DIR%\lib\macromt.lib          del %HB_DIR%\lib\macromt.lib
   if exist %HB_DIR%\lib\nulsys.lib           del %HB_DIR%\lib\nulsys.lib
   if exist %HB_DIR%\lib\optcon.lib           del %HB_DIR%\lib\optcon.lib
   if exist %HB_DIR%\lib\optconmt.lib         del %HB_DIR%\lib\optconmt.lib
   if exist %HB_DIR%\lib\optgui.lib           del %HB_DIR%\lib\optgui.lib
   if exist %HB_DIR%\lib\optguimt.lib         del %HB_DIR%\lib\optguimt.lib
   if exist %HB_DIR%\lib\pcrepos.lib          del %HB_DIR%\lib\pcrepos.lib
   if exist %HB_DIR%\lib\pcreposmt.lib        del %HB_DIR%\lib\pcreposmt.lib
   if exist %HB_DIR%\lib\pp.lib               del %HB_DIR%\lib\pp.lib
   if exist %HB_DIR%\lib\ppmt.lib             del %HB_DIR%\lib\ppmt.lib
   if exist %HB_DIR%\lib\rdd.lib              del %HB_DIR%\lib\rdd.lib
   if exist %HB_DIR%\lib\rddmt.lib            del %HB_DIR%\lib\rddmt.lib
   if exist %HB_DIR%\lib\rtl.lib              del %HB_DIR%\lib\rtl.lib
   if exist %HB_DIR%\lib\rtlmt.lib            del %HB_DIR%\lib\rtlmt.lib
   if exist %HB_DIR%\lib\tip.lib              del %HB_DIR%\lib\tip.lib
   if exist %HB_DIR%\lib\tipmt.lib            del %HB_DIR%\lib\tipmt.lib
   if exist %HB_DIR%\lib\vm.lib               del %HB_DIR%\lib\vm.lib
   if exist %HB_DIR%\lib\vmmt.lib             del %HB_DIR%\lib\vmmt.lib

   REM Cleaning temp bin folder

   if exist %BIN_DIR%\harbour.exe          del %BIN_DIR%\harbour.exe
   if exist %BIN_DIR%\harbour.exp          del %BIN_DIR%\harbour.exp

   if exist %BIN_DIR%\xharbour.exp         del %BIN_DIR%\xharbour.exp
   if exist %BIN_DIR%\xharbour.exe         del %BIN_DIR%\xharbour.exe
   if exist %BIN_DIR%\xharbour.dll         del %BIN_DIR%\xharbour.dll

   if exist %BIN_DIR%\hbdoc.exe            del %BIN_DIR%\hbdoc.exe
   if exist %BIN_DIR%\hbdocdll.exe         del %BIN_DIR%\hbdocdll.exe

   if exist %BIN_DIR%\hbmake.exe           del %BIN_DIR%\hbmake.exe
   if exist %BIN_DIR%\hbmakedll.exe        del %BIN_DIR%\hbmakedll.exe

   if exist %BIN_DIR%\hbpp.exe             del %BIN_DIR%\hbpp.exe

   if exist %BIN_DIR%\hbrun.exe            del %BIN_DIR%\hbrun.exe
   if exist %BIN_DIR%\hbrundll.exe         del %BIN_DIR%\hbrundll.exe
   if exist %BIN_DIR%\hbrunmt.exe          del %BIN_DIR%\hbrunmt.exe

   if exist %BIN_DIR%\hbtest.exe           del %BIN_DIR%\hbtest.exe
   if exist %BIN_DIR%\hbtestdll.exe        del %BIN_DIR%\hbtestdll.exe
   if exist %BIN_DIR%\hbtestmt.exe         del %BIN_DIR%\hbtestmt.exe

   if exist %BIN_DIR%\xbscript.exe         del %BIN_DIR%\xbscript.exe
   if exist %BIN_DIR%\xbscriptdll.exe      del %BIN_DIR%\xbscriptdll.exe

   REM Cleaning temp lib folder

   if exist %LIB_DIR%\codepage.lib         del %LIB_DIR%\codepage.lib
   if exist %LIB_DIR%\common.lib           del %LIB_DIR%\common.lib
   if exist %LIB_DIR%\ct.lib               del %LIB_DIR%\ct.lib
   if exist %LIB_DIR%\ctmt.lib             del %LIB_DIR%\ctmt.lib
   if exist %LIB_DIR%\dbfcdx.lib           del %LIB_DIR%\dbfcdx.lib
   if exist %LIB_DIR%\dbfcdxmt.lib         del %LIB_DIR%\dbfcdxmt.lib
   if exist %LIB_DIR%\dbfdbt.lib           del %LIB_DIR%\dbfdbt.lib
   if exist %LIB_DIR%\dbfdbtmt.lib         del %LIB_DIR%\dbfdbtmt.lib
   if exist %LIB_DIR%\dbffpt.lib           del %LIB_DIR%\dbffpt.lib
   if exist %LIB_DIR%\dbffptmt.lib         del %LIB_DIR%\dbffptmt.lib
   if exist %LIB_DIR%\dbfntx.lib           del %LIB_DIR%\dbfntx.lib
   if exist %LIB_DIR%\dbfntxmt.lib         del %LIB_DIR%\dbfntxmt.lib
   if exist %LIB_DIR%\debug.lib            del %LIB_DIR%\debug.lib
   if exist %LIB_DIR%\dllmain.lib          del %LIB_DIR%\dllmain.lib
   if exist %LIB_DIR%\harbour.lib          del %LIB_DIR%\harbour.lib
   if exist %LIB_DIR%\hbodbc.lib           del %LIB_DIR%\hbodbc.lib
   if exist %LIB_DIR%\hbodbcmt.lib         del %LIB_DIR%\hbodbcmt.lib
   if exist %LIB_DIR%\hbsix.lib            del %LIB_DIR%\hbsix.lib
   if exist %LIB_DIR%\hbsixmt.lib          del %LIB_DIR%\hbsixmt.lib
   if exist %LIB_DIR%\hsx.lib              del %LIB_DIR%\hsx.lib
   if exist %LIB_DIR%\hsxmt.lib            del %LIB_DIR%\hsxmt.lib
   if exist %LIB_DIR%\gtnul.lib            del %LIB_DIR%\gtnul.lib
   if exist %LIB_DIR%\gtstd.lib            del %LIB_DIR%\gtstd.lib
   if exist %LIB_DIR%\gtwin.lib            del %LIB_DIR%\gtwin.lib
   if exist %LIB_DIR%\gtcgi.lib            del %LIB_DIR%\gtcgi.lib
   if exist %LIB_DIR%\gtpca.lib            del %LIB_DIR%\gtpca.lib
   if exist %LIB_DIR%\gtwvt.lib            del %LIB_DIR%\gtwvt.lib
   if exist %LIB_DIR%\lang.lib             del %LIB_DIR%\lang.lib
   if exist %LIB_DIR%\macro.lib            del %LIB_DIR%\macro.lib
   if exist %LIB_DIR%\macromt.lib          del %LIB_DIR%\macromt.lib
   if exist %LIB_DIR%\nulsys.lib           del %LIB_DIR%\nulsys.lib
   if exist %LIB_DIR%\optcon.lib           del %LIB_DIR%\optcon.lib
   if exist %LIB_DIR%\optconmt.lib         del %LIB_DIR%\optconmt.lib
   if exist %LIB_DIR%\optgui.lib           del %LIB_DIR%\optgui.lib
   if exist %LIB_DIR%\optguimt.lib         del %LIB_DIR%\optguimt.lib
   if exist %LIB_DIR%\pcrepos.lib          del %LIB_DIR%\pcrepos.lib
   if exist %LIB_DIR%\pcreposmt.lib        del %LIB_DIR%\pcreposmt.lib
   if exist %LIB_DIR%\pp.lib               del %LIB_DIR%\pp.lib
   if exist %LIB_DIR%\ppmt.lib             del %LIB_DIR%\ppmt.lib
   if exist %LIB_DIR%\rdd.lib              del %LIB_DIR%\rdd.lib
   if exist %LIB_DIR%\rddmt.lib            del %LIB_DIR%\rddmt.lib
   if exist %LIB_DIR%\rtl.lib              del %LIB_DIR%\rtl.lib
   if exist %LIB_DIR%\rtlmt.lib            del %LIB_DIR%\rtlmt.lib
   if exist %LIB_DIR%\tip.lib              del %LIB_DIR%\tip.lib
   if exist %LIB_DIR%\tipmt.lib            del %LIB_DIR%\tipmt.lib
   if exist %LIB_DIR%\vm.lib               del %LIB_DIR%\vm.lib
   if exist %LIB_DIR%\vmmt.lib             del %LIB_DIR%\vmmt.lib

   REM Cleaning temp obj folder

   if exist %OBJ_DIR%\*.c                  del %OBJ_DIR%\*.c
   if exist %OBJ_DIR%\*.h                  del %OBJ_DIR%\*.h
   if exist %OBJ_DIR%\*.obj                del %OBJ_DIR%\*.obj
   if exist %OBJ_DIR%\*.output             del %OBJ_DIR%\*.output

   if exist %OBJ_DIR%\ct\*.c               del %OBJ_DIR%\ct\*.c
   if exist %OBJ_DIR%\ct\*.obj             del %OBJ_DIR%\ct\*.obj

   if exist %OBJ_DIR%\opt\*.c              del %OBJ_DIR%\opt\*.c
   if exist %OBJ_DIR%\opt\*.obj            del %OBJ_DIR%\opt\*.obj

   if exist %OBJ_DIR%\opt\console\*.c      del %OBJ_DIR%\opt\console\*.c
   if exist %OBJ_DIR%\opt\console\*.obj    del %OBJ_DIR%\opt\console\*.obj

   if exist %OBJ_DIR%\opt\gui\*.c          del %OBJ_DIR%\opt\gui\*.c
   if exist %OBJ_DIR%\opt\gui\*.obj        del %OBJ_DIR%\opt\gui\*.obj

   if exist %OBJ_DIR%\mt\*.c               del %OBJ_DIR%\mt\*.c
   if exist %OBJ_DIR%\mt\*.h               del %OBJ_DIR%\mt\*.h
   if exist %OBJ_DIR%\mt\*.obj             del %OBJ_DIR%\mt\*.obj

   if exist %OBJ_DIR%\mt\opt\gui\*.c       del %OBJ_DIR%\mt\opt\gui\*.c
   if exist %OBJ_DIR%\mt\opt\gui\*.obj     del %OBJ_DIR%\mt\opt\gui\*.obj

   if exist %OBJ_DIR%\mt\opt\console\*.c   del %OBJ_DIR%\mt\opt\console\*.c
   if exist %OBJ_DIR%\mt\opt\console\*.obj del %OBJ_DIR%\mt\opt\console\*.obj

   if exist %OBJ_DIR%\mt\opt\gui\*.c       del %OBJ_DIR%\mt\opt\gui\*.c
   if exist %OBJ_DIR%\mt\opt\gui\*.obj     del %OBJ_DIR%\mt\opt\gui\*.obj

   if exist %OBJ_DIR%\mt\ct\*.c            del %OBJ_DIR%\mt\ct\*.c
   if exist %OBJ_DIR%\mt\ct\*.obj          del %OBJ_DIR%\mt\ct\*.obj

   goto EXIT


:EXIT

SET POCC_DIR=
SET HB_DIR=
SET BISON_DIR=
SET BISON_SIMPLE=

SET OBJ_DIR=
SET LIB_DIR=
SET BIN_DIR=
SET HB_MT=

SET PATH=%_PATH%
SET LIB=%_LIB%
SET INCLUDE=%_INCLUDE%

SET _PATH=
SET _LIB=
SET _INCLUDE=

