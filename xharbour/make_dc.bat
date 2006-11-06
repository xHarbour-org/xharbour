@ECHO OFF
rem
rem $Id: make_dc.bat,v 1.2 2005/03/31 19:51:25 andijahja Exp $
rem
rem Batch File For Building xHarbour with DigitalMars C/C++ Compiler
rem

rem ONLY Modify the following These envars please
@if not exist xdmcdll.def goto CREATE_DEF
GOTO BEGIN_CFG

:CREATE_DEF

ECHO LIBRARY "xharbour.dll" > xdmcdll.def
ECHO EXETYPE NT >> xdmcdll.def
ECHO SUBSYSTEM CONSOLE >> xdmcdll.def
ECHO CODE SHARED EXECUTE >> xdmcdll.def
ECHO DATA WRITE >> xdmcdll.def

:BEGIN_CFG
SET DMCMAIN=F:\DM
SET XHARBOUR_ROOT=C:\XHARBOUR
SET MAKEEXE=F:\DM\BIN\MAKE.EXE
SET BISONPATH=F:\bison\bin

REM Let the next ENVARS be like that
SET OBJ_DIR=obj\dmc
SET LIB_DIR=lib\dmc
SET BIN_DIR=bin\dmc
SET _PATH=%PATH%
SET PATH=%BISONPATH%;%_PATH%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist obj                       md obj
if not exist %LIB_DIR%                 md %LIB_DIR%
if not exist %BIN_DIR%                 md %BIN_DIR%
if not exist %OBJ_DIR%                 md %OBJ_DIR%
if not exist %OBJ_DIR%\libmisc         md %OBJ_DIR%\libmisc
if not exist %OBJ_DIR%\rddads          md %OBJ_DIR%\rddads
if not exist %OBJ_DIR%\unicode         md %OBJ_DIR%\unicode
if not exist %OBJ_DIR%\nanfor          md %OBJ_DIR%\nanfor
if not exist %OBJ_DIR%\design          md %OBJ_DIR%\design
if not exist %OBJ_DIR%\html            md %OBJ_DIR%\html
if not exist %OBJ_DIR%\ct              md %OBJ_DIR%\ct
if not exist %OBJ_DIR%\mt              md %OBJ_DIR%\mt
if not exist %OBJ_DIR%\opt             md %OBJ_DIR%\opt
if not exist %OBJ_DIR%\bin             md %OBJ_DIR%\bin
if not exist %OBJ_DIR%\opt\console     md %OBJ_DIR%\opt\console
if not exist %OBJ_DIR%\opt\gui         md %OBJ_DIR%\opt\gui
if not exist %OBJ_DIR%\mt\opt          md %OBJ_DIR%\mt\opt
if not exist %OBJ_DIR%\mt\opt\gui      md %OBJ_DIR%\mt\opt\gui
if not exist %OBJ_DIR%\mt\opt\console  md %OBJ_DIR%\mt\opt\console
if not exist %OBJ_DIR%\mt\opt\gui      md %OBJ_DIR%\mt\opt\gui
if not exist %OBJ_DIR%\mt\rddads       md %OBJ_DIR%\mt\rddads
if not exist %OBJ_DIR%\mt\unicode      md %OBJ_DIR%\mt\unicode
if not exist %OBJ_DIR%\mt\nanfor       md %OBJ_DIR%\mt\nanfor
if not exist %OBJ_DIR%\mt\libmisc      md %OBJ_DIR%\mt\libmisc
if not exist %OBJ_DIR%\mt\ct           md %OBJ_DIR%\mt\ct

   REM
   REM The Free Version of DMC make.exe does not support IF, ELSE etc
   REM in makefiles. So the following set ups may look cumbersome
   REM However, DigitalMars sells their tools, among others, SMAKE, which
   REM support IF, ENDIF in makefiles.
   REM

   SET THREAD_MODE=
   SET MTFLAGS=
   SET VM_OBJ_DIR=%OBJ_DIR%
   SET RTL_OBJ_DIR=%OBJ_DIR%
   SET EXE_OBJ_DIR=%OBJ_DIR%\bin
   SET MACRO_OBJ_DIR=%OBJ_DIR%
   SET RDD_OBJ_DIR=%OBJ_DIR%
   SET DBT_OBJ_DIR=%OBJ_DIR%
   SET FPT_OBJ_DIR=%OBJ_DIR%
   SET NTX_OBJ_DIR=%OBJ_DIR%
   SET CDX_OBJ_DIR=%OBJ_DIR%
   SET CT_OBJ_DIR=%OBJ_DIR%\ct
   SET TIP_OBJ_DIR=%OBJ_DIR%
   SET PP_OBJ_DIR=%OBJ_DIR%
   SET SAM_OBJ_DIR=%OBJ_DIR%
   SET GTWVW_OBJ_DIR=%OBJ_DIR%
   SET GTWVT_OBJ_DIR=%OBJ_DIR%
   SET OPT_CON_OBJ_DIR=%OBJ_DIR%/opt/console
   SET OPT_GUI_OBJ_DIR=%OBJ_DIR%/opt/gui
   SET RDDADS_OBJ_DIR=%OBJ_DIR%/rddads
   SET UNICODE_OBJ_DIR=%OBJ_DIR%/unicode
   SET NF_OBJ_DIR=%OBJ_DIR%/nanfor
   SET MISC_OBJ_DIR=%OBJ_DIR%/libmisc
   %MAKEEXE% -fmakefile.dc
   if errorlevel 1 goto BUILD_ERR

   SET THREAD_MODE=mt
   SET MTFLAGS=-D"HB_THREAD_SUPPORT"
   SET VM_OBJ_DIR=%OBJ_DIR%\mt
   SET RTL_OBJ_DIR=%OBJ_DIR%\mt
   SET MACRO_OBJ_DIR=%OBJ_DIR%\mt
   SET RDD_OBJ_DIR=%OBJ_DIR%\mt
   SET DBT_OBJ_DIR=%OBJ_DIR%\mt
   SET FPT_OBJ_DIR=%OBJ_DIR%\mt
   SET NTX_OBJ_DIR=%OBJ_DIR%\mt
   SET CDX_OBJ_DIR=%OBJ_DIR%\mt
   SET CT_OBJ_DIR=%OBJ_DIR%\mt\ct
   SET TIP_OBJ_DIR=%OBJ_DIR%\mt
   SET PP_OBJ_DIR=%OBJ_DIR%\mt
   SET SAM_OBJ_DIR=%OBJ_DIR%\mt
   SET GTWVW_OBJ_DIR=%OBJ_DIR%\mt
   SET GTWVT_OBJ_DIR=%OBJ_DIR%\mt
   SET OPT_CON_OBJ_DIR=%OBJ_DIR%\mt\opt\console
   SET OPT_GUI_OBJ_DIR=%OBJ_DIR%\mt\opt\gui
   SET RDDADS_OBJ_DIR=%OBJ_DIR%\mt\rddads
   SET UNICODE_OBJ_DIR=%OBJ_DIR%\mt\unicode
   SET NF_OBJ_DIR=%OBJ_DIR%\mt\nanfor
   SET MISC_OBJ_DIR=%OBJ_DIR%\mt\libmisc
   %MAKEEXE% -fmakefile.dc
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK

   goto EXIT

:BUILD_ERR

   goto EXIT

:CLEAN

   if exist %BIN_DIR%\xharbour.exe         del %BIN_DIR%\xharbour.exe
   if exist %BIN_DIR%\xharbour.dll         del %BIN_DIR%\xharbour.dll
   if exist %BIN_DIR%\harbour.exe          del %BIN_DIR%\harbour.exe
   if exist %BIN_DIR%\hbdoc.exe            del %BIN_DIR%\hbdoc.exe
   if exist %BIN_DIR%\hbmake.exe           del %BIN_DIR%\hbmake.exe
   if exist %BIN_DIR%\hbpp.exe             del %BIN_DIR%\hbpp.exe
   if exist %BIN_DIR%\hbrun.exe            del %BIN_DIR%\hbrun.exe
   if exist %BIN_DIR%\hbrunmt.exe          del %BIN_DIR%\hbrunmt.exe
   if exist %BIN_DIR%\hbtest.exe           del %BIN_DIR%\hbtest.exe
   if exist %BIN_DIR%\xbscript.exe         del %BIN_DIR%\xbscript.exe

   if exist %LIB_DIR%\common.lib           del %LIB_DIR%\common.lib
   if exist %LIB_DIR%\xharbour.lib         del %LIB_DIR%\xharbour.lib
   if exist %LIB_DIR%\xharbour.def         del %LIB_DIR%\xharbour.def
   if exist %LIB_DIR%\codepage.lib         del %LIB_DIR%\codepage.lib
   if exist %LIB_DIR%\dbfcdx.lib           del %LIB_DIR%\dbfcdx.lib
   if exist %LIB_DIR%\dbfcdxmt.lib         del %LIB_DIR%\dbfcdxmt.lib
   if exist %LIB_DIR%\bmdbfcdx.lib         del %LIB_DIR%\bmdbfcdx.lib
   if exist %LIB_DIR%\bmdbfcdxmt.lib       del %LIB_DIR%\bmdbfcdxmt.lib
   if exist %LIB_DIR%\dbfdbt.lib           del %LIB_DIR%\dbfdbt.lib
   if exist %LIB_DIR%\dbfdbtmt.lib         del %LIB_DIR%\dbfdbtmt.lib
   if exist %LIB_DIR%\dbffpt.lib           del %LIB_DIR%\dbffpt.lib
   if exist %LIB_DIR%\dbffptmt.lib         del %LIB_DIR%\dbffptmt.lib
   if exist %LIB_DIR%\dbfntx.lib           del %LIB_DIR%\dbfntx.lib
   if exist %LIB_DIR%\dbfntxmt.lib         del %LIB_DIR%\dbfntxmt.lib
   if exist %LIB_DIR%\debug.lib            del %LIB_DIR%\debug.lib
   if exist %LIB_DIR%\design.lib           del %LIB_DIR%\design.lib
   if exist %LIB_DIR%\gtnul.lib            del %LIB_DIR%\gtnul.lib
   if exist %LIB_DIR%\gtstd.lib            del %LIB_DIR%\gtstd.lib
   if exist %LIB_DIR%\gtwin.lib            del %LIB_DIR%\gtwin.lib
   if exist %LIB_DIR%\gtwvt.lib            del %LIB_DIR%\gtwvt.lib
   if exist %LIB_DIR%\gtwvtmt.lib          del %LIB_DIR%\gtwvtmt.lib
   if exist %LIB_DIR%\gtwvw.lib            del %LIB_DIR%\gtwvw.lib
   if exist %LIB_DIR%\gtcgi.lib            del %LIB_DIR%\gtcgi.lib
   if exist %LIB_DIR%\gtpca.lib            del %LIB_DIR%\gtpca.lib
   if exist %LIB_DIR%\gtwvwmt.lib          del %LIB_DIR%\gtwvwmt.lib
   if exist %LIB_DIR%\html.lib             del %LIB_DIR%\html.lib
   if exist %LIB_DIR%\lang.lib             del %LIB_DIR%\lang.lib
   if exist %LIB_DIR%\macro.lib            del %LIB_DIR%\macro.lib
   if exist %LIB_DIR%\macromt.lib          del %LIB_DIR%\macromt.lib
   if exist %LIB_DIR%\nf.lib               del %LIB_DIR%\nf.lib
   if exist %LIB_DIR%\nfmt.lib             del %LIB_DIR%\nfmt.lib
   if exist %LIB_DIR%\ct.lib               del %LIB_DIR%\ct.lib
   if exist %LIB_DIR%\ctmt.lib             del %LIB_DIR%\ctmt.lib
   if exist %LIB_DIR%\nulsys.lib           del %LIB_DIR%\nulsys.lib
   if exist %LIB_DIR%\optcon.lib           del %LIB_DIR%\optcon.lib
   if exist %LIB_DIR%\optconmt.lib         del %LIB_DIR%\optconmt.lib
   if exist %LIB_DIR%\optgui.lib           del %LIB_DIR%\optgui.lib
   if exist %LIB_DIR%\optguimt.lib         del %LIB_DIR%\optguimt.lib
   if exist %LIB_DIR%\pp.lib               del %LIB_DIR%\pp.lib
   if exist %LIB_DIR%\ppmt.lib             del %LIB_DIR%\ppmt.lib
   if exist %LIB_DIR%\rdd.lib              del %LIB_DIR%\rdd.lib
   if exist %LIB_DIR%\rddads.lib           del %LIB_DIR%\rddads.lib
   if exist %LIB_DIR%\rddadsmt.lib         del %LIB_DIR%\rddadsmt.lib
   if exist %LIB_DIR%\rddmt.lib            del %LIB_DIR%\rddmt.lib
   if exist %LIB_DIR%\rtl.lib              del %LIB_DIR%\rtl.lib
   if exist %LIB_DIR%\rtlmt.lib            del %LIB_DIR%\rtlmt.lib
   if exist %LIB_DIR%\samples.lib          del %LIB_DIR%\samples.lib
   if exist %LIB_DIR%\samplesmt.lib        del %LIB_DIR%\samplesmt.lib
   if exist %LIB_DIR%\tip.lib              del %LIB_DIR%\tip.lib
   if exist %LIB_DIR%\tipmt.lib            del %LIB_DIR%\tipmt.lib
   if exist %LIB_DIR%\unicode.lib          del %LIB_DIR%\unicode.lib
   if exist %LIB_DIR%\unicodemt.lib        del %LIB_DIR%\unicodemt.lib
   if exist %LIB_DIR%\hbodbc.lib           del %LIB_DIR%\hbodbc.lib
   if exist %LIB_DIR%\hbodbcmt.lib         del %LIB_DIR%\hbodbcmt.lib
   if exist %LIB_DIR%\vm.lib               del %LIB_DIR%\vm.lib
   if exist %LIB_DIR%\vmmt.lib             del %LIB_DIR%\vmmt.lib

   if exist %OBJ_DIR%\*.c                  del %OBJ_DIR%\*.c
   if exist %OBJ_DIR%\*.h                  del %OBJ_DIR%\*.h
   if exist %OBJ_DIR%\*.obj                del %OBJ_DIR%\*.obj
   if exist %OBJ_DIR%\*.output             del %OBJ_DIR%\*.output

   if exist %OBJ_DIR%\bin\*.c              del %OBJ_DIR%\bin\*.c
   if exist %OBJ_DIR%\bin\*.h              del %OBJ_DIR%\bin\*.h
   if exist %OBJ_DIR%\bin\*.obj            del %OBJ_DIR%\bin\*.obj
   if exist %OBJ_DIR%\bin\*.output         del %OBJ_DIR%\bin\*.output

   if exist %OBJ_DIR%\libmisc\*.c          del %OBJ_DIR%\libmisc\*.c
   if exist %OBJ_DIR%\libmisc\*.obj        del %OBJ_DIR%\libmisc\*.obj

   if exist %OBJ_DIR%\rddads\*.c           del %OBJ_DIR%\rddads\*.c
   if exist %OBJ_DIR%\rddads\*.obj         del %OBJ_DIR%\rddads\*.obj

   if exist %OBJ_DIR%\unicode\*.c          del %OBJ_DIR%\unicode\*.c
   if exist %OBJ_DIR%\unicode\*.obj        del %OBJ_DIR%\unicode\*.obj

   if exist %OBJ_DIR%\nanfor\*.c           del %OBJ_DIR%\nanfor\*.c
   if exist %OBJ_DIR%\nanfor\*.obj         del %OBJ_DIR%\nanfor\*.obj

   if exist %OBJ_DIR%\design\*.c           del %OBJ_DIR%\design\*.c
   if exist %OBJ_DIR%\design\*.obj         del %OBJ_DIR%\design\*.obj

   if exist %OBJ_DIR%\html\*.c             del %OBJ_DIR%\html\*.c
   if exist %OBJ_DIR%\html\*.obj           del %OBJ_DIR%\html\*.obj

   if exist %OBJ_DIR%\ct\*.c               del %OBJ_DIR%\ct\*.c
   if exist %OBJ_DIR%\ct\*.obj             del %OBJ_DIR%\ct\*.obj

   if exist %OBJ_DIR%\mt\*.c               del %OBJ_DIR%\mt\*.c
   if exist %OBJ_DIR%\mt\*.h               del %OBJ_DIR%\mt\*.h
   if exist %OBJ_DIR%\mt\*.obj             del %OBJ_DIR%\mt\*.obj

   if exist %OBJ_DIR%\opt\*.c              del %OBJ_DIR%\opt\*.c
   if exist %OBJ_DIR%\opt\*.obj            del %OBJ_DIR%\opt\*.obj

   if exist %OBJ_DIR%\opt\console\*.c      del %OBJ_DIR%\opt\console\*.c
   if exist %OBJ_DIR%\opt\console\*.obj    del %OBJ_DIR%\opt\console\*.obj

   if exist %OBJ_DIR%\opt\gui\*.c          del %OBJ_DIR%\opt\gui\*.c
   if exist %OBJ_DIR%\opt\gui\*.obj        del %OBJ_DIR%\opt\gui\*.obj

   if exist %OBJ_DIR%\mt\opt\gui\*.c       del %OBJ_DIR%\mt\opt\gui\*.c
   if exist %OBJ_DIR%\mt\opt\gui\*.obj     del %OBJ_DIR%\mt\opt\gui\*.obj

   if exist %OBJ_DIR%\mt\opt\console\*.c   del %OBJ_DIR%\mt\opt\console\*.c
   if exist %OBJ_DIR%\mt\opt\console\*.obj del %OBJ_DIR%\mt\opt\console\*.obj

   if exist %OBJ_DIR%\mt\opt\gui\*.c       del %OBJ_DIR%\mt\opt\gui\*.c
   if exist %OBJ_DIR%\mt\opt\gui\*.obj     del %OBJ_DIR%\mt\opt\gui\*.obj

   if exist %OBJ_DIR%\mt\rddads\*.c        del %OBJ_DIR%\mt\rddads\*.c
   if exist %OBJ_DIR%\mt\rddads\*.obj      del %OBJ_DIR%\mt\rddads\*.obj

   if exist %OBJ_DIR%\mt\unicode\*.c       del %OBJ_DIR%\mt\unicode\*.c
   if exist %OBJ_DIR%\mt\unicode\*.obj     del %OBJ_DIR%\mt\unicode\*.obj

   if exist %OBJ_DIR%\mt\ct\*.c            del %OBJ_DIR%\mt\ct\*.c
   if exist %OBJ_DIR%\mt\ct\*.obj          del %OBJ_DIR%\mt\ct\*.obj

   if exist %OBJ_DIR%\mt\nanfor\*.c        del %OBJ_DIR%\mt\nanfor\*.c
   if exist %OBJ_DIR%\mt\nanfor\*.obj      del %OBJ_DIR%\mt\nanfor\*.obj

   if exist %OBJ_DIR%\mt\libmisc\*.c       del %OBJ_DIR%\mt\libmisc\*.c
   if exist %OBJ_DIR%\mt\libmisc\*.obj     del %OBJ_DIR%\mt\libmisc\*.obj

   if exist xdmcdll.def del xdmcdll.def

   goto EXIT

:EXIT

SET PATH=%_PATH%
SET _PATH=
SET DMCMAIN=
SET XHARBOUR_ROOT=
SET OBJ_DIR=
SET LIB_DIR=
SET BIN_DIR=
SET MAKEEXE=
SET BISONPATH=
