@ECHO OFF
rem ============================================================================
rem
rem $Id: mdir.bat,v 1.11 2008/07/14 02:50:56 kaddath Exp $
rem
rem FILE    : mdir.bat
rem PURPOSE : Create Target Directories If Not Exist and Clean Up
rem WARNING : Do Not Call This Batch File Directly.(AJ:2008-04-26)
rem
rem ============================================================================

if "%1"=="CLEAN"       goto REMOVE
if "%1"=="clean"       goto REMOVE
if "%1"=="DLLCREATE"   goto CREATEDLL
if "%1"=="dllcreate"   goto CREATEDLL
if "%1"=="DLLCOPY"     goto COPYDLL
if "%1"=="dllcopy"     goto COPYDLL
if "%1"=="COPYTOBIN"   goto COPYBIN
if "%1"=="copytobin"   goto COPYBIN
if "%1"=="COPYCONTRIB" goto COPYCONTRIBLIBS
if "%1"=="copycontrib" goto COPYCONTRIBLIBS
if "%1"=="resetenvar"  goto RESET_ENVAR
if "%1"=="RESETENVAR"  goto RESET_ENVAR
if "%1"=="howto"       goto _SYNTAX
if "%1"=="HOWTO"       goto _SYNTAX

rem=============================================================================
:CREATE
rem=============================================================================
IF "%__BLD__%"=="CONTRIB_BLD" ECHO Creating Contrib Libraries ...
IF "%__BLD__%"=="CONTRIB_BLD" goto _CORE
IF "%HB_MT%"=="" ECHO Creating System Files (ST) ...
IF "%HB_MT%"=="" goto _CORE
ECHO Creating System Files (MT) ...

:_CORE
if not exist obj               md obj
if not exist obj\%SUB_DIR%     md obj\%SUB_DIR%
if not exist obj\%SUB_DIR%\mt  md obj\%SUB_DIR%\mt

if not exist lib               md lib
if not exist lib\%SUB_DIR%     md lib\%SUB_DIR%

if not exist bin               md bin
if not exist bin\%SUB_DIR%     md bin\%SUB_DIR%
goto EXIT

rem=============================================================================
:CREATEDLL
rem=============================================================================
ECHO Creating DLL Files ...
if not exist obj               md obj
if not exist obj\%SUB_DIR%     md obj\%SUB_DIR%
if not exist obj\%SUB_DIR%\dll md obj\%SUB_DIR%\dll
if not exist lib\%SUB_DIR%     md lib\%SUB_DIR%
goto EXIT

rem=============================================================================
:COPYBIN
rem=============================================================================
ECHO System Files Succesfully Built ...
ECHO Copying System Files to BIN and LIB Folders ...
if exist bin\%SUB_DIR%\*.exe xcopy     bin\%SUB_DIR%\*.exe bin /D /Y > nul
if exist lib\%SUB_DIR%\*%LIBEXT% xcopy lib\%SUB_DIR%\*%LIBEXT% lib /D /Y > nul
if exist bin\%SUB_DIR%\*.tds xcopy     bin\%SUB_DIR%\*.tds bin /D /Y > nul
if exist lib\%SUB_DIR%\*.bak del       lib\%SUB_DIR%\*.bak
ECHO Done ...
ECHO.
goto EXIT

rem=============================================================================
:COPYDLL
rem=============================================================================
ECHO DLL Files Succesfully Built ...
ECHO Copying DLL Files to BIN Folder ...
if exist hdll.tmp del hdll.tmp
if exist bin\%SUB_DIR%\harbour.lib     copy bin\%SUB_DIR%\harbour.lib     lib > nul
if exist bin\%SUB_DIR%\harbour.dll     copy bin\%SUB_DIR%\harbour.dll     bin > nul
if exist bin\%SUB_DIR%\hbdocdll.exe    copy bin\%SUB_DIR%\hbdocdll.exe    bin > nul
if exist bin\%SUB_DIR%\hbmakedll.exe   copy bin\%SUB_DIR%\hbmakedll.exe   bin > nul
if exist bin\%SUB_DIR%\hbrundll.exe    copy bin\%SUB_DIR%\hbrundll.exe    bin > nul
if exist bin\%SUB_DIR%\hbtestdll.exe   copy bin\%SUB_DIR%\hbtestdll.exe   bin > nul
if exist bin\%SUB_DIR%\xbscriptdll.exe copy bin\%SUB_DIR%\xbscriptdll.exe bin > nul
ECHO Done ...
ECHO.
goto EXIT

rem=============================================================================
:COPYCONTRIBLIBS
rem=============================================================================
ECHO Contrib Libraries Succesfully Built ...
ECHO Copying Contrib Libraries to LIB Folder ...
if exist lib\%SUB_DIR%\%LIBPREFIX%mysql%LIBEXT%    copy lib\%SUB_DIR%\%LIBPREFIX%mysql%LIBEXT%    lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%firebird%LIBEXT% copy lib\%SUB_DIR%\%LIBPREFIX%firebird%LIBEXT% lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%fi_lib%LIBEXT%   copy lib\%SUB_DIR%\%LIBPREFIX%fi_lib%LIBEXT%   lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%gdlib%LIBEXT%    copy lib\%SUB_DIR%\%LIBPREFIX%gdlib%LIBEXT%    lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%hbzip%LIBEXT%    copy lib\%SUB_DIR%\%LIBPREFIX%hbzip%LIBEXT%    lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%libnf%LIBEXT%    copy lib\%SUB_DIR%\%LIBPREFIX%libnf%LIBEXT%    lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%pdflib%LIBEXT%   copy lib\%SUB_DIR%\%LIBPREFIX%pdflib%LIBEXT%   lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%ace32%LIBEXT%    copy lib\%SUB_DIR%\%LIBPREFIX%ace32%LIBEXT%    lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%rddads%LIBEXT%   copy lib\%SUB_DIR%\%LIBPREFIX%rddads%LIBEXT%   lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%telepath%LIBEXT% copy lib\%SUB_DIR%\%LIBPREFIX%telepath%LIBEXT% lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%hbcc%LIBEXT%     copy lib\%SUB_DIR%\%LIBPREFIX%hbcc%LIBEXT%     lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%what32%LIBEXT%   copy lib\%SUB_DIR%\%LIBPREFIX%what32%LIBEXT%   lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%png%LIBEXT%      copy lib\%SUB_DIR%\%LIBPREFIX%png%LIBEXT%      lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%hbhpdf%LIBEXT%   copy lib\%SUB_DIR%\%LIBPREFIX%hbhpdf%LIBEXT%   lib >NUL
if exist lib\%SUB_DIR%\%LIBPREFIX%gtwvg%LIBEXT%    copy lib\%SUB_DIR%\%LIBPREFIX%gtwvg%LIBEXT%    lib >NUL
ECHO Done ...
ECHO.
goto EXIT

rem=============================================================================
:REMOVE
rem=============================================================================
ECHO Removing System and Object Files ...

if exist source\pp\pptable.c         del source\pp\pptable.c
if exist obj\%SUB_DIR%\pptable.obj   del obj\%SUB_DIR%\pptable.obj

if exist bin\harbour.exe    del bin\harbour.exe
if exist bin\harbour.tds    del bin\harbour.tds
if exist bin\harbour.map    del bin\harbour.map
if exist bin\harbour.cgl    del bin\harbour.cgl

if exist bin\hbdoc.exe      del bin\hbdoc.exe
if exist bin\hbdoc.tds      del bin\hbdoc.tds
if exist bin\hbdoc.map      del bin\hbdoc.map
if exist bin\hbdoc.cgl      del bin\hbdoc.cgl

if exist bin\hbmake.exe     del bin\hbmake.exe
if exist bin\hbmake.tds     del bin\hbmake.tds
if exist bin\hbmake.map     del bin\hbmake.map
if exist bin\hbmake.cgl     del bin\hbmake.cgl

if exist bin\hbpp.exe       del bin\hbpp.exe
if exist bin\hbpp.tds       del bin\hbpp.tds
if exist bin\hbpp.map       del bin\hbpp.map
if exist bin\hbpp.cgl       del bin\hbpp.cgl

if exist bin\hbrun.exe      del bin\hbrun.exe
if exist bin\hbrun.tds      del bin\hbrun.tds
if exist bin\hbrun.map      del bin\hbrun.map
if exist bin\hbrun.cgl      del bin\hbrun.cgl

if exist bin\hbrunmt.exe    del bin\hbrunmt.exe
if exist bin\hbrunmt.tds    del bin\hbrunmt.tds
if exist bin\hbrunmt.map    del bin\hbrunmt.map
if exist bin\hbrunmt.cgl    del bin\hbrunmt.cgl

if exist bin\hbtest.exe     del bin\hbtest.exe
if exist bin\hbtest.tds     del bin\hbtest.tds
if exist bin\hbtest.map     del bin\hbtest.map
if exist bin\hbtest.cgl     del bin\hbtest.cgl

if exist bin\hbtestmt.exe   del bin\hbtestmt.exe
if exist bin\hbtestmt.tds   del bin\hbtestmt.tds
if exist bin\hbtestmt.map   del bin\hbtestmt.map
if exist bin\hbtestmt.cgl   del bin\hbtestmt.cgl

if exist bin\xbscript.exe   del bin\xbscript.exe
if exist bin\xbscript.tds   del bin\xbscript.tds
if exist bin\xbscript.map   del bin\xbscript.map
if exist bin\xbscript.cgl   del bin\xbscript.cgl

if exist bin\ppgen.exe      del bin\ppgen.exe
if exist bin\ppgen.tds      del bin\ppgen.tds
if exist bin\ppgen.map      del bin\ppgen.map
if exist bin\ppgen.cgl      del bin\ppgen.cgl

if exist lib\%LIBPREFIX%xharbour%LIBEXT%             del lib\%LIBPREFIX%xharbour%LIBEXT%
if exist lib\%LIBPREFIX%codepage%LIBEXT%             del lib\%LIBPREFIX%codepage%LIBEXT%
if exist lib\%LIBPREFIX%common%LIBEXT%               del lib\%LIBPREFIX%common%LIBEXT%
if exist lib\%LIBPREFIX%ct%LIBEXT%                   del lib\%LIBPREFIX%ct%LIBEXT%
if exist lib\%LIBPREFIX%ctmt%LIBEXT%                 del lib\%LIBPREFIX%ctmt%LIBEXT%
if exist lib\%LIBPREFIX%dbfcdx%LIBEXT%               del lib\%LIBPREFIX%dbfcdx%LIBEXT%
if exist lib\%LIBPREFIX%dbfcdxmt%LIBEXT%             del lib\%LIBPREFIX%dbfcdxmt%LIBEXT%
if exist lib\%LIBPREFIX%bmdbfcdx%LIBEXT%             del lib\%LIBPREFIX%bmdbfcdx%LIBEXT%
if exist lib\%LIBPREFIX%bmdbfcdxmt%LIBEXT%           del lib\%LIBPREFIX%bmdbfcdxmt%LIBEXT%
if exist lib\%LIBPREFIX%bmsixcdx%LIBEXT%             del lib\%LIBPREFIX%bmsixcdx%LIBEXT%
if exist lib\%LIBPREFIX%bmsixcdxmt%LIBEXT%           del lib\%LIBPREFIX%bmsixcdxmt%LIBEXT%
if exist lib\%LIBPREFIX%dbfdbt%LIBEXT%               del lib\%LIBPREFIX%dbfdbt%LIBEXT%
if exist lib\%LIBPREFIX%dbfdbtmt%LIBEXT%             del lib\%LIBPREFIX%dbfdbtmt%LIBEXT%
if exist lib\%LIBPREFIX%dbffpt%LIBEXT%               del lib\%LIBPREFIX%dbffpt%LIBEXT%
if exist lib\%LIBPREFIX%dbffptmt%LIBEXT%             del lib\%LIBPREFIX%dbffptmt%LIBEXT%
if exist lib\%LIBPREFIX%dbfntx%LIBEXT%               del lib\%LIBPREFIX%dbfntx%LIBEXT%
if exist lib\%LIBPREFIX%dbfntxmt%LIBEXT%             del lib\%LIBPREFIX%dbfntxmt%LIBEXT%
if exist lib\%LIBPREFIX%debug%LIBEXT%                del lib\%LIBPREFIX%debug%LIBEXT%
if exist lib\%LIBPREFIX%dllmain%LIBEXT%              del lib\%LIBPREFIX%dllmain%LIBEXT%
if exist lib\%LIBPREFIX%fmstat%LIBEXT%               del lib\%LIBPREFIX%fmstat%LIBEXT%
if exist lib\%LIBPREFIX%fmstatmt%LIBEXT%             del lib\%LIBPREFIX%fmstatmt%LIBEXT%
if exist lib\%LIBPREFIX%gtcgi%LIBEXT%                del lib\%LIBPREFIX%gtcgi%LIBEXT%
if exist lib\%LIBPREFIX%gtgui%LIBEXT%                del lib\%LIBPREFIX%gtgui%LIBEXT%
if exist lib\%LIBPREFIX%gtnul%LIBEXT%                del lib\%LIBPREFIX%gtnul%LIBEXT%
if exist lib\%LIBPREFIX%gtpca%LIBEXT%                del lib\%LIBPREFIX%gtpca%LIBEXT%
if exist lib\%LIBPREFIX%gtstd%LIBEXT%                del lib\%LIBPREFIX%gtstd%LIBEXT%
if exist lib\%LIBPREFIX%gtwin%LIBEXT%                del lib\%LIBPREFIX%gtwin%LIBEXT%
if exist lib\%LIBPREFIX%gtwvg%LIBEXT%                del lib\%LIBPREFIX%gtwvg%LIBEXT%
if exist lib\%LIBPREFIX%gtwvt%LIBEXT%                del lib\%LIBPREFIX%gtwvt%LIBEXT%
if exist lib\%LIBPREFIX%hbodbc%LIBEXT%               del lib\%LIBPREFIX%hbodbc%LIBEXT%
if exist lib\%LIBPREFIX%hbodbcmt%LIBEXT%             del lib\%LIBPREFIX%hbodbcmt%LIBEXT%
if exist lib\%LIBPREFIX%hbsix%LIBEXT%                del lib\%LIBPREFIX%hbsix%LIBEXT%
if exist lib\%LIBPREFIX%hbsixmt%LIBEXT%              del lib\%LIBPREFIX%hbsixmt%LIBEXT%
if exist lib\%LIBPREFIX%hsx%LIBEXT%                  del lib\%LIBPREFIX%hsx%LIBEXT%
if exist lib\%LIBPREFIX%hsxmt%LIBEXT%                del lib\%LIBPREFIX%hsxmt%LIBEXT%
if exist lib\%LIBPREFIX%lang%LIBEXT%                 del lib\%LIBPREFIX%lang%LIBEXT%
if exist lib\%LIBPREFIX%libmisc%LIBEXT%              del lib\%LIBPREFIX%libmisc%LIBEXT%
if exist lib\%LIBPREFIX%macro%LIBEXT%                del lib\%LIBPREFIX%macro%LIBEXT%
if exist lib\%LIBPREFIX%macromt%LIBEXT%              del lib\%LIBPREFIX%macromt%LIBEXT%
if exist lib\%LIBPREFIX%nulsys%LIBEXT%               del lib\%LIBPREFIX%nulsys%LIBEXT%
if exist lib\%LIBPREFIX%pcrepos%LIBEXT%              del lib\%LIBPREFIX%pcrepos%LIBEXT%
if exist lib\%LIBPREFIX%pp%LIBEXT%                   del lib\%LIBPREFIX%pp%LIBEXT%
if exist lib\%LIBPREFIX%ppmt%LIBEXT%                 del lib\%LIBPREFIX%ppmt%LIBEXT%
if exist lib\%LIBPREFIX%rdd%LIBEXT%                  del lib\%LIBPREFIX%rdd%LIBEXT%
if exist lib\%LIBPREFIX%rddmt%LIBEXT%                del lib\%LIBPREFIX%rddmt%LIBEXT%
if exist lib\%LIBPREFIX%rdds%LIBEXT%                 del lib\%LIBPREFIX%rdds%LIBEXT%
if exist lib\%LIBPREFIX%rddsmt%LIBEXT%               del lib\%LIBPREFIX%rddsmt%LIBEXT%
if exist lib\%LIBPREFIX%rtl%LIBEXT%                  del lib\%LIBPREFIX%rtl%LIBEXT%
if exist lib\%LIBPREFIX%rtlmt%LIBEXT%                del lib\%LIBPREFIX%rtlmt%LIBEXT%
if exist lib\%LIBPREFIX%samples%LIBEXT%              del lib\%LIBPREFIX%samples%LIBEXT%
if exist lib\%LIBPREFIX%samplesmt%LIBEXT%            del lib\%LIBPREFIX%samplesmt%LIBEXT%
if exist lib\%LIBPREFIX%sixcdx%LIBEXT%               del lib\%LIBPREFIX%sixcdx%LIBEXT%
if exist lib\%LIBPREFIX%sixcdxmt%LIBEXT%             del lib\%LIBPREFIX%sixcdxmt%LIBEXT%
if exist lib\%LIBPREFIX%tip%LIBEXT%                  del lib\%LIBPREFIX%tip%LIBEXT%
if exist lib\%LIBPREFIX%tipmt%LIBEXT%                del lib\%LIBPREFIX%tipmt%LIBEXT%
if exist lib\%LIBPREFIX%usrrdd%LIBEXT%               del lib\%LIBPREFIX%usrrdd%LIBEXT%
if exist lib\%LIBPREFIX%usrrddmt%LIBEXT%             del lib\%LIBPREFIX%usrrddmt%LIBEXT%
if exist lib\%LIBPREFIX%vm%LIBEXT%                   del lib\%LIBPREFIX%vm%LIBEXT%
if exist lib\%LIBPREFIX%vmmt%LIBEXT%                 del lib\%LIBPREFIX%vmmt%LIBEXT%
if exist lib\%LIBPREFIX%zlib%LIBEXT%                 del lib\%LIBPREFIX%zlib%LIBEXT%
if exist lib\%LIBPREFIX%png%LIBEXT%                  del lib\%LIBPREFIX%png%LIBEXT%
if exist lib\%LIBPREFIX%hbhpdf%LIBEXT%               del lib\%LIBPREFIX%hbhpdf%LIBEXT%
if exist lib\%LIBPREFIX%*.bak                        del lib\%LIBPREFIX%*.bak
if exist lib\%LIBPREFIX%*%OBJEXT%                    del lib\%LIBPREFIX%*%OBJEXT%

if exist bin\*.tds del bin\*.tds
if exist bin\*.map del bin\*.map

if exist bin\%SUB_DIR%\harbour.exe                   del bin\%SUB_DIR%\harbour.exe
if exist bin\%SUB_DIR%\hbdoc.exe                     del bin\%SUB_DIR%\hbdoc.exe
if exist bin\%SUB_DIR%\hbmake.exe                    del bin\%SUB_DIR%\hbmake.exe
if exist bin\%SUB_DIR%\hbpp.exe                      del bin\%SUB_DIR%\hbpp.exe
if exist bin\%SUB_DIR%\hbrun.exe                     del bin\%SUB_DIR%\hbrun.exe
if exist bin\%SUB_DIR%\hbrunmt.exe                   del bin\%SUB_DIR%\hbrunmt.exe
if exist bin\%SUB_DIR%\hbtest.exe                    del bin\%SUB_DIR%\hbtest.exe
if exist bin\%SUB_DIR%\hbtestmt.exe                  del bin\%SUB_DIR%\hbtestmt.exe
if exist bin\%SUB_DIR%\ppgen.exe                     del bin\%SUB_DIR%\ppgen.exe
if exist bin\%SUB_DIR%\xbscript.exe                  del bin\%SUB_DIR%\xbscript.exe
if exist bin\%SUB_DIR%\*.tds                         del bin\%SUB_DIR%\*.tds
if exist bin\%SUB_DIR%\*.map                         del bin\%SUB_DIR%\*.map
if exist bin\%SUB_DIR%\*.cgl                         del bin\%SUB_DIR%\*.cgl
if exist bin\%SUB_DIR%\*%LIBEXT%                     del bin\%SUB_DIR%\*%LIBEXT%

if exist lib\%SUB_DIR%\%LIBPREFIX%*.bak              del lib\%SUB_DIR%\%LIBPREFIX%*.bak
if exist lib\%SUB_DIR%\%LIBPREFIX%*%OBJEXT%          del lib\%SUB_DIR%\%LIBPREFIX%*%OBJEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%*.map              del lib\%SUB_DIR%\%LIBPREFIX%*.map
if exist lib\%SUB_DIR%\%LIBPREFIX%bmdbfcdx%LIBEXT%   del lib\%SUB_DIR%\%LIBPREFIX%bmdbfcdx%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%bmdbfcdxmt%LIBEXT% del lib\%SUB_DIR%\%LIBPREFIX%bmdbfcdxmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%bmsixcdx%LIBEXT%   del lib\%SUB_DIR%\%LIBPREFIX%bmsixcdx%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%bmsixcdxmt%LIBEXT% del lib\%SUB_DIR%\%LIBPREFIX%bmsixcdxmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%codepage%LIBEXT%   del lib\%SUB_DIR%\%LIBPREFIX%codepage%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%common%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%common%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%ct%LIBEXT%         del lib\%SUB_DIR%\%LIBPREFIX%ct%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%ctmt%LIBEXT%       del lib\%SUB_DIR%\%LIBPREFIX%ctmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%dbfcdx%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%dbfcdx%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%dbfcdxmt%LIBEXT%   del lib\%SUB_DIR%\%LIBPREFIX%dbfcdxmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%dbffpt%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%dbffpt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%dbffptmt%LIBEXT%   del lib\%SUB_DIR%\%LIBPREFIX%dbffptmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%dbfntx%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%dbfntx%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%dbfntxmt%LIBEXT%   del lib\%SUB_DIR%\%LIBPREFIX%dbfntxmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%debug%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%debug%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%dllmain%LIBEXT%    del lib\%SUB_DIR%\%LIBPREFIX%dllmain%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%fmstat%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%fmstat%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%fmstatmt%LIBEXT%   del lib\%SUB_DIR%\%LIBPREFIX%fmstatmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%gtcgi%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%gtcgi%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%gtgui%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%gtgui%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%gtpca%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%gtpca%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%gtstd%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%gtstd%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%gtwin%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%gtwin%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%gtwvg%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%gtwvg%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%gtwvt%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%gtwvt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%hbodbc%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%hbodbc%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%hbsix%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%hbsix%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%hbsixmt%LIBEXT%    del lib\%SUB_DIR%\%LIBPREFIX%hbsixmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%hsx%LIBEXT%        del lib\%SUB_DIR%\%LIBPREFIX%hsx%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%hsxmt%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%hsxmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%lang%LIBEXT%       del lib\%SUB_DIR%\%LIBPREFIX%lang%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%libmisc%LIBEXT%    del lib\%SUB_DIR%\%LIBPREFIX%libmisc%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%macro%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%macro%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%macromt%LIBEXT%    del lib\%SUB_DIR%\%LIBPREFIX%macromt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%nulsys%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%nulsys%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%pcrepos%LIBEXT%    del lib\%SUB_DIR%\%LIBPREFIX%pcrepos%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%pp%LIBEXT%         del lib\%SUB_DIR%\%LIBPREFIX%pp%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%ppmt%LIBEXT%       del lib\%SUB_DIR%\%LIBPREFIX%ppmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%rdd%LIBEXT%        del lib\%SUB_DIR%\%LIBPREFIX%rdd%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%rddmt%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%rddmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%rdds%LIBEXT%       del lib\%SUB_DIR%\%LIBPREFIX%rdds%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%rddsmt%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%rddsmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%rtl%LIBEXT%        del lib\%SUB_DIR%\%LIBPREFIX%rtl%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%rtlmt%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%rtlmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%sixcdx%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%sixcdx%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%sixcdxmt%LIBEXT%   del lib\%SUB_DIR%\%LIBPREFIX%sixcdxmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%tip%LIBEXT%        del lib\%SUB_DIR%\%LIBPREFIX%tip%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%tipmt%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%tipmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%usrrdd%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%usrrdd%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%usrrddmt%LIBEXT%   del lib\%SUB_DIR%\%LIBPREFIX%usrrddmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%vm%LIBEXT%         del lib\%SUB_DIR%\%LIBPREFIX%vm%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%vmmt%LIBEXT%       del lib\%SUB_DIR%\%LIBPREFIX%vmmt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%zlib%LIBEXT%       del lib\%SUB_DIR%\%LIBPREFIX%zlib%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%hbhpdf%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%hbhpdf%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%png%LIBEXT%        del lib\%SUB_DIR%\%LIBPREFIX%png%LIBEXT%

if exist obj\%SUB_DIR%\*.bak                         del obj\%SUB_DIR%\*.bak
if exist obj\%SUB_DIR%\*%OBJEXT%                     del obj\%SUB_DIR%\*%OBJEXT%
if exist obj\%SUB_DIR%\*.output                      del obj\%SUB_DIR%\*.output
if exist obj\%SUB_DIR%\*.c                           del obj\%SUB_DIR%\*.c
if exist obj\%SUB_DIR%\*.ppo                         del obj\%SUB_DIR%\*.ppo
if exist obj\%SUB_DIR%\*.h                           del obj\%SUB_DIR%\*.h

if exist obj\%SUB_DIR%\bin\*.bak                     del obj\%SUB_DIR%\bin\*.bak
if exist obj\%SUB_DIR%\bin\*%OBJEXT%                 del obj\%SUB_DIR%\bin\*%OBJEXT%
if exist obj\%SUB_DIR%\bin\*.output                  del obj\%SUB_DIR%\bin\*.output
if exist obj\%SUB_DIR%\bin\*.c                       del obj\%SUB_DIR%\bin\*.c
if exist obj\%SUB_DIR%\bin\*.h                       del obj\%SUB_DIR%\bin\*.h

if exist obj\%SUB_DIR%\mt\*%OBJEXT%                  del obj\%SUB_DIR%\mt\*%OBJEXT%
if exist obj\%SUB_DIR%\mt\*.output                   del obj\%SUB_DIR%\mt\*.output
if exist obj\%SUB_DIR%\mt\*.c                        del obj\%SUB_DIR%\mt\*.c
if exist obj\%SUB_DIR%\mt\*.ppo                      del obj\%SUB_DIR%\mt\*.ppo
if exist obj\%SUB_DIR%\mt\*.h                        del obj\%SUB_DIR%\mt\*.h

if exist bin\%SUB_DIR%\harbour.exp                   del bin\%SUB_DIR%\harbour.exp
if exist bin\%SUB_DIR%\hbdoc.exp                     del bin\%SUB_DIR%\hbdoc.exp
if exist bin\%SUB_DIR%\hbmake.exp                    del bin\%SUB_DIR%\hbmake.exp
if exist bin\%SUB_DIR%\hbpp.exp                      del bin\%SUB_DIR%\hbpp.exp
if exist bin\%SUB_DIR%\hbrun.exp                     del bin\%SUB_DIR%\hbrun.exp
if exist bin\%SUB_DIR%\hbrunMT.exp                   del bin\%SUB_DIR%\hbrunMT.exp
if exist bin\%SUB_DIR%\hbtest.exp                    del bin\%SUB_DIR%\hbtest.exp
if exist bin\%SUB_DIR%\hbtestMT.exp                  del bin\%SUB_DIR%\hbtestMT.exp
if exist bin\%SUB_DIR%\ppgen.exp                     del bin\%SUB_DIR%\ppgen.exp
if exist bin\%SUB_DIR%\xbscript.exp                  del bin\%SUB_DIR%\xbscript.exp

if exist bin\%SUB_DIR%\harbour%LIBEXT%               del bin\%SUB_DIR%\harbour%LIBEXT%
if exist bin\%SUB_DIR%\hbdoc%LIBEXT%                 del bin\%SUB_DIR%\hbdoc%LIBEXT%
if exist bin\%SUB_DIR%\hbmake%LIBEXT%                del bin\%SUB_DIR%\hbmake%LIBEXT%
if exist bin\%SUB_DIR%\hbpp%LIBEXT%                  del bin\%SUB_DIR%\hbpp%LIBEXT%
if exist bin\%SUB_DIR%\hbrun%LIBEXT%                 del bin\%SUB_DIR%\hbrun%LIBEXT%
if exist bin\%SUB_DIR%\hbrunMT%LIBEXT%               del bin\%SUB_DIR%\hbrunMT%LIBEXT%
if exist bin\%SUB_DIR%\hbtest%LIBEXT%                del bin\%SUB_DIR%\hbtest%LIBEXT%
if exist bin\%SUB_DIR%\hbtestMT%LIBEXT%              del bin\%SUB_DIR%\hbtestMT%LIBEXT%
if exist bin\%SUB_DIR%\ppgen%LIBEXT%                 del bin\%SUB_DIR%\ppgen%LIBEXT%
if exist bin\%SUB_DIR%\xbscript%LIBEXT%              del bin\%SUB_DIR%\xbscript%LIBEXT%

ECHO Removing DLL and Object Files ...
if exist obj\%SUB_DIR%\dll\*%OBJEXT%                 del obj\%SUB_DIR%\dll\*%OBJEXT%
if exist obj\%SUB_DIR%\dll\*.c                       del obj\%SUB_DIR%\dll\*.c
if exist obj\%SUB_DIR%\dll\*.ppo                     del obj\%SUB_DIR%\dll\*.ppo
if exist obj\%SUB_DIR%\dll\*.h                       del obj\%SUB_DIR%\dll\*.h
if exist obj\%SUB_DIR%\dll\*.output                  del obj\%SUB_DIR%\dll\*.output

if exist bin\%SUB_DIR%\harbour.map		     del bin\%SUB_DIR%\harbour.map
if exist bin\%SUB_DIR%\harbour.tds		     del bin\%SUB_DIR%\harbour.tds
if exist bin\%SUB_DIR%\harbour.dll		     del bin\%SUB_DIR%\harbour.dll
if exist bin\%SUB_DIR%\%LIBPREFIX%harbour%LIBEXT%    del bin\%SUB_DIR%\%LIBPREFIX%harbour%LIBEXT%
if exist bin\%SUB_DIR%\hbdocdll.exe                  del bin\%SUB_DIR%\hbdocdll.exe
if exist bin\%SUB_DIR%\hbdocdll.map                  del bin\%SUB_DIR%\hbdocdll.map
if exist bin\%SUB_DIR%\hbdocdll.tds                  del bin\%SUB_DIR%\hbdocdll.tds
if exist bin\%SUB_DIR%\hbmakedll.exe                 del bin\%SUB_DIR%\hbmakedll.exe
if exist bin\%SUB_DIR%\hbmakedll.map                 del bin\%SUB_DIR%\hbmakedll.map
if exist bin\%SUB_DIR%\hbmakedll.tds                 del bin\%SUB_DIR%\hbmakedll.tds
if exist bin\%SUB_DIR%\hbrundll.exe                  del bin\%SUB_DIR%\hbrundll.exe
if exist bin\%SUB_DIR%\hbrundll.map                  del bin\%SUB_DIR%\hbrundll.map
if exist bin\%SUB_DIR%\hbrundll.tds                  del bin\%SUB_DIR%\hbrundll.tds
if exist bin\%SUB_DIR%\hbtestdll.exe                 del bin\%SUB_DIR%\hbtestdll.exe
if exist bin\%SUB_DIR%\hbtestdll.map                 del bin\%SUB_DIR%\hbtestdll.map
if exist bin\%SUB_DIR%\hbtestdll.tds                 del bin\%SUB_DIR%\hbtestdll.tds
if exist bin\%SUB_DIR%\xbscriptdll.exe               del bin\%SUB_DIR%\xbscriptdll.exe
if exist bin\%SUB_DIR%\xbscriptdll.map               del bin\%SUB_DIR%\xbscriptdll.map
if exist bin\%SUB_DIR%\xbscriptdll.tds               del bin\%SUB_DIR%\xbscriptdll.tds

if exist lib\%LIBPREFIX%harbour%LIBEXT%              del lib\%LIBPREFIX%harbour%LIBEXT%
if exist bin\%SUB_DIR%\hbdocdll.exp                  del bin\%SUB_DIR%\hbdocdll.exp
if exist bin\%SUB_DIR%\hbdocdll.lib                  del bin\%SUB_DIR%\hbdocdll.lib
if exist bin\%SUB_DIR%\hbrundll.exp                  del bin\%SUB_DIR%\hbrundll.exp
if exist bin\%SUB_DIR%\hbrundll.lib                  del bin\%SUB_DIR%\hbrundll.lib
if exist bin\%SUB_DIR%\hbtestdll.exp                 del bin\%SUB_DIR%\hbtestdll.exp
if exist bin\%SUB_DIR%\hbtestdll.lib                 del bin\%SUB_DIR%\hbtestdll.lib
if exist bin\%SUB_DIR%\hbmakedll.exp                 del bin\%SUB_DIR%\hbmakedll.exp
if exist bin\%SUB_DIR%\hbmakedll.lib                 del bin\%SUB_DIR%\hbmakedll.lib
if exist bin\%SUB_DIR%\xbscriptdll.exp               del bin\%SUB_DIR%\xbscriptdll.exp
if exist bin\%SUB_DIR%\xbscriptdll.lib               del bin\%SUB_DIR%\xbscriptdll.lib

if exist bin\%SUB_DIR%\hbdocdll.ilc                  del bin\%SUB_DIR%\hbdocdll.ilc
if exist bin\%SUB_DIR%\hbdocdll.ild                  del bin\%SUB_DIR%\hbdocdll.ild
if exist bin\%SUB_DIR%\hbdocdll.ilf                  del bin\%SUB_DIR%\hbdocdll.ilf
if exist bin\%SUB_DIR%\hbdocdll.ils                  del bin\%SUB_DIR%\hbdocdll.ils
if exist bin\%SUB_DIR%\hbmakedll.ilc                 del bin\%SUB_DIR%\hbmakedll.ilc
if exist bin\%SUB_DIR%\hbmakedll.ild                 del bin\%SUB_DIR%\hbmakedll.ild
if exist bin\%SUB_DIR%\hbmakedll.ilf                 del bin\%SUB_DIR%\hbmakedll.ilf
if exist bin\%SUB_DIR%\hbmakedll.ils                 del bin\%SUB_DIR%\hbmakedll.ils
if exist bin\%SUB_DIR%\hbrundll.ilc                  del bin\%SUB_DIR%\hbrundll.ilc
if exist bin\%SUB_DIR%\hbrundll.ild                  del bin\%SUB_DIR%\hbrundll.ild
if exist bin\%SUB_DIR%\hbrundll.ilf                  del bin\%SUB_DIR%\hbrundll.ilf
if exist bin\%SUB_DIR%\hbrundll.ils                  del bin\%SUB_DIR%\hbrundll.ils
if exist bin\%SUB_DIR%\hbtestdll.ilc                 del bin\%SUB_DIR%\hbtestdll.ilc
if exist bin\%SUB_DIR%\hbtestdll.ild                 del bin\%SUB_DIR%\hbtestdll.ild
if exist bin\%SUB_DIR%\hbtestdll.ilf                 del bin\%SUB_DIR%\hbtestdll.ilf
if exist bin\%SUB_DIR%\hbtestdll.ils                 del bin\%SUB_DIR%\hbtestdll.ils
if exist bin\%SUB_DIR%\xbscriptdll.ilc               del bin\%SUB_DIR%\xbscriptdll.ilc
if exist bin\%SUB_DIR%\xbscriptdll.ild               del bin\%SUB_DIR%\xbscriptdll.ild
if exist bin\%SUB_DIR%\xbscriptdll.ilf               del bin\%SUB_DIR%\xbscriptdll.ilf
if exist bin\%SUB_DIR%\xbscriptdll.ils               del bin\%SUB_DIR%\xbscriptdll.ils

if exist bin\harbour.dll                             del bin\harbour.dll
if exist bin\hbdocdll.exe                            del bin\hbdocdll.exe
if exist bin\hbmakedll.exe                           del bin\hbmakedll.exe
if exist bin\hbrundll.exe                            del bin\hbrundll.exe
if exist bin\hbtestdll.exe                           del bin\hbtestdll.exe
if exist bin\xbscriptdll.exe                         del bin\xbscriptdll.exe

ECHO Removing Contrib Libraries and Object Files ...
if exist obj\%SUB_DIR%\*%OBJEXT%                    del obj\%SUB_DIR%\*%OBJEXT%
if exist obj\%SUB_DIR%\mt\*%OBJEXT%                 del obj\%SUB_DIR%\mt\*%OBJEXT%
if exist obj\%SUB_DIR%\*.ppo                        del obj\%SUB_DIR%\*.ppo
if exist obj\%SUB_DIR%\*.idb                        del obj\%SUB_DIR%\*.idb
if exist obj\%SUB_DIR%\*.pch                        del obj\%SUB_DIR%\*.pch
if exist obj\%SUB_DIR%\mt\*.ppo                     del obj\%SUB_DIR%\mt\*.ppo
if exist obj\%SUB_DIR%\*.c                          del obj\%SUB_DIR%\*.c
if exist obj\%SUB_DIR%\mt\*.c                       del obj\%SUB_DIR%\mt\*.c

if exist lib\%SUB_DIR%\%LIBPREFIX%mysql%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%mysql%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%firebird%LIBEXT%  del lib\%SUB_DIR%\%LIBPREFIX%firebird%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%fi_lib%LIBEXT%    del lib\%SUB_DIR%\%LIBPREFIX%fi_lib%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%gdlib%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%gdlib%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%hbzip%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%hbzip%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%libnf%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%libnf%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%pdflib%LIBEXT%    del lib\%SUB_DIR%\%LIBPREFIX%pdflib%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%ace32%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%ace32%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%rddads%LIBEXT%    del lib\%SUB_DIR%\%LIBPREFIX%rddads%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%telepath%LIBEXT%  del lib\%SUB_DIR%\%LIBPREFIX%telepath%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%hbcc%LIBEXT%      del lib\%SUB_DIR%\%LIBPREFIX%hbcc%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%what32%LIBEXT%    del lib\%SUB_DIR%\%LIBPREFIX%what32%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%xwt%LIBEXT%       del lib\%SUB_DIR%\%LIBPREFIX%xwt%LIBEXT%
if exist lib\%SUB_DIR%\%LIBPREFIX%gtwvg%LIBEXT%     del lib\%SUB_DIR%\%LIBPREFIX%gtwvg%LIBEXT%

if exist lib\%LIBPREFIX%mysql%LIBEXT%               del lib\%LIBPREFIX%mysql%LIBEXT%
if exist lib\%LIBPREFIX%firebird%LIBEXT%            del lib\%LIBPREFIX%firebird%LIBEXT%
if exist lib\%LIBPREFIX%fi_lib%LIBEXT%              del lib\%LIBPREFIX%fi_lib%LIBEXT%
if exist lib\%LIBPREFIX%gdlib%LIBEXT%               del lib\%LIBPREFIX%gdlib%LIBEXT%
if exist lib\%LIBPREFIX%hbzip%LIBEXT%               del lib\%LIBPREFIX%hbzip%LIBEXT%
if exist lib\%LIBPREFIX%libnf%LIBEXT%               del lib\%LIBPREFIX%libnf%LIBEXT%
if exist lib\%LIBPREFIX%pdflib%LIBEXT%              del lib\%LIBPREFIX%pdflib%LIBEXT%
if exist lib\%LIBPREFIX%rddads%LIBEXT%              del lib\%LIBPREFIX%rddads%LIBEXT%
if exist lib\%LIBPREFIX%telepath%LIBEXT%            del lib\%LIBPREFIX%telepath%LIBEXT%
if exist lib\%LIBPREFIX%hbcc%LIBEXT%                del lib\%LIBPREFIX%hbcc%LIBEXT%
if exist lib\%LIBPREFIX%what32%LIBEXT%              del lib\%LIBPREFIX%what32%LIBEXT%
if exist lib\%LIBPREFIX%xwt%LIBEXT%                 del lib\%LIBPREFIX%xwt%LIBEXT%
if exist lib\%LIBPREFIX%gtwvg%LIBEXT%               del lib\%LIBPREFIX%gtwvg%LIBEXT%

IF EXIST make_%SUB_DIR%.log                         del make_%SUB_DIR%.log
IF EXIST dll_%SUB_DIR%.log                          del dll_%SUB_DIR%.log
IF EXIST cont_%SUB_DIR%.log                         del cont_%SUB_DIR%.log
ECHO Done ...
ECHO.
goto EXIT

rem=============================================================================
:_SYNTAX
rem=============================================================================
ECHO. Syntax:    make_%SUB_DIR% [all, core, dll, contrib, clean] [nomt] [build]
ECHO.
ECHO. Argument:
ECHO.  default : build xHarbour CORE files
ECHO.  core    : build xHarbour CORE files
ECHO.  all     : build CORE, DLL and CONTRIB
ECHO.  dll     : build xHarbour DLL
ECHO.  contrib : build CONTRIB Libraries
ECHO.  clean   : erase files once built and don't rebuild
ECHO.
ECHO. Examples:
ECHO.  make_%SUB_DIR%               : build ST and MT system files
ECHO.  make_%SUB_DIR% nomt          : ST core only, don't build MT system files
ECHO.  make_%SUB_DIR% clean nomt    : clean build, don't build MT system files
ECHO.  make_%SUB_DIR% clean         : clean only, don't build
ECHO.  make_%SUB_DIR% clean build   : clean only, then rebuild ALL
ECHO.
goto EXIT

rem=============================================================================
:RESET_ENVAR
rem=============================================================================
SET CC_DIR=
SET BISON_DIR=
SET SUB_DIR=
SET HB_GT_LIB=
SET LIBEXT=
SET OBJEXT=
SET DIR_SEP=
SET PATH=%_PATH%

IF NOT "%LIBPREFIX%"=="" SET LIBPREFIX=
IF NOT "%_PATH%"    =="" SET _PATH=
IF NOT "%HB_MT%"    =="" SET HB_MT=
IF NOT "%HB_MT_DIR%"=="" SET HB_MT_DIR=
IF NOT "%MAKEALL%"  =="" SET MAKEALL=
IF NOT "%MAKE_EXE%" =="" SET MAKE_EXE=
IF NOT "%__BLD__%"  =="" SET __BLD__=

rem=============================================================================
:EXIT
rem=============================================================================
