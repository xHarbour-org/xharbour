
@echo off

if exist error.log del error.log

set HB_INSTALL=c:\harbour
set WGUI_INSTALL=..
set BCC_DIR=c:\bcc55

%HB_INSTALL%\bin\harbour %1 -p -n -w -i%HB_INSTALL%\include;%WGUI_INSTALL%\include
if errorlevel 1 goto exit

rem %BCC_DIR%\BIN\bcc32 -I%HB_INSTALL%\include;%BCC_DIR%\include -L%HB_INSTALL%\lib\;%BCC_DIR%\lib\ %1.c debug.lib vm.lib rtl.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib %WGUI_INSTALL%\lib\woopgui.lib
%BCC_DIR%\BIN\bcc32 -5 -O2 -tW -M -DHB_API_MACROS -DHB_STACK_MACROS -DHB_OS_WIN_32 -DHB_FM_STATISTICS_OFF -I%HB_INSTALL%\include;%BCC_DIR%\include -L%HB_INSTALL%\lib\;%BCC_DIR%\lib\ %1.c %WGUI_INSTALL%\lib\woopgui.lib %WGUI_INSTALL%\lib\wapilib.lib %WGUI_INSTALL%\lib\utils.lib debug.lib vm.lib rtl.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib 
if errorlevel 1 goto exit

del *.tds
del %1.c
del %1.map
del %1.obj
rem del %1.res
rem del b32.bc

if exist %1.exe %1

:exit
