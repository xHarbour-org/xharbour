@echo off

set HB_INSTALL=%HB_PATH%
set HWGUI_INSTALL=..

SET HB_MT=

#SET C_DEFINES= -DHB_THREAD_SUPPORT
#SET H_DEFINES= -DHB_THREAD_SUPPORT

SET C_DEFINES= 
SET H_DEFINES= 

%HB_INSTALL%\bin\harbour %1.prg %H_DEFINES% -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include %2 %3

bcc32  -c %C_DEFINES% -O2 -tW -M -I%HB_INSTALL%\include;%HWGUI_INSTALL%\include %1.c

IF EXIST %1.rc brc32 -r %1

echo c0w32.obj + > b32.bc
echo %1.obj, + >> b32.bc
echo %1.exe, + >> b32.bc
echo %1.map, + >> b32.bc
echo %HWGUI_INSTALL%\lib\hwgui.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\procmisc.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\hwg_qhtm.lib + >> b32.bc
echo %HB_INSTALL%\lib\rtl%HB_MT%.lib + >> b32.bc
echo %HB_INSTALL%\lib\vm%HB_MT%.lib + >> b32.bc
echo %HB_INSTALL%\lib\gtwin.lib + >> b32.bc
echo %HB_INSTALL%\lib\lang.lib + >> b32.bc
rem echo %HB_INSTALL%\lib\codepage.lib + >> b32.bc
echo %HB_INSTALL%\lib\macro%HB_MT%.lib + >> b32.bc
echo %HB_INSTALL%\lib\rdd%HB_MT%.lib + >> b32.bc
echo %HB_INSTALL%\lib\dbfntx%HB_MT%.lib + >> b32.bc
echo %HB_INSTALL%\lib\dbfcdx%HB_MT%.lib + >> b32.bc
echo %HB_INSTALL%\lib\dbfdbt%HB_MT%.lib + >> b32.bc
echo %HB_INSTALL%\lib\common.lib + >> b32.bc
echo %HB_INSTALL%\lib\debug.lib + >> b32.bc
echo %HB_INSTALL%\lib\pp.lib + >> b32.bc
rem echo %HB_INSTALL%\lib\libct.lib + >> b32.bc

echo cw32.lib + >> b32.bc
echo import32.lib, >> b32.bc
IF EXIST %1.res echo %1.res >> b32.bc
ilink32 -Gn -Tpe -aa @b32.bc

del *.tds
del %1.c
del %1.map
del %1.obj
del b32.bc
