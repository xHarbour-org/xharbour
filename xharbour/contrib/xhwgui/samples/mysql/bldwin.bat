@echo off

set HB_INSTALL=\harbour
set HWGUI_INSTALL=..\..

%HB_INSTALL%\bin\harbour hwmysql.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include

   bcc32 -c -O2 -tW -M -I%HB_INSTALL%\include hwmysql.c

   brc32 -r hwmysql.rc

echo c0w32.obj + > b32.bc
echo hwmysql.obj, + >> b32.bc
echo hwmysql.exe, + >> b32.bc
echo hwmysql.map, + >> b32.bc
echo %HWGUI_INSTALL%\lib\hwgui.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\procmisc.lib + >> b32.bc
echo %HB_INSTALL%\lib\rtl.lib + >> b32.bc
echo %HB_INSTALL%\lib\vm.lib + >> b32.bc
echo %HB_INSTALL%\lib\gtwin.lib + >> b32.bc
echo %HB_INSTALL%\lib\lang.lib + >> b32.bc
echo %HB_INSTALL%\lib\macro.lib + >> b32.bc
echo %HB_INSTALL%\lib\rdd.lib + >> b32.bc
echo %HB_INSTALL%\lib\dbfntx.lib + >> b32.bc
echo %HB_INSTALL%\lib\common.lib + >> b32.bc
echo %HB_INSTALL%\lib\pp.lib + >> b32.bc
echo %HB_INSTALL%\lib\mysql.lib + >> b32.bc
echo libmysql.lib + >> b32.bc

echo cw32.lib + >> b32.bc
echo import32.lib, >> b32.bc
echo hwmysql.res >> b32.bc
ilink32 -Gn -aa -Tpe -s -v @b32.bc

del *.tds
del *.c
del *.map
del *.obj
del b32.bc