@echo off

set HB_INSTALL=\xharbour
set HWGUI_INSTALL=..\..

%HB_INSTALL%\bin\harbour designer.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include
%HB_INSTALL%\bin\harbour hctrl.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include
%HB_INSTALL%\bin\harbour hformgen.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include
%HB_INSTALL%\bin\harbour propert.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include

   bcc32 -c -O2 -tW -M -I%HB_INSTALL%\include designer.c hctrl.c hformgen.c propert.c ctrlgen.c
   brc32 -r designer.rc

echo c0w32.obj + > b32.bc
echo designer.obj + >> b32.bc
echo hctrl.obj + >> b32.bc
echo hformgen.obj + >> b32.bc
echo ctrlgen.obj + >> b32.bc
echo propert.obj, + >> b32.bc
echo designer.exe, + >> b32.bc
echo designer.map, + >> b32.bc
echo %HWGUI_INSTALL%\lib\hwgui.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\procmisc.lib + >> b32.bc
echo %HB_INSTALL%\lib\rtl.lib + >> b32.bc
echo %HB_INSTALL%\lib\vm.lib + >> b32.bc
echo %HB_INSTALL%\lib\gtwin.lib + >> b32.bc
echo %HB_INSTALL%\lib\lang.lib + >> b32.bc
echo %HB_INSTALL%\lib\macro.lib + >> b32.bc
echo %HB_INSTALL%\lib\rdd.lib + >> b32.bc
echo %HB_INSTALL%\lib\dbfntx.lib + >> b32.bc
echo %HB_INSTALL%\lib\dbfdbt.lib + >> b32.bc
echo %HB_INSTALL%\lib\common.lib + >> b32.bc
echo %HB_INSTALL%\lib\pp.lib + >> b32.bc

echo cw32.lib + >> b32.bc
echo import32.lib, >> b32.bc
echo designer.res >> b32.bc
ilink32 -Gn -aa -Tpe @b32.bc

del *.tds
del designer.c
del hctrl.c
del hformgen.c
del propert.c
del *.map
del *.obj
del designer.res
del b32.bc
