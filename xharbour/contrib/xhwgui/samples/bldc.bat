@echo off

bcc32 -c -O2 -tW -M -I%HB_INSTALL%\include;%HWGUI_INSTALL%\include %1.c

echo c0w32.obj + > b32.bc
echo %1.obj, + >> b32.bc
echo %1.exe, + >> b32.bc
echo %1.map, + >> b32.bc
echo cw32.lib + >> b32.bc
echo import32.lib, >> b32.bc
ilink32 -Gn -Tpe -aa @b32.bc

del *.tds
del %1.map
del %1.obj
del b32.bc