@echo off

set HB_INSTALL=\harbour
set HWGUI_INSTALL=..

%HB_INSTALL%\bin\harbour %1.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include %2 %3

cl /c /TP /W3 /nologo /Fo -I%HB_INSTALL%\include -I%HWGUI_INSTALL%\include %1.c


echo %HWGUI_INSTALL%\lib\hwgui.lib  > b32.vc
echo %HWGUI_INSTALL%\lib\procmisc.lib  >> b32.vc
echo %HB_INSTALL%\lib\rtl.lib  >> b32.vc
echo %HB_INSTALL%\lib\vm.lib  >> b32.vc
echo %HB_INSTALL%\lib\gtwin.lib >> b32.vc
echo %HB_INSTALL%\lib\lang.lib  >> b32.vc
echo %HB_INSTALL%\lib\macro.lib >> b32.vc
echo %HB_INSTALL%\lib\rdd.lib  >> b32.vc
echo %HB_INSTALL%\lib\dbfntx.lib >> b32.vc
echo %HB_INSTALL%\lib\common.lib >> b32.vc
echo %HB_INSTALL%\lib\debug.lib >> b32.vc
echo %HB_INSTALL%\lib\pp.lib >> b32.vc
echo user32.lib >> b32.vc
echo gdi32.lib >> b32.vc
echo comdlg32.lib >> b32.vc
echo shell32.lib  >> b32.vc
echo comctl32.lib >> b32.vc
echo winspool.lib >> b32.vc

rem IF EXIST %1.rc brc32 -r %1
link -SUBSYSTEM:WINDOWS -LIBPATH:d:\progra~1\micros~1\vc98\lib %1.obj @b32.vc
del %1.c
rem del %1.map
del %1.obj
del b32.vc