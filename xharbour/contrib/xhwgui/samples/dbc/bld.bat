@echo off

set HB_INSTALL=\harbour
set HWGUI_INSTALL=..\..
set RDD_ADS_INC=\harbour\contrib\rdd_ads

%HB_INSTALL%\bin\harbour dbchw.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include;%RDD_ADS_INC% -dRDD_ADS
%HB_INSTALL%\bin\harbour commands.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include;%RDD_ADS_INC% -dRDD_ADS
%HB_INSTALL%\bin\harbour modistru.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include;%RDD_ADS_INC% -dRDD_ADS
%HB_INSTALL%\bin\harbour move.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include;%RDD_ADS_INC% -dRDD_ADS
%HB_INSTALL%\bin\harbour query.prg -n -i%HB_INSTALL%\include;%HWGUI_INSTALL%\include;%RDD_ADS_INC% -dRDD_ADS

   bcc32 -c -O2 -tW -M -I%HB_INSTALL%\include dbchw.c commands.c modistru.c move.c query.c

   brc32 -r dbchw.rc

echo c0w32.obj + > b32.bc
echo dbchw.obj + >> b32.bc
echo commands.obj + >> b32.bc
echo modistru.obj + >> b32.bc
echo move.obj + >> b32.bc
echo query.obj, + >> b32.bc
echo dbchw.exe, + >> b32.bc
echo dbchw.map, + >> b32.bc
echo %HWGUI_INSTALL%\lib\hwgui.lib + >> b32.bc
echo %HWGUI_INSTALL%\lib\procmisc.lib + >> b32.bc
rem echo %HB_INSTALL%\lib\rddnul.lib + >> b32.bc
echo %HB_INSTALL%\lib\rtl.lib + >> b32.bc
echo %HB_INSTALL%\lib\vm.lib + >> b32.bc
echo %HB_INSTALL%\lib\gtwin.lib + >> b32.bc
echo %HB_INSTALL%\lib\lang.lib + >> b32.bc
echo %HB_INSTALL%\lib\macro.lib + >> b32.bc
echo %HB_INSTALL%\lib\rdd.lib + >> b32.bc
echo %HB_INSTALL%\lib\dbfntx.lib + >> b32.bc
rem echo %HB_INSTALL%\lib\nulsys.lib + >> b32.bc
echo %HB_INSTALL%\lib\common.lib + >> b32.bc
echo %HB_INSTALL%\lib\pp.lib + >> b32.bc
echo %HB_INSTALL%\lib\rddads.lib + >> b32.bc
echo %HB_INSTALL%\lib\ace32.lib + >> b32.bc

echo cw32.lib + >> b32.bc
echo import32.lib, >> b32.bc
echo dbchw.res >> b32.bc
ilink32 -Gn -aa -Tpe @b32.bc

del *.tds
del *.c
del *.map
del *.obj
del *.res
del b32.bc