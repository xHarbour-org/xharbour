del demo.exe
CLS
@ECHO OFF

IF '%HB_INSTALL%'=='' SET HB_INSTALL=..\..\..

echo c0w32.obj + > b32.bc

rem Compile demo

call hb demo
IF ERRORLEVEL 1 GOTO EXIT

:ENDCOMPILE
echo , demo.exe, + >> b32.bc
echo demo.map, + >> b32.bc
echo ..\lib\what32.lib +  >>  b32.bc
echo ..\lib\whatplus.lib +  >>  b32.bc
echo %HB_INSTALL%\lib\rtl.lib + >> b32.bc
echo %HB_INSTALL%\lib\vm.lib + >> b32.bc
echo %HB_INSTALL%\lib\gtwin.lib + >> b32.bc
echo %HB_INSTALL%\lib\lang.lib + >> b32.bc
echo %HB_INSTALL%\lib\macro.lib + >> b32.bc
echo %HB_INSTALL%\lib\rdd.lib + >> b32.bc
echo %HB_INSTALL%\lib\dbfntx.lib + >> b32.bc
echo %HB_INSTALL%\lib\dbfcdx.lib + >> b32.bc
echo %HB_INSTALL%\lib\debug.lib + >> b32.bc
echo %HB_INSTALL%\lib\common.lib + >> b32.bc
echo %HB_INSTALL%\lib\pp.lib + >> b32.bc
echo cw32.lib + >> b32.bc
echo import32.lib,, + >> b32.bc
    IF EXIST demo.res echo demo.res >> b32.bc


ECHO * 
ECHO Linking demo ...
rem Use these flags to avoid the console window creation
    ilink32 -Gn -aa -Tpe -s @b32.bc > comp.log
    @type comp.log

rem delete temporary files
@del demo.map
@del demo.obj
@del demo.tds
rem @del demo.ppo

IF ERRORLEVEL 1 GOTO LINKERROR
ECHO * Application successfully built

:RES
IF NOT EXIST demo.rc GOTO EXIT
Echo *
echo Adding demo.rc
brc32 -r demo

IF ERRORLEVEL 0 GOTO EXIT
Echo Resource linking problem
GOTO EXIT
ECHO

:LINKERROR
rem if exist meminfo.txt notepad meminfo.txt
GOTO EXIT
:EXIT
IF ERRORLEVEL 1 GOTO END
  Echo Launching demo.exe ...
  demo.exe
  EXIT
:END

