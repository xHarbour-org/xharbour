
@echo off

if exist error.log del error.log

set HB_INSTALL=..\..
set WHOO=..\whoo
set BCC_DIR=c:\bcc55
set WHAT32=..\what32
ECHO Compiling...

%HB_INSTALL%\bin\harbour %1 -es2 -p -n -w -i%HB_INSTALL%\include;%WHAT32%\include > comp.log
IF ERRORLEVEL 1 type comp.log
IF ERRORLEVEL 1 PAUSE
if errorlevel 1 goto exit

IF EXIST %1.rc %BCC_DIR%\BIN\brc32 -r %1
if errorlevel 1 goto exit

echo -O2 -tW -M -DHB_API_MACROS -DHB_STACK_MACROS -DHB_OS_WIN_32 -DHB_FM_STATISTICS_OFF > b32.bc
echo -I%HB_INSTALL%\include;%BCC_DIR%\include >> b32.bc
echo -c %1.c >> b32.bc
%BCC_DIR%\BIN\bcc32 @b32.bc
if errorlevel 1 goto exit

echo %BCC_DIR%\lib\c0w32.obj + > b32.bc
echo %1.obj, + >> b32.bc
echo %1.exe, + >> b32.bc
echo %1.map, + >> b32.bc
echo %WHAT32%\lib\What32.lib + >> b32.bc
echo %WHOO%\lib\whoo.lib  + >> b32.bc
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
echo %HB_INSTALL%\lib\bcc640.lib + >> b32.bc

rem Uncomment these two lines to use Advantage RDD
rem echo %hdir%\lib\rddads.lib + >> b32.bc
rem echo %hdir%\lib\ace32.lib + >> b32.bc

echo %BCC_DIR%\lib\cw32.lib + >> b32.bc
echo %BCC_DIR%\lib\import32.lib, >> b32.bc

IF EXIST %1.res echo %1.res >> b32.bc

ECHO *
ECHO Linking...
%BCC_DIR%\bin\ilink32 -Gn -Tpe -aa -L%HB_INSTALL%\lib\;%BCC_DIR%\lib\;%WHOO%\lib\ -s @b32.bc

if errorlevel 1 goto exit

del *.tds
del *.c
del *.map
del *.obj

del comp.log

if exist %1.exe %1

:exit
