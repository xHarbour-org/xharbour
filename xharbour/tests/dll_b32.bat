@ECHO OFF
IF '%1' == '' GOTO Error

IF '%BCC_DIR%' == '' SET BCC_DIR=C:\BCC55
IF '%HRB_DIR%' == '' SET HRB_DIR=..

make -DPRGFILE=%1 -fdll_b32.mak /B %2 %3

REM if exist %1.c del %1.c

if exist %1.obj del %1.obj
if exist %1.bak del %1.bak
if exist %1.tds del %1.tds
if exist %1.ilc del %1.ilc
if exist %1.ild del %1.ild
if exist %1.ilf del %1.ilf
if exist %1.ils del %1.ils

REM if exist %1.c del %1.map

if exist %1.exe dir %1.exe
if exist %1.exe echo.
if exist %1.exe echo %1.exe succesfully built
if exist %1.exe echo.

GOTO End

:Error
Echo Syntax DLL [PROGRAM] // WITHOUT PRG EXTENSION

:End
