@echo off
if '%1' == '' goto error

set _HRB_=d:\xhrb
set _BCCDIR_=C:\BORLAND\BCC55
set _BCC_=C:\BORLAND\BCC55\BIN
set _PATH=%path%
set PATH=%_BCC_%;%PATH%

make -DPRGFILE=%1 -DHRB_DIR=%_HRB_% -DCOMPILERDIR=%_BCCDIR_% -fdll.mak /B %2 %3

set PATH=%_path%
set _PATH=
set _HRB_=
set _BCCDIR_
set _BCC_=

if exist %1.c del %1.c
if exist %1.obj del %1.obj
if exist %1.bak del %1.bak
if exist %1.tds del %1.tds
if exist %1.ilc del %1.ilc
if exist %1.ild del %1.ild
if exist %1.ilf del %1.ilf
if exist %1.ils del %1.ils
if exist %1.exe dir %1.exe
if exist %1.exe echo.
if exist %1.exe echo %1.exe succesfully built
if exist %1.exe echo.
goto end

:error
Echo Syntax DLL [PROGRAM] // WITHOUT PRG EXTENSION
goto end

:end
