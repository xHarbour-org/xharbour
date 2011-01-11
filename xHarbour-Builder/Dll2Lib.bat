@echo off

if NOT EXIST %1.dll GOTO :NOEXIST

xlib %1.dll /out:%1.lib
GOTO OK

:NOEXIST
ECHO Connot find %1.dll
GOTO EXIT

:OK
ECHO Build %1.LIB ok!

:EXIT