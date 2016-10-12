@echo off

if NOT EXIST %1.dll GOTO :NOEXIST

Xlib %1.dll /MACHINE:IX86 /def:bgd_XCC.def /out:%1.lib
GOTO OK

:NOEXIST
ECHO Connot find %1.dll
GOTO EXIT

:OK
ECHO Build %1.LIB ok!

:EXIT