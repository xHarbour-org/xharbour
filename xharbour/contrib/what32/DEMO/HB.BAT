ECHO OFF
ECHO *
ECHO Compiling... %1
del comp.log

rem *** Create C source if PRG exists
IF NOT EXIST %1.prg  GOTO MAKEOBJ

rem harbour %1 /w /n /m /gc0 /es2 /i%HB_INC_INSTALL% > comp.log
harbour %1 /w /p /n /m /gc0 /es2 /i%HB_INC_INSTALL%;include /o%1 >> comp.log
@type comp.log

IF ERRORLEVEL 1 GOTO EXIT

:MAKEOBJ
rem *** Create OBJ from C

bcc32 -DHB_API_MACROS -M -c -O2 -e%1 -I%HB_INC_INSTALL% %1.c > comp.log
@type comp.log

IF ERRORLEVEL 1 GOTO EXIT

IF NOT EXIST %1.prg  GOTO DONE
@del %1.c

:DONE

echo %1.obj + >> b32.bc
:EXIT

