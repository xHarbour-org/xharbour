DEL log.txt

DEL pegged.exe

SET HARBOUR_INSTALL=..\..\
SET WHAT32_INSTALL=..\..\contrib\what32

:: prg -> c

%HARBOUR_INSTALL%\bin\harbour pegged -n -p -w -i%HARBOUR_INSTALL%\include;%WHAT32_INSTALL%\include %HARBOURFLAGS% >> log.txt

:: c -> obj

bcc32 -c -O2 -tW -M -I%HARBOUR_INSTALL%\include;%WHAT32_INSTALL%\include pegged.c >> log.txt

:: rc -> res

IF EXIST pegged.rc brc32 -r pegged >> log.txt

:: create file b32.bc

ECHO c0w32.obj + > b32.bc
ECHO pegged, + >> b32.bc
ECHO pegged.exe, + >> b32.bc
ECHO pegged.map, + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\rtl.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\vm.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\gtwin.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\lang.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\codepage.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\macro.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\rdd.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\dbfntx.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\dbfdbt.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\dbfcdx.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\dbffpt.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\common.lib + >> b32.bc
::ECHO %HARBOUR_INSTALL%\lib\debug.lib + >> b32.bc
::echo %HARBOUR_INSTALL%\lib\pp.lib + >> b32.bc
::echo %HARBOUR_INSTALL%\lib\samples.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\ct.lib + >> b32.bc
ECHO %HARBOUR_INSTALL%\lib\what32.lib + >> b32.bc
ECHO cw32.lib + >> b32.bc
ECHO import32.lib, >> b32.bc
IF EXIST pegged.res ECHO pegged.res >> b32.bc

:: create pegged.exe

ilink32 -Gn -Tpe -aa @b32.bc >> log.txt

:: delete temporary files

DEL *.tds
DEL *.c
DEL pegged.map
DEL *.obj
DEL b32.bc

