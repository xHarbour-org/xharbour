@echo off
REM $Id: wvwtest9.bat $


SET _BCC55=%BCC55%
SET BCC55=c:\software\borland\bcc55

REM this is the batch file to compile and link wvwtest9.prg together with gtwvw and some other files
REM with the main being wvwtest9.prg
REM (toolbar code sample)

if NOT "%1"=="" goto invalid
goto new

:new
echo. > err
if exist wvwtest9.exe del wvwtest9.exe
if exist wvwtest9.res del wvwtest9.res

brc32 -r wvwtest9.rc -i%BCC55%\include >> err
if not exist wvwtest9.res goto brcfail
goto cont1

:cont1
make -fwvwtest9.bc >> err
if not exist wvwtest9.exe notepad err
if exist wvwtest9.exe echo wvwtest9.exe was successfully created
goto selesai

:invalid
echo this program is for compiling wvwtest9.prg et all
echo usage: wvwtest9 (without parameter)
goto selesai

:brcfail
echo brc fail
goto selesai

:selesai

SET BCC55=%_BCC55%
SET _BCC55=
