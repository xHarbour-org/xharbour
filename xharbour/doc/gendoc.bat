@echo off
rem 
rem $Id: genhtm.bat,v 1.4 2003/10/13 03:49:04 lculik Exp $
rem 

if "%1" =="/OS2" goto OS2
if "%1" =="/os2" goto OS2
if "%1" =="/RTF" goto RTF
if "%1" =="/rtf" goto RTF
if "%1" =="/HTM" goto HTM
if "%1" =="/htm" goto HTM
if "%1" =="" goto HELP
ECHO Assembling input files
:NG
hbdoc /ngi rddads.lnk xharbour.rsp
REM Compile the sources
Echo Compiling the sources
Processing Input Files
Copy ngi\overview.ngi .
Compiling Sources
ngxc overview.ngi
Linking the Guide
ngxl rddads.lnk
del *.ngi
del *.ngo
del ngi\*.txt
del ngi\*.ngi
GOTO END
:OS2
   hbdoc /OS2 rddads.lnk xharbour.rsp
GOTO END
:RTF
   hbdoc /RTF rddads.lnk xharbour.rsp
   HCW HARBOUR.HPJ
GOTO END
:HTM
    hbdoc /HTM rddads.lnk xharbour.rsp
   GOTO END
:HELP

ECHO HELP
ECHO GENDOC /HTM to generate .html files
ECHO GENDOC /RTF to generate .rtf files and thos .hlp files
goto END
:END
del ass*.bat

