@echo off
rem 
rem $Id: genhtm.bat,v 1.2 2003/01/10 18:26:07 patrickmast Exp $
rem 

del htm\*.htm
..\bin\b32\hbdoc -htm genhtm.lnk genhtm.rsp
cd htm
echo renaming Harbour.htm to index.htm
ren harbour.htm index.htm
cd ..