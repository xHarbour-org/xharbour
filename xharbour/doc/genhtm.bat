@echo off
rem 
rem $Id: genhtm.bat,v 1.4 2003/10/13 03:49:04 lculik Exp $
rem 

del htm\*.htm
..\bin\hbdoc -htm xharbour.lnk xharbour.rsp
cd htm
echo renaming xHarbour.htm to index.htm
ren xharbour.htm index.htm
cd ..
