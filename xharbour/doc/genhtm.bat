@echo off
rem 
rem $Id: genhtm.bat,v 1.3 2003/03/29 11:01:09 patrickmast Exp $
rem 

del htm\*.htm
hbdoc -rtf xharbour.lnk xharbour.rsp
cd htm
echo renaming xHarbour.htm to index.htm
ren xharbour.htm index.htm
cd ..
