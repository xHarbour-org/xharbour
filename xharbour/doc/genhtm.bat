@echo off
rem 
rem $Id$
rem 

del htm\*.htm
..\bin\hbdoc -htm xharbour.lnk xharbour.rsp
cd htm
echo renaming xHarbour.htm to index.htm
ren xharbour.htm index.htm
cd ..
