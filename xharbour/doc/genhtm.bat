@echo off
rem 
rem $Id: genhtm.bat,v 1.2 2000/04/30 07:10:48 lculik Exp $
rem 

hbdoc -htm genwww.lnk genwww.rsp
cd htm
echo renaming Harbour.htm to index.htm
ren harbour.htm index.htm
del genwww.lnk
ren genwww.old genwww.lnk

