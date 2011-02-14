@echo off
rem 
rem $Id$
rem 

..\bin\b32\hbdoc -htm genhtm.lnk genhtm.rsp
cd htm
echo renaming Harbour.htm to index.htm
ren harbour.htm index.htm