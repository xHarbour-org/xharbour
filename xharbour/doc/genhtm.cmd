@echo off
rem 
rem $Id: genhtm.cmd,v 1.1.1.1 2001/12/21 10:48:09 ronpinkas Exp $
rem 

..\bin\b32\hbdoc -htm genhtm.lnk genhtm.rsp
cd htm
echo renaming Harbour.htm to index.htm
ren harbour.htm index.htm