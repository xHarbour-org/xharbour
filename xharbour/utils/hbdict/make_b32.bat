@echo off
rem
rem $Id: make_b32.bat,v 1.1 2003/06/24 02:17:21 fsgiudice Exp $
rem

rem ---------------------------------------------------------------
rem This is a generic template file, if it doesn't fit your own needs
rem please DON'T MODIFY IT.
rem
rem Instead, make a local copy and modify that one, or make a call to
rem this batch file from your customized one. [vszakats]
rem ---------------------------------------------------------------

SET HB_MT=
SET HB_GT_LIB=gtwin

call %HB_BIN_INSTALL%\bld.bat hbdict

if errorlevel 1 goto exit
copy hbdict.exe %HB_BIN_INSTALL%
copy i18n\it_IT.hit %HB_BIN_INSTALL%\hbdict_it_IT.hit

exit:

