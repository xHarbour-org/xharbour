@echo off
rem
rem $Id: makallvc.bat,v 1.21 2008/03/19 00:17:32 ronpinkas Exp $
rem

echo creating system files
call make_vc.bat %1
if errorlevel 1 goto error

echo harbour.dll
call dll_vc %1
if errorlevel 1 goto error

:firebird
echo firebird
echo Uncomment this section if you have firebird installed
rem cd contrib\firebird
rem if exist make_vc.bat call make_vc.bat %1
rem cd ..\..
rem if errorlevel 1 goto error

:gd
echo gd
cd contrib\gd
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto error

:gtwvw
REM echo gtwvw
REM cd contrib\gtwvw
REM call make_vc.bat %1
REM cd ..\..
REM if errorlevel 1 goto error

:hbzlib
echo hbzip
cd contrib\hbzlib
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto error

:libnf
echo libnf
cd contrib\libnf
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto error

:mysql
echo mysql
echo Uncomment this section if you have mysql installed
rem cd contrib\mysql
rem if exist make_vc.bat call make_vc.bat %1
rem cd ..\..
rem if errorlevel 1 goto error

:pgsql
echo pgsql
echo Uncomment this section if you have pgsql installed
rem cd contrib\pgsql
rem if exist make_vc.bat call make_vc.bat %1
rem cd ..\..
rem if errorlevel 1 goto error

:pdf
echo pdf
cd contrib\pdflib
if exist make_vc.bat call make_vc.bat %1
cd ..\..
if errorlevel 1 goto error

:rdd_ads
echo rdd_ads
cd contrib\rdd_ads
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto error

:tp
echo tp
cd contrib\tp_
if exist make_vc.bat call make_vc.bat %1
cd ..\..
if errorlevel 1 goto error

:unicode
echo unicode
cd contrib\unicode
if exist make_vc.bat call make_vc.bat %1
cd ..\..
if errorlevel 1 goto error

:what32
echo what32
cd contrib\what32
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto error

:xwt
echo xwt
cd contrib\xwt
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto error

goto end

:error
echo Build Failed! Please review log files.
:end
