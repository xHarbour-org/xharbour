@echo off
rem
rem $Id: makallbc.bat,v 1.18 2005/10/06 05:48:09 marcosgambeta Exp $
rem

echo create system files
call make_b32 %1

echo harbour.dll
call dll_b32 %1
if errorlevel 1 goto end

:firebird
echo firebird
echo Uncomment this section if you have Firebird installed
rem cd contrib\firebird
rem if exist make_b32.bat call make_b32.bat %1
rem cd ..\..
rem if errorlevel 1 goto end

:hbzlib
echo hbzip
cd contrib\hbzlib
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:libnf
echo libnf
cd contrib\libnf
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:mysql
echo mysql
echo Uncomment this section if you have mysql installed
rem requires mysql.h to be installed
rem cd contrib\mysql
rem call make_b32.bat %1
rem cd ..\..
if errorlevel 1 goto end

:pgsql
echo pgsql
echo Uncomment this section if you have pgsql installed
rem cd contrib\pgsql
rem if exist make_b32.bat call make_b32.bat %1
rem cd ..\..
rem if errorlevel 1 goto end

:pdf
echo pdf
cd contrib\pdflib
if exist make_b32.bat call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:rdd_ads
echo rdd_ads
cd contrib\rdd_ads
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:tp
echo tp
cd contrib\tp_
if exist make_b32.bat call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:unicode
echo unicode
cd contrib\unicode
if exist make_b32.bat call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:what32
echo what32
cd contrib\what32
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:xwt
echo xwt
cd contrib\xwt
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:gtwvw
echo gtwvw
cd contrib\gtwvw
call make_b32.bat %1
cd ..\..

:wvtgui
echo wvtgui
cd contrib\wvtgui
call make_b32.bat %1
cd ..\..

:gd
echo gd
cd contrib\gd
call make_b32.bat %1
cd ..\..

:end
