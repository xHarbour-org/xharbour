@echo off
rem
rem $Id: makallpc.bat,v 1.1 2005/11/02 18:00:15 modalsist Exp $
rem
rem MakeFile for build xHarbour binaries, core libs and contribs under
rem Pelles C Compiler.
rem


if exist make_pc.bat call make_pc %1
rem if errorlevel 1 goto end

:dll
if exist dll_pc.bat call dll_pc %1
rem if errorlevel 1 goto end

:firebird
echo Uncomment this section if you have Firebird installed
rem cd contrib\firebird
rem if exist make_pc.bat call make_pc %1
rem cd ..\..
rem if errorlevel 1 goto end

:gd
cd contrib\gd
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:gtwvw
cd contrib\gtwvw
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:hbzlib
cd contrib\hbzlib
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:libnf
cd contrib\libnf
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:mysql
echo Uncomment this section if you have mysql installed
rem requires mysql.h to be installed
rem cd contrib\mysql
rem if exist make_pc.bat call make_pc %1
rem cd ..\..
rem if errorlevel 1 goto end

:pdf
cd contrib\pdflib
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:pgsql
echo Uncomment this section if you have pgsql installed
rem cd contrib\pgsql
rem if exist make_pc.bat call make_pc %1
rem cd ..\..
rem if errorlevel 1 goto end

:rdd_ads
cd contrib\rdd_ads
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:tp
cd contrib\tp_
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:unicode
cd contrib\unicode
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:what32
cd contrib\what32
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:wvtgui
cd contrib\wvtgui
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:xwt
cd contrib\xwt
if exist make_pc.bat call make_pc %1
cd ..\..
rem if errorlevel 1 goto end

:end
