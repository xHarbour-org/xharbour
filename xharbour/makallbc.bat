@echo off
rem 
rem $Id: makallbc.bat,v 1.5 2003/10/01 17:44:49 paultucker Exp $
rem 

echo create system files
call make_b32 %1

echo harbour.dll
call dll_b32 %1
if errorlevel 1 goto end

:start
echo hbzip
cd contrib\hbzlib
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:htmllib
echo htmllib
cd contrib\htmllib
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:internet
echo internet
cd contrib\internet
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:libct
echo libct
cd contrib\libct
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:libmisc
echo libmisc
cd contrib\libmisc
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
rem echo mysql
rem requires mysql.h to be installed
rem cd contrib\mysql
rem call make_b32.bat %1
rem cd ..\..
if errorlevel 1 goto end

:rdd_ads
echo rdd_ads
cd contrib\rdd_ads
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:tip
echo tip
cd contrib\tip
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:what32
echo what32
cd contrib\what32
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:whoo
echo whoo
cd contrib\Whoo
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:woopgui
echo WoopGui
cd contrib\WoopGUI
call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:xVisual
echo xVisual
cd contrib\xVisual
if exist make_b32.bat call make_b32.bat %1
cd ..\..
if errorlevel 1 goto end

:xwt
echo xwt
cd contrib\xwt
call make_b32.bat %1
cd ..\..
:end
