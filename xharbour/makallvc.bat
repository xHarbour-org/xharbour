@echo off
rem 
rem $Id: makallvc.bat,v 1.5 2003/09/23 21:36:51 paultucker Exp $
rem 

echo creating system files
call make_vc %1

echo harbour.dll
call dll_vc %1
if errorlevel 1 goto end

:hbzlib
echo hbzip
cd contrib\hbzlib
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:htmllib
echo htmllib
cd contrib\htmllib
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:internet
echo internet
cd contrib\internet
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:libct
echo libct
cd contrib\libct
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:libmisc
echo libmisc
cd contrib\libmisc
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:mysql
rem echo mysql
rem requires mysql.h to be installed
rem cd contrib\mysql
rem call make_vc.bat %1
rem cd ..\..
if errorlevel 1 goto end

:rdd_ads
echo rdd_ads
cd contrib\rdd_ads
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:tip
echo tip
cd contrib\tip
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:what32
echo what32
cd contrib\what32
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:whoo
echo whoo
cd contrib\Whoo
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:woopgui
echo WoopGui
cd contrib\WoopGUI
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:xVisual
echo xVisual
cd contrib\xVisual
call make_vc.bat %1
cd ..\..
if errorlevel 1 goto end

:xwt
echo xwt
cd contrib\xwt
call make_vc.bat %1
cd ..\..
:end
