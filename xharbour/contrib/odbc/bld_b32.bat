@echo off
rem
rem $Id: bld_b32.bat,v 1.4 2000/07/04 02:03:38 vszel Exp $
rem

..\..\bin\harbour %1 /n /i..\include %2
bcc32 -e%1.exe -O2 -d -I..\..\include -L..\..\lib\b32 %1.c debug.lib vm.lib rtl.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib hbodbc.lib odbc32.lib

rem del %1.c
