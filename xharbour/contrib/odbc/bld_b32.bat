@echo off
rem
rem $Id: bld_b32.bat,v 1.1 2004/01/13 01:16:13 ronpinkas Exp $
rem

..\..\bin\harbour %1 /n /i..\include %2
bcc32 -e%1.exe -O2 -d -I..\..\include -L..\..\lib\b32 %1.c debug.lib vm.lib rtl.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfdbt.lib dbfcdx.lib dbffpt.lib common.lib hbodbc.lib odbc32.lib

rem del %1.c
