@echo off
rem
rem $Id: bld_b32.bat,v 1.4 2000/04/14 20:01:21 vszel Exp $
rem

..\..\bin\harbour hscript /n /i..\..\include
bcc32 -O2 -I..\..\include -L..\..\lib -ehscript.exe hscript.c debug.lib vm.lib rtl.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib
rem del hscript.c
