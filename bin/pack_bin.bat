@echo off
rem
rem $Id$
rem

rem RDDADS separate from this, include headers and readme

rem This batch requires "Free ZIP" and/or "TAR" utilities for compression.

rem set hb_architecture=w32
rem set hb_compiler=bcc32

set hb_ver=1.20.01

if "%hb_architecture%" == "linux" GOTO set_linux

set hb_archbin=zip
set hb_archopt=-D -X
set hb_archoptr=-D -X -r
set hb_ext=.zip

GOTO set_file

:SET_LINUX
set hb_archbin=tar
set hb_archopt=-czf --no-recursion
set hb_archoptr=-czf
set hb_ext=.tar.gz

:SET_FILE

set hb_filename=xharbour-%hb_ver%.bin.%hb_architecture%.%hb_compiler%%hb_ext%

if not "%hb_architecture%" == "os2" goto NOT_EMX:
if not "%hb_compiler%"     == "gcc" goto NOT_EMX:

for %%a in (bin\*.exe) do emxbind.exe -s %%a
goto NEXT_STEP

:NOT_EMX

if     "%hb_compiler%"     == "gcc"     strip bin/*
if     "%hb_compiler%"     == "mingw32" strip bin/*
if     "%hb_compiler%"     == "djgpp"   strip bin/*

:NEXT_STEP

if exist %hb_filename% del %hb_filename%

rem README.TXT

if     "%hb_architecture%" == "os2"     %hb_archbin% %hb_archopt%  %hb_filename% bin/bld.cmd
if not "%hb_architecture%" == "os2"     %hb_archbin% %hb_archopt%  %hb_filename% bin/bld.bat
if     "%hb_compiler%"     == "bcc16"   %hb_archbin% %hb_archopt%  %hb_filename% bin/bld_b16*.bat
if     "%hb_compiler%"     == "bcc32"   %hb_archbin% %hb_archopt%  %hb_filename% bin/bld_b32*.bat
if     "%hb_compiler%"     == "msvc"    %hb_archbin% %hb_archopt%  %hb_filename% bin/bld_vc*.bat
if     "%hb_architecture%" == "linux"   %hb_archbin% %hb_archopt%  %hb_filename% bin/*.
if not "%hb_architecture%" == "linux"   %hb_archbin% %hb_archopt%  %hb_filename% bin/harbour.exe
if not "%hb_architecture%" == "linux"   %hb_archbin% %hb_archopt%  %hb_filename% bin/hbmake.exe
if not "%hb_architecture%" == "linux"   %hb_archbin% %hb_archopt%  %hb_filename% bin/hbrun.exe
if not "%hb_architecture%" == "linux"   %hb_archbin% %hb_archopt%  %hb_filename% bin/xbscript.exe
rem if not "%hb_architecture%" == "linux"   %hb_archbin% %hb_archopt%  %hb_filename% bin/*.EXE
                                        %hb_archbin% %hb_archoptr% %hb_filename% doc/*.txt doc/en/*.txt doc/es/*.txt
                                        %hb_archbin% %hb_archopt%  %hb_filename% include/*.api
                                        %hb_archbin% %hb_archopt%  %hb_filename% include/*.ch
                                        %hb_archbin% %hb_archopt%  %hb_filename% include/*.h

                                        %hb_archbin% %hb_archopt%  %hb_filename% COPYING

if     "%hb_compiler%"     == "gcc"     %hb_archbin% %hb_archopt%  %hb_filename% lib/*.a
if     "%hb_compiler%"     == "mingw32" %hb_archbin% %hb_archopt%  %hb_filename% lib/*.a
if     "%hb_compiler%"     == "djgpp"   %hb_archbin% %hb_archopt%  %hb_filename% lib/*.a
if     "%hb_compiler%"     == "rsxnt"   %hb_archbin% %hb_archopt%  %hb_filename% lib/*.a
if     "%hb_compiler%"     == "rsx32"   %hb_archbin% %hb_archopt%  %hb_filename% lib/*.a
if     "%hb_compiler%"     == "watcom"  %hb_archbin% %hb_archopt%  %hb_filename% lib/*.lib
if     "%hb_compiler%"     == "bcc16"   %hb_archbin% %hb_archopt%  %hb_filename% lib/*.lib
if     "%hb_compiler%"     == "bcc32"   %hb_archbin% %hb_archopt%  %hb_filename% lib/*.lib
if     "%hb_compiler%"     == "icc"     %hb_archbin% %hb_archopt%  %hb_filename% lib/*.lib
if     "%hb_compiler%"     == "msvc"    %hb_archbin% %hb_archopt%  %hb_filename% lib/*.lib
if     "%hb_compiler%"     == "pocc"    %hb_archbin% %hb_archopt%  %hb_filename% lib/*.lib

set hb_filename=xharbour-%hb_ver%.log.%hb_architecture%.%hb_compiler%%hb_ext%

if exist %hb_filename% del %hb_filename%

if     "%hb_compiler%"     == "bcc16"   %hb_archbin% %hb_archopt%  %hb_filename% bin/b16/*.tds
if     "%hb_compiler%"     == "bcc32"   %hb_archbin% %hb_archopt%  %hb_filename% bin/b32/*.tds

if     "%hb_compiler%"     == "bcc16"   %hb_archbin% %hb_archopt%  %hb_filename% bin/b16/*.map
if     "%hb_compiler%"     == "bcc32"   %hb_archbin% %hb_archopt%  %hb_filename% bin/b32/*.map
if     "%hb_compiler%"     == "msvc"    %hb_archbin% %hb_archopt%  %hb_filename% bin/vc/*.map
rem if     "%hb_compiler%"     == "pocc"    %hb_archbin% %hb_archopt%  %hb_filename% bin/pc/*.map

if     "%hb_compiler%"     == "bcc16"   %hb_archbin% %hb_archopt%  %hb_filename% make_b16.log
if     "%hb_compiler%"     == "bcc32"   %hb_archbin% %hb_archopt%  %hb_filename% make_b32.log
if     "%hb_compiler%"     == "msvc"    %hb_archbin% %hb_archopt%  %hb_filename% make_vc.log
if     "%hb_compiler%"     == "pocc"    %hb_archbin% %hb_archopt%  %hb_filename% *_pc.log

if     "%hb_compiler%"     == "gcc"     %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log
if     "%hb_compiler%"     == "mingw32" %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log
if     "%hb_compiler%"     == "djgpp"   %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log
if     "%hb_compiler%"     == "rsxnt"   %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log
if     "%hb_compiler%"     == "rsx32"   %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log
