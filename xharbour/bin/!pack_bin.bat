@echo off
rem
rem $Id: !pack_bin.bat,v 1.5 2002/02/01 01:05:58 ronpinkas Exp $
rem

rem RDDADS separate from this, include headers and readme

rem This batch requires "Free ZIP" and/or "TAR" utilities for compression.

rem set hb_architecture=w32
rem set hb_compiler=bcc32

set hb_ver=0.72.0

if not "%hb_architecture%" == "linux"   set hb_archbin=zip
if not "%hb_architecture%" == "linux"   set hb_archopt=-D -X
if not "%hb_architecture%" == "linux"   set hb_archoptr=-D -X -r
if not "%hb_architecture%" == "linux"   set hb_ext=.zip
if     "%hb_architecture%" == "linux"   set hb_archbin=tar
if     "%hb_architecture%" == "linux"   set hb_archopt=-czf --no-recursion
if     "%hb_architecture%" == "linux"   set hb_archoptr=-czf
if     "%hb_architecture%" == "linux"   set hb_ext=.tar.gz

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
if not "%hb_architecture%" == "linux"   %hb_archbin% %hb_archopt%  %hb_filename% bin/*.exe
if not "%hb_architecture%" == "linux"   %hb_archbin% %hb_archopt%  %hb_filename% bin/*.EXE
                                        %hb_archbin% %hb_archoptr% %hb_filename% doc/*.txt doc/en/*.txt
                                        %hb_archbin% %hb_archoptr% %hb_filename% doc/*.txt doc/es/*.txt
                                        %hb_archbin% %hb_archopt%  %hb_filename% include/*.api
                                        %hb_archbin% %hb_archopt%  %hb_filename% include/*.ch
                                        %hb_archbin% %hb_archopt%  %hb_filename% include/*.h
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

set hb_filename=harbour-%hb_ver%.log.%hb_architecture%.%hb_compiler%%hb_ext%

if exist %hb_filename% del %hb_filename%

if     "%hb_compiler%"     == "bcc16"   %hb_archbin% %hb_archopt%  %hb_filename% bin/b16/*.tds
if     "%hb_compiler%"     == "bcc32"   %hb_archbin% %hb_archopt%  %hb_filename% bin/b32/*.tds
if     "%hb_compiler%"     == "msvc"    %hb_archbin% %hb_archopt%  %hb_filename% bin/vc/*.tds

if     "%hb_compiler%"     == "bcc16"   %hb_archbin% %hb_archopt%  %hb_filename% bin/b16/*.map
if     "%hb_compiler%"     == "bcc32"   %hb_archbin% %hb_archopt%  %hb_filename% bin/b32/*.map
if     "%hb_compiler%"     == "msvc"    %hb_archbin% %hb_archopt%  %hb_filename% bin/vc/*.map

if     "%hb_compiler%"     == "bcc16"   %hb_archbin% %hb_archopt%  %hb_filename% make_b16.log
if     "%hb_compiler%"     == "bcc32"   %hb_archbin% %hb_archopt%  %hb_filename% make_b32.log
if     "%hb_compiler%"     == "msvc"    %hb_archbin% %hb_archopt%  %hb_filename% make_vc.log

if     "%hb_compiler%"     == "gcc"     %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log
if     "%hb_compiler%"     == "mingw32" %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log
if     "%hb_compiler%"     == "djgpp"   %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log
if     "%hb_compiler%"     == "rsxnt"   %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log
if     "%hb_compiler%"     == "rsx32"   %hb_archbin% %hb_archopt%  %hb_filename% make_gnu.log

