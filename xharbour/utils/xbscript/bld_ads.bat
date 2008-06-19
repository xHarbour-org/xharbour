if "%HB_ADS_IMPLIB%" == "no" goto adslibs
set _ACE32_=ace32.lib

:adslibs
if "%ADS_LIBS%"=="" set ADS_LIBS=rddads.lib %_ACE32_%
set _ACE32_=

set tmpHARBOURFLAGS=%HARBOURFLAGS%
set HARBOURFLAGS=%HARBOURFLAGS% -DADS %1 %2
call bld_b32 xbscript
set HARBOURFLAGS=%tmpHARBOURFLAGS%
set tmpHARBOURFLAGS=

if exist xbscript.exe copy xbscript.exe ..\..\bin\xbscriptADS.exe
if exist xbscript.ppo del xbscript.ppo
if exist xbscript.c   del xbscript.c
if exist xbscript.obj del xbscript.obj
if exist xbscript.tds del xbscript.tds
if exist xbscript.exe del xbscript.exe
