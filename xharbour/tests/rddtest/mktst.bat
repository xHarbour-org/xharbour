@echo off
clipper rddmktst /m/n/w/es2
if errorlevel 1 goto error
exospace fi rddmktst lib _dbfcdx, dbfcdx
if errorlevel 1 goto error
del *.obj
rddmktst out.prg
rem goto error
clipper out /m/n/w/es2
if errorlevel 1 goto error
exospace fi out lib _dbfcdx, dbfcdx

:error
del *.obj
