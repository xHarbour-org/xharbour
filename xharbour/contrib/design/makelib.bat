rem hbcc.lib creation sample for Bcc 5.5.1
rem change next envvars to meet your environment:
set HBDIR=\hb
rem HBDIR=\harbour
set BCDIR=\bcc32
rem set BCDIR=\bcc55

del %HBDIR%\lib\hbcc.lib
%HBDIR%\bin\harbour /n /l hbcc.prg

%BCDIR%\bin\bcc32 /c /DHB_STATISTICS_OFF /d /OS /O2 /I%HBDIR%\include hbcc.c
%BCDIR%\bin\bcc32 /c /DHB_STATISTICS_OFF /d /OS /O2 /I%HBDIR%\include hbc7.c
%BCDIR%\bin\bcc32 /c /DHB_STATISTICS_OFF /d /OS /O2 /I%HBDIR%\include hbcu.c
%BCDIR%\bin\bcc32 /c /DHB_STATISTICS_OFF /d /OS /O2 /I%HBDIR%\include hbcx.c
%BCDIR%\bin\bcc32 /c /DHB_STATISTICS_OFF /d /OS /O2 /I%HBDIR%\include hbcy.c
%BCDIR%\bin\bcc32 /c /DHB_STATISTICS_OFF /d /OS /O2 /I%HBDIR%\include hbhex.c
%BCDIR%\bin\bcc32 /c /DHB_STATISTICS_OFF /d /OS /O2 /I%HBDIR%\include hbcrc16.c
%BCDIR%\bin\bcc32 /c /DHB_STATISTICS_OFF /d /OS /O2 /I%HBDIR%\include hbcrc32.c
%BCDIR%\bin\bcc32 /c /DHB_STATISTICS_OFF /d /OS /O2 /I%HBDIR%\include hbcrpt32.c

%BCDIR%\bin\tlib %HBDIR%\lib\hbcc.lib +hbcc.obj +hbc7.obj +hbcu.obj +hbcx.obj +hbcy.obj +hbhex.obj +hbcrc16.obj +hbcrc32.obj +hbcrpt32.obj

del hbcc.c
del hbcc.obj
del hbc7.obj
del hbcu.obj
del hbcx.obj
del hbcy.obj
del hbhex.obj
del hbcrc16.obj
del hbcrc32.obj
del hbcrpt32.obj
