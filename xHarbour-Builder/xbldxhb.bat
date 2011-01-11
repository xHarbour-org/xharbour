SET DJGPP=C:\DJGPP\DJGPP.ENV
BISON

xcopy G:\xhb\*.xbp /u

xcopy \progra~1\pellesc\bin\*.exe   bin /u /d
xcopy \progra~1\pellesc\lib\*.lib   c_lib /d /s
xcopy \progra~1\pellesc\include\*.* c_include /d /s

xcopy \progra~1\pellesc\bin\*.exe   \xhb\bin /u /d
xcopy \progra~1\pellesc\lib\*.lib   \xhb\c_lib /d /s
xcopy \progra~1\pellesc\include\*.* \xhb\c_include /d /s

rem DEL \xhb\include
rem DEL \xhb\lib

xcopy include\*.api \xhb\include\*.api /d /s
xcopy include\*.ch  \xhb\include\*.ch /d /s
xcopy include\*.h   \xhb\include\*.h /d /s

REM ---------
REM Compiler
REM ---------

xbuild.exe xhb.exe.xbp   -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xcopy xbp\release\bin\xhb.exe bin /d
xcopy xbp\release\bin\xhb.exe \xhb\bin /d

REM ---------
REM Debug Compiler
REM ---------

xbuild.exe xhbd.exe.xbp   -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xcopy xbp\debug\bin\xhbd.exe bin /d
xcopy xbp\debug\bin\xhbd.exe \xhb\bin /d

REM ---------
REM ST
REM ---------

xbuild.exe xhb.lib.xbp   -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe nordd.lib.xbp -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe dbf.lib.xbp   -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe ntx.lib.xbp   -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe cdx.lib.xbp   -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe dllmain.lib.xbp -x\xharbour -NoXbp %1 %2 %3 %4 %5

xbuild.exe xhbdll.dll.xbp -x\xharbour -NoXbp %1 %2 %3 %4 %5

REM ---------
REM MT
REM ---------

xbuild.exe xhbmt.lib.xbp   -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe dbfmt.lib.xbp   -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe ntxmt.lib.xbp   -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe cdxmt.lib.xbp   -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe xhbmtdll.dll.xbp -x\xharbour -NoXbp %1 %2 %3 %4 %5

xcopy xbp\release\lib\*.lib lib /d
xcopy xbp\release\lib\*.lib \xhb\lib /d

xcopy xbp\release\lib\*.dll \xhb\dll /d

REM ---------
REM Debug
REM ---------

xbuild.exe xhbd.lib.xbp   -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe norddd.lib.xbp -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe dbfd.lib.xbp   -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe ntxd.lib.xbp   -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe cdxd.lib.xbp   -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 


xbuild.exe dllmaind.lib.xbp -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5

xbuild.exe xhbddll.dll.xbp  -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5


REM ---------
REM Debug  MT
REM ---------

xbuild.exe xhbdmt.lib.xbp  -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe dbfdmt.lib.xbp  -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe ntxdmt.lib.xbp  -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe cdxdmt.lib.xbp  -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5 

xbuild.exe xhbdmtdll.dll.xbp -Debug -x\xharbour -NoXbp %1 %2 %3 %4 %5

xcopy xbp\debug\lib\*.lib lib /d
xcopy xbp\debug\lib\*.lib \xhb\lib /d

xcopy xbp\debug\lib\*.dll \xhb\dll /d

xcopy \xhb\dll\xhb*.dll tests /d