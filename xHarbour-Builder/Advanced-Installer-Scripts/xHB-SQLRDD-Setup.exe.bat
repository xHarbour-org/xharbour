MD \xbp\xHb-SQLRDD-Setup\
\xHB\Bin\xBuild.exe xHB-SQLRDD-Setup.exe.xbp %1
IF ERRORLEVEL == 0 DEL *.log /Q
IF ERRORLEVEL == 0 DEL *.map /Q
IF ERRORLEVEL == 0 UPX xHB-SQLRDD-Setup.exe
