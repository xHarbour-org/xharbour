MD W:\xbp\xHb-Build-Setup\
W:\xHB\Bin\xBuild.exe xHB-Build-Setup.exe.xbp %1
IF ERRORLEVEL == 0 DEL *.log /Q
IF ERRORLEVEL == 0 DEL *.map /Q
IF ERRORLEVEL == 0 UPX xHB-Build-Setup.exe
