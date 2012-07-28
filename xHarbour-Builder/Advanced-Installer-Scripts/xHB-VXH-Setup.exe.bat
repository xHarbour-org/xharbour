MD W:\xbp\xHb-VXH-Setup\
W:\xHB\Bin\xBuild.exe xHB-VXH-Setup.exe.xbp %1
IF ERRORLEVEL == 0 DEL *.log /Q
IF ERRORLEVEL == 0 DEL *.map /Q
IF ERRORLEVEL == 0 UPX xHB-VXH-Setup.exe
