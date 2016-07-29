MD \xbp\xHb-Build-Setup\
\xHB\Bin\xBuild.exe xHB-Build-Setup.exe.xbp -all

IF ERRORLEVEL == 0 DEL *.log /Q

IF ERRORLEVEL == 0 DEL *.map /Q

IF ERRORLEVEL == 0 UPX xHB-Build-Setup.exe

pause