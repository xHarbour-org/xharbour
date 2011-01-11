    RD c:\xHB-SetupFiles\xHB\ /Q /S

REM @call W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles.bat Demo
REM W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-Build-Setup.exe DEMO W

    @call W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles.bat Personal
    W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-Build-Setup.exe PERS W

    @call W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles.bat Professional 
    W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-Build-Setup.exe PROF W

    @call W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles.bat Enterprise
    W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-Build-Setup.exe ENT W