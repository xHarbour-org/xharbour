    RD C:\xHB-SetupFiles\xHB\ /Q /S

REM @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles.bat Demo
REM C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-Build-Setup.exe DEMO C

    @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles.bat Personal
    C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-Build-Setup.exe PERS C

    @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles.bat Professional 
    C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-Build-Setup.exe PROF C

    @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles.bat Enterprise
    C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-Build-Setup.exe ENT C