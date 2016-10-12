    RD c:\xHB-SetupFiles\vxh\ /Q /S

REM @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-VXH.bat Demo
REM C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH-Setup.exe DEMO C

    @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-VXH.bat Personal
    C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH-Setup.exe PERS C

    @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-VXH.bat Professional
    C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH-Setup.exe PROF C

    @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-VXH.bat Enterprise
    C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH-Setup.exe ENT C