    RD c:\xHB-SetupFiles\vxh\ /Q /S

REM @call W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-VXH.bat Demo
REM W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH-Setup.exe DEMO W

    @call W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-VXH.bat Personal
    W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH-Setup.exe PERS W

    @call W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-VXH.bat Professional
    W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH-Setup.exe PROF W

    @call W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-VXH.bat Enterprise
    W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH-Setup.exe ENT W