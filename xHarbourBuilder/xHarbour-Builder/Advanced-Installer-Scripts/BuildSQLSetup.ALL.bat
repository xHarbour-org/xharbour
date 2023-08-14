    RD C:\xHB-SetupFiles\sqlrdd\ /Q /S

    @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-SQLRDD.bat XHB-RELEASE
    C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-SQLRDD-Setup.exe C XHB-RELEASE

    @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-SQLRDD.bat XHB-DEMO
    C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-SQLRDD-Setup.exe C XHB-DEMO

REM @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-SQLRDD.bat BCC-RELEASE
REM C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-SQLRDD-Setup.exe C BCC-RELEASE

REM @call C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\CopyFiles-SQLRDD.bat BCC-DEMO
REM C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-SQLRDD-Setup.exe C BCC-DEMO

