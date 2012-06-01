SET XHB_BCC=C:\xHarbour
SET BCCDIR=C:\Borland\BCC55

RD \xbp\bc5\demo\sql.lib /S /Q 

    \xHB\Bin\xBuild.exe sql.lib.xbp   -NoXbp %1

REN \xhb\lib\bcc\sql.lib SQLBDEMOCC55.lib
