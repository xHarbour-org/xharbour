SET PATH=%PATH%;C:\BCC58\Bin
SET XHB_BCC=C:\xHarbour
SET BCCDIR=C:\BCC58

RD \xbp\bc5\sql.lib /S /Q 

    \xHB\Bin\xBuild.exe sql.lib.xbp   -NoXbp %1

REN \xhb\lib\bcc\sql.lib SQLBCC55.lib