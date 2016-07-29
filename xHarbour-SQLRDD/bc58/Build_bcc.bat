SET PATH=%PATH%;C:\Borland\BCC58\Bin
SET XHB_BCC=C:\xHarbour
SET BCCDIR=C:\Borland\BCC58

RD \xbp\bc5\sql.lib /S /Q 

    \xHB\Bin\xBuild.exe sqlbcc58.lib.xbp   -NoXbp %1

REN \xhb\lib\bcc\sql.lib SQLBCC58.lib