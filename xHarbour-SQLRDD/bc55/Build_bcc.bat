SET PATH=%PATH%;C:\Borland\BCC55\Bin
SET XHB_BCC=C:\xHarbour
SET BCCDIR=C:\Borland\BCC55

RD \xbp\bc5\sql.lib /S /Q 

    \xHB\Bin\xBuild.exe sqlbcc55.lib.xbp   -NoXbp %1

REN \xhb\lib\bcc\sql.lib SQLBCC55.lib