SET PATH=%PATH%;C:\Borland\BCC65\Bin
SET XHB_BCC=C:\xHarbour
SET BCCDIR=C:\Borland\BCC65

RD \xbp\bc5\sql.lib /S /Q 

    \xHB\Bin\xBuild.exe sqlbcc65.lib.xbp   -NoXbp %1

REN \xhb\lib\bcc\sql.lib SQLBCC65.lib