
    IF "%XHB_BCC%" == "" SET XHB_BCC=C:\xHarbour
    IF "%BCCDIR%" == "" SET BCCDIR=C:\Borland\BCC55

    MD C:\xharbour\xbp\obj-bc55\sql.lib
    MD C:\xharbour\xbp\obj-demo-bc55\sql-demo.lib

    SET PATH=C:\xHarbour\bin

    SET INCLUDE=
    SET LIB=

    \xHB\Bin\xBuild.exe sqlbcc55.lib.xbp      -NoXbp %1
rem    \xHB\Bin\xBuild.exe sqlmt.lib.xbp    -NoXbp %1
