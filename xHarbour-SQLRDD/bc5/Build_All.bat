    MD %XHB_BUILD_DRIVE%:\xharbour\xbp\obj-bc5\sql.lib
    MD %XHB_BUILD_DRIVE%:\xharbour\xbp\obj-demo-bc5\sql-demo.lib
    MD %XHB_BUILD_DRIVE%:\xharbour\xbp\obj-demo-bc5\mysql-demo.lib

    IF "%PROG_PATH%"=="" SET PROG_PATH=%ProgramFiles%
    IF "%XHB_BUILD_DRIVE%" == "" SET XHB_BUILD_DRIVE=C
    IF "%BCCDIR%" == "" SET BCCDIR=C:\BCC55

    SET PATH=%XHB_BUILD_DRIVE%:\xHarbour\bin;"C:\Program Files\GnuWin32\bin"

    SET INCLUDE=
    SET LIB=

    REM BISON > NUL:

    \xHB\Bin\xBuild.exe sql.lib.xbp                     -NoXbp %1
    \xHB\Bin\xBuild.exe sql-demo.lib.xbp                -NoXbp %1
    \xHB\Bin\xBuild.exe sqlmt.lib.xbp                   -NoXbp %1

    \xHB\Bin\xBuild.exe mysql.lib.xbp                   -NoXbp %1
    \xHB\Bin\xBuild.exe mysql-demo.lib.xbp              -NoXbp %1
    \xHB\Bin\xBuild.exe mysqlmt.lib.xbp                 -NoXbp %1
