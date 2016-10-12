    IF "%PROG_PATH%"=="" SET PROG_PATH=%ProgramFiles%
    IF "%XHB_BUILD_DRIVE%" == "" SET XHB_BUILD_DRIVE=C
    IF "%BCCDIR%" == "" SET BCCDIR=C:\BCC55

    SET PATH=%XHB_BUILD_DRIVE%:\xHarbour\bin;%path%

    SET INCLUDE=
    SET LIB=

    xBuild.exe six.lib.xbp        -NoXbp %1
    xBuild.exe six-demo.lib.xbp   -NoXbp %1