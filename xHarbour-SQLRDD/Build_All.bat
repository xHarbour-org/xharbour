    IF "%PROG_PATH%"=="" SET PROG_PATH=%ProgramFiles%
    IF "%XHB_BUILD_DRIVE%" == "" SET XHB_BUILD_DRIVE=C

    SET PATH=%XHB_BUILD_DRIVE%:\djgpp\bin\;%XHB_BUILD_DRIVE%:\xHarbour\bin;%path%
    SET DJGPP=%XHB_BUILD_DRIVE%:\djgpp\djgpp.env

    SET INCLUDE=
    SET LIB=

    BISON > NUL:

    xBuild.exe libsqlrdd.a.xbp                 -x\xhb -cXCC=\xhb -NoXbp %1
    xBuild.exe sql-demo.a.xbp                  -x\xhb -cXCC=\xhb -NoXbp %1
