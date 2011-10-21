
CD \xHarbour

    IF NOT EXIST "%_XHB_BIN%\Demo"         MD "%_XHB_BIN%\Demo"
    IF NOT EXIST "%_XHB_LIB%\Demo"         MD "%_XHB_LIB%\Demo"
    IF NOT EXIST "%_XHB_BIN%\Personal"     MD "%_XHB_BIN%\Personal"
    IF NOT EXIST "%_XHB_LIB%\Personal"     MD "%_XHB_LIB%\Personal"
    IF NOT EXIST "%_XHB_BIN%\Professional" MD "%_XHB_BIN%\Professional"
    IF NOT EXIST "%_XHB_LIB%\Professional" MD "%_XHB_LIB%\Professional"
    IF NOT EXIST "%_XHB_DLL%"              MD "%_XHB_DLL%"
    IF NOT EXIST "%_XHARBOUR_BIN%"         MD "%_XHARBOUR_BIN%"
    IF NOT EXIST "%_XHARBOUR_XBP%"         MD "%_XHARBOUR_XBP%"
    IF NOT EXIST "%_XHARBOUR_XBP%\lib\vc8" MD "%_XHARBOUR_XBP%\lib\vc8"
    IF NOT EXIST "%_XHARBOUR_XBP_DEMO%"    MD "%_XHARBOUR_XBP_DEMO%"
    IF NOT EXIST "%_XHARBOUR_XBP_PERS%"    MD "%_XHARBOUR_XBP_PERS%"

IF "%_XB_Compiler%"=="vc8" XCOPY \xHarbour\bin\vc\harbour.dll  \xHb\bin\vc8 /d /y /i
IF "%_XB_Compiler%"=="bc5" XCOPY \xHarbour\bin\b32\harbour.dll \xHb\bin\bc5 /d /y /i

IF "%_XB_Compiler%"=="xcc" GOTO No_MakeFolders
   IF NOT EXIST \xhb\bin\%_XB_Compiler% MD \xhb\bin\%_XB_Compiler%
   IF NOT EXIST \xhb\dll\%_XB_Compiler% MD \xhb\dll\%_XB_Compiler%
   IF NOT EXIST \xhb\lib\%_XB_Compiler% MD \xhb\lib\%_XB_Compiler%
:No_MakeFolders



REM  ===============================================
REM  ===============================================
ECHO XCC Linker
REM  ===============================================
REM  ===============================================

      IF NOT "%_XB_Compiler%"=="xcc" GOTO No_xLink
        CD \xHarbour.com\xHarbour-XCC\xlink
        CALL g.bat
     :No_xLink



REM  ===============================================
REM  ===============================================
ECHO Compiler
REM  ===============================================
REM  ===============================================

     IF "%_BUILD_XHB.EXE%"=="NO" GOTO No_Compiler

        CD \xHarbour
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\xHB.exe.xbp" "\xHarbour\xHB.exe.xbp" /y
        COPY "\xHarbour.com\xHarbour-Builder\stxhb.files.inc"            "\xHarbour\stxhb.files.inc" /y

        \xhb\bin\XBUILD.EXE xHB.exe.xbp %_XB_Debug% %1

        IF "%_BUILD_DEMO%"=="NO" GOTO No_xhbexe_Demo
           COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\Demo\xHB.exe.xbp" "\xHarbour\xhb-Demo.exe.xbp" /y
           \xhb\bin\XBUILD.EXE xHB-Demo.exe.xbp %_XB_Debug% %1
           IF EXIST "%_XHB_BIN%\Demo\xHB.exe"  DEL "%_XHB_BIN%\Demo\xHB.exe"  /Q
           RENAME "%_XHB_BIN%\Demo\xHB-Demo.exe" "xHB.exe"
        :No_xhbexe_Demo

     :No_Compiler



REM  ===============================================
REM  ===============================================
ECHO CORE LIB's
REM  ===============================================
REM  ===============================================

     IF "%_BUILD_CORE%"=="NO" GOTO No_Core


        REM ===================
        REM Single Thread LIB's
        REM ===================

        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\xhb.lib.xbp"    "\xHarbour\xhb.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\dbf.lib.xbp"    "\xHarbour\dbf.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\ntx.lib.xbp"    "\xHarbour\ntx.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\cdx.lib.xbp"    "\xHarbour\cdx.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\nsx.lib.xbp"    "\xHarbour\nsx.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\sixcdx.lib.xbp" "\xHarbour\sixcdx.lib.xbp" /y

        COPY "\xHarbour.com\xHarbour-Builder\stdbf.files.inc"  "\xHarbour\stdbf.files.inc"  /y
        COPY "\xHarbour.com\xHarbour-Builder\stdlib.files.inc" "\xHarbour\stdlib.files.inc" /y

        CD "\xHarbour"
        IF "%_BUILD_XHB.EXE%"=="NO" GOTO No_XHBLIB
        \xhb\bin\XBUILD.EXE xHB.lib.xbp           %_XB_Debug% %1
        :No_XHBLIB
        \xhb\bin\XBUILD.EXE DBF.lib.xbp           %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE NTX.lib.xbp           %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE CDX.lib.xbp           %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE NSX.lib.xbp           %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE SixCDX.lib.xbp        %_XB_Debug% %1

        IF "%_BUILD_DEMO%"=="NO" GOTO No_xhblib_Demo
           COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\Demo\xhb.lib.xbp" "\xHarbour\xhb-Demo.lib.xbp" /y
           \xhb\bin\XBUILD.EXE xHB-demo.lib.xbp %_XB_Debug% %1
           IF EXIST "%_XHB_LIB%\Demo\xHB.lib" DEL "%_XHB_LIB%\Demo\xhb.lib" /Q
           RENAME "%_XHB_LIB%\Demo\xHB-Demo.lib" "xhb.lib"
        :No_xhblib_Demo

        CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
        \xhb\bin\XBUILD.EXE noRDD.lib.xbp %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE OptG.lib.xbp  %_XB_Debug% %1

        IF "%_BUILD_XHB.DLL%"=="NO" GOTO No_XHBDLL
           \xhb\bin\XBUILD.EXE dMain.lib.xbp  %_XB_Debug% %1
           \xhb\bin\XBUILD.EXE UseDll.lib.xbp %_XB_Debug% %1

           IF     EXIST "%_XHB_BIN%\xhbdll.lib"       DEL "%_XHB_BIN%\xhbdll.lib" /Q
           IF     EXIST "%_XHB_LIB%\xhbdll.lib"            \xhb\bin\XBUILD.EXE xHBDll.dll.xbp %_XB_NonDebug% %1
           IF NOT EXIST "%_XHB_LIB%\xhbdll.lib"            \xhb\bin\XBUILD.EXE xHBDll.dll.xbp %_XB_NonDebug% %1 -link
           IF     EXIST "%_XHB_BIN%\xhbdll.lib"   MOVE /Y "%_XHB_BIN%\xhbdll.lib" "%_XHB_LIB%"

           IF     EXIST "%_XHB_BIN%\xhbddll.lib"      DEL "%_XHB_BIN%\xhbddll.lib" /Q
           IF     EXIST "%_XHB_LIB%\xhbddll.lib"           \xhb\bin\XBUILD.EXE xHBdDll.dll.xbp %_XB_Debug% %1
           IF NOT EXIST "%_XHB_LIB%\xhbddll.lib"           \xhb\bin\XBUILD.EXE xHBdDll.dll.xbp %_XB_Debug% %1 -link
           IF     EXIST "%_XHB_BIN%\xhbddll.lib"  MOVE /Y "%_XHB_BIN%\xhbddll.lib" "%_XHB_LIB%"

        :No_XHBDLL


        REM =======================
        REM Multi Thread (MT) LIB's
        REM =======================

         IF "%_BUILD_MT%"=="NO" GOTO No_MT

            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\XHBmt.lib.xbp"    "\xHarbour\XHBmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\DBFmt.lib.xbp"    "\xHarbour\DBFmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\NTXmt.lib.xbp"    "\xHarbour\NTXmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\CDXmt.lib.xbp"    "\xHarbour\CDXmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\NSXmt.lib.xbp"    "\xHarbour\NSXmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\SixCDXmt.lib.xbp" "\xHarbour\SixCDXmt.lib.xbp" /y

            CD \xHarbour
            IF "%_BUILD_XHB.EXE%"=="NO" GOTO No_XHBLIBmt
               \xhb\bin\XBUILD.EXE xHBmt.lib.xbp        %_XB_Debug% %1
            :No_XHBLIBmt
            \xhb\bin\XBUILD.EXE DBFmt.lib.xbp           %_XB_Debug% %1
            \xhb\bin\XBUILD.EXE NTXmt.lib.xbp           %_XB_Debug% %1
            \xhb\bin\XBUILD.EXE CDXmt.lib.xbp           %_XB_Debug% %1
            \xhb\bin\XBUILD.EXE NSXmt.lib.xbp           %_XB_Debug% %1
            \xhb\bin\XBUILD.EXE SixCDXmt.lib.xbp        %_XB_Debug% %1

            CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
            \xhb\bin\XBUILD.EXE OptGmt.lib.xbp          %_XB_Debug% %1

           IF "%_BUILD_XHB.DLL%"=="NO" GOTO No_XHBDLLmt

               IF     EXIST "%_XHB_BIN%\xhbmtdll.lib"      DEL "%_XHB_BIN%\xhbmtdll.lib" /Q
               IF     EXIST "%_XHB_LIB%\xhbmtdll.lib"           \xhb\bin\XBUILD.EXE xHBmtDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\xhbmtdll.lib"           \xhb\bin\XBUILD.EXE xHBmtDll.dll.xbp %_XB_NonDebug% %1 -link
               IF     EXIST "%_XHB_BIN%\xhbmtdll.lib"  MOVE /Y "%_XHB_BIN%\xhbmtdll.lib" "%_XHB_LIB%"

               IF     EXIST "%_XHB_BIN%\xhbdmtdll.lib"      DEL "%_XHB_BIN%\xhbdmtdll.lib" /Q
               IF     EXIST "%_XHB_LIB%\xhbdmtdll.lib"           \xhb\bin\XBUILD.EXE xHBdmtDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\xhbdmtdll.lib"           \xhb\bin\XBUILD.EXE xHBdmtDll.dll.xbp %_XB_NonDebug% %1 -link
               IF     EXIST "%_XHB_BIN%\xhbdmtdll.lib"  MOVE /Y "%_XHB_BIN%\xhbdmtdll.lib" "%_XHB_LIB%"

            :No_XHBDLLmt

         :No_MT

     :No_Core



REM  ===============================================
REM  ===============================================
ECHO xBuild.exe
REM  ===============================================
REM  ===============================================


     IF "%_BUILD_XBUILD%"=="NO" GOTO No_xBuild
REM  IF NOT "%_XB_Compiler%"=="xcc" GOTO No_xBuild

        REM - Can not build inplace because file is in-use!
        \xhb\bin\xbuild.exe \xharbour\bin\xBuild.exe \xharbour.com\xharbour-xbuild\xBuild.prg \xharbour.com\xharbour-xbuild\tproject.prg \xharbour.com\xharbour-xbuild\tproject-c.prg -New -x\xhb -NoXbp -o\xbp\%_XB_Compiler%\xBuild-1.exe
        \xharbour\bin\xbuild.exe \xhb\bin\xBuild.exe \xharbour.com\xharbour-xbuild\xBuild.prg \xharbour.com\xharbour-xbuild\tproject.prg \xharbour.com\xharbour-xbuild\tproject-c.prg -New -x\xhb -NoXbp -o\xbp\%_XB_Compiler%\xBuild-2.exe
        IF EXIST \xharbour\bin\xBuild.exe DEL \xharbour\bin\xBuild.exe

        IF "%_BUILD_DEMO%"=="NO" GOTO No_xBuild_Demo
           CD "\xHarbour.com\xHarbour-xBuild\%_XB_Compiler%\Demo"
           \xhb\bin\XBUILD.EXE xBuild.exe.xbp %_XB_NonDebug% %1
        :No_xBuild_Demo

     :No_xBuild


REM  ===============================================
REM  ===============================================
ECHO xHarbour.org Contrib LIB's
REM  ===============================================
REM  ===============================================

     IF "%_BUILD_CONTRIB%"=="NO" GOTO No_Contrib

        CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
        \xhb\bin\XBUILD.EXE WVT.lib.xbp     %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE WVG.lib.xbp     %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE ODBC.lib.xbp    %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE LibMisc.lib.xbp %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE Nanfor.lib.xbp  %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE GD.lib.xbp      %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE TipSSL.lib.xbp  %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE WVW.lib.xbp     %_XB_Debug% %1

     :No_Contrib



REM  ===============================================
REM  ===============================================
ECHO xHarbour.com LIB's
REM  ===============================================
REM  ===============================================


REM --> BGD.lib
        CD "\xHarbour.com\xHarbour-Builder"
        IF "%XBUILD_XCC%"=="YES" \xhb\bin\XLIB \xhb\dll\BGD.dll /out:\xhb\lib\BGD.lib
        IF "%XBUILD_VC8%"=="YES" XCOPY BGD.lib \xhb\lib\vc8 /d /y
        IF "%XBUILD_BC5%"=="YES" XCOPY BGD.lib \xhb\lib\bc5 /d /y


REM --> ADS.lib
        IF "%_BUILD_ADS%"=="NO" GOTO No_ADS
        CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
        \xhb\bin\XBUILD.EXE ads.lib.xbp  %_XB_Debug% %1
REM     \xhb\bin\XBUILD.EXE ads7.lib.xbp %_XB_Debug% %1

        IF "%XBUILD_XCC%"=="YES" XCOPY \xhb\dll\ADS\ACE32.dll \xhb\bin /d /y
        IF "%XBUILD_XCC%"=="YES" \xhb\bin\XLIB \xhb\bin\ace32.dll /out:\xhb\lib\ACE32.lib

        IF "%XBUILD_VC8%"=="YES" XCOPY \xhb\dll\ADS\ace32.dll \xhb\bin\vc8 /d /y
        IF "%XBUILD_VC8%"=="YES" XCOPY "%HB_DIR_ADS%\ace32.lib" \xhb\lib\vc8 /d /y

        IF "%XBUILD_BC5%"=="YES" XCOPY \xhb\dll\ADS\ace32.dll \xhb\bin\bc5 /d /y
        IF "%XBUILD_BC5%"=="YES" XCOPY \xharbour\lib\bc5\ace32.lib \xhb\lib\bc5
        :No_ADS

REM --> xbScript.lib
        CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
        \xhb\bin\XBUILD.EXE XBScript.lib.xbp %_XB_Debug% %1


REM --> TProject.lib
        IF "%_BUILD_TPROJECT.LIB%"=="NO" GOTO No_TProjectLIB
           CD "\xHarbour.com\xHarbour-xBuild\%_XB_Compiler%"
           \xhb\bin\XBUILD.EXE TProject.lib.xbp %_XB_Debug% %1
        :No_TProjectLIB


REM --> AxtiveX.lib
        CD "\xHarbour.com\xHarbour-AxtiveX\%_XB_Compiler%"
        \xhb\bin\XBUILD.EXE ActiveX.lib.xbp %_XB_Debug% %1


REM --> xEdit*.LIB
        IF "%_BUILD_XEDIT.LIB%"=="NO" GOTO No_xEditLIB
           CD "\xHarbour.com\Visual-xHarbour\xEdit\%_XB_Compiler%"
REM        \xhb\bin\XBUILD.EXE xEdit.lib.xbp        %_XB_Debug% %1
           \xhb\bin\XBUILD.EXE xEditW.lib.xbp       %_XB_Debug% %1
           \xhb\bin\XBUILD.EXE xEditWScript.lib.xbp %_XB_Debug% %1
           \xhb\bin\XBUILD.EXE xEditVXH.lib.xbp     %_XB_Debug% %1
        :No_xEditLIB


REM --> WinCore.lib
        IF "%_XB_Compiler%"=="bc5" GOTO No_WinCore.LIB
           CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
           \xhb\bin\XBUILD.EXE WinCore.lib.xbp %_XB_Debug% %1
        :No_WinCore.LIB


REM --> WinApi.LIB
        IF "%_BUILD_WINAPI.LIB%"=="NO" GOTO No_WinApi.LIB
            CD "\xHarbour.com\Visual-xHarbour\Library\%_XB_Compiler%"
            \xhb\bin\XBUILD.EXE WinAPI.lib.xbp %_XB_Debug% %1

            CD "\xHarbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Professional"
            IF EXIST WinAPI.lib.xbp \xhb\bin\XBUILD.EXE WinAPI.lib.xbp %_XB_Debug% %1

            IF "%_BUILD_DEMO%"=="NO" GOTO No_WinApi_Demo
               CD "\xHarbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Demo"
               \xhb\bin\XBUILD.EXE WinAPI.lib.xbp %_XB_Debug% %1
            :No_WinApi_Demo
        :No_WinApi.LIB

REM --> Ole
        CD "\xHarbour.com\xHarbour-OleServer\%_XB_Compiler%"
        \xhb\bin\XBUILD.EXE Ole.lib.xbp          %_XB_Debug% %1
        \xhb\bin\XBUILD.EXE OleServer.lib.xbp    %_XB_Debug% %1

        IF "%_BUILD_DEMO%"=="NO" GOTO No_Ole_Demo
           CD "\xHarbour.com\xHarbour-OleServer\%_XB_Compiler%\Demo"
           \xhb\bin\XBUILD.EXE Ole.lib.xbp       %_XB_Debug% %1
           \xhb\bin\XBUILD.EXE OleServer.lib.xbp %_XB_Debug% %1
        :No_Ole_Demo


REM --> ZipArchive & ZLib & HBZlib
        IF "%_BUILD_HBZLIB%"=="NO" GOTO No_HBZlib

           CD "\xharbour.com\ZipArchive"
           \xhb\bin\XBUILD.EXE ZipArchive.lib.xbp -NoXbp -Debug %1

           CD "\xharbour.com\ZipArchive\ZLib"
           \xhb\bin\XBUILD.EXE ZLib.lib.xbp -NoXbp -Debug %1

           CD "\xHarbour.com\xHarbour-HBZLib\%_XB_Compiler%"
           \xhb\bin\XBUILD.EXE xHBzip.lib.xbp %_XB_Debug% %1

           CD "\xHarbour.com\xHarbour-HBZLib\xHbZipDll"
           IF     EXIST "%_XHB_DLL%\xHBZipDll.lib"     DEL "%_XHB_DLL%\xHBZipDll.lib" /Q
           IF     EXIST "%_XHB_LIB%\xHBZipDll.lib"          \xhb\bin\XBUILD.EXE xHBZipDll.dll.xbp -NoXbp %1
           IF NOT EXIST "%_XHB_LIB%\xHBZipDll.lib"          \xhb\bin\XBUILD.EXE xHBZipDll.dll.xbp -NoXbp %1 -link
           IF     EXIST "%_XHB_DLL%\xHBZipDll.lib" MOVE /Y "%_XHB_DLL%\xHBZipDll.lib" "%_XHB_LIB%"

       :No_HBZlib


REM --> SQLRDD
        IF "%_BUILD_SQLRDD%"=="NO" GOTO No_SQLRDD
            CD "\xHarbour.com\xHarbour-SQLRDD\%_XB_Compiler%"
            \xhb\bin\XBUILD.EXE SQL.lib.xbp                             %_XB_Debug% %1
REM        \xhb\bin\XBUILD.EXE mySQL.lib.xbp                           %_XB_Debug% %1
            IF "%_BUILD_MT%"=="YES" \xhb\bin\XBUILD.EXE SQLmt.lib.xbp   %_XB_Debug% %1
REM        IF "%_BUILD_MT%"=="YES" \xhb\bin\XBUILD.EXE mySQLmt.lib.xbp %_XB_Debug% %1

            IF "%_BUILD_DEMO%"=="NO" GOTO No_SQL_Demo
               CD "\xHarbour.com\xHarbour-SQLRDD\%_XB_Compiler%\Demo"
               \xhb\bin\XBUILD.EXE SQL.lib.xbp                             %_XB_Debug% %1
REM           \xhb\bin\XBUILD.EXE mySQL.lib.xbp                           %_XB_Debug% %1
               IF "%_BUILD_MT%"=="YES" \xhb\bin\XBUILD.EXE SQLmt.lib.xbp   %_XB_Debug% %1
REM           IF "%_BUILD_MT%"=="YES" \xhb\bin\XBUILD.EXE mySQLmt.lib.xbp %_XB_Debug% %1
            :NO_SQL_Demo
        :No_SQLRDD


REM --> REDBFCDX
        IF "%_BUILD_REDBFCDX%"=="NO" GOTO No_REDBFCDX
           CD "\xHarbour"
       COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\redbfcdx.lib.xbp" "\xHarbour\redbfcdx.lib.xbp"    /y
           \xhb\bin\XBUILD.EXE REDBFCDX.lib.xbp %_XB_Debug% %1
        :No_REDBFCDX


REM --> BMDBFCDX
        IF "%_BUILD_BMDBFCDX%"=="NO" GOTO No_BMDBFCDX
           CD "\xHarbour"
       COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\bmdbfcdx.lib.xbp" "\xHarbour\bmdbfcdx.lib.xbp"    /y
           \xhb\bin\XBUILD.EXE BMDBFCDX.lib.xbp %_XB_Debug% %1
        :No_BMDBFCDX


REM --> ApolloRDD
        IF "%_BUILD_APOLLORDD%"=="NO" GOTO No_ApolloRDD
            CD "\xHarbour.com\xHarbour-ApolloRDD\%_XB_Compiler%"
            \xhb\bin\XBUILD.EXE Six.lib.xbp %_XB_Debug% %1

            IF "%_BUILD_DEMO%"=="NO" GOTO No_Apollo_Demo
               CD "\xHarbour.com\xHarbour-ApolloRDD\%_XB_Compiler%\Demo"
               \xhb\bin\XBUILD.EXE Six.lib.xbp %_XB_Debug% %1
            :No_Apollo_Demo

            IF NOT EXIST "%_XHB_LIB%\fts32.lib" \xhb\bin\XLIB "%_XHB_DLL%\ApolloRDD\fts32.dll" /out:"%_XHB_LIB%\FTS32.lib"
            IF NOT EXIST "%_XHB_LIB%\sde61.lib" \xhb\bin\XLIB "%_XHB_DLL%\ApolloRDD\sde61.dll" /out:"%_XHB_LIB%\SDE61.lib"
         :No_ApolloRDD


REM --> xHBComm
        IF "%_BUILD_XHBCOMM%"=="NO" GOTO No_xHBComm
            CD "\xHarbour.com\xHarbour-xHBComm\Comm"
            \xhb\bin\XBUILD.EXE Comm.lib.xbp -NoXbp -Debug %1

            CD "\xHarbour.com\xHarbour-xHBComm\xHBCommDll"
            IF     EXIST "%_XHB_DLL%\xHBCommDll.lib"     DEL "%_XHB_DLL%\xHBCommDll.lib" /Q
            IF     EXIST "%_XHB_LIB%\xHBCommDll.lib"          \xhb\bin\XBUILD.EXE xHBCommDll.dll.xbp -NoXbp %1
            IF NOT EXIST "%_XHB_LIB%\xHBCommDll.lib"          \xhb\bin\XBUILD.EXE xHBCommDll.dll.xbp -NoXbp -link
            IF     EXIST "%_XHB_DLL%\xHBCommDll.lib" MOVE /Y "%_XHB_DLL%\xHBCommDll.lib" "%_XHB_LIB%"

            CD "\xHarbour.com\xHarbour-xHBComm\%_XB_Compiler%"
            \xhb\bin\XBUILD.EXE xHBComm.lib.xbp %_XB_Debug% %1
        :No_xHBComm


REM --> CT3-Comm
        IF "%_BUILD_CT3COMM%"=="NO" GOTO No_CT3Comm
           CD "\xHarbour.com\xHarbour-CT3Comm\%_XB_Compiler%"
           \xhb\bin\XBUILD.EXE CT3Comm.lib.xbp %_XB_Debug% %1
        :No_CT3Comm


REM --> RMDBFCDX.lib
        CD "\xHarbour.com\xHarbour-Rushmore\%_XB_Compiler%"
        \xhb\bin\XBUILD.EXE RMDBFCDX.lib.xbp %_XB_Debug% %1


REM --> XDO
        IF "%_BUILD_XDO.DLL%"=="NO" GOTO No_XDO.DLL
            CD "\xHarbour.com\xHarbour-XDO\%_XB_Compiler%"
            IF EXIST "%_XHB_DLL%\XDO.lib" DEL "%_XHB_DLL%\XDO.lib" /Q
            \xhb\bin\XBUILD.EXE XDO.dll.xbp %_XB_NonDebug% %1
            IF EXIST "%_XHB_DLL%\XDO.lib" MOVE /Y "%_XHB_DLL%\XDO.lib" "%_XHB_LIB%"
        :No_XDO.DLL


REM --> IEGui
        IF "%_BUILD_IEGUI.DLL%"=="NO" GOTO No_IEGUI.LIB
           CD "\xHarbour.com\IEGui\%_XB_Compiler%"
           \xhb\bin\XBUILD.EXE IEGui.lib.xbp %_XB_Debug% %1
        :No_IEGui.LIB


REM --> FreeImage
        CD "\xHarbour.com\FreeImage\%_XB_Compiler%"
        \xhb\bin\XBUILD.EXE FreeImage-xHB.lib.xbp %_XB_Debug% %1
         XCOPY FreeImage.lib "%_XHB_LIB%" /d /y
         XCOPY FreeImage.dll "%_XHB_DLL%" /d /y


REM --> VXH.lib
        IF "%_BUILD_VXH_AS%"=="NONE" GOTO No_VXHLib
        IF "%_BUILD_VXH_AS%"=="NO"   GOTO No_VXHLib
             CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%"
             \xhb\bin\XBUILD.EXE VXH.lib.xbp %_XB_Debug% %1

            CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Personal"
             IF EXIST VXH.lib.xbp \xhb\bin\XBUILD.EXE VXH.lib.xbp %_XB_Debug% %1

            CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Professional"
             IF EXIST VXH.lib.xbp \xhb\bin\XBUILD.EXE VXH.lib.xbp %_XB_Debug% %1

             IF "%_BUILD_DEMO%"=="NO" GOTO No_VXHLib_Demo
               CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Demo"
               \xhb\bin\XBUILD.EXE VXH.lib.xbp %_XB_Debug% %1
            :No_VXHLib_Demo
         :No_VXHLib


REM --> VXH.DLL
        IF "%_BUILD_VXHDLL%"=="NO"   GOTO No_VXHDLL
           CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%"

           IF     EXIST "%_XHB_BIN%\vxhdll.lib"       DEL "%_XHB_BIN%\vxhdll.lib" /Q
           IF     EXIST "%_XHB_LIB%\vxhdll.lib"            \xhb\bin\XBUILD.EXE VXHDll.dll.xbp %_XB_NonDebug% %1
           IF NOT EXIST "%_XHB_LIB%\vxhdll.lib"            \xhb\bin\XBUILD.EXE VXHDll.dll.xbp %_XB_NonDebug% %1 -link
           IF     EXIST "%_XHB_BIN%\vxhdll.lib"   MOVE /Y "%_XHB_BIN%\vxhdll.lib" "%_XHB_LIB%"

           IF     EXIST "%_XHB_BIN%\vxhddll.lib"      DEL "%_XHB_BIN%\vxhddll.lib" /Q
           IF     EXIST "%_XHB_LIB%\vxhddll.lib"           \xhb\bin\XBUILD.EXE VXHdDll.dll.xbp %_XB_NonDebug% %1
           IF NOT EXIST "%_XHB_LIB%\vxhddll.lib"           \xhb\bin\XBUILD.EXE VXHdDll.dll.xbp %_XB_NonDebug% %1 -link
           IF     EXIST "%_XHB_BIN%\vxhddll.lib"  MOVE /Y "%_XHB_BIN%\vxhddll.lib" "%_XHB_LIB%"


           CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Professional"

           IF     EXIST "%_XHB_BIN%\Professional\vxhdll.lib"       DEL "%_XHB_BIN%\Professional\vxhdll.lib" /Q
           IF     EXIST "%_XHB_LIB%\Professional\vxhdll.lib"            \xhb\bin\XBUILD.EXE VXHDll.dll.xbp %_XB_NonDebug% %1
           IF NOT EXIST "%_XHB_LIB%\Professional\vxhdll.lib"            \xhb\bin\XBUILD.EXE VXHDll.dll.xbp %_XB_NonDebug% %1 -link
           IF     EXIST "%_XHB_BIN%\Professional\vxhdll.lib"   MOVE /Y "%_XHB_BIN%\Professional\vxhdll.lib" "%_XHB_LIB%\Professional"

           IF     EXIST "%_XHB_BIN%\Professional\vxhddll.lib"      DEL "%_XHB_BIN%\Professional\vxhddll.lib" /Q
           IF     EXIST "%_XHB_LIB%\Professional\vxhddll.lib"           \xhb\bin\XBUILD.EXE VXHdDll.dll.xbp %_XB_NonDebug% %1
           IF NOT EXIST "%_XHB_LIB%\Professional\vxhddll.lib"           \xhb\bin\XBUILD.EXE VXHdDll.dll.xbp %_XB_NonDebug% %1 -link
           IF     EXIST "%_XHB_BIN%\Professional\vxhddll.lib"  MOVE /Y "%_XHB_BIN%\Professional\vxhddll.lib" "%_XHB_LIB%\Professional%"

        :No_VXHDLL


REM --> DebugServer
        CD "\xHarbour.com\xHarbour-DebugServer\server\%_XB_Compiler%"
        \xhb\bin\XBUILD.EXE DbgServe.lib.xbp %_XB_Debug% %1


REM --> VXHDebugger
        IF "%_BUILD_DBG_CLIENT%"=="NO" GOTO No_DebugClient
           CD "\xHarbour.com\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%"
           \xhb\bin\XBUILD.EXE VXHD.lib.xbp %_XB_Debug% %1
        :No_DebugClient


REM  ===============================================
REM  ===============================================
ECHO EXE's
REM  ===============================================
REM  ===============================================


REM --> xBuildW.exe
        IF "%_BUILD_XBUILDW_AS%"=="NONE" GOTO No_xBuildW
        IF "%_BUILD_XBUILDW_AS%"=="NO"   GOTO No_xBuildW
           IF "%XBUILD_VC8%"=="NO" GOTO No_VC8_1
               IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\open.bmp        COPY \xHarbour.com\xHarbour-xBuild\*.bmp       \xHarbour.com\xHarbour-xBuild\vc8\*.bmp /Y
                IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\xbuild.ico      COPY \xHarbour.com\xHarbour-xBuild\*.ico       \xHarbour.com\xHarbour-xBuild\vc8\*.ico /Y
                IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\plus.ico        COPY \xHarbour.com\Visual-xHarbour\xEdit\*.ico \xHarbour.com\xHarbour-xBuild\vc8\*.ico /Y
                IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\xharbour320.avi COPY \xHarbour.com\xHarbour-xBuild\*.avi       \xHarbour.com\xHarbour-xBuild\vc8\*.avi /Y
                IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\xbuild.xml      COPY \xHarbour.com\xHarbour-xBuild\*.xml       \xHarbour.com\xHarbour-xBuild\vc8\*.xml /Y
            :No_VC8_1
           CD "\xHarbour.com\xHarbour-xBuild\%_XB_Compiler%"
           \xhb\bin\XBUILD.EXE xBuildW.exe.xbp %_XB_Exe% %1
           IF EXIST "%_XHB_BIN%\xBuildW.lib" DEL "%_XHB_BIN%\xBuildW.lib" /Q


           IF "%_BUILD_PERSONAL%"=="NO" GOTO No_xBuildW_Personal
               IF "%XBUILD_VC8%"=="NO" GOTO No_VC8_2
                   IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Personal\open.bmp        COPY \xHarbour.com\xHarbour-xBuild\*.bmp       \xHarbour.com\xHarbour-xBuild\vc8\Personal\*.bmp /Y
                    IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Personal\xbuild.ico      COPY \xHarbour.com\xHarbour-xBuild\*.ico       \xHarbour.com\xHarbour-xBuild\vc8\Personal\*.ico /Y
                    IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Personal\plus.ico        COPY \xHarbour.com\Visual-xHarbour\xEdit\*.ico \xHarbour.com\xHarbour-xBuild\vc8\Personal\*.ico /Y
                    IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Personal\xharbour320.avi COPY \xHarbour.com\xHarbour-xBuild\*.avi       \xHarbour.com\xHarbour-xBuild\vc8\Personal\*.avi /Y
                    IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Personal\xbuild.xml      COPY \xHarbour.com\xHarbour-xBuild\*.xml       \xHarbour.com\xHarbour-xBuild\vc8\Personal\*.xml /Y
                :No_VC8_2
              CD "\xHarbour.com\xHarbour-xBuild\%_XB_Compiler%\Personal"
              \xhb\bin\XBUILD.EXE xBuildW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Personal\xBuildW.lib" DEL "%_XHB_BIN%\Personal\xBuildW.lib" /Q
           :No_xBuildW_Personal

           IF "%_BUILD_DEMO%"=="NO" GOTO No_xBuildW_Demo
               IF "%XBUILD_VC8%"=="NO" GOTO No_VC8_3
                   IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Demo\open.bmp        COPY \xHarbour.com\xHarbour-xBuild\*.bmp       \xHarbour.com\xHarbour-xBuild\vc8\Demo\*.bmp /Y
                    IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Demo\xbuild.ico      COPY \xHarbour.com\xHarbour-xBuild\*.ico       \xHarbour.com\xHarbour-xBuild\vc8\Demo\*.ico /Y
                    IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Demo\plus.ico        COPY \xHarbour.com\Visual-xHarbour\xEdit\*.ico \xHarbour.com\xHarbour-xBuild\vc8\Demo\*.ico /Y
                    IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Demo\xharbour320.avi COPY \xHarbour.com\xHarbour-xBuild\*.avi       \xHarbour.com\xHarbour-xBuild\vc8\Demo\*.avi /Y
                    IF NOT EXIST \xHarbour.com\xHarbour-xBuild\vc8\Demo\xbuild.xml      COPY \xHarbour.com\xHarbour-xBuild\*.xml       \xHarbour.com\xHarbour-xBuild\vc8\Demo\*.xml /Y
                :No_VC8_3
              CD "\xHarbour.com\xHarbour-xBuild\%_XB_Compiler%\Demo"
              \xhb\bin\XBUILD.EXE xBuildW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Demo\xBuildW.lib" DEL "%_XHB_BIN%\Demo\xBuildW.lib" /Q
           :No_xBuildW_Demo
        :No_xBuildW


REM --> xPrompt
        IF "%_BUILD_XPROMPT_AS%"=="NONE"   GOTO No_XPROMPT
        IF "%_BUILD_XPROMPT_AS%"=="NO"     GOTO No_XPROMPT
            IF "%_BUILD_XPROMPT_AS%"=="EXE" GOTO No_XPROMPTDLL
               CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
               \xhb\bin\XBUILD.EXE xPrompt.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPrompt.lib" DEL "%_XHB_BIN%\xPrompt.lib" /Q
               \xhb\bin\XBUILD.EXE xPromptSQL.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPromptSQL.lib" DEL "%_XHB_BIN%\xPromptSQL.lib" /Q
            :No_XPROMPTDLL

            IF "%_BUILD_XPROMPT_AS%"=="DLL" GOTO No_XPROMPTEXE
               CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\XPROMPT.exe"
               \xhb\bin\XBUILD.EXE xPrompt.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPrompt.lib" DEL "%_XHB_BIN%\xPrompt.lib" /Q
               \xhb\bin\XBUILD.EXE xPromptSQL.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPromptSQL.lib" DEL "%_XHB_BIN%\xPromptSQL.lib" /Q
            :No_XPROMPTEXE
        :No_XPROMPT


REM --> VXH.EXE
        IF "%_BUILD_VXH_AS%"=="NONE"   GOTO No_VXH
        IF "%_BUILD_VXH_AS%"=="NO"     GOTO No_VXH
            IF "%_BUILD_VXH_AS%"=="EXE" GOTO No_VXHDLL

               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%"
               \xhb\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\vxh.lib" DEL "%_XHB_BIN%\vxh.lib" /Q

               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Personal"
              IF EXIST VXH.exe.xbp \xhb\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Personal\vxh.lib" DEL "%_XHB_BIN%\Personal\vxh.lib" /Q

               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Professional"
              IF EXIST VXH.exe.xbp \xhb\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Professional\vxh.lib" DEL "%_XHB_BIN%\Professional\vxh.lib" /Q

            :No_VXHDLL

            IF "%_BUILD_VXH_AS%"=="DLL" GOTO No_VXHEXE

               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\vxh.exe"
               \xhb\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\vxh.lib" DEL "%_XHB_BIN%\vxh.lib" /Q

               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Personal\vxh.exe"
              IF EXIST VXH.exe.xbp \xhb\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Personal\vxh.lib" DEL "%_XHB_BIN%\Personal\vxh.lib" /Q

               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Professional\vxh.exe"
              IF EXIST VXH.exe.xbp \xhb\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Professional\vxh.lib" DEL "%_XHB_BIN%\Professional\vxh.lib" /Q

            :No_VXHEXE

            IF "%_BUILD_DEMO%"=="NO" GOTO No_VXHExe_Demo
               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Demo"
               \xhb\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Demo\vxh.lib" DEL "%_XHB_BIN%\Demo\vxh.lib" /Q
            :No_VXHExe_Demo
        :No_VXH


REM --> xDebugW
        IF "%_BUILD_xDebugW_AS%"=="NONE"   GOTO No_xDebugW
        IF "%_BUILD_xDebugW_AS%"=="NO"     GOTO No_xDebugW
            IF "%_BUILD_xDebugW_AS%"=="EXE" GOTO No_xDebugWDLL
               CD "\xHarbour.com\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%"
               \xhb\bin\XBUILD.EXE xDebugW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xDebugW.lib" DEL "%_XHB_BIN%\xDebugW.lib" /Q
            :No_xDebugWDLL

            IF "%_BUILD_xDebugW_AS%"=="DLL" GOTO No_xDebugWEXE
               CD "\xHarbour.com\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%\xDebugW.exe"
               \xhb\bin\XBUILD.EXE xDebugW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xDebugW.lib" DEL "%_XHB_BIN%\xDebugW.lib" /Q
            :No_xDebugWEXE

            IF "%_BUILD_DEMO%"=="NO" GOTO No_xDebugWExe_Demo
               CD "\xHarbour.com\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%\Demo"
               \xhb\bin\XBUILD.EXE xDebugW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Demo\xDebugW.lib" DEL "%_XHB_BIN%\Demo\xDebugW.lib" /Q
            :No_xDebugWExe_Demo
        :No_xDebugW


REM --> xEditW
          IF "%_BUILD_xEditW_AS%"=="NONE"   GOTO No_xEditW
          IF "%_BUILD_xEditW_AS%"=="NO"     GOTO No_xEditW
              IF "%_BUILD_xEditW_AS%"=="EXE" GOTO No_xEditWDLL
                 CD "\xHarbour.com\Visual-xHarbour\xEdit\%_XB_Compiler%"
                 \xhb\bin\XBUILD.EXE xEditW.exe.xbp %_XB_Exe% %1
                 IF EXIST "%_XHB_BIN%\xEditW.lib" DEL "%_XHB_BIN%\xEditW.lib" /Q
              :No_xEditWDLL

              IF "%_BUILD_xEditW_AS%"=="DLL" GOTO No_xEditWEXE
                 CD "\xHarbour.com\Visual-xHarbour\xEdit\%_XB_Compiler%\xEditW.exe"
                 \xhb\bin\XBUILD.EXE xEditW.exe.xbp %_XB_Exe% %1
                 IF EXIST "%_XHB_BIN%\xEditW.lib" DEL "%_XHB_BIN%\xEditW.lib" /Q
                 IF EXIST "%_XHB_BIN%\xEditW.lib" DEL "%_XHB_BIN%\xEditW.lib" /Q
              :No_xEditWEXE
          :No_xEditW


IF "%XBUILD_XCC%"=="YES" GOTO CleanXCC
IF "%XBUILD_VC8%"=="YES" GOTO CleanVC8
IF "%XBUILD_BC5%"=="YES" GOTO CleanBC5

:CleanXCC
 DEL \xHB\Bin\*.MAP /Q
 DEL \xHB\Bin\*.EXP /Q
 DEL \xHB\Bin\Demo\*.MAP /Q
 DEL \xHB\Bin\Demo\*.EXP /Q
 DEL \xHB\Bin\Personal\*.MAP /Q
 DEL \xHB\Bin\Personal\*.EXP /Q
 DEL \xHB\Bin\Professional\*.MAP /Q
 DEL \xHB\Bin\Professional\*.EXP /Q
 DEL \xHB\Dll\*.MAP /Q
 DEL \xHB\Dll\*.EXP /Q
 DEL \xHB\Dll\*.LIB /Q
GOTO Done

:CleanVC8
 DEL \xHB\Bin\vc8\*.PDB /Q
 DEL \xHB\Bin\vc8\*.EXP /Q
 DEL \xHB\Bin\vc8\Demo\*.PDB /Q
 DEL \xHB\Bin\vc8\Demo\*.EXP /Q
 DEL \xHB\Bin\vc8\Personal\*.PDB /Q
 DEL \xHB\Bin\vc8\Personal\*.EXP /Q
 DEL \xHB\Bin\vc8\Professional\*.PDB /Q
 DEL \xHB\Bin\vc8\Professional\*.EXP /Q
 DEL \xHB\Dll\vc8\*.EXP /Q
GOTO Done

:CleanBC5
 DEL \xHB\Bin\*.MAP /Q
 DEL \xHB\Bin\*.EXP /Q
 DEL \xHB\Bin\Demo\*.MAP /Q
 DEL \xHB\Bin\Demo\*.EXP /Q
 DEL \xHB\Bin\Personal\*.MAP /Q
 DEL \xHB\Bin\Personal\*.EXP /Q
 DEL \xHB\Bin\Professional\*.MAP /Q
 DEL \xHB\Bin\Professional\*.EXP /Q
 DEL \xHB\Dll\*.MAP /Q
 DEL \xHB\Dll\*.EXP /Q

:Done