
CD \xHarbour

    IF "%_BUILD_DEMO%"=="NO" GOTO N4
       IF NOT EXIST "%_XHB_BIN%\Demo"         MD "%_XHB_BIN%\Demo"
       IF NOT EXIST "%_XHB_LIB%\Demo"         MD "%_XHB_LIB%\Demo"
    :N4
    IF "%_BUILD_PERSONAL%"=="NO" GOTO N8
       IF NOT EXIST "%_XHB_BIN%\Personal"     MD "%_XHB_BIN%\Personal"
       IF NOT EXIST "%_XHB_LIB%\Personal"     MD "%_XHB_LIB%\Personal"
    :N8
    IF "%_BUILD_PROF%"=="NO" GOTO N12
       IF NOT EXIST "%_XHB_BIN%\Professional" MD "%_XHB_BIN%\Professional"
       IF NOT EXIST "%_XHB_LIB%\Professional" MD "%_XHB_LIB%\Professional"
    :N12
    IF NOT EXIST "%_XHB_DLL%"         MD "%_XHB_DLL%"
    IF NOT EXIST \xHB\c_lib\Win       MD \xHB\c_lib\Win

IF "%_XB_Compiler%"=="vc8" XCOPY \xHarbour\bin\vc\harbour.dll  \xHB\bin\vc8 /d /y /i

IF "%_XB_Compiler%"=="xcc" GOTO No_MakeFolders
   IF NOT EXIST \xHB\bin\%_XB_Compiler% MD \xHB\bin\%_XB_Compiler%
   IF NOT EXIST \xHB\dll\%_XB_Compiler% MD \xHB\dll\%_XB_Compiler%
   IF NOT EXIST \xHB\lib\%_XB_Compiler% MD \xHB\lib\%_XB_Compiler%
:No_MakeFolders

ATTRIB -R \xharbour\xHarbourBuilder\xbuild*.ini /S

REM  ===============================================
REM  ===============================================
ECHO XCC
REM  ===============================================
REM  ===============================================

copy \xharbour\xharbourbuilder\Visual-xHarbour\Library\include\winuser.ch \xharbour\include
copy \xharbour\xharbourbuilder\Visual-xHarbour\Library\include\winapi.ch \xharbour\include

      IF NOT "%_XB_Compiler%"=="xcc" GOTO No_xCC
      IF "%XCC_XCC%"=="NO" GOTO No_xCC
        CD \xharbour\xHarbourBuilder\xHarbour-XCC
        CALL build_xcc.bat
     :No_xCC

REM  ===============================================
REM  ===============================================
ECHO xBuild
REM  ===============================================
REM  ===============================================

     IF EXIST \xHB\bin\xBuild.exe GOTO No_xBuild    
        CD \xharbour\xHarbourBuilder\xHarbour-xBuild\ 
        CALL \xharbour\bin\bld_vc xbuild tproject tproject-c
        COPY xBuild.exe \xHB\bin /y
     :No_xBuild

REM  ===============================================
REM  ===============================================
ECHO Compiler
REM  ===============================================
REM  ===============================================

     IF "%_BUILD_XHB_EXE%"=="NO" GOTO No_Compiler

        CD \xHarbour
        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\xHB.exe.xbp" "\xHarbour\xHB.exe.xbp" /y
        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\stxHB.files.inc"            "\xHarbour\stxHB.files.inc" /y

        \xHB\bin\xBuild.exe xHB.exe.xbp %_XB_Debug% %1

        IF "%_BUILD_DEMO%"=="NO" GOTO No_xhbexe_Demo
           COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\Demo\xHB.exe.xbp" "\xHarbour\xHB-Demo.exe.xbp" /y
           \xHB\bin\xBuild.exe xHB-Demo.exe.xbp %_XB_Debug% %1
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

        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\xHB.lib.xbp"    "\xHarbour\xHB.lib.xbp"    /y
        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\dbf.lib.xbp"    "\xHarbour\dbf.lib.xbp"    /y
        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\ntx.lib.xbp"    "\xHarbour\ntx.lib.xbp"    /y
        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\cdx.lib.xbp"    "\xHarbour\cdx.lib.xbp"    /y
        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\nsx.lib.xbp"    "\xHarbour\nsx.lib.xbp"    /y
        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\sixcdx.lib.xbp" "\xHarbour\sixcdx.lib.xbp" /y

        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\stdbf.files.inc"  "\xHarbour\stdbf.files.inc"  /y
        COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\stdlib.files.inc" "\xHarbour\stdlib.files.inc" /y

        CD "\xHarbour"
        IF "%_BUILD_XHB_EXE%"=="NO" GOTO No_XHBLIB
        \xHB\bin\xBuild.exe xHB.lib.xbp           %_XB_Debug% %1
        :No_XHBLIB
        \xHB\bin\xBuild.exe DBF.lib.xbp           %_XB_Debug% %1
        \xHB\bin\xBuild.exe NTX.lib.xbp           %_XB_Debug% %1
        \xHB\bin\xBuild.exe CDX.lib.xbp           %_XB_Debug% %1
        \xHB\bin\xBuild.exe NSX.lib.xbp           %_XB_Debug% %1
        \xHB\bin\xBuild.exe SixCDX.lib.xbp        %_XB_Debug% %1

        IF "%_BUILD_DEMO%"=="NO" GOTO No_xhblib_Demo
           COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\Demo\xHB.lib.xbp" "\xHarbour\xHB-Demo.lib.xbp" /y
           \xHB\bin\xBuild.exe xHB-demo.lib.xbp %_XB_Debug% %1
           IF EXIST "%_XHB_LIB%\Demo\xHB.lib" DEL "%_XHB_LIB%\Demo\xHB.lib" /Q
           RENAME "%_XHB_LIB%\Demo\xHB-Demo.lib" "xHB.lib"
        :No_xhblib_Demo

        CD "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%"
        \xHB\bin\xBuild.exe noRDD.lib.xbp %_XB_Debug% %1
        \xHB\bin\xBuild.exe OptG.lib.xbp  %_XB_Debug% %1

        IF "%_BUILD_XHB_DLL%"=="NO" GOTO No_XHBDLL
           \xHB\bin\xBuild.exe DMAIN.LIB.xbp  %_XB_Debug% %1
           \xHB\bin\xBuild.exe UseDll.lib.xbp %_XB_Debug% %1

           IF     EXIST "%_XHB_BIN%\xHBdll.lib" DEL "%_XHB_BIN%\xHBdll.lib" /Q
           IF     EXIST "%_XHB_LIB%\xHBdll.lib" \xHB\bin\xBuild.exe xHBDll.dll.xbp %_XB_NonDebug% %1
           IF NOT EXIST "%_XHB_LIB%\xHBdll.lib" \xHB\bin\xBuild.exe xHBDll.dll.xbp %_XB_NonDebug% %1 -link
           IF     EXIST "%_XHB_BIN%\xHBdll.lib" MOVE /Y "%_XHB_BIN%\xHBdll.lib" "%_XHB_LIB%"

           IF     EXIST "%_XHB_BIN%\xHBddll.lib" DEL "%_XHB_BIN%\xHBddll.lib" /Q
           IF     EXIST "%_XHB_LIB%\xHBddll.lib" \xHB\bin\xBuild.exe xHBdDll.dll.xbp %_XB_Debug% %1
           IF NOT EXIST "%_XHB_LIB%\xHBddll.lib" \xHB\bin\xBuild.exe xHBdDll.dll.xbp %_XB_Debug% %1 -link
           IF     EXIST "%_XHB_BIN%\xHBddll.lib" MOVE /Y "%_XHB_BIN%\xHBddll.lib" "%_XHB_LIB%"
           
        :No_XHBDLL


        REM =======================
        REM Multi Thread (MT) LIB's
        REM =======================

         IF "%_BUILD_MT%"=="NO" GOTO No_MT

            COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\xHBmt.lib.xbp"    "\xHarbour\xHBmt.lib.xbp"    /y
            COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\DBFmt.lib.xbp"    "\xHarbour\DBFmt.lib.xbp"    /y
            COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\NTXmt.lib.xbp"    "\xHarbour\NTXmt.lib.xbp"    /y
            COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\CDXmt.lib.xbp"    "\xHarbour\CDXmt.lib.xbp"    /y
            COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\NSXmt.lib.xbp"    "\xHarbour\NSXmt.lib.xbp"    /y
            COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\SixCDXmt.lib.xbp" "\xHarbour\SixCDXmt.lib.xbp" /y

            CD \xHarbour
            IF "%_BUILD_XHB_EXE%"=="NO" GOTO No_XHBLIBmt
               \xHB\bin\xBuild.exe xHBmt.lib.xbp        %_XB_Debug% %1
            :No_XHBLIBmt
            \xHB\bin\xBuild.exe DBFmt.lib.xbp           %_XB_Debug% %1
            \xHB\bin\xBuild.exe NTXmt.lib.xbp           %_XB_Debug% %1
            \xHB\bin\xBuild.exe CDXmt.lib.xbp           %_XB_Debug% %1
            \xHB\bin\xBuild.exe NSXmt.lib.xbp           %_XB_Debug% %1
            \xHB\bin\xBuild.exe SixCDXmt.lib.xbp        %_XB_Debug% %1

            CD "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%"
            \xHB\bin\xBuild.exe OptGmt.lib.xbp          %_XB_Debug% %1

           IF "%_BUILD_XHB_DLL%"=="NO" GOTO No_XHBDLLmt

               IF     EXIST "%_XHB_BIN%\xHBmtdll.lib"      DEL "%_XHB_BIN%\xHBmtdll.lib" /Q
               IF     EXIST "%_XHB_LIB%\xHBmtdll.lib"           \xHB\bin\xBuild.exe xHBmtDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\xHBmtdll.lib"           \xHB\bin\xBuild.exe xHBmtDll.dll.xbp %_XB_NonDebug% %1 -link
               IF     EXIST "%_XHB_BIN%\xHBmtdll.lib"  MOVE /Y "%_XHB_BIN%\xHBmtdll.lib" "%_XHB_LIB%"

               IF     EXIST "%_XHB_BIN%\xHBdmtdll.lib"      DEL "%_XHB_BIN%\xHBdmtdll.lib" /Q
               IF     EXIST "%_XHB_LIB%\xHBdmtdll.lib"           \xHB\bin\xBuild.exe xHBdmtDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\xHBdmtdll.lib"           \xHB\bin\xBuild.exe xHBdmtDll.dll.xbp %_XB_NonDebug% %1 -link
               IF     EXIST "%_XHB_BIN%\xHBdmtdll.lib"  MOVE /Y "%_XHB_BIN%\xHBdmtdll.lib" "%_XHB_LIB%"

            :No_XHBDLLmt

         :No_MT

     :No_Core



REM  ===============================================
REM  ===============================================
ECHO xBuild.exe
REM  ===============================================
REM  ===============================================


     IF "%_BUILD_XBUILD%"=="NO" GOTO No_xBuild

        REM - Can not build inplace because file is in-use!
        \xHB\bin\xBuild.exe \xharbour\bin\xBuild.exe \xharbour\xHarbourBuilder\xharbour-xbuild\xBuild.prg \xharbour\xHarbourBuilder\xharbour-xbuild\tproject.prg \xharbour\xHarbourBuilder\xharbour-xbuild\tproject-c.prg -New -x\xHB -NoXbp -o\xbp\%_XB_Compiler%\xBuild-1.exe
        \xharbour\bin\xBuild.exe \xHB\bin\xBuild.exe \xharbour\xHarbourBuilder\xharbour-xbuild\xBuild.prg \xharbour\xHarbourBuilder\xharbour-xbuild\tproject.prg \xharbour\xHarbourBuilder\xharbour-xbuild\tproject-c.prg -New -x\xHB -NoXbp -o\xbp\%_XB_Compiler%\xBuild-2.exe
        IF EXIST \xharbour\bin\xBuild.exe DEL \xharbour\bin\xBuild.exe

        IF "%_BUILD_DEMO%"=="NO" GOTO No_xBuild_Demo
           CD "\xharbour\xHarbourBuilder\xHarbour-xBuild\%_XB_Compiler%\Demo"
           \xHB\bin\xBuild.exe xBuild.exe.xbp %_XB_NonDebug% %1
        :No_xBuild_Demo

     :No_xBuild


REM  ===============================================
REM  ===============================================
ECHO xHarbour.org Contrib LIB's
REM  ===============================================
REM  ===============================================

     IF "%_BUILD_CONTRIB%"=="NO" GOTO No_Contrib

        CD "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%"
        \xHB\bin\xBuild.exe WVT.lib.xbp     %_XB_Debug% %1
        \xHB\bin\xBuild.exe WVG.lib.xbp     %_XB_Debug% %1
        \xHB\bin\xBuild.exe ODBC.lib.xbp    %_XB_Debug% %1
        \xHB\bin\xBuild.exe LibMisc.lib.xbp %_XB_Debug% %1
        \xHB\bin\xBuild.exe Nanfor.lib.xbp  %_XB_Debug% %1
        \xHB\bin\xBuild.exe GD.lib.xbp      %_XB_Debug% %1
        \xHB\bin\xBuild.exe TipSSL.lib.xbp  %_XB_Debug% %1
        \xHB\bin\xBuild.exe WVW.lib.xbp     %_XB_Debug% %1

     :No_Contrib



REM  ===============================================
REM  ===============================================
ECHO xHarbour.com LIB's
REM  ===============================================
REM  ===============================================


REM --> WinPrint.lib
        IF "%_BUILD_WINPRINT%"=="YES" (
           CD "\xharbour\xHarbourBuilder\WinPrint\Build LIB\"
           \xHB\Bin\xBuild.exe WinPrint.lib.xbp %_XB_Debug% %1
           )

REM --> BGD.lib
        IF "%_BUILD_BGD%"=="YES" (
           CD "\xharbour\xHarbourBuilder\xHarbour-Builder"
           IF "%XBUILD_XCC%"=="YES" \xHB\bin\XLIB \xHB\dll\BGD.dll /out:\xHB\lib\BGD.lib
           IF "%XBUILD_VC8%"=="YES" XCOPY BGD.lib \xHB\lib\vc8 /d /y
           )

REM --> ADS.lib
        IF "%_BUILD_ADS%"=="NO" GOTO No_ADS
           CD "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%"
           \xHB\bin\xBuild.exe ads.lib.xbp %_XB_Debug% %1
           
           IF "%XBUILD_XCC%"=="YES" XCOPY "%HB_DIR_ADS%\ACE32.dll" \xHB\bin /d /y
           IF "%XBUILD_XCC%"=="YES" \xHB\bin\XLIB \xHB\bin\ace32.dll /out:\xHB\lib\ACE32.lib

           IF "%XBUILD_VC8%"=="YES" XCOPY "%HB_DIR_ADS%\ACE32.dll" \xHB\bin\vc8 /d /y
           IF "%XBUILD_VC8%"=="YES" XCOPY "%HB_DIR_ADS%\ace32.lib" \xHB\lib\vc8 /d /y

        :No_ADS

REM --> xbScript.lib
        IF "%_BUILD_XBSCRIPT_LIB%"=="NO" GOTO No_XBSCRIPT_LIB
           CD "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%"
           \xHB\bin\xBuild.exe XBScript.lib.xbp %_XB_Debug% %1
        :No_XBSCRIPT_LIB

REM --> TProject.lib
        IF "%_BUILD_TPROJECT_LIB%"=="NO" GOTO No_TProjectLIB
           CD "\xharbour\xHarbourBuilder\xHarbour-xBuild\%_XB_Compiler%"
           \xHB\bin\xBuild.exe TProject.lib.xbp %_XB_Debug% %1
        :No_TProjectLIB

REM --> ActiveX.lib
         CD "\xharbour\xHarbourBuilder\xHarbour-ActiveX\%_XB_Compiler%"
        \xHB\bin\xBuild.exe ActiveX.lib.xbp %_XB_Debug% %1

REM --> xEdit*.LIB
        IF "%_BUILD_XEDIT_LIB%"=="NO" GOTO No_xEditLIB
           CD "\xharbour\xHarbourBuilder\Visual-xHarbour\xEdit\%_XB_Compiler%"
REM        \xHB\bin\xBuild.exe xEdit.lib.xbp        %_XB_Debug% %1
REM        \xHB\bin\xBuild.exe xEditVXH.lib.xbp     %_XB_Debug% %1
           \xHB\bin\xBuild.exe xEditW.lib.xbp       %_XB_Debug% %1
           \xHB\bin\xBuild.exe xEditWScript.lib.xbp %_XB_Debug% %1
        :No_xEditLIB

REM --> WinCore.lib
        IF "%_BUILD_WINCORE%"=="NO" GOTO No_WinCore
           CD "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%"
           \xHB\bin\xBuild.exe WinCore.lib.xbp %_XB_Debug% %1
        :No_WinCore

REM --> WINAPI.LIB
        IF "%_BUILD_WINAPI_LIB%"=="NO" GOTO No_WINAPI_LIB
            CD "\xharbour\xHarbourBuilder\Visual-xHarbour\Library\%_XB_Compiler%"
            \xHB\bin\xBuild.exe WINAPI.LIB.xbp %_XB_Debug% %1

            IF "%_BUILD_PROF%"=="NO" GOTO No_WinApi_Prof
               CD "\xharbour\xHarbourBuilder\Visual-xHarbour\Library\%_XB_Compiler%\Professional"
               \xHB\bin\xBuild.exe WINAPI.LIB.xbp %_XB_Debug% %1
            :No_WinApi_Prof

            IF "%_BUILD_DEMO%"=="NO" GOTO No_WinApi_Demo
               CD "\xharbour\xHarbourBuilder\Visual-xHarbour\Library\%_XB_Compiler%\Demo"
               \xHB\bin\xBuild.exe WINAPI.LIB.xbp %_XB_Debug% %1
            :No_WinApi_Demo
            
        :No_WINAPI_LIB

REM --> Ole
        CD "\xharbour\xHarbourBuilder\xHarbour-OleServer\%_XB_Compiler%"
        \xHB\bin\xBuild.exe Ole.lib.xbp          %_XB_Debug% %1
        \xHB\bin\xBuild.exe OleServer.lib.xbp    %_XB_Debug% %1

        IF "%_BUILD_DEMO%"=="NO" GOTO No_Ole_Demo
           CD "\xharbour\xHarbourBuilder\xHarbour-OleServer\%_XB_Compiler%\Demo"
           \xHB\bin\xBuild.exe Ole.lib.xbp       %_XB_Debug% %1
           \xHB\bin\xBuild.exe OleServer.lib.xbp %_XB_Debug% %1
        :No_Ole_Demo


REM --> ZipArchive & ZLib & HBZlib
        IF "%_BUILD_HBZLIB%"=="NO" GOTO No_HBZlib

           CD "\xharbour\xHarbourBuilder\ZipArchive"
           \xHB\bin\xBuild.exe ZipArchive.lib.xbp %_XB_Debug% %1

           CD "\xharbour\xHarbourBuilder\ZipArchive\ZLib"
           \xHB\bin\xBuild.exe ZLib.lib.xbp %_XB_Debug% %1

           CD "\xharbour\xHarbourBuilder\xHarbour-HBZLib\%_XB_Compiler%"
           \xHB\bin\xBuild.exe xHBzip.lib.xbp %_XB_Debug% %1

           CD "\xharbour\xHarbourBuilder\xHarbour-HBZLib\xHbZipDll"
           IF     EXIST "%_XHB_DLL%\xHBZipDll.lib"     DEL "%_XHB_DLL%\xHBZipDll.lib" /Q
           IF     EXIST "%_XHB_LIB%\xHBZipDll.lib"          \xHB\bin\xBuild.exe xHBZipDll.dll.xbp -NoXbp %1
           IF NOT EXIST "%_XHB_LIB%\xHBZipDll.lib"          \xHB\bin\xBuild.exe xHBZipDll.dll.xbp -NoXbp %1 -link
           IF     EXIST "%_XHB_DLL%\xHBZipDll.lib" MOVE /Y "%_XHB_DLL%\xHBZipDll.lib" "%_XHB_LIB%"

       :No_HBZlib


REM --> SQLRDD
        IF "%_BUILD_SQLRDD%"=="NO" GOTO No_SQLRDD
        
            CD "\xharbour\xHarbourBuilder\xHarbour-SQLRDD\%_XB_Compiler%"
            \xHB\bin\xBuild.exe SQL.lib.xbp %_XB_Debug% %1
            IF "%_BUILD_MT%"=="YES" \xHB\bin\xBuild.exe SQLmt.lib.xbp %_XB_Debug% %1

            IF "%_BUILD_DEMO%"=="NO" GOTO No_SQL_Demo
               CD "\xharbour\xHarbourBuilder\xHarbour-SQLRDD\%_XB_Compiler%\Demo"
               \xHB\bin\xBuild.exe SQL.lib.xbp %_XB_Debug% %1
               IF "%_BUILD_MT%"=="YES" \xHB\bin\xBuild.exe SQLmt.lib.xbp %_XB_Debug% %1
            :NO_SQL_Demo
            
        :No_SQLRDD


REM --> REDBFCDX
        IF "%_BUILD_REDBFCDX%"=="NO" GOTO No_REDBFCDX
           CD "\xHarbour"
           COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\redbfcdx.lib.xbp" "\xHarbour\redbfcdx.lib.xbp"    /y
           \xHB\bin\xBuild.exe REDBFCDX.lib.xbp %_XB_Debug% %1
        :No_REDBFCDX


REM --> BMDBFCDX
        IF "%_BUILD_BMDBFCDX%"=="NO" GOTO No_BMDBFCDX
           CD "\xHarbour"
           COPY "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\bmdbfcdx.lib.xbp" "\xHarbour\bmdbfcdx.lib.xbp"    /y
           \xHB\bin\xBuild.exe BMDBFCDX.lib.xbp %_XB_Debug% %1
        :No_BMDBFCDX


REM --> ApolloRDD
        IF "%_BUILD_APOLLORDD%"=="NO" GOTO No_ApolloRDD
            CD "\xharbour\xHarbourBuilder\xHarbour-ApolloRDD\%_XB_Compiler%"
            \xHB\bin\xBuild.exe Six.lib.xbp %_XB_Debug% %1

            IF "%_BUILD_DEMO%"=="NO" GOTO No_Apollo_Demo
               CD "\xharbour\xHarbourBuilder\xHarbour-ApolloRDD\%_XB_Compiler%\Demo"
               \xHB\bin\xBuild.exe Six.lib.xbp %_XB_Debug% %1
            :No_Apollo_Demo

            IF NOT EXIST "%_XHB_LIB%\fts32.lib" \xHB\bin\XLIB "%_XHB_DLL%\ApolloRDD\fts32.dll" /out:"%_XHB_LIB%\FTS32.lib"
            IF NOT EXIST "%_XHB_LIB%\sde61.lib" \xHB\bin\XLIB "%_XHB_DLL%\ApolloRDD\sde61.dll" /out:"%_XHB_LIB%\SDE61.lib"
         
         :No_ApolloRDD


REM --> xHBComm
        IF "%_BUILD_XHBCOMM%"=="NO" GOTO No_xHBComm
            CD "\xharbour\xHarbourBuilder\xHarbour-xHBComm\Comm"
            \xHB\bin\xBuild.exe Comm.lib.xbp %_XB_Debug% %1

            CD "\xharbour\xHarbourBuilder\xHarbour-xHBComm\xHBCommDll"
            IF     EXIST "%_XHB_DLL%\xHBCommDll.lib"     DEL "%_XHB_DLL%\xHBCommDll.lib" /Q
            IF     EXIST "%_XHB_LIB%\xHBCommDll.lib"          \xHB\bin\xBuild.exe xHBCommDll.dll.xbp -NoXbp %1
            IF NOT EXIST "%_XHB_LIB%\xHBCommDll.lib"          \xHB\bin\xBuild.exe xHBCommDll.dll.xbp -NoXbp -link
            IF     EXIST "%_XHB_DLL%\xHBCommDll.lib" MOVE /Y "%_XHB_DLL%\xHBCommDll.lib" "%_XHB_LIB%"

            CD "\xharbour\xHarbourBuilder\xHarbour-xHBComm\%_XB_Compiler%"
            \xHB\bin\xBuild.exe xHBComm.lib.xbp %_XB_Debug% %1
            
        :No_xHBComm


REM --> CT3-Comm
        IF "%_BUILD_CT3COMM%"=="NO" GOTO No_CT3Comm		
           CD "\xharbour\xHarbourBuilder\xHarbour-CT3Comm\%_XB_Compiler%"
           \xHB\bin\xBuild.exe CT3Comm.lib.xbp %_XB_Debug% %1
        :No_CT3Comm


REM --> RMDBFCDX.lib
        CD "\xharbour\xHarbourBuilder\xHarbour-Rushmore\%_XB_Compiler%"
        \xHB\bin\xBuild.exe RMDBFCDX.lib.xbp %_XB_Debug% %1


REM --> XDO
        IF "%_BUILD_XDO_DLL%"=="NO" GOTO No_XDO_DLL
            CD "\xharbour\xHarbourBuilder\xHarbour-XDO\%_XB_Compiler%"
            IF EXIST "%_XHB_DLL%\XDO.lib" DEL "%_XHB_DLL%\XDO.lib" /Q
            \xHB\bin\xBuild.exe XDO.dll.xbp %_XB_NonDebug% %1
            IF EXIST "%_XHB_DLL%\XDO.lib" MOVE /Y "%_XHB_DLL%\XDO.lib" "%_XHB_LIB%"
        :No_XDO_DLL


REM --> IEGui
        IF "%_BUILD_IEGUI_LIB%"=="NO" GOTO No_IEGUI_LIB
           CD "\xharbour\xHarbourBuilder\IEGui\%_XB_Compiler%"
           \xHB\bin\xBuild.exe IEGui.lib.xbp %_XB_Debug% %1
        :No_IEGUI_LIB


REM --> FreeImage
        CD "\xharbour\xHarbourBuilder\FreeImage\%_XB_Compiler%"
        \xHB\bin\xBuild.exe FreeImage-xHB.lib.xbp %_XB_Debug% %1
        CD "\xharbour\xHarbourBuilder\FreeImage"
        XCOPY FreeImage.lib "%_XHB_LIB%" /d /y
        XCOPY FreeImage.dll "%_XHB_DLL%" /d /y


REM --> VXH.lib
        IF "%_BUILD_VXH_AS%"=="NONE" GOTO No_VXHLib
        IF "%_BUILD_VXH_AS%"=="NO"   GOTO No_VXHLib
        
             CD "\xharbour\xHarbourBuilder\Visual-xHarbour\Library\%_XB_Compiler%"
            \xHB\bin\xBuild.exe VXH.lib.xbp %_XB_Debug% %1

            IF "%_BUILD_DEMO%"=="NO" GOTO N433
               CD "\xharbour\xHarbourBuilder\Visual-xHarbour\Library\%_XB_Compiler%\Demo"
               IF EXIST VXH.lib.xbp \xHB\bin\xBuild.exe VXH.lib.xbp %_XB_Debug% %1
            :N433

            IF "%_BUILD_PERSONAL%"=="NO" GOTO N438
               CD "\xharbour\xHarbourBuilder\Visual-xHarbour\Library\%_XB_Compiler%\Personal"
               IF EXIST VXH.lib.xbp \xHB\bin\xBuild.exe VXH.lib.xbp %_XB_Debug% %1
            :N438

            IF "%_BUILD_PROF%"=="NO" GOTO N443
               CD "\xharbour\xHarbourBuilder\Visual-xHarbour\Library\%_XB_Compiler%\Professional"
               IF EXIST VXH.lib.xbp \xHB\bin\xBuild.exe VXH.lib.xbp %_XB_Debug% %1
            :N443

         :No_VXHLib


REM --> VXH.DLL
         IF "%_BUILD_VXHDLL%"=="NO"   GOTO No_VXHDLL
           CD "\xharbour\xHarbourBuilder\Visual-xHarbour\Library\%_XB_Compiler%"

            IF     EXIST "%_XHB_BIN%\vxhdll.lib"       DEL "%_XHB_BIN%\vxhdll.lib" /Q
            IF     EXIST "%_XHB_LIB%\vxhdll.lib"            \xHB\bin\xBuild.exe VXHDll.dll.xbp %_XB_NonDebug% %1
            IF NOT EXIST "%_XHB_LIB%\vxhdll.lib"            \xHB\bin\xBuild.exe VXHDll.dll.xbp %_XB_NonDebug% %1 -link
            IF     EXIST "%_XHB_BIN%\vxhdll.lib"   MOVE /Y "%_XHB_BIN%\vxhdll.lib" "%_XHB_LIB%"

            IF "%_BUILD_DEBUG%"=="NO" GOTO N472
               IF     EXIST "%_XHB_BIN%\vxhddll.lib"      DEL "%_XHB_BIN%\vxhddll.lib" /Q
               IF     EXIST "%_XHB_LIB%\vxhddll.lib"           \xHB\bin\xBuild.exe VXHdDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\vxhddll.lib"           \xHB\bin\xBuild.exe VXHdDll.dll.xbp %_XB_NonDebug% %1 -link
               IF     EXIST "%_XHB_BIN%\vxhddll.lib"  MOVE /Y "%_XHB_BIN%\vxhddll.lib" "%_XHB_LIB%"
            :N472

            IF "%_BUILD_PROF%"=="NO" GOTO N467
               CD "\xharbour\xHarbourBuilder\Visual-xHarbour\Library\%_XB_Compiler%\Professional"

               IF     EXIST "%_XHB_BIN%\Professional\vxhdll.lib"       DEL "%_XHB_BIN%\Professional\vxhdll.lib" /Q
               IF     EXIST "%_XHB_LIB%\Professional\vxhdll.lib"            \xHB\bin\xBuild.exe VXHDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\Professional\vxhdll.lib"            \xHB\bin\xBuild.exe VXHDll.dll.xbp %_XB_NonDebug% %1 -link
               IF     EXIST "%_XHB_BIN%\Professional\vxhdll.lib"   MOVE /Y "%_XHB_BIN%\Professional\vxhdll.lib" "%_XHB_LIB%\Professional"
               
               IF "%_BUILD_DEBUG%"=="NO" GOTO N487
                  IF     EXIST "%_XHB_BIN%\Professional\vxhddll.lib"      DEL "%_XHB_BIN%\Professional\vxhddll.lib" /Q
                  IF     EXIST "%_XHB_LIB%\Professional\vxhddll.lib"           \xHB\bin\xBuild.exe VXHdDll.dll.xbp %_XB_NonDebug% %1
                  IF NOT EXIST "%_XHB_LIB%\Professional\vxhddll.lib"           \xHB\bin\xBuild.exe VXHdDll.dll.xbp %_XB_NonDebug% %1 -link
                  IF     EXIST "%_XHB_BIN%\Professional\vxhddll.lib"  MOVE /Y "%_XHB_BIN%\Professional\vxhddll.lib" "%_XHB_LIB%\Professional%"
               :N487
               
            :N467
           
         :No_VXHDLL


REM --> DebugServer
       IF "%_BUILD_DBG_SERVER%"=="NO" GOTO No_DebugServer
        CD "\xharbour\xHarbourBuilder\xHarbour-DebugServer\server\%_XB_Compiler%"
        \xHB\bin\xBuild.exe DbgServe.lib.xbp %_XB_Debug% %1
       :No_DebugServer

REM --> VXHDebugger
        IF "%_BUILD_DBG_CLIENT%"=="NO" GOTO No_DebugClient
           CD "\xharbour\xHarbourBuilder\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%"
           \xHB\bin\xBuild.exe VXHD.lib.xbp %_XB_Debug% %1
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
               IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\open.bmp        COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.bmp       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\*.bmp /Y
                IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\xbuild.ico      COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.ico       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\*.ico /Y
                IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\plus.ico        COPY \xharbour\xHarbourBuilder\Visual-xHarbour\xEdit\*.ico \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\*.ico /Y
                IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\xharbour320.avi COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.avi       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\*.avi /Y
                IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\xbuild.xml      COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.xml       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\*.xml /Y
            :No_VC8_1
           CD "\xharbour\xHarbourBuilder\xHarbour-xBuild\%_XB_Compiler%"
           \xHB\bin\xBuild.exe xBuildW.exe.xbp %_XB_Exe% %1
           IF EXIST "%_XHB_BIN%\xBuildW.lib" DEL "%_XHB_BIN%\xBuildW.lib" /Q


           IF "%_BUILD_PERSONAL%"=="NO" GOTO No_xBuildW_Personal
               IF "%XBUILD_VC8%"=="NO" GOTO No_VC8_2
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\open.bmp        COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.bmp       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\*.bmp /Y
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\xbuild.ico      COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.ico       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\*.ico /Y
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\plus.ico        COPY \xharbour\xHarbourBuilder\Visual-xHarbour\xEdit\*.ico \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\*.ico /Y
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\xharbour320.avi COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.avi       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\*.avi /Y
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\xbuild.xml      COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.xml       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Personal\*.xml /Y
                :No_VC8_2
              CD "\xharbour\xHarbourBuilder\xHarbour-xBuild\%_XB_Compiler%\Personal"
              \xHB\bin\xBuild.exe xBuildW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Personal\xBuildW.lib" DEL "%_XHB_BIN%\Personal\xBuildW.lib" /Q
           :No_xBuildW_Personal
           

           IF "%_BUILD_DEMO%"=="NO" GOTO No_xBuildW_Demo
               IF "%XBUILD_VC8%"=="NO" GOTO No_VC8_3
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\open.bmp        COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.bmp       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\*.bmp /Y
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\xbuild.ico      COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.ico       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\*.ico /Y
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\plus.ico        COPY \xharbour\xHarbourBuilder\Visual-xHarbour\xEdit\*.ico \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\*.ico /Y
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\xharbour320.avi COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.avi       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\*.avi /Y
                   IF NOT EXIST \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\xbuild.xml      COPY \xharbour\xHarbourBuilder\xHarbour-xBuild\*.xml       \xharbour\xHarbourBuilder\xHarbour-xBuild\vc8\Demo\*.xml /Y
                :No_VC8_3
              CD "\xharbour\xHarbourBuilder\xHarbour-xBuild\%_XB_Compiler%\Demo"
              \xHB\bin\xBuild.exe xBuildW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Demo\xBuildW.lib" DEL "%_XHB_BIN%\Demo\xBuildW.lib" /Q
           :No_xBuildW_Demo
        :No_xBuildW


REM --> xPrompt
        IF "%_BUILD_XPROMPT_AS%"=="NONE"   GOTO No_XPROMPT
        IF "%_BUILD_XPROMPT_AS%"=="NO"     GOTO No_XPROMPT
        
            IF "%_BUILD_XPROMPT_AS%"=="EXE" GOTO No_XPROMPTDLL
               CD "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%"
               \xHB\bin\xBuild.exe xPrompt.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPrompt.lib" DEL "%_XHB_BIN%\xPrompt.lib" /Q
               \xHB\bin\xBuild.exe xPromptSQL.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPromptSQL.lib" DEL "%_XHB_BIN%\xPromptSQL.lib" /Q
            :No_XPROMPTDLL

            IF "%_BUILD_XPROMPT_AS%"=="DLL" GOTO No_XPROMPTEXE
               CD "\xharbour\xHarbourBuilder\xHarbour-Builder\%_XB_Compiler%\XPROMPT.exe"
               \xHB\bin\xBuild.exe xPrompt.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPrompt.lib" DEL "%_XHB_BIN%\xPrompt.lib" /Q
               \xHB\bin\xBuild.exe xPromptSQL.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPromptSQL.lib" DEL "%_XHB_BIN%\xPromptSQL.lib" /Q
            :No_XPROMPTEXE
            
        :No_XPROMPT


REM --> VXH.EXE
         IF "%_BUILD_VXH_AS%"=="NONE"   GOTO No_VXH
         IF "%_BUILD_VXH_AS%"=="NO"     GOTO No_VXH
         
            CD "\xharbour\xHarbourBuilder\Visual-xHarbour\IDE\%_XB_Compiler%"
            \xHB\bin\xBuild.exe VXH.exe.xbp %_XB_Exe% %1
            IF EXIST "%_XHB_BIN%\vxh.lib" DEL "%_XHB_BIN%\vxh.lib" /Q
               
            IF "%_BUILD_DEMO%"=="NO" GOTO NoVXHDemo
               CD "\xharbour\xHarbourBuilder\Visual-xHarbour\IDE\%_XB_Compiler%\Demo"
               \xHB\bin\xBuild.exe VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Demo\vxh.lib" DEL "%_XHB_BIN%\Demo\vxh.lib" /Q
            :NoVXHDemo

            IF "%_BUILD_PERSONAL%"=="NO" GOTO NoVXHPersonal
               CD "\xharbour\xHarbourBuilder\Visual-xHarbour\IDE\%_XB_Compiler%\Personal"
               IF EXIST VXH.exe.xbp \xHB\bin\xBuild.exe VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Personal\vxh.lib" DEL "%_XHB_BIN%\Personal\vxh.lib" /Q
            :NoVXHPersonal

            IF "%_BUILD_PROF%"=="NO" GOTO NoVXHProf
               CD "\xharbour\xHarbourBuilder\Visual-xHarbour\IDE\%_XB_Compiler%\Professional"
               IF EXIST VXH.exe.xbp \xHB\bin\xBuild.exe VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Professional\vxh.lib" DEL "%_XHB_BIN%\Professional\vxh.lib" /Q
            :NoVXHProf

         :No_VXH


REM --> VR.EXE
         IF "%_BUILD_VR%"=="NO" GOTO No_VR
            CD "\xharbour\xHarbourBuilder\Visual-Report\%_XB_Compiler%""
            \xHB\bin\xBuild.exe VR.lib.xbp %_XB_Exe% %1
            \xHB\bin\xBuild.exe VR.exe.xbp %_XB_Exe% %1
         :No_VR

REM --> xDebugW
         IF "%_BUILD_xDebugW_AS%"=="NONE"   GOTO No_xDebugW
         IF "%_BUILD_xDebugW_AS%"=="NO"     GOTO No_xDebugW
         
            IF "%_BUILD_xDebugW_AS%"=="EXE"    GOTO No_xDebugWDLL
               CD "\xharbour\xHarbourBuilder\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%"
               \xHB\bin\xBuild.exe xDebugW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xDebugW.lib" DEL "%_XHB_BIN%\xDebugW.lib" /Q
            :No_xDebugWDLL

            IF "%_BUILD_xDebugW_AS%"=="DLL" GOTO No_xDebugWEXE
               CD "\xharbour\xHarbourBuilder\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%\xDebugW.exe"
               \xHB\bin\xBuild.exe xDebugW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xDebugW.lib" DEL "%_XHB_BIN%\xDebugW.lib" /Q
            :No_xDebugWEXE

            IF "%_BUILD_DEMO%"=="NO" GOTO No_xDebugWExe_Demo
               CD "\xharbour\xHarbourBuilder\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%\Demo"
               \xHB\bin\xBuild.exe xDebugW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Demo\xDebugW.lib" DEL "%_XHB_BIN%\Demo\xDebugW.lib" /Q
            :No_xDebugWExe_Demo
            
        :No_xDebugW


REM --> xEditW
          IF "%_BUILD_xEditW_AS%"=="NONE"   GOTO No_xEditW
          IF "%_BUILD_xEditW_AS%"=="NO"     GOTO No_xEditW
          
              IF "%_BUILD_xEditW_AS%"=="EXE" GOTO No_xEditWDLL
                 CD "\xharbour\xHarbourBuilder\Visual-xHarbour\xEdit\%_XB_Compiler%"
                 \xHB\bin\xBuild.exe xEditW.exe.xbp %_XB_Exe% %1
                 IF EXIST "%_XHB_BIN%\xEditW.lib" DEL "%_XHB_BIN%\xEditW.lib" /Q
              :No_xEditWDLL

              IF "%_BUILD_xEditW_AS%"=="DLL" GOTO No_xEditWEXE
                 CD "\xharbour\xHarbourBuilder\Visual-xHarbour\xEdit\%_XB_Compiler%\xEditW.exe"
                 \xHB\bin\xBuild.exe xEditW.exe.xbp %_XB_Exe% %1
                 IF EXIST "%_XHB_BIN%\xEditW.lib" DEL "%_XHB_BIN%\xEditW.lib" /Q
                 IF EXIST "%_XHB_BIN%\xEditW.lib" DEL "%_XHB_BIN%\xEditW.lib" /Q
              :No_xEditWEXE
          :No_xEditW


REM --> xbScript.dll
        IF "%_BUILD_XBSCRIPT_DLL%"=="NO" GOTO No_XBSCRIPT_DLL
          CALL "\xharbour\xHarbourBuilder\xHarbour-XBScript\Build XBScript.dll.bat" %1
        :No_XBSCRIPT_DLL



IF "%XBUILD_XCC%"=="YES" GOTO CleanXCC
IF "%XBUILD_VC8%"=="YES" GOTO CleanVC8

:CleanXCC
 DEL \xHB\*.MAP /Q /S
 DEL \xHB\*.EXP /Q /S
 DEL \xHB\Dll\*.LIB /Q
GOTO Done

:CleanVC8
 DEL \xHB\*.EXP /Q /S
GOTO Done


:Done
ECHO "*************************"
ECHO "*** BUILD SUCCESFULLY ***"
ECHO "*************************"