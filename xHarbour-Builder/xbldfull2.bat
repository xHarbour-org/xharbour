
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
    IF NOT EXIST "%_XHB_DLL%"                 MD "%_XHB_DLL%"
    IF NOT EXIST \xHB\c_lib\Win       MD \xHB\c_lib\Win

IF "%_XB_Compiler%"=="vc8" XCOPY \xHarbour\bin\vc\harbour.dll  \xHB\bin\vc8 /d /y /i

IF "%_XB_Compiler%"=="xcc" GOTO No_MakeFolders
   IF NOT EXIST \xHB\bin\%_XB_Compiler% MD \xHB\bin\%_XB_Compiler%
   IF NOT EXIST \xHB\dll\%_XB_Compiler% MD \xHB\dll\%_XB_Compiler%
   IF NOT EXIST \xHB\lib\%_XB_Compiler% MD \xHB\lib\%_XB_Compiler%
:No_MakeFolders

ATTRIB +R \xHarbour.com\xbuild*.ini /S

REM  ===============================================
REM  ===============================================
ECHO XCC
REM  ===============================================
REM  ===============================================

      IF NOT "%_XB_Compiler%"=="xcc" GOTO No_xCC
        CD \xHarbour.com\xHarbour-XCC
        CALL build_xcc.bat
     :No_xCC

REM  ===============================================
REM  ===============================================
ECHO xBuild
REM  ===============================================
REM  ===============================================

     IF EXIST \xHB\bin\xbuild.exe GOTO No_xBuild    
        CD \xHarbour.com\xHarbour-xBuild\ 
        CALL \xharbour\bin\bld_vc xbuild tproject tproject-c
        COPY xbuild.exe \xHB\bin /y
     :No_xBuild

REM  ===============================================
REM  ===============================================
ECHO Compiler
REM  ===============================================
REM  ===============================================

     IF "%_BUILD_XHB_EXE%"=="NO" GOTO No_Compiler

        CD \xHarbour
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\xHB.exe.xbp" "\xHarbour\xHB.exe.xbp" /y
        COPY "\xHarbour.com\xHarbour-Builder\stxHB.files.inc"            "\xHarbour\stxHB.files.inc" /y

        \xHB\bin\XBUILD.EXE xHB.exe.xbp %_XB_Debug% %1

        IF "%_BUILD_DEMO%"=="NO" GOTO No_xhbexe_Demo
           COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\Demo\xHB.exe.xbp" "\xHarbour\xHB-Demo.exe.xbp" /y
           \xHB\bin\XBUILD.EXE xHB-Demo.exe.xbp %_XB_Debug% %1
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

        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\xHB.lib.xbp"    "\xHarbour\xHB.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\dbf.lib.xbp"    "\xHarbour\dbf.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\ntx.lib.xbp"    "\xHarbour\ntx.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\cdx.lib.xbp"    "\xHarbour\cdx.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\nsx.lib.xbp"    "\xHarbour\nsx.lib.xbp"    /y
        COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\sixcdx.lib.xbp" "\xHarbour\sixcdx.lib.xbp" /y

        COPY "\xHarbour.com\xHarbour-Builder\stdbf.files.inc"  "\xHarbour\stdbf.files.inc"  /y
        COPY "\xHarbour.com\xHarbour-Builder\stdlib.files.inc" "\xHarbour\stdlib.files.inc" /y

        CD "\xHarbour"
        IF "%_BUILD_XHB_EXE%"=="NO" GOTO No_XHBLIB
        \xHB\bin\XBUILD.EXE xHB.lib.xbp           %_XB_Debug% %1
        :No_XHBLIB
        \xHB\bin\XBUILD.EXE DBF.lib.xbp           %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE NTX.lib.xbp           %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE CDX.lib.xbp           %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE NSX.lib.xbp           %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE SixCDX.lib.xbp        %_XB_Debug% %1

        IF "%_BUILD_DEMO%"=="NO" GOTO No_xhblib_Demo
           COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\Demo\xHB.lib.xbp" "\xHarbour\xHB-Demo.lib.xbp" /y
           \xHB\bin\XBUILD.EXE xHB-demo.lib.xbp %_XB_Debug% %1
           IF EXIST "%_XHB_LIB%\Demo\xHB.lib" DEL "%_XHB_LIB%\Demo\xHB.lib" /Q
           RENAME "%_XHB_LIB%\Demo\xHB-Demo.lib" "xHB.lib"
        :No_xhblib_Demo

        CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
        \xHB\bin\XBUILD.EXE noRDD.lib.xbp %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE OptG.lib.xbp  %_XB_Debug% %1

        IF "%_BUILD_XHB_DLL%"=="NO" GOTO No_XHBDLL
           \xHB\bin\XBUILD.EXE DMAIN.LIB.xbp  %_XB_Debug% %1
           \xHB\bin\XBUILD.EXE UseDll.lib.xbp %_XB_Debug% %1

           IF     EXIST "%_XHB_BIN%\xHBdll.lib" DEL "%_XHB_BIN%\xHBdll.lib" /Q
           IF     EXIST "%_XHB_LIB%\xHBdll.lib" \xHB\bin\XBUILD.EXE xHBDll.dll.xbp %_XB_NonDebug% %1
           IF NOT EXIST "%_XHB_LIB%\xHBdll.lib" \xHB\bin\XBUILD.EXE xHBDll.dll.xbp %_XB_NonDebug% %1 -link
           IF     EXIST "%_XHB_BIN%\xHBdll.lib" MOVE /Y "%_XHB_BIN%\xHBdll.lib" "%_XHB_LIB%"

           IF "%_BUILD_DEBUG%"=="NO" GOTO N137
              IF     EXIST "%_XHB_BIN%\xHBddll.lib" DEL "%_XHB_BIN%\xHBddll.lib" /Q
              IF     EXIST "%_XHB_LIB%\xHBddll.lib" \xHB\bin\XBUILD.EXE xHBdDll.dll.xbp %_XB_Debug% %1
              IF NOT EXIST "%_XHB_LIB%\xHBddll.lib" \xHB\bin\XBUILD.EXE xHBdDll.dll.xbp %_XB_Debug% %1 -link
              IF     EXIST "%_XHB_BIN%\xHBddll.lib" MOVE /Y "%_XHB_BIN%\xHBddll.lib" "%_XHB_LIB%"
           :N137
           
        :No_XHBDLL


        REM =======================
        REM Multi Thread (MT) LIB's
        REM =======================

         IF "%_BUILD_MT%"=="NO" GOTO No_MT

            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\xHBmt.lib.xbp"    "\xHarbour\xHBmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\DBFmt.lib.xbp"    "\xHarbour\DBFmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\NTXmt.lib.xbp"    "\xHarbour\NTXmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\CDXmt.lib.xbp"    "\xHarbour\CDXmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\NSXmt.lib.xbp"    "\xHarbour\NSXmt.lib.xbp"    /y
            COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\SixCDXmt.lib.xbp" "\xHarbour\SixCDXmt.lib.xbp" /y

            CD \xHarbour
            IF "%_BUILD_XHB_EXE%"=="NO" GOTO No_XHBLIBmt
               \xHB\bin\XBUILD.EXE xHBmt.lib.xbp        %_XB_Debug% %1
            :No_XHBLIBmt
            \xHB\bin\XBUILD.EXE DBFmt.lib.xbp           %_XB_Debug% %1
            \xHB\bin\XBUILD.EXE NTXmt.lib.xbp           %_XB_Debug% %1
            \xHB\bin\XBUILD.EXE CDXmt.lib.xbp           %_XB_Debug% %1
            \xHB\bin\XBUILD.EXE NSXmt.lib.xbp           %_XB_Debug% %1
            \xHB\bin\XBUILD.EXE SixCDXmt.lib.xbp        %_XB_Debug% %1

            CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
            \xHB\bin\XBUILD.EXE OptGmt.lib.xbp          %_XB_Debug% %1

           IF "%_BUILD_XHB_DLL%"=="NO" GOTO No_XHBDLLmt

               IF     EXIST "%_XHB_BIN%\xHBmtdll.lib"      DEL "%_XHB_BIN%\xHBmtdll.lib" /Q
               IF     EXIST "%_XHB_LIB%\xHBmtdll.lib"           \xHB\bin\XBUILD.EXE xHBmtDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\xHBmtdll.lib"           \xHB\bin\XBUILD.EXE xHBmtDll.dll.xbp %_XB_NonDebug% %1 -link
               IF     EXIST "%_XHB_BIN%\xHBmtdll.lib"  MOVE /Y "%_XHB_BIN%\xHBmtdll.lib" "%_XHB_LIB%"

               IF     EXIST "%_XHB_BIN%\xHBdmtdll.lib"      DEL "%_XHB_BIN%\xHBdmtdll.lib" /Q
               IF     EXIST "%_XHB_LIB%\xHBdmtdll.lib"           \xHB\bin\XBUILD.EXE xHBdmtDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\xHBdmtdll.lib"           \xHB\bin\XBUILD.EXE xHBdmtDll.dll.xbp %_XB_NonDebug% %1 -link
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
        \xHB\bin\xbuild.exe \xharbour\bin\xBuild.exe \xharbour.com\xharbour-xbuild\xBuild.prg \xharbour.com\xharbour-xbuild\tproject.prg \xharbour.com\xharbour-xbuild\tproject-c.prg -New -x\xHB -NoXbp -o\xbp\%_XB_Compiler%\xBuild-1.exe
        \xharbour\bin\xbuild.exe \xHB\bin\xBuild.exe \xharbour.com\xharbour-xbuild\xBuild.prg \xharbour.com\xharbour-xbuild\tproject.prg \xharbour.com\xharbour-xbuild\tproject-c.prg -New -x\xHB -NoXbp -o\xbp\%_XB_Compiler%\xBuild-2.exe
        IF EXIST \xharbour\bin\xBuild.exe DEL \xharbour\bin\xBuild.exe

        IF "%_BUILD_DEMO%"=="NO" GOTO No_xBuild_Demo
           CD "\xHarbour.com\xHarbour-xBuild\%_XB_Compiler%\Demo"
           \xHB\bin\XBUILD.EXE xBuild.exe.xbp %_XB_NonDebug% %1
        :No_xBuild_Demo

     :No_xBuild


REM  ===============================================
REM  ===============================================
ECHO xHarbour.org Contrib LIB's
REM  ===============================================
REM  ===============================================

     IF "%_BUILD_CONTRIB%"=="NO" GOTO No_Contrib

        CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
        \xHB\bin\XBUILD.EXE WVT.lib.xbp     %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE WVG.lib.xbp     %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE ODBC.lib.xbp    %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE LibMisc.lib.xbp %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE Nanfor.lib.xbp  %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE GD.lib.xbp      %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE TipSSL.lib.xbp  %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE WVW.lib.xbp     %_XB_Debug% %1

     :No_Contrib



REM  ===============================================
REM  ===============================================
ECHO xHarbour.com LIB's
REM  ===============================================
REM  ===============================================


REM --> BGD.lib
        IF "%_BUILD_BGD%"=="YES" (
           CD "\xHarbour.com\xHarbour-Builder"
           IF "%XBUILD_XCC%"=="YES" \xHB\bin\XLIB \xHB\dll\BGD.dll /out:\xHB\lib\BGD.lib
           IF "%XBUILD_VC8%"=="YES" XCOPY BGD.lib \xHB\lib\vc8 /d /y
           )

REM --> ADS.lib
        IF "%_BUILD_ADS%"=="NO" GOTO No_ADS
           CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
           \xHB\bin\XBUILD.EXE ads.lib.xbp  %_XB_Debug% %1
           
           IF "%XBUILD_XCC%"=="YES" XCOPY "%HB_DIR_ADS%\ACE32.dll" \xHB\bin /d /y
           IF "%XBUILD_XCC%"=="YES" \xHB\bin\XLIB \xHB\bin\ace32.dll /out:\xHB\lib\ACE32.lib

           IF "%XBUILD_VC8%"=="YES" XCOPY "%HB_DIR_ADS%\ACE32.dll" \xHB\bin\vc8 /d /y
           IF "%XBUILD_VC8%"=="YES" XCOPY "%HB_DIR_ADS%\ace32.lib" \xHB\lib\vc8 /d /y

        :No_ADS

REM --> xbScript.lib
        IF "%_BUILD_XBSCRIPT_LIB%"=="NO" GOTO No_XBSCRIPT_LIB
           CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
           \xHB\bin\XBUILD.EXE XBScript.lib.xbp %_XB_Debug% %1
        :No_XBSCRIPT_LIB

REM --> TProject.lib
        IF "%_BUILD_TPROJECT_LIB%"=="NO" GOTO No_TProjectLIB
           CD "\xHarbour.com\xHarbour-xBuild\%_XB_Compiler%"
           \xHB\bin\XBUILD.EXE TProject.lib.xbp %_XB_Debug% %1
        :No_TProjectLIB

REM --> ActiveX.lib
         CD "\xHarbour.com\xHarbour-ActiveX\%_XB_Compiler%"
        \xHB\bin\XBUILD.EXE ActiveX.lib.xbp %_XB_Debug% %1

REM --> xEdit*.LIB
        IF "%_BUILD_XEDIT_LIB%"=="NO" GOTO No_xEditLIB
           CD "\xHarbour.com\Visual-xHarbour\xEdit\%_XB_Compiler%"
REM        \xHB\bin\XBUILD.EXE xEdit.lib.xbp        %_XB_Debug% %1
           \xHB\bin\XBUILD.EXE xEditW.lib.xbp       %_XB_Debug% %1
           \xHB\bin\XBUILD.EXE xEditWScript.lib.xbp %_XB_Debug% %1
           \xHB\bin\XBUILD.EXE xEditVXH.lib.xbp     %_XB_Debug% %1
        :No_xEditLIB

REM --> WinCore.lib
        IF "%_BUILD_WINCORE%"=="NO" GOTO No_WinCore
           CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
           \xHB\bin\XBUILD.EXE WinCore.lib.xbp %_XB_Debug% %1
        :No_WinCore

REM --> WINAPI.LIB
        IF "%_BUILD_WINAPI_LIB%"=="NO" GOTO No_WINAPI_LIB
            CD "\xHarbour.com\Visual-xHarbour\Library\%_XB_Compiler%"
            \xHB\bin\XBUILD.EXE WINAPI.LIB.xbp %_XB_Debug% %1

            IF "%_BUILD_PROF%"=="NO" GOTO No_WinApi_Prof
               CD "\xHarbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Professional"
               IF EXIST WINAPI.LIB.xbp \xHB\bin\XBUILD.EXE WINAPI.LIB.xbp %_XB_Debug% %1
            :No_WinApi_Prof

            IF "%_BUILD_DEMO%"=="NO" GOTO No_WinApi_Demo
               CD "\xHarbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Demo"
               \xHB\bin\XBUILD.EXE WINAPI.LIB.xbp %_XB_Debug% %1
            :No_WinApi_Demo
            
        :No_WINAPI_LIB

REM --> Ole
        CD "\xHarbour.com\xHarbour-OleServer\%_XB_Compiler%"
        \xHB\bin\XBUILD.EXE Ole.lib.xbp          %_XB_Debug% %1
        \xHB\bin\XBUILD.EXE OleServer.lib.xbp    %_XB_Debug% %1

        IF "%_BUILD_DEMO%"=="NO" GOTO No_Ole_Demo
           CD "\xHarbour.com\xHarbour-OleServer\%_XB_Compiler%\Demo"
           \xHB\bin\XBUILD.EXE Ole.lib.xbp       %_XB_Debug% %1
           \xHB\bin\XBUILD.EXE OleServer.lib.xbp %_XB_Debug% %1
        :No_Ole_Demo


REM --> ZipArchive & ZLib & HBZlib
        IF "%_BUILD_HBZLIB%"=="NO" GOTO No_HBZlib

           CD "\xharbour.com\ZipArchive"
           \xHB\bin\XBUILD.EXE ZipArchive.lib.xbp -NoXbp -Debug %1

           CD "\xharbour.com\ZipArchive\ZLib"
           \xHB\bin\XBUILD.EXE ZLib.lib.xbp -NoXbp -Debug %1

           CD "\xHarbour.com\xHarbour-HBZLib\%_XB_Compiler%"
           \xHB\bin\XBUILD.EXE xHBzip.lib.xbp %_XB_Debug% %1

           CD "\xHarbour.com\xHarbour-HBZLib\xHbZipDll"
           IF     EXIST "%_XHB_DLL%\xHBZipDll.lib"     DEL "%_XHB_DLL%\xHBZipDll.lib" /Q
           IF     EXIST "%_XHB_LIB%\xHBZipDll.lib"          \xHB\bin\XBUILD.EXE xHBZipDll.dll.xbp -NoXbp %1
           IF NOT EXIST "%_XHB_LIB%\xHBZipDll.lib"          \xHB\bin\XBUILD.EXE xHBZipDll.dll.xbp -NoXbp %1 -link
           IF     EXIST "%_XHB_DLL%\xHBZipDll.lib" MOVE /Y "%_XHB_DLL%\xHBZipDll.lib" "%_XHB_LIB%"

       :No_HBZlib


REM --> SQLRDD
        IF "%_BUILD_SQLRDD%"=="NO" GOTO No_SQLRDD
        
            CD "\xHarbour.com\xHarbour-SQLRDD\%_XB_Compiler%"
            \xHB\bin\XBUILD.EXE SQL.lib.xbp                             %_XB_Debug% %1
            IF "%_BUILD_MT%"=="YES" \xHB\bin\XBUILD.EXE SQLmt.lib.xbp   %_XB_Debug% %1

            IF "%_BUILD_DEMO%"=="NO" GOTO No_SQL_Demo
               CD "\xHarbour.com\xHarbour-SQLRDD\%_XB_Compiler%\Demo"
               \xHB\bin\XBUILD.EXE SQL.lib.xbp                             %_XB_Debug% %1
               IF "%_BUILD_MT%"=="YES" \xHB\bin\XBUILD.EXE SQLmt.lib.xbp   %_XB_Debug% %1
               
            :NO_SQL_Demo
            
        :No_SQLRDD


REM --> REDBFCDX
        IF "%_BUILD_REDBFCDX%"=="NO" GOTO No_REDBFCDX
           CD "\xHarbour"
           COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\redbfcdx.lib.xbp" "\xHarbour\redbfcdx.lib.xbp"    /y
           \xHB\bin\XBUILD.EXE REDBFCDX.lib.xbp %_XB_Debug% %1
        :No_REDBFCDX


REM --> BMDBFCDX
        IF "%_BUILD_BMDBFCDX%"=="NO" GOTO No_BMDBFCDX
           CD "\xHarbour"
           COPY "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\bmdbfcdx.lib.xbp" "\xHarbour\bmdbfcdx.lib.xbp"    /y
           \xHB\bin\XBUILD.EXE BMDBFCDX.lib.xbp %_XB_Debug% %1
        :No_BMDBFCDX


REM --> ApolloRDD
        IF "%_BUILD_APOLLORDD%"=="NO" GOTO No_ApolloRDD
            CD "\xHarbour.com\xHarbour-ApolloRDD\%_XB_Compiler%"
            \xHB\bin\XBUILD.EXE Six.lib.xbp %_XB_Debug% %1

            IF "%_BUILD_DEMO%"=="NO" GOTO No_Apollo_Demo
               CD "\xHarbour.com\xHarbour-ApolloRDD\%_XB_Compiler%\Demo"
               \xHB\bin\XBUILD.EXE Six.lib.xbp %_XB_Debug% %1
            :No_Apollo_Demo

            IF NOT EXIST "%_XHB_LIB%\fts32.lib" \xHB\bin\XLIB "%_XHB_DLL%\ApolloRDD\fts32.dll" /out:"%_XHB_LIB%\FTS32.lib"
            IF NOT EXIST "%_XHB_LIB%\sde61.lib" \xHB\bin\XLIB "%_XHB_DLL%\ApolloRDD\sde61.dll" /out:"%_XHB_LIB%\SDE61.lib"
         
         :No_ApolloRDD


REM --> xHBComm
        IF "%_BUILD_XHBCOMM%"=="NO" GOTO No_xHBComm
            CD "\xHarbour.com\xHarbour-xHBComm\Comm"
            \xHB\bin\XBUILD.EXE Comm.lib.xbp -NoXbp -Debug %1

            CD "\xHarbour.com\xHarbour-xHBComm\xHBCommDll"
            IF     EXIST "%_XHB_DLL%\xHBCommDll.lib"     DEL "%_XHB_DLL%\xHBCommDll.lib" /Q
            IF     EXIST "%_XHB_LIB%\xHBCommDll.lib"          \xHB\bin\XBUILD.EXE xHBCommDll.dll.xbp -NoXbp %1
            IF NOT EXIST "%_XHB_LIB%\xHBCommDll.lib"          \xHB\bin\XBUILD.EXE xHBCommDll.dll.xbp -NoXbp -link
            IF     EXIST "%_XHB_DLL%\xHBCommDll.lib" MOVE /Y "%_XHB_DLL%\xHBCommDll.lib" "%_XHB_LIB%"

            CD "\xHarbour.com\xHarbour-xHBComm\%_XB_Compiler%"
            \xHB\bin\XBUILD.EXE xHBComm.lib.xbp %_XB_Debug% %1
            
        :No_xHBComm


REM --> CT3-Comm
        IF "%_BUILD_CT3COMM%"=="NO" GOTO No_CT3Comm
           CD "\xHarbour.com\xHarbour-CT3Comm\%_XB_Compiler%"
           \xHB\bin\XBUILD.EXE CT3Comm.lib.xbp %_XB_Debug% %1
        :No_CT3Comm


REM --> RMDBFCDX.lib
        CD "\xHarbour.com\xHarbour-Rushmore\%_XB_Compiler%"
        \xHB\bin\XBUILD.EXE RMDBFCDX.lib.xbp %_XB_Debug% %1


REM --> XDO
        IF "%_BUILD_XDO_DLL%"=="NO" GOTO No_XDO_DLL
            CD "\xHarbour.com\xHarbour-XDO\%_XB_Compiler%"
            IF EXIST "%_XHB_DLL%\XDO.lib" DEL "%_XHB_DLL%\XDO.lib" /Q
            \xHB\bin\XBUILD.EXE XDO.dll.xbp %_XB_NonDebug% %1
            IF EXIST "%_XHB_DLL%\XDO.lib" MOVE /Y "%_XHB_DLL%\XDO.lib" "%_XHB_LIB%"
        :No_XDO_DLL


REM --> IEGui
        IF "%_BUILD_IEGUI_LIB%"=="NO" GOTO No_IEGUI_LIB
           CD "\xHarbour.com\IEGui\%_XB_Compiler%"
           \xHB\bin\XBUILD.EXE IEGui.lib.xbp %_XB_Debug% %1
        :No_IEGUI_LIB


REM --> FreeImage
        CD "\xHarbour.com\FreeImage\%_XB_Compiler%"
        \xHB\bin\XBUILD.EXE FreeImage-xHB.lib.xbp %_XB_Debug% %1
         XCOPY FreeImage.lib "%_XHB_LIB%" /d /y
         XCOPY FreeImage.dll "%_XHB_DLL%" /d /y


REM --> VXH.lib
        IF "%_BUILD_VXH_AS%"=="NONE" GOTO No_VXHLib
        IF "%_BUILD_VXH_AS%"=="NO"   GOTO No_VXHLib
        
             CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%"
            \xHB\bin\XBUILD.EXE VXH.lib.xbp %_XB_Debug% %1

            IF "%_BUILD_DEMO%"=="NO" GOTO N433
            CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Personal"
               IF EXIST VXH.lib.xbp \xHB\bin\XBUILD.EXE VXH.lib.xbp %_XB_Debug% %1
            :N433

            IF "%_BUILD_PERSONAL%"=="NO" GOTO N438
            CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Professional"
               IF EXIST VXH.lib.xbp \xHB\bin\XBUILD.EXE VXH.lib.xbp %_XB_Debug% %1
            :N438

            IF "%_BUILD_PROF%"=="NO" GOTO N443
               CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Demo"
               IF EXIST VXH.lib.xbp \xHB\bin\XBUILD.EXE VXH.lib.xbp %_XB_Debug% %1
            :N443

            
         :No_VXHLib


REM --> VXH.DLL
         IF "%_BUILD_VXHDLL%"=="NO"   GOTO No_VXHDLL
           CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%"

            IF     EXIST "%_XHB_BIN%\vxhdll.lib"       DEL "%_XHB_BIN%\vxhdll.lib" /Q
            IF     EXIST "%_XHB_LIB%\vxhdll.lib"            \xHB\bin\XBUILD.EXE VXHDll.dll.xbp %_XB_NonDebug% %1
            IF NOT EXIST "%_XHB_LIB%\vxhdll.lib"            \xHB\bin\XBUILD.EXE VXHDll.dll.xbp %_XB_NonDebug% %1 -link
            IF     EXIST "%_XHB_BIN%\vxhdll.lib"   MOVE /Y "%_XHB_BIN%\vxhdll.lib" "%_XHB_LIB%"

            IF "%_BUILD_DEBUG%"=="NO" GOTO N472
               IF     EXIST "%_XHB_BIN%\vxhddll.lib"      DEL "%_XHB_BIN%\vxhddll.lib" /Q
               IF     EXIST "%_XHB_LIB%\vxhddll.lib"           \xHB\bin\XBUILD.EXE VXHdDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\vxhddll.lib"           \xHB\bin\XBUILD.EXE VXHdDll.dll.xbp %_XB_NonDebug% %1 -link
               IF     EXIST "%_XHB_BIN%\vxhddll.lib"  MOVE /Y "%_XHB_BIN%\vxhddll.lib" "%_XHB_LIB%"
            :N472

            IF "%_BUILD_PROF%"=="NO" GOTO N467
               CD "\xharbour.com\Visual-xHarbour\Library\%_XB_Compiler%\Professional"

               IF     EXIST "%_XHB_BIN%\Professional\vxhdll.lib"       DEL "%_XHB_BIN%\Professional\vxhdll.lib" /Q
               IF     EXIST "%_XHB_LIB%\Professional\vxhdll.lib"            \xHB\bin\XBUILD.EXE VXHDll.dll.xbp %_XB_NonDebug% %1
               IF NOT EXIST "%_XHB_LIB%\Professional\vxhdll.lib"            \xHB\bin\XBUILD.EXE VXHDll.dll.xbp %_XB_NonDebug% %1 -link
               IF     EXIST "%_XHB_BIN%\Professional\vxhdll.lib"   MOVE /Y "%_XHB_BIN%\Professional\vxhdll.lib" "%_XHB_LIB%\Professional"
               
               IF "%_BUILD_DEBUG%"=="NO" GOTO N487
                  IF     EXIST "%_XHB_BIN%\Professional\vxhddll.lib"      DEL "%_XHB_BIN%\Professional\vxhddll.lib" /Q
                  IF     EXIST "%_XHB_LIB%\Professional\vxhddll.lib"           \xHB\bin\XBUILD.EXE VXHdDll.dll.xbp %_XB_NonDebug% %1
                  IF NOT EXIST "%_XHB_LIB%\Professional\vxhddll.lib"           \xHB\bin\XBUILD.EXE VXHdDll.dll.xbp %_XB_NonDebug% %1 -link
                  IF     EXIST "%_XHB_BIN%\Professional\vxhddll.lib"  MOVE /Y "%_XHB_BIN%\Professional\vxhddll.lib" "%_XHB_LIB%\Professional%"
               :N487
               
            :N467
           
         :No_VXHDLL


REM --> DebugServer
        CD "\xHarbour.com\xHarbour-DebugServer\server\%_XB_Compiler%"
        \xHB\bin\XBUILD.EXE DbgServe.lib.xbp %_XB_Debug% %1


REM --> VXHDebugger
        IF "%_BUILD_DBG_CLIENT%"=="NO" GOTO No_DebugClient
           CD "\xHarbour.com\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%"
           \xHB\bin\XBUILD.EXE VXHD.lib.xbp %_XB_Debug% %1
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
           \xHB\bin\XBUILD.EXE xBuildW.exe.xbp %_XB_Exe% %1
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
              \xHB\bin\XBUILD.EXE xBuildW.exe.xbp %_XB_Exe% %1
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
              \xHB\bin\XBUILD.EXE xBuildW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Demo\xBuildW.lib" DEL "%_XHB_BIN%\Demo\xBuildW.lib" /Q
           :No_xBuildW_Demo
        :No_xBuildW


REM --> xPrompt
        IF "%_BUILD_XPROMPT_AS%"=="NONE"   GOTO No_XPROMPT
        IF "%_BUILD_XPROMPT_AS%"=="NO"     GOTO No_XPROMPT
        
            IF "%_BUILD_XPROMPT_AS%"=="EXE" GOTO No_XPROMPTDLL
               CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%"
               \xHB\bin\XBUILD.EXE xPrompt.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPrompt.lib" DEL "%_XHB_BIN%\xPrompt.lib" /Q
               \xHB\bin\XBUILD.EXE xPromptSQL.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPromptSQL.lib" DEL "%_XHB_BIN%\xPromptSQL.lib" /Q
            :No_XPROMPTDLL

            IF "%_BUILD_XPROMPT_AS%"=="DLL" GOTO No_XPROMPTEXE
               CD "\xHarbour.com\xHarbour-Builder\%_XB_Compiler%\XPROMPT.exe"
               \xHB\bin\XBUILD.EXE xPrompt.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPrompt.lib" DEL "%_XHB_BIN%\xPrompt.lib" /Q
               \xHB\bin\XBUILD.EXE xPromptSQL.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xPromptSQL.lib" DEL "%_XHB_BIN%\xPromptSQL.lib" /Q
            :No_XPROMPTEXE
            
        :No_XPROMPT


REM --> VXH.EXE
         IF "%_BUILD_VXH_AS%"=="NONE"   GOTO No_VXH
         IF "%_BUILD_VXH_AS%"=="NO"     GOTO No_VXH
         
            IF "%_BUILD_VXH_AS%"=="EXE"    GOTO No_VXHDLL

               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%"
               \xHB\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\vxh.lib" DEL "%_XHB_BIN%\vxh.lib" /Q
               
               IF "%_BUILD_PERSONAL%"=="NO" GOTO N568
                  CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Personal"
                  IF EXIST VXH.exe.xbp \xHB\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
                  IF EXIST "%_XHB_BIN%\Personal\vxh.lib" DEL "%_XHB_BIN%\Personal\vxh.lib" /Q
               :N568

               IF "%_BUILD_PROF%"=="NO" GOTO N574
               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Professional"
                  IF EXIST VXH.exe.xbp \xHB\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
                  IF EXIST "%_XHB_BIN%\Professional\vxh.lib" DEL "%_XHB_BIN%\Professional\vxh.lib" /Q
               :N574
               
            :No_VXHDLL

            IF "%_BUILD_VXH_AS%"=="DLL" GOTO No_VXHEXE

               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\vxh.exe"
               \xHB\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\vxh.lib" DEL "%_XHB_BIN%\vxh.lib" /Q

               IF "%_BUILD_PERSONAL%"=="NO" GOTO N588
               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Personal\vxh.exe"
                  IF EXIST VXH.exe.xbp \xHB\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
                  IF EXIST "%_XHB_BIN%\Personal\vxh.lib" DEL "%_XHB_BIN%\Personal\vxh.lib" /Q
               :N588

               IF "%_BUILD_PROF%"=="NO" GOTO N594
                  CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Professional\vxh.exe"
                  IF EXIST VXH.exe.xbp \xHB\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
                  IF EXIST "%_XHB_BIN%\Professional\vxh.lib" DEL "%_XHB_BIN%\Professional\vxh.lib" /Q
               :N594
               
            :No_VXHEXE

            IF "%_BUILD_DEMO%"=="NO" GOTO N602
               CD "\xHarbour.com\Visual-xHarbour\IDE\%_XB_Compiler%\Demo"
               \xHB\bin\XBUILD.EXE VXH.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Demo\vxh.lib" DEL "%_XHB_BIN%\Demo\vxh.lib" /Q
            :N602
            
         :No_VXH


REM --> xDebugW
         IF "%_BUILD_xDebugW_AS%"=="NONE"   GOTO No_xDebugW
         IF "%_BUILD_xDebugW_AS%"=="NO"     GOTO No_xDebugW
         
            IF "%_BUILD_xDebugW_AS%"=="EXE"    GOTO No_xDebugWDLL
               CD "\xHarbour.com\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%"
               \xHB\bin\XBUILD.EXE xDebugW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xDebugW.lib" DEL "%_XHB_BIN%\xDebugW.lib" /Q
            :No_xDebugWDLL

            IF "%_BUILD_xDebugW_AS%"=="DLL" GOTO No_xDebugWEXE
               CD "\xHarbour.com\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%\xDebugW.exe"
               \xHB\bin\XBUILD.EXE xDebugW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\xDebugW.lib" DEL "%_XHB_BIN%\xDebugW.lib" /Q
            :No_xDebugWEXE

            IF "%_BUILD_DEMO%"=="NO" GOTO No_xDebugWExe_Demo
               CD "\xHarbour.com\xHarbour-DebugClient\vxhdebug\%_XB_Compiler%\Demo"
               \xHB\bin\XBUILD.EXE xDebugW.exe.xbp %_XB_Exe% %1
               IF EXIST "%_XHB_BIN%\Demo\xDebugW.lib" DEL "%_XHB_BIN%\Demo\xDebugW.lib" /Q
            :No_xDebugWExe_Demo
            
        :No_xDebugW


REM --> xEditW
          IF "%_BUILD_xEditW_AS%"=="NONE"   GOTO No_xEditW
          IF "%_BUILD_xEditW_AS%"=="NO"     GOTO No_xEditW
          
              IF "%_BUILD_xEditW_AS%"=="EXE" GOTO No_xEditWDLL
                 CD "\xHarbour.com\Visual-xHarbour\xEdit\%_XB_Compiler%"
                 \xHB\bin\XBUILD.EXE xEditW.exe.xbp %_XB_Exe% %1
                 IF EXIST "%_XHB_BIN%\xEditW.lib" DEL "%_XHB_BIN%\xEditW.lib" /Q
              :No_xEditWDLL

              IF "%_BUILD_xEditW_AS%"=="DLL" GOTO No_xEditWEXE
                 CD "\xHarbour.com\Visual-xHarbour\xEdit\%_XB_Compiler%\xEditW.exe"
                 \xHB\bin\XBUILD.EXE xEditW.exe.xbp %_XB_Exe% %1
                 IF EXIST "%_XHB_BIN%\xEditW.lib" DEL "%_XHB_BIN%\xEditW.lib" /Q
                 IF EXIST "%_XHB_BIN%\xEditW.lib" DEL "%_XHB_BIN%\xEditW.lib" /Q
              :No_xEditWEXE
          :No_xEditW


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