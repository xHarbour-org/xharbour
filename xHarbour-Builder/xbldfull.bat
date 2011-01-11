ECHO OFF

REM --> Lets time this build
    TIME /T > xbldfull.log


REM --> Save Environment
    SET _PATH=%PATH%
REM SET _DJGPP=%DJGPP%
    SET _INCLUDE=%INCLUDE%
    SET _LIB=%LIB%


REM --> Set Environment
    IF "%PROG_PATH%"==""         SET PROG_PATH=%ProgramFiles%
    IF "%BCCDIR%" == ""          SET BCCDIR=\BCC55

    SET INCLUDE=
    SET LIB=

    IF EXIST xbuild.ini         DEL xBuild.ini > NUL:
    IF EXIST xbuild.windows.ini DEL xBuild.Windows.ini > NUL:


REM --> Cleanup for -ALL
    IF "%1"=="-all" GOTO CLEAN_ALL
    IF "%1"=="-All" GOTO CLEAN_ALL
    IF "%1"=="-ALL" GOTO CLEAN_ALL

    GOTO AFTER_CLEAN_ALL

    :CLEAN_ALL

    ATTRIB +r \xhb\bin\xcc.*
    ATTRIB +r \xhb\bin\xrc.*
    ATTRIB +r \xhb\bin\xlib.exe
    ATTRIB +r \xhb\c_lib\*.lib /s
    ATTRIB +r \xhb\bin\xbuild.exe

    IF "%XBUILD_XCC%"=="YES" DEL \xhb\bin\*.exe /s
    IF "%XBUILD_XCC%"=="YES" DEL \xhb\bin\*.map /s
    IF "%XBUILD_XCC%"=="YES" DEL \xhb\bin\*.exp /s
    IF "%XBUILD_XCC%"=="YES" RD  \xhb\bin\Demo  /s /q
    IF "%XBUILD_XCC%"=="YES" RD  \xhb\bin\Personal /s /q

    IF "%XBUILD_VC8%"=="YES" RD \xhb\bin\vc8 /s /q
    IF "%XBUILD_VC8%"=="YES" MD \xhb\bin\vc8
    IF "%XBUILD_BC5%"=="YES" RD \xhb\bin\bc5 /s /q
    IF "%XBUILD_BC5%"=="YES" MD \xhb\bin\bc5

    ATTRIB -r \xhb\bin\xbuild.exe

    IF "%XBUILD_XCC%"=="YES" DEL \xhb\dll\*.dll /s
    IF "%XBUILD_XCC%"=="YES" RD  \xhb\dll\Ads /s /q
    IF "%XBUILD_XCC%"=="YES" RD  \xhb\dll\ApolloRDD /s /q
    IF "%XBUILD_XCC%"=="YES" RD  \xhb\dll\SQLRDD /s /q
    IF "%XBUILD_XCC%"=="YES" DEL \xhb\lib\*.lib /s
    IF "%XBUILD_XCC%"=="YES" RD  \xhb\lib\demo /s /q

    IF "%XBUILD_VC8%"=="YES" RD \xhb\dll\vc8 /s /q
    IF "%XBUILD_VC8%"=="YES" RD \xhb\lib\vc8 /s /q
    IF "%XBUILD_VC8%"=="YES" MD \xhb\lib\vc8
    IF "%XBUILD_BC5%"=="YES" RD \xhb\dll\bc5 /s /q
    IF "%XBUILD_BC5%"=="YES" RD \xhb\lib\bc5 /s /q
    IF "%XBUILD_BC5%"=="YES" MD \xhb\lib\bc5

    DEL \xharbour\bin\xcc.*
    DEL \xharbour\bin\xrc.*
    DEL \xharbour\bin\xlib.exe
    DEL \xharbour\bin\xlink.exe
    DEL \xharbour\bin\xhb.exe

    IF EXIST \xhb\include             RD \xhb\include /s /q
    IF EXIST \xharbour\c_include      RD \xharbour\c_include /s /q
    IF EXIST \xharbour\c_lib          RD \xharbour\c_lib /s /q

    REM We will use only \xharbour\lib\vc or bc5 so make sure that's the case
    REM when done using make_vc all or make_b32 all will quickly copy them back!
    DEL \xharbour\lib\*.lib

    REM Clean up XBP
    RD \xbp

    :AFTER_CLEAN_ALL



REM --> Copy files

    IF NOT EXIST "\xhb\lib"         MD "\xhb\lib"
    IF NOT EXIST "\xhb\dll"         MD "\xhb\dll"
    IF NOT EXIST "\xhb\include\w32" MD "\xhb\include\w32"

    REM ** SQLRDD **
    IF NOT EXIST \xhb\dll\SQLRDD MD \xhb\dll\SQLRDD
    XCOPY \xharbour.com\xHarbour-SQLRDD\dll\*.dll \xhb\dll\SQLRDD /d /y
    XCOPY \xharbour.com\xHarbour-SQLRDD\lib\*.lib \xhb\lib /d /y
    XCOPY \xHarbour.com\xHarbour-SQLRDD\include   \xHb\include /d /y /i
    IF EXIST \xHB\include\sqlrdd.xns DEL \xHB\include\sqlrdd.xns /Q


    REM ** ADS **
    REM HB_DIR_ADS is the ONE place ace32.dll SHOULD be in.
    IF NOT EXIST \xhb\dll\ADS MD \xhb\dll\ADS
    XCOPY "%HB_DIR_ADS%\Ace32.dll"       \xhb\dll\ADS /d /y
    XCOPY "%HB_DIR_ADS%\AdsLoc32.dll"    \xhb\dll\ADS /d /y
    XCOPY "%HB_DIR_ADS%\AXCws32.dll"     \xhb\dll\ADS /d /y
    XCOPY "%HB_DIR_ADS%\Ansi.chr"        \xhb\dll\ADS /d /y
    XCOPY "%HB_DIR_ADS%\Extend.chr"      \xhb\dll\ADS /d /y
    XCOPY "%HB_DIR_ADS%\AdsLocal.cfg"    \xhb\dll\ADS /d /y
    XCOPY \xHarbour\contrib\rdd_ads\*.ch \xHb\include /d /y /i

    REM ** FreeImage **
    IF NOT EXIST \xhb\lib\vc8 MD \xhb\lib\vc8
    XCOPY \xHarbour.com\FreeImage\FreeImage.lib    \xhb\lib\     /d /y /i
    XCOPY \xHarbour.com\FreeImage\FreeImage.lib    \xhb\lib\vc8  /d /y /i
    XCOPY \xHarbour.com\FreeImage\FreeImage.dll    \xhb\dll\     /d /y /i
    XCOPY \xHarbour\contrib\FreeImage\include\*.ch \xhb\include\ /d /y /i
    XCOPY \xHarbour\contrib\FreeImage\include\*.h  \xhb\include\ /d /y /i


    REM ** ApolloRDD **
    IF NOT EXIST \xhb\dll\ApolloRDD MD \xhb\dll\ApolloRDD
    XCOPY \xHarbour.com\xHarbour-ApolloRDD\dll\*.dll \xhb\dll\ApolloRDD /d /y /i
    XCOPY \xHarbour.com\xHarbour-ApolloRDD\*.h  \xHb\include /d /y /i
    XCOPY \xHarbour.com\xHarbour-ApolloRDD\*.ch \xHb\include /d /y /i


    REM ** BGD.DLL **
    XCOPY \xHarbour.com\xHarbour-Builder\BGD.DLL \xhb\dll  /d /y /i


    XCOPY \xHarbour\include\*.api                       \xHb\include /d /y /i
    XCOPY \xHarbour\include\*.ch                        \xHb\include /d /y /i
    XCOPY \xHarbour\include\*.h                         \xHb\include /d /y /i
    XCOPY \xHarbour\include\*.c                         \xHb\include /d /y /i

    XCOPY \xHarbour\source\rtl\pcre\*.h                 \xHb\include /d /y /i
    XCOPY \xHarbour\source\rtl\pcre\*.generic           \xHb\include /d /y /i
    XCOPY \xHarbour\obj\vc\hbverbld.h                   \xHb\include /d /y /i
    XCOPY \xHarbour\contrib\gd\include                  \xHb\include /d /y /i

    XCOPY \xHarbour.com\xHarbour-Builder\include        \xHb\include /d /y /i
    XCOPY \xHarbour.com\xHarbour-AxtiveX\ole.ch         \xHb\include\w32 /d /y
    XCOPY \xHarbour.com\Visual-xHarbour\library\include \xHb\include\w32 /d /y /i
    XCOPY \xHarbour.com\IEGui\iegui.ch                  \xHb\include /d /y
    XCOPY \xHarbour.com\xHarbour-Builder\xcc*.lib       \xHarbour.com\xHarbour-XCC\xcc\xcc*.lib /d /y /i


    IF EXIST \xHarbour\include\Ado.ch      DEL \xHarbour\include\Ado.ch
    IF EXIST \xHarbour\include\Colors.ch   DEL \xHarbour\include\Colors.ch
    IF EXIST \xHarbour\include\CommCtrl.ch DEL \xHarbour\include\CommCtrl.ch
    IF EXIST \xHarbour\include\CommDlg.ch  DEL \xHarbour\include\CommDlg.ch
    IF EXIST \xHarbour\include\Debug.ch.ch DEL \xHarbour\include\Debug.ch.ch
    IF EXIST \xHarbour\include\Import.ch   DEL \xHarbour\include\Import.ch
    IF EXIST \xHarbour\include\Ole.ch      DEL \xHarbour\include\Ole.ch
    IF EXIST \xHarbour\include\RichEdit.ch DEL \xHarbour\include\RichEdit.ch
    IF EXIST \xHarbour\include\SQLTypes.ch DEL \xHarbour\include\SQLTypes.ch
    IF EXIST \xHarbour\include\VXH.ch      DEL \xHarbour\include\VXH.ch
    IF EXIST \xHarbour\include\What32.ch   DEL \xHarbour\include\What32.ch
    IF EXIST \xHarbour\include\WinApi.ch   DEL \xHarbour\include\WinApi.ch
    IF EXIST \xHarbour\include\WinGDI.ch   DEL \xHarbour\include\WinGDI.ch
    IF EXIST \xHarbour\include\WinInet.ch  DEL \xHarbour\include\WinInet.ch
    IF EXIST \xHarbour\include\WinReg.ch   DEL \xHarbour\include\WinReg.ch
    IF EXIST \xHarbour\include\WinStruc.ch DEL \xHarbour\include\WinStruc.ch
    IF EXIST \xHarbour\include\WinUser.ch  DEL \xHarbour\include\WinUser.ch

    IF EXIST \xHB\include\Ado.ch           DEL \xHB\include\Ado.ch
    IF EXIST \xHB\include\Colors.ch        DEL \xHB\include\Colors.ch
    IF EXIST \xHB\include\CommCtrl.ch      DEL \xHB\include\CommCtrl.ch
    IF EXIST \xHB\include\CommDlg.ch       DEL \xHB\include\CommDlg.ch
    IF EXIST \xHB\include\Debug.ch.ch      DEL \xHB\include\Debug.ch.ch
    IF EXIST \xHB\include\Import.ch        DEL \xHB\include\Import.ch
    IF EXIST \xHB\include\Ole.ch           DEL \xHB\include\Ole.ch
    IF EXIST \xHB\include\RichEdit.ch      DEL \xHB\include\RichEdit.ch
    IF EXIST \xHB\include\SQLTypes.ch      DEL \xHB\include\SQLTypes.ch
    IF EXIST \xHB\include\VXH.ch           DEL \xHB\include\VXH.ch
    IF EXIST \xHB\include\What32.ch        DEL \xHB\include\What32.ch
    IF EXIST \xHB\include\WinApi.ch        DEL \xHB\include\WinApi.ch
    IF EXIST \xHB\include\WinGDI.ch        DEL \xHB\include\WinGDI.ch
    IF EXIST \xHB\include\WinInet.ch       DEL \xHB\include\WinInet.ch
    IF EXIST \xHB\include\WinReg.ch        DEL \xHB\include\WinReg.ch
    IF EXIST \xHB\include\WinStruc.ch      DEL \xHB\include\WinStruc.ch
    IF EXIST \xHB\include\WinUser.ch       DEL \xHB\include\WinUser.ch
    IF EXIST \xHB\include\W32\WinTypes.ch  DEL \xHB\include\W32\WinTypes.ch


REM  ===============================================
REM  ===============================================
REM  ===============================================
REM  ===============================================


IF "%XBUILD_XCC%"=="YES" CALL \xHarbour.com\xHarbour-Builder\xbldfull-XCC.bat %1
IF "%XBUILD_XCC%"=="YES" CALL \xHarbour.com\xHarbour-Builder\xbldfull2.bat %1

IF "%XBUILD_VC8%"=="YES" CALL \xHarbour.com\xHarbour-Builder\xbldfull-VC8.bat %1
IF "%XBUILD_VC8%"=="YES" CALL \xHarbour.com\xHarbour-Builder\xbldfull2.bat %1

IF "%XBUILD_BC5%"=="YES" CALL \xHarbour.com\xHarbour-Builder\xbldfull-BC5.bat %1
IF "%XBUILD_BC5%"=="YES" CALL \xHarbour.com\xHarbour-Builder\xbldfull2.bat %1


REM  ===============================================
REM  ===============================================
REM  ===============================================
REM  ===============================================




REM  ===============================================
ECHO Restore Environment
REM  ===============================================
    SET PATH=%_PATH%
REM SET DJGPP=%_DJGPP%
    SET INCLUDE=%_INCLUDE%
    SET LIB=%_LIB%

    SET _PATH=
REM SET _DJGPP=
    SET _INCLUDE=
    SET _LIB=

    SET _XB_NonDebug=
    SET _XB_Debug=
    SET _XB_Exe=

    SET _XB_Compiler=

    SET _XHB_LIB=
    SET _XHB_BIN=
    SET _XHB_DLL=

    SET _XHARBOUR_ROOT=
    SET _XHARBOUR_LIB=
    SET _XHARBOUR_BIN=
    SET _XHARBOUR_XBP=
    SET _XHARBOUR_XBP_DEMO=
    SET _XHARBOUR_XBP_PERS=

    SET VC8_MT=
    SET VC8_DEMO=
    SET VC8_PERSONAL=
    SET VC8_XHB.EXE=
    SET VC8_XHB.LIB=
    SET VC8_XHB.DLL=
    SET VC8_CORELIBS=
    SET VC8_CONTRIB=
    SET VC8_DMAIN.LIB=
    SET VC8_VXHDLL=
    SET VC8_XBUILDW_AS=
    SET VC8_VXH_AS=
    SET VC8_XEDITW_AS=
    SET VC8_XDEBUGW_AS=
    SET VC8_XPROMPT_AS=
    SET VC8_DEBUG=

    SET XCC_MT=
    SET XCC_DEMO=
    SET XCC_PERSONAL=
    SET XCC_XHB.EXE=
    SET XCC_XHB.LIB=
    SET XCC_XHB.DLL=
    SET XCC_CORELIBS=
    SET XCC_CONTRIB=
    SET XCC_DMAIN.LIB=
    SET XCC_VXHDLL=
    SET XCC_XBUILDW_AS=
    SET XCC_VXH_AS=
    SET XCC_XEDITW_AS=
    SET XCC_XDEBUGW_AS=
    SET XCC_XPROMPT_AS=
    SET XCC_DEBUG=

    SET _BUILD_MT=
    SET _BUILD_DEMO=
    SET _BUILD_PERSONAL=
    SET _BUILD_XHB.EXE=
    SET _BUILD_XHB.LIB=
    SET _BUILD_XHB.DLL=
    SET _BUILD_CORE=
    SET _BUILD_CONTRIB=
    SET _BUILD_DMAIN.LIB=
    SET _BUILD_VXHDLL=
    SET _BUILD_XBUILDW_AS=
    SET _BUILD_VXH_AS=
    SET _BUILD_XEDITW_AS=
    SET _BUILD_XDEBUGW_AS=
    SET _BUILD_XPROMPT_AS=

    SET _BUILD_ACTIVEX=
    SET _BUILD_ADS=
    SET _BUILD_APOLLORDD=
    SET _BUILD_CT3COMM=
    SET _BUILD_DBG_CLIENT=
    SET _BUILD_HBZLIB=
    SET _BUILD_OLE=
    SET _BUILD_RMDBFCDX=
    SET _BUILD_SQLRDD=
    SET _BUILD_BMDBFCDX=
    SET _BUILD_REDEBFCD=
    SET _BUILD_TPROJECT.LIB=
    SET _BUILD_WINAPI.LIB=
    SET _BUILD_WINCORE=
    SET _BUILD_XBSCRIPT.LIB=
    SET _BUILD_XBUILD=
    SET _BUILD_XDO.DLL=
    SET _BUILD_XEDIT.LIB=
    SET _BUILD_XHBCOMM=

    SET _XB_Echo=
    SET _XB_Exe=

REM XCOPY \xHb\bin\xbuild.exe \xHarbour\bin       /d /y
REM XCOPY \xHb\bin\xhb.exe    \xHarbour\bin       /d /y
REM XCOPY \xHb\bin\xcc.exe    \xHarbour\bin       /d /y
REM XCOPY \xHb\bin\xcc.dll    \xHarbour\bin       /d /y
REM XCOPY \xHb\bin\xrc.exe    \xHarbour\bin       /d /y
REM XCOPY \xHb\bin\xrc.dll    \xHarbour\bin       /d /y
REM XCOPY \xHb\bin\xlib.exe   \xHarbour\bin       /d /y
REM XCOPY \xHb\bin\xlink.exe  \xHarbour\bin       /d /y
REM XCOPY \xHb\lib\*.lib      \xHarbour\lib       /d /y /s
REM XCOPY \xhb\dll\*.dll      %windir%\system32   /d /y /u

    CD \xHarbour.com\xHarbour-Builder

    TIME /T >> xbldfull.log

:Done1

