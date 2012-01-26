DEL C:\xHB-SetupFiles\xHB.back*.aip /Q

REM SET XHBTO=C:\xHB-SetupFiles\xHB-%1
    SET XHBTO=C:\xHB-SetupFiles\xHB-Files

    C:
    RD "%XHBTO%" /S /Q
    MD "%XHBTO%"
    CD "%XHBTO%"

REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM                  /Common/..
REM ===============================================

    SET RC_From=W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Common.Files
    SET RC_To=%XHBTO%
    SET RC_Exclude_Folder=CVS CVSROOT .SVN
    SET RC_Exclude_File=.cvsignore
    ROBOCOPY "%RC_From%" "%RC_To%" *.* /NS /NC /NP /XF %RC_Exclude_File% /S /XD %RC_Exclude_Folder%


REM ===============================================
REM                /Common/bin
REM ===============================================


    SET RC_To=%XHBTO%\Bin

    SET RC_Include=xcc.* xrc.* xHB.exe xLib.exe xLink.exe xEditW.exe xHBDll.dll xPrompt.exe
    ROBOCOPY "C:\xHB\Bin" "%RC_To%" %RC_Include% /NS /NC /NP
    ROBOCOPY "W:\xHarbour.com\Visual-xHarbour\xEdit" "%RC_To%" *.xbs /NS /NC /NP

    COPY "W:\xHarbour.com\xHarbour-Builder\Dll2Lib.bat" "%RC_To%"
    COPY "C:\xHB\Dll\ads\ace32.dll"       "%RC_To%"
    COPY "C:\xHB\Dll\SQLRDD\fbclient.dll" "%RC_To%"
    COPY "C:\xHB\Dll\SQLRDD\libmysql.dll" "%RC_To%"
    COPY "C:\xHB\Dll\FreeImage.dll"       "%RC_To%"

REM ===============================================
REM                /Common/c_include
REM ===============================================

    SET RC_To=%XHBTO%\c_include
    SET RC_Exclude_File=.cvsignore
    SET RC_Exclude_Folder=CVS CVSROOT .SVN

    SET RC_From=C:\xHB\c_include
    ROBOCOPY "%RC_From%" "%RC_To%" *.* /NS /NC /NP /S /XF %RC_Exclude_File% /XD %RC_Exclude_Folder%

    SET RC_From=W:\xHarbour\contrib\rdd_ads
    ROBOCOPY "%RC_From%" "%RC_To%" *.h /NS /NC /NP /S /XD %RC_Exclude_Folder%


REM ===============================================
REM                /Common/c_lib
REM ===============================================


    SET RC_To=%XHBTO%\c_lib

    SET RC_From=C:\xHB\c_lib
    SET RC_Exclude_File=.cvsignore
    SET RC_Exclude_Folder=CVS CVSROOT .SVN
    ROBOCOPY "%RC_From%" "%RC_To%" *.* /NS /NC /NP /S /XF %RC_Exclude_File% /XD %RC_Exclude_Folder%



REM ===============================================
REM                /Common/doc
REM ===============================================

    SET RC_To=%XHBTO%\Doc

    MD "%RC_To%\Ads"
    MD "%RC_To%\SQLRDD"
    MD "%RC_To%\HBZLib"
    MD "%RC_To%\xHBComm"
    MD "%RC_To%\HBZLib"
    MD "%RC_To%\ApolloRDD"

    SET XHB-DOC=W:\xHarbour.com\xHarbour-Documentation

    SET RC_Exclude_File=.cvsignore
    SET RC_Exclude_Folder=CVS CVSROOT .SVN
    ROBOCOPY "W:\xHarbour\contrib\gd\doc" "%RC_To%\GD" *.* /NS /NC /NP /S /XF %RC_Exclude_File% /XD %RC_Exclude_Folder%

    COPY "%XHB-DOC%\Getting-Started.pdf" "%RC_To%\Getting Started with xHarbour Builder.pdf"
    COPY "%XHB-DOC%\VXH Getting Started 1.0\VXH-Getting-Started.pdf" "%RC_To%\Getting Started with Visual xHarbour.pdf"
    COPY "W:\xHarbour.com\xHarbour-Builder\Wise Install Scripts (xHBDocs)\Files\xHarbour Language Reference Guide.chm" "%RC_To%"

    COPY "W:\xHarbour\xDiff.txt"         "%RC_To%"
    COPY "W:\xHarbour\Doc\namespace.txt" "%RC_To%"

    COPY "W:\xHarbour.com\xHarbour-xHBComm\CommFunc.txt" "%RC_To%\xHBComm\"

    COPY "W:\xHarbour\contrib\rdd_ads\doc\en\adsfuncs.txt" "%RC_To%\Ads\"
    COPY "W:\xHarbour\contrib\rdd_ads\readme.txt"          "%RC_To%\Ads\Readme1-ADS.txt"
    COPY "W:\xHarbour\contrib\rdd_ads\doc\en\readme.txt"   "%RC_To%\Ads\Readme2-ADS.txt"

    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD Manual - EN.pdf"    "%RC_To%\SQLRDD\SQLRDD Manual.pdf"
    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD Reference - EN.pdf" "%RC_To%\SQLRDD\SQLRDD Reference.pdf"
    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_methodology_en.pdf" "%RC_To%\SQLRDD\SQLRDD Methodology.pdf"
    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_Reference_EN.chm"   "%RC_To%\SQLRDD\SQLRDD_Reference_EN.chm"
    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_Reference_br.chm"   "%RC_To%\SQLRDD\SQLRDD_Reference_BR.chm"

    COPY "W:\xHarbour.com\xHarbour-ApolloRDD\ApolloAPI6.chm" "%RC_To%\ApolloRDD\"

    COPY "W:\xHarbour.com\xHarbour-HBZLib\doc\Xbuilder Zip Archive Function Extension.doc" "%RC_To%\HBZLib\HBZlib-Functions.doc"



REM ===============================================
REM                 /Common/include
REM ===============================================


    SET RC_To=%XHBTO%\Include

    SET RC_Exclude_File=.cvsignore
    SET RC_Exclude_Folder=CVS CVSROOT .SVN
    ROBOCOPY "C:\xHB\Include" "%RC_To%" *.* /NS /NC /NP /S /XF %RC_Exclude_File% /XD %RC_Exclude_Folder%
    ROBOCOPY "W:\xHarbour.com\xHarbour-SQLRDD\include" "%RC_To%" *.* /NS /NC /NP /S /XF %RC_Exclude_File% /XD %RC_Exclude_Folder%

    COPY "W:\xHarbour\contrib\rdd_ads\*.ch" "%RC_To%"
    COPY "W:\xHarbour.com\xHarbour-ApolloRDD\Apollo.ch" "%RC_To%"
    COPY "W:\xHarbour.com\xHarbour-xHBComm\tcomm.ch" "%RC_To%"



REM ===============================================
REM                  /Common/Lib
REM ===============================================

    SET RC_To=%XHBTO%\Lib

    MD "%RC_To%"

    COPY "C:\xHB\Lib\ACE32.lib" "%RC_To%"
    COPY "C:\xHB\Lib\ADS.lib" "%RC_To%"
    COPY "C:\xHB\Lib\BGD.lib" "%RC_To%"
    COPY "C:\xHB\Lib\CDX.lib" "%RC_To%"
    COPY "C:\xHB\Lib\DBF.lib" "%RC_To%"
    COPY "C:\xHB\Lib\DbgServe.lib" "%RC_To%"
    COPY "C:\xHB\Lib\fbclient_ms.lib" "%RC_To%"
    COPY "C:\xHB\Lib\FreeImage*.lib" "%RC_To%"
    COPY "C:\xHB\Lib\FTS32.lib" "%RC_To%"
    COPY "C:\xHB\Lib\GD.lib" "%RC_To%"
    COPY "C:\xHB\Lib\LibMisc.lib" "%RC_To%"
    COPY "C:\xHB\Lib\libmysql.lib" "%RC_To%"
    COPY "C:\xHB\Lib\libpq.lib" "%RC_To%"
    COPY "C:\xHB\Lib\Nanfor.lib" "%RC_To%"
    COPY "C:\xHB\Lib\NoRDD.lib" "%RC_To%"
    COPY "C:\xHB\Lib\NSX.lib" "%RC_To%"
    COPY "C:\xHB\Lib\NTX.lib" "%RC_To%"
    COPY "C:\xHB\Lib\oci.lib" "%RC_To%"
    COPY "C:\xHB\Lib\ODBC.lib" "%RC_To%"
    COPY "C:\xHB\Lib\Optg.lib" "%RC_To%"
    COPY "C:\xHB\Lib\SDE61.lib" "%RC_To%"
    COPY "C:\xHB\Lib\SixCDX.lib" "%RC_To%"
    COPY "C:\xHB\Lib\VXHd.lib" "%RC_To%"
    COPY "C:\xHB\Lib\WinCore.lib" "%RC_To%"
    COPY "C:\xHB\Lib\WVG.lib" "%RC_To%"
    COPY "C:\xHB\Lib\WVT.lib" "%RC_To%"
    COPY "C:\xHB\Lib\WVW.lib" "%RC_To%"
    COPY "C:\xHB\Lib\XDO.lib" "%RC_To%"

REM TipSSL:
    COPY "C:\xHB\Lib\TipSSL.lib" "%RC_To%"
    COPY "C:\OpenSSL\lib\libeay32.lib" "%RC_To%"
    COPY "C:\OpenSSL\lib\SSLeay32.lib" "%RC_To%"

    COPY "W:\xHarbour.com\xHarbour-Builder\bgd.lib" "%RC_To%"
    COPY "W:\xHarbour.com\FreeImage\FreeImage.lib" "%RC_To%"
    COPY "W:\xHarbour.com\xHarbour-SQLRDD\lib\*.lib" "%RC_To%"


REM ===============================================
REM                  /Common/Dll
REM ===============================================

    SET RC_To=%XHBTO%\Dll
    SET RC_Exclude_Folder=CVS CVSROOT .SVN

    ROBOCOPY "C:\xHB\Dll" "%RC_To%" bgd.dll XDO.dll FreeImage.dll /NS /NC /NP
    ROBOCOPY "C:\xHB\Bin" "%RC_To%" xHBdll.dll xHBddll.dll /NS /NC /NP

    ROBOCOPY "C:\xHB\Dll\ADS" "%RC_To%\ADS" *.* /NS /NC /NP /S /XD %RC_Exclude_Folder%

    ROBOCOPY "W:\xHarbour.com\xHarbour-SQLRDD\dll" "%RC_To%\SQLRDD" *.* /NS /NC /NP /S /XD %RC_Exclude_Folder%
    ROBOCOPY "W:\xHarbour.com\xHarbour-ApolloRDD\dll" "%RC_To%\ApolloRDD" *.* /NS /NC /NP /S /XD %RC_Exclude_Folder%

REM TipSSL:
    COPY C:\OpenSSL\Libeay32.dll "%RC_To%"
	COPY C:\OpenSSL\SSLeay32.dll "%RC_To%"
	

REM ===============================================
REM                /Common/sample
REM ===============================================

    SET RC_To=%XHBTO%\Samples

    SET RC_Exclude_Folder=CVS CVSROOT .SVN
    ROBOCOPY "W:\xHarbour.com\Samples"         "%RC_To%"         *.* /NS /NC /NP    /XD %RC_Exclude_Folder% /XA:H
    ROBOCOPY "W:\xHarbour.com\Samples\FiveWin" "%RC_To%\FiveWin" *.* /NS /NC /NP /S /XD %RC_Exclude_Folder% /XA:H
    ROBOCOPY "W:\xHarbour.com\Samples\GD"      "%RC_To%\GD"      *.* /NS /NC /NP /S /XD %RC_Exclude_Folder% /XA:H
    ROBOCOPY "W:\xHarbour.com\Samples\UsrRDD"  "%RC_To%\UsrRDD"  *.* /NS /NC /NP /S /XD %RC_Exclude_Folder% /XA:H

    ROBOCOPY "W:\xHarbour.com\Samples\Visual-xHarbour" "%RC_To%\Visual-xHarbour" *.* /NS /NC /NP /XF Changelog /S /XD %RC_Exclude_Folder% /XA:H

    ROBOCOPY "W:\xHarbour.com\xHarbour-HBZLib\test"    "%RC_To%\HBZlib" *.* /NS /NC /NP /XF %RC_Exclude_File% /XA:H
    ROBOCOPY "W:\xHarbour.com\xHarbour-SQLRDD\samples" "%RC_To%\SQLRDD" *.* /NS /NC /NP /XF %RC_Exclude_File% /XA:H

    SET RC_Exclude_File=Makefile *.bat *.mak
    ROBOCOPY "W:\xHarbour\Tests" "%RC_To%\xHarbour" *.* /NS /NC /NP /XF %RC_Exclude_File% /S /XD %RC_Exclude_Folder% /XA:H

    COPY "W:\xHarbour.com\xHarbour-xHBComm\querycls.prg" "%RC_To%\xHBComm\"



REM ===============================================
REM                /Common/source
REM ===============================================

    SET RC_To=%XHBTO%\Source

    SET RC_Exclude_Folder=CVS CVSROOT .SVN
    ROBOCOPY "W:\xHarbour\Source" "%RC_To%" *.c* *.prg *.s* *.y *.h *.x* *.gen* *.def *.gcc /NS /NC /NP /S /XD %RC_Exclude_Folder%

    ROBOCOPY "W:\xHarbour\Source\tip\encoding" "%RC_To%\Tip\Encoding" *.c /NS /NC /NP /XD %RC_Exclude_Folder%
    ROBOCOPY "W:\xHarbour\tests\tiptest" "%RC_To%\Tip\Tests" *.* /NS /NC /NP /XD %RC_Exclude_Folder%

	COPY "W:\xHarbour\Source\rdd\dbf0.prg"           "%RC_To%"
	COPY "W:\xHarbour\Source\rdd\dbfcdx\dbfcdx0.prg" "%RC_To%"
	COPY "W:\xHarbour\Source\rdd\dbfdbt\dbfdbt0.prg" "%RC_To%"
	COPY "W:\xHarbour\Source\rdd\dbffpt\dbffpt0.prg" "%RC_To%"
	COPY "W:\xHarbour\Source\rdd\dbfntx\dbfntx0.prg" "%RC_To%"
	COPY "W:\xHarbour\Source\rdd\nulsys\nulsys.prg"  "%RC_To%"

	COPY "W:\xHarbour\Source\rdd\dbf0.prg"           "%RC_To%"
	COPY "W:\xHarbour\Source\rdd\rddsys.prg"         "%RC_To%"
	COPY "W:\xHarbour\Source\rdd\sdf0.prg"           "%RC_To%"
	COPY "W:\xHarbour\Source\rdd\delim0.prg"         "%RC_To%"

	COPY "W:\xHarbour\Source\rtl\errorsys.prg"       "%RC_To%"
	COPY "W:\xHarbour\Source\rtl\getsys.prg"         "%RC_To%"
    COPY "W:\xHarbour\Source\vm\harbinit.prg"        "%RC_To%"


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM                /Demo/bin
REM ===============================================

    SET RC_To=%XHBTO%\Bin

    SET RC_Include=VXH.exe xDebugW.exe xBuild*.exe
    ROBOCOPY "C:\xHB\Bin\Demo" "%RC_To%" %RC_Include% /NS /NC /NP



REM ===============================================
REM                /Demo/lib
REM ===============================================

    SET RC_To=%XHBTO%\Lib

    SET RC_Include=vxh.lib xHB.lib WinApi.lib SQL.lib six.lib
    ROBOCOPY "C:\xHB\Lib\Demo" "%RC_To%" %RC_Include% /NS /NC /NP

REM *********************************
REM * We need DEMO LIBs for these: *
REM *********************************
    COPY "C:\xHB\c_lib\Win\version.lib"   "%RC_To%\RMDBFCDX.lib"  /y /b
    COPY "C:\xHB\c_lib\Win\version.lib"   "%RC_To%\ActiveX.lib"   /y /b
    COPY "C:\xHB\c_lib\Win\version.lib"   "%RC_To%\XBScript.lib"  /y /b
    COPY "C:\xHB\c_lib\Win\version.lib"   "%RC_To%\CT3Comm.lib"   /y /b
    COPY "C:\xHB\c_lib\Win\version.lib"   "%RC_To%\Comm.lib"      /y /b
    COPY "C:\xHB\c_lib\Win\version.lib"   "%RC_To%\xHBCOMM.lib"   /y /b
    COPY "C:\xHB\c_lib\Win\version.lib"   "%RC_To%\xHBZIP.lib"    /y /b
    COPY "C:\xHB\c_lib\Win\version.lib"   "%RC_To%\OleServer.lib" /y /b
    COPY "C:\xHB\c_lib\Win\version.lib"   "%RC_To%\Ole.lib"       /y /b


REM ===============================================
REM              /Demo/dll
REM ===============================================

    SET CopyTo=%XHBTO%\Dll
    SET EmptyDLL=W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\empty.dll

    COPY "%EmptyDLL%" "%CopyTo%\xHBmtDLL.dll" /b
    COPY "%EmptyDLL%" "%CopyTo%\xHBDmtDLL.dll" /b
    COPY "%EmptyDLL%" "%CopyTo%\xHBCommDll.dll" /b
    COPY "%EmptyDLL%" "%CopyTo%\xHBZipDll.dll" /b

IF "%1" == "Demo" GOTO :EOF


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================


REM ===============================================
REM                /Personal/bin
REM ===============================================

    SET RC_To=%XHBTO%\Bin

    COPY "C:\xHB\Bin\Personal\VXH.exe"     "%RC_To%\VXH.exe" /y /b
    COPY "C:\xHB\Bin\Demo\xDebugW.exe"     "%RC_To%\xDebugW.exe" /y /b
    COPY "C:\xHB\Bin\Personal\xBuildW.exe" "%RC_To%\xBuildW.exe" /y /b
    COPY "C:\xHB\Bin\xBuild.exe"           "%RC_To%\xBuild.exe" /y /b


REM ===============================================
REM                /Personal/lib
REM ===============================================

    SET RC_To=%XHBTO%\Lib

    COPY "C:\xHB\Lib\xHB.lib"          "%RC_To%\xHB.lib"  /y
    COPY "C:\xHB\Lib\Personal\VXH.lib" "%RC_To%\VXH.lib"  /y


REM ===============================================
REM              /Personal/dll
REM ===============================================

    SET CopyTo=%XHBTO%\Dll
    SET EmptyDLL=W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\empty.dll

    COPY "%EmptyDLL%" "%CopyTo%\xHBmtDLL.dll" /b
    COPY "%EmptyDLL%" "%CopyTo%\xHBDmtDLL.dll" /b
    COPY "%EmptyDLL%" "%CopyTo%\xHBCommDll.dll" /b
    COPY "%EmptyDLL%" "%CopyTo%\xHBZipDll.dll" /b

IF "%1" == "Personal" GOTO :EOF


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM              /Professional/bin
REM ===============================================

    SET RC_To=%XHBTO%\Bin

    COPY "C:\xHB\Bin\xBuildW.exe"          "%RC_To%\xBuildW.exe" /y /b
    COPY "C:\xHB\Bin\xDebugW.exe"          "%RC_To%\xDebugW.exe" /y /b

    SET RC_Include=*.exe *.dll
    ROBOCOPY "C:\xHB\Bin\Professional" "%RC_To%" %RC_Include% /NS /NC /NP

REM ===============================================
REM              /Professional/lib
REM ===============================================

    SET RC_To=%XHBTO%\Lib

    SET RC_Include=*.lib
    SET RC_Exclude_File=xEditVXH.lib
    ROBOCOPY "C:\xHB\Lib" "%RC_To%" %RC_Include% /NS /NC /NP /XF %RC_Exclude_File%

    SET RC_Include=vxh.lib WinApi.lib vxhdll.lib vxhddll.lib
    ROBOCOPY "C:\xHB\Lib\Professional" "%RC_To%" %RC_Include% /NS /NC /NP

    SET RC_Include=*sql*.lib
    ROBOCOPY "C:\xHB\Lib\Demo" "%RC_To%" %RC_Include% /NS /NC /NP

REM ===============================================
REM              /Professional/dll
REM ===============================================

    SET RC_To=%XHBTO%\Dll

    SET RC_Include=xhbcommdll.dll xHBZipDll.dll
    ROBOCOPY "C:\xHB\Dll" "%RC_To%" %RC_Include% /NS /NC /NP

    SET RC_Include=xhbdmtdll.dll xhbmtdll.dll
    ROBOCOPY "C:\xHB\Bin" "%RC_To%" %RC_Include% /NS /NC /NP

    SET RC_Include=*.dll
    ROBOCOPY "C:\xHB\Bin\Professional" "%RC_To%" %RC_Include% /NS /NC /NP

REM    SET CopyTo=%XHBTO%\Dll
REM    SET EmptyDLL=W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\empty.dll

REM    COPY "%EmptyDLL%" "%CopyTo%\xHBmtDLL.dll" /b
REM    COPY "%EmptyDLL%" "%CopyTo%\xHBDmtDLL.dll" /b
REM    COPY "%EmptyDLL%" "%CopyTo%\xHBCommDll.dll" /b
REM    COPY "%EmptyDLL%" "%CopyTo%\xHBZipDll.dll" /b


REM ===============================================
REM              /Professional/Samples
REM ===============================================

    SET RC_To=%XHBTO%\Samples

    SET RC_Include=xhbdmtdll.dll xhbmtdll.dll xhbcommdll.dll xHBZipDll.dll
    ROBOCOPY "C:\xHB\Dll\Professional" "%RC_To%" %RC_Include% /NS /NC /NP

    SET RC_Exclude_Folder=CVS CVSROOT .SVN
    ROBOCOPY "W:\xHarbour.com\Samples\Samples for PROF and ENT versions" "%RC_To%" *.* /NS /NC /NP /S /XD %RC_Exclude_Folder% /XA:H


IF "%1" == "Professional" GOTO :EOF


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================


REM ===============================================
REM              /Enterprise/bin
REM ===============================================


    SET RC_To=%XHBTO%\Bin

    SET RC_Include=vxh.exe vxh*.dll xPromptSQL.exe
    ROBOCOPY "C:\xHB\Bin" "%RC_To%" %RC_Include% /NS /NC /NP


REM ===============================================
REM              /Enterprise/Lib
REM ===============================================


    SET RC_To=%XHBTO%\Lib

    SET RC_Include=vxh*.lib WinApi.lib *sql*.lib
    ROBOCOPY "C:\xHB\Lib" "%RC_To%" %RC_Include% /NS /NC /NP


REM ===============================================
REM              /Enterprise/dll
REM ===============================================

    SET RC_To=%XHBTO%\Dll

    SET RC_Include=vxh*.dll
    ROBOCOPY "C:\xHB\Dll" "%RC_To%" %RC_Include% /NS /NC /NP


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

