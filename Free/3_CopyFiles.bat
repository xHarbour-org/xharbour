    IF "%1"=="PC5" SET __COMPILER=PC
    IF "%1"=="PC6" SET __COMPILER=PC
    IF "%1"=="BC5" SET __COMPILER=B32
    IF "%1"=="VC9" SET __COMPILER=VC

    RD C:\xHarbour.Files /S /Q

    MD C:\xHarbour.Files\bin
    MD C:\xHarbour.Files\lib\SQLRDD
    MD C:\xHarbour.Files\doc\xHarbour\gd
    MD C:\xHarbour.Files\dll\Redistribute\ads
    MD C:\xHarbour.Files\dll\Redistribute\SQLRDD
    MD C:\xHarbour.Files\dll\Redistribute\XBScript
    MD C:\xHarbour.Files\source\tip\encoding
    MD C:\xHarbour.Files\source\tip\tests

    MD C:\xHarbour.Files\include\SQLRDD
    MD C:\xHarbour.Files\samples\SQLRDD

    MD C:\xHarbour.Files\include\WVWTools
    MD C:\xHarbour.Files\samples\WVWTools
    MD C:\xHarbour.Files\source\WVWTools


REM ===============================================
REM                  /xHarbour.Files
REM ===============================================

    COPY "W:\Clean CVS\xHarbour.com\Free\%1\cfgCCompiler.xbs" C:\xHarbour.Files /y

    SET __xH.comW=W:\Clean CVS\xHarbour.com\xHarbour-Builder\Wise Install Scripts

    COPY "%__xH.comW%\xHarbour Builder Buttons.url"   C:\xHarbour.Files /y
    COPY "%__xH.comW%\xHarbour.com.url"               C:\xHarbour.Files /y
    COPY "%__xH.comW%\xHarbour.doc.url"               C:\xHarbour.Files /y
    COPY "%__xH.comW%\xHarbour.org.url"               C:\xHarbour.Files /y
    COPY "%__xH.comW%\Visual xHarbour.url"            C:\xHarbour.Files /y

    COPY "%__xH.comW%\XBScript.lib License.txt"       C:\xHarbour.Files /y

    COPY "W:\Clean CVS\xHarbour.com\Free\ReadMe.rtf"            C:\xHarbour.Files /y
    COPY "W:\Clean CVS\xHarbour.com\Free\free.xHarbour.com.url" C:\xHarbour.Files /y



REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================




REM ===============================================
REM                /xHarbour.Files/bin
REM ===============================================

    SET __xH.comZ="W:\xHB"

    COPY "%__xH.comZ%\bin\xEditW.exe"       C:\xHarbour.Files\bin\xEditW.exe  /y /b
    COPY "%__xH.comZ%\bin\xBuildW.exe"      C:\xHarbour.Files\bin\xBuildW.exe /y /b
    COPY "%__xH.comZ%\bin\xBuild.exe"       C:\xHarbour.Files\bin\xBuild.exe  /y /b
    COPY "%__xH.comZ%\bin\xPrompt.exe"      C:\xHarbour.Files\bin\xPrompt.exe /y /b

    COPY "%__xH.comZ%\bin\*.xbs"            C:\xHarbour.Files\bin\*.xbs       /y 

    COPY C:\xHarbour\bin\%__COMPILER%\harbour.exe  C:\xHarbour.Files\bin\*.exe /y /b
REM COPY C:\xHarbour\bin\%__COMPILER%\harbour.dll  C:\xHarbour.Files\bin\*.dll /y /b


REM ===============================================
REM                /xHarbour.Files/c_include
REM ===============================================


    COPY  C:\xHarbour\contrib\rdd_ads\*.h             C:\xHarbour.Files\include\ /y



REM ===============================================
REM                /xHarbour.Files/doc
REM ===============================================

    SET __xHB.com=W:\Clean CVS\xHarbour.com\xHarbour-Builder\Wise Install Scripts (xHBDocs)\Files

    COPY "%__xHB.com%\xHarbour Language Reference Guide.chm" "C:\xHarbour.Files\Doc\xHarbour Language Reference Guide.chm" /y

    COPY C:\xHarbour\xdiff.txt                            C:\xHarbour.Files\Doc\xDiff.txt /y

    COPY C:\xHarbour\doc\*.txt                            C:\xHarbour.Files\Doc\xHarbour  /y
    COPY C:\xHarbour\doc\en\*.txt                         C:\xHarbour.Files\Doc\xHarbour  /y

    COPY C:\xHarbour\contrib\rdd_ads\doc\en\adsfuncs.txt  C:\xHarbour.Files\Doc\xHarbour /y
    COPY C:\xHarbour\contrib\rdd_ads\readme.txt           C:\xHarbour.Files\Doc\xHarbour\Readme1-ADS.txt /y
    COPY C:\xHarbour\contrib\rdd_ads\doc\en\readme.txt    C:\xHarbour.Files\Doc\xHarbour\Readme2-ADS.txt /y

    COPY C:\xHarbour\contrib\gd\doc\*.*                   C:\xHarbour.Files\Doc\xHarbour\gd\*.* /y

    SET __xHB.com=W:\Clean CVS\xHarbour.com\xHarbour-SQLRDD\doc
    COPY "%__xHB.com%\SQLRDD Manual - EN.pdf"            "C:\xHarbour.Files\Doc\SQLRDD Manual.pdf"       /y
    COPY "%__xHB.com%\SQLRDD Reference - EN.pdf"         "C:\xHarbour.Files\Doc\SQLRDD Reference.pdf"    /y
    COPY "%__xHB.com%\SQLRDD_methodology_en.pdf"         "C:\xHarbour.Files\Doc\SQLRDD Methodology.pdf"  /y
    COPY "%__xHB.com%\SQLRDD_Reference_br.chm"           "C:\xHarbour.Files\Doc\SQLRDD_Reference_br.chm" /y
	



REM ===============================================
REM                 /xHarbour.Files/include
REM ===============================================


    XCOPY C:\xHarbour\include                                     C:\xHarbour.Files\include /y /i /s
    DEL C:\xHarbour.Files\include\Makefile /Q

    COPY C:\xHarbour\contrib\rdd_ads\*.ch                         C:\xHarbour.Files\include          /y
    COPY "W:\Clean CVS\xHarbour.com\xHarbour-Builder\include\*.*" C:\xHarbour.Files\include          /y
    COPY "W:\Clean CVS\xHarbour.com\xHarbour-SQLRDD\Include\*.*"  C:\xHarbour.Files\include\SQLRDD   /y
    DEL C:\xHarbour.Files\include\SQLRDD\mysql.h /Q

    XCOPY C:\WVWTools\include                                     C:\xHarbour.Files\include\WVWTools /y /i /s

REM IEGUI
    DEL C:\xHarbour.Files\include\iegui.ch

    

REM ===============================================
REM                  /xHarbour.Files/Lib
REM ===============================================


    XCOPY C:\xHarbour\Lib\%__COMPILER%\*.lib             C:\xHarbour.Files\Lib\*.lib        /y /i /s

    COPY C:\WVWTools\WVWTools.lib                        C:\xHarbour.Files\Lib\WVWTools.lib    /y
     
    SET __xHB.com=W:\Clean CVS\xHarbour.com\xHarbour-SQLRDD
    IF "%__COMPILER%"=="B32" COPY "%__xHB.com%\lib\bc5\fbclient_bc.lib" C:\xHarbour.Files\Lib\SQLRDD\fbclient_bc.lib /y
    IF "%__COMPILER%"=="B32" COPY "%__xHB.com%\lib\bc5\libmysql.lib"    C:\xHarbour.Files\Lib\SQLRDD\libmysql.lib    /y
    IF "%__COMPILER%"=="B32" COPY "%__xHB.com%\lib\bc5\libpq.lib"       C:\xHarbour.Files\Lib\SQLRDD\libpq.lib       /y
    IF "%__COMPILER%"=="B32" COPY "%__xHB.com%\lib\bc5\oci.lib"         C:\xHarbour.Files\Lib\SQLRDD\oci.lib         /y
    IF "%__COMPILER%"=="B32" COPY "%__xHB.com%\lib\bc5\odbccp32.lib"    C:\xHarbour.Files\Lib\SQLRDD\odbccp32.lib    /y

    IF "%__COMPILER%"=="VC" COPY "%__xHB.com%\lib\libmysql.lib"         C:\xHarbour.Files\Lib\SQLRDD\libmysql.lib    /y
    IF "%__COMPILER%"=="VC" COPY "%__xHB.com%\lib\libpq.lib"            C:\xHarbour.Files\Lib\SQLRDD\libpq.lib       /y
    IF "%__COMPILER%"=="VC" COPY "%__xHB.com%\lib\oci.lib"              C:\xHarbour.Files\Lib\SQLRDD\oci.lib         /y

    IF "%__COMPILER%"=="PC" COPY "%__xHB.com%\lib\libmysql.lib"         C:\xHarbour.Files\Lib\SQLRDD\libmysql.lib    /y
    IF "%__COMPILER%"=="PC" COPY "%__xHB.com%\lib\libpq.lib"            C:\xHarbour.Files\Lib\SQLRDD\libpq.lib       /y
    IF "%__COMPILER%"=="PC" COPY "%__xHB.com%\lib\oci.lib"              C:\xHarbour.Files\Lib\SQLRDD\oci.lib         /y

    COPY "%__xHB.com%\lib\fbclient_ms.lib"                              C:\xHarbour.Files\Lib\SQLRDD\fbclient_ms.lib /y

    IF "%__COMPILER%"=="B32" COPY W:\xHB\lib\BC5\Demo\SQL.lib  C:\xHarbour.Files\Lib\SQLRDD\SQL.lib         /y
    IF "%__COMPILER%"=="VC"  COPY W:\xHB\lib\VC8\Demo\SQL.lib  C:\xHarbour.Files\Lib\SQLRDD\SQL.lib         /y
    IF "%__COMPILER%"=="PC"  COPY W:\xHB\lib\Demo\SQL.lib      C:\xHarbour.Files\Lib\SQLRDD\SQL.lib         /y



REM ===============================================
REM                  /xHarbour.Files/Dll
REM ===============================================


    COPY W:\xHB\Dll\ads\adslocal.cfg      C:\xHarbour.Files\Dll\Redistribute\ads\ADSLocal.cfg /y
    COPY W:\xHB\Dll\ads\ansi.chr          C:\xHarbour.Files\Dll\Redistribute\ads\Ansi.chr     /y
    COPY W:\xHB\Dll\ads\extend.chr        C:\xHarbour.Files\Dll\Redistribute\ads\Extend.chr   /y
    COPY W:\xHB\Dll\ads\ace32.dll         C:\xHarbour.Files\Dll\Redistribute\ads\ACE32.dll    /y
    COPY W:\xHB\Dll\ads\adsloc32.dll      C:\xHarbour.Files\Dll\Redistribute\ads\ADSLoc32.dll /y
    COPY W:\xHB\Dll\ads\axcws32.dll       C:\xHarbour.Files\Dll\Redistribute\ads\AXCws32.dll  /y

    COPY "W:\Clean CVS\xHarbour.com\xHarbour-SQLRDD\DLL\*.*"  C:\xHarbour.Files\Dll\Redistribute\SQLRDD\ /y

    COPY W:\xHB\Dll\XBScript\XBScript.dll C:\xHarbour.Files\Dll\Redistribute\XBScript\XBScript.dll /y

    COPY C:\xHarbour\bin\%__COMPILER%\harbour.dll C:\xHarbour.Files\Dll\Harbour.dll /y


REM ===============================================
REM                /xHarbour.Files/sample
REM ===============================================


    COPY  "W:\Clean CVS\xHarbour.com\Samples\*.*"                          C:\xHarbour.Files\samples\*.* /y
    DEL C:\xHarbour.Files\samples\Test_Six*.exe.xbp /Q
    DEL C:\xHarbour.Files\samples\WinTest.exe.xbp /Q

REM IEGUI
    DEL C:\xHarbour.Files\samples\IE_App.prg /Q

    XCOPY "W:\Clean CVS\xHarbour.com\Samples\UsrRDD\*.prg"                 C:\xHarbour.Files\samples\UsrRDD /y /s /i
    XCOPY "W:\Clean CVS\xHarbour.com\Samples\FiveWin\*.prg"                C:\xHarbour.Files\samples\FiveWin /y /s /i
    XCOPY "W:\Clean CVS\xHarbour.com\Samples\FiveWin\*.xbp"                C:\xHarbour.Files\samples\FiveWin /y /s /i

REM COPY  "W:\Clean CVS\xHarbour.com\xHarbour-xHBComm\querycls.prg"        C:\xHarbour.Files\samples  /y
REM XCOPY "W:\Clean CVS\xHarbour.com\xHarbour-HBZLib\test\*.prg"           C:\xHarbour.Files\Samples\HBZlib\*.prg /y /s
    XCOPY "W:\Clean CVS\xHarbour.com\xHarbour-SQLRDD\samples"              C:\xHarbour.Files\Samples\sqlrdd /y /s /i

    XCOPY "W:\Clean CVS\xHarbour.com\Samples\GD\*.*"                       C:\xHarbour.Files\samples\GD /y /s /i

    XCOPY C:\xHarbour\Tests\*.prg                                          C:\xHarbour.Files\samples\xHarbour /y /s /i

REM XCOPY "W:\Clean CVS\xHarbour.com\Samples\Visual-xHarbour"              C:\xHarbour.Files\Samples\Visual-xHarbour  /y /s /i
REM DEL   xHarbour.Files\Samples\Visual-xHarbour\Changelog

    COPY C:\wvwTools\wvwtest.prg  C:\xHarbour.Files\samples\WVWTools /y
    COPY C:\wvwTools\wvwtest.rc   C:\xHarbour.Files\samples\WVWTools /y


REM ===============================================
REM                /xHarbour.Files/source
REM ===============================================


    XCOPY C:\xHarbour\Source\*.prg                     C:\xHarbour.Files\Source\*.prg        /y /s /i
    XCOPY C:\xHarbour\Source\*.c                       C:\xHarbour.Files\Source\*.c          /y /s /i
    
    COPY C:\WVWTools\Source\*.prg                      C:\xHarbour.Files\Source\WVWTools     /y


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================


    RDCVS C:\xHarbour.Files
