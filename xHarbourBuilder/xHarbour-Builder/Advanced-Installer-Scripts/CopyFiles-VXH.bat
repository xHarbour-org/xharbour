DEL C:\xHB-SetupFiles\xHB.back*.aip /Q

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
REM                /Common/doc
REM ===============================================

    SET RC_To=%XHBTO%\Doc
    SET XHB-DOC=\xHarbour.com\xHarbour-Documentation

    COPY "%XHB-DOC%\VXH Getting Started 1.0\VXH-Getting-Started.pdf" "%RC_To%\Getting Started with Visual xHarbour.pdf"



REM ===============================================
REM                 /Common/include
REM ===============================================


    SET RC_To=%XHBTO%\Include\w32

    ROBOCOPY "\xHarbour.com\Visual-xHarbour\library\include" "%RC_To%" *.* /NS /NC /NP /S



REM ===============================================
REM                  /Common/Lib
REM ===============================================

    SET RC_To=%XHBTO%\Lib

    MD "%RC_To%"

    COPY "\xHB\Lib\FreeImage*.lib" "%RC_To%"
    COPY "\xHB\Lib\VXHd.lib" "%RC_To%"
    COPY "\xHB\Lib\WinCore.lib" "%RC_To%"

    COPY "\xHarbour.com\FreeImage\FreeImage.lib" "%RC_To%"



REM ===============================================
REM                /Common/sample
REM ===============================================

    SET RC_To=%XHBTO%\Samples
    SET RC_Exclude_Folder=CVS CVSROOT

    ROBOCOPY "\xHarbour.com\Samples\Visual-xHarbour" "%RC_To%\Visual-xHarbour" *.* /NS /NC /NP /XF Changelog /S /XD %RC_Exclude_Folder% /XA:H



REM ===============================================
REM              /Common/dll
REM ===============================================

    SET RC_To=%XHBTO%\Dll

    SET RC_Include=vxh*.dll FreeImage.dll
    ROBOCOPY "\xHB\Dll" "%RC_To%" %RC_Include% /NS /NC /NP

    SET RC_Include=fbclient.dll libmysql.dll
    ROBOCOPY "\xHB\Dll\SQLRDD" "%RC_To%" %RC_Include% /NS /NC /NP
    
    SET RC_Include=ace32.dll
    ROBOCOPY "\xHB\Dll\ADS" "%RC_To%" %RC_Include% /NS /NC /NP

    

REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM                /Demo/bin
REM ===============================================

    SET RC_To=%XHBTO%\Bin
    SET RC_Include=VXH.exe

    ROBOCOPY "\xHB\Bin\Demo" "%RC_To%" %RC_Include% /NS /NC /NP



REM ===============================================
REM                /Demo/lib
REM ===============================================

    SET RC_To=%XHBTO%\Lib
    SET RC_Include=vxh.lib WinApi.lib

    ROBOCOPY "\xHB\Lib\Demo" "%RC_To%" %RC_Include% /NS /NC /NP


IF "%1" == "Demo" GOTO :EOF


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================


REM ===============================================
REM                /Personal/bin
REM ===============================================

    SET RC_To=%XHBTO%\Bin

    COPY "\xHB\Bin\Personal\VXH.exe"     "%RC_To%\VXH.exe" /y /b


REM ===============================================
REM                /Personal/lib
REM ===============================================

    SET RC_To=%XHBTO%\Lib

    COPY "\xHB\Lib\Personal\VXH.lib" "%RC_To%\VXH.lib"  /y


IF "%1" == "Personal" GOTO :EOF


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM              /Professional/bin
REM ===============================================

    SET RC_To=%XHBTO%\Bin
    SET RC_Include=*.exe *.dll

    ROBOCOPY "\xHB\Bin\Professional" "%RC_To%" %RC_Include% /NS /NC /NP

REM ===============================================
REM              /Professional/lib
REM ===============================================

    SET RC_To=%XHBTO%\Lib

    SET RC_Include=vxh.lib WinApi.lib vxhdll.lib vxhddll.lib
    ROBOCOPY "\xHB\Lib\Professional" "%RC_To%" %RC_Include% /NS /NC /NP



IF "%1" == "Professional" GOTO :EOF


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================


REM ===============================================
REM              /Enterprise/bin
REM ===============================================


    SET RC_To=%XHBTO%\Bin

    SET RC_Include=vxh.exe vxh*.dll
    ROBOCOPY "\xHB\Bin" "%RC_To%" %RC_Include% /NS /NC /NP


REM ===============================================
REM              /Enterprise/Lib
REM ===============================================


    SET RC_To=%XHBTO%\Lib

    SET RC_Include=vxh*.lib WinApi.lib
    ROBOCOPY "\xHB\Lib" "%RC_To%" %RC_Include% /NS /NC /NP
    

REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

