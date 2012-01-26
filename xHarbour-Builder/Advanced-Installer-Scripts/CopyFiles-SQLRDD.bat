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

    MD "%RC_To%\SQLRDD"

    SET XHB-DOC=W:\xHarbour.com\xHarbour-Documentation

    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD Manual - EN.pdf"    "%RC_To%\SQLRDD\SQLRDD Manual.pdf"
    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD Reference - EN.pdf" "%RC_To%\SQLRDD\SQLRDD Reference.pdf"
    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_methodology_en.pdf" "%RC_To%\SQLRDD\SQLRDD Methodology.pdf"
    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_Reference_EN.chm"   "%RC_To%\SQLRDD\SQLRDD_Reference_EN.chm"
    COPY "W:\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_Reference_br.chm"   "%RC_To%\SQLRDD\SQLRDD_Reference_BR.chm"


REM ===============================================
REM                 /Common/include
REM ===============================================


    SET RC_To=%XHBTO%\Include

    SET RC_Exclude_File=.cvsignore
    SET RC_Exclude_Folder=CVS CVSROOT
    ROBOCOPY "W:\xHarbour.com\xHarbour-SQLRDD\include" "%RC_To%" *.* /NS /NC /NP /S /XF %RC_Exclude_File% /XD %RC_Exclude_Folder%


REM ===============================================
REM                  /Common/Dll
REM ===============================================

    SET RC_To=%XHBTO%\Dll
    SET RC_Exclude_Folder=CVS CVSROOT

    ROBOCOPY "W:\xHarbour.com\xHarbour-SQLRDD\dll" "%RC_To%\SQLRDD" *.* /NS /NC /NP /S /XD %RC_Exclude_Folder%


REM ===============================================
REM                /Common/sample
REM ===============================================

    SET RC_To=%XHBTO%\Samples

    SET RC_Exclude_Folder=CVS CVSROOT
    ROBOCOPY "W:\xHarbour.com\xHarbour-SQLRDD\samples" "%RC_To%\SQLRDD" *.* /NS /NC /NP /XF %RC_Exclude_File% /XA:H


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM                /Demo/lib
REM ===============================================

    IF "%1" == "XHB-DEMO" SET RC_To=%XHBTO%\Lib
    IF "%1" == "BCC-DEMO" SET RC_To=%XHBTO%\Lib\Bcc

    IF "%1" == "XHB-DEMO" SET RC_Include=SQL.lib
    IF "%1" == "BCC-DEMO" SET RC_Include=SQLBCC55.lib SQLBCC58.lib

    IF "%1" == "XHB-DEMO" ROBOCOPY "W:\xHB\Lib\Demo"     "%RC_To%" %RC_Include% /NS /NC /NP
    IF "%1" == "BCC-DEMO" ROBOCOPY "W:\xHB\Lib\Demo\Bcc" "%RC_To%" %RC_Include% /NS /NC /NP

IF "%1" == "Demo" GOTO :EOF


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================


REM ===============================================
REM              /Full/Lib
REM ===============================================


    IF "%1" == "XHB-RELEASE" SET RC_To=%XHBTO%\Lib
    IF "%1" == "BCC-RELEASE" SET RC_To=%XHBTO%\Lib\Bcc

    IF "%1" == "XHB-RELEASE" SET RC_Include=SQL.lib SQLMT.lib
    IF "%1" == "BCC-RELEASE" SET RC_Include=SQLBCC55.lib SQLBCC58.lib

    IF "%1" == "XHB-RELEASE" ROBOCOPY "W:\xHB\Lib"     "%RC_To%" %RC_Include% /NS /NC /NP
    IF "%1" == "BCC-RELEASE" ROBOCOPY "W:\xHB\Lib\Bcc" "%RC_To%" %RC_Include% /NS /NC /NP


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

