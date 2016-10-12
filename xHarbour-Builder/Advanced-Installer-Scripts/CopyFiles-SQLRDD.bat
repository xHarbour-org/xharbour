DEL C:\xHB-SetupFiles\xHB.back*.aip /Q

    SET XHBTO=C:\xHB-SetupFiles\xHB-Files

    C:
    RD "C:\xHB-SetupFiles\xHB-Files" /S /Q
    MD "C:\xHB-SetupFiles\xHB-Files"
    CD "C:\xHB-SetupFiles\xHB-Files"

REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM                /Common/doc
REM ===============================================

    MD "C:\xHB-SetupFiles\xHB-Files\Doc\SQLRDD"

    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD Manual - EN.pdf"    "C:\xHB-SetupFiles\xHB-Files\Doc\SQLRDD\SQLRDD Manual.pdf"
    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD Reference - EN.pdf" "C:\xHB-SetupFiles\xHB-Files\Doc\SQLRDD\SQLRDD Reference.pdf"
    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_methodology_en.pdf" "C:\xHB-SetupFiles\xHB-Files\Doc\SQLRDD\SQLRDD Methodology.pdf"
    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_Reference_EN.chm"   "C:\xHB-SetupFiles\xHB-Files\Doc\SQLRDD\SQLRDD_Reference_EN.chm"
    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_Reference_br.chm"   "C:\xHB-SetupFiles\xHB-Files\Doc\SQLRDD\SQLRDD_Reference_BR.chm"


REM ===============================================
REM                 /Common/include
REM ===============================================


    ROBOCOPY "\xHarbour.com\xHarbour-SQLRDD\include" "C:\xHB-SetupFiles\xHB-Files\Include" *.* /NS /NC /NP /S


REM ===============================================
REM                  /Common/Dll
REM ===============================================


    ROBOCOPY "\xHarbour.com\xHarbour-SQLRDD\dll" "C:\xHB-SetupFiles\xHB-Files\Dll\SQLRDD" *.* /NS /NC /NP /S


REM ===============================================
REM                /Common/sample
REM ===============================================


    ROBOCOPY "\xHarbour.com\xHarbour-SQLRDD\samples" "C:\xHB-SetupFiles\xHB-Files\Samples\SQLRDD" *.* /NS /NC /NP /XA:H


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM                /Demo/lib
REM ===============================================

    IF "%1" == "XHB-DEMO" SET RC_To=C:\xHB-SetupFiles\xHB-Files\Lib
    IF "%1" == "BCC-DEMO" SET RC_To=C:\xHB-SetupFiles\xHB-Files\Lib\Bcc

    IF "%1" == "XHB-DEMO" SET RC_Include=SQL.lib
    IF "%1" == "BCC-DEMO" SET RC_Include=SQL.lib

    IF "%1" == "XHB-DEMO" ROBOCOPY "\xHB\Lib\Demo"     "%RC_To%" %RC_Include% /NS /NC /NP
    IF "%1" == "BCC-DEMO" ROBOCOPY "\xHB\Lib\Demo\Bcc" "%RC_To%" %RC_Include% /NS /NC /NP

IF "%1" == "Demo" GOTO :EOF


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================


REM ===============================================
REM              /Full/Lib
REM ===============================================


   ROBOCOPY "\xHB\Lib"     "C:\xHB-SetupFiles\xHB-Files\Lib" SQL.lib SQLMT.lib /NS /NC /NP
   ROBOCOPY "\xHB\Lib\Bcc" "C:\xHB-SetupFiles\xHB-Files\Lib\Bcc" SQL.lib /NS /NC /NP


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

