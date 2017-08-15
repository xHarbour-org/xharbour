DEL C:\Temp\xHB.back*.aip /Q

    SET XHBTO=C:\Temp\xHB-Files-SQLRDD

    C:
    RD "C:\Temp\xHB-Files-SQLRDD" /S /Q
    MD "C:\Temp\xHB-Files-SQLRDD"
    CD "C:\Temp\xHB-Files-SQLRDD"

REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM                /Common/doc
REM ===============================================

    MD "C:\Temp\xHB-Files-SQLRDD\Doc\SQLRDD"

    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD Manual - EN.pdf"    "C:\Temp\xHB-Files-SQLRDD\Doc\SQLRDD\SQLRDD Manual.pdf"
    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD Reference - EN.pdf" "C:\Temp\xHB-Files-SQLRDD\Doc\SQLRDD\SQLRDD Reference.pdf"
    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_methodology_en.pdf" "C:\Temp\xHB-Files-SQLRDD\Doc\SQLRDD\SQLRDD Methodology.pdf"
    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_Reference_EN.chm"   "C:\Temp\xHB-Files-SQLRDD\Doc\SQLRDD\SQLRDD_Reference_EN.chm"
    COPY "\xHarbour.com\xHarbour-SQLRDD\doc\SQLRDD_Reference_br.chm"   "C:\Temp\xHB-Files-SQLRDD\Doc\SQLRDD\SQLRDD_Reference_BR.chm"

REM ===============================================
REM                 /Common/include
REM ===============================================


    ROBOCOPY "\xHarbour.com\xHarbour-SQLRDD\include" "C:\Temp\xHB-Files-SQLRDD\Include" *.* /NS /NC /NP /S


REM ===============================================
REM                  /Common/Dll
REM ===============================================


    ROBOCOPY "\xHarbour.com\xHarbour-SQLRDD\dll" "C:\Temp\xHB-Files-SQLRDD\Dll\SQLRDD" *.* /NS /NC /NP /S


REM ===============================================
REM                /Common/sample
REM ===============================================


    ROBOCOPY "\xHarbour.com\xHarbour-SQLRDD\samples" "C:\Temp\xHB-Files-SQLRDD\Samples\SQLRDD" *.* /NS /NC /NP /XA:H


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

REM ===============================================
REM                /Demo/lib
REM ===============================================

    IF "%1" == "XHB-DEMO" SET RC_To=C:\Temp\xHB-Files-SQLRDD\Lib
    IF "%1" == "BCC-DEMO" SET RC_To=C:\Temp\xHB-Files-SQLRDD\Lib\Bcc

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


   ROBOCOPY "\xHB\Lib"     "C:\Temp\xHB-Files-SQLRDD\Lib" SQL.lib SQLMT.lib /NS /NC /NP
   ROBOCOPY "\xHB\Lib\Bcc" "C:\Temp\xHB-Files-SQLRDD\Lib\Bcc" SQL.lib /NS /NC /NP


REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================
REM ==============================================================================================

