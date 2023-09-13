CLS
@ECHO off

IF "%1"=="-ALL" (RD "C:\Temp\vh-OBJ\SQLRDD.lib" /S /Q)
IF "%1"=="-All" (RD "C:\Temp\vh-OBJ\SQLRDD.lib" /S /Q)
IF "%1"=="-all" (RD "C:\Temp\vh-OBJ\SQLRDD.lib" /S /Q)

SET HDIR=c:\Harbour
SET PATH=%path%;C:\GnuWin32\bin

rem CALL C\wf-PRG\Build\@SetEnv.bat

CALL "%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars32.bat"

rem IF EXIST     "C:\Program Files (x86)" (CALL "%ProgramFiles% (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars32.bat")
rem IF NOT EXIST "C:\Program Files (x86)" (CALL "%ProgramFiles%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars32.bat")

CD \xHarbour.com\xHarbour-SQLRDD
%hdir%\bin\win\msvc\hbmk2 SQLRDD.lib.hbp
