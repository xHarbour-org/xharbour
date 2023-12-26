IF "%HB_USE_BISON%"=="0" GOTO END

:FIND_BISON
   IF EXIST C:\win_flex_bison-2.5.5\win_bison.exe  GOTO SET_WIN_FLEX_BISON_C_2_5_5
   IF EXIST "%ProgramFiles(x86)%\GnuWin32\Bin"     GOTO SET_BISONX86
   IF EXIST "%ProgramFiles%\GnuWin32\Bin"          GOTO SET_BISON1
   IF EXIST \GnuWin32\Bin                          GOTO SET_BISON2
   GOTO FOUND

:SET_WIN_FLEX_BISON_C_2_5_5
   SET BISON_DIR=C:\win_flex_bison-2.5.5
   SET BISON=win_bison.exe
   SET HB_USE_BISON=1
   GOTO FOUND

:SET_BISONX86
   SET BISON_DIR=%ProgramFiles(x86)%\GnuWin32\Bin
   GOTO FOUND
   
:SET_BISON1
   SET BISON_DIR=%ProgramFiles%\GnuWin32\Bin
   SET BISON=bison.exe
   SET HB_USE_BISON=1
   GOTO FOUND

:SET_BISON2
   SET BISON_DIR=\GnuWin32\Bin
   SET BISON=bison.exe
   SET HB_USE_BISON=1
   GOTO FOUND

GOTO NOT_FOUND

:FOUND
   REM echo %BISON_DIR%
   REM echo %BISON%
   GOTO END

:NOT_FOUND
   rem Let's return an error code to the caller
   rem echo "ERROR: Bison not found!"
   exit /b 1

:END

