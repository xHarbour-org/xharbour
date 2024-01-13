IF "%HB_USE_BISON%" == "" GOTO END

:FIND_BISON
   IF EXIST \win_flex_bison-2.5.25\win_bison.exe   GOTO SET_WIN_FLEX_BISON_2_5_25
   IF EXIST C:\win_flex_bison-2.5.25\win_bison.exe GOTO SET_WIN_FLEX_BISON_C_2_5_25
   IF EXIST \winflexbison-2.5.16\win_bison.exe     GOTO SET_WINFLEXBISON_2_5_16
   IF EXIST C:\winflexbison-2.5.16\win_bison.exe   GOTO SET_WINFLEXBISON_C_2_5_16
   IF EXIST \win_flex_bison-2.5.16\win_bison.exe   GOTO SET_WIN_FLEX_BISON_2_5_16
   IF EXIST C:\win_flex_bison-2.5.16\win_bison.exe GOTO SET_WIN_FLEX_BISON_C_2_5_16
   IF EXIST \win_flex_bison-2.5.5\win_bison.exe    GOTO SET_WIN_FLEX_BISON_2_5_5
   IF EXIST C:\win_flex_bison-2.5.5\win_bison.exe  GOTO SET_WIN_FLEX_BISON_C_2_5_5
   IF EXIST "%ProgramFiles(x86)%\GnuWin32\Bin"     GOTO SET_BISONX86
   IF EXIST "%ProgramFiles%\GnuWin32\Bin"          GOTO SET_BISON1
   IF EXIST \GnuWin32\Bin                          GOTO SET_BISON2
   GOTO FOUND

:SET_WIN_FLEX_BISON_2_5_25
   SET BISON_DIR=\win_flex_bison-2.5.25
   SET BISON=win_bison.exe
   SET HB_USE_BISON=1
   GOTO FOUND

:SET_WIN_FLEX_BISON_C_2_5_25
   SET BISON_DIR=C:\win_flex_bison-2.5.25
   SET BISON=win_bison.exe
   SET HB_USE_BISON=1
   GOTO FOUND

:SET_WINFLEXBISON_2_5_16
   SET BISON_DIR=\winflexbison-2.5.16
   SET BISON=win_bison.exe
   SET HB_USE_BISON=1
   GOTO FOUND

:SET_WINFLEXBISON_C_2_5_16
   SET BISON_DIR=C:\winflexbison-2.5.16
   SET BISON=win_bison.exe
   SET HB_USE_BISON=1
   GOTO FOUND

:SET_WIN_FLEX_BISON_2_5_16
   SET BISON_DIR=\win_flex_bison-2.5.16
   SET BISON=win_bison.exe
   SET HB_USE_BISON=1
   GOTO FOUND

:SET_WIN_FLEX_BISON_C_2_5_16
   SET BISON_DIR=C:\win_flex_bison-2.5.16
   SET BISON=win_bison.exe
   SET HB_USE_BISON=1
   GOTO FOUND

:SET_WIN_FLEX_BISON_2_5_5
   SET BISON_DIR=\win_flex_bison-2.5.5
   SET BISON=win_bison.exe
   SET HB_USE_BISON=1
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
   %BISON% --version > NUL 2>&1 || SET "PATH=%BISON_DIR%;%PATH%"
   GOTO END

:NOT_FOUND
   rem Let's return an error code to the caller
   rem echo "ERROR: Bison not found!"
   exit /b 1

:END

