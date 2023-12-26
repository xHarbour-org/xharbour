REM User specified compiler location
IF NOT "%CC_DIR%"=="" GOTO DIR_SET

IF NOT "%VCINSTALLDIR%"=="" SET CC_DIR=%VCINSTALLDIR% && GOTO DIR_SET

@for /f "delims=" %%a in ('where cl.exe 2^>nul') do set "_MSVC_BIN=%%a"
IF NOT "%_MSVC_BIN%"=="" GOTO FIND_ROOT

:FIND_VC
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Enterprise\VC"   GOTO SET_VC2022EX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2022\Enterprise\VC"        GOTO SET_VC2022E
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Professional\VC" GOTO SET_VC2022PX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2022\Professional\VC"      GOTO SET_VC2022P
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Community\VC"    GOTO SET_VC2022CX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2022\Community\VC"         GOTO SET_VC2022C

   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\VC"   GOTO SET_VC2017EX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Enterprise\VC"        GOTO SET_VC2017E
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\VC" GOTO SET_VC2017PX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Professional\VC"      GOTO SET_VC2017P
   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC"    GOTO SET_VC2017CX86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\2017\Community\VC"         GOTO SET_VC2017C

   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC" GOTO SET_VC2015X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 14.0\Vc"      GOTO SET_VC2015

   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\VC" GOTO SET_VC2013X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 12.0\Vc"      GOTO SET_VC2013

   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\vc" GOTO SET_VC2012X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 11.0\vc"      GOTO SET_VC2012

   IF EXIST "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\vc" GOTO SET_VC2010X86
   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 10.0\vc"      GOTO SET_VC2010

   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 9.0\vc"       GOTO SET_VC2008

   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 8\vc"         GOTO SET_VC2005

   IF EXIST "%ProgramFiles%\Microsoft Visual Studio 2003\vc"      GOTO SET_VC2003

   IF EXIST "%ProgramFiles%\Microsoft Visual Studio\vc8"          GOTO SET_VC6
   GOTO FOUND

:SET_VC2022EX86
   SET "__MSC__=17"
   CALL "%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Enterprise\Common7\Tools\VsDevCmd.bat" -arch=%HB_VS_ARCH%
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Enterprise\Vc"
   IF "%VS140COMNTOOLS%"=="" SET "VS140COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Enterprise\Common7\Tools\"
   IF NOT "%VS140COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS140COMNTOOLS%"
   GOTO FOUND

:SET_VC2022E
   SET __MSC__=17
   CALL "%ProgramFiles%\Microsoft Visual Studio\2022\Enterprise\Common7\Tools\VsDevCmd.bat" -arch=%HB_VS_ARCH%
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2022\Enterprise\Vc"
   IF "%VS170COMNTOOLS%"=="" SET "VS170COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio\2022\Enterprise\Common7\Tools\"
   IF NOT "%VS170COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS170COMNTOOLS%"
   GOTO FOUND

:SET_VC2022PX86
   SET __MSC__=17
   CALL "%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Professional\Common7\Tools\VsDevCmd.bat" -arch=%HB_VS_ARCH%
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Professional\Vc"
   IF "%VS170COMNTOOLS%"=="" SET "VS170COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Professional\Common7\Tools\"
   IF NOT "%VS170COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS170COMNTOOLS%"
   GOTO FOUND

:SET_VC2022P
   SET __MSC__=17
   CALL "%ProgramFiles%\Microsoft Visual Studio\2022\Professional\Common7\Tools\VsDevCmd.bat" -arch=%HB_VS_ARCH%
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2022\Professional\Vc"
   IF "%VS170COMNTOOLS%"=="" SET "VS170COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio\2022\Professional\Common7\Tools\"
   IF NOT "%VS170COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS170COMNTOOLS%"
   GOTO FOUND

:SET_VC2022CX86
   SET __MSC__=17
   CALL "%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Community\Common7\Tools\VsDevCmd.bat" -arch=%HB_VS_ARCH%
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Community\Vc"
   IF "%VS170COMNTOOLS%"=="" SET "VS170COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio\2022\Community\Common7\Tools\"
   IF NOT "%VS170COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS170COMNTOOLS%"
   GOTO FOUND

:SET_VC2022C
   SET __MSC__=17
   CALL "%ProgramFiles%\Microsoft Visual Studio\2022\Community\Common7\Tools\VsDevCmd.bat" -arch=%HB_VS_ARCH%
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2022\Community\Vc"
   IF "%VS170COMNTOOLS%"=="" SET "VS170COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio\2022\Community\Common7\Tools\"
   IF NOT "%VS170COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS170COMNTOOLS%"
   GOTO FOUND


:SET_VC2017EX86
   SET __MSC__=15
   CALL "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools\VsDevCmd.bat"
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Vc"
   IF "%VS140COMNTOOLS%"=="" SET "VS140COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools\"
   IF NOT "%VS140COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS140COMNTOOLS%"
   IF EXIST "%ProgramFiles(x86)%\Windows Kits\8.1\Bin\x86\RC.Exe" SET "RC_DIR=%ProgramFiles(x86)%\Windows Kits\8.1\Bin\"
   GOTO FOUND

:SET_VC2017E
   SET __MSC__=15
   CALL "%ProgramFiles%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools\VsDevCmd.bat"
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2017\Enterprise\Vc"
   IF "%VS140COMNTOOLS%"=="" SET "VS140COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio\2017\Enterprise\Common7\Tools\"
   IF NOT "%VS140COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS140COMNTOOLS%"
   IF EXIST "%ProgramFiles%\Windows Kits\8.1\Bin\x86\RC.Exe" SET "RC_DIR=%ProgramFiles%\Windows Kits\8.1\Bin\"
   GOTO FOUND

:SET_VC2017PX86
   SET __MSC__=15
   CALL "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\Common7\Tools\VsDevCmd.bat"
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\Vc"
   IF "%VS140COMNTOOLS%"=="" SET "VS140COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\Common7\Tools\"
   IF NOT "%VS140COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS140COMNTOOLS%"
   IF EXIST "%ProgramFiles(x86)%\Windows Kits\8.1\Bin\x86\RC.Exe" SET "RC_DIR=%ProgramFiles(x86)%\Windows Kits\8.1\Bin\"
   GOTO FOUND

:SET_VC2017P
   SET __MSC__=15
   CALL "%ProgramFiles%\Microsoft Visual Studio\2017\Professional\Common7\Tools\VsDevCmd.bat"
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2017\Professional\Vc"
   IF "%VS140COMNTOOLS%"=="" SET "VS140COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio\2017\Professional\Common7\Tools\"
   IF NOT "%VS140COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS140COMNTOOLS%"
   IF EXIST "%ProgramFiles%\Windows Kits\8.1\Bin\x86\RC.Exe" SET "RC_DIR=%ProgramFiles%\Windows Kits\8.1\Bin\"
   GOTO FOUND

:SET_VC2017CX86
   SET __MSC__=15
   CALL "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\Common7\Tools\VsDevCmd.bat"
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\Vc"
   IF "%VS140COMNTOOLS%"=="" SET "VS140COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\Common7\Tools\"
   IF NOT "%VS140COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS140COMNTOOLS%"
   IF EXIST "%ProgramFiles(x86)%\Windows Kits\8.1\Bin\x86\RC.Exe" SET "RC_DIR=%ProgramFiles(x86)%\Windows Kits\8.1\Bin\"
   GOTO FOUND

:SET_VC2017C
   SET __MSC__=15
   CALL "%ProgramFiles%\Microsoft Visual Studio\2017\Community\Common7\Tools\VsDevCmd.bat"
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio\2017\Community\Vc"
   IF "%VS140COMNTOOLS%"=="" SET "VS140COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio\2017\Community\Common7\Tools\"
   IF NOT "%VS140COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS140COMNTOOLS%"
   IF EXIST "%ProgramFiles%\Windows Kits\8.1\Bin\x86\RC.Exe" SET "RC_DIR=%ProgramFiles%\Windows Kits\8.1\Bin\"
   GOTO FOUND

:SET_VC2015X86
   SET __MSC__=14
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\Vc"
   IF "%VS140COMNTOOLS%"=="" SET "VS140COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\Common7\Tools\"
   IF NOT "%VS140COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS140COMNTOOLS%"
   IF EXIST "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET "RC_DIR=%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1A\Bin\"
   GOTO FOUND

:SET_VC2015
   SET __MSC__=14
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio 14.0\vc"
   IF "%VS140COMNTOOLS%"=="" SET "VS140COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio 14.0\Common7\Tools\"
   IF NOT "%VS140COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS140COMNTOOLS%"
   IF EXIST "%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET "RC_DIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\"
   GOTO FOUND

:SET_VC2013X86
   SET __MSC__=12
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\Vc"
   IF "%VS120COMNTOOLS%"=="" SET "VS120COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\Common7\Tools\"
   IF NOT "%VS120COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS120COMNTOOLS%"
   IF EXIST "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET "RC_DIR=%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1A\Bin\"
   GOTO FOUND

:SET_VC2013
   SET __MSC__=12
   SET CC_DIR=%ProgramFiles%\Microsoft Visual Studio 12.0\vc
   IF "%VS120COMNTOOLS%"=="" SET VS120COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio 12.0\Common7\Tools\
   IF NOT "%VS120COMNTOOLS%"=="" SET VSCOMMONTOOLS=%VS120COMNTOOLS%
   IF EXIST "%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET RC_DIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\
   GOTO FOUND

:SET_VC2012X86
   SET __MSC__=11
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\vc"
   IF "%VS110COMNTOOLS%"=="" SET "VS110COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\Common7\Tools\"
   IF NOT "%VS110COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS110COMNTOOLS%"
   IF EXIST "%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET "RC_DIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\"
   IF EXIST "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET "RC_DIR=%ProgramFiles(x86)%\Microsoft SDKs\Windows\v7.1A\Bin\"
   IF EXIST "%ProgramFiles(x86)%\Windows kits\v8.0\Bin\x86\RC.Exe" SET "RC_DIR=%ProgramFiles(x86)%\Windows kits\v8.0\Bin\x86\"
   IF EXIST "%ProgramFiles(x86)%\Windows kits\v8.1\Bin\x86\RC.Exe" SET "RC_DIR=%ProgramFiles(x86)%\Windows kits\v8.1\Bin\x86\"
   GOTO FOUND

:SET_VC2012
   SET __MSC__=11
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio 11.0\vc"
   IF "%VS110COMNTOOLS%"=="" SET "VS110COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio 11.0\Common7\Tools\"
   IF NOT "%VS110COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS110COMNTOOLS%"
   IF EXIST "%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\RC.Exe" SET "RC_DIR=%ProgramFiles%\Microsoft SDKs\Windows\v7.1A\Bin\"
   IF EXIST "%ProgramFiles(x86)%\Windows kits\v8.0\Bin\RC.Exe" SET "RC_DIR=%ProgramFiles(x86)%\Windows kits\v8.0\Bin\"
   GOTO FOUND

:SET_VC2010X86
   SET __MSC__=10
   SET "CC_DIR=%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\vc"
   IF "%VS100COMNTOOLS%"=="" SET "VS100COMNTOOLS=%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\Common7\Tools\"
   IF NOT "%VS100COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS100COMNTOOLS%"
   GOTO FOUND

:SET_VC2010
   SET __MSC__=10
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio 10.0\vc"
   IF "%VS100COMNTOOLS%"=="" SET "VS100COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio 10.0\Common7\Tools\"
   IF NOT "%VS100COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS100COMNTOOLS%"
   GOTO FOUND

:SET_VC2008
   SET __MSC__=9
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio 9.0\vc"
   IF "%VS90COMNTOOLS%"=="" SET "VS90COMNTOOLS=%ProgramFiles%\Microsoft Visual Studio 9.0\Common7\Tools\"
   IF NOT "%VS90COMNTOOLS%"=="" SET "VSCOMMONTOOLS=%VS90COMNTOOLS%"
   GOTO FOUND

:SET_VC2005
   SET __MSC__=8
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio 8\vc"
   GOTO FOUND

:SET_VC2003
   SET __MSC__=7
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio .NET 2003\VC7"
   GOTO FOUND

:SET_VC6
   SET __MSC__=6
   SET "CC_DIR=%ProgramFiles%\Microsoft Visual Studio\VC98"
   GOTO FOUND

:FIND_ROOT
   SETLOCAL EnableDelayedExpansion
      SET "fullPath=%_MSVC_BIN%"
      SET "leftPortion="
      SET "rest=!fullPath!"

      :SplitPath
      FOR /F "tokens=1* delims=\" %%a IN ("!rest!") DO (
         SET "currentPart=%%a"
         SET "rest=%%b"

         IF "!currentPart!"=="VC" (
            SET "leftPortion=!leftPortion!\!currentPart!"
            GOTO Found
         )
         IF DEFINED rest (
            IF "!leftPortion!"=="" (
               SET "leftPortion=!currentPart!" 
            ) ELSE (
               SET "leftPortion=!leftPortion!\!currentPart!"
            )
            GOTO SplitPath
         ) ELSE (
            GOTO NotFound
         )
      )

      :Found
      REM ECHO Found: '!leftPortion!'
      ECHO !leftPortion! > %TEMP%\leftPortion.txt
      GOTO EndScript

      :NotFound
      REM ECHO Not found!
      GOTO EndScript

      :EndScript
   ENDLOCAL

   SET /P CC_DIR=<%TEMP%\leftPortion.txt
   IF "%CC_DIR%"=="" GOTO NOT_FOUND

   REM remove trailing backslash
   SET CC_DIR=%CC_DIR:~0,-1%

   REM GOTO DIR_SET 

:DIR_SET
   REM In MSVC case cl.exe may be in many sub sub directories - so don't check for it's existence instead make sure that cl.exe is in the path
   cl > nul 2>&1 || GOTO NOT_FOUND
   GOTO FOUND

:NOT_FOUND
   rem Let's return an error code to the caller
   echo "ERROR: Microsoft Visual C++ not found!"
   exit /b 1
   
:FOUND
   IF "%CC%"=="" SET "CC=cl"
   IF "%HB_ARCH%"=="" SET "HB_ARCH=w32"

   IF     "%HB_ARCH%"=="w32" SET "SUB_DIR=vc32"
   IF NOT "%HB_ARCH%"=="w32" SET "SUB_DIR=vc64"
   exit /b 0   
