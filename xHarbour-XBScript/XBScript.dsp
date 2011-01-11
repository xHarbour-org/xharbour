# Microsoft Developer Studio Project File - Name="XBScript" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=XBScript - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "XBScript.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "XBScript.mak" CFG="XBScript - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "XBScript - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "XBScript - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "XBScript - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O1 /I "Scripting" /I "Interpreter" /I "Active Script Engine" /I "\xharbour\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /TP /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ad1.lib vm.lib rtl.lib common.lib macro.lib pp.lib lang.lib rdd.lib dbfntx.lib dbfdbt.lib dbfcdx.lib dbffpt.lib what32.lib  g:\sqlrdd\lib\sql.lib /nologo /version:1.7 /subsystem:windows /dll /map /debug /machine:I386 /nodefaultlib:"libc.lib" /libpath:"c:\xharbour\lib" /libpath:"Scripting" /FORCE:MULTIPLE
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "XBScript - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ad1.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "XBScript - Win32 Release"
# Name "XBScript - Win32 Debug"
# Begin Group "Active Script Engine"

# PROP Default_Filter ""
# Begin Source File

SOURCE=".\Active Script Engine\activeDepends.h"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\ActiveScriptTrace.h"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\CASErrorHandler.cpp"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\CASErrorHandler.h"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\CASInterpreter.cpp"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\CASInterpreter.h"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\CEventHandler.cpp"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\CEventHandler.h"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\ClassFactory.cpp"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\ClassFactory.h"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\NamedItem.cpp"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\NamedItem.h"
# End Source File
# Begin Source File

SOURCE=.\Interpreter\TList.h
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\XBScript.cpp"
# End Source File
# Begin Source File

SOURCE=".\Active Script Engine\XBScript.h"
# End Source File
# End Group
# Begin Source File

SOURCE=.\DLLFunctions.cpp
# End Source File
# Begin Source File

SOURCE=.\DLLFunctions.h
# End Source File
# Begin Source File

SOURCE=.\gt_nul.cpp

!IF  "$(CFG)" == "XBScript - Win32 Release"

# ADD CPP /I "c:\xharbour\include"

!ELSEIF  "$(CFG)" == "XBScript - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\misc.cpp
# End Source File
# Begin Source File

SOURCE=.\mousenul.cpp

!IF  "$(CFG)" == "XBScript - Win32 Release"

# ADD CPP /I "c:\xharbour\include"

!ELSEIF  "$(CFG)" == "XBScript - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\..\xharbour\tests\pp.c

!IF  "$(CFG)" == "XBScript - Win32 Release"

# ADD CPP /I "c:\xHarbour\include"
# SUBTRACT CPP /I "Scripting" /I "Interpreter" /I "Active Script Engine" /YX

!ELSEIF  "$(CFG)" == "XBScript - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Readme.txt
# End Source File
# Begin Source File

SOURCE=.\TestPage.htm
# End Source File
# Begin Source File

SOURCE=.\XBScript.def
# End Source File
# End Target
# End Project
