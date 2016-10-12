/******************************************************************************
*
*  File: ActiveScriptTrace.h
*
*  Author:  Joel Alley
*
*  Date: November 5, 1998
*
*  Description:   Each of the Active Scripting classes implements a debug macro
*                 that allows debug output to be routed for that class by 
*                 defining the macro as output to some sink.  This file 
*                 centralizes the definition of those macros for ease of use.  
*                 All Active Scripting source files are dependent on this file.
*
*  Modifications:
******************************************************************************/

#ifndef _h_ActiveScriptTrace
#define _h_ActiveScriptTrace

#include <stdio.h>

#ifndef _DEBUG
   #undef OutputDebugString
   #define OutputDebugString(c)

   #define OutputDebugValues
#endif

#define SAMPLESCRIPTFACTORYTRACE(x) OutputDebugString( x )
#define CASINTERPRETERTRACE(x)  OutputDebugString( x )
#define EVENTTRACE(x)  
#define NAMEDITEMTRACE(x)  //OutputDebugString( x )
#define SAMPLESCRIPTTRACE(x)  OutputDebugString( x )
#define CASERRORHANDLERTRACE(x) OutputDebugString( x ) 

#endif
