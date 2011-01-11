/******************************************************************************
*
*  File: InterpreterTrace.h
*
*  Author:  Joel Alley
*
*  Date: November 5, 1998
*
*  Description:   Each class in the interpreter implements a debug macro that
*                 allows debug output to be routed for that class by defining 
*                 the macro as output to some sink.  This file centralizes the
*                 definition of those macros for ease of use.  All source files
*                 in the interpreter are dependent on this file.
*
*  Modifications:
******************************************************************************/

#define SCRIPTVARIANTTRACE(x) 
#define CINTERPRETERTRACE(x) 
#define CSYMBOLTRACE(x) 
#define CSYMBOLTABLETRACE(x) 
#define CSTACKEDSYMBOLTABLETRACE(x) 
#define PARSETRACE(x)
#define CERRORHANDLERTRACE(x) 
