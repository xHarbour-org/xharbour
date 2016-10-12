/******************************************************************************
*
*  File: depends.h
*
*  Author:  Joel Alley
*
*  Date: October 4, 1998
*
*  Description:   This file centralizes the #include directives of the various
*                 source files of this project so they're easier to maintain.
*                 It uses conditional compilation to select which headers to
*                 #include with each source file.
*
*  Modifications:
******************************************************************************/

/******************************************************************************
*  Dependencies -- These #define statements control dependencies between 
*  various files in the project.
******************************************************************************/
#ifdef CINTERPRETER
#define SCRIPTVARIANT
#define LIST
#define ERRORHANDLER
#define DEBUGRESUME
#endif

#ifdef ERRORHANDLER
#define DEBUGRESUME
#endif

#ifdef LIST
#endif

#ifdef SCRIPTVARIANT
#endif

/******************************************************************************
*  #includes -- These #include statements include the files in order such that
*  each files dependencies have been included before they're required.
******************************************************************************/
#include "InterpreterTrace.h"
#include "activscp.h"

#ifdef SCRIPTVARIANT
#include "ScriptVariant.h"
#endif

#ifdef LIST
#include "TList.h"
#endif

#ifdef DEBUGRESUME
#include "DebugResume.h"
#endif

#ifdef ERRORHANDLER
#include "ErrorHandler.h"
#endif
