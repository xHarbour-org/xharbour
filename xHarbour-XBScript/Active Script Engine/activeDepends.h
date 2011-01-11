/******************************************************************************
*
*  File: activeDepends.h
*
*  Author:  Joel Alley
*
*  Date: November 3, 1998
*
*  Description:   This file centralizes the #include directives of the various
*                 source files of this project so they're easier to maintain.
*                 It uses conditional compilation to select which headers to
*                 #include with each source file.
*
*  Modifications: 11/3 -- split this file out to handle just dependencies for
*                         Active Scripting support.
******************************************************************************/


/******************************************************************************
*  Dependencies -- These #define statements control dependencies between
*  various files in the project.
******************************************************************************/

//#pragma comment( linker, "warning disable :4006" )

#include <crtdbg.h>

// To avoid hack forcing C mode in hbole.h
#define HB_OLE_H_

#ifdef CLASSFACTORY
#define XBSCRIPT
#endif

#ifdef XBSCRIPT
#define CASINTERPRETER
#define NAMEDITEM
#define EVENTHANDLER
#define LIST
#define CASERRORHANDLER
#define CLASSFACTORY
#endif

#ifdef EVENTHANDLER
#define LIST
#endif

#ifdef CASERRORHANDLER
#define CASINTERPRETER
#endif

#ifdef CASINTERPRETER
#define NAMEDITEM
#define LIST
#define CASERRORHANDLER
#endif

#if defined( CASINTERPRETER ) || defined( CLASSFACTORY )
//xHarbour
#include "hbapi.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbvm.h"
#include "hboo.ch"
#include "hbdate.h"
#include "hbapilng.h"
#endif

/******************************************************************************
*  #includes -- These #include statements include the files in order such that
*  each files dependencies have been included before they're required.
******************************************************************************/
#include <activscp.h>
#include "ActiveScriptTrace.h"

#ifdef LIST
#include "Tlist.h"
#endif

#ifdef CLASSFACTORY
#include "ClassFactory.h"
#endif

#ifdef CASERRORHANDLER
#include "DebugResume.h"
#include "CASErrorHandler.h"
#endif

#ifdef NAMEDITEM
#include "NamedItem.h"
#endif

#ifdef CASINTERPRETER
#include "CASInterpreter.h"
#endif

#ifdef EVENTHANDLER
#include "CEventHandler.h"
#endif

#ifdef XBSCRIPT
#include "Hostinfo.h"
#include "Objsafe.h"
#include "ASXBScript.h"

#ifdef DLLFUNCTIONS
#include "DLLFunctions.h"
#include "comdef.h"
#include "initguid.h"
#endif

#endif