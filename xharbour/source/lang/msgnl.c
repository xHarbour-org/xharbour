/*
 * $Id: msgnl.c,v 1.2 2005/06/10 22:51:37 ronpinkas Exp $
 */

/* Language Support Module */

/* Language name: Dutch */
/* ISO language code (2 chars): NL */
/* Codepage: ???? */

#include "hbdefs.h"

char *hb_dateMonthsName[ 12 ] =
{
   "januari",
   "februari",
   "maart",
   "april",
   "mei",
   "juni",
   "juli",
   "augustus",
   "september",
   "oktober",
   "november",
   "december"
};

char *hb_dateDaysName[ 7 ] =
{
   "zondag",
   "maandag",
   "dinsdag",
   "woensdag",
   "donderdag",
   "vrijdag",
   "zaterdag"
};

char *hb_errorsGeneric[] =
{
   "Unknown error",
   "Argument error",
   "Bound error",
   "String overflow",
   "Numeric overflow",
   "Zero divisor",
   "Numeric error",
   "Syntax error",
   "Operation too complex",
   "",
   "",
   "Memory low",
   "Undefined function",
   "No exported method",
   "Variable does not exist",
   "Alias does not exist",
   "No exported variable",
   "Illegal characters in alias",
   "Alias already in use",
   "",
   "Create error",
   "Open error",
   "Close error",
   "Read error",
   "Write error",
   "Print error",
   "",
   "",
   "",
   "",
   "Operation not supported",
   "Limit exceeded",
   "Corruption detected",
   "Data type error",
   "Data width error",
   "Workarea not in use",
   "Workarea not indexed",
   "Exclusive required",
   "Lock required",
   "Write not allowed",
   "Append lock failed",
   "Lock Failure",
   "",
   "",
   "",
   "Incorrect number of arguments",
   "array access",
   "array assign",
   "not an array",
   "conditional",
   "Invalid self"
};

HB_LANG_ANNOUNCE( NL );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_NL )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_NL )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup hb_lang_Init_NL
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_hb_lang_Init_NL = hb_lang_Init_NL;
   #pragma data_seg()
#endif

