/*
 * $Id: hbslex.c,v 1.5 2003/06/17 14:22:22 ronpinkas Exp $
 */

#include "hbcomp.h"
#include "harboury.h"
#include "hbsetup.h"
#include "hberrors.h"
#include "hbdefs.h"

#define MAX_STREAM                              16384 /* Max length of in-line LITERAL */
#define MAX_STREAM_STARTER                          7 /* "QOUT([" */
#define MAX_STREAM_TERMINATOR                       4 /* "])\n"   */
#define MAX_STREAM_EXCLUSIONS                       2

#define TOKEN_SIZE             ( 65 > HB_SYMBOL_NAME_LEN + 1 ? 65 : HB_SYMBOL_NAME_LEN + 1 )

#define YY_BUF_SIZE HB_PP_STR_SIZE

#ifdef OS_DOS_COMPATIBLE
   #define SLX_RULES "..\source\compiler\harbour.slx"
#else
   #define SLX_RULES "../source/compiler/harbour.slx"
#endif

#include "simplex.c"
