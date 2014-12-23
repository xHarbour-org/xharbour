/*
 * $Id$
 */

#define yylex hb_comp_yylex

#include "hbcomp.h"
#include "harboury.h"
#include "hbsetup.h"
#include "hberrors.h"
#include "hbdefs.h"

#define MAX_STREAM_STARTER    3                       /* "[[" */
#define MAX_STREAM_TERMINATOR 3                       /* "]]"   */
#define MAX_STREAM_EXCLUSIONS 2

#define TOKEN_SIZE            ( 65 > HB_SYMBOL_NAME_LEN + 1 ? 65 : HB_SYMBOL_NAME_LEN + 1 )

#define YY_BUF_SIZE           0 /* Use PP allocated buffer. */

#if ( defined( __LCC__ ) || defined( __XCC__ ) )
   #define SLX_RULES          "source/compiler/harbour.slx"
#else
   #define SLX_RULES          "../source/compiler/harbour.slx"
#endif

#include "simplex.c"
