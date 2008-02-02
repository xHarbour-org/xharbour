/*
 * $Id: macrob.c,v 1.11 2006/07/09 18:11:31 ronpinkas Exp $
 */

/* hbexprb.c is also included from ../compiler/exproptb.c
 * However it produces a slighty different code if used in
 * macro compiler (there is an additional parameter passed to some functions)
 * 1.122 - ignore this revision number - this is used to force compilation
*/

#define HB_MACRO_SUPPORT

#include "hbmacro.h"
#include "hbcomp.h"

#include "hbexprb.c"
