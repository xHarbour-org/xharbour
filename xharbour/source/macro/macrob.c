/*
 * $Id: macrob.c,v 1.1.1.1 2001/12/21 10:43:27 ronpinkas Exp $
 */

/* hbexprb.c is also included from ../compiler/exproptb.c
 * However it produces a slighty different code if used in
 * macro compiler (there is an additional parameter passed to some functions)
 * 5 - ignore this magic number - this is used to force compilation
*/

#define HB_MACRO_SUPPORT

#include "hbmacro.h"
#include "hbcomp.h"

#include "hbexprb.c"
