/*
 * $Id: macrob.c,v 1.2 2002/10/18 05:15:55 ronpinkas Exp $
 */

/* hbexprb.c is also included from ../compiler/exproptb.c
 * However it produces a slighty different code if used in
 * macro compiler (there is an additional parameter passed to some functions)
 * 16 - ignore this magic number - this is used to force compilation
*/

#define HB_MACRO_SUPPORT

#include "hbmacro.h"
#include "hbcomp.h"

#include "hbexprb.c"
