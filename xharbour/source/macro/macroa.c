/*
 * $Id: macroa.c,v 1.8 2001/08/04 14:19:29 rglab Exp $
 */

/* hbexpra.c is also included from ../compiler/expropta.c
 * However it produces a slighty different code if used in 
 * macro compiler (there is an additional parameter passed to some functions)
 * 7 - ignore this magic number - this is used to force compilation
*/

#define HB_MACRO_SUPPORT

#include "hbmacro.h"
#include "hbcomp.h"

#include "hbexpra.c"
