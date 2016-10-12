/****************************************************************************
 *                                                                          *
 * File    : _stof.c                                                        *
 *                                                                          *
 * Purpose : __stof function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xmath.h"
#include "xxcctype.h"
#include "xxfftype.h"

FTYPE __cdecl __stof(const CTYPE *s, CTYPE **endptr, long pten)
#include "xxstod.h"

