/****************************************************************************
 *                                                                          *
 * File    : _wcstof.c                                                      *
 *                                                                          *
 * Purpose : __wcstof function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>
#include "xmath.h"
#include "xxwctype.h"
#include "xxfftype.h"

FTYPE __cdecl __wcstof(const CTYPE *s, CTYPE **endptr, long pten)
#include "xxstod.h"

