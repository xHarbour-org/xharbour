/****************************************************************************
 *                                                                          *
 * File    : _wcstod.c                                                      *
 *                                                                          *
 * Purpose : __wcstod function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <wchar.h>
#include "xmath.h"
#include "xxwctype.h"
#include "xxdftype.h"

FTYPE __cdecl __wcstod(const CTYPE *s, CTYPE **endptr, long pten)
#include "xxstod.h"

