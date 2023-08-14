/****************************************************************************
 *                                                                          *
 * File    : xxnan.h                                                        *
 *                                                                          *
 * Purpose : Common nan[fl] functionality.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <float.h>
#include "xmath.h"

/* construct a NaN */
FTYPE __cdecl FFUN(nan)(const char *s)
{
    return (FCONST(nan));
}

