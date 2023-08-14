/****************************************************************************
 *                                                                          *
 * File    : xxlog10.h                                                      *
 *                                                                          *
 * Purpose : Common log10[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute ln(x) */
FTYPE __cdecl (FFUN(log10))(FTYPE x)
{
    return FNAME(log)(x, 1);
}

