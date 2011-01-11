/****************************************************************************
 *                                                                          *
 * File    : xxlog2.h                                                       *
 *                                                                          *
 * Purpose : Common log2[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute log2(x) */
FTYPE __cdecl (FFUN(log2))(FTYPE x)
{
    return FNAME(log)(x, 2);
}

