/****************************************************************************
 *                                                                          *
 * File    : xxlog.h                                                        *
 *                                                                          *
 * Purpose : Common log[fl] functionality.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute ln(x) */
FTYPE __cdecl (FFUN(log))(FTYPE x)
{
    return FNAME(log)(x, 0);
}

