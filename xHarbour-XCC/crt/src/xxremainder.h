/****************************************************************************
 *                                                                          *
 * File    : xxremainder.h                                                  *
 *                                                                          *
 * Purpose : Common remainder[fl] functionality.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute remainder of x/y */
FTYPE __cdecl FFUN(remainder)(FTYPE x, FTYPE y)
{
    return FFUN(remquo)(x, y, 0);
}

