/****************************************************************************
 *                                                                          *
 * File    : xxcos.h                                                        *
 *                                                                          *
 * Purpose : Common cos[fl] functionality.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute cos(x) */
FTYPE __cdecl (FFUN(cos))(FTYPE x)
{
    return FNAME(sin)(x, 1);
}

