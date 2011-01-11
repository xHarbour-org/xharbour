/****************************************************************************
 *                                                                          *
 * File    : xxsin.h                                                        *
 *                                                                          *
 * Purpose : Common sin[fl] functionality.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute sin(x) */
FTYPE __cdecl (FFUN(sin))(FTYPE x)
{
    return FNAME(sin)(x, 0);
}

