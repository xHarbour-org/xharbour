/****************************************************************************
 *                                                                          *
 * File    : xxcosh.h                                                       *
 *                                                                          *
 * Purpose : Common cosh[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute cosh(x) */
FTYPE __cdecl (FFUN(cosh))(FTYPE x)
{
    return FNAME(cosh)(x, FLIT(1.0));
}

