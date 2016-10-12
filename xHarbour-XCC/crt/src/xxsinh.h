/****************************************************************************
 *                                                                          *
 * File    : xxsinh.h                                                       *
 *                                                                          *
 * Purpose : Common sinh[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute sinh(x) */
FTYPE __cdecl (FFUN(sinh))(FTYPE x)
{
    return FNAME(sinh)(x, FLIT(1.0));
}

