/****************************************************************************
 *                                                                          *
 * File    : xxxdclass.h                                                    *
 *                                                                          *
 * Purpose : Common _[FL]Dclass functionality [new C99].                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* test floating-point classification */
int __cdecl FNAME(fpclass)(FTYPE x)
{
    return FNAME(fptest)(&x);
}

