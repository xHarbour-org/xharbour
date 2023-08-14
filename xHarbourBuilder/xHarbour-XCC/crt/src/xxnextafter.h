/****************************************************************************
 *                                                                          *
 * File    : xxnextafter.h                                                  *
 *                                                                          *
 * Purpose : Common nextafter[fl] functionality.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute next value after x going toward y */
FTYPE __cdecl FFUN(nextafter)(FTYPE x, FTYPE y)
{
    return FFUN(nexttoward)(x, (long double)y);
}

