/****************************************************************************
 *                                                                          *
 * File    : _matherr.c                                                     *
 *                                                                          *
 * Purpose : _matherr function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-06-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <math.h>

/* default _matherr function */
int __cdecl _matherr(struct _exception *except)
{
    return 0;
}
