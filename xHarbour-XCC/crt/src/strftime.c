/****************************************************************************
 *                                                                          *
 * File    : strftime.c                                                     *
 *                                                                          *
 * Purpose : strftime function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xtime.h"

/* format time to string */
size_t __cdecl (strftime)(char * restrict s, size_t n, const char * restrict fmt, const struct tm * restrict t)
{
    return __strftime(s, n, fmt, t, &__times);
}

