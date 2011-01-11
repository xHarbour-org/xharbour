/****************************************************************************
 *                                                                          *
 * File    : strtoll.c                                                      *
 *                                                                          *
 * Purpose : strtoll function (new C99).                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>

/* convert string to long long, with checking */
long long __cdecl (strtoll)(const char * restrict s, char ** restrict endptr, int base)
{
    return __stoll(s, endptr, base);
}

