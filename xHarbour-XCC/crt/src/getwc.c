/****************************************************************************
 *                                                                          *
 * File    : getwc.c                                                        *
 *                                                                          *
 * Purpose : getwc function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* get a character from wide stream */
wint_t __cdecl (getwc)(FILE *str)
{
    return fgetwc(str);
}

