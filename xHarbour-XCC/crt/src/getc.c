/****************************************************************************
 *                                                                          *
 * File    : getc.c                                                         *
 *                                                                          *
 * Purpose : getc function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* get a character from stream */
int __cdecl (getc)(FILE *str)
{
    return fgetc(str);
}

