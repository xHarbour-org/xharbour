/****************************************************************************
 *                                                                          *
 * File    : putc.c                                                         *
 *                                                                          *
 * Purpose : putc function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* put character to stream */
int __cdecl (putc)(int c, FILE *str)
{
    return fputc(c, str);
}

