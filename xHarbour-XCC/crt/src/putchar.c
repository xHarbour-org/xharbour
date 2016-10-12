/****************************************************************************
 *                                                                          *
 * File    : putchar.c                                                      *
 *                                                                          *
 * Purpose : putchar function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* put character to stdout */
int __cdecl (putchar)(int c)
{
    return fputc(c, stdout);
}

