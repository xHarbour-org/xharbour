/****************************************************************************
 *                                                                          *
 * File    : atoll.c                                                        *
 *                                                                          *
 * Purpose : atoll function [new C99].                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* convert string to long long */
long long __cdecl (atoll)(const char *s)
{
    return (long long)__stoull(s, 0, 10);
}

