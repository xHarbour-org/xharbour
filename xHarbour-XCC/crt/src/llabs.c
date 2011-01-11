/****************************************************************************
 *                                                                          *
 * File    : llabs.c                                                        *
 *                                                                          *
 * Purpose : llabs function [new C99].                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* compute absolute value of long long argument */
long long __cdecl (llabs)(long long i)
{
    return (i < 0) ? -i : i;
}

