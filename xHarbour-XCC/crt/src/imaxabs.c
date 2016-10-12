/****************************************************************************
 *                                                                          *
 * File    : imaxabs.c                                                      *
 *                                                                          *
 * Purpose : imaxabs function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stddef.h>
#include <inttypes.h>

/* compute absolute value of intmax_t argument */
intmax_t __cdecl (imaxabs)(intmax_t i)
{
    return (i < 0) ? -i : i;
}

