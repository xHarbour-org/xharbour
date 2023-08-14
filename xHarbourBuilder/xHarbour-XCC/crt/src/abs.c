/****************************************************************************
 *                                                                          *
 * File    : abs.c                                                          *
 *                                                                          *
 * Purpose : abs function.                                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* compute absolute value of int argument */
int __cdecl (abs)(int i)
{
    return (i < 0) ? -i : i;
}

