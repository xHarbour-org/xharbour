/****************************************************************************
 *                                                                          *
 * File    : labs.c                                                         *
 *                                                                          *
 * Purpose : labs function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* compute absolute value of long argument */
long __cdecl (labs)(long i)
{
    return (i < 0) ? -i : i;
}

