/****************************************************************************
 *                                                                          *
 * File    : atol.c                                                         *
 *                                                                          *
 * Purpose : atol function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* convert string to long */
long __cdecl (atol)(const char *s)
{
    return (long)__stoul(s, 0, 10);
}

