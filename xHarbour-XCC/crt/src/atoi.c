/****************************************************************************
 *                                                                          *
 * File    : atoi.c                                                         *
 *                                                                          *
 * Purpose : atoi function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* convert string to int */
int __cdecl (atoi)(const char *s)
{
    return (int)__stoul(s, 0, 10);
}

