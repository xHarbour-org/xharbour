/****************************************************************************
 *                                                                          *
 * File    : atof.c                                                         *
 *                                                                          *
 * Purpose : atof function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* convert string to double */
double __cdecl (atof)(const char *s)
{
    return __stod(s, 0, 0);
}

