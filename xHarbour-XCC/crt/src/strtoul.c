/****************************************************************************
 *                                                                          *
 * File    : strtoul.c                                                      *
 *                                                                          *
 * Purpose : strtoul function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* convert string to unsigned long, with checking */
unsigned long __cdecl (strtoul)(const char * restrict s, char ** restrict endptr, int base)
{
    return __stoul(s, endptr, base);
}

