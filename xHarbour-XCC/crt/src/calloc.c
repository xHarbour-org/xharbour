/****************************************************************************
 *                                                                          *
 * File    : calloc.c                                                       *
 *                                                                          *
 * Purpose : calloc function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>

/* allocate a data object on the heap and clear it */
void * __cdecl (calloc)(size_t nelem, size_t size)
{
    const size_t n = nelem * size;
    char *p = (char *)malloc(n);

    if (p) memset(p, '\0', n);

    return p;
}

