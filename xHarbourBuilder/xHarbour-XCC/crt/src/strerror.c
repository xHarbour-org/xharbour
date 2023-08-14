/****************************************************************************
 *                                                                          *
 * File    : strerror.c                                                     *
 *                                                                          *
 * Purpose : strerror function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#include <string.h>

char *_Strerror(int, char *);

/* find error message corresponding to errcode */
char * __cdecl (strerror)(int errcode)
{
    return _Strerror(errcode, 0);
}

