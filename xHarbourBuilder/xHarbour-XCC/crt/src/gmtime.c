/****************************************************************************
 *                                                                          *
 * File    : gmtime.c                                                       *
 *                                                                          *
 * Purpose : gmtime function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-03  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xtime.h"

/* convert to Greenwich Mean Time (UTC) */
struct tm * __cdecl (gmtime)(const time_t *tod)
{
    return __ttotm(0, *tod, 0);
}

