/****************************************************************************
 *                                                                          *
 * File    : ctime.c                                                        *
 *                                                                          *
 * Purpose : ctime function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <time.h>

/* convert calendar time to local text */
char * __cdecl (ctime)(const time_t *tod)
{
    return asctime(localtime(tod));
}

