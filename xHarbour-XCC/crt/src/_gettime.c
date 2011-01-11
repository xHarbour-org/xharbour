/****************************************************************************
 *                                                                          *
 * File    : _gettime.c                                                     *
 *                                                                          *
 * Purpose : __gettime function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xtime.h"

/* get time info from environment */
const char *__gettime(const char *s, int n, int *len)
{
    const char delim = (*s) ? *s++ : '\0';
    const char *s1;

    for (;; --n, s = s1 + 1)
    {
        /* find end of current field */
        if ((s1 = strchr(s, delim)) == 0)
            s1 = s + strlen(s);

        if (n <= 0)
        {
            /* found proper field */
            *len = s1 - s;
            return s;
        }
        else if (*s1 == '\0')
        {
            /* not enough fields */
            *len = 0;
            return s1;
        }
    }
}

