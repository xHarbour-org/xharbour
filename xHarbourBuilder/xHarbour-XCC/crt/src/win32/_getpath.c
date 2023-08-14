/****************************************************************************
 *                                                                          *
 * File    : _getpath.c                                                     *
 *                                                                          *
 * Purpose : __getpath function -- win32 version.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xcrt.h"

/* extract a pathname from a semicolon-delimited list of pathnames */
char * __getpath(const char *src, char *dst, size_t maxlen)
{
    const char *save_src = src;

    /* strip off leading semicolons */
    while (*src == ';')
        src++;

    /* adjust for the terminating '\0' */
    if (--maxlen == 0)
        return 0;

    /*
     * get the next path in src string.
     */
    while (*src && *src != ';')
    {
        /*
         * check for quote char.
         */
        if (*src != '"')
        {
            *dst++ = *src++;

            if (--maxlen == 0)
                return 0;
        }
        else
        {
            /*
             * found a quote. copy all chars until we hit the
             * final quote or the end of the string.
             */
            src++;   /* skip over opening quote */

            while (*src && *src != '"')
            {
                *dst++ = *src++;

                if (--maxlen == 0)
                    return 0;
            }

            if (*src) src++;  /* skip over closing quote */
        }
    }

    /*
     * if we copied something and stopped because of a ';',
     * skip the ';' before returning.
     */
    while (*src == ';')
        src++;

    /* terminate the result */
    *dst = '\0';
    return (save_src != src) ? (char *)src : 0;
}


