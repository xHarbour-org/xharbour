/****************************************************************************
 *                                                                          *
 * File    : tmpnam.c                                                       *
 *                                                                          *
 * Purpose : tmpnam function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "xstdio.h"

#if L_tmpnam < 13
#error BAD ASSUMPTION ABOUT L_tmpnam
#endif

/* create a temporary file name */
char * __cdecl (tmpnam)(char *s)
{
    char *p;
    int i;
    unsigned short t;
    static char buf[L_tmpnam] = {0};
    static unsigned short seed = 0;
    static char *root = 0;

    if (s == 0)
        s = buf;

    if (root)
        ;
    else if ((p = getenv("TEMP")) == 0 || strlen(p) + 14 > L_tmpnam || (root = (char *)malloc(strlen(p) + 5)) == 0)
        root = "ctm";
    else
    {
        /* setup root directory */
        strcpy(root, p);
        strcat(root, "/ctm");
    }

    ++seed;
    strcpy(s, root);
    i = 5;
    p = s + strlen(s) + i;
    strcpy(p, ".tmp");

    for (t = seed; --i >= 0; t >>= 3)
        *--p = '0' + (t & 07);

    return s;
}

