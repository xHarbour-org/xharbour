/****************************************************************************
 *                                                                          *
 * File    : strcoll.c                                                      *
 *                                                                          *
 * Purpose : strcoll function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xstrxfrm.h"

/* type definitions */
typedef struct Storage {
    char buf[32];
    const char *sout;
    __xfrm xstate;
} Storage;

/* get transformed chars */
static size_t getxfrm(Storage *p)
{
    p->sout = (const char *)p->buf;
    return __cstrxfrm(p->buf, sizeof(p->buf), &p->xstate);
}

/* compare s1[], s2[] using locale-dependent rule */
int __cdecl (strcoll)(const char *s1, const char *s2)
{
    static const Storage initial = {0};
    Storage st1 = initial;
    Storage st2 = initial;
    size_t n1, n2;

    st1.xstate.sin = st1.xstate.sbegin = (const unsigned char *)s1;
    st2.xstate.sin = st2.xstate.sbegin = (const unsigned char *)s2;
    for (n1 = n2 = 0;;)
    {
        /* compare transformed chars */
        int ans;
        size_t n;

        if (n1 == 0)
            n1 = getxfrm(&st1);
        if (n2 == 0)
            n2 = getxfrm(&st2);

        n = (n1 < n2) ? n1 : n2;
        if (n == 0)
            return (n1 == n2) ? 0 : (n2 > 0) ? -1 : +1;
        else if ((ans = memcmp(st1.sout, st2.sout, n)) != 0 || n1 == n2 && st1.sout[n - 1] == '\0')
            return ans;

        st1.sout += n, n1 -= n;
        st2.sout += n, n2 -= n;
    }
}

