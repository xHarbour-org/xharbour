/****************************************************************************
 *                                                                          *
 * File    : wcscoll.c                                                      *
 *                                                                          *
 * Purpose : wcscoll function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwcsxfrm.h"

/* type definitions */
typedef struct {
    wchar_t buf[32];
    const wchar_t *s1, *s2, *sout;
    mbstate_t state;
} _WSctl;

/* get transformed wchar_ts */
static size_t getwxfrm(_WSctl *p)
{
    size_t i;

    do
    {
        /* loop until wchar_t's delivered */
        p->sout = (const wchar_t *)p->buf;
        i = __wcsxfrm(p->buf, &p->s1, sizeof(p->buf) / sizeof(wchar_t), &p->state);
        if (i > 0 && p->buf[i-1] == L'\0')
            break;
        else if (*p->s1 == L'\0')
            p->s1 = p->s2;  /* rescan */
    } while (i == 0);

    return i;
}

/* compare s1[], s2[] using locale-dependent rule */
int __cdecl (wcscoll)(const wchar_t *s1, const wchar_t *s2)
{
    static const mbstate_t initial = {0};
    size_t n1, n2;
    _WSctl st1, st2;

    st1.s1 = st1.s2 = (const wchar_t *)s1;
    st1.state = initial;
    st2.s1 = st2.s2 = (const wchar_t *)s2;
    st2.state = initial;

    for (n1 = n2 = 0;;)
    {
        /* compare transformed wchar_ts */
        int ans;
        size_t n;

        if (n1 == 0)
            n1 = getwxfrm(&st1);

        if (n2 == 0)
            n2 = getwxfrm(&st2);

        n = (n1 < n2) ? n1 : n2;
        if (n == 0)
            return (n1 == n2) ? 0 : (n2 > 0) ? -1 : +1;
        else if ((ans = wmemcmp(st1.sout, st2.sout, n)) != 0 || n1 == n2 && st1.sout[n-1] == L'\0')
            return ans;

        st1.sout += n, n1 -= n;
        st2.sout += n, n2 -= n;
    }
}

