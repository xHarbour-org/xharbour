/****************************************************************************
 *                                                                          *
 * File    : wcstok.c                                                       *
 *                                                                          *
 * Purpose : wcstok function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* find next token in wchar_t s1[] delimited by s2[] */
wchar_t * __cdecl (wcstok)(wchar_t * restrict s1, const wchar_t * restrict s2, wchar_t ** restrict ps)
{
    static const wchar_t nullstr[1] = {L'\0'};
    wchar_t *sbegin, *send;

    sbegin = (s1) ? s1 : *ps;
    sbegin += wcsspn(sbegin, s2);

    if (*sbegin == L'\0')
    {
        /* end of scan */
        *ps = (wchar_t *)nullstr;  /* for safety */
        return 0;
    }

    send = sbegin + wcscspn(sbegin, s2);

    if (*send != L'\0')
        *send++ = L'\0';

    *ps = send;

    return sbegin;
}

