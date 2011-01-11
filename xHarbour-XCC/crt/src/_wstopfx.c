/****************************************************************************
 *                                                                          *
 * File    : _wstopfx.c                                                     *
 *                                                                          *
 * Purpose : __wstopfx function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>
#include "xmath.h"

/* parse prefix of floating-point field */
int __wstopfx(const wchar_t **ps, wchar_t **endptr)
{
    const wchar_t *s = *ps;
    int code = 0;

    for (; iswspace(*s); ++s)
            ;

    if (*s == L'-')
        code = FL_NEG, ++s;
    else if (*s == L'+')
        ++s;

    if (*s == L'n' || *s == L'N')
    {
        /* parse "nan" or fail */
        if ((*++s != L'a' && *s != L'A') || (*++s != L'n' && *s != L'N'))
            code = FL_ERR;
        else if (*++s != L'(')
            code = FL_NAN;
        else
        {
            /* parse (n-char-sequence) */
            for (; iswalnum(*++s) || *s == L'_'; )
                ;
            if (*s != L')')
                code = FL_ERR;
            else
                code = FL_NAN, ++s;
        }

        if (endptr != 0)
            *endptr = (wchar_t *)s;
    }
    else if (*s == L'i' || *s == L'I')
    {
        /* parse "inf" or fail */
        if ((*++s != L'n' && *s != L'N') || (*++s != L'f' && *s != L'F'))
            code = FL_ERR;
        else if (*++s != L'i' && *s != L'I')
            code |= FL_INF;
        else
        {
            /* parse rest of L"infinity" */
            if (*++s != L'n' && *s != L'N' || *++s != L'i' && *s != L'I' || *++s != L't' && *s != L'T' || *++s != L'y' && *s != L'Y')
                code = FL_ERR;
            else
                code |= FL_INF, ++s;
        }

        if (endptr != 0)
            *endptr = (wchar_t *)s;
    }
    else if (*s == L'0' && (s[1] == L'x' || s[1] == L'X'))
        s += 2, code |= FL_HEX;
    else
        code |= FL_DEC;

    *ps = s;
    return code;
}

