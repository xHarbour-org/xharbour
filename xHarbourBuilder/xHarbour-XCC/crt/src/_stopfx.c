/****************************************************************************
 *                                                                          *
 * File    : _stopfx.c                                                      *
 *                                                                          *
 * Purpose : __stopfx function [new C99].                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include "xmath.h"

/* parse prefix of floating-point field */
int __stopfx(const char **ps, char **endptr)
{
    const char *s = *ps;
    int code = 0;

    for (; isspace(*s); ++s)
        ;

    if (*s == '-')
        code = FL_NEG, ++s;
    else if (*s == '+')
        ++s;

    if (*s == 'n' || *s == 'N')
    {
        /* parse "nan" or fail */
        if ((*++s != 'a' && *s != 'A') || (*++s != 'n' && *s != 'N'))
            code = FL_ERR;
        else if (*s != '(')
            code = FL_NAN, ++s;
        else
        {
            /* parse (n-char-sequence) */
            for (; isalnum(*++s) || *s == '_'; )
                ;
            if (*s != ')')
                code = FL_ERR;
            else
                code = FL_NAN, ++s;
        }

        if (endptr != 0)
            *endptr = (char *)s;
    }
    else if (*s == 'i' || *s == 'I')
    {
        /* parse "inf" or fail */
        if ((*++s != 'n' && *s != 'N') || (*++s != 'f' && *s != 'F'))
            code = FL_ERR;
        else if (*++s != 'i' && *s != 'I')
            code |= FL_INF;
        else
        {
            /* parse rest of  "infinity" */
            if (*++s != 'n' && *s != 'N' || *++s != 'i' && *s != 'I' || *++s != 't' && *s != 'T' || *++s != 'y' && *s != 'Y')
                code = FL_ERR;
            else
                code |= FL_INF, ++s;
        }

        if (endptr != 0)
            *endptr = (char *)s;
    }
    else if (*s == '0' && (s[1] == 'x' || s[1] == 'X'))
        s += 2, code |= FL_HEX;
    else
        code |= FL_DEC;

    *ps = s;
    return code;
}

