/****************************************************************************
 *                                                                          *
 * File    : wcsupr.c                                                       *
 *                                                                          *
 * Purpose : _wcsupr function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>
#include <wctype.h>

/* convert a wide string in place to uppercase */
wchar_t * __cdecl (_wcsupr)(wchar_t *s)
{
    wchar_t *s1;

    if (s)
    {
        for (s1 = s; *s1 != '\0'; s1++)
            if (iswlower(*s1)) *s1 = towupper(*s1);
    }

    return s;
}

