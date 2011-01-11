/****************************************************************************
 *                                                                          *
 * File    : wcslwr.c                                                       *
 *                                                                          *
 * Purpose : _wcslwr function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>
#include <wctype.h>

/* convert a wide string in place to lowercase */
wchar_t * __cdecl (_wcslwr)(wchar_t *s)
{
    wchar_t *s1;

    if (s)
    {
        for (s1 = s; *s1 != '\0'; s1++)
            if (iswupper(*s1)) *s1 = towlower(*s1);
    }

    return s;
}

