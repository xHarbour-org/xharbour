/****************************************************************************
 *                                                                          *
 * File    : wcsdup.c                                                       *
 *                                                                          *
 * Purpose : _wcsdup function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>
#include <stdlib.h>

/* duplicate wide string into allocated memory */
wchar_t * __cdecl (_wcsdup)(const wchar_t *s)
{
    wchar_t *s1;

    if (s != 0 && (s1 = (wchar_t *)malloc((wcslen(s) + 1) * sizeof(wchar_t))) != 0)
        return wcscpy(s1, s);

    return 0;
}

