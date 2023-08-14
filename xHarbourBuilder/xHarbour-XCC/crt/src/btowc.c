/****************************************************************************
 *                                                                          *
 * File    : btowc.c                                                        *
 *                                                                          *
 * Purpose : btowc function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* convert single byte to wide character */
wint_t __cdecl (btowc)(int c)
{
    return __btowc(c);
}

