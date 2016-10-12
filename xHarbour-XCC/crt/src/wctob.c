/****************************************************************************
 *                                                                          *
 * File    : wctob.c                                                        *
 *                                                                          *
 * Purpose : wctob function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* translate wint_t to one-byte multibyte */
int __cdecl (wctob)(wint_t wc)
{
    return __wctob(wc);
}

