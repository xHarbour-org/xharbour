/****************************************************************************
 *                                                                          *
 * File    : iswctype.c                                                     *
 *                                                                          *
 * Purpose : iswctype function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* external wrapper */
int __cdecl (iswctype)(wint_t wc, wctype_t desc)
{
    return __iswctype(wc, desc);
}

