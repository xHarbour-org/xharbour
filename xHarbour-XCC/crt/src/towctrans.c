/****************************************************************************
 *                                                                          *
 * File    : towctrans.c                                                    *
 *                                                                          *
 * Purpose : towctrans function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* external wrapper */
wint_t __cdecl (towctrans)(wint_t wc, wctrans_t desc)
{
    return __towctrans(wc, desc);
}

