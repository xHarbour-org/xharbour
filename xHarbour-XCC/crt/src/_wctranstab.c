/****************************************************************************
 *                                                                          *
 * File    : _wctranstab.c                                                  *
 *                                                                          *
 * Purpose : xwctrtab conversion table -- ASCII based version.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xwctype.h"

/* static data */
static const wchar_t tr_range_tab[] = {
    L'A', L'Z', L'a' - L'A',            /* 0: towlower */
    L'a', L'z', (wchar_t)(L'A' - L'a')  /* 3: towupper */
};

static const __wcinfo wctrans_tab[] = {
    { (const char *)tr_range_tab, 0 },  /* table pointer, allocated size */
    { "tolower", 0 },
    { "toupper", 3 },
    { (const char *)0, 6 }  /* null pointer, first unused offset */
};

const __wcinfo *__wctranstab = &wctrans_tab[0];

