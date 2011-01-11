/****************************************************************************
 *                                                                          *
 * File    : _putstr.c                                                      *
 *                                                                          *
 * Purpose : __putstr function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <stdlib.h>
#include "xstdio.h"
#include "xwchar.h"

#define BUF_SIZE  64

/* convert wchar_t string to text */
int __putstr(__prtinfo *px, const wchar_t *pwc)
{
    char ac[BUF_SIZE < MB_LEN_MAX ? MB_LEN_MAX : BUF_SIZE];
    char buf[MB_LEN_MAX], *pac;
    int m = (px->prec < 0) ? INT_MAX : px->prec;
    int n, stat;
    size_t acsize = px->width + MB_CUR_MAX;
    mbstate_t mbst = {0};

    if (px->flags & _FMI || acsize <= sizeof(ac))
        pac = ac, acsize = sizeof(ac);
    else if ((pac = (char *)malloc(acsize)) == 0)
        return EOF;

    if (pwc == 0) pwc = L"(null)";

    for (stat = 0; m > 0; ++pwc, m -= n)
    {
        /* convert a wide character */
        if ((n = __wctomb(buf, *pwc, &mbst)) < 0 || *pwc == 0 && --n < 0)
        {
            /* stop on bad conversion */
            stat = EOF;
            break;
        }
        else if (m < n)
            break;  /* precision exhausted */

        if (acsize < px->n0 + n)
        {
            /* drain buffer */
            px->width = 0;
            __puttxt(px, pac);
            px->n0 = 0;
        }

        memcpy(&pac[px->n0], buf, n);
        px->n0 += n;

        if (*pwc == L'\0')
            break;
    }

    __puttxt(px, pac);
    px->n0 = px->width = 0;

    if (pac != ac)
        free(pac);

    return stat;
}

