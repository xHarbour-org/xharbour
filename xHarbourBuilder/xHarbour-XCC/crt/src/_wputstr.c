/****************************************************************************
 *                                                                          *
 * File    : _wputstr.c                                                     *
 *                                                                          *
 * Purpose : __wputstr function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xwstdio.h"
#include "xwchar.h"

#define BUF_SIZE  64

/* convert byte string to wchar_ts */
int __wputstr(__wprtinfo *px, const char *pc)
{
    int m = (px->prec < 0) ? INT_MAX : px->prec;
    int n, stat;
    size_t acsize = px->width;
    wchar_t ac[BUF_SIZE], *pac;
    mbstate_t mbst = {0};

    if (px->flags & _FMI || acsize <= sizeof(ac) / sizeof(wchar_t))
        pac = ac, acsize = sizeof(ac);
    else if ((pac = (wchar_t *)malloc(acsize)) == 0)
        return EOF;

    for (stat = 0; m > 0; pc += n, ++px->n0, --m)
    {
        /* generate a wide character */
        if (acsize <= px->n0)
        {
            /* drain buffer */
            px->width = 0;
            __wputtxt(px, pac);
            px->n0 = 0;
        }

        if ((n = __mbtowc(&pac[px->n0], pc, INT_MAX, &mbst)) < 0)
        {
            /* stop on bad conversion */
            stat = EOF;
            break;
        }
        else if (n == 0 && pac[px->n0] == L'\0')
            break;
    }

    __wputtxt(px, pac);
    px->n0 = px->width = 0;

    if (pac != ac)
        free(pac);

    return stat;
}

