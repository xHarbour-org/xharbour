/****************************************************************************
 *                                                                          *
 * File    : _fslot.c                                                       *
 *                                                                          *
 * Purpose : __fslot function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xstdio.h"

/* find an available FILE */
FILE *__fslot(void)
{
    FILE *str = 0;
    size_t i;

    for (i = 0; i < FOPEN_MAX; ++i)
    {
        if (__filetab[i] == 0)
        {
            /* setup empty __filetab[i] */
            str = (FILE *)malloc(sizeof(FILE));
            if (str == 0) break;

            __filetab[i] = str;
            str->mode = _MALFIL;
            break;
        }
        else if (__filetab[i]->mode == 0)
        {
            /* setup preallocated __filetab[i] */
            str = __filetab[i];
            str->mode = (unsigned short)~_MALFIL;
            break;
        }
    }

    return str;
}

