/****************************************************************************
 *                                                                          *
 * File    : _files.c                                                       *
 *                                                                          *
 * Purpose : __filetab data object.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* standard error buffer */
static unsigned char ebuf[80] = {0};

/* the standard streams */
FILE __stdin = {  /* standard input */
    _MOPENR, 0,
    &__stdin.cbuf, &__stdin.cbuf + 1, &__stdin.cbuf,
    &__stdin.cbuf, &__stdin.cbuf,
    __stdin.backbuf + sizeof(__stdin.backbuf),
    __stdin.wbackbuf + sizeof(__stdin.wbackbuf) / sizeof(wchar_t)
};

FILE __stdout = { /* standard output */
    _MOPENW, 1,
    &__stdout.cbuf, &__stdout.cbuf + 1, &__stdout.cbuf,
    &__stdout.cbuf, &__stdout.cbuf,
    __stdout.backbuf + sizeof(__stdout.backbuf),
    __stdout.wbackbuf + sizeof(__stdout.wbackbuf) / sizeof(wchar_t)
};

FILE __stderr = { /* standard error */
    _MOPENW|_MNBF, 2,
    ebuf, ebuf + sizeof(ebuf), ebuf,
    ebuf, ebuf,
    __stderr.backbuf + sizeof(__stderr.backbuf),
    __stderr.wbackbuf + sizeof(__stderr.wbackbuf) / sizeof(wchar_t)
};

/* the array of stream pointers */
FILE *__filetab[FOPEN_MAX] = { &__stdin, &__stdout, &__stderr };

