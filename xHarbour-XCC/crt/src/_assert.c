/****************************************************************************
 *                                                                          *
 * File    : _assert.c                                                      *
 *                                                                          *
 * Purpose : __assert function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

/* print assertion message and abort */
void __cdecl __assert(char *__expr, char *__file, int __line, const char *__func)
{
    char ac[10], *p;

    *(p = &ac[sizeof(ac) - 1]) = '\0';
    do *--p = __line % 10 + '0'; while ((__line /= 10) != 0);

    fputs("Assertion failed: ", stderr);
    fputs("file ", stderr);
    fputs(__file, stderr);
    fputs(", func ", stderr);
    fputs(__func, stderr);
    fputs(", line ", stderr);
    fputs(p, stderr);
    fputs(", ", stderr);
    fputs(__expr, stderr);
    fputs("\n", stderr);
    fflush(0);

    raise(SIGABRT);
    exit(EXIT_FAILURE);
}

