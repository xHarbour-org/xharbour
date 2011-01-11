/****************************************************************************
 *                                                                          *
 * File    : abort.c                                                        *
 *                                                                          *
 * Purpose : abort function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <signal.h>

/* terminate abruptly */
void __cdecl (abort)(void)
{
    raise(SIGABRT);
    exit(EXIT_FAILURE);
}

