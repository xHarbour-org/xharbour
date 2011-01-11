/****************************************************************************
 *                                                                          *
 * File    : setbuf.c                                                       *
 *                                                                          *
 * Purpose : setbuf function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* set up buffer for a stream */
void __cdecl (setbuf)(FILE * restrict str, char * restrict buf)
{
    setvbuf(str, buf, (buf) ? _IOFBF : _IONBF, BUFSIZ);
}

