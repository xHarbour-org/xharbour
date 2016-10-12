/****************************************************************************
 *                                                                          *
 * File    : remove.c                                                       *
 *                                                                          *
 * Purpose : remove function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-08-30  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"
#include "io.h"

/* remove a file */
int __cdecl (remove)(const char *fname)
{
    return _unlink(fname);
}

