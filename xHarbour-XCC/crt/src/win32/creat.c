/****************************************************************************
 *                                                                          *
 * File    : creat.c                                                        *
 *                                                                          *
 * Purpose : lowio _creat function -- win32 version.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <fcntl.h>
#include "xio.h"

/* create a new file */
int __cdecl _creat(const char *fname, int pmode)
{
    /* creat is just the same as open... */
    return _open(fname, _O_CREAT|_O_TRUNC|_O_RDWR, pmode);
}

