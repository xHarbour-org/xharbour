/****************************************************************************
 *                                                                          *
 * File    : tell.c                                                         *
 *                                                                          *
 * Purpose : lowio _tell function -- win32 version.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>
#include <io.h>

/* get current position of the file pointer */
long __cdecl (_tell)(int fh)
{
    return _lseek(fh, 0, SEEK_CUR);
}

