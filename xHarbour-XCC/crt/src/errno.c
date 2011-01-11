/****************************************************************************
 *                                                                          *
 * File    : errno.c                                                        *
 *                                                                          *
 * Purpose : errno storage.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#undef errno

/* errno storage */

#ifndef __MT__
int errno = 0;
#endif

