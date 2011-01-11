/****************************************************************************
 *                                                                          *
 * File    : spawnl.c                                                       *
 *                                                                          *
 * Purpose : _spawnl function -- win32 version.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <process.h>

/* spawn a child process */
int __cdecl (_spawnl)(int mode, const char *cmd, const char *arg0, ...)
{
    return _spawnve(mode, cmd, (char * const *)&arg0, 0);
}

