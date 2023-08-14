/****************************************************************************
 *                                                                          *
 * File    : spawnv.c                                                       *
 *                                                                          *
 * Purpose : _spawnv function -- win32 version.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <process.h>

/* spawn a child process */
int __cdecl (_spawnv)(int mode, const char *cmd, char * const *argv)
{
    return _spawnve(mode, cmd, argv, 0);
}

