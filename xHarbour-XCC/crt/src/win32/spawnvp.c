/****************************************************************************
 *                                                                          *
 * File    : spawnvp.c                                                      *
 *                                                                          *
 * Purpose : _spawnvp function -- win32 version.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <process.h>

/* spawn a child process - search along PATH */
int __cdecl (_spawnvp)(int mode, const char *cmd, char * const *argv)
{
    return _spawnvpe(mode, cmd, argv, 0);
}

