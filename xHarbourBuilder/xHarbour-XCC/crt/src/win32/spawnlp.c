/****************************************************************************
 *                                                                          *
 * File    : spawnlp.c                                                      *
 *                                                                          *
 * Purpose : _spawnlp function -- win32 version.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <process.h>

/* spawn a child process - search along PATH */
int __cdecl (_spawnlp)(int mode, const char *cmd, const char *arg0, ...)
{
    return _spawnvp(mode, cmd, (char * const *)&arg0);
}

