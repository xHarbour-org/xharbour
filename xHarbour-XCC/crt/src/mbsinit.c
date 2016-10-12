/****************************************************************************
 *                                                                          *
 * File    : mbsinit.c                                                      *
 *                                                                          *
 * Purpose : mbsinit function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* test for initial state */
int __cdecl (mbsinit)(const mbstate_t *pst)
{
    return pst == 0 || pst->state == 0;
}

