/****************************************************************************
 *                                                                          *
 * File    : _getmem.c                                                      *
 *                                                                          *
 * Purpose : __getmem function -- win32 version.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "xalloc.h"
#include "xcrt.h"

void *__crtheap;

/* allocate raw storage */
void *__getmem(size_t size)
{
    MEMORY_BASIC_INFORMATION mbi;
    void *base;

    if (!VirtualQuery(__crtheap, &mbi, sizeof(mbi)))
        return 0;

    /* calculate the base address of the new block */
    base = (mbi.State == MEM_COMMIT) ? (char *)__crtheap + mbi.RegionSize : __crtheap;

    if (!VirtualAlloc(base, size, MEM_COMMIT, PAGE_READWRITE))
        return 0;

    return base;
}

