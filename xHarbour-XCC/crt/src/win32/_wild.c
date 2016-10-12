/****************************************************************************
 *                                                                          *
 * File    : _wild.c                                                        *
 *                                                                          *
 * Purpose : _Cwild function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-02-08  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "xcrt.h"


/***
*wild.c - wildcard expander
*
*       Copyright (c) 1985-1997, Microsoft Corporation. All rights reserved.
*
*Purpose:
*        expands wildcards in argv
*
*        handles '*' (none or more of any char) and '?' (exactly one char)
*
*******************************************************************************/

#include <cruntime.h>
#include <oscalls.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <msdos.h>
#include <internal.h>
#include <tchar.h>

#ifdef _MBCS
#include <mbdata.h>
#include <mbstring.h>
#endif  /* _MBCS */
#include <dbgint.h>

/*
** these are the data structures
**
**     __argv
**     -------     ------
**     |     |---->|    |---->"arg0"
**     -------     ------
**                 |    |---->"arg1"
**                 ------
**                  ....
**                 ------
**                 |    |---->"argn"
**                 ------
**                 |NULL|
**                 ------
**                                       argend
**                                       -------
**     -------                           |     |
**     |     | __argc                    -------
**     -------                              |
**                                          |
**  arghead                                 V
**  ------     ---------                ----------
**  |    |---->|   |   |----> .... ---->|   |NULL|
**  ------     ---------                ----------
**               |                        |
**               V                        V
**            "narg0"                  "nargn"
*/

#define SLASH           _T("\\")
#define FWDSLASH        _T("/")
#define STAR            _T("*.*")
#define DOT             _T(".")
#define DOTDOT          _T("..")

#define WILDSTRING      _T("*?")


struct argnode {
    signed char *argptr;
    struct argnode *nextnode;
};

static struct argnode *arghead;
static struct argnode *argend;


static int __cdecl match(char *, char *);
static int __cdecl add(char *);
static void __cdecl sort(struct argnode *);
static char * __cdecl find (char *pattern);

/* expands wildcard in file specs in argv */
int __cdecl _cwild(void)
{
    char **argv = __argv;
    struct argnode *nodeptr;
    int argc;
    signed char **tmp;
    signed char *wchar;

    arghead = argend = NULL;

    for (argv = __argv; *argv; argv++)  /* for each arg... */
    {
        if ( *(*argv)++ == '"')
        {
            /* strip leading quote from quoted arg */
            if (add(*argv)) return -1;
        }
        else if (wchar = _tcspbrk(*argv, WILDSTRING))
        {
            /* attempt to expand arg with wildcard */
            if (match(*argv, wchar))
                return -1;
        }
        else if (add(*argv))  /* normal arg, just add */
            return -1;
    }

    /* count the args */
    for (argc = 0, nodeptr = arghead; nodeptr != 0; nodeptr = nodeptr->nextnode, argc++)
        ;

    /* try to get new arg vector */
    if (!(tmp = (signed char **)_malloc_crt(sizeof(signed char *)*(argc+1))))
        return -1;

    /* the new arg vector... */
    __argv = tmp;

    /* the new arg count... */
    __argc = argc;

    /* install the new args */
    for (nodeptr = arghead; nodeptr != 0; nodeptr = nodeptr->nextnode)
        *tmp++ = nodeptr->argptr;

    /* the terminal NULL */
    *tmp = NULL;

    /* free up local data */
    for (nodeptr = arghead; nodeptr != 0; nodeptr = arghead)
    {
        arghead = arghead->nextnode;
        _free_crt(nodeptr);
    }

    /* return success */
    return 0;
}


/* ??? */
static int __cdecl match(signed char *arg, signed char *ptr)
{
    signed char *new;
    int length = 0;
    signed char *all;
    struct argnode *first;
    int gotone = 0;

    while (ptr != arg && *ptr != '\\' && *ptr != '/' && *ptr != ':')
    {
        /* find first slash or ':' before wildcard */
#ifdef _MBCS
        if (--ptr > arg)
            ptr = _mbsdec(arg,ptr+1);
#else  /* _MBCS */
        ptr--;
#endif  /* _MBCS */
    }

    if (*ptr == ':' && ptr != arg+1)  /* weird name, just add it as is */
        return add(arg);

    if (*ptr == '\\' || *ptr == '/' || *ptr == ':')  /* pathname */
        length = ptr - arg + 1;  /* length of dir prefix */

    if (new = find(arg))
    {
        /* get the first file name */
        first = argend;

        do
        {
            /* got a file name */
            if (_tcscmp(new, DOT) && _tcscmp(new, DOTDOT))
            {
                if (*ptr != '\\' && *ptr != ':' && *ptr != '/')
                {
                    /* current directory; don't need path */
                    if (!(arg = _tcsdup(new)) || add(arg))
                        return -1;
                }
                else
                {
                    /* add full pathname */
                    if (!(all = _malloc_crt((length+_tcslen(new)+1)*sizeof(signed char))) || add(_tcscpy(_tcsncpy(all,arg,length)+length,new) - length))
                        return -1;
                }

                gotone++;
            }
        } while (new = find(NULL));  /* get following files */

        if (gotone)
        {
            sort(first ? first->nextnode : arghead);
            return 0;
        }
    }

    return add(arg);  /* no match */
}


/* ??? */
static int __cdecl add(signed char *arg)
{
    struct argnode *nodeptr;

    if (!(nodeptr = (struct argnode *)_malloc_crt(sizeof(struct argnode))))
        return -1;

    nodeptr->argptr = arg;
    nodeptr->nextnode = NULL;

    if (arghead)
        argend->nextnode = nodeptr;
    else
        arghead = nodeptr;

    argend = nodeptr;
    return 0;
}


/* ??? */
static void __cdecl sort(struct argnode *first)
{
    struct argnode *nodeptr;
    signed char *temp;

    if (first)  /* something to sort */
    {
        while (nodeptr = first->nextnode)
        {
            do
            {
                if (_tcsicmp(nodeptr->argptr, first->argptr) < 0)
                {
                    temp = first->argptr;
                    first->argptr = nodeptr->argptr;
                    nodeptr->argptr = temp;
                }
            } while (nodeptr = nodeptr->nextnode);

            first = first->nextnode;
        }
    }
}


/* find matching filename */
static char * __cdecl find(signed char *pattern)
{
    signed char *retval;

    static HANDLE _WildFindHandle;
    static LPWIN32_FIND_DATA findbuf;

    if (pattern)
    {
        if (findbuf == 0)
            findbuf = (LPWIN32_FIND_DATA)_malloc_crt(MAX_PATH + sizeof(*findbuf));

        if (_WildFindHandle != 0)
        {
            (void)FindClose(_WildFindHandle);
            _WildFindHandle = 0;
        }

        _WildFindHandle = FindFirstFile((LPTSTR)pattern, findbuf);
        if (_WildFindHandle == (HANDLE)0xffffffff)
            return 0;
    }
    else if (!FindNextFile(_WildFindHandle, findbuf))
    {
        (void)FindClose(_WildFindHandle);
        _WildFindHandle = NULL;
        return 0;
    }

    retval = findbuf->cFileName;

    return retval;
}

