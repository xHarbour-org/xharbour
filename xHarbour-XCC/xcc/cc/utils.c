/****************************************************************************
 *                                                                          *
 * File    : utils.c                                                        *
 *                                                                          *
 * Purpose : ISO C Compiler; Support functions.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-13  Removed newstring(), added my_strncat().             *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"
#include <resource.h>

#ifdef PRERELEASE
#ifndef __POCC__
#include <malloc.h>
#endif
extern size_t tot_size;
extern size_t cur_size;
#endif

/* Locals */
static uchar_t warnbits[(CLANG_ERRNUM_MAX - CLANG_ERRNUM_MIN) / CHAR_BIT];

/****************************************************************************
 *                                                                          *
 * Function: my_alloc                                                       *
 *                                                                          *
 * Purpose : Allocate a memory block.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) && defined(__POCC__)
void *my_trace_alloc(size_t size, const char *filename, long line)
#else
void *my_alloc(size_t size)
#endif
{
    void *p = malloc(size);

    if (p == NULL)
        apperror(RCFATAL(ERROR_OUT_OF_MEMORY));

#ifdef PRERELEASE
    cur_size += _msize(p);
    tot_size = max(tot_size, cur_size);
#endif

#if defined(PODEBUG) && defined(__POCC__)
    RECORD_FILE_AND_LINE(p, filename, line);
#endif

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: my_realloc                                                     *
 *                                                                          *
 * Purpose : Re-allocate a memory block.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) && defined(__POCC__)
void *my_trace_realloc(void *p, size_t newsize, const char *filename, long line)
#else
void *my_realloc(void *p, size_t newsize)
#endif
{
#ifdef PRERELEASE
    if (p) cur_size -= _msize(p);
#endif

    p = realloc(p, newsize);

    if (p == NULL)
        apperror(RCFATAL(ERROR_OUT_OF_MEMORY));

#ifdef PRERELEASE
    cur_size += _msize(p);
    tot_size = max(tot_size, cur_size);
#endif

#if defined(PODEBUG) && defined(__POCC__)
    RECORD_FILE_AND_LINE(p, filename, line);
#endif

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: my_free                                                        *
 *                                                                          *
 * Purpose : Free a memory block.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void my_free(void *p)
{
#ifdef PRERELEASE
    if (p) cur_size -= _msize(p);
#endif
#if defined(PODEBUG) && defined(__POCC__)
    if (p)
    {
        DELETE_RECORD(p);
    }
#endif
    if (p) free(p);
}

/****************************************************************************
 *                                                                          *
 * Function: my_bsearch                                                     *
 *                                                                          *
 * Purpose : Binary search routine.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int my_bsearch(const char *string, const char **array, int size,
    int (__cdecl *compare)(const char *s1, const char *s2))
{
    int lo = 0;
    int hi = size-1;

    while (hi >= lo)
    {
        int this = (lo + hi) / 2;
        int cond = compare(string, array[this]);

        if (cond < 0)
            hi = this-1;
        else if (cond > 0)
            lo = this+1;
        else
            return this;
    }

    return -1;
}

/****************************************************************************
 *                                                                          *
 * Function: my_strdup                                                      *
 *                                                                          *
 * Purpose : Duplicate the contents of a string, by using my_alloc.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

char *my_strdup(const char *s)
{
    return strcpy(my_alloc(strlen(s)+1), s);
}

/****************************************************************************
 *                                                                          *
 * Function: my_strndup                                                     *
 *                                                                          *
 * Purpose : Special strdup using my_alloc() and maximum length.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

char *my_strndup(const char *s, size_t len)
{
    char *p = my_alloc(len+1);
    p[len] = '\0';
    return strncpy(p, s, len);
}

/****************************************************************************
 *                                                                          *
 * Function: my_strncat                                                     *
 *                                                                          *
 * Purpose : Special strcat using my_alloc() and maximum length.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-03-13  Created                                              *
 *                                                                          *
 ****************************************************************************/

char *my_strncat(const char *s1, size_t len1, const char *s2, size_t len2)
{
    char *p = my_alloc(len1 + len2 + 1);
    p[len1 + len2] = '\0';
    strncpy(p, s1, len1);
    strncpy(p + len1, s2, len2);
    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: outnum                                                         *
 *                                                                          *
 * Purpose : Convert a number to a string.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

char *outnum(char *p, int n)
{
    if (n >= 10)
        p = outnum(p, n/10);
    *p++ = n%10 + '0';

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: bitcount                                                       *
 *                                                                          *
 * Purpose : Count number of bits in the bitmask.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

int bitcount(uint_t mask)
{
    uint_t i, n = 0;

    for (i = 1; i != 0; i <<= 1)
        if (mask & i) n++;

    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: update_extension_in_file                                       *
 *                                                                          *
 * Purpose : Update extension in a filename.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-18  Rewritten. Now supports forward slashes too.         *
 *                                                                          *
 ****************************************************************************/

void update_extension_in_file(char *filename, const char *ext)
{
    char *s;

    for (s = filename + strlen(filename) - 1;
         s >= filename && *s != '\\' && *s != '/' && *s != '.';
         s--)
        ;

    if (s >= filename && *s == '.')
        strcpy(s, ext);
    else
        strcat(filename, ext);
}

/****************************************************************************
 *                                                                          *
 * Function: my_fullpath                                                    *
 *                                                                          *
 * Purpose : Create a full path.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-18  Rewritten. Now supports forward slashes too.         *
 *           04-04-11  Bugfix: failed when relpath ended with "." or ".."   *
 *                                                                          *
 ****************************************************************************/

size_t my_fullpath(char *buf, const char *relpath, const char *curpath)
{
    if (relpath[0] == '\\' || relpath[0] == '/' || (relpath[0] && relpath[1] == ':') ||
        curpath == NULL || *curpath == '\0')
    {
        return strlen(strcpy(buf, relpath));
    }
    else
    {
        const char *p;
        char *s;

        strcpy(buf, curpath);
        s = buf + strlen(curpath);

        p = relpath;
        while (*p)
        {
            if (*p == '.')
            {
                if (*++p == '.')
                {
                    while (--s > buf && *s != '\\' && *s != '/')
                        ;

                    p++;
                }

                if (*p && *p++ != '\\' && p[-1] != '/')
                    return 0;
            }
            else
            {
                if (s[-1] != '\\' && s[-1] != '/')
                    *s++ = '\\';

                while (*p && *p != '\\' && *p != '/')
                    *s++ = *p++;

                if (*p) p++;
            }
        }

        *s = '\0';
        return strlen(buf);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: basename                                                       *
 *                                                                          *
 * Purpose : Return filename part of a pathname.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-18  Rewritten. Now supports forward slashes too.         *
 *                                                                          *
 ****************************************************************************/

const char *basename(const char *filename)
{
    if (filename != NULL)
    {
        const char *s;

        for (s = filename + strlen(filename); 
             s > filename && s[-1] != '\\' && s[-1] != '/'; 
             s--)
            ;
        return s;
    }

    return "";
}

/****************************************************************************
 *                                                                          *
 * Function: my_getversion                                                  *
 *                                                                          *
 * Purpose : Read file version from resources.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-12-10  Created                                              *
 *                                                                          *
 ****************************************************************************/

void my_getversion(short *major, short *minor)
{
    HRSRC hrsrc;
    HGLOBAL hglob;
    void *vp;

    *major = *minor = 0;

    if ((hrsrc = FindResource(NULL, MAKEINTRESOURCE(VS_VERSION_INFO), RT_VERSION)) != NULL &&
        (hglob = LoadResource(NULL, hrsrc)) != NULL &&
        (vp = LockResource(hglob)) != NULL)
    {
        vp = (char *)vp + sizeof(RSRC_VERHDR) + sizeof(L"VS_VERSION_INFO") + 2;
        if (((RSRC_VERFIX *)vp)->signature == RSRC_VF_SIGNATURE)
        {
            *major = ((RSRC_VERFIX *)vp)->file_hi_majver;
            *minor = ((RSRC_VERFIX *)vp)->file_hi_minver;
        }

        /* FreeResource(hgRes);  OBSOLETE! */
    }
}

/****************************************************************************
 *                                                                          *
 * Function: disabled_warning                                               *
 *                                                                          *
 * Purpose : Check if the specified warning is disabled.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-10  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t disabled_warning(WINERR err)
{
    uint_t n = ERRNUM(err) - CLANG_ERRNUM_MIN;

    if (n < sizeof(warnbits) * CHAR_BIT)
        return warnbits[n / CHAR_BIT] & (1 << (n % CHAR_BIT));

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: enable_or_disable_warning                                      *
 *                                                                          *
 * Purpose : Enable or disable the specified warning.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-10  Created                                              *
 *                                                                          *
 ****************************************************************************/

void enable_or_disable_warning(bool_t enable, WINERR err)
{
    uint_t n = ERRNUM(err) - CLANG_ERRNUM_MIN;

    if (n < sizeof(warnbits) * CHAR_BIT)
    {
        register uint_t mask = (1 << (n % CHAR_BIT));
        if (enable) warnbits[n / CHAR_BIT] &= ~mask; else warnbits[n / CHAR_BIT] |= mask;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: copy_warnings                                                  *
 *                                                                          *
 * Purpose : Make a copy of the current warning state.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-10  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *copy_warnings(void)
{
    return memcpy(my_alloc(sizeof(warnbits)), warnbits, sizeof(warnbits));
}

/****************************************************************************
 *                                                                          *
 * Function: restore_warnings                                               *
 *                                                                          *
 * Purpose : Restore the warning state from a previously saved copy.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-10  Created                                              *
 *                                                                          *
 ****************************************************************************/

void restore_warnings(void *p)
{
    memcpy(warnbits, p, sizeof(warnbits));
    free(p);
}
