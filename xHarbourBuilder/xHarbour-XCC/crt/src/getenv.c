/****************************************************************************
 *                                                                          *
 * File    : getenv.c                                                       *
 *                                                                          *
 * Purpose : getenv function (in-memory version).                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "xcrt.h"

/* search environment list for named entry */
char * __cdecl (getenv)(const char *name)
{
    const char *s;
    size_t n = strlen(name);

    /* look for name match */
    for (s = __envp; *s != '\0'; s += strlen(s) + 1)
    {
#ifdef STANDARD_C
        if (strncmp(s, name, n) == 0 && s[n] == '=')
            return (char *)&s[n+1];
#else
        if (_strnicmp(s, name, n) == 0 && s[n] == '=')
            return (char *)&s[n+1];
#endif
    }
    return 0;
}

