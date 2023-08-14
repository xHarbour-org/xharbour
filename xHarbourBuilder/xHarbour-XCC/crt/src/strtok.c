/****************************************************************************
 *                                                                          *
 * File    : strtok.c                                                       *
 *                                                                          *
 * Purpose : strtok function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-12  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xthread.h"

/* find next token in s1[] delimited by s2[] */
char * __cdecl (strtok)(char * restrict s1, const char * restrict s2)
{
#ifdef __MT__
    tiddata *mtd = __get_mtd();
    char **pssave = &mtd->token;
#else /* __MT__ */
    static char *ssave = "";
    char **pssave = &ssave;
#endif /* __MT__ */
    char *sbegin, *send;

    sbegin = (s1) ? s1 : *pssave;
    sbegin += strspn(sbegin, s2);
    if (*sbegin == '\0')
    {
        /* end of scan */
        *pssave = "";   /* for safety */
        return 0;
    }
    send = sbegin + strcspn(sbegin, s2);
    if (*send != '\0')
        *send++ = '\0';
    *pssave = send;

    return sbegin;
}

