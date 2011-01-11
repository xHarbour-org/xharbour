/****************************************************************************
 *                                                                          *
 * File    : peephole.c                                                     *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic Assembler; Peephole optimizer.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-12-03  Bugfix: missed some 64-bit translations.             *
 *           04-06-10  Bugfix: didn't handle unsigned long long -> float.   *
 *           04-06-21  Bugfix: remove spurious "mov a64,a64".               *
 *           04-07-09  Support for long long integers moved to backend.     *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

/* Static function prototypes */
static bool_t match(char *, const char *, char **);
static char *subst(const char *, char **);

/****************************************************************************
 *                                                                          *
 * Function: peephole_optimizer                                             *
 *                                                                          *
 * Purpose : Simple peephole optimizer.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-26  Skip over #line directives.                          *
 *           01-04-06  Skip over #line directives at beginning of loop.     *
 *                                                                          *
 ****************************************************************************/

void peephole_optimizer(const char *rules[])
{
    LIST *lp = aslist;
    do
    {
        int i = 0;

        /* skip #line, or we will loose some of them below */
        if (*(char *)lp->link->data == '#') continue;

        while (rules[i] != NULL)
        {
            char *vars[10] = {0};
            LIST *xp = lp;
            int n = 0;

            while (rules[i++] != NULL)
            {
                do
                {
                    xp = xp->link; n++;
                } while (*(char *)xp->data == '#');  /* skip #line */

                if (!match(xp->data, rules[i-1], vars))
                {
                    while (rules[i++] != NULL)
                        ;
                    while (rules[i++] != NULL)
                        ;
                    goto next_rule;
                }
            }

            /* delete old instructions */
            do
                aslist = listdelete(lp, aslist);
            while (--n);

            /* insert new instructions */
            for (xp = lp; rules[i++] != NULL; xp = xp->link)
                aslist = listinsert(subst(rules[i-1], vars), xp, aslist);

            /* restart */
            i = 0;
next_rule: ;
        }
    } while ((lp = lp->link) != aslist);
}

/****************************************************************************
 *                                                                          *
 * Function: match                                                          *
 *                                                                          *
 * Purpose : match ins against pat and set vars.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-12-18  Support for pound (#) variables added.               *
 *                                                                          *
 ****************************************************************************/

#define isnumber(p)  (((p)[0] == '-' && isdigit((p)[1])) || isdigit((p)[0]))
static bool_t match(char *ins, const char *pat, char **vars)
{
    while (*ins && *pat)
    {
        if ((pat[0] == '%' || pat[0] == '$' || pat[0] == '#') && isdigit(pat[1]))
        {
            char buf[MAXLINE], *s;

            for (s = buf; *ins && *ins != pat[2]; )
                *s++ = *ins++;
            *s = 0;

            /* percent variables *must* contain a register name */
            if (pat[0] == '%' && as.scanner.asmreg(buf) == -1)
                return FALSE;

            /* pound variables *must* contain a number */
            if (pat[0] == '#' && !isnumber(buf))
                return FALSE;

            s = string(buf);
            if (vars[pat[1]-'0'] == NULL)
                vars[pat[1]-'0'] = s;
            else if (vars[pat[1]-'0'] != s)
                return FALSE;

            pat += 2;
        }
        else if (*pat++ != *ins++)
            return FALSE;
    }

    return *pat == *ins;
}
#undef isnumber

/****************************************************************************
 *                                                                          *
 * Function: subst                                                          *
 *                                                                          *
 * Purpose : return result of substituting vars into pat.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-12-18  Support for pound (#) variables added.               *
 *                                                                          *
 ****************************************************************************/

static char *subst(const char *pat, char **vars)
{
    char buf[MAXLINE], *s = buf;

    for (;;)
    {
        if ((pat[0] == '%' || pat[0] == '$' || pat[0] == '#') && isdigit(pat[1]))
        {
            char *q;

            for (q = vars[pat[1]-'0']; s < &buf[MAXLINE] && (*s = *q++) != '\0'; s++)
                ;

            pat += 2;
        }
        else if (s > &buf[MAXLINE])
            assert(0);
        else if ((*s++ = *pat++) == '\0')
            return string(buf);
    }
}

