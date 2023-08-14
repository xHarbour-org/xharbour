/****************************************************************************
 *                                                                          *
 * File    : nlist.c                                                        *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; preprocessor; symbol table.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "rc.h"

static NLIST *nlist[512];

struct kwtab
{
    char * const kw;
    int val;
    int flag;
}
static kwtab[] =
{
    "if",           KIF,            ISKW,
    "ifdef",        KIFDEF,         ISKW,
    "ifndef",       KIFNDEF,        ISKW,
    "elif",         KELIF,          ISKW,
    "else",         KELSE,          ISKW,
    "endif",        KENDIF,         ISKW,
    "include",      KINCLUDE,       ISKW,
    "define",       KDEFINE,        ISKW,
    "undef",        KUNDEF,         ISKW,
    "line",         KLINE,          ISKW,
    "error",        KERROR,         ISKW,
    "pragma",       KPRAGMA,        ISKW,
    "eval",         KEVAL,          ISKW,
    "defined",      KDEFINED,       ISDEFINED+ISUNCHANGE,
    "__LINE__",     KLINENO,        ISMAC+ISUNCHANGE,
    "__FILE__",     KFILE,          ISMAC+ISUNCHANGE,
    "__DATE__",     KDATE,          ISMAC+ISUNCHANGE,
    "__TIME__",     KTIME,          ISMAC+ISUNCHANGE,
    "__STDC__",     KSTDC,          ISUNCHANGE,
    NULL
};

/* Globals */
ulong_t namebit[63+1];
NLIST *kwdefined;

/****************************************************************************
 *                                                                          *
 * Function: setup_kwtab                                                    *
 *                                                                          *
 * Purpose : Add reserved words to the symbol table.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void setup_kwtab(void)
{
    static TOKEN deftoken[1] = {{ NAME, 0, 0, 0, 7, (uchar_t *)"defined" }};
    static TOKENROW deftr = { deftoken, deftoken, deftoken+1, 1 };
    struct kwtab *kp;

    for (kp = kwtab; kp->kw != NULL; kp++)
    {
        NLIST *np;
        TOKEN t;

        t.t = (uchar_t *)kp->kw;
        t.len = strlen(kp->kw);

        np = pp_lookup(&t, TRUE);
        np->flag = kp->flag;
        np->val = kp->val;

        if (np->val == KDEFINED)
        {
            kwdefined = np;
            np->val = NAME;
            np->vp = &deftr;
            np->ap = 0;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pp_lookup                                                      *
 *                                                                          *
 * Purpose : Lookup a symbol in the symbol table.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

NLIST *pp_lookup(TOKEN *tp, bool_t install)
{
    uint_t h;
    NLIST *np;
    uchar_t *cp;
    uchar_t *cpe;

    h = 0;
    for (cp = tp->t, cpe = cp + tp->len; cp < cpe; )
        h += *cp++;
    h %= NELEMS(nlist);

    for (np = nlist[h]; np != NULL; np = np->next)
    {
        if (*tp->t == *np->name && (signed)tp->len == np->len &&
            strncmp((char *)tp->t, (char *)np->name, tp->len) == 0)
            return np;
    }

    if (install)
    {
        nident++;

        np = new(NLIST);
        np->vp = NULL;
        np->ap = NULL;
        np->flag = 0;
        np->val = 0;
        np->len = tp->len;
        np->name = newstring(tp->t, tp->len, 0);
        np->next = nlist[h];
        nlist[h] = np;
        quickset(tp->t[0], (tp->len > 1) ? tp->t[1] : 0);
    }

    return np;
}

