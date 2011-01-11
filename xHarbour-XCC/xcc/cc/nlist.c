/****************************************************************************
 *                                                                          *
 * File    : nlist.c                                                        *
 *                                                                          *
 * Purpose : ISO C Compiler; Preprocessor; Symbol table management.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#define PREPROCESSOR
#include "lcc.h"

static NLIST *nlist[512];

struct kwtab {
    char * const name;
    int val;
    int flag;
}
static kwtab[] = {
    "if",                   KIF,            ISKW,
    "ifdef",                KIFDEF,         ISKW,
    "ifndef",               KIFNDEF,        ISKW,
    "elif",                 KELIF,          ISKW,
    "else",                 KELSE,          ISKW,
    "endif",                KENDIF,         ISKW,
    "include",              KINCLUDE,       ISKW,
    "define",               KDEFINE,        ISKW,
    "undef",                KUNDEF,         ISKW,
    "line",                 KLINE,          ISKW,
    "error",                KERROR,         ISKW,
    "pragma",               KPRAGMA,        ISKW,
    "eval",                 KEVAL,          ISKW,
    "defined",              KDEFINED,       ISDEFINED+ISUNCHANGE,
    "__LINE__",             KLINENO,        ISMAC+ISUNCHANGE,
    "__FILE__",             KFILE,          ISMAC+ISUNCHANGE,
    "__DATE__",             KDATE,          ISMAC+ISUNCHANGE,
    "__TIME__",             KTIME,          ISMAC+ISUNCHANGE,
    NULL
},
stdc_kwtab[] = {
    "__STDC__",             KSTDC,          ISMAC+ISUNCHANGE,
    "__STDC_HOSTED__",      KSTDCHOSTED,    ISMAC+ISUNCHANGE,
    "__STDC_IEC_559__",     KSTDCIEC559,    ISMAC+ISUNCHANGE,
    "__STDC_ISO_10646__",   KSTDCISO10646,  ISMAC+ISUNCHANGE,
    "__STDC_VERSION__",     KSTDCVERSION,   ISMAC+ISUNCHANGE,
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
 *           00-06-06  Created                                              *
 *           04-07-12  Added argument stdc (to optionally use stdc_kwtab).  *
 *                                                                          *
 ****************************************************************************/

void setup_kwtab(bool_t stdc)
{
    static TOKEN deftoken[1] = {{ NAME, 0, 0, 0, 7, (uchar_t *)"defined" }};
    static TOKENROW deftr = { deftoken, deftoken, deftoken+1, 1 };
    struct kwtab *kp;

    for (kp = stdc ? stdc_kwtab : kwtab; kp->name != NULL; kp++)
    {
        NLIST *np;
        TOKEN t;

        t.t = (uchar_t *)kp->name;
        t.len = strlen(kp->name);

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
 * Comment : This routine is heavily utilized and will benefit from         *
 *           almost any type of optimization.                               *
 *                                                                          *
 *           2000-12-03, 16.23.46:                                          *
 *             331 ms,  149925 ggr: _pp_lookup                              *
 *             270 ms,    1902 ggr: _fillbuf                                *
 *             200 ms,  138635 ggr: _my_alloc                               *
 *             200 ms,  129506 ggr: _get_tokens                             *
 *             181 ms,  110999 ggr: _lookup_symbol                          *
 *             180 ms,   98253 ggr: _stringn                                *
 *             ...                                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-13  Changed newstring() to my_strndup().                 *
 *                                                                          *
 ****************************************************************************/

NLIST *pp_lookup(TOKEN *tp, bool_t install)
{
    uint_t h;
    NLIST *np;
    uchar_t *cp;
    uchar_t *cpe;

    h = 0;
    for (cp = tp->t, cpe = cp + tp->len; cp < cpe; cp++)
        h += *cp;
    h %= NELEMS(nlist);

    for (np = nlist[h]; np != NULL; np = np->next)
    {
        if ((signed)tp->len == np->len && *tp->t == *np->name &&
            strncmp((char *)tp->t, (char *)np->name, tp->len) == 0)
            return np;
    }

    if (install)
    {
        np = my_alloc(sizeof(*np));
        np->vp = NULL;
        np->ap = NULL;
        np->flag = 0;
        np->val = 0;
        np->len = tp->len;
        np->name = (uchar_t *)my_strndup((char *)tp->t, tp->len);
        np->next = nlist[h];
        nlist[h] = np;
        quickset(tp->t[0], (tp->len > 1) ? tp->t[1] : 0);
    }

    return np;
}

