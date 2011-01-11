/****************************************************************************
 *                                                                          *
 * File    : tokens.c                                                       *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; preprocessor; token management.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "rc.h"

static char wbuf[4*OBS];
static char *wbp = wbuf;

/*
 * 1 for tokens that don't need whitespace when they
 * get inserted by macro expansion.
 */
static const char wstab[] =
{
    0,      /* END */
    0,      /* UNCLASS */
    0,      /* NAME */
    0,      /* NUMBER */
    0,      /* STRING */
    0,      /* CCON */
    1,      /* NL */
    0,      /* WS */
    0,      /* DSHARP */
    0,      /* EQ */
    0,      /* NEQ */
    0,      /* LEQ */
    0,      /* GEQ */
    0,      /* LSH */
    0,      /* RSH */
    0,      /* LAND */
    0,      /* LOR */
    0,      /* PPLUS */
    0,      /* MMINUS */
    0,      /* ARROW */
    1,      /* SBRA */
    1,      /* SKET */
    1,      /* LP */
    1,      /* RP */
    0,      /* DOT */
    0,      /* AND */
    0,      /* STAR */
    0,      /* PLUS */
    0,      /* MINUS */
    0,      /* TILDE */
    0,      /* NOT */
    0,      /* SLASH */
    0,      /* PCT */
    0,      /* LT */
    0,      /* GT */
    0,      /* CIRC */
    0,      /* OR */
    0,      /* QUEST */
    0,      /* COLON */
    0,      /* ASGN */
    1,      /* COMMA */
    0,      /* SHARP */
    1,      /* SEMIC */
    1,      /* CBRA */
    1,      /* CKET */
    0,      /* ASPLUS */
    0,      /* ASMINUS */
    0,      /* ASSTAR */
    0,      /* ASSLASH */
    0,      /* ASPCT */
    0,      /* ASCIRC */
    0,      /* ASLSH */
    0,      /* ASRSH */
    0,      /* ASOR */
    0,      /* ASAND */
    0,      /* ELLIPS */
    0,      /* DSHARP1 */
    0,      /* NAME1 */
    0,      /* DEFINED */
    0,      /* UMINUS */
};

/* Static function prototypes */
static void addspace(TOKEN *, uint_t);

/****************************************************************************
 *                                                                          *
 * Function: make_tokenrow                                                  *
 *                                                                          *
 * Purpose : Allocate a new TOKENROW.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void make_tokenrow(int size, TOKENROW *trp)
{
    trp->max = size;

    if (size > 0)
        trp->bp = (TOKEN *)my_alloc(size * sizeof(TOKEN));
    else
        trp->bp = NULL;

    trp->tp = trp->bp;
    trp->lp = trp->bp;
}

/****************************************************************************
 *                                                                          *
 * Function: grow_tokenrow                                                  *
 *                                                                          *
 * Purpose : Increase the allocation of a TOKENROW.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

TOKEN *grow_tokenrow(TOKENROW *trp)
{
    int ncur = trp->tp - trp->bp;
    int nlast = trp->lp - trp->bp;

    trp->max = 3*trp->max/2 + 1;
    trp->bp = (TOKEN *)my_realloc(trp->bp, trp->max * sizeof(TOKEN));
    trp->lp = &trp->bp[nlast];
    trp->tp = &trp->bp[ncur];

    return trp->lp;
}

/****************************************************************************
 *                                                                          *
 * Function: compare_tokens                                                 *
 *                                                                          *
 * Purpose : Compare a row of tokens, ignoring the content of WS;           *
 *           return != 0 if different.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

int compare_tokens(TOKENROW *tr1, TOKENROW *tr2)
{
    TOKEN *tp1 = tr1->tp;
    TOKEN *tp2 = tr2->tp;

    if (tr1->lp-tp1 != tr2->lp-tp2)
        return 1;

    for (; tp1 < tr1->lp; tp1++, tp2++)
    {
        if (tp1->type != tp2->type ||
            (tp1->wslen == 0) != (tp2->wslen == 0) ||
            tp1->len != tp2->len ||
            strncmp((char *)tp1->t, (char *)tp2->t, tp1->len) != 0)
            return 1;
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: insertrow                                                      *
 *                                                                          *
 * Purpose : Replace ntok tokens starting at dtr->tp with the contents      *
 *           of str. tp ends up pointing just beyond the replacement.       *
 *           Canonical whitespace is assured on each side.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if 1
void insertrow(TOKENROW *dtr, int ntok, TOKENROW *str)
{
    size_t nrtok = rowlen(str);

    dtr->tp += ntok;
    adjustrow(dtr, nrtok-ntok);
    dtr->tp -= ntok;
    move_tokenrow(dtr, str);
    makespace(dtr);
    dtr->tp += nrtok;
    makespace(dtr);
}
#else
void insertrow(TOKENROW *dtr, int ntok, TOKENROW *str)
{
    uint_t wslen = dtr->tp->wslen;
    int nrtok = rowlen(str);

    dtr->tp += ntok;
    adjustrow(dtr, nrtok - ntok);
    dtr->tp -= ntok;
    move_tokenrow(dtr, str);
    makespace(dtr);
    addspace(dtr->tp, wslen);  /* restore WS value */
    dtr->tp += nrtok;
    makespace(dtr);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: makespace                                                      *
 *                                                                          *
 * Purpose : Make sure there is WS before trp->tp, if tokens might          *
 *           merge in the output.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if 1
void makespace(TOKENROW *trp)
{
    TOKEN *tp = trp->tp;
    uchar_t *tt;

    if (tp >= trp->lp)
        return;

    if (tp->wslen)
    {
        if ((tp->flag & XPWS) &&
            (wstab[tp->type] || trp->tp > trp->bp && wstab[(tp-1)->type]))
        {
            tp->wslen = 0;
            return;
        }

        tp->t[-1] = ' ';
        return;
    }

    if (wstab[tp->type] || trp->tp > trp->bp && wstab[(tp-1)->type])
        return;

    tt = newstring(tp->t, tp->len, 1);
    *tt++ = ' ';
    tp->t = tt;
    tp->wslen = 1;
    tp->flag |= XPWS;
}
#else
void makespace(TOKENROW *trp)
{
    TOKEN *tp = trp->tp;

    if (tp >= trp->lp)
        return;

    if (tp->wslen)
    {
        if ((tp->flag & XPWS) && (wstab[tp->type] || trp->tp > trp->bp && wstab[(tp-1)->type]))
        {
            tp->wslen = 0;
            return;
        }

        tp->t[-1] = ' ';
        return;
    }

    if (wstab[tp->type] || trp->tp > trp->bp && wstab[(tp-1)->type])
        return;

    addspace(tp, 1);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: addspace                                                       *
 *                                                                          *
 * Purpose : Add wslen spaces before token tp.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-10-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if 0
static void addspace(TOKEN *tp, uint_t wslen)
{
    if (wslen > tp->wslen)
    {
        uchar_t *tt;

        tt = newstring(tp->t, tp->len, wslen);
        tp->wslen = wslen;
        while (wslen--) *tt++ = ' ';
        tp->t = tt;
        tp->flag |= XPWS;
    }
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: move_tokenrow                                                  *
 *                                                                          *
 * Purpose : Copy an entire tokenrow into another, at tp.                   *
 *           It is assumed that there is enough space.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void move_tokenrow(TOKENROW *dtr, TOKENROW *str)
{
    size_t nby;

    /* nby = sizeof(TOKEN) * (str->lp - str->bp); */
    nby = (char *)str->lp - (char *)str->bp;
    memmove(dtr->tp, str->bp, nby);
}

/****************************************************************************
 *                                                                          *
 * Function: adjustrow                                                      *
 *                                                                          *
 * Purpose : Move the tokens in a row, starting at tr->tp, rightward by     *
 *           nt tokens; nt may be negative (left move).                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void adjustrow(TOKENROW *trp, int nt)
{
    int size;
    size_t nby;

    if (nt == 0)
        return;

    size = (trp->lp - trp->bp) + nt;

    while (size > trp->max)
        grow_tokenrow(trp);

    /* nby = sizeof(TOKEN) * (trp->lp - trp->tp); */
    nby = (char *)trp->lp - (char *)trp->tp;
    if (nby) memmove(trp->tp+nt, trp->tp, nby);

    trp->lp += nt;
}

/****************************************************************************
 *                                                                          *
 * Function: copy_tokenrow                                                  *
 *                                                                          *
 * Purpose : Copy a row of tokens into the destination holder, allocating   *
 *           the space for the contents. Return the destination.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

TOKENROW *copy_tokenrow(TOKENROW *dtr, TOKENROW *str)
{
    int len = rowlen(str);

    make_tokenrow(len, dtr);
    move_tokenrow(dtr, str);
    dtr->lp += len;
    return dtr;
}

/****************************************************************************
 *                                                                          *
 * Function: norm_tokenrow                                                  *
 *                                                                          *
 * Purpose : Produce a copy of a row of tokens.  Start at trp->tp.          *
 *           The value strings are copied as well.  The first token         *
 *           has WS available.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

TOKENROW *norm_tokenrow(TOKENROW *trp)
{
    TOKENROW *ntrp = new(TOKENROW);
    TOKEN *tp;
    int len;

    len = trp->lp - trp->tp;
    if (len <= 0) len = 1;
    make_tokenrow(len, ntrp);

    for (tp = trp->tp; tp < trp->lp; tp++)
    {
        *ntrp->lp = *tp;
        if (tp->len)
        {
            ntrp->lp->t = newstring(tp->t, tp->len, 1);
            *ntrp->lp->t++ = ' ';
            if (tp->wslen) ntrp->lp->wslen = 1;
        }
        ntrp->lp++;
    }

    if (ntrp->lp > ntrp->bp)
        ntrp->bp->wslen = 0;

    return ntrp;
}

/****************************************************************************
 *                                                                          *
 * Function: emptyrow                                                       *
 *                                                                          *
 * Purpose : Turn a row into just a newline.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-10-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

void emptyrow(TOKENROW *trp)
{
    static TOKEN nltoken = { NL, 0, 0, 0, 1, (uchar_t *)"\n" };

    trp->tp = trp->bp;
    trp->lp = trp->bp+1;
    *trp->bp = nltoken;
}

/****************************************************************************
 *                                                                          *
 * Function: peek_tokens                                                    *
 *                                                                          *
 * Purpose : Debugging.                                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef PODEBUG
void peek_tokens(TOKENROW *trp, char *str)
{
    TOKEN *tp;

    tp = trp->tp;

    if (str) fprintf(stdout, "%s ", str);

    if (tp < trp->bp || tp > trp->lp)
        fprintf(stdout, "(tp offset %d) ", tp-trp->bp);

    for (tp = trp->bp; tp < trp->lp && tp < trp->bp+32; tp++)
    {
        if (tp->type != NL)
        {
            int c = tp->t[tp->len];
            tp->t[tp->len] = 0;
            fprintf(stdout, "%s", tp->t);
            tp->t[tp->len] = c;
        }
        if (tp->type == NAME)
        {
            fprintf(stdout, (tp == trp->tp) ? "{*" : "{");
            print_hideset(tp->hideset);
            fprintf(stdout, "} ");
        }
        else
        {
            fprintf(stdout, (tp == trp->tp) ? "{%x*} " : "{%x} ", tp->type);
        }
    }

    fprintf(stdout, "\n");
    fflush(stdout);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: put_tokens                                                     *
 *                                                                          *
 * Purpose : Write tokens to the output buffer.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t put_tokens(TOKENROW *trp)
{
    TOKEN *tp;
    int len;
    uchar_t *p;

    tp = trp->bp;
    for (; tp < trp->lp; tp++)
    {
        len = tp->len + tp->wslen;
        p = tp->t-tp->wslen;

        while (tp < trp->lp-1 && p+len == (tp+1)->t - (tp+1)->wslen)
        {
            tp++;
            len += tp->wslen + tp->len;
        }

        memcpy(wbp, p, len);
        wbp += len;

        if (wbp >= &wbuf[3*OBS])
            pp_error(RCFATAL(ERROR_INTERNAL), "put_tokens");
    }
    trp->tp = tp;

    return (wbp < &wbuf[OBS]);
}

/****************************************************************************
 *                                                                          *
 * Function: pp_read                                                        *
 *                                                                          *
 * Purpose : Copy from preprocessor output to compiler input buffer.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

size_t pp_read(uchar_t *buf, size_t nbuf)
{
    static bool_t eof = FALSE;
    size_t size = 0;

    if (eof)
    {
        size = (wbp - wbuf);
        if (size > nbuf)
        {
            memcpy(buf, wbuf, nbuf);
            memcpy(wbuf, wbuf+nbuf, size-nbuf);
            wbp -= nbuf;
            return nbuf;
        }
        else if (size > 0)
            memcpy(buf, wbuf, size);

        wbp -= size;
        return size;
    }

    if (wbp < &wbuf[OBS])
    {
        do
            eof = pp_process(&maintr);
        while (!eof && wbp < &wbuf[OBS]);
    }

    if (wbp >= &wbuf[OBS])
    {
        memcpy(buf, wbuf, OBS);
        size = OBS;
        if (wbp > &wbuf[OBS])
            memcpy(wbuf, wbuf+OBS, wbp - &wbuf[OBS]);
        wbp -= OBS;
    }
    else
    {
        size = (wbp - wbuf);
        memcpy(buf, wbuf, size);
        wbp -= size;
    }

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: outnum                                                         *
 *                                                                          *
 * Purpose : Generate a number.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

char *outnum(char *p, int n)
{
    if (n >= 10)
        p = outnum(p, n/10);
    *p++ = n%10 + '0';

    return p;
}

