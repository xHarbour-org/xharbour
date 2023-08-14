/****************************************************************************
 *                                                                          *
 * File    : tokens.c                                                       *
 *                                                                          *
 * Purpose : ISO C Compiler; Preprocessor; Token management.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#define PREPROCESSOR
#include "lcc.h"

static uchar_t wbuf[4*OBS];
static uchar_t *wbp = wbuf;

/*
 * 1 for tokens that don't need whitespace when they
 * get inserted by macro expansion.
 */
static const char wstab[] = {
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

/****************************************************************************
 *                                                                          *
 * Function: make_tokenrow                                                  *
 *                                                                          *
 * Purpose : Allocate a new TOKENROW.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void make_tokenrow(int size, TOKENROW *trp)
{
    trp->max = size;

    if (size > 0)
        trp->bp = my_alloc(size * sizeof(TOKEN));
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
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TOKEN *grow_tokenrow(TOKENROW *trp)
{
    int ncur = trp->tp - trp->bp;
    int nlast = trp->lp - trp->bp;

    trp->max = 3*trp->max/2 + 1;
    trp->bp = my_realloc(trp->bp, trp->max * sizeof(TOKEN));
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
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int compare_tokens(TOKENROW *trp1, TOKENROW *trp2)
{
    TOKEN *tp1 = trp1->tp;
    TOKEN *tp2 = trp2->tp;

    if (trp1->lp - tp1 != trp2->lp - tp2)
        return 1;

    for (; tp1 < trp1->lp; tp1++, tp2++)
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
 * Function: replace_tokens                                                 *
 *                                                                          *
 * Purpose : Replace ntok tokens starting at dtr->tp with the contents      *
 *           of new. tp ends up pointing just beyond the replacement.       *
 *           Canonical whitespace is assured on each side.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void replace_tokens(TOKENROW *trp, int ntok, TOKENROW *new)
{
    size_t nrtok = rowlen(new);

    trp->tp += ntok;
    adjust_tokenrow(trp, nrtok - ntok);
    trp->tp -= ntok;
    move_tokenrow(trp, new);
    makespace(trp);
    trp->tp += nrtok;
    makespace(trp);
}

/****************************************************************************
 *                                                                          *
 * Function: makespace                                                      *
 *                                                                          *
 * Purpose : Make sure there is WS before trp->tp, if tokens might          *
 *           merge in the output.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-13  Changed newstring() to my_strncat().                 *
 *                                                                          *
 ****************************************************************************/

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

    tp->t = (uchar_t *)my_strncat(" ", 1, (char *)tp->t, tp->len) + 1;
    tp->wslen = 1;
    tp->flag |= XPWS;
}

/****************************************************************************
 *                                                                          *
 * Function: move_tokenrow                                                  *
 *                                                                          *
 * Purpose : Copy an entire tokenrow into another, at tp.                   *
 *           It is assumed that there is enough space.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-11-18  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

void move_tokenrow(TOKENROW *trp, TOKENROW *new)
{
    memmove(trp->tp, new->bp, rowlen(new) * sizeof(TOKEN));
}

/****************************************************************************
 *                                                                          *
 * Function: adjust_tokenrow                                                *
 *                                                                          *
 * Purpose : Move the tokens in a row, starting at tr->tp, rightward by     *
 *           ntok tokens; ntok may be negative (left move).                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-11-18  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

void adjust_tokenrow(TOKENROW *trp, int ntok)
{
    if (ntok != 0)  /* positive or negative */
    {
        int size = rowlen(trp) + ntok;

        while (size > trp->max)
            grow_tokenrow(trp);

        if (trp->tp < trp->lp)
            memmove(trp->tp + ntok, trp->tp, (trp->lp - trp->tp) * sizeof(TOKEN));

        trp->lp += ntok;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: copy_tokenrow                                                  *
 *                                                                          *
 * Purpose : Copy a row of tokens into the destination holder, allocating   *
 *           the space for the contents. Return the destination.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TOKENROW *copy_tokenrow(TOKENROW *trp, TOKENROW *new)
{
    int len = rowlen(new);

    make_tokenrow(len, trp);
    move_tokenrow(trp, new);
    trp->lp += len;
    return trp;
}

/****************************************************************************
 *                                                                          *
 * Function: norm_tokenrow                                                  *
 *                                                                          *
 * Purpose : Produce a copy of a row of tokens. Start at trp->tp.           *
 *           The value strings are copied as well. The first token          *
 *           has WS available.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-13  Changed newstring() to my_strncat().                 *
 *                                                                          *
 ****************************************************************************/

TOKENROW *norm_tokenrow(TOKENROW *trp)
{
    TOKENROW *ntrp = my_alloc(sizeof(*ntrp));
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
            ntrp->lp->t = (uchar_t *)my_strncat(" ", 1, (char *)tp->t, tp->len) + 1;
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
 * Function: empty_tokenrow                                                 *
 *                                                                          *
 * Purpose : Turn a row into just a newline.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void empty_tokenrow(TOKENROW *trp)
{
    static TOKEN nltoken = { NL, 0, 0, 0, 1, (uchar_t *)"\n" };

    trp->tp = trp->bp;
    trp->lp = trp->bp + 1;
    *trp->bp = nltoken;
}

/****************************************************************************
 *                                                                          *
 * Function: peek_tokens                                                    *
 *                                                                          *
 * Purpose : Debugging.                                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG)
void peek_tokens(TOKENROW *trp, char *str)
{
    TOKEN *tp;

    tp = trp->tp;

    if (str) fprintf(stdout, "%s ", str);

    if (tp < trp->bp || tp > trp->lp)
        fprintf(stdout, "(tp offset %d) ", tp-trp->bp);

    for (tp = trp->bp; tp < trp->lp && tp < trp->bp + 32; tp++)
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
 *           00-06-06  Created                                              *
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
 *           00-06-06  Created                                              *
 *           03-10-12  Replaced stupid code! Never use someone else code!   *
 *                                                                          *
 ****************************************************************************/

size_t pp_read(uchar_t *buf, size_t nbuf)
{
    static bool_t eof = FALSE;
    size_t size;

    while (!eof && wbp < &wbuf[OBS])
        eof = pp_process(&maintr);

    size = (wbp - wbuf);
    if (size > nbuf)
    {
        memcpy(buf, wbuf, nbuf);
        memcpy(wbuf, wbuf + nbuf, size - nbuf);
        wbp -= nbuf;
        return nbuf;
    }
    else
    {
        memcpy(buf, wbuf, size);
        wbp -= size;
        return size;
    }
}
