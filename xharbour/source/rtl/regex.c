/*
 * $Id: regex.c,v 1.2 2002/12/11 19:02:50 ronpinkas Exp $
 */

/*
Copyright 1992, 1993, 1994, 1997 Henry Spencer.  All rights reserved.
This software is not subject to any license of the American Telephone
and Telegraph Company or of the Regents of the University of California.

Permission is granted to anyone to use this software for any purpose on
any computer system, and to alter it and redistribute it, subject
to the following restrictions:

1. The author is not responsible for the consequences of use of this
   software, no matter how awful, even if they arise from flaws in it.

2. The origin of this software must not be misrepresented, either by
   explicit claim or by omission.  Since few users ever read sources,
   credits must appear in the documentation.

3. Altered versions must be plainly marked as such, and must not be
   misrepresented as being the original software.  Since few users
   ever read sources, credits must appear in the documentation.

4. This notice may not be removed or altered.
*/

/*
 *
 *  This file was created from merging the folowing original files:
 *
 * regcomp.c regexec.c engine.c regfree.c regerror.c
 *
 * Additionally include file regex.h was created by merging the following include file:
 *
 * regex.h regex2.h cclass.h cname.h utils.h
 * regcomp.ih engine.ih regerror.ih
 *
 * Above and xHarbour specific code was added by Ron Pinkas Ron@RonPinkas.com
 *
 */

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>
#include <assert.h>

#include "hbdefs.h"

#include "regex.h"

/*
 * parse structure, passed up and down to avoid global variables and
 * other clumsinesses
 */
struct parse {
    char *next;     /* next character in RE */
    char *end;      /* end of string (-> NUL normally) */
    int error;      /* has an error been seen? */
    sop *strip;     /* malloced strip */
    sopno ssize;        /* malloced strip size (allocated) */
    sopno slen;     /* malloced strip length (used) */
    int ncsalloc;       /* number of csets allocated */
    struct re_guts *g;
#   define  NPAREN  10  /* we need to remember () 1-9 for back refs */
    sopno pbegin[NPAREN];   /* -> ( ([0] unused) */
    sopno pend[NPAREN]; /* -> ) ([0] unused) */
};

//#include "regcomp.ih"
#ifdef __cplusplus
extern "C" {
#endif

static void p_ere(register struct parse *p, int stop);
static void p_ere_exp(register struct parse *p);
static void p_str(register struct parse *p);
static void p_bre(register struct parse *p, register int end1, register int end2);
static int p_simp_re(register struct parse *p, int starordinary);
static int p_count(register struct parse *p);
static void p_bracket(register struct parse *p);
static void p_b_term(register struct parse *p, register cset *cs);
static void p_b_cclass(register struct parse *p, register cset *cs);
static void p_b_eclass(register struct parse *p, register cset *cs);
static char p_b_symbol(register struct parse *p);
static char p_b_coll_elem(register struct parse *p, int endc);
static char othercase(int ch);
static void bothcases(register struct parse *p, int ch);
static void ordinary(register struct parse *p, register int ch);
static void nonnewline(register struct parse *p);
static void repeat(register struct parse *p, sopno start, int from, int to);
static int seterr(register struct parse *p, int e);
static cset *allocset(register struct parse *p);
static void freeset(register struct parse *p, register cset *cs);
static int freezeset(register struct parse *p, register cset *cs);
static int firstch(register struct parse *p, register cset *cs);
static int nch(register struct parse *p, register cset *cs);
static void mcadd(register struct parse *p, register cset *cs, register char *cp);
#if 1
static void mcsub(register cset *cs, register char *cp);
static int mcin(register cset *cs, register char *cp);
static char *mcfind(register cset *cs, register char *cp);
#endif
static void mcinvert(register struct parse *p, register cset *cs);
static void mccase(register struct parse *p, register cset *cs);
static int isinsets(register struct re_guts *g, int c);
static int samesets(register struct re_guts *g, int c1, int c2);
static void categorize(struct parse *p, register struct re_guts *g);
static sopno dupl(register struct parse *p, sopno start, sopno finish);
static void doemit(register struct parse *p, sop op, size_t opnd);
static void doinsert(register struct parse *p, sop op, size_t opnd, sopno pos);
static void dofwd(register struct parse *p, sopno pos, sop value);
static void enlarge(register struct parse *p, sopno size);
static void stripsnug(register struct parse *p, register struct re_guts *g);
static void findmust(register struct parse *p, register struct re_guts *g);
static sopno pluscount(register struct parse *p, register struct re_guts *g);

#ifdef __cplusplus
}
#endif

static char nuls[10];       /* place to point scanner in event of error */

/*
 * macros for use with parse structure
 * BEWARE:  these know that the parse structure is named `p' !!!
 */
#define PEEK()  (*p->next)
#define PEEK2() (*(p->next+1))
#define MORE()  (p->next < p->end)
#define MORE2() (p->next+1 < p->end)
#define SEE(c)  (MORE() && PEEK() == (c))
#define SEETWO(a, b)    (MORE() && MORE2() && PEEK() == (a) && PEEK2() == (b))
#define EAT(c)  ((SEE(c)) ? (NEXT(), 1) : 0)
#define EATTWO(a, b)    ((SEETWO(a, b)) ? (NEXT2(), 1) : 0)
#define NEXT()  (p->next++)
#define NEXT2() (p->next += 2)
#define NEXTn(n)    (p->next += (n))
#define GETNEXT()   (*p->next++)
#define SETERROR(e) seterr(p, (e))
#define REQUIRE(co, e)  ((co) || SETERROR(e))
#define MUSTSEE(c, e)   (REQUIRE(MORE() && PEEK() == (c), e))
#define MUSTEAT(c, e)   (REQUIRE(MORE() && GETNEXT() == (c), e))
#define MUSTNOTSEE(c, e)    (REQUIRE(!MORE() || PEEK() != (c), e))
#define EMIT(op, sopnd) doemit(p, (sop)(op), (size_t)(sopnd))
#define INSERT(op, pos) doinsert(p, (sop)(op), HERE()-(pos)+1, pos)
#define AHEAD(pos)      dofwd(p, pos, HERE()-(pos))
#define ASTERN(sop, pos)    EMIT(sop, HERE()-pos)
#define HERE()      (p->slen)
#define THERE()     (p->slen - 1)
#define THERETHERE()    (p->slen - 2)
#define DROP(n) (p->slen -= (n))

#ifndef NDEBUG
static int never = 0;       /* for use in asserts; shuts lint up */
#else
#define never   0       /* some <assert.h>s have bugs too */
#endif

/*
 - regcomp - interface for parser and compilation
 = extern int regcomp(regex_t *, const char *, int);
 = #define  REG_BASIC   0000
 = #define  REG_EXTENDED    0001
 = #define  REG_ICASE   0002
 = #define  REG_NOSUB   0004
 = #define  REG_NEWLINE 0010
 = #define  REG_NOSPEC  0020
 = #define  REG_PEND    0040
 = #define  REG_DUMP    0200
 */

/* 0 success, otherwise REG_something */
int regcomp( regex_t *preg, const char *pattern, int cflags )
{
    struct parse pa;
    register struct re_guts *g;
    register struct parse *p = &pa;
    register int i;
    register size_t len;

#ifdef REDEBUG
#   define  GOODCFLAGS(f)    (f)
#else
#   define  GOODCFLAGS(f)    ((f)&~REG_DUMP)
#endif

    cflags = GOODCFLAGS(cflags);
    if ((cflags&REG_EXTENDED) && (cflags&REG_NOSPEC))
        return(REG_INVARG);

    if (cflags&REG_PEND) {
        if (preg->re_endp < pattern)
            return(REG_INVARG);
        len = preg->re_endp - pattern;
    } else
        len = strlen((char *)pattern);

    /* do the mallocs early so failure handling is easy */
    g = (struct re_guts *)malloc(sizeof(struct re_guts) +
                            (NC-1)*sizeof(cat_t));
    if (g == NULL)
        return(REG_ESPACE);
    p->ssize = len/(size_t)2*(size_t)3 + (size_t)1; /* ugh */
    p->strip = (sop *)malloc(p->ssize * sizeof(sop));
    p->slen = 0;
    if (p->strip == NULL) {
        free((char *)g);
        return(REG_ESPACE);
    }

    /* set things up */
    p->g = g;
    p->next = (char *)pattern;  /* convenience; we do not modify it */
    p->end = p->next + len;
    p->error = 0;
    p->ncsalloc = 0;
    for (i = 0; i < NPAREN; i++) {
        p->pbegin[i] = 0;
        p->pend[i] = 0;
    }
    g->csetsize = NC;
    g->sets = NULL;
    g->setbits = NULL;
    g->ncsets = 0;
    g->cflags = cflags;
    g->iflags = 0;
    g->nbol = 0;
    g->neol = 0;
    g->must = NULL;
    g->mlen = 0;
    g->nsub = 0;
    g->ncategories = 1; /* category 0 is "everything else" */
    g->categories = &g->catspace[-(CHAR_MIN)];
    (void) memset((char *)g->catspace, 0, NC*sizeof(cat_t));
    g->backrefs = 0;

    /* do it */
    EMIT(OEND, 0);
    g->firststate = THERE();
    if (cflags&REG_EXTENDED)
        p_ere(p, OUT);
    else if (cflags&REG_NOSPEC)
        p_str(p);
    else
        p_bre(p, OUT, OUT);
    EMIT(OEND, 0);
    g->laststate = THERE();

    /* tidy up loose ends and fill things in */
    categorize(p, g);
    stripsnug(p, g);
    findmust(p, g);
    g->nplus = pluscount(p, g);
    g->magic = MAGIC2;
    preg->re_nsub = g->nsub;
    preg->re_g = g;
    preg->re_magic = MAGIC1;
#ifndef REDEBUG
    /* not debugging, so can't rely on the assert() in regexec() */
    if (g->iflags&BAD)
        SETERROR(REG_ASSERT);
#endif

    /* win or lose, we're done */
    if (p->error != 0)  /* lose */
        regfree(preg);
    return(p->error);
}

/*
 - p_ere - ERE parser top level, concatenation and alternation
 == static void p_ere(register struct parse *p, int stop);
 */
static void p_ere( register struct parse *p, int stop /* character this ERE should end at */ )
{
    register char c;
    register sopno prevback;
    register sopno prevfwd;
    register sopno conc;
    register int first = 1;     /* is this the first alternative? */

    for (;;) {
        /* do a bunch of concatenated expressions */
        conc = HERE();
        while (MORE() && (c = PEEK()) != '|' && c != stop)
            p_ere_exp(p);
        REQUIRE(HERE() != conc, REG_EMPTY); /* require nonempty */

        if (!EAT('|'))
            break;      /* NOTE BREAK OUT */

        if (first) {
            INSERT(OCH_, conc); /* offset is wrong */
            prevfwd = conc;
            prevback = conc;
            first = 0;
        }
        ASTERN(OOR1, prevback);
        prevback = THERE();
        AHEAD(prevfwd);         /* fix previous offset */
        prevfwd = HERE();
        EMIT(OOR2, 0);          /* offset is very wrong */
    }

    if (!first) {       /* tail-end fixups */
        AHEAD(prevfwd);
        ASTERN(O_CH, prevback);
    }

    assert(!MORE() || SEE(stop));
}

/*
 - p_ere_exp - parse one subERE, an atom possibly followed by a repetition op
 == static void p_ere_exp(register struct parse *p);
 */
static void p_ere_exp( register struct parse *p )
{
    register char c;
    register sopno pos;
    register int count;
    register int count2;
    register sopno subno;
    int wascaret = 0;

    assert(MORE());     /* caller should have ensured this */
    c = GETNEXT();

    pos = HERE();
    switch (c) {
    case '(':
        REQUIRE(MORE(), REG_EPAREN);
        p->g->nsub++;
        subno = p->g->nsub;
        if (subno < NPAREN)
            p->pbegin[subno] = HERE();
        EMIT(OLPAREN, subno);
        if (!SEE(')'))
            p_ere(p, ')');
        if (subno < NPAREN) {
            p->pend[subno] = HERE();
            assert(p->pend[subno] != 0);
        }
        EMIT(ORPAREN, subno);
        MUSTEAT(')', REG_EPAREN);
        break;
#ifndef POSIX_MISTAKE
    case ')':       /* happens only if no current unmatched ( */
        /*
         * You may ask, why the ifndef?  Because I didn't notice
         * this until slightly too late for 1003.2, and none of the
         * other 1003.2 regular-expression reviewers noticed it at
         * all.  So an unmatched ) is legal POSIX, at least until
         * we can get it fixed.
         */
        SETERROR(REG_EPAREN);
        break;
#endif
    case '^':
        EMIT(OBOL, 0);
        p->g->iflags |= USEBOL;
        p->g->nbol++;
        wascaret = 1;
        break;
    case '$':
        EMIT(OEOL, 0);
        p->g->iflags |= USEEOL;
        p->g->neol++;
        break;
    case '|':
        SETERROR(REG_EMPTY);
        break;
    case '*':
    case '+':
    case '?':
        SETERROR(REG_BADRPT);
        break;
    case '.':
        if (p->g->cflags&REG_NEWLINE)
            nonnewline(p);
        else
            EMIT(OANY, 0);
        break;
    case '[':
        p_bracket(p);
        break;
    case '\\':
        REQUIRE(MORE(), REG_EESCAPE);
        c = GETNEXT();
        ordinary(p, c);
        break;
    case '{':       /* okay as ordinary except if digit follows */
        REQUIRE(!MORE() || !isdigit(PEEK()), REG_BADRPT);
        /* FALLTHROUGH */
    default:
        ordinary(p, c);
        break;
    }

    if (!MORE())
        return;
    c = PEEK();
    /* we call { a repetition if followed by a digit */
    if (!( c == '*' || c == '+' || c == '?' ||
                (c == '{' && MORE2() && isdigit(PEEK2())) ))
        return;     /* no repetition, we're done */
    NEXT();

    REQUIRE(!wascaret, REG_BADRPT);
    switch (c) {
    case '*':   /* implemented as +? */
        /* this case does not require the (y|) trick, noKLUDGE */
        INSERT(OPLUS_, pos);
        ASTERN(O_PLUS, pos);
        INSERT(OQUEST_, pos);
        ASTERN(O_QUEST, pos);
        break;
    case '+':
        INSERT(OPLUS_, pos);
        ASTERN(O_PLUS, pos);
        break;
    case '?':
        /* KLUDGE: emit y? as (y|) until subtle bug gets fixed */
        INSERT(OCH_, pos);      /* offset slightly wrong */
        ASTERN(OOR1, pos);      /* this one's right */
        AHEAD(pos);         /* fix the OCH_ */
        EMIT(OOR2, 0);          /* offset very wrong... */
        AHEAD(THERE());         /* ...so fix it */
        ASTERN(O_CH, THERETHERE());
        break;
    case '{':
        count = p_count(p);
        if (EAT(',')) {
            if (isdigit(PEEK())) {
                count2 = p_count(p);
                REQUIRE(count <= count2, REG_BADBR);
            } else      /* single number with comma */
                count2 = INFINITY;
        } else      /* just a single number */
            count2 = count;
        repeat(p, pos, count, count2);
        if (!EAT('}')) {    /* error heuristics */
            while (MORE() && PEEK() != '}')
                NEXT();
            REQUIRE(MORE(), REG_EBRACE);
            SETERROR(REG_BADBR);
        }
        break;
    }

    if (!MORE())
        return;
    c = PEEK();
    if (!( c == '*' || c == '+' || c == '?' ||
                (c == '{' && MORE2() && isdigit(PEEK2())) ) )
        return;
    SETERROR(REG_BADRPT);
}

/*
 - p_str - string (no metacharacters) "parser"
 == static void p_str(register struct parse *p);
 */
static void p_str( register struct parse *p )
{
    REQUIRE(MORE(), REG_EMPTY);
    while (MORE())
        ordinary(p, GETNEXT());
}

/*
 - p_bre - BRE parser top level, anchoring and concatenation
 == static void p_bre(register struct parse *p, register int end1, \
 == register int end2);
 * Giving end1 as OUT essentially eliminates the end1/end2 check.
 *
 * This implementation is a bit of a kludge, in that a trailing $ is first
 * taken as an ordinary character and then revised to be an anchor.  The
 * only undesirable side effect is that '$' gets included as a character
 * category in such cases.  This is fairly harmless; not worth fixing.
 * The amount of lookahead needed to avoid this kludge is excessive.
 */

static void p_bre( register struct parse *p, register int end1 , register int end2 )
{
    register sopno start = HERE();
    register int first = 1;         /* first subexpression? */
    register int wasdollar = 0;

    if (EAT('^')) {
        EMIT(OBOL, 0);
        p->g->iflags |= USEBOL;
        p->g->nbol++;
    }
    while (MORE() && !SEETWO(end1, end2)) {
        wasdollar = p_simp_re(p, first);
        first = 0;
    }
    if (wasdollar) {    /* oops, that was a trailing anchor */
        DROP(1);
        EMIT(OEOL, 0);
        p->g->iflags |= USEEOL;
        p->g->neol++;
    }

    REQUIRE(HERE() != start, REG_EMPTY);    /* require nonempty */
}

/*
 - p_simp_re - parse a simple RE, an atom possibly followed by a repetition
 == static int p_simp_re(register struct parse *p, int starordinary);
 */

/* was the simple RE an unbackslashed $? */
static int p_simp_re( register struct parse *p, int starordinary /* is a leading * an ordinary character? */ )
{
    register int c;
    register int count;
    register int count2;
    register sopno pos;
    register int i;
    register sopno subno;
#   define  BACKSL  (1<<CHAR_BIT)

    pos = HERE();       /* repetion op, if any, covers from here */

    assert(MORE());     /* caller should have ensured this */
    c = GETNEXT();
    if (c == '\\') {
        REQUIRE(MORE(), REG_EESCAPE);
        c = BACKSL | (unsigned char)GETNEXT();
    }
    switch (c) {
    case '.':
        if (p->g->cflags&REG_NEWLINE)
            nonnewline(p);
        else
            EMIT(OANY, 0);
        break;
    case '[':
        p_bracket(p);
        break;
    case BACKSL|'{':
        SETERROR(REG_BADRPT);
        break;
    case BACKSL|'(':
        p->g->nsub++;
        subno = p->g->nsub;
        if (subno < NPAREN)
            p->pbegin[subno] = HERE();
        EMIT(OLPAREN, subno);
        /* the MORE here is an error heuristic */
        if (MORE() && !SEETWO('\\', ')'))
            p_bre(p, '\\', ')');
        if (subno < NPAREN) {
            p->pend[subno] = HERE();
            assert(p->pend[subno] != 0);
        }
        EMIT(ORPAREN, subno);
        REQUIRE(EATTWO('\\', ')'), REG_EPAREN);
        break;
    case BACKSL|')':    /* should not get here -- must be user */
    case BACKSL|'}':
        SETERROR(REG_EPAREN);
        break;
    case BACKSL|'1':
    case BACKSL|'2':
    case BACKSL|'3':
    case BACKSL|'4':
    case BACKSL|'5':
    case BACKSL|'6':
    case BACKSL|'7':
    case BACKSL|'8':
    case BACKSL|'9':
        i = (c&~BACKSL) - '0';
        assert(i < NPAREN);
        if (p->pend[i] != 0) {
            assert((size_t)i <= p->g->nsub);
            EMIT(OBACK_, i);
            assert(p->pbegin[i] != 0);
            assert(OP(p->strip[p->pbegin[i]]) == OLPAREN);
            assert(OP(p->strip[p->pend[i]]) == ORPAREN);
            (void) dupl(p, p->pbegin[i]+1, p->pend[i]);
            EMIT(O_BACK, i);
        } else
            SETERROR(REG_ESUBREG);
        p->g->backrefs = 1;
        break;
    case '*':
        REQUIRE(starordinary, REG_BADRPT);
        /* FALLTHROUGH */
    default:
        ordinary(p, (char)c);   /* takes off BACKSL, if any */
        break;
    }

    if (EAT('*')) {     /* implemented as +? */
        /* this case does not require the (y|) trick, noKLUDGE */
        INSERT(OPLUS_, pos);
        ASTERN(O_PLUS, pos);
        INSERT(OQUEST_, pos);
        ASTERN(O_QUEST, pos);
    } else if (EATTWO('\\', '{')) {
        count = p_count(p);
        if (EAT(',')) {
            if (MORE() && isdigit(PEEK())) {
                count2 = p_count(p);
                REQUIRE(count <= count2, REG_BADBR);
            } else      /* single number with comma */
                count2 = INFINITY;
        } else      /* just a single number */
            count2 = count;
        repeat(p, pos, count, count2);
        if (!EATTWO('\\', '}')) {   /* error heuristics */
            while (MORE() && !SEETWO('\\', '}'))
                NEXT();
            REQUIRE(MORE(), REG_EBRACE);
            SETERROR(REG_BADBR);
        }
    } else if (c == (unsigned char)'$') /* $ (but not \$) ends it */
        return(1);

    return(0);
}

/*
 - p_count - parse a repetition count
 == static int p_count(register struct parse *p);
 */
static int /* the value */ p_count( register struct parse *p )
{
    register int count = 0;
    register int ndigits = 0;

    while (MORE() && isdigit(PEEK()) && count <= DUPMAX) {
        count = count*10 + (GETNEXT() - '0');
        ndigits++;
    }

    REQUIRE(ndigits > 0 && count <= DUPMAX, REG_BADBR);
    return(count);
}

/*
 - p_bracket - parse a bracketed character list
 == static void p_bracket(register struct parse *p);
 *
 * Note a significant property of this code:  if the allocset() did SETERROR,
 * no set operations are done.
 */
static void p_bracket( register struct parse *p )
{
    register cset *cs = allocset(p);
    register int invert = 0;

    /* Dept of Truly Sickening Special-Case Kludges */
    if (p->next + 5 < p->end && strncmp(p->next, "[:<:]]", 6) == 0) {
        EMIT(OBOW, 0);
        NEXTn(6);
        return;
    }
    if (p->next + 5 < p->end && strncmp(p->next, "[:>:]]", 6) == 0) {
        EMIT(OEOW, 0);
        NEXTn(6);
        return;
    }

    if (EAT('^'))
        invert++;   /* make note to invert set at end */
    if (EAT(']'))
        CHadd(cs, ']');
    else if (EAT('-'))
        CHadd(cs, '-');
    while (MORE() && PEEK() != ']' && !SEETWO('-', ']'))
        p_b_term(p, cs);
    if (EAT('-'))
        CHadd(cs, '-');
    MUSTEAT(']', REG_EBRACK);

    if (p->error != 0)  /* don't mess things up further */
        return;

    if (p->g->cflags&REG_ICASE) {
        register int i;
        register int ci;

        for (i = p->g->csetsize - 1; i >= 0; i--)
            if (CHIN(cs, i) && isalpha(i)) {
                ci = othercase(i);
                if (ci != i)
                    CHadd(cs, ci);
            }
        if (cs->multis != NULL)
            mccase(p, cs);
    }
    if (invert) {
        register int i;

        for (i = p->g->csetsize - 1; i >= 0; i--)
            if (CHIN(cs, i))
                CHsub(cs, i);
            else
                CHadd(cs, i);
        if (p->g->cflags&REG_NEWLINE)
            CHsub(cs, '\n');
        if (cs->multis != NULL)
            mcinvert(p, cs);
    }

    assert(cs->multis == NULL);     /* xxx */

    if (nch(p, cs) == 1) {      /* optimize singleton sets */
        ordinary(p, firstch(p, cs));
        freeset(p, cs);
    } else
        EMIT(OANYOF, freezeset(p, cs));
}

/*
 - p_b_term - parse one term of a bracketed character list
 == static void p_b_term(register struct parse *p, register cset *cs);
 */
static void p_b_term( register struct parse *p, register cset *cs )
{
    register char c;
    register char start, finish;
    register int i;

    /* classify what we've got */
    switch ((MORE()) ? PEEK() : '\0') {
    case '[':
        c = (MORE2()) ? PEEK2() : '\0';
        break;
    case '-':
        SETERROR(REG_ERANGE);
        return;         /* NOTE RETURN */
    default:
        c = '\0';
        break;
    }

    switch (c) {
    case ':':       /* character class */
        NEXT2();
        REQUIRE(MORE(), REG_EBRACK);
        c = PEEK();
        REQUIRE(c != '-' && c != ']', REG_ECTYPE);
        p_b_cclass(p, cs);
        REQUIRE(MORE(), REG_EBRACK);
        REQUIRE(EATTWO(':', ']'), REG_ECTYPE);
        break;
    case '=':       /* equivalence class */
        NEXT2();
        REQUIRE(MORE(), REG_EBRACK);
        c = PEEK();
        REQUIRE(c != '-' && c != ']', REG_ECOLLATE);
        p_b_eclass(p, cs);
        REQUIRE(MORE(), REG_EBRACK);
        REQUIRE(EATTWO('=', ']'), REG_ECOLLATE);
        break;
    default:        /* symbol, ordinary character, or range */
/* xxx revision needed for multichar stuff */
        start = p_b_symbol(p);
        if (SEE('-') && MORE2() && PEEK2() != ']') {
            /* range */
            NEXT();
            if (EAT('-'))
                finish = '-';
            else
                finish = p_b_symbol(p);
        } else
            finish = start;
/* xxx what about signed chars here... */
        REQUIRE(start <= finish, REG_ERANGE);
        for (i = start; i <= finish; i++)
            CHadd(cs, i);
        break;
    }
}

/*
 - p_b_cclass - parse a character-class name and deal with it
 == static void p_b_cclass(register struct parse *p, register cset *cs);
 */
static void p_b_cclass( register struct parse *p, register cset *cs )
{
    register char *sp = p->next;
    register struct cclass *cp;
    register size_t len;
    register char *u;
    register char c;

    while (MORE() && isalpha(PEEK()))
        NEXT();
    len = p->next - sp;
    for (cp = cclasses; cp->name != NULL; cp++)
        if (strncmp(cp->name, sp, len) == 0 && cp->name[len] == '\0')
            break;
    if (cp->name == NULL) {
        /* oops, didn't find it */
        SETERROR(REG_ECTYPE);
        return;
    }

    u = cp->chars;
    while ((c = *u++) != '\0')
        CHadd(cs, c);
    for (u = cp->multis; *u != '\0'; u += strlen(u) + 1)
        MCadd(p, cs, u);
}

/*
 - p_b_eclass - parse an equivalence-class name and deal with it
 == static void p_b_eclass(register struct parse *p, register cset *cs);
 *
 * This implementation is incomplete. xxx
 */
static void p_b_eclass( register struct parse *p, register cset *cs )
{
    register char c;

    c = p_b_coll_elem(p, '=');
    CHadd(cs, c);
}

/*
 - p_b_symbol - parse a character or [..]ed multicharacter collating symbol
 == static char p_b_symbol(register struct parse *p);
 */
static char /* value of symbol */ p_b_symbol( register struct parse *p )
{
    register char value;

    REQUIRE(MORE(), REG_EBRACK);
    if (!EATTWO('[', '.'))
        return(GETNEXT());

    /* collating symbol */
    value = p_b_coll_elem(p, '.');
    REQUIRE(EATTWO('.', ']'), REG_ECOLLATE);
    return(value);
}

/*
 - p_b_coll_elem - parse a collating-element name and look it up
 == static char p_b_coll_elem(register struct parse *p, int endc);
 */
/* value of collating element */
static char p_b_coll_elem( register struct parse *p, int endc /* name ended by endc,']' */ )
{
    register char *sp = p->next;
    register struct cname *cp;
    register int len;

    while (MORE() && !SEETWO(endc, ']'))
        NEXT();
    if (!MORE()) {
        SETERROR(REG_EBRACK);
        return(0);
    }
    len = p->next - sp;
    for (cp = cnames; cp->name != NULL; cp++)
        if (strncmp(cp->name, sp, len) == 0 && cp->name[len] == '\0')
            return(cp->code);   /* known name */
    if (len == 1)
        return(*sp);    /* single character */
    SETERROR(REG_ECOLLATE);         /* neither */
    return(0);
}

/*
 - othercase - return the case counterpart of an alphabetic
 == static char othercase(int ch);
 */
/* if no counterpart, return ch */
static char othercase( int ch )
{
    assert(isalpha(ch));
    if (isupper(ch))
        return(tolower(ch));
    else if (islower(ch))
        return(toupper(ch));
    else            /* peculiar, but could happen */
        return(ch);
}

/*
 - bothcases - emit a dualcase version of a two-case character
 == static void bothcases(register struct parse *p, int ch);
 *
 * Boy, is this implementation ever a kludge...
 */
static void bothcases( register struct parse *p, int ch )
{
    register char *oldnext = p->next;
    register char *oldend = p->end;
    char bracket[3];

    assert(othercase(ch) != ch);    /* p_bracket() would recurse */
    p->next = bracket;
    p->end = bracket+2;
    bracket[0] = ch;
    bracket[1] = ']';
    bracket[2] = '\0';
    p_bracket(p);
    assert(p->next == bracket+2);
    p->next = oldnext;
    p->end = oldend;
}

/*
 - ordinary - emit an ordinary character
 == static void ordinary(register struct parse *p, register int ch);
 */
static void ordinary( register struct parse *p, register int ch )
{
    register cat_t *cap = p->g->categories;

    if ((p->g->cflags&REG_ICASE) && isalpha(ch) && othercase(ch) != ch)
        bothcases(p, ch);
    else {
        EMIT(OCHAR, (unsigned char)ch);
        if (cap[ch] == 0)
            cap[ch] = p->g->ncategories++;
    }
}

/*
 - nonnewline - emit REG_NEWLINE version of OANY
 == static void nonnewline(register struct parse *p);
 *
 * Boy, is this implementation ever a kludge...
 */
static void nonnewline( register struct parse *p )
{
    register char *oldnext = p->next;
    register char *oldend = p->end;
    char bracket[4];

    p->next = bracket;
    p->end = bracket+3;
    bracket[0] = '^';
    bracket[1] = '\n';
    bracket[2] = ']';
    bracket[3] = '\0';
    p_bracket(p);
    assert(p->next == bracket+3);
    p->next = oldnext;
    p->end = oldend;
}

/*
 - repeat - generate code for a bounded repetition, recursively if needed
 == static void repeat(register struct parse *p, sopno start, int from, int to);
 */
static void repeat( register struct parse *p,
                    sopno start /* operand from here to end of strip */,
                    int from,           /* repeated from this number */
                    int to )             /* to this number of times (maybe INFINITY) */
{
    register sopno finish = HERE();
#   define  N   2
#   define  INF 3
#   define  REP(f, t)   ((f)*8 + (t))
#   define  MAP(n)  (((n) <= 1) ? (n) : ((n) == INFINITY) ? INF : N)
    register sopno copy;

    if (p->error != 0)  /* head off possible runaway recursion */
        return;

    assert(from <= to);

    switch (REP(MAP(from), MAP(to))) {
    case REP(0, 0):         /* must be user doing this */
        DROP(finish-start); /* drop the operand */
        break;
    case REP(0, 1):         /* as x{1,1}? */
    case REP(0, N):         /* as x{1,n}? */
    case REP(0, INF):       /* as x{1,}? */
        /* KLUDGE: emit y? as (y|) until subtle bug gets fixed */
        INSERT(OCH_, start);        /* offset is wrong... */
        repeat(p, start+1, 1, to);
        ASTERN(OOR1, start);
        AHEAD(start);           /* ... fix it */
        EMIT(OOR2, 0);
        AHEAD(THERE());
        ASTERN(O_CH, THERETHERE());
        break;
    case REP(1, 1):         /* trivial case */
        /* done */
        break;
    case REP(1, N):         /* as x?x{1,n-1} */
        /* KLUDGE: emit y? as (y|) until subtle bug gets fixed */
        INSERT(OCH_, start);
        ASTERN(OOR1, start);
        AHEAD(start);
        EMIT(OOR2, 0);          /* offset very wrong... */
        AHEAD(THERE());         /* ...so fix it */
        ASTERN(O_CH, THERETHERE());
        copy = dupl(p, start+1, finish+1);
        assert(copy == finish+4);
        repeat(p, copy, 1, to-1);
        break;
    case REP(1, INF):       /* as x+ */
        INSERT(OPLUS_, start);
        ASTERN(O_PLUS, start);
        break;
    case REP(N, N):         /* as xx{m-1,n-1} */
        copy = dupl(p, start, finish);
        repeat(p, copy, from-1, to-1);
        break;
    case REP(N, INF):       /* as xx{n-1,INF} */
        copy = dupl(p, start, finish);
        repeat(p, copy, from-1, to);
        break;
    default:            /* "can't happen" */
        SETERROR(REG_ASSERT);   /* just in case */
        break;
    }
}

/*
 - seterr - set an error condition
 == static int seterr(register struct parse *p, int e);
 */
static int /* useless but makes type checking happy */
seterr(
register struct parse *p,
int e )
{
    if (p->error == 0)  /* keep earliest error condition */
        p->error = e;
    p->next = nuls;     /* try to bring things to a halt */
    p->end = nuls;
    return(0);      /* make the return value well-defined */
}

/*
 - allocset - allocate a set of characters for []
 == static cset *allocset(register struct parse *p);
 */
static cset *
allocset
( register struct parse *p )
{
    register int no = p->g->ncsets++;
    register size_t nc;
    register size_t nbytes;
    register cset *cs;
    register size_t css = (size_t)p->g->csetsize;
    register int i;

    if (no >= p->ncsalloc) {    /* need another column of space */
        p->ncsalloc += CHAR_BIT;
        nc = p->ncsalloc;
        assert(nc % CHAR_BIT == 0);
        nbytes = nc / CHAR_BIT * css;
        if (p->g->sets == NULL)
            p->g->sets = (cset *)malloc(nc * sizeof(cset));
        else
            p->g->sets = (cset *)realloc((char *)p->g->sets,
                            nc * sizeof(cset));
        if (p->g->setbits == NULL)
            p->g->setbits = (uch *)malloc(nbytes);
        else {
            p->g->setbits = (uch *)realloc((char *)p->g->setbits,
                                nbytes);
            /* xxx this isn't right if setbits is now NULL */
            for (i = 0; i < no; i++)
                p->g->sets[i].ptr = p->g->setbits + css*(i/CHAR_BIT);
        }
        if (p->g->sets != NULL && p->g->setbits != NULL)
            (void) memset((char *)p->g->setbits + (nbytes - css),
                                0, css);
        else {
            no = 0;
            SETERROR(REG_ESPACE);
            /* caller's responsibility not to do set ops */
        }
    }

    assert(p->g->sets != NULL); /* xxx */
    cs = &p->g->sets[no];
    cs->ptr = p->g->setbits + css*((no)/CHAR_BIT);
    cs->mask = 1 << ((no) % CHAR_BIT);
    cs->hash = 0;
    cs->smultis = 0;
    cs->multis = NULL;

    return(cs);
}

/*
 - freeset - free a now-unused set
 == static void freeset(register struct parse *p, register cset *cs);
 */
static void
freeset
( register struct parse *p,
register cset *cs )
{
    register int i;
    register cset *top = &p->g->sets[p->g->ncsets];
    register size_t css = (size_t)p->g->csetsize;

    for (i = 0; (size_t)i < css; i++)
        CHsub(cs, i);
    if (cs == top-1)    /* recover only the easy case */
        p->g->ncsets--;
}

/*
 - freezeset - final processing on a set of characters
 == static int freezeset(register struct parse *p, register cset *cs);
 *
 * The main task here is merging identical sets.  This is usually a waste
 * of time (although the hash code minimizes the overhead), but can win
 * big if REG_ICASE is being used.  REG_ICASE, by the way, is why the hash
 * is done using addition rather than xor -- all ASCII [aA] sets xor to
 * the same value!
 */
static int          /* set number */
freezeset
( register struct parse *p,
register cset *cs )
{
    register uch h = cs->hash;
    register int i;
    register cset *top = &p->g->sets[p->g->ncsets];
    register cset *cs2;
    register size_t css = (size_t)p->g->csetsize;

    /* look for an earlier one which is the same */
    for (cs2 = &p->g->sets[0]; cs2 < top; cs2++)
        if (cs2->hash == h && cs2 != cs) {
            /* maybe */
            for (i = 0; (size_t)i < css; i++)
                if (!!CHIN(cs2, i) != !!CHIN(cs, i))
                    break;      /* no */
            if ((size_t)i == css)
                break;          /* yes */
        }

    if (cs2 < top) {    /* found one */
        freeset(p, cs);
        cs = cs2;
    }

    return((int)(cs - p->g->sets));
}

/*
 - firstch - return first character in a set (which must have at least one)
 == static int firstch(register struct parse *p, register cset *cs);
 */
static int          /* character; there is no "none" value */
firstch(
register struct parse *p,
register cset *cs )
{
    register int i;
    register size_t css = (size_t)p->g->csetsize;

    for (i = 0; (size_t)i < css; i++)
        if (CHIN(cs, i))
            return((char)i);
    assert(never);
    return(0);      /* arbitrary */
}

/*
 - nch - number of characters in a set
 == static int nch(register struct parse *p, register cset *cs);
 */
static int
nch
( register struct parse *p,
register cset *cs )
{
    register int i;
    register size_t css = (size_t)p->g->csetsize;
    register int n = 0;

    for (i = 0; (size_t)i < css; i++)
        if (CHIN(cs, i))
            n++;
    return(n);
}

/*
 - mcadd - add a collating element to a cset
 == static void mcadd(register struct parse *p, register cset *cs, \
 == register char *cp);
 */
static void
mcadd
( register struct parse *p,
register cset *cs,
register char *cp )
{
    register size_t oldend = cs->smultis;

    cs->smultis += strlen(cp) + 1;
    if (cs->multis == NULL)
        cs->multis = (char *) malloc(cs->smultis);
    else
        cs->multis = (char *) realloc(cs->multis, cs->smultis);
    if (cs->multis == NULL) {
        SETERROR(REG_ESPACE);
        return;
    }

    (void) strcpy(cs->multis + oldend - 1, cp);
    cs->multis[cs->smultis - 1] = '\0';
}

/*
 - mcsub - subtract a collating element from a cset
 == static void mcsub(register cset *cs, register char *cp);
 */
static void
mcsub
( register cset *cs,
register char *cp )
{
    register char *fp = mcfind(cs, cp);
    register size_t len = strlen(fp);

    assert(fp != NULL);
    (void) memmove(fp, fp + len + 1,
                cs->smultis - (fp + len + 1 - cs->multis));
    cs->smultis -= len;

    if (cs->smultis == 0) {
        free(cs->multis);
        cs->multis = NULL;
        return;
    }

    cs->multis = (char *) realloc(cs->multis, cs->smultis);
    assert(cs->multis != NULL);
}

/*
 - mcin - is a collating element in a cset?
 == static int mcin(register cset *cs, register char *cp);
 */
static int
mcin
( register cset *cs,
register char *cp )
{
    return(mcfind(cs, cp) != NULL);
}

/*
 - mcfind - find a collating element in a cset
 == static char *mcfind(register cset *cs, register char *cp);
 */
static char *
mcfind
( register cset *cs,
register char *cp )
{
    register char *p;

    if (cs->multis == NULL)
        return(NULL);
    for (p = cs->multis; *p != '\0'; p += strlen(p) + 1)
        if (strcmp(cp, p) == 0)
            return(p);
    return(NULL);
}

/*
 - mcinvert - invert the list of collating elements in a cset
 == static void mcinvert(register struct parse *p, register cset *cs);
 *
 * This would have to know the set of possibilities.  Implementation
 * is deferred.
 */
static void
mcinvert
( register struct parse *p,
register cset *cs )
{
    HB_SYMBOL_UNUSED( p );

    assert(cs->multis == NULL); /* xxx */
}

/*
 - mccase - add case counterparts of the list of collating elements in a cset
 == static void mccase(register struct parse *p, register cset *cs);
 *
 * This would have to know the set of possibilities.  Implementation
 * is deferred.
 */
static void
mccase
( register struct parse *p,
register cset *cs )
{
    HB_SYMBOL_UNUSED( p );
    assert(cs->multis == NULL); /* xxx */
}

/*
 - isinsets - is this character in any sets?
 == static int isinsets(register struct re_guts *g, int c);
 */
static int          /* predicate */
isinsets(
register struct re_guts *g,
int c )
{
    register uch *col;
    register int i;
    register int ncols = (g->ncsets+(CHAR_BIT-1)) / CHAR_BIT;
    register unsigned uc = (unsigned char)c;

    for (i = 0, col = g->setbits; i < ncols; i++, col += g->csetsize)
        if (col[uc] != 0)
            return(1);
    return(0);
}

/*
 - samesets - are these two characters in exactly the same sets?
 == static int samesets(register struct re_guts *g, int c1, int c2);
 */
static int          /* predicate */
samesets(
register struct re_guts *g,
int c1,
int c2 )
{
    register uch *col;
    register int i;
    register int ncols = (g->ncsets+(CHAR_BIT-1)) / CHAR_BIT;
    register unsigned uc1 = (unsigned char)c1;
    register unsigned uc2 = (unsigned char)c2;

    for (i = 0, col = g->setbits; i < ncols; i++, col += g->csetsize)
        if (col[uc1] != col[uc2])
            return(0);
    return(1);
}

/*
 - categorize - sort out character categories
 == static void categorize(struct parse *p, register struct re_guts *g);
 */
static void
categorize(
struct parse *p,
register struct re_guts *g )
{
    register cat_t *cats = g->categories;
    register int c;
    register int c2;
    register cat_t cat;

    /* avoid making error situations worse */
    if (p->error != 0)
        return;

    for (c = CHAR_MIN; c <= CHAR_MAX; c++)
        if (cats[c] == 0 && isinsets(g, c)) {
            cat = g->ncategories++;
            cats[c] = cat;
            for (c2 = c+1; c2 <= CHAR_MAX; c2++)
                if (cats[c2] == 0 && samesets(g, c, c2))
                    cats[c2] = cat;
        }
}

/*
 - dupl - emit a duplicate of a bunch of sops
 == static sopno dupl(register struct parse *p, sopno start, sopno finish);
 */
static sopno            /* start of duplicate */
dupl(
register struct parse *p,
sopno start,            /* from here */
sopno finish )           /* to this less one */
{
    register sopno ret = HERE();
    register sopno len = finish - start;

    assert(finish >= start);
    if (len == 0)
        return(ret);
    enlarge(p, p->ssize + len); /* this many unexpected additions */
    assert(p->ssize >= p->slen + len);
    (void) memcpy((char *)(p->strip + p->slen),
        (char *)(p->strip + start), (size_t)len*sizeof(sop));
    p->slen += len;
    return(ret);
}

/*
 - doemit - emit a strip operator
 == static void doemit(register struct parse *p, sop op, size_t opnd);
 *
 * It might seem better to implement this as a macro with a function as
 * hard-case backup, but it's just too big and messy unless there are
 * some changes to the data structures.  Maybe later.
 */
static void
doemit(
register struct parse *p,
sop op,
size_t opnd )
{
    /* avoid making error situations worse */
    if (p->error != 0)
        return;

    /* deal with oversize operands ("can't happen", more or less) */
    assert(opnd < 1<<OPSHIFT);

    /* deal with undersized strip */
    if (p->slen >= p->ssize)
        enlarge(p, (p->ssize+1) / 2 * 3);   /* +50% */
    assert(p->slen < p->ssize);

    /* finally, it's all reduced to the easy case */
    p->strip[p->slen++] = SOP(op, opnd);
}

/*
 - doinsert - insert a sop into the strip
 == static void doinsert(register struct parse *p, sop op, size_t opnd, sopno pos);
 */
static void
doinsert(
register struct parse *p,
sop op,
size_t opnd,
sopno pos )
{
    register sopno sn;
    register sop s;
    register int i;

    /* avoid making error situations worse */
    if (p->error != 0)
        return;

    sn = HERE();
    EMIT(op, opnd);     /* do checks, ensure space */
    assert(HERE() == sn+1);
    s = p->strip[sn];

    /* adjust paren pointers */
    assert(pos > 0);
    for (i = 1; i < NPAREN; i++) {
        if (p->pbegin[i] >= pos) {
            p->pbegin[i]++;
        }
        if (p->pend[i] >= pos) {
            p->pend[i]++;
        }
    }

    memmove((char *)&p->strip[pos+1], (char *)&p->strip[pos],
                        (HERE()-pos-1)*sizeof(sop));
    p->strip[pos] = s;
}

/*
 - dofwd - complete a forward reference
 == static void dofwd(register struct parse *p, sopno pos, sop value);
 */
static void
dofwd(
register struct parse *p,
register sopno pos,
sop value )
{
    /* avoid making error situations worse */
    if (p->error != 0)
        return;

    assert(value < 1<<OPSHIFT);
    p->strip[pos] = OP(p->strip[pos]) | value;
}

/*
 - enlarge - enlarge the strip
 == static void enlarge(register struct parse *p, sopno size);
 */
static void
enlarge(
register struct parse *p,
register sopno size )
{
    register sop *sp;

    if (p->ssize >= size)
        return;

    sp = (sop *)realloc(p->strip, size*sizeof(sop));
    if (sp == NULL) {
        SETERROR(REG_ESPACE);
        return;
    }
    p->strip = sp;
    p->ssize = size;
}

/*
 - stripsnug - compact the strip
 == static void stripsnug(register struct parse *p, register struct re_guts *g);
 */
static void
stripsnug(
register struct parse *p,
register struct re_guts *g )
{
    g->nstates = p->slen;
    g->strip = (sop *)realloc((char *)p->strip, p->slen * sizeof(sop));
    if (g->strip == NULL) {
        SETERROR(REG_ESPACE);
        g->strip = p->strip;
    }
}

/*
 - findmust - fill in must and mlen with longest mandatory literal string
 == static void findmust(register struct parse *p, register struct re_guts *g);
 *
 * This algorithm could do fancy things like analyzing the operands of |
 * for common subsequences.  Someday.  This code is simple and finds most
 * of the interesting cases.
 *
 * Note that must and mlen got initialized during setup.
 */
static void
findmust(
struct parse *p,
register struct re_guts *g )
{
    register sop *scan;
    sop *start;
    register sop *newstart;
    register sopno newlen;
    register sop s;
    register char *cp;
    register sopno i;

    /* avoid making error situations worse */
    if (p->error != 0)
        return;

    /* find the longest OCHAR sequence in strip */
    newlen = 0;
    scan = g->strip + 1;
    do {
        s = *scan++;
        switch (OP(s)) {
        case OCHAR:     /* sequence member */
            if (newlen == 0)        /* new sequence */
                newstart = scan - 1;
            newlen++;
            break;
        case OPLUS_:        /* things that don't break one */
        case OLPAREN:
        case ORPAREN:
            break;
        case OQUEST_:       /* things that must be skipped */
        case OCH_:
            scan--;
            do {
                scan += OPND(s);
                s = *scan;
                /* assert() interferes w debug printouts */
                if (OP(s) != O_QUEST && OP(s) != O_CH &&
                            OP(s) != OOR2) {
                    g->iflags |= BAD;
                    return;
                }
            } while (OP(s) != O_QUEST && OP(s) != O_CH);
            /* fallthrough */
        default:        /* things that break a sequence */
            if (newlen > g->mlen) {     /* ends one */
                start = newstart;
                g->mlen = newlen;
            }
            newlen = 0;
            break;
        }
    } while (OP(s) != OEND);

    if (g->mlen == 0)       /* there isn't one */
        return;

    /* turn it into a character string */
    g->must = (char *) malloc((size_t)g->mlen + 1);
    if (g->must == NULL) {      /* argh; just forget it */
        g->mlen = 0;
        return;
    }
    cp = g->must;
    scan = start;
    for (i = g->mlen; i > 0; i--) {
        while (OP(s = *scan++) != OCHAR)
            continue;
        assert(cp < g->must + g->mlen);
        *cp++ = (char)OPND(s);
    }
    assert(cp == g->must + g->mlen);
    *cp/*++*/ = '\0';       /* just on general principles */
}

/*
 - pluscount - count + nesting
 == static sopno pluscount(register struct parse *p, register struct re_guts *g);
 */
static sopno            /* nesting depth */
pluscount(
struct parse *p,
register struct re_guts *g )
{
    register sop *scan;
    register sop s;
    register sopno plusnest = 0;
    register sopno maxnest = 0;

    if (p->error != 0)
        return(0);  /* there may not be an OEND */

    scan = g->strip + 1;
    do {
        s = *scan++;
        switch (OP(s)) {
        case OPLUS_:
            plusnest++;
            break;
        case O_PLUS:
            if (plusnest > maxnest)
                maxnest = plusnest;
            plusnest--;
            break;
        }
    } while (OP(s) != OEND);
    if (plusnest != 0)
        g->iflags |= BAD;
    return(maxnest);
}
/*
 * the outer shell of regexec()
 *
 * This file includes engine.c *twice*, after muchos fiddling with the
 * macros that code uses.  This lets the same code operate on two different
 * representations for state sets.
 */

// See above.
#if 0
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "regex.h"

#include "utils.h"
#include "regex2.h"
#endif

static int nope = 0;		/* for use in asserts; shuts lint up */

/* macros for manipulating states, small version */
#define	states	unsigned
#define	states1	unsigned	/* for later use in regexec() decision */
#define	CLEAR(v)	((v) = 0)
#define	SET0(v, n)	((v) &= ~((unsigned)1 << (n)))
#define	SET1(v, n)	((v) |= (unsigned)1 << (n))
#define	ISSET(v, n)	((v) & ((unsigned)1 << (n)))
#define	ASSIGN(d, s)	((d) = (s))
#define	EQ(a, b)	((a) == (b))
#define	STATEVARS	int dummy	/* dummy version */
#define	STATESETUP(m, n)	/* nothing */
#define	STATETEARDOWN(m)	/* nothing */
#define	SETUP(v)	((v) = 0)
#define	onestate	unsigned
#define	INIT(o, n)	((o) = (unsigned)1 << (n))
#define	INC(o)	((o) <<= 1)
#define	ISSTATEIN(v, o)	((v) & (o))
/* some abbreviations; note that some of these know variable names! */
/* do "if I'm here, I can also be there" etc without branches */
#define	FWD(dst, src, n)	((dst) |= ((unsigned)(src)&(here)) << (n))
#define	BACK(dst, src, n)	((dst) |= ((unsigned)(src)&(here)) >> (n))
#define	ISSETBACK(v, n)	((v) & ((unsigned)here >> (n)))
/* function names */
#define SNAMES			/* engine.c looks after details */

/* NOT using small model
#include "engine.c"
*/

/* now undo things */
#undef	states
#undef	CLEAR
#undef	SET0
#undef	SET1
#undef	ISSET
#undef	ASSIGN
#undef	EQ
#undef	STATEVARS
#undef	STATESETUP
#undef	STATETEARDOWN
#undef	SETUP
#undef	onestate
#undef	INIT
#undef	INC
#undef	ISSTATEIN
#undef	FWD
#undef	BACK
#undef	ISSETBACK
#undef	SNAMES

/* macros for manipulating states, large version */
#define	states	char *
#define	CLEAR(v)	memset(v, 0, m->g->nstates)
#define	SET0(v, n)	((v)[n] = 0)
#define	SET1(v, n)	((v)[n] = 1)
#define	ISSET(v, n)	((v)[n])
#define	ASSIGN(d, s)	memcpy(d, s, m->g->nstates)
#define	EQ(a, b)	(memcmp(a, b, m->g->nstates) == 0)
#define	STATEVARS	int vn; char *space
#define STATESETUP(m, nv)   { (m)->space = (char *) malloc((nv)*(m)->g->nstates); \
				if ((m)->space == NULL) return(REG_ESPACE); \
				(m)->vn = 0; }
#define	STATETEARDOWN(m)	{ free((m)->space); }
#define	SETUP(v)	((v) = &m->space[m->vn++ * m->g->nstates])
#define	onestate	int
#define	INIT(o, n)	((o) = (n))
#define	INC(o)	((o)++)
#define	ISSTATEIN(v, o)	((v)[o])
/* some abbreviations; note that some of these know variable names! */
/* do "if I'm here, I can also be there" etc without branches */
#define	FWD(dst, src, n)	((dst)[here+(n)] |= (src)[here])
#define	BACK(dst, src, n)	((dst)[here-(n)] |= (src)[here])
#define	ISSETBACK(v, n)	((v)[here - (n)])
/* function names */
#define	LNAMES			/* flag */

//#include "engine.c"
#ifdef SNAMES
#define matcher smatcher
#define fast    sfast
#define slow    sslow
#define dissect sdissect
#define backref sbackref
#define step    sstep
#define print   sprint
#define at  sat
#define match   smat
#endif
#ifdef LNAMES
#define matcher lmatcher
#define fast    lfast
#define slow    lslow
#define dissect ldissect
#define backref lbackref
#define step    lstep
#define print   lprint
#define at  lat
#define match   lmat
#endif

/* another structure passed up and down to avoid zillions of parameters */
struct match {
    struct re_guts *g;
    int eflags;
    regmatch_t *pmatch; /* [nsub+1] (0 element unused) */
    char *offp;     /* offsets work from here */
    char *beginp;       /* start of string -- virtual NUL precedes */
    char *endp;     /* end of string -- virtual NUL here */
    char *coldp;        /* can be no match starting before here */
    char **lastpos;     /* [nplus+1] */
    STATEVARS;
    states st;      /* current states */
    states fresh;       /* states for a fresh start */
    states tmp;     /* temporary */
    states empty;       /* empty set of states */
};

//#include "engine.ih"
#ifdef __cplusplus
extern "C" {
#endif

static int matcher(register struct re_guts *g, char *string, size_t nmatch, regmatch_t pmatch[], int eflags);
static char *dissect(register struct match *m, char *start, char *stop, sopno startst, sopno stopst);
static char *backref(register struct match *m, char *start, char *stop, sopno startst, sopno stopst, sopno lev);
static char *fast(register struct match *m, char *start, char *stop, sopno startst, sopno stopst);
static char *slow(register struct match *m, char *start, char *stop, sopno startst, sopno stopst);
static states step(register struct re_guts *g, sopno start, sopno stop, register states bef, int ch, register states aft);
#define BOL     (OUT+1)
#define EOL     (BOL+1)
#define BOLEOL  (BOL+2)
#define NOTHING (BOL+3)
#define BOW     (BOL+4)
#define EOW     (BOL+5)
#define CODEMAX (BOL+5)         /* highest code used */
#define NONCHAR(c)      ((c) > CHAR_MAX)
#define NNONCHAR        (CODEMAX-CHAR_MAX)
#ifdef REDEBUG
static void print(struct match *m, char *caption, states st, int ch, FILE *d);
#endif
#ifdef REDEBUG
static void at(struct match *m, char *title, char *start, char *stop, sopno startst, sopno stopst);
#endif
#ifdef REDEBUG
static char *pchar(int ch);
#endif

#ifdef __cplusplus
}
#endif

#ifdef REDEBUG
#define SP(t, s, c) print(m, t, s, c, stdout)
#define AT(t, p1, p2, s1, s2)   at(m, t, p1, p2, s1, s2)
#define NOTE(str)   { if (m->eflags&REG_TRACE) printf("=%s\n", (str)); }
#else
#define SP(t, s, c) /* nothing */
#define AT(t, p1, p2, s1, s2)   /* nothing */
#define NOTE(s) /* nothing */
#endif

/*
 - matcher - the actual matching engine
 == static int matcher(register struct re_guts *g, char *string, \
 == size_t nmatch, regmatch_t pmatch[], int eflags);
 */
static int          /* 0 success, REG_NOMATCH failure */
matcher(
register struct re_guts *g,
char *string,
size_t nmatch,
regmatch_t pmatch[],
int eflags )
{
    register char *endp;
    register int i;
    struct match mv;
    register struct match *m = &mv;
    register char *dp;
    const register sopno gf = g->firststate+1;  /* +1 for OEND */
    const register sopno gl = g->laststate;
    char *start;
    char *stop;

    /* simplify the situation where possible */
    if (g->cflags&REG_NOSUB)
        nmatch = 0;
    if (eflags&REG_STARTEND) {
        start = string + pmatch[0].rm_so;
        stop = string + pmatch[0].rm_eo;
    } else {
        start = string;
        stop = start + strlen(start);
    }
    if (stop < start)
        return(REG_INVARG);

    /* prescreening; this does wonders for this rather slow code */
    if (g->must != NULL) {
        for (dp = start; dp < stop; dp++)
            if (*dp == g->must[0] && stop - dp >= g->mlen &&
                memcmp(dp, g->must, (size_t)g->mlen) == 0)
                break;
        if (dp == stop)     /* we didn't find g->must */
            return(REG_NOMATCH);
    }

    /* match struct setup */
    m->g = g;
    m->eflags = eflags;
    m->pmatch = NULL;
    m->lastpos = NULL;
    m->offp = string;
    m->beginp = start;
    m->endp = stop;
    STATESETUP(m, 4);
    SETUP(m->st);
    SETUP(m->fresh);
    SETUP(m->tmp);
    SETUP(m->empty);
    CLEAR(m->empty);

    /* this loop does only one repetition except for backrefs */
    for (;;) {
        endp = fast(m, start, stop, gf, gl);
        if (endp == NULL) {     /* a miss */
            STATETEARDOWN(m);
            return(REG_NOMATCH);
        }
        if (nmatch == 0 && !g->backrefs)
            break;      /* no further info needed */

        /* where? */
        assert(m->coldp != NULL);
        for (;;) {
            NOTE("finding start");
            endp = slow(m, m->coldp, stop, gf, gl);
            if (endp != NULL)
                break;
            assert(m->coldp < m->endp);
            m->coldp++;
        }
        if (nmatch == 1 && !g->backrefs)
            break;      /* no further info needed */

        /* oh my, he wants the subexpressions... */
        if (m->pmatch == NULL)
            m->pmatch = (regmatch_t *)malloc((m->g->nsub + 1) *
                            sizeof(regmatch_t));
        if (m->pmatch == NULL) {
            STATETEARDOWN(m);
            return(REG_ESPACE);
        }
        for (i = 1; (size_t) i <= m->g->nsub; i++)
            m->pmatch[i].rm_so = m->pmatch[i].rm_eo = -1;
        if (!g->backrefs && !(m->eflags&REG_BACKR)) {
            NOTE("dissecting");
            dp = dissect(m, m->coldp, endp, gf, gl);
        } else {
            if (g->nplus > 0 && m->lastpos == NULL)
                m->lastpos = (char **)malloc((g->nplus+1) *
                            sizeof(char *));
            if (g->nplus > 0 && m->lastpos == NULL) {
                free(m->pmatch);
                STATETEARDOWN(m);
                return(REG_ESPACE);
            }
            NOTE("backref dissect");
            dp = backref(m, m->coldp, endp, gf, gl, (sopno)0);
        }
        if (dp != NULL)
            break;

        /* uh-oh... we couldn't find a subexpression-level match */
        assert(g->backrefs);    /* must be back references doing it */
        assert(g->nplus == 0 || m->lastpos != NULL);
        for (;;) {
            if (dp != NULL || endp <= m->coldp)
                break;      /* defeat */
            NOTE("backoff");
            endp = slow(m, m->coldp, endp-1, gf, gl);
            if (endp == NULL)
                break;      /* defeat */
            /* try it on a shorter possibility */
#ifndef NDEBUG
            for (i = 1; i <= m->g->nsub; i++) {
                assert(m->pmatch[i].rm_so == -1);
                assert(m->pmatch[i].rm_eo == -1);
            }
#endif
            NOTE("backoff dissect");
            dp = backref(m, m->coldp, endp, gf, gl, (sopno)0);
        }
        assert(dp == NULL || dp == endp);
        if (dp != NULL)     /* found a shorter one */
            break;

        /* despite initial appearances, there is no match here */
        NOTE("false alarm");
        start = m->coldp + 1;   /* recycle starting later */
        assert(start <= stop);
    }

    /* fill in the details if requested */
    if (nmatch > 0) {
        pmatch[0].rm_so = m->coldp - m->offp;
        pmatch[0].rm_eo = endp - m->offp;
    }
    if (nmatch > 1) {
        assert(m->pmatch != NULL);
        for (i = 1; (size_t) i < nmatch; i++)
            if ((size_t) i <= m->g->nsub)
                pmatch[i] = m->pmatch[i];
            else {
                pmatch[i].rm_so = -1;
                pmatch[i].rm_eo = -1;
            }
    }

    if (m->pmatch != NULL)
        free((char *)m->pmatch);
    if (m->lastpos != NULL)
        free((char *)m->lastpos);
    STATETEARDOWN(m);
    return(0);
}

/*
 - dissect - figure out what matched what, no back references
 == static char *dissect(register struct match *m, char *start, \
 == char *stop, sopno startst, sopno stopst);
 */
static char *           /* == stop (success) always */
dissect(
register struct match *m,
char *start,
char *stop,
sopno startst,
sopno stopst )
{
    register int i;
    register sopno ss;  /* start sop of current subRE */
    register sopno es;  /* end sop of current subRE */
    register char *sp;  /* start of string matched by it */
    register char *stp; /* string matched by it cannot pass here */
    register char *rest;    /* start of rest of string */
    register char *tail;    /* string unmatched by rest of RE */
    register sopno ssub;    /* start sop of subsubRE */
    register sopno esub;    /* end sop of subsubRE */
    register char *ssp; /* start of string matched by subsubRE */
    register char *sep; /* end of string matched by subsubRE */
    register char *oldssp;  /* previous ssp */
    register char *dp;

    AT("diss", start, stop, startst, stopst);
    sp = start;
    for (ss = startst; ss < stopst; ss = es) {
        /* identify end of subRE */
        es = ss;
        switch (OP(m->g->strip[es])) {
        case OPLUS_:
        case OQUEST_:
            es += OPND(m->g->strip[es]);
            break;
        case OCH_:
            while (OP(m->g->strip[es]) != O_CH)
                es += OPND(m->g->strip[es]);
            break;
        }
        es++;

        /* figure out what it matched */
        switch (OP(m->g->strip[ss])) {
        case OEND:
            assert(nope);
            break;
        case OCHAR:
            sp++;
            break;
        case OBOL:
        case OEOL:
        case OBOW:
        case OEOW:
            break;
        case OANY:
        case OANYOF:
            sp++;
            break;
        case OBACK_:
        case O_BACK:
            assert(nope);
            break;
        /* cases where length of match is hard to find */
        case OQUEST_:
            stp = stop;
            for (;;) {
                /* how long could this one be? */
                rest = slow(m, sp, stp, ss, es);
                assert(rest != NULL);   /* it did match */
                /* could the rest match the rest? */
                tail = slow(m, rest, stop, es, stopst);
                if (tail == stop)
                    break;      /* yes! */
                /* no -- try a shorter match for this one */
                stp = rest - 1;
                assert(stp >= sp);  /* it did work */
            }
            ssub = ss + 1;
            esub = es - 1;
            /* did innards match? */
            if (slow(m, sp, rest, ssub, esub) != NULL) {
                dp = dissect(m, sp, rest, ssub, esub);
                assert(dp == rest);
            } else      /* no */
                assert(sp == rest);
            sp = rest;
            break;
        case OPLUS_:
            stp = stop;
            for (;;) {
                /* how long could this one be? */
                rest = slow(m, sp, stp, ss, es);
                assert(rest != NULL);   /* it did match */
                /* could the rest match the rest? */
                tail = slow(m, rest, stop, es, stopst);
                if (tail == stop)
                    break;      /* yes! */
                /* no -- try a shorter match for this one */
                stp = rest - 1;
                assert(stp >= sp);  /* it did work */
            }
            ssub = ss + 1;
            esub = es - 1;
            ssp = sp;
            oldssp = ssp;
            for (;;) {  /* find last match of innards */
                sep = slow(m, ssp, rest, ssub, esub);
                if (sep == NULL || sep == ssp)
                    break;  /* failed or matched null */
                oldssp = ssp;   /* on to next try */
                ssp = sep;
            }
            if (sep == NULL) {
                /* last successful match */
                sep = ssp;
                ssp = oldssp;
            }
            assert(sep == rest);    /* must exhaust substring */
            assert(slow(m, ssp, sep, ssub, esub) == rest);
            dp = dissect(m, ssp, sep, ssub, esub);
            assert(dp == sep);
            sp = rest;
            break;
        case OCH_:
            stp = stop;
            for (;;) {
                /* how long could this one be? */
                rest = slow(m, sp, stp, ss, es);
                assert(rest != NULL);   /* it did match */
                /* could the rest match the rest? */
                tail = slow(m, rest, stop, es, stopst);
                if (tail == stop)
                    break;      /* yes! */
                /* no -- try a shorter match for this one */
                stp = rest - 1;
                assert(stp >= sp);  /* it did work */
            }
            ssub = ss + 1;
            esub = ss + OPND(m->g->strip[ss]) - 1;
            assert(OP(m->g->strip[esub]) == OOR1);
            for (;;) {  /* find first matching branch */
                if (slow(m, sp, rest, ssub, esub) == rest)
                    break;  /* it matched all of it */
                /* that one missed, try next one */
                assert(OP(m->g->strip[esub]) == OOR1);
                esub++;
                assert(OP(m->g->strip[esub]) == OOR2);
                ssub = esub + 1;
                esub += OPND(m->g->strip[esub]);
                if (OP(m->g->strip[esub]) == OOR2)
                    esub--;
                else
                    assert(OP(m->g->strip[esub]) == O_CH);
            }
            dp = dissect(m, sp, rest, ssub, esub);
            assert(dp == rest);
            sp = rest;
            break;
        case O_PLUS:
        case O_QUEST:
        case OOR1:
        case OOR2:
        case O_CH:
            assert(nope);
            break;
        case OLPAREN:
            i = OPND(m->g->strip[ss]);
            assert(0 < i && (size_t) i <= m->g->nsub);
            m->pmatch[i].rm_so = sp - m->offp;
            break;
        case ORPAREN:
            i = OPND(m->g->strip[ss]);
            assert(0 < i && (size_t) i <= m->g->nsub);
            m->pmatch[i].rm_eo = sp - m->offp;
            break;
        default:        /* uh oh */
            assert(nope);
            break;
        }
    }

    assert(sp == stop);
    return(sp);
}

/*
 - backref - figure out what matched what, figuring in back references
 == static char *backref(register struct match *m, char *start, \
 == char *stop, sopno startst, sopno stopst, sopno lev);
 */
static char *           /* == stop (success) or NULL (failure) */
backref(
register struct match *m,
char *start,
char *stop,
sopno startst,
sopno stopst,
sopno lev )          /* PLUS nesting level */
{
    register int i;
    register sopno ss;  /* start sop of current subRE */
    register char *sp;  /* start of string matched by it */
    register sopno ssub;    /* start sop of subsubRE */
    register sopno esub;    /* end sop of subsubRE */
    register char *ssp; /* start of string matched by subsubRE */
    register char *dp;
    register size_t len;
    register int hard;
    register sop s;
    register regoff_t offsave;
    register cset *cs;

    AT("back", start, stop, startst, stopst);
    sp = start;

    /* get as far as we can with easy stuff */
    hard = 0;
    for (ss = startst; !hard && ss < stopst; ss++)
        switch (OP(s = m->g->strip[ss])) {
        case OCHAR:
            if (sp == stop || *sp++ != (char)OPND(s))
                return(NULL);
            break;
        case OANY:
            if (sp == stop)
                return(NULL);
            sp++;
            break;
        case OANYOF:
            cs = &m->g->sets[OPND(s)];
            if (sp == stop || !CHIN(cs, *sp++))
                return(NULL);
            break;
        case OBOL:
            if ( (sp == m->beginp && !(m->eflags&REG_NOTBOL)) ||
                    (sp < m->endp && *(sp-1) == '\n' &&
                        (m->g->cflags&REG_NEWLINE)) )
                { /* yes */ }
            else
                return(NULL);
            break;
        case OEOL:
            if ( (sp == m->endp && !(m->eflags&REG_NOTEOL)) ||
                    (sp < m->endp && *sp == '\n' &&
                        (m->g->cflags&REG_NEWLINE)) )
                { /* yes */ }
            else
                return(NULL);
            break;
        case OBOW:
            if (( (sp == m->beginp && !(m->eflags&REG_NOTBOL)) ||
                    (sp < m->endp && *(sp-1) == '\n' &&
                        (m->g->cflags&REG_NEWLINE)) ||
                    (sp > m->beginp &&
                            !ISWORD(*(sp-1))) ) &&
                    (sp < m->endp && ISWORD(*sp)) )
                { /* yes */ }
            else
                return(NULL);
            break;
        case OEOW:
            if (( (sp == m->endp && !(m->eflags&REG_NOTEOL)) ||
                    (sp < m->endp && *sp == '\n' &&
                        (m->g->cflags&REG_NEWLINE)) ||
                    (sp < m->endp && !ISWORD(*sp)) ) &&
                    (sp > m->beginp && ISWORD(*(sp-1))) )
                { /* yes */ }
            else
                return(NULL);
            break;
        case O_QUEST:
            break;
        case OOR1:  /* matches null but needs to skip */
            ss++;
            s = m->g->strip[ss];
            do {
                assert(OP(s) == OOR2);
                ss += OPND(s);
            } while (OP(s = m->g->strip[ss]) != O_CH);
            /* note that the ss++ gets us past the O_CH */
            break;
        default:    /* have to make a choice */
            hard = 1;
            break;
        }
    if (!hard) {        /* that was it! */
        if (sp != stop)
            return(NULL);
        return(sp);
    }
    ss--;           /* adjust for the for's final increment */

    /* the hard stuff */
    AT("hard", sp, stop, ss, stopst);
    s = m->g->strip[ss];
    switch (OP(s)) {
    case OBACK_:        /* the vilest depths */
        i = OPND(s);
        assert(0 < i && (size_t) i <= m->g->nsub);
        if (m->pmatch[i].rm_eo == -1)
            return(NULL);
        assert(m->pmatch[i].rm_so != -1);
        len = m->pmatch[i].rm_eo - m->pmatch[i].rm_so;
        assert((size_t) ( stop - m->beginp ) >= len);
        if (sp > stop - len)
            return(NULL);   /* not enough left to match */
        ssp = m->offp + m->pmatch[i].rm_so;
        if (memcmp(sp, ssp, len) != 0)
            return(NULL);
        while (m->g->strip[ss] != SOP(O_BACK, i))
            ss++;
        return(backref(m, sp+len, stop, ss+1, stopst, lev));
        //break;
    case OQUEST_:       /* to null or not */
        dp = backref(m, sp, stop, ss+1, stopst, lev);
        if (dp != NULL)
            return(dp); /* not */
        return(backref(m, sp, stop, ss+OPND(s)+1, stopst, lev));
        //break;
    case OPLUS_:
        assert(m->lastpos != NULL);
        assert(lev+1 <= m->g->nplus);
        m->lastpos[lev+1] = sp;
        return(backref(m, sp, stop, ss+1, stopst, lev+1));
        //break;
    case O_PLUS:
        if (sp == m->lastpos[lev])  /* last pass matched null */
            return(backref(m, sp, stop, ss+1, stopst, lev-1));
        /* try another pass */
        m->lastpos[lev] = sp;
        dp = backref(m, sp, stop, ss-OPND(s)+1, stopst, lev);
        if (dp == NULL)
            return(backref(m, sp, stop, ss+1, stopst, lev-1));
        else
            return(dp);
        //break;
    case OCH_:      /* find the right one, if any */
        ssub = ss + 1;
        esub = ss + OPND(s) - 1;
        assert(OP(m->g->strip[esub]) == OOR1);
        for (;;) {  /* find first matching branch */
            dp = backref(m, sp, stop, ssub, esub, lev);
            if (dp != NULL)
                return(dp);
            /* that one missed, try next one */
            if (OP(m->g->strip[esub]) == O_CH)
                return(NULL);   /* there is none */
            esub++;
            assert(OP(m->g->strip[esub]) == OOR2);
            ssub = esub + 1;
            esub += OPND(m->g->strip[esub]);
            if (OP(m->g->strip[esub]) == OOR2)
                esub--;
            else
                assert(OP(m->g->strip[esub]) == O_CH);
        }
        //break;
    case OLPAREN:       /* must undo assignment if rest fails */
        i = OPND(s);
        assert(0 < i && (size_t) i <= m->g->nsub);
        offsave = m->pmatch[i].rm_so;
        m->pmatch[i].rm_so = sp - m->offp;
        dp = backref(m, sp, stop, ss+1, stopst, lev);
        if (dp != NULL)
            return(dp);
        m->pmatch[i].rm_so = offsave;
        return(NULL);
        //break;
    case ORPAREN:       /* must undo assignment if rest fails */
        i = OPND(s);
        assert(0 < i && (size_t) i <= m->g->nsub);
        offsave = m->pmatch[i].rm_eo;
        m->pmatch[i].rm_eo = sp - m->offp;
        dp = backref(m, sp, stop, ss+1, stopst, lev);
        if (dp != NULL)
            return(dp);
        m->pmatch[i].rm_eo = offsave;
        return(NULL);
        //break;
    default:        /* uh oh */
        assert(nope);
        break;
    }

    /* "can't happen" */
    assert(nope);
    /* NOTREACHED */
    return((char *)NULL);   /* dummy */
}

/*
 - fast - step through the string at top speed
 == static char *fast(register struct match *m, char *start, \
 == char *stop, sopno startst, sopno stopst);
 */
static char *           /* where tentative match ended, or NULL */
fast(
register struct match *m,
char *start,
char *stop,
sopno startst,
sopno stopst )
{
    register states st = m->st;
    register states fresh = m->fresh;
    register states tmp = m->tmp;
    register char *p = start;
    register int c = (start == m->beginp) ? OUT : *(start-1);
    register int lastc; /* previous c */
    register int flagch;
    register int i;
    register char *coldp;   /* last p after which no match was underway */

    CLEAR(st);
    SET1(st, startst);
    st = step(m->g, startst, stopst, st, NOTHING, st);
    ASSIGN(fresh, st);
    SP("start", st, *p);
    coldp = NULL;
    for (;;) {
        /* next character */
        lastc = c;
        c = (p == m->endp) ? OUT : *p;
        if (EQ(st, fresh))
            coldp = p;

        /* is there an EOL and/or BOL between lastc and c? */
        flagch = '\0';
        i = 0;
        if ( (lastc == '\n' && m->g->cflags&REG_NEWLINE) ||
                (lastc == OUT && !(m->eflags&REG_NOTBOL)) ) {
            flagch = BOL;
            i = m->g->nbol;
        }
        if ( (c == '\n' && m->g->cflags&REG_NEWLINE) ||
                (c == OUT && !(m->eflags&REG_NOTEOL)) ) {
            flagch = (flagch == BOL) ? BOLEOL : EOL;
            i += m->g->neol;
        }
        if (i != 0) {
            for (; i > 0; i--)
                st = step(m->g, startst, stopst, st, flagch, st);
            SP("boleol", st, c);
        }

        /* how about a word boundary? */
        if ( (flagch == BOL || (lastc != OUT && !ISWORD(lastc))) &&
                    (c != OUT && ISWORD(c)) ) {
            flagch = BOW;
        }
        if ( (lastc != OUT && ISWORD(lastc)) &&
                (flagch == EOL || (c != OUT && !ISWORD(c))) ) {
            flagch = EOW;
        }
        if (flagch == BOW || flagch == EOW) {
            st = step(m->g, startst, stopst, st, flagch, st);
            SP("boweow", st, c);
        }

        /* are we done? */
        if (ISSET(st, stopst) || p == stop)
            break;      /* NOTE BREAK OUT */

        /* no, we must deal with this character */
        ASSIGN(tmp, st);
        ASSIGN(st, fresh);
        assert(c != OUT);
        st = step(m->g, startst, stopst, tmp, c, st);
        SP("aft", st, c);
        assert(EQ(step(m->g, startst, stopst, st, NOTHING, st), st));
        p++;
    }

    assert(coldp != NULL);
    m->coldp = coldp;
    if (ISSET(st, stopst))
        return(p+1);
    else
        return(NULL);
}

/*
 - slow - step through the string more deliberately
 == static char *slow(register struct match *m, char *start, \
 == char *stop, sopno startst, sopno stopst);
 */
static char *           /* where it ended */
slow(
register struct match *m,
char *start,
char *stop,
sopno startst,
sopno stopst )
{
    register states st = m->st;
    register states empty = m->empty;
    register states tmp = m->tmp;
    register char *p = start;
    register int c = (start == m->beginp) ? OUT : *(start-1);
    register int lastc; /* previous c */
    register int flagch;
    register int i;
    register char *matchp;  /* last p at which a match ended */

    AT("slow", start, stop, startst, stopst);
    CLEAR(st);
    SET1(st, startst);
    SP("sstart", st, *p);
    st = step(m->g, startst, stopst, st, NOTHING, st);
    matchp = NULL;
    for (;;) {
        /* next character */
        lastc = c;
        c = (p == m->endp) ? OUT : *p;

        /* is there an EOL and/or BOL between lastc and c? */
        flagch = '\0';
        i = 0;
        if ( (lastc == '\n' && m->g->cflags&REG_NEWLINE) ||
                (lastc == OUT && !(m->eflags&REG_NOTBOL)) ) {
            flagch = BOL;
            i = m->g->nbol;
        }
        if ( (c == '\n' && m->g->cflags&REG_NEWLINE) ||
                (c == OUT && !(m->eflags&REG_NOTEOL)) ) {
            flagch = (flagch == BOL) ? BOLEOL : EOL;
            i += m->g->neol;
        }
        if (i != 0) {
            for (; i > 0; i--)
                st = step(m->g, startst, stopst, st, flagch, st);
            SP("sboleol", st, c);
        }

        /* how about a word boundary? */
        if ( (flagch == BOL || (lastc != OUT && !ISWORD(lastc))) &&
                    (c != OUT && ISWORD(c)) ) {
            flagch = BOW;
        }
        if ( (lastc != OUT && ISWORD(lastc)) &&
                (flagch == EOL || (c != OUT && !ISWORD(c))) ) {
            flagch = EOW;
        }
        if (flagch == BOW || flagch == EOW) {
            st = step(m->g, startst, stopst, st, flagch, st);
            SP("sboweow", st, c);
        }

        /* are we done? */
        if (ISSET(st, stopst))
            matchp = p;
        if (EQ(st, empty) || p == stop)
            break;      /* NOTE BREAK OUT */

        /* no, we must deal with this character */
        ASSIGN(tmp, st);
        ASSIGN(st, empty);
        assert(c != OUT);
        st = step(m->g, startst, stopst, tmp, c, st);
        SP("saft", st, c);
        assert(EQ(step(m->g, startst, stopst, st, NOTHING, st), st));
        p++;
    }

    return(matchp);
}


/*
 - step - map set of states reachable before char to set reachable after
 == static states step(register struct re_guts *g, sopno start, sopno stop, \
 == register states bef, int ch, register states aft);
 == #define BOL (OUT+1)
 == #define EOL (BOL+1)
 == #define BOLEOL  (BOL+2)
 == #define NOTHING (BOL+3)
 == #define BOW (BOL+4)
 == #define EOW (BOL+5)
 == #define CODEMAX (BOL+5)     // highest code used
 == #define NONCHAR(c)  ((c) > CHAR_MAX)
 == #define NNONCHAR    (CODEMAX-CHAR_MAX)
 */
static states
step(
register struct re_guts *g,
sopno start,            /* start state within strip */
sopno stop,         /* state after stop state within strip */
register states bef,        /* states reachable before */
int ch,             /* character or NONCHAR code */
register states aft )        /* states already known reachable after */
{
    register cset *cs;
    register sop s;
    register sopno pc;
    register onestate here;     /* note, macros know this name */
    register sopno look;
    register long i;

    for (pc = start, INIT(here, pc); pc != stop; pc++, INC(here)) {
        s = g->strip[pc];
        switch (OP(s)) {
        case OEND:
            assert(pc == stop-1);
            break;
        case OCHAR:
            /* only characters can match */
            assert(!NONCHAR(ch) || ch != (char)OPND(s));
            if (ch == (char)OPND(s))
                FWD(aft, bef, 1);
            break;
        case OBOL:
            if (ch == BOL || ch == BOLEOL)
                FWD(aft, bef, 1);
            break;
        case OEOL:
            if (ch == EOL || ch == BOLEOL)
                FWD(aft, bef, 1);
            break;
        case OBOW:
            if (ch == BOW)
                FWD(aft, bef, 1);
            break;
        case OEOW:
            if (ch == EOW)
                FWD(aft, bef, 1);
            break;
        case OANY:
            if (!NONCHAR(ch))
                FWD(aft, bef, 1);
            break;
        case OANYOF:
            cs = &g->sets[OPND(s)];
            if (!NONCHAR(ch) && CHIN(cs, ch))
                FWD(aft, bef, 1);
            break;
        case OBACK_:        /* ignored here */
        case O_BACK:
            FWD(aft, aft, 1);
            break;
        case OPLUS_:        /* forward, this is just an empty */
            FWD(aft, aft, 1);
            break;
        case O_PLUS:        /* both forward and back */
            FWD(aft, aft, 1);
            i = ISSETBACK(aft, OPND(s));
            BACK(aft, aft, OPND(s));
            if (!i && ISSETBACK(aft, OPND(s))) {
                /* oho, must reconsider loop body */
                pc -= OPND(s) + 1;
                INIT(here, pc);
            }
            break;
        case OQUEST_:       /* two branches, both forward */
            FWD(aft, aft, 1);
            FWD(aft, aft, OPND(s));
            break;
        case O_QUEST:       /* just an empty */
            FWD(aft, aft, 1);
            break;
        case OLPAREN:       /* not significant here */
        case ORPAREN:
            FWD(aft, aft, 1);
            break;
        case OCH_:      /* mark the first two branches */
            FWD(aft, aft, 1);
            assert(OP(g->strip[pc+OPND(s)]) == OOR2);
            FWD(aft, aft, OPND(s));
            break;
        case OOR1:      /* done a branch, find the O_CH */
            if (ISSTATEIN(aft, here)) {
                for (look = 1;
                        OP(s = g->strip[pc+look]) != O_CH;
                        look += OPND(s))
                    assert(OP(s) == OOR2);
                FWD(aft, aft, look);
            }
            break;
        case OOR2:      /* propagate OCH_'s marking */
            FWD(aft, aft, 1);
            if (OP(g->strip[pc+OPND(s)]) != O_CH) {
                assert(OP(g->strip[pc+OPND(s)]) == OOR2);
                FWD(aft, aft, OPND(s));
            }
            break;
        case O_CH:      /* just empty */
            FWD(aft, aft, 1);
            break;
        default:        /* ooooops... */
            assert(nope);
            break;
        }
    }

    return(aft);
}

#ifdef REDEBUG
/*
 - print - print a set of states
 == #ifdef REDEBUG
 == static void print(struct match *m, char *caption, states st, \
 == int ch, FILE *d);
 == #endif
 */
static void
print(
struct match *m,
char *caption,
states st,
int ch,
FILE *d )
{
    register struct re_guts *g = m->g;
    register int i;
    register int first = 1;

    if (!(m->eflags&REG_TRACE))
        return;

    fprintf(d, "%s", caption);
    if (ch != '\0')
        fprintf(d, " %s", pchar(ch));
    for (i = 0; i < g->nstates; i++)
        if (ISSET(st, i)) {
            fprintf(d, "%s%d", (first) ? "\t" : ", ", i);
            first = 0;
        }
    fprintf(d, "\n");
}

/*
 - at - print current situation
 == #ifdef REDEBUG
 == static void at(struct match *m, char *title, char *start, char *stop, \
 ==                     sopno startst, sopno stopst);
 == #endif
 */
static void
at(
struct match *m,
char *title,
char *start,
char *stop,
sopno startst,
sopno stopst )
{
    if (!(m->eflags&REG_TRACE))
        return;

    printf("%s %s-", title, pchar(*start));
    printf("%s ", pchar(*stop));
    printf("%ld-%ld\n", (long)startst, (long)stopst);
}

#ifndef PCHARDONE
#define PCHARDONE   /* never again */
/*
 - pchar - make a character printable
 == #ifdef REDEBUG
 == static char *pchar(int ch);
 == #endif
 *
 * Is this identical to regchar() over in debug.c?  Well, yes.  But a
 * duplicate here avoids having a debugging-capable regexec.o tied to
 * a matching debug.o, and this is convenient.  It all disappears in
 * the non-debug compilation anyway, so it doesn't matter much.
 */
static char *           /* -> representation */
pchar(
int ch )
{
    static char pbuf[10];

    if (isprint(ch) || ch == ' ')
        sprintf(pbuf, "%c", ch);
    else
        sprintf(pbuf, "\\%o", ch);
    return(pbuf);
}
#endif
#endif

#undef  matcher
#undef  fast
#undef  slow
#undef  dissect
#undef  backref
#undef  step
#undef  print
#undef  at
#undef  match


/*
 - regexec - interface for matching
 = extern int regexec(const regex_t *, const char *, size_t, \
 =					regmatch_t [], int);
 = #define	REG_NOTBOL	00001
 = #define	REG_NOTEOL	00002
 = #define	REG_STARTEND	00004
 = #define	REG_TRACE	00400	// tracing of execution
 = #define	REG_LARGE	01000	// force large representation
 = #define	REG_BACKR	02000	// force use of backref code
 *
 * We put this here so we can exploit knowledge of the state representation
 * when choosing which matcher to call.  Also, by this point the matchers
 * have been prototyped.
 */
int				/* 0 success, REG_NOMATCH failure */
regexec(
const regex_t *preg,
const char *string,
size_t nmatch,
regmatch_t pmatch[],
int eflags )
{
	register struct re_guts *g = preg->re_g;

#ifdef REDEBUG
#   define  GOODEFLAGS(f)   (f)
#else
#   define  GOODEFLAGS(f)   ((f)&(REG_NOTBOL|REG_NOTEOL|REG_STARTEND))
#endif

	if (preg->re_magic != MAGIC1 || g->magic != MAGIC2)
		return(REG_BADPAT);
	assert(!(g->iflags&BAD));
	if (g->iflags&BAD)		/* backstop for no-debug case */
		return(REG_BADPAT);
    eflags = GOODEFLAGS(eflags);

    //if (g->nstates <= CHAR_BIT*sizeof(states1) && !(eflags&REG_LARGE))
    //   return(smatcher(g, (char *)string, nmatch, pmatch, eflags));
    //else
		return(lmatcher(g, (char *)string, nmatch, pmatch, eflags));
}

#if 0
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>

#include "regex.h"

#include "utils.h"
#endif

//#include "regerror.ih"
#ifdef __cplusplus
extern "C" {
#endif

static char *regatoi(const regex_t *preg, char *localbuf);

#ifdef __cplusplus
}
#endif

/*
 = #define  REG_OKAY     0
 = #define  REG_NOMATCH  1
 = #define  REG_BADPAT   2
 = #define  REG_ECOLLATE     3
 = #define  REG_ECTYPE   4
 = #define  REG_EESCAPE  5
 = #define  REG_ESUBREG  6
 = #define  REG_EBRACK   7
 = #define  REG_EPAREN   8
 = #define  REG_EBRACE   9
 = #define  REG_BADBR   10
 = #define  REG_ERANGE  11
 = #define  REG_ESPACE  12
 = #define  REG_BADRPT  13
 = #define  REG_EMPTY   14
 = #define  REG_ASSERT  15
 = #define  REG_INVARG  16
 = #define  REG_ATOI    255 // convert name to number (!)
 = #define  REG_ITOA    0400    // convert number to name (!)
 */
static struct rerr {
    int code;
    char *name;
    char *explain;
} rerrs[] = {
    REG_OKAY,   "REG_OKAY", "no errors detected",
    REG_NOMATCH,    "REG_NOMATCH",  "regexec() failed to match",
    REG_BADPAT, "REG_BADPAT",   "invalid regular expression",
    REG_ECOLLATE,   "REG_ECOLLATE", "invalid collating element",
    REG_ECTYPE, "REG_ECTYPE",   "invalid character class",
    REG_EESCAPE,    "REG_EESCAPE",  "trailing backslash (\\)",
    REG_ESUBREG,    "REG_ESUBREG",  "invalid backreference number",
    REG_EBRACK, "REG_EBRACK",   "brackets ([ ]) not balanced",
    REG_EPAREN, "REG_EPAREN",   "parentheses not balanced",
    REG_EBRACE, "REG_EBRACE",   "braces not balanced",
    REG_BADBR,  "REG_BADBR",    "invalid repetition count(s)",
    REG_ERANGE, "REG_ERANGE",   "invalid character range",
    REG_ESPACE, "REG_ESPACE",   "out of memory",
    REG_BADRPT, "REG_BADRPT",   "repetition-operator operand invalid",
    REG_EMPTY,  "REG_EMPTY",    "empty (sub)expression",
    REG_ASSERT, "REG_ASSERT",   "\"can't happen\" -- you found a bug",
    REG_INVARG, "REG_INVARG",   "invalid argument to regex routine",
    -1,     "",     "*** unknown regexp error code ***",
};

/*
 - regerror - the interface to error numbers
 = extern size_t regerror(int, const regex_t *, char *, size_t);
 */
/* ARGSUSED */
size_t
regerror(
int errcode,
const regex_t *preg,
char *errbuf,
size_t errbuf_size )
{
    register struct rerr *r;
    register size_t len;
    register int target = errcode &~ REG_ITOA;
    register char *s;
    char convbuf[50];

    if (errcode == REG_ATOI)
        s = regatoi(preg, convbuf);
    else {
        for (r = rerrs; r->code >= 0; r++)
            if (r->code == target)
                break;

        if (errcode&REG_ITOA) {
            if (r->code >= 0)
                (void) strcpy(convbuf, r->name);
            else
                sprintf(convbuf, "REG_0x%x", target);
            assert(strlen(convbuf) < sizeof(convbuf));
            s = convbuf;
        } else
            s = r->explain;
    }

    len = strlen(s) + 1;
    if (errbuf_size > 0) {
        if (errbuf_size > len)
            (void) strcpy(errbuf, s);
        else {
            (void) strncpy(errbuf, s, errbuf_size-1);
            errbuf[errbuf_size-1] = '\0';
        }
    }

    return(len);
}

/*
 - regatoi - internal routine to implement REG_ATOI
 == static char *regatoi(const regex_t *preg, char *localbuf);
 */
static char *
regatoi(
const regex_t *preg,
char *localbuf )
{
    register struct rerr *r;

    for (r = rerrs; r->code >= 0; r++)
        if (strcmp(r->name, preg->re_endp) == 0)
            break;
    if (r->code < 0)
        return("0");

    sprintf(localbuf, "%d", r->code);
    return(localbuf);
}

#if 0
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>

#include "regex.h"

#include "utils.h"
#include "regex2.h"
#endif

/*
 - regfree - free everything
 = extern void regfree(regex_t *);
 */
void
regfree(
regex_t *preg )
{
    register struct re_guts *g;

    if (preg->re_magic != MAGIC1)   /* oops */
        return;         /* nice to complain, but hard */

    g = preg->re_g;
    if (g == NULL || g->magic != MAGIC2)    /* oops again */
        return;
    preg->re_magic = 0;     /* mark it invalid */
    g->magic = 0;           /* mark it invalid */

    if (g->strip != NULL)
        free((char *)g->strip);
    if (g->sets != NULL)
        free((char *)g->sets);
    if (g->setbits != NULL)
        free((char *)g->setbits);
    if (g->must != NULL)
        free(g->must);
    free((char *)g);
}

#include "hbapi.h"

HB_FUNC( HB_ATX )
{
    #define REGEX_MAX_GROUPS 16
    regex_t re;
    regmatch_t aMatches[REGEX_MAX_GROUPS];
    int CFlags = REG_EXTENDED, EFlags = 0;//REG_BACKR;
    unsigned long ulLen;

    PHB_ITEM pRegEx = hb_param( 1, HB_IT_STRING );
    PHB_ITEM pString = hb_param( 2, HB_IT_STRING );
    PHB_ITEM pCaseSensitive = hb_param( 3, HB_IT_LOGICAL );
    PHB_ITEM pStart = hb_param( 4, HB_IT_INTEGER );
    PHB_ITEM pEnd = hb_param( 5, HB_IT_INTEGER );

    if( pCaseSensitive && pCaseSensitive->item.asLogical.value == FALSE )
    {
       CFlags |= REG_ICASE;
    }

    if( pRegEx && pString )
    {
        if( regcomp( &re, pRegEx->item.asString.value, CFlags ) == 0 )
        {
           if( pStart || pEnd )
           {
              EFlags |= REG_STARTEND;
              aMatches[0].rm_so = 0;
              aMatches[0].rm_eo = pString->item.asString.length;
           }

           if( pStart && pStart->item.asInteger.value > 0 && pStart->item.asInteger.value <= aMatches[0].rm_eo )
           {
              aMatches[0].rm_so = pStart->item.asInteger.value - 1;
           }

           if( pEnd && pEnd->item.asInteger.value > 0 && aMatches[0].rm_so + pEnd->item.asInteger.value <= aMatches[0].rm_eo )
           {
              aMatches[0].rm_eo = aMatches[0].rm_so + pEnd->item.asInteger.value;
           }

           if( regexec( &re, pString->item.asString.value, REGEX_MAX_GROUPS, aMatches, EFlags ) == 0 )
           {
              ulLen = aMatches[0].rm_eo - aMatches[0].rm_so;

              if( hb_pcount() > 3 )
              {
                 hb_stornl( aMatches[0].rm_so + 1, 4 );
              }
              if( hb_pcount() > 4 )
              {
                 hb_stornl( aMatches[0].rm_eo - aMatches[0].rm_so, 5 );
              }

              hb_retclen( pString->item.asString.value + aMatches[0].rm_so, ulLen );
              return;
           }
        }
    }

    if( hb_pcount() > 3 )
    {
       hb_stornl( 0, 4 );
    }
    if( hb_pcount() > 4 )
    {
       hb_stornl( 0, 5 );
    }

    hb_ret();
}
