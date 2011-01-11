/****************************************************************************
 *                                                                          *
 * File    : cpplex.c                                                       *
 *                                                                          *
 * Purpose : ISO C Compiler; Preprocessor; Input lexer.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-11-07  Bugfix: only room for 3 characters in fsm: ch[].     *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#define PREPROCESSOR
#include "lcc.h"

/*
 * lexical FSM encoding;
 *
 *   when in state state, and one of the characters in ch arrives,
 *   enter nextstate.
 *
 *   States >= S_SELF are either final, or at least require special action.
 *   In 'fsm' there is a line for each state X charset X nextstate.
 *   List chars that overwrite previous entries later (e.g. C_ALPH can
 *   be overridden by '_' by a later entry; and C_XX is the universal set,
 *   and should always be first.
 *
 *   States above S_SELF are represented in the big table as negative values.
 *   S_SELF and S_SELFB encode the resulting token type in the upper bits.
 *   These actions differ in that S_SELF doesn't have a lookahead char,
 *   S_SELFB does.
 *
 *   The encoding is blown out into a big table for time-efficiency.
 *
 *   Entries have
 *      nextstate: 6 bits; ?\ marker: 1 bit; tokentype: 9 bits.
 */

#define MAXSTATE      32
#define ACT(tok,act)  ((tok << 7) + act)
#define QBSBIT        0x40
#define GETACT(st)    ((st >> 7) & 0x1FF)

/* character classes */
#define C_WS    1
#define C_ALPH  2
#define C_NUM   3
#define C_EOF   4
#define C_XX    5

enum state {
    START=0, NUM1, NUM2, NUM3, ID1, ST1, ST2, ST3, COM1, COM2, COM3, COM4,
    CC1, CC2, WS1, PLUS1, MINUS1, STAR1, PCT1, SHARP1,
    CIRC1, GT1, GT2, LT1, LT2, OR1, AND1, ASG1, NOT1, DOTS1,
    S_SELF=MAXSTATE, S_SELFB, S_EOF, S_NL, S_EOFSTR,
    S_STNL, S_COMNL, S_EOFCOM, S_COMMENT, S_EOB, S_WS, S_NAME
};

struct fsm {
    int state;          /* if in this state */
    uchar_t ch[4];      /* and see one of these characters */
    int nextstate;      /* enter this state if +ve */
};

/*const*/ struct fsm fsm[] = {
    /* start state */
    START,  { C_XX },       ACT(UNCLASS,S_SELF),
    START,  { ' ', '\t', '\v' }, WS1,
    START,  { C_NUM },      NUM1,
    START,  { '.' },        NUM3,
    START,  { C_ALPH },     ID1,
    START,  { 'L' },        ST1,
    START,  { '"' },        ST2,
    START,  { '\'' },       CC1,
    START,  { '/' },        COM1,
    START,  { EOFC },       S_EOF,
    START,  { '\n' },       S_NL,
    START,  { '-' },        MINUS1,
    START,  { '+' },        PLUS1,
    START,  { '<' },        LT1,
    START,  { '>' },        GT1,
    START,  { '=' },        ASG1,
    START,  { '!' },        NOT1,
    START,  { '&' },        AND1,
    START,  { '|' },        OR1,
    START,  { '#' },        SHARP1,
    START,  { '%' },        PCT1,
    START,  { '[' },        ACT(SBRA,S_SELF),
    START,  { ']' },        ACT(SKET,S_SELF),
    START,  { '(' },        ACT(LP,S_SELF),
    START,  { ')' },        ACT(RP,S_SELF),
    START,  { '*' },        STAR1,
    START,  { ',' },        ACT(COMMA,S_SELF),
    START,  { '?' },        ACT(QUEST,S_SELF),
    START,  { ':' },        ACT(COLON,S_SELF),
    START,  { ';' },        ACT(SEMIC,S_SELF),
    START,  { '{' },        ACT(CBRA,S_SELF),
    START,  { '}' },        ACT(CKET,S_SELF),
    START,  { '~' },        ACT(TILDE,S_SELF),
    START,  { '^' },        CIRC1,

    /* saw a digit */
    NUM1,   { C_XX },       ACT(NUMBER,S_SELFB),
    NUM1,   { C_NUM, C_ALPH, '.' }, NUM1,
    /* NUM1,   { 'E', 'e', 'P', 'p' }, NUM2, */
    NUM1,   { 'E', 'e' }, NUM2,  /* bugfix: 04-11-07 */
    NUM1,   { 'P', 'p' }, NUM2,  /* bugfix: 04-11-07 */
    NUM1,   { '_' },        ACT(NUMBER,S_SELFB),

    /* saw possible start of exponent, digits-e */
    NUM2,   { C_XX },       ACT(NUMBER,S_SELFB),
    NUM2,   { '+', '-' },   NUM1,
    NUM2,   { C_NUM, C_ALPH },      NUM1,
    NUM2,   { '_' },        ACT(NUMBER,S_SELFB),

    /* saw a '.', which could be a number or an operator */
    NUM3,   { C_XX },       ACT(DOT,S_SELFB),
    NUM3,   { '.' },        DOTS1,
    NUM3,   { C_NUM },      NUM1,

    DOTS1,  { C_XX },       ACT(UNCLASS,S_SELFB),
    DOTS1,  { C_NUM },      NUM1,
    DOTS1,  { '.' },        ACT(ELLIPS,S_SELF),

    /* saw a letter or _ */
    ID1,    { C_XX },       ACT(NAME,S_NAME),
    ID1,    { C_ALPH, C_NUM },      ID1,

    /* saw L (start of wide string?) */
    ST1,    { C_XX },       ACT(NAME,S_NAME),
    ST1,    { C_ALPH, C_NUM },      ID1,
    ST1,    { '"' },        ST2,
    ST1,    { '\'' },       CC1,

    /* saw " beginning string */
    ST2,    { C_XX },       ST2,
    ST2,    { '"' },        ACT(STRING,S_SELF),
    ST2,    { '\\' },       ST3,
    ST2,    { '\n' },       S_STNL,
    ST2,    { EOFC },       S_EOFSTR,

    /* saw \ in string */
    ST3,    { C_XX },       ST2,
    ST3,    { '\n' },       S_STNL,
    ST3,    { EOFC },       S_EOFSTR,

    /* saw ' beginning character const */
    CC1,    { C_XX },       CC1,
    CC1,    { '\'' },       ACT(CCON,S_SELF),
    CC1,    { '\\' },       CC2,
    CC1,    { '\n' },       S_STNL,
    CC1,    { EOFC },       S_EOFSTR,

    /* saw \ in ccon */
    CC2,    { C_XX },       CC1,
    CC2,    { '\n' },       S_STNL,
    CC2,    { EOFC },       S_EOFSTR,

    /* saw /, perhaps start of comment */
    COM1,   { C_XX },       ACT(SLASH,S_SELFB),
    COM1,   { '=' },        ACT(ASSLASH,S_SELF),
    COM1,   { '*' },        COM2,
    COM1,   { '/' },        COM4,

    /* saw / then *, start of comment */
    COM2,   { C_XX },       COM2,
    COM2,   { '\n' },       S_COMNL,
    COM2,   { '*' },        COM3,
    COM2,   { EOFC },       S_EOFCOM,

    /* saw the * possibly ending a comment */
    COM3,   { C_XX },       COM2,
    COM3,   { '\n' },       S_COMNL,
    COM3,   { '*' },        COM3,
    COM3,   { '/' },        S_COMMENT,

    /* // comment */
    COM4,   { C_XX },       COM4,
    COM4,   { '\n' },       S_NL,
    COM4,   { EOFC },       S_EOFCOM,

    /* saw white space, eat it up */
    WS1,    { C_XX },       S_WS,
    WS1,    { ' ', '\t', '\v' },    WS1,

    /* saw -, check --, -=, -> */
    MINUS1, { C_XX },       ACT(MINUS,S_SELFB),
    MINUS1, { '-' },        ACT(MMINUS,S_SELF),
    MINUS1, { '=' },        ACT(ASMINUS,S_SELF),
    MINUS1, { '>' },        ACT(ARROW,S_SELF),

    /* saw +, check ++, += */
    PLUS1,  { C_XX },       ACT(PLUS,S_SELFB),
    PLUS1,  { '+' },        ACT(PPLUS,S_SELF),
    PLUS1,  { '=' },        ACT(ASPLUS,S_SELF),

    /* saw <, check <<, <<=, <= */
    LT1,    { C_XX },       ACT(LT,S_SELFB),
    LT1,    { '<' },        LT2,
    LT1,    { '=' },        ACT(LEQ,S_SELF),
    LT2,    { C_XX },       ACT(LSH,S_SELFB),
    LT2,    { '=' },        ACT(ASLSH,S_SELF),

    /* saw >, check >>, >>=, >= */
    GT1,    { C_XX },       ACT(GT,S_SELFB),
    GT1,    { '>' },        GT2,
    GT1,    { '=' },        ACT(GEQ,S_SELF),
    GT2,    { C_XX },       ACT(RSH,S_SELFB),
    GT2,    { '=' },        ACT(ASRSH,S_SELF),

    /* = */
    ASG1,   { C_XX },       ACT(ASGN,S_SELFB),
    ASG1,   { '=' },        ACT(EQ,S_SELF),

    /* ! */
    NOT1,   { C_XX },       ACT(NOT,S_SELFB),
    NOT1,   { '=' },        ACT(NEQ,S_SELF),

    /* & */
    AND1,   { C_XX },       ACT(AND,S_SELFB),
    AND1,   { '&' },        ACT(LAND,S_SELF),
    AND1,   { '=' },        ACT(ASAND,S_SELF),

    /* | */
    OR1,    { C_XX },       ACT(OR,S_SELFB),
    OR1,    { '|' },        ACT(LOR,S_SELF),
    OR1,    { '=' },        ACT(ASOR,S_SELF),

    /* # */
    SHARP1, { C_XX },       ACT(SHARP,S_SELFB),
    SHARP1, { '#' },        ACT(DSHARP,S_SELF),

    /* % */
    PCT1,   { C_XX },       ACT(PCT,S_SELFB),
    PCT1,   { '=' },        ACT(ASPCT,S_SELF),

    /* * */
    STAR1,  { C_XX },       ACT(STAR,S_SELFB),
    STAR1,  { '=' },        ACT(ASSTAR,S_SELF),

    /* ^ */
    CIRC1,  { C_XX },       ACT(CIRC,S_SELFB),
    CIRC1,  { '=' },        ACT(ASCIRC,S_SELF),

    -1
};

/* first index is char, second is state */
/* increase #states to power of 2 to encourage use of shift */
static short bigfsm[256][MAXSTATE];

/* Static function prototypes */
static bool_t trigraph(SOURCE *);
static bool_t digraph(SOURCE *);
static bool_t foldline(SOURCE *);
static int fillbuf(SOURCE *);

/****************************************************************************
 *                                                                          *
 * Function: setup_lexfsm                                                   *
 *                                                                          *
 * Purpose : Initialize the lexical finite state machine.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void setup_lexfsm(void)
{
    /*const*/ struct fsm *fp;
    int i, j, nstate;

    for (fp = fsm; fp->state >= 0; fp++)
    {
        for (i = 0; fp->ch[i]; i++)
        {
            nstate = fp->nextstate;
            if (nstate >= S_SELF)
                nstate = ~nstate;

            switch (fp->ch[i])
            {
                case C_XX:  /* random characters */
                    for (j = 0; j < 256; j++)
                        bigfsm[j][fp->state] = nstate;
                    continue;

                case C_ALPH:
                    for (j = 0; j < 256; j++)
                        if ('a' <= j && j <= 'z' || 
                            'A' <= j && j <= 'Z' || j == '_')
                            bigfsm[j][fp->state] = nstate;
                    continue;

                case C_NUM:
                    for (j = '0'; j <= '9'; j++)
                        bigfsm[j][fp->state] = nstate;
                    continue;

                default:
                    bigfsm[fp->ch[i]][fp->state] = nstate;
                    continue;
            }
        }
    }

    /* special cases for ? (trigraphs), \ (splicing), <:% (digraphs), runes, and EOB */
    for (i = 0; i < MAXSTATE; i++)
    {
        for (j = 0; j < 255; j++)
        {
            if (j == '?' || j == '\\' || j == '<' || j == ':' || j == '%')
            {
                if (bigfsm[j][i] > 0)
                    bigfsm[j][i] = ~bigfsm[j][i];
                bigfsm[j][i] &= ~QBSBIT;
            }
        }
        bigfsm[EOB][i] = ~S_EOB;
        if (bigfsm[EOFC][i] >= 0)
            bigfsm[EOFC][i] = ~S_EOF;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: get_tokens                                                     *
 *                                                                          *
 * Purpose : Fill in a row of tokens from input, terminated by NL or END.   *
 *           First token is put at trp->lp.                                 *
 *                                                                          *
 * Comment : Reset is non-zero when the input buffer can be "rewound."      *
 *           The return value is a flag indicating that possible macros     *
 *           have been seen in the row.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-25  Must check that state is START for digraph chars.    *
 *           04-11-24  Try to handle special Microsoft /##/ pasting case.   *
 *                                                                          *
 ****************************************************************************/

int get_tokens(TOKENROW *trp, bool_t reset)
{
    int c, state, oldstate;
    uchar_t *ip;
    TOKEN *tp;
    TOKEN *maxp;
    int runelen;
    SOURCE *s = cursource;
    int nmac = 0;

    tp = trp->lp;
    ip = s->inp;

    if (reset)
    {
        s->lineinc = 0;
        if (ip >= s->inl)  /* nothing in buffer */
        {
            s->inl = s->inb;
            fillbuf(s);
            ip = s->inp = s->inb;
        }
        else if (ip >= s->inb+(3*INS/4))
        {
            memmove(s->inb, ip, 4+s->inl-ip);
            s->inl = s->inb+(s->inl-ip);
            ip = s->inp = s->inb;
        }
    }

    maxp = &trp->bp[trp->max];
    runelen = 1;

    for (;;)
    {
continue2:
        if (tp >= maxp)
        {
            trp->lp = tp;
            tp = grow_tokenrow(trp);
            maxp = &trp->bp[trp->max];
        }

        tp->type = UNCLASS;
        tp->hideset = 0;
        tp->t = ip;
        tp->wslen = 0;
        tp->flag = 0;
        state = START;

        for (;;)
        {
            oldstate = state;
            c = *ip;
            if ((state = bigfsm[c][state]) >= 0)
            {
                ip += runelen;
                runelen = 1;
                continue;
            }
            state = ~state;
reswitch:
            switch (state & 0x7F)
            {
                case S_SELF:
                    ip += runelen;
                    runelen = 1;
                    /* fall through */
                case S_SELFB:
                    tp->type = GETACT(state);
                    tp->len = ip - tp->t;
                    tp++;
                    goto continue2;

                case S_NAME:  /* like S_SELFB but with nmac check */
                    tp->type = NAME;
                    tp->len = ip - tp->t;
                    nmac |= quicklook(tp->t[0], (tp->len > 1) ? tp->t[1] : 0);
                    tp++;
                    goto continue2;

                case S_WS:
                    tp->wslen = ip - tp->t;
                    tp->t = ip;
                    state = START;
                    continue;

                default:
                    if ((state & QBSBIT) == 0)
                    {
                        ip += runelen;
                        runelen = 1;
                        continue;
                    }
                    state &= ~QBSBIT;
                    s->inp = ip;
                    if (c == '?')  /* check trigraph */
                    {
                        if (trigraph(s))
                        {
                            state = oldstate;
                            continue;
                        }
                        goto reswitch;
                    }
                    if (c == '\\')  /* line-folding */
                    {
                        if (foldline(s))
                        {
                            s->lineinc++;
                            state = oldstate;
                            continue;
                        }
                        goto reswitch;
                    }
                    if (c == '<' || c == ':' || c == '%')  /* check digraph */
                    {
                        if (oldstate == START && digraph(s))  /* bugfix 04-03-25 */
                        {
                            state = oldstate;
                            continue;
                        }
                        goto reswitch;
                    }
                    pp_error(RCWARNING1(ERROR_LEXICAL_BOTCH));
                    ip += runelen;
                    runelen = 1;
                    continue;

                case S_EOB:
                    s->inp = ip;
                    fillbuf(cursource);
                    state = oldstate;
                    continue;

                case S_EOF:
                    tp->type = END;
                    tp->len = 0;
                    s->inp = ip;
                    if (tp != trp->bp && (tp-1)->type != NL && cursource->fd != -1)
                        pp_error(RCWARNING1(ERROR_NO_NEWLINE_AT_EOF));
                    trp->lp = tp+1;
                    return nmac;

                case S_STNL:
                    pp_error(RCERROR(ERROR_UNTERM_STRING_OR_CHAR));
                    /* fall through */
                case S_NL:
                    tp->t = ip;
                    tp->type = NL;
                    tp->len = 1;
                    tp->wslen = 0;
                    s->lineinc++;
                    s->inp = ip+1;
                    trp->lp = tp+1;
                    return nmac;

                case S_EOFSTR:
                    pp_error(RCFATAL(ERROR_EOF_IN_STRING_OR_CHAR));
                    break;

                case S_COMNL:
                    s->lineinc++;
                    state = COM2;
                    ip += runelen;
                    runelen = 1;
                    if (ip >= s->inb+(7*INS/8))  /* very long comment */
                    {
                        memmove(tp->t, ip, 4+s->inl-ip);
                        s->inl -= ip-tp->t;
                        ip = tp->t+1;
                    }
                    continue;

                case S_EOFCOM:
                    /* try to handle special Microsoft /##/ pasting case (04-11-24) */
                    if (options.microsoft && cursource->fd == -1)
                    {
                        tp->type = MSCOM;
                        tp->len = 0;
                        tp++;
                    }
                    else
                    {
                        pp_error(RCWARNING1(ERROR_EOF_INSIDE_COMMENT));
                    }
                    --ip;
                    /* fall through */
                case S_COMMENT:
                    ++ip;
                    tp->t = ip;
                    tp->t[-1] = ' ';
                    tp->wslen = 1;
                    state = START;
                    continue;
            }
            break;
        }

        ip += runelen;
        runelen = 1;
        tp->len = ip - tp->t;
        tp++;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: trigraph                                                       *
 *                                                                          *
 * Purpose : Have seen ?; handle the trigraph it starts (if any) else 0.    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-05-04  Warning message added (level 2).                     *
 *                                                                          *
 ****************************************************************************/

static bool_t trigraph(SOURCE *s)
{
    int c;

    while (s->inp+2 >= s->inl && fillbuf(s) != EOF)
        ;

    if (s->inp[1] != '?')
        return FALSE;

    c = (s->inp[2] == '=') ? '#' :      /* ??=  # */
        (s->inp[2] == '(') ? '[' :      /* ??(  [ */
        (s->inp[2] == ')') ? ']' :      /* ??)  ] */
        (s->inp[2] == '/') ? '\\' :     /* ??/  \ */
        (s->inp[2] == '\'') ? '^' :     /* ??'  ^ */
        (s->inp[2] == '<') ? '{' :      /* ??<  { */
        (s->inp[2] == '!') ? '|' :      /* ??!  | */
        (s->inp[2] == '>') ? '}' :      /* ??>  } */
        (s->inp[2] == '-') ? '~' : 0;   /* ??-  ~ */

    if (c)
    {
        pp_error(RCWARNING2(ERROR_TRIGRAPH_USED), s->inp[2]);
        *s->inp = c;
        memmove(s->inp+1, s->inp+3, s->inl-s->inp+2);
        s->inl -= 2;
        return TRUE;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: digraph                                                        *
 *                                                                          *
 * Purpose : Have seen < : %; handle digraph it starts (if any) else 0.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t digraph(SOURCE *s)
{
    int c;

    while (s->inp+1 >= s->inl && fillbuf(s) != EOF)
        ;

    c = (s->inp[0] == '<' && s->inp[1] == ':') ? '[' :
        (s->inp[0] == ':' && s->inp[1] == '>') ? ']' :
        (s->inp[0] == '<' && s->inp[1] == '%') ? '{' :
        (s->inp[0] == '%' && s->inp[1] == '>') ? '}' :
        (s->inp[0] == '%' && s->inp[1] == ':') ? '#' : 0;

    if (c)
    {
        s->inp[0] = c;
        memmove(s->inp+1, s->inp+2, s->inl-s->inp+3);
        s->inl -= 1;
        return TRUE;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: foldline                                                       *
 *                                                                          *
 * Purpose : Have seen \; handle the newline it escapes (if any).           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t foldline(SOURCE *s)
{
    while (s->inp+1 >= s->inl && fillbuf(s) != EOF)
        ;

    if (s->inp[1] == '\n')
    {
        memmove(s->inp, s->inp+2, s->inl-s->inp+3);
        s->inl -= 2;
        return TRUE;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: fillbuf                                                        *
 *                                                                          *
 * Purpose : Fill the source buffers, if reading from a file.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int fillbuf(SOURCE *s)
{
    int n, nr;

    nr = INS/8;

    if ((char *)s->inl+nr > (char *)s->inb+INS)
        pp_error(RCFATAL(ERROR_INPUT_BUFFER_OVERFLOW));

    if (s->fd == -1 || (n = _read(s->fd, (char *)s->inl, INS/8)) <= 0)
        n = 0;

    if ((*s->inp & 0xFF) == EOB)  /* sentinel character appears in input */
        *s->inp = EOFC;

    s->inl += n;
    s->inl[0] = s->inl[1] = s->inl[2] = s->inl[3] = EOB;

    if (n == 0)
    {
        s->inl[0] = s->inl[1] = s->inl[2] = s->inl[3] = EOFC;
        return EOF;
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: set_source                                                     *
 *                                                                          *
 * Purpose : Push down to a new source of characters.                       *
 *                                                                          *
 *           If fd > 0 and str == NULL, then from a file 'name';            *
 *           if fd == -1 and str, then from the string.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-05-19  Added verbose output of source and include files.    *
 *                                                                          *
 ****************************************************************************/

SOURCE *set_source(const char *filename, int fd, const char *str)
{
    SOURCE *s = my_alloc(sizeof(*s));
    int len;

    s->line = 1;
    s->lineinc = 0;
    s->fd = fd;
    s->filename = filename;
    s->next = cursource;
    s->ifdepth = 0;
    cursource = s;

    /* slop at right for EOB */
    if (str)
    {
        len = strlen(str);
        s->inb = my_alloc(len+4);
        s->inp = s->inb;
        strncpy((char *)s->inp, str, len);
    }
    else
    {
        s->inb = my_alloc(INS+4);
        s->inp = s->inb;
        len = 0;
    }

    s->inl = s->inp + len;
    s->inl[0] = s->inl[1] = EOB;

    if (fd != -1)
    {
        if (options.verbose && !s->next)  /* compatibility */
            printf("%s\n", filename);
        else if (options.infolevel > 0)
        {
            SOURCE *t;
            for (t = s; t->next != 0; t = t->next)
                printf("  ");
            printf("%s\n", filename);
        }
    }

    return s;
}

/****************************************************************************
 *                                                                          *
 * Function: unset_source                                                   *
 *                                                                          *
 * Purpose : Restore to the previous source of characters.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void unset_source(void)
{
    SOURCE *s = cursource;

    if (s->fd != -1)
    {
        _close(s->fd);
        my_free(s->inb);
    }

    cursource = s->next;
    my_free(s);
}
