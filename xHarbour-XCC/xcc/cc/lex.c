/****************************************************************************
 *                                                                          *
 * File    : lex.c                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; Input lexer.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-12-10  Optional wide string NLS conversion added.           *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop
#include <float.h>
#include <errno.h>

#include "lcc.h"

COORDINATE src;     /* current source coordinate */
int tok;            /* current token id */
char *tokstr;       /* current token */
SYMBOL *toksym;     /* symbol table entry for current token */

char kind[] = {
#define xx(a,b,c,d,e,f,g) f,
#define yy(a,b,c,d,e,f,g) f,
#include "token.h"
};

#define MAXTOKEN 32

enum { BLANK=1, NEWLINE=2, LETTER=4, DIGIT=8, HEX=16, OTHER=32 };

static const uchar_t map[256] = {
    /* 00 nul */        0,
    /* 01 soh */        0,
    /* 02 stx */        0,
    /* 03 etx */        0,
    /* 04 eot */        0,
    /* 05 enq */        0,
    /* 06 ack */        0,
    /* 07 bel */        0,
    /* 08 bs  */        0,
    /* 09 ht  */        BLANK,
    /* 0A nl  */        NEWLINE,
    /* 0B vt  */        BLANK,
    /* 0C ff  */        BLANK,
    /* 0D cr  */        0,
    /* 0E so  */        0,
    /* 0F si  */        0,
    /* 10 dle */        0,
    /* 11 dc1 */        0,
    /* 12 dc2 */        0,
    /* 13 dc3 */        0,
    /* 14 dc4 */        0,
    /* 15 nak */        0,
    /* 16 syn */        0,
    /* 17 etb */        0,
    /* 18 can */        0,
    /* 19 em  */        0,
    /* 1A sub */        0,
    /* 1B esc */        0,
    /* 1C fs  */        0,
    /* 1D gs  */        0,
    /* 1E rs  */        0,
    /* 1F us  */        0,
    /* 20 sp  */        BLANK,
    /* 21 !   */        OTHER,
    /* 22 "   */        OTHER,
    /* 23 #   */        OTHER,
    /* 24 $   */        0,
    /* 25 %   */        OTHER,
    /* 26 &   */        OTHER,
    /* 27 '   */        OTHER,
    /* 28 (   */        OTHER,
    /* 29 )   */        OTHER,
    /* 2A *   */        OTHER,
    /* 2B +   */        OTHER,
    /* 2C ,   */        OTHER,
    /* 2D -   */        OTHER,
    /* 2E .   */        OTHER,
    /* 2F /   */        OTHER,
    /* 30 0   */        DIGIT,
    /* 31 1   */        DIGIT,
    /* 32 2   */        DIGIT,
    /* 33 3   */        DIGIT,
    /* 34 4   */        DIGIT,
    /* 35 5   */        DIGIT,
    /* 36 6   */        DIGIT,
    /* 37 7   */        DIGIT,
    /* 38 8   */        DIGIT,
    /* 39 9   */        DIGIT,
    /* 3A :   */        OTHER,
    /* 3B ;   */        OTHER,
    /* 3C <   */        OTHER,
    /* 3D =   */        OTHER,
    /* 3E >   */        OTHER,
    /* 3F ?   */        OTHER,
    /* 40 @   */        0,
    /* 41 A   */        LETTER|HEX,
    /* 42 B   */        LETTER|HEX,
    /* 43 C   */        LETTER|HEX,
    /* 44 D   */        LETTER|HEX,
    /* 45 E   */        LETTER|HEX,
    /* 46 F   */        LETTER|HEX,
    /* 47 G   */        LETTER,
    /* 48 H   */        LETTER,
    /* 49 I   */        LETTER,
    /* 4A J   */        LETTER,
    /* 4B K   */        LETTER,
    /* 4C L   */        LETTER,
    /* 4D M   */        LETTER,
    /* 4E N   */        LETTER,
    /* 4F O   */        LETTER,
    /* 50 P   */        LETTER,
    /* 51 Q   */        LETTER,
    /* 52 R   */        LETTER,
    /* 53 S   */        LETTER,
    /* 54 T   */        LETTER,
    /* 55 U   */        LETTER,
    /* 56 V   */        LETTER,
    /* 57 W   */        LETTER,
    /* 58 X   */        LETTER,
    /* 59 Y   */        LETTER,
    /* 5A Z   */        LETTER,
    /* 5B [   */        OTHER,
    /* 5C \   */        OTHER,
    /* 5D ]   */        OTHER,
    /* 5E ^   */        OTHER,
    /* 5F _   */        LETTER,
    /* 60 `   */        0,
    /* 61 a   */        LETTER|HEX,
    /* 62 b   */        LETTER|HEX,
    /* 63 c   */        LETTER|HEX,
    /* 64 d   */        LETTER|HEX,
    /* 65 e   */        LETTER|HEX,
    /* 66 f   */        LETTER|HEX,
    /* 67 g   */        LETTER,
    /* 68 h   */        LETTER,
    /* 69 i   */        LETTER,
    /* 6A j   */        LETTER,
    /* 6B k   */        LETTER,
    /* 6C l   */        LETTER,
    /* 6D m   */        LETTER,
    /* 6E n   */        LETTER,
    /* 6F o   */        LETTER,
    /* 70 p   */        LETTER,
    /* 71 q   */        LETTER,
    /* 72 r   */        LETTER,
    /* 73 s   */        LETTER,
    /* 74 t   */        LETTER,
    /* 75 u   */        LETTER,
    /* 76 v   */        LETTER,
    /* 77 w   */        LETTER,
    /* 78 x   */        LETTER,
    /* 79 y   */        LETTER,
    /* 7A z   */        LETTER,
    /* 7B {   */        OTHER,
    /* 7C |   */        OTHER,
    /* 7D }   */        OTHER,
    /* 7E ~   */        OTHER,
    /* 7F     */        0,
    /* 80     */        0,
    /* 81     */        0,
    /* 82     */        0,
    /* 83     */        0,
    /* 84     */        0,
    /* 85     */        0,
    /* 86     */        0,
    /* 87     */        0,
    /* 88     */        0,
    /* 89     */        0,
    /* 8A     */        0,
    /* 8B     */        0,
    /* 8C     */        0,
    /* 8D     */        0,
    /* 8E     */        0,
    /* 8F     */        0,
    /* 90     */        0,
    /* 91     */        0,
    /* 92     */        0,
    /* 93     */        0,
    /* 94     */        0,
    /* 95     */        0,
    /* 96     */        0,
    /* 97     */        0,
    /* 98     */        0,
    /* 99     */        0,
    /* 9A     */        0,
    /* 9B     */        0,
    /* 9C     */        0,
    /* 9D     */        0,
    /* 9E     */        0,
    /* 9F     */        0,
    /* A0     */        0,
    /* A1     */        0,
    /* A2     */        0,
    /* A3     */        0,
    /* A4     */        0,
    /* A5     */        0,
    /* A6     */        0,
    /* A7     */        0,
    /* A8     */        0,
    /* A9     */        0,
    /* AA     */        0,
    /* AB     */        0,
    /* AC     */        0,
    /* AD     */        0,
    /* AE     */        0,
    /* AF     */        0,
    /* B0     */        0,
    /* B1     */        0,
    /* B2     */        0,
    /* B3     */        0,
    /* B4     */        0,
    /* B5     */        0,
    /* B6     */        0,
    /* B7     */        0,
    /* B8     */        0,
    /* B9     */        0,
    /* BA     */        0,
    /* BB     */        0,
    /* BC     */        0,
    /* BD     */        0,
    /* BE     */        0,
    /* BF     */        0,
    /* C0     */        0,
    /* C1     */        0,
    /* C2     */        0,
    /* C3     */        0,
    /* C4 Ä   */        LETTER,     /* ANSI */
    /* C5 Å   */        LETTER,     /* ANSI */
    /* C6     */        0,
    /* C7     */        0,
    /* C8     */        0,
    /* C9 É   */        LETTER,     /* ANSI */
    /* CA     */        0,
    /* CB     */        0,
    /* CC     */        0,
    /* CD     */        0,
    /* CE     */        0,
    /* CF     */        0,
    /* D0     */        0,
    /* D1     */        0,
    /* D2     */        0,
    /* D3     */        0,
    /* D4     */        0,
    /* D5     */        0,
    /* D6 Ö   */        LETTER,     /* ANSI */
    /* D7     */        0,
    /* D8     */        0,
    /* D9     */        0,
    /* DA     */        0,
    /* DB     */        0,
    /* DC     */        0,
    /* DD     */        0,
    /* DE     */        0,
    /* DF     */        0,
    /* E0     */        0,
    /* E1     */        0,
    /* E2     */        0,
    /* E3     */        0,
    /* E4 ä   */        LETTER,     /* ANSI */
    /* E5 å   */        LETTER,     /* ANSI */
    /* E6     */        0,
    /* E7     */        0,
    /* E8     */        0,
    /* E9 é   */        LETTER,     /* ANSI */
    /* EA     */        0,
    /* EB     */        0,
    /* EC     */        0,
    /* ED     */        0,
    /* EE     */        0,
    /* EF     */        0,
    /* F0     */        0,
    /* F1     */        0,
    /* F2     */        0,
    /* F3     */        0,
    /* F4     */        0,
    /* F5     */        0,
    /* F6 ö   */        LETTER,     /* ANSI */
    /* F7     */        0,
    /* F8     */        0,
    /* F9     */        0,
    /* FA     */        0,
    /* FB     */        0,
    /* FC     */        0,
    /* FD     */        0,
    /* FE     */        0,
    /* FF     */        0
};

static SYMBOL tokval;
static char cbuf[BUFSIZE+1];
static widechar_t wcbuf[BUFSIZE+1];

/* Static function prototypes */
static SYMBOL *iconst(uintmax_t, bool_t, int);
static SYMBOL *fconst(bool_t);
static void ppnumber(int);
static void *sconst(int, void *(int, void *), void *);
static void *cput(int, void *);
static void *wcput(int, void *);
static int backslash(int);

/****************************************************************************
 *                                                                          *
 * Function: expect                                                         *
 *                                                                          *
 * Purpose : Accept current token or report error.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void expect(int token)
{
    if (tok == token)
        tok = gettok();
    else
        apperror(RCERROR(ERROR_EXPECTED_TOKEN_X), token);
}

/****************************************************************************
 *                                                                          *
 * Function: skipto                                                         *
 *                                                                          *
 * Purpose : Skip to next token in set.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void skipto(int token, char set[])
{
    char *s;

    assert(set);
    for (; tok != EOI && tok != token; tok = gettok())
    {
        for (s = set; *s && kind[tok] != *s; s++)
            ;

        if (kind[tok] == *s)
            break;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: follow                                                         *
 *                                                                          *
 * Purpose : Combination of expect() and skipto().                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void follow(int token, char set[])
{
    if (tok == token)
    {
        tok = gettok();
    }
    else
    {
        expect(token);
        skipto(token, set);

        if (tok == token)
            tok = gettok();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: gettok                                                         *
 *                                                                          *
 * Purpose : Return next token from the input stream.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-17  Support for hexadecimal-floating-constants added.    *
 *           03-08-13  Accept _declspec as alias for __declspec.            *
 *           04-04-02  Bugfix: don't accept empty character constants.      *
 *           04-07-02  Accept _fastcall and __fastcall keywords.            *
 *           04-12-10  Optional wide string NLS conversion added.           *
 *           04-12-12  Accept _forceinline and __forceinline.               *
 *                                                                          *
 ****************************************************************************/

int gettok(void)
{
    for (;;)
    {
        register uchar_t *rcp = cp;

        while (map[*rcp] & BLANK)
            rcp++;

        if (limit - rcp < MAXTOKEN)
        {
            cp = rcp;
            cc_fillbuf();
            rcp = cp;
        }

        src.file = file;
        src.x = (char *)rcp - line;
        src.y = lineno;
        cp = rcp + 1;

        switch (*rcp++)
        {
            case '/':
                if (*rcp == '*')
                {
                    int c = 0;
                    for (rcp++; *rcp != '/' || c != '*'; )
                    {
                        if (map[*rcp] & NEWLINE)
                        {
                            if (rcp < limit)
                                c = *rcp;

                            cp = rcp + 1;
                            cc_nextline();
                            rcp = cp;

                            if (rcp == limit)
                                break;
                        }
                        else
                        {
                            c = *rcp++;
                        }
                    }

                    if (rcp < limit)
                        rcp++;
                    else
                        apperror(RCERROR(ERROR_UNCLOSED_COMMENT));
                    cp = rcp;
                    continue;
                }
                return '/';

            case '<':
                if (*rcp == '=') return cp++, LEQ;
                if (*rcp == '<') return cp++, LSHIFT;
                return '<';

            case '>':
                if (*rcp == '=') return cp++, GEQ;
                if (*rcp == '>') return cp++, RSHIFT;
                return '>';

            case '-':
                if (*rcp == '>') return cp++, DEREF;
                if (*rcp == '-') return cp++, DECR;
                return '-';

            case '=': return (*rcp == '=') ? cp++, EQL    : '=';
            case '!': return (*rcp == '=') ? cp++, NEQ    : '!';
            case '|': return (*rcp == '|') ? cp++, OROR   : '|';
            case '&': return (*rcp == '&') ? cp++, ANDAND : '&';
            case '+': return (*rcp == '+') ? cp++, INCR   : '+';

            case ';': case ',': case ':':
            case '*': case '~': case '%': case '^': case '?':
            case '[': case ']': case '{': case '}': case '(': case ')':
                return rcp[-1];

            case '\n': case '\v': case '\r': case '\f':
                cc_nextline();
                if (cp == limit)
                {
                    toksym = NULL;
                    return EOI;
                }
                continue;

            case 'i':
                if (rcp[0] == 'f' &&
                    !(map[rcp[1]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 1;
                    return IF;
                }
                if (rcp[0] == 'n' &&
                    rcp[1] == 't' &&
                    !(map[rcp[2]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 2;
                    toksym = inttype->u.sym;
                    return INT_;
                }
                /* C99 supports inline functions */
                if (rcp[0] == 'n' &&
                    rcp[1] == 'l' &&
                    rcp[2] == 'i' &&
                    rcp[3] == 'n' &&
                    rcp[4] == 'e' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    return INLINE;
                }
                goto id;

            case 'h': case 'j': case 'k': case 'm': case 'n': case 'o':
            case 'p': case 'q': case 'x': case 'y': case 'z':
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
            case 'G': case 'H': case 'I': case 'J': case 'K':
            case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
            case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
            case 'Y': case 'Z':
            case '\xc4': case '\xc5': case '\xc9': case '\xd6':
            case '\xe4': case '\xe5': case '\xe9': case '\xf6':
id:
                if (limit - rcp < MAXLINE)
                {
                    cp = rcp - 1;
                    cc_fillbuf();
                    rcp = ++cp;
                }
                assert(cp == rcp);
                tokstr = (char *)rcp - 1;
                while (map[*rcp] & (DIGIT|LETTER))
                    rcp++;
                tokstr = stringn(tokstr, (char *)rcp - tokstr);
                toksym = lookup_symbol(tokstr, identifiers);
                cp = rcp;
                return ID;

            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
            {
                uintmax_t n = 0;
                if (limit - rcp < MAXLINE)
                {
                    cp = rcp - 1;
                    cc_fillbuf();
                    rcp = ++cp;
                }
                assert(cp == rcp);
                tokstr = (char *)rcp - 1;
                if (*tokstr == '0' && (*rcp == 'x' || *rcp == 'X'))
                {
                    bool_t overflow = FALSE;
                    while (*++rcp)
                    {
                        int d;
                        if (map[*rcp] & DIGIT)
                            d = *rcp - '0';
                        else if (*rcp >= 'a' && *rcp <= 'f')
                            d = *rcp - 'a' + 10;
                        else if (*rcp >= 'A' && *rcp <= 'F')
                            d = *rcp - 'A' + 10;
                        else
                            break;
                        if (n & ~(~((uintmax_t)0) >> 4))
                            overflow = TRUE;
                        else
                            n = (n << 4) + d;
                    }
                    if ((char *)rcp - tokstr <= 2)
                        apperror(RCERROR(ERROR_INVALID_HEX_CONSTANT), tokstr, (char *)rcp-tokstr);
                    if (*rcp == '.' || *rcp == 'p' || *rcp == 'P')  /* new 01-09-17 */
                    {
                        cp = rcp;
                        toksym = fconst(TRUE);
                        return FCONST;
                    }
                    cp = rcp;
                    toksym = iconst(n, overflow, 16);
                }
                else if (*tokstr == '0')
                {
                    bool_t overflow = FALSE, err = FALSE;
                    for ( ; map[*rcp] & DIGIT; rcp++)
                    {
                        if (*rcp == '8' || *rcp == '9')
                            err = TRUE;
                        if (n & ~(~((uintmax_t)0) >> 3))
                            overflow = TRUE;
                        else
                            n = (n << 3) + (*rcp - '0');
                    }
                    if (*rcp == '.' || *rcp == 'e' || *rcp == 'E')
                    {
                        cp = rcp;
                        toksym = fconst(FALSE);
                        return FCONST;
                    }
                    cp = rcp;
                    toksym = iconst(n, overflow, 8);
                    if (err)
                        apperror(RCERROR(ERROR_INVALID_OCT_CONSTANT), tokstr, (char *)cp-tokstr);
                }
                else
                {
                    bool_t overflow = FALSE;
                    for (n = *tokstr - '0'; map[*rcp] & DIGIT; )
                    {
                        int d = *rcp++ - '0';
                        if (n > (UINTMAX_MAX - d)/10)
                            overflow = TRUE;
                        else
                            n = 10*n + d;
                    }
                    if (*rcp == '.' || *rcp == 'e' || *rcp == 'E')
                    {
                        cp = rcp;
                        toksym = fconst(FALSE);
                        return FCONST;
                    }
                    cp = rcp;
                    toksym = iconst(n, overflow, 10);
                }
                return ICONST;
            }

            case '.':
                if (rcp[0] == '.' && rcp[1] == '.')
                {
                    cp += 2;
                    return ELLIPSIS;
                }

                if ((map[*rcp] & DIGIT) == 0)
                    return '.';

                if (limit - rcp < MAXLINE)
                {
                    cp = rcp - 1;
                    cc_fillbuf();
                    rcp = ++cp;
                }
                assert(cp == rcp);
                cp = rcp - 1;
                tokstr = (char *)cp;
                toksym = fconst(FALSE);
                return FCONST;

            case 'L':
                if (*rcp == '\'')
                {
                    if (options.codepage != -1)
                    {
                        char *s = sconst(*cp, cput, cbuf);
                        if (s - cbuf > 2)
                            apperror(RCWARNING1(ERROR_EXCESS_WIDE_CHAR));
                        else if (s - cbuf < 2)
                            apperror(RCERROR(ERROR_EMPTY_WIDE_CHAR));
                        if (MultiByteToWideChar(options.codepage, 0, cbuf, (s - cbuf), wcbuf, NELEMS(wcbuf)) == 0)
                            apperror(RCFATAL(ERROR_INVALID_CODEPAGE), options.codepage);
                    }
                    else
                    {
                        widechar_t *s = sconst(*cp, wcput, wcbuf);
                        if (s - wcbuf > 2)
                            apperror(RCWARNING1(ERROR_EXCESS_WIDE_CHAR));
                        else if (s - wcbuf < 2)
                            apperror(RCERROR(ERROR_EMPTY_WIDE_CHAR));
                    }
                    tokval.type = widechartype;
                    tokval.u.c.v.u = wcbuf[0];
                    toksym = &tokval;
                    return ICONST;
                }
                else if (*rcp == '"')
                {
                    if (options.codepage != -1)
                    {
                        char *s = sconst(*cp, cput, cbuf);
                        if (MultiByteToWideChar(options.codepage, 0, cbuf, (s - cbuf), wcbuf, NELEMS(wcbuf)) == 0)
                            apperror(RCFATAL(ERROR_INVALID_CODEPAGE), options.codepage);
                        tokval.type = new_array(widechartype, s - cbuf, 0);
                    }
                    else
                    {
                        widechar_t *s = sconst(*cp, wcput, wcbuf);
                        tokval.type = new_array(widechartype, s - wcbuf, 0);
                    }
                    tokval.u.c.v.p = wcbuf;
                    toksym = &tokval;
                    return SCONST;
                }
                else goto id;

            case '\'':
            {
                char *s = sconst(*--cp, cput, cbuf);
                if (s - cbuf > 2)
                    apperror(RCWARNING1(ERROR_EXCESS_MULTIBYTE_CHAR));
                else if (s - cbuf < 2)
                    apperror(RCERROR(ERROR_EMPTY_MULTIBYTE_CHAR));
                tokval.type = inttype;
                if (chartype->op == INT_)
                    tokval.u.c.v.i = extend(cbuf[0], chartype);
                else
                    tokval.u.c.v.i = cbuf[0] & 0xFF;
                toksym = &tokval;
                return ICONST;
            }

            case '"':
            {
                char *s = sconst(*--cp, cput, cbuf);
                tokval.type = new_array(chartype, s - cbuf, 0);
                tokval.u.c.v.p = cbuf;
                toksym = &tokval;
                return SCONST;
            }

            case 'a':
                if (rcp[0] == 'u' &&
                    rcp[1] == 't' &&
                    rcp[2] == 'o' &&
                    !(map[rcp[3]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 3;
                    return AUTO;
                }
                goto id;

            case 'b':
                if (rcp[0] == 'r' &&
                    rcp[1] == 'e' &&
                    rcp[2] == 'a' &&
                    rcp[3] == 'k' &&
                    !(map[rcp[4]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 4;
                    return BREAK;
                }
                goto id;

            case 'c':
                if (rcp[0] == 'a' &&
                    rcp[1] == 's' &&
                    rcp[2] == 'e' &&
                    !(map[rcp[3]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 3;
                    return CASE;
                }
                if (rcp[0] == 'h' &&
                    rcp[1] == 'a' &&
                    rcp[2] == 'r' &&
                    !(map[rcp[3]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 3;
                    toksym = chartype->u.sym;
                    return CHAR_;
                }
                if (rcp[0] == 'o' &&
                    rcp[1] == 'n' &&
                    rcp[2] == 's' &&
                    rcp[3] == 't' &&
                    !(map[rcp[4]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 4;
                    return CONST_;
                }
                if (rcp[0] == 'o' &&
                    rcp[1] == 'n' &&
                    rcp[2] == 't' &&
                    rcp[3] == 'i' &&
                    rcp[4] == 'n' &&
                    rcp[5] == 'u' &&
                    rcp[6] == 'e' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
                    return CONTINUE;
                }
                goto id;

            case 'd':
                if (rcp[0] == 'e' &&
                    rcp[1] == 'f' &&
                    rcp[2] == 'a' &&
                    rcp[3] == 'u' &&
                    rcp[4] == 'l' &&
                    rcp[5] == 't' &&
                    !(map[rcp[6]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 6;
                    return DEFAULT;
                }
                if (rcp[0] == 'o' &&
                    rcp[1] == 'u' &&
                    rcp[2] == 'b' &&
                    rcp[3] == 'l' &&
                    rcp[4] == 'e' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    toksym = doubletype->u.sym;
                    return DOUBLE_;
                }
                if (rcp[0] == 'o' &&
                    !(map[rcp[1]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 1;
                    return DO;
                }
                goto id;

            case 'e':
                if (rcp[0] == 'l' &&
                    rcp[1] == 's' &&
                    rcp[2] == 'e' &&
                    !(map[rcp[3]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 3;
                    return ELSE;
                }
                if (rcp[0] == 'n' &&
                    rcp[1] == 'u' &&
                    rcp[2] == 'm' &&
                    !(map[rcp[3]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 3;
                    return ENUM;
                }
                if (rcp[0] == 'x' &&
                    rcp[1] == 't' &&
                    rcp[2] == 'e' &&
                    rcp[3] == 'r' &&
                    rcp[4] == 'n' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    return EXTERN;
                }
                goto id;

            case 'f':
                if (rcp[0] == 'l' &&
                    rcp[1] == 'o' &&
                    rcp[2] == 'a' &&
                    rcp[3] == 't' &&
                    !(map[rcp[4]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 4;
                    toksym = floattype->u.sym;
                    return FLOAT_;
                }
                if (rcp[0] == 'o' &&
                    rcp[1] == 'r' &&
                    !(map[rcp[2]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 2;
                    return FOR;
                }
                goto id;

            case 'g':
                if (rcp[0] == 'o' &&
                    rcp[1] == 't' &&
                    rcp[2] == 'o' &&
                    !(map[rcp[3]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 3;
                    return GOTO;
                }
                goto id;

            case 'l':
                if (rcp[0] == 'o' &&
                    rcp[1] == 'n' &&
                    rcp[2] == 'g' &&
                    !(map[rcp[3]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 3;
                    return LONG_;
                }
                goto id;

            case 'r':
                if (rcp[0] == 'e' &&
                    rcp[1] == 'g' &&
                    rcp[2] == 'i' &&
                    rcp[3] == 's' &&
                    rcp[4] == 't' &&
                    rcp[5] == 'e' &&
                    rcp[6] == 'r' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
                    return REGISTER;
                }
                if (rcp[0] == 'e' &&
                    rcp[1] == 't' &&
                    rcp[2] == 'u' &&
                    rcp[3] == 'r' &&
                    rcp[4] == 'n' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    return RETURN;
                }
                /* C99 supports restrict pointers */
                if (rcp[0] == 'e' &&
                    rcp[1] == 's' &&
                    rcp[2] == 't' &&
                    rcp[3] == 'r' &&
                    rcp[4] == 'i' &&
                    rcp[5] == 'c' &&
                    rcp[6] == 't' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
                    return RESTRICT_;
                }
                goto id;

            case 's':
                if (rcp[0] == 'h' &&
                    rcp[1] == 'o' &&
                    rcp[2] == 'r' &&
                    rcp[3] == 't' &&
                    !(map[rcp[4]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 4;
                    return SHORT_;
                }
                if (rcp[0] == 'i' &&
                    rcp[1] == 'g' &&
                    rcp[2] == 'n' &&
                    rcp[3] == 'e' &&
                    rcp[4] == 'd' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    return SIGNED;
                }
                if (rcp[0] == 'i' &&
                    rcp[1] == 'z' &&
                    rcp[2] == 'e' &&
                    rcp[3] == 'o' &&
                    rcp[4] == 'f' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    return SIZEOF;
                }
                if (rcp[0] == 't' &&
                    rcp[1] == 'a' &&
                    rcp[2] == 't' &&
                    rcp[3] == 'i' &&
                    rcp[4] == 'c' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    return STATIC;
                }
                if (rcp[0] == 't' &&
                    rcp[1] == 'r' &&
                    rcp[2] == 'u' &&
                    rcp[3] == 'c' &&
                    rcp[4] == 't' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    return STRUCT;
                }
                if (rcp[0] == 'w' &&
                    rcp[1] == 'i' &&
                    rcp[2] == 't' &&
                    rcp[3] == 'c' &&
                    rcp[4] == 'h' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    return SWITCH;
                }
                goto id;

            case 't':
                if (rcp[0] == 'y' &&
                    rcp[1] == 'p' &&
                    rcp[2] == 'e' &&
                    rcp[3] == 'd' &&
                    rcp[4] == 'e' &&
                    rcp[5] == 'f' &&
                    !(map[rcp[6]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 6;
                    return TYPEDEF;
                }
                goto id;

            case 'u':
                if (rcp[0] == 'n' &&
                    rcp[1] == 'i' &&
                    rcp[2] == 'o' &&
                    rcp[3] == 'n' &&
                    !(map[rcp[4]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 4;
                    return UNION;
                }
                if (rcp[0] == 'n' &&
                    rcp[1] == 's' &&
                    rcp[2] == 'i' &&
                    rcp[3] == 'g' &&
                    rcp[4] == 'n' &&
                    rcp[5] == 'e' &&
                    rcp[6] == 'd' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
                    return UNSIGNED;
                }
                goto id;

            case 'v':
                if (rcp[0] == 'o' &&
                    rcp[1] == 'i' &&
                    rcp[2] == 'd' &&
                    !(map[rcp[3]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 3;
                    toksym = voidtype->u.sym;
                    return VOID_;
                }
                if (rcp[0] == 'o' &&
                    rcp[1] == 'l' &&
                    rcp[2] == 'a' &&
                    rcp[3] == 't' &&
                    rcp[4] == 'i' &&
                    rcp[5] == 'l' &&
                    rcp[6] == 'e' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
                    return VOLATILE_;
                }
                goto id;

            case 'w':
                if (rcp[0] == 'h' &&
                    rcp[1] == 'i' &&
                    rcp[2] == 'l' &&
                    rcp[3] == 'e' &&
                    !(map[rcp[4]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 4;
                    return WHILE;
                }
                goto id;

            case '_':
                /* C99 supports _Bool type */
                if (rcp[0] == 'B' &&
                    rcp[1] == 'o' &&
                    rcp[2] == 'o' &&
                    rcp[3] == 'l' &&
                    !(map[rcp[4]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 4;
                    toksym = booltype->u.sym;
                    return BOOL_;
                }
                /* C99 supports _Complex type */
                if (rcp[0] == 'C' &&
                    rcp[1] == 'o' &&
                    rcp[2] == 'm' &&
                    rcp[3] == 'p' &&
                    rcp[4] == 'l' &&
                    rcp[5] == 'e' &&
                    rcp[6] == 'x' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
#ifdef HAS_C99_COMPLEX
                    return COMPLEX_;
#else
                    return CONST_;
#endif
                }
                /* C99 supports _Imaginary type */
                if (rcp[0] == 'I' &&
                    rcp[1] == 'm' &&
                    rcp[2] == 'a' &&
                    rcp[3] == 'g' &&
                    rcp[4] == 'i' &&
                    rcp[5] == 'n' &&
                    rcp[6] == 'a' &&
                    rcp[7] == 'r' &&
                    rcp[8] == 'y' &&
                    !(map[rcp[9]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 9;
#ifdef HAS_C99_COMPLEX
                    return COMPLEX_;
#else
                    return CONST_;
#endif
                }
                /* C99 supports __func__ (block scope) */
                if (funcsym &&
                    rcp[0] == '_' &&
                    rcp[1] == 'f' &&
                    rcp[2] == 'u' &&
                    rcp[3] == 'n' &&
                    rcp[4] == 'c' &&
                    rcp[5] == '_' &&
                    rcp[6] == '_' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
                    tokval.type = qual(CONST_, new_array(chartype, strlen(funcsym->name) + 1, 0));
                    tokval.u.c.v.p = funcsym->name;
                    toksym = &tokval;
                    return SCONST;
                }
                /* __cdecl */
                if (rcp[0] == '_' &&
                    rcp[1] == 'c' &&
                    rcp[2] == 'd' &&
                    rcp[3] == 'e' &&
                    rcp[4] == 'c' &&
                    rcp[5] == 'l' &&
                    !(map[rcp[6]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 6;
                    return CDECL_;
                }
                /* _cdecl (compatibility) */
                if (rcp[0] == 'c' &&
                    rcp[1] == 'd' &&
                    rcp[2] == 'e' &&
                    rcp[3] == 'c' &&
                    rcp[4] == 'l' &&
                    !(map[rcp[5]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 5;
                    return CDECL_;
                }
                /* __stdcall */
                if (rcp[0] == '_' &&
                    rcp[1] == 's' &&
                    rcp[2] == 't' &&
                    rcp[3] == 'd' &&
                    rcp[4] == 'c' &&
                    rcp[5] == 'a' &&
                    rcp[6] == 'l' &&
                    rcp[7] == 'l' &&
                    !(map[rcp[8]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 8;
                    return STDCALL_;
                }
                /* _stdcall (compatibility) */
                if (rcp[0] == 's' &&
                    rcp[1] == 't' &&
                    rcp[2] == 'd' &&
                    rcp[3] == 'c' &&
                    rcp[4] == 'a' &&
                    rcp[5] == 'l' &&
                    rcp[6] == 'l' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
                    return STDCALL_;
                }
                /* __fastcall */
                if (rcp[0] == '_' &&
                    rcp[1] == 'f' &&
                    rcp[2] == 'a' &&
                    rcp[3] == 's' &&
                    rcp[4] == 't' &&
                    rcp[5] == 'c' &&
                    rcp[6] == 'a' &&
                    rcp[7] == 'l' &&
                    rcp[8] == 'l' &&
                    !(map[rcp[9]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 9;
                    return FASTCALL_;
                }
                /* _fastcall (compatibility) */
                if (rcp[0] == 'f' &&
                    rcp[1] == 'a' &&
                    rcp[2] == 's' &&
                    rcp[3] == 't' &&
                    rcp[4] == 'c' &&
                    rcp[5] == 'a' &&
                    rcp[6] == 'l' &&
                    rcp[7] == 'l' &&
                    !(map[rcp[8]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 8;
                    return FASTCALL_;
                }
                /* __asm */
                if (rcp[0] == '_' &&
                    rcp[1] == 'a' &&
                    rcp[2] == 's' &&
                    rcp[3] == 'm' &&
                    !(map[rcp[4]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 4;
                    return ASM;
                }
                /* _asm (compatibility) */
                if (rcp[0] == 'a' &&
                    rcp[1] == 's' &&
                    rcp[2] == 'm' &&
                    !(map[rcp[3]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 3;
                    return ASM;
                }
                /* __declspec */
                if (rcp[0] == '_' &&
                    rcp[1] == 'd' &&
                    rcp[2] == 'e' &&
                    rcp[3] == 'c' &&
                    rcp[4] == 'l' &&
                    rcp[5] == 's' &&
                    rcp[6] == 'p' &&
                    rcp[7] == 'e' &&
                    rcp[8] == 'c' &&
                    !(map[rcp[9]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 9;
                    return DECLSPEC;
                }
                /* _declspec (compatibility) */
                if (rcp[0] == 'd' &&
                    rcp[1] == 'e' &&
                    rcp[2] == 'c' &&
                    rcp[3] == 'l' &&
                    rcp[4] == 's' &&
                    rcp[5] == 'p' &&
                    rcp[6] == 'e' &&
                    rcp[7] == 'c' &&
                    !(map[rcp[8]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 8;
                    return DECLSPEC;
                }
                /* __inline */
                if (rcp[0] == '_' &&
                    rcp[1] == 'i' &&
                    rcp[2] == 'n' &&
                    rcp[3] == 'l' &&
                    rcp[4] == 'i' &&
                    rcp[5] == 'n' &&
                    rcp[6] == 'e' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
                    return INLINE;
                }
                /* _inline (compatibility) */
                if (rcp[0] == 'i' &&
                    rcp[1] == 'n' &&
                    rcp[2] == 'l' &&
                    rcp[3] == 'i' &&
                    rcp[4] == 'n' &&
                    rcp[5] == 'e' &&
                    !(map[rcp[6]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 6;
                    return INLINE;
                }
                /* __try */
                if (rcp[0] == '_' &&
                    rcp[1] == 't' &&
                    rcp[2] == 'r' &&
                    rcp[3] == 'y' &&
                    !(map[rcp[4]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 4;
                    return TRY;
                }
                /* __except */
                if (rcp[0] == '_' &&
                    rcp[1] == 'e' &&
                    rcp[2] == 'x' &&
                    rcp[3] == 'c' &&
                    rcp[4] == 'e' &&
                    rcp[5] == 'p' &&
                    rcp[6] == 't' &&
                    !(map[rcp[7]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 7;
                    return EXCEPT;
                }
                /* __finally */
                if (rcp[0] == '_' &&
                    rcp[1] == 'f' &&
                    rcp[2] == 'i' &&
                    rcp[3] == 'n' &&
                    rcp[4] == 'a' &&
                    rcp[5] == 'l' &&
                    rcp[6] == 'l' &&
                    rcp[7] == 'y' &&
                    !(map[rcp[8]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 8;
                    return FINALLY;
                }
                /* __leave */
                if (rcp[0] == '_' &&
                    rcp[1] == 'l' &&
                    rcp[2] == 'e' &&
                    rcp[3] == 'a' &&
                    rcp[4] == 'v' &&
                    rcp[5] == 'e' &&
                    !(map[rcp[6]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 6;
                    return LEAVE;
                }
                /* __forceinline */
                if (rcp[0] == '_' &&
                    rcp[1] == 'f' &&
                    rcp[2] == 'o' &&
                    rcp[3] == 'r' &&
                    rcp[4] == 'c' &&
                    rcp[5] == 'e' &&
                    rcp[6] == 'i' &&
                    rcp[7] == 'n' &&
                    rcp[8] == 'l' &&
                    rcp[9] == 'i' &&
                    rcp[10] == 'n' &&
                    rcp[11] == 'e' &&
                    !(map[rcp[12]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 12;
                    return INLINE;
                }
                /* _forceinline (compatibility?) */
                if (rcp[0] == 'f' &&
                    rcp[1] == 'o' &&
                    rcp[2] == 'r' &&
                    rcp[3] == 'c' &&
                    rcp[4] == 'e' &&
                    rcp[5] == 'i' &&
                    rcp[6] == 'n' &&
                    rcp[7] == 'l' &&
                    rcp[8] == 'i' &&
                    rcp[9] == 'n' &&
                    rcp[10] == 'e' &&
                    !(map[rcp[11]] & (DIGIT|LETTER)))
                {
                    cp = rcp + 11;
                    return INLINE;
                }
                goto id;

            default:
                if ((map[cp[-1]] & BLANK) == 0)
                    apperror(RCERROR(ERROR_ILLEGAL_CHAR), cp[-1]);
                break;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: iconst                                                         *
 *                                                                          *
 * Purpose : Parse a integer constant.                                      *
 *                                                                          *
 * Comment:  The type of an integer constant is the first of the            *
 *           corresponding list in which its value can be represented.      *
 *                                                                          *
 *           Suffix     Decimal Constant        Octal/Hexadecimal Constant  *
 *                                                                          *
 *           none       int                     int                         *
 *                      long int                unsigned int                *
 *                      long long int           long int                    *
 *                                              unsigned long int           *
 *                                              long long int               *
 *                                              unsigned long long int      *
 *                                                                          *
 *           u or U     unsigned int            unsigned int                *
 *                      unsigned long int       unsigned long int           *
 *                      unsigned long long int  unsigned long long int      *
 *                                                                          *
 *           l or L     long int                long int                    *
 *                      long long int           unsigned long int           *
 *                                              long long int               *
 *                                              unsigned long long int      *
 *                                                                          *
 *           Both u/U   unsigned long int       unsigned long int           *
 *           and l/L    unsigned long long int  unsigned long long int      *
 *                                                                          *
 *           ll or LL   long long int           long long int               *
 *                                              unsigned long long int      *
 *                                                                          *
 *           Both u/U   unsigned long long int  unsigned long long int      *
 *           and ll/LL                                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-17  Several bug-fixes added.                             *
 *           04-03-21  Added support for Microsoft 64-bit style (I64/UI64). *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *iconst(uintmax_t n, bool_t overflow, int base)
{
    if (options.microsoft && (*cp == 'u' || *cp == 'U') && (cp[1] == 'i' || cp[1] == 'I') && cp[2] == '6' && cp[3] == '4')
    {
        tokval.type = unsignedlonglongtype;
        cp += 4;
    }
    else if (options.microsoft && (*cp == 'i' || *cp == 'I') && cp[1] == '6' && cp[2] == '4')
    {
        if (overflow || (base != 10 && n > (uintmax_t)longlongtype->u.sym->u.limits.max.i))
            tokval.type = unsignedlonglongtype;
        else
            tokval.type = longlongtype;
        cp += 3;
    }
    else if ((*cp == 'u' || *cp == 'U') && (cp[1] == 'l' || cp[1] == 'L') && (cp[2] == 'l' || cp[2] == 'L') ||
             (*cp == 'l' || *cp == 'L') && (cp[1] == 'l' || cp[1] == 'L') && (cp[2] == 'u' || cp[2] == 'U'))
    {
        tokval.type = unsignedlonglongtype;
        cp += 3;
    }
    else if ((*cp == 'l' || *cp == 'L') && (cp[1] == 'l' || cp[1] == 'L'))
    {
        if (overflow || (base != 10 && n > (uintmax_t)longlongtype->u.sym->u.limits.max.i))
            tokval.type = unsignedlonglongtype;
        else
            tokval.type = longlongtype;
        cp += 2;
    }
    else if ((*cp == 'u' || *cp == 'U') && (cp[1] == 'l' || cp[1] == 'L') ||
             (*cp == 'l' || *cp == 'L') && (cp[1] == 'u' || cp[1] == 'U'))
    {
        if (overflow || n > (uintmax_t)unsignedlongtype->u.sym->u.limits.max.u)
            tokval.type = unsignedlonglongtype;
        else
            tokval.type = unsignedlongtype;
        cp += 2;
    }
    else if (*cp == 'u' || *cp == 'U')
    {
        if (overflow || n > (uintmax_t)unsignedlongtype->u.sym->u.limits.max.u)
            tokval.type = unsignedlonglongtype;
        else if (n > (uintmax_t)unsignedtype->u.sym->u.limits.max.u)
            tokval.type = unsignedlongtype;
        else
            tokval.type = unsignedtype;
        cp += 1;
    }
    else if (*cp == 'l' || *cp == 'L')
    {
        if (overflow || n > (uintmax_t)longlongtype->u.sym->u.limits.max.i)
            tokval.type = unsignedlonglongtype;
        else if (base != 10 && n > (uintmax_t)unsignedlongtype->u.sym->u.limits.max.u ||
                 base == 10 && n > (uintmax_t)longtype->u.sym->u.limits.max.i)
            tokval.type = longlongtype;
        else if (base != 10 && n > (uintmax_t)longtype->u.sym->u.limits.max.i)
            tokval.type = unsignedlongtype;
        else
            tokval.type = longtype;
        cp += 1;
    }
    else if (overflow || base != 10 && n > (uintmax_t)longlongtype->u.sym->u.limits.max.i)
        tokval.type = unsignedlonglongtype;
    else if (base != 10 && n > (uintmax_t)unsignedlongtype->u.sym->u.limits.max.u ||
             base == 10 && n > (uintmax_t)longtype->u.sym->u.limits.max.i)
        tokval.type = longlongtype;
    else if (base != 10 && n > (uintmax_t)longtype->u.sym->u.limits.max.i)
        tokval.type = unsignedlongtype;
    else if (base != 10 && n > (uintmax_t)unsignedtype->u.sym->u.limits.max.u ||
             base == 10 && n > (uintmax_t)inttype->u.sym->u.limits.max.i)
        tokval.type = longtype;
    else if (base != 10 && n > (uintmax_t)inttype->u.sym->u.limits.max.i)
        tokval.type = unsignedtype;
    else
        tokval.type = inttype;

    /* special hack for targets where longlong is same as long */
    if (tokval.type == unsignedlonglongtype && unsignedlonglongtype->size == unsignedlongtype->size)
        tokval.type = unsignedlongtype;
    else if (tokval.type == longlongtype && longlongtype->size == longtype->size)
        tokval.type = longtype;

    switch (tokval.type->op)
    {
        case INT_:
            if (overflow || n > (uintmax_t)tokval.type->u.sym->u.limits.max.i)
            {
                apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONSTANT), tokstr, (char *)cp - tokstr);
                tokval.u.c.v.i = tokval.type->u.sym->u.limits.max.i;
            }
            else
            {
                tokval.u.c.v.i = n;
            }
            break;

        case UNSIGNED:
            if (overflow || n > tokval.type->u.sym->u.limits.max.u)
            {
                apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONSTANT), tokstr, (char *)cp - tokstr);
                tokval.u.c.v.u = tokval.type->u.sym->u.limits.max.u;
            }
            else
            {
                tokval.u.c.v.u = n;
            }
            break;

        default:
            assert(0);
    }

    ppnumber(ERROR_PP_NUM_BUT_NO_INT_CONST);
    return &tokval;
}

/****************************************************************************
 *                                                                          *
 * Function: fconst                                                         *
 *                                                                          *
 * Purpose : Parse a floating point constant.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-17  Support for hexadecimal-floating-constants added.    *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *fconst(bool_t hex)
{
    if (*cp == '.')
    {
        do
            cp++;
        while (map[*cp] & ((hex) ? DIGIT|HEX : DIGIT));
    }

    if (*cp == (hex ? 'p' : 'e') || *cp == (hex ? 'P' : 'E'))
    {
        if (*++cp == '-' || *cp == '+')
            cp++;

        if (map[*cp] & DIGIT)
        {
            do
                cp++;
            while (map[*cp] & DIGIT);
        }
        else
        {
            apperror(RCERROR(ERROR_INVALID_FP_CONSTANT), tokstr, (char *)cp - tokstr);
        }
    }

#ifdef __POCC__
    /* verify that we are linked with correct CRT */
    assert(strtod("0xb.6p2", NULL) == 45.5);
#endif

    errno = 0;
    tokval.u.c.v.d = strtod(tokstr, NULL);
    if (errno == ERANGE)
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_FP_CONSTANT), tokstr, (char *)cp - tokstr);

    if (*cp == 'f' || *cp == 'F')
    {
        ++cp;
        if (tokval.u.c.v.d > floattype->u.sym->u.limits.max.d)
            apperror(RCWARNING1(ERROR_OVERFLOW_IN_FP_CONSTANT), tokstr, (char *)cp - tokstr);
        tokval.type = floattype;
    }
    else if (*cp == 'l' || *cp == 'L')
    {
        cp++;
        tokval.type = longdoubletype;
    }
    else
    {
        if (tokval.u.c.v.d > doubletype->u.sym->u.limits.max.d)
            apperror(RCWARNING1(ERROR_OVERFLOW_IN_FP_CONSTANT), tokstr, (char *)cp - tokstr);
        tokval.type = doubletype;
    }

    ppnumber(ERROR_PP_NUM_BUT_NO_FP_CONST);
    return &tokval;
}

/****************************************************************************
 *                                                                          *
 * Function: ppnumber                                                       *
 *                                                                          *
 * Purpose : Advance past a number from the preprocessor.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-17  Support for hexadecimal-floating-constants added.    *
 *                                                                          *
 ****************************************************************************/

static void ppnumber(int msg)
{
    uchar_t *rcp = cp--;

    for ( ; (map[*cp] & (DIGIT|LETTER)) || *cp == '.'; cp++)
    {
        if ((cp[0] == 'E' || cp[0] == 'e' || cp[0] == 'P' || cp[0] == 'p') &&
            (cp[1] == '-' || cp[1] == '+'))
            cp++;
    }

    if (cp > rcp)
        apperror(RCERROR(msg), tokstr, (char *)cp - tokstr);
}

/****************************************************************************
 *                                                                          *
 * Function: sconst                                                         *
 *                                                                          *
 * Purpose : Parse a string constant.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-05-29  Bugfix: concatination of L"..." and L"..." now OK.   *
 *           03-09-21  Better fix for the above from LCC 4.2                *
 *                                                                          *
 ****************************************************************************/

static void *sconst(int q, void *put(int c, void *cl), void *cl)
{
    int n = 0, nbad = 0;

    do
    {
        cp++;
        while (*cp != q)
        {
            int c;

            if (map[*cp] & NEWLINE)
            {
                if (cp < limit)
                    break;

                cp++;
                cc_nextline();

                if (cp == limit)
                    break;

                continue;
            }

            c = *cp++;
            if (c == '\\')
            {
                if (map[*cp] & NEWLINE)
                {
                    if (cp++ < limit)
                        continue;

                    cc_nextline();
                }

                if (limit - cp < MAXTOKEN)
                    cc_fillbuf();

                c = backslash(q);
            }
            else if (c < 0 || c > 255 || map[c] == 0)
                nbad++;

            if (n++ < BUFSIZE)
                cl = put(c, cl);
        }

        if (*cp == q)
            cp++;
        else
            apperror(RCERROR(ERROR_MISSING_CHAR), q);

        if (q == '"' && put == wcput && getchr() == 'L')
        {
            if (limit - cp < 2)
                cc_fillbuf();
            if (cp[1] == '"')
                cp++;
        }

    } while (q == '"' && getchr() == '"');
    cl = put(0, cl);

    if (n >= BUFSIZE)
        apperror(q == '"' ? RCERROR(ERROR_STRING_LITERAL_TOO_LONG) :
            RCERROR(ERROR_CHAR_LITERAL_TOO_LONG));

    if (q == '"' && n > 4093)  /* old limit 509 */
        apperror(RCWARNING2(ERROR_MORE_THAN_X_CHARS), 4095);

    if (nbad > 0)
        apperror(q == '"' ? RCWARNING2(ERROR_NON_PORTABLE_STR_CHARS) :
            RCWARNING2(ERROR_NON_PORTABLE_CHR_CHARS));

    return cl;
}

/****************************************************************************
 *                                                                          *
 * Function: cput                                                           *
 *                                                                          *
 * Purpose : Emit a character to the output buffer.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *cput(int c, void *cl)
{
    char *s = cl;

    if (c < 0 || c > 255)
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_ESC_SEQ), c);

    *s++ = c;
    return s;
}

/****************************************************************************
 *                                                                          *
 * Function: wcput                                                          *
 *                                                                          *
 * Purpose : Emit a wide character to the output buffer.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *wcput(int c, void *cl)
{
    widechar_t *s = cl;
    *s++ = c;
    return s;
}

/****************************************************************************
 *                                                                          *
 * Function: backslash                                                      *
 *                                                                          *
 * Purpose : Handle a escape sequence.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int backslash(int q)
{
    uint_t c;

    switch (*cp++)
    {
        case 'a': return 7;
        case 'b': return '\b';
        case 'f': return '\f';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case 'v': return '\v';
        case '\'': case '"': case '\\': case '\?': break;
        case 'x':
        {
            bool_t overflow = FALSE;

            if ((map[*cp] & (DIGIT|HEX)) == 0)
            {
                if (*cp < ' ' || *cp >= 127)
                    apperror(RCERROR(ERROR_INVALID_HEX_ESC_SEQ));
                else
                    apperror(RCERROR(ERROR_INVALID_HEX_ESC_SEQ_X), *cp);

                if (*cp != q)
                    cp++;

                return 0;
            }

            for (c = 0; (map[*cp] & (DIGIT|HEX)); cp++)
            {
                if (c >> (8*widechartype->size - 4))
                    overflow = TRUE;

                if (map[*cp] & DIGIT)
                    c = (c << 4) + *cp - '0';
                else
                    c = (c << 4) + (*cp & ~32) - 'A' + 10;
            }

            if (overflow)
                apperror(RCWARNING1(ERROR_OVERFLOW_IN_HEX_ESC_SEQ));

            return c & (uint_t)ones(8*widechartype->size);
        }

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
            c = *(cp-1) - '0';
            if (*cp >= '0' && *cp <= '7')
            {
                c = (c << 3) + *cp++ - '0';
                if (*cp >= '0' && *cp <= '7')
                    c = (c << 3) + *cp++ - '0';
            }
            return c;

        default:
            if (cp[-1] < ' ' || cp[-1] >= 127)
                apperror(RCWARNING1(ERROR_INVALID_CHAR_ESC_SEQ));
            else
                apperror(RCWARNING1(ERROR_INVALID_CHAR_ESC_SEQ_X), cp[-1]);
    }

    return cp[-1];
}

/****************************************************************************
 *                                                                          *
 * Function: getchr                                                         *
 *                                                                          *
 * Purpose : Return next character from the input stream.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int getchr(void)
{
    for (;;)
    {
        while (map[*cp] & BLANK)
            cp++;

        if (!(map[*cp] & NEWLINE))
            return *cp;

        cp++;
        cc_nextline();

        if (cp == limit)
            return EOI;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: getinp                                                         *
 *                                                                          *
 * Purpose : Return raw data from the input stream (for __asm statement).   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-21  New logic to handle embedded curly brackets.         *
 *                                                                          *
 ****************************************************************************/

char *getinp(void)
{
    int lev = 0;  /* curly bracket level */
    char *s = cbuf;

    src.x = (char *)cp - line;
    src.y = lineno;

    if (limit - cp < MAXLINE)
        cc_fillbuf();

    while (map[*cp] & BLANK)
        cp++;

    while (!(map[*cp] & NEWLINE))
    {
        /*
         * The __asm keyword is, according to Microsoft, a statement separator
         * (meaning that we can put several assembly instructions on the same line).
         * Especially useful in macros.
         */
        if (cp[0] == '_' &&
            cp[1] == '_' &&
            cp[2] == 'a' &&
            cp[3] == 's' &&
            cp[4] == 'm' &&
            !(map[cp[5]] & (DIGIT|LETTER)))
            break;

        if (cp[0] == '_' &&
            cp[1] == 'a' &&
            cp[2] == 's' &&
            cp[3] == 'm' &&
            !(map[cp[4]] & (DIGIT|LETTER)))
            break;

        if (*cp == '{')
            lev++;
        else if (*cp == '}' && lev-- == 0)
            break;

        *s++ = *cp++;
    }
    *s = '\0';

    return cbuf;
}

