/****************************************************************************
 *                                                                          *
 * File    : scanner.c                                                      *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; scanner module.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           98-11-08  HTML token added.                                    *
 *           00-01-23  PLUGPLAY and VXD tokens added.                       *
 *           01-05-24  MANIFEST token added.                                *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop
#include <ctype.h>
#include <limits.h>

#include "rc.h"

SCANNER s = {0};

BOOL bNewLine = TRUE;

/* Character classification */
#define ALPHA  'a'
#define DIGIT  '0'

/* Locals */
struct kwtab
{
    enum rctoken tok;
    char * const name;
}
static kwtab[] =
{
    C_LINESYNC,         "#line",
    C_ACCELERATORS,     "accelerators",
    C_ALT,              "alt",
    C_ANICURSOR,        "anicursor",
    C_ANIICON,          "aniicon",
    C_ASCII,            "ascii",
    C_AUTO3STATE,       "auto3state",
    C_AUTOCHECKBOX,     "autocheckbox",
    C_AUTORADIOBUTTON,  "autoradiobutton",
    C_BEGIN,            "begin",
    C_BITMAP,           "bitmap",
    C_BLOCK,            "block",
    C_CAPTION,          "caption",
    C_CHARACTERISTICS,  "characteristics",
    C_CHECKBOX,         "checkbox",
    C_CHECKED,          "checked",
    C_CLASS,            "class",
    C_COMBOBOX,         "combobox",
    C_CONTROL,          "control",
    C_CTEXT,            "ctext",
    C_CURSOR,           "cursor",
    C_DEFPUSHBUTTON,    "defpushbutton",
    C_DIALOG,           "dialog",
    C_DIALOGEX,         "dialogex",
    C_DISCARDABLE,      "discardable",
    C_DLGINCLUDE,       "dlginclude",
    C_EDITTEXT,         "edittext",
    C_END,              "end",
    C_EXSTYLE,          "exstyle",
    C_FILEFLAGS,        "fileflags",
    C_FILEFLAGSMASK,    "fileflagsmask",
    C_FILEOS,           "fileos",
    C_FILESUBTYPE,      "filesubtype",
    C_FILETYPE,         "filetype",
    C_FILEVERSION,      "fileversion",
    C_FIXED,            "fixed",
    C_FONT,             "font",
    C_GRAYED,           "grayed",
    C_GROUPBOX,         "groupbox",
    C_HELP,             "help",
    C_HTML,             "html",
    C_ICON,             "icon",
    C_IMPURE,           "impure",
    C_INACTIVE,         "inactive",
    C_LANGUAGE,         "language",
    C_LISTBOX,          "listbox",
    C_LOADONCALL,       "loadoncall",
    C_LTEXT,            "ltext",
    C_MANIFEST,         "manifest",
    C_MENU,             "menu",
    C_MENUBARBREAK,     "menubarbreak",
    C_MENUBREAK,        "menubreak",
    C_MENUEX,           "menuex",
    C_MENUITEM,         "menuitem",
    C_MESSAGETABLE,     "messagetable",
    C_MOVEABLE,         "moveable",
    C_NOINVERT,         "noinvert",
    C_NOT,              "not",
    C_PLUGPLAY,         "plugplay",
    C_POPUP,            "popup",
    C_PRELOAD,          "preload",
    C_PRODUCTVERSION,   "productversion",
    C_PURE,             "pure",
    C_PUSHBOX,          "pushbox",
    C_PUSHBUTTON,       "pushbutton",
    C_RADIOBUTTON,      "radiobutton",
    C_RCDATA,           "rcdata",
    C_RTEXT,            "rtext",
    C_SCROLLBAR,        "scrollbar",
    C_SEPARATOR,        "separator",
    C_SHIFT,            "shift",
    C_STATE3,           "state3",
    C_STRINGTABLE,      "stringtable",
    C_STYLE,            "style",
    C_VALUE,            "value",
    C_VERSION,          "version",
    C_VERSIONINFO,      "versioninfo",
    C_VIRTKEY,          "virtkey",
    C_VXD,              "vxd"
};

#define putchr(ch)  (s.ch = (ch))

static char idtab[256] = {0};

/* Static function prototypes */
static long acceptnumber(long, long);
static char nextchar(void);
static enum rctoken nextsymbol(void);
static char classify(char);
static enum rctoken alpha(char);
static enum rctoken digit(char);
static enum rctoken string(enum rctoken);
static enum rctoken borland(enum rctoken);
static enum rctoken lookup(const char *);

/****************************************************************************
 *                                                                          *
 * Function: accept_ulong_expression                                        *
 *                                                                          *
 * Purpose : Accept an expression of unsigned longs.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-05-17  Created                                              *
 *           98-07-01  Added argument with start value (n).                 *
 *                                                                          *
 ****************************************************************************/

ulong_t accept_ulong_expression(ulong_t n)
{
    do
    {
        if (trysymbol(C_NOT))
        {
            n &= ~accept_ulong();
        }
        else
        {
            n |= accept_ulong();
        }
    } while (trysymbol(C_OR));

    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: accept_ulong                                                   *
 *                                                                          *
 * Purpose : Accept current symbol as a unsigned long.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

ulong_t accept_ulong(void)
{
    long n = s.sym.num;

    accept_symbol(C_NUMBER);
    return (ulong_t)n;
}

/****************************************************************************
 *                                                                          *
 * Function: accept_short                                                   *
 *                                                                          *
 * Purpose : Accept current symbol as a short.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

short accept_short(void)
{
    long n = s.sym.num;

    if (s.sym.tok == C_NUMBER)
    {
        /* Accept something like -32768 to +65535 */
        if (s.sym.num < SHRT_MIN || s.sym.num > USHRT_MAX)
            apperror(RCWARNING1(ERROR_NUMBER_LIMIT_EXCEEDED));
    }

    accept_symbol(C_NUMBER);
    return (short)n;
}

/****************************************************************************
 *                                                                          *
 * Function: accept_ushort                                                  *
 *                                                                          *
 * Purpose : Accept current symbol as a unsigned short.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

ushort_t accept_ushort(void)
{
    return accept_short();
}

/****************************************************************************
 *                                                                          *
 * Function: accept_symbol                                                  *
 *                                                                          *
 * Purpose : Return symbol if current symbol is of requested type.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           99-08-15  ERROR_UNKNOWN_KEYWORD message added.                 *
 *           02-03-04  Check for C_EOF added.                               *
 *                                                                          *
 ****************************************************************************/

bool_t accept_symbol(enum rctoken tok)
{
    if (trysymbol(tok))
        return TRUE;

    switch (tok)
    {
        case C_NUMBER:
            if (s.sym.tok == C_SYMBOL)
                apperror(RCERROR(ERROR_UNKNOWN_KEYWORD), s.sym.str);
            else
                apperror(RCERROR(ERROR_NUMBER_EXPECTED));
            break;

        case C_STRING:
            apperror(RCERROR(ERROR_STRING_EXPECTED));
            break;

        case C_BEGIN:
            apperror(RCERROR(ERROR_BEGIN_EXPECTED));
            break;

        default:
            apperror(RCERROR(ERROR_INVALID_SYNTAX));
            break;
    }

    while (s.sym.tok != C_END && s.sym.tok != C_EOF && s.sym.tok != tok)
        getsymbol();

    if (s.sym.tok == C_EOF)
        apperror(RCFATAL(ERROR_UNEXPECTED_EOF));

    if (s.sym.tok != C_END)
        getsymbol();

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: trysymbol                                                      *
 *                                                                          *
 * Purpose : Return TRUE if current symbol is of requested type.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           00-02-12  Added aliases for BEGIN and END: { and }.            *
 *                                                                          *
 ****************************************************************************/

bool_t trysymbol(enum rctoken tok)
{
    if (s.sym.tok == tok)
    {
        getsymbol();
        return TRUE;
    }

    if ((tok == C_BEGIN && s.sym.tok == C_LBRACE) ||
        (tok == C_END && s.sym.tok == C_RBRACE))
    {
        getsymbol();
        return TRUE;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: getsymbol                                                      *
 *                                                                          *
 * Purpose : Get next symbol from input stream (wrapper).                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

enum rctoken getsymbol(void)
{
#ifdef PODEBUG
    static int verified = 0;

    if (!verified)
    {
        int i, j;

        for (i = 0; i < NELEMS(kwtab)-1; i++)
            for (j = i+1; j < NELEMS(kwtab); j++)
                if (_stricmp(kwtab[i].name, kwtab[j].name) > 0)
                    printf("bug-bug: %s -- %s\n", kwtab[i].name, kwtab[j].name);

        verified = 1;
    }
#endif

    return (s.sym.tok = nextsymbol());
}

/****************************************************************************
 *                                                                          *
 * Function: nextsymbol                                                     *
 *                                                                          *
 * Purpose : Get next symbol from input stream.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           98-07-01  Bugfix: push back second comma, if two in a row.     *
 *           00-02-12  C_LBRACE and C_RBRACE cases added.                   *
 *           01-11-23  C_PLUS case added.                                   *
 *           03-08-02  C_BORLAND case added.                                *
 *           03-08-13  Added '.' to alpha() case for relative paths.        *
 *           03-09-08  Added '_' to alpha() case for identifiers!           *
 *                                                                          *
 ****************************************************************************/

static enum rctoken nextsymbol(void)
{
    int ncomma = 0;
    char ch;

    while ((ch = nextchar()) != '\0')
    {
        switch (classify(ch))
        {
            case ' ':
            case '\r':
            case '\t':
                continue;

            case '\n':
                bNewLine = TRUE;
                s.line++;
                continue;

            case ',':
                if (++ncomma > 1)  /* ?,,? -> ?,0,? */
                {
                    putchr(ch);  /* 98-07-01 */
                    s.sym.num = 0;
                    return C_NUMBER;
                }
                continue;

            case ALPHA:
            case '#':
            case '\\':
            case '.':
            case '_':  /* 03-09-08 */
                return alpha(ch);

            case DIGIT:
            case '-':
                return digit(ch);

            case '"':
                return string(C_STRING);

            case '\'':
                return borland(C_BORLAND);

            case '|':
                return C_OR;

            case '+':
                return C_PLUS;

            case '{':
                return C_LBRACE;

            case '}':
                return C_RBRACE;

            default:
                return C_NOTHING;
        }
    }

    return C_EOF;
}

/****************************************************************************
 *                                                                          *
 * Function: nextchar                                                       *
 *                                                                          *
 * Purpose : Get next character from input stream.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static char nextchar(void)
{
    static char inbuf[INS];
    char ch;

    ch = s.ch;
    if (ch)
    {
        s.ch = '\0';
        return ch;
    }

    if (s.nchars == 0)
    {
        s.nchars = pp_read((uchar_t *)(s.ip = inbuf), INS);
        if (s.nchars == 0) return '\0';
    }

    return (s.nchars--, *s.ip++);
}

/****************************************************************************
 *                                                                          *
 * Function: classify                                                       *
 *                                                                          *
 * Purpose : Classify char from input stream.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static char classify(char ch)
{
    if (IsCharAlphaA(ch)) return ALPHA;
    if (isdigit(ch)) return DIGIT;

    return ch;
}

/****************************************************************************
 *                                                                          *
 * Function: alpha                                                          *
 *                                                                          *
 * Purpose : Handle sequence of alpha-numeric characters; this includes     *
 *           separating symbols from reserved words.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static enum rctoken alpha(char ch)
{
    enum rctoken tok;
    int i;

    /*
     * Special case: Unicode strings.
     */
    if (ch == 'L')
    {
        char ch;

        if ((ch = nextchar()) == '"')
            return string(C_UNISTRING);
        else
            putchr(ch);
    }

    /*
     * Scan for identifier or filename characters.
     */
    for (i = 0; i < 256 && idtab[ch]; i++)
    {
        s.sym.str[i] = ch;
        ch = nextchar();
    }
    s.sym.str[i] = '\0';
    putchr(ch);

    /*
     * Scan for keywords.
     */
    tok = lookup(s.sym.str);

    if (tok)
    {
       /*
        * So that menu attributes are NOT reserved words.
        */
       if ( bNewLine == TRUE && bDialog == FALSE )
       {
          bNewLine = FALSE;

          switch (tok)
          {
            case C_CHECKED:
            case C_GRAYED:
            case C_HELP:
            case C_INACTIVE:
            case C_MENUBARBREAK:
            case C_MENUBREAK:

            case C_CHARACTERISTICS:
            /* case C_LANGUAGE: */
            case C_VERSION:
            case C_CAPTION:
            case C_CLASS:
            case C_FONT:
            case C_MENU:
            case C_STYLE:
            case C_EXSTYLE:
                return C_SYMBOL;
          }
       }

       bNewLine = FALSE;
       return tok;
    }

    bNewLine = FALSE;
    return C_SYMBOL;
}

/****************************************************************************
 *                                                                          *
 * Function: digit                                                          *
 *                                                                          *
 * Purpose : Handle sequence of numeric characters.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static enum rctoken digit(char ch)
{
    long val = 0;
    int base = 10;
    int neg = 0;
    int i;

    // 2003-Sep-12 Ron Pinkas added to allow Identifiers starting with a Digit (see below).
    int iStrLen = 0;
    char asString[256];

    asString[iStrLen++] = ch;

    if (ch == '-')
    {
        neg = 1;
        ch = nextchar();
    }

    if (ch == '0')
    {
        base = 8;
        ch = nextchar();

        if (ch == 'x' || ch == 'X')
        {
            asString[iStrLen++] = ch;
            base = 16;
            ch = nextchar();
        }
    }

    for (;;)
    {
        asString[iStrLen++] = ch;

        if ((i = getdigit(ch)) < 0)
            break;

        val *= base;
        val += i;

        ch = nextchar();
    }

    // 2003-Sep-12 Ron Pinkas added to allow Identifiers starting with a Digit.
    if (idtab[ch] && asString[0] != '-')
    {
        if (ch == 'l' || ch == 'L' ||
            ch == 'u' || ch == 'U')
        {
           char lastChr = ch;

           ch = nextchar();
           putchr(ch);

           // 2004-Jan-28 Pelle added check for ',' '|' and '+' - this can be improved!
           if (isspace(ch) || ch == ',' || ch == '|' || ch == '+')
           {
              ch = lastChr;
              goto NoSymbol;
           }

           ch = lastChr;
        }

        strcpy( s.sym.str, asString );

        for (i = iStrLen; i < 256 && idtab[ch]; i++)
        {
            s.sym.str[i] = ch;
            ch = nextchar();
        }
        s.sym.str[i] = '\0';
        putchr(ch);

        return C_SYMBOL;
    }

  NoSymbol:

    s.sym.is_long = (ch == 'l' || ch == 'L');

    if (ch == 'l' || ch == 'L' ||
        ch == 'u' || ch == 'U')
        ch = nextchar();

    putchr(ch);

    s.sym.num = (neg) ? -val : val;

    return C_NUMBER;
}

/****************************************************************************
 *                                                                          *
 * Function: string                                                         *
 *                                                                          *
 * Purpose : Handle quoted strings: "string".                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static enum rctoken string(enum rctoken tok)
{
    char *p;
    char ch;

    p = s.sym.str;
    while ((ch = nextchar()) != '\0')
    {
        if (ch == '\\')
        {
            int i;
            char n = 0;

            if ( bWantsFile )
            {
               *p++ = '/';
               continue;
            }

            ch = nextchar();
            if ((i = getdigit(ch)) >= 0 && i <= 7)
            {
                n = i;
                ch = nextchar();
                if ((i = getdigit(ch)) >= 0 && i <= 7)
                {
                    n = n * 8 + i;
                    ch = nextchar();
                    if ((i = getdigit(ch)) >= 0 && i <= 7)
                    {
                        n = n * 8 + i;
                        ch = nextchar();
                    }
                }

                putchr(ch);
                ch = n;
            }
            else if (ch == 'x')
            {
                ch = nextchar();
                while ((i = getdigit(ch)) >= 0 && i <= 15)
                {
                    n = n * 16 + i;
                    ch = nextchar();
                }

                putchr(ch);
                ch = n;
            }
            else if (ch == 'b') ch = '\b';
            else if (ch == 'f') ch = '\f';
            else if (ch == 'n') ch = '\n';
            else if (ch == 'r') ch = '\r';
            else if (ch == 't') ch = '\t';
            else if (ch == 'v') ch = '\v';
            else if (ch == '?') ch = '?';
            else if (ch == '"') ch = '"';
            else if (ch == '\'') ch = '\'';
            else if (ch == '\\') ch = '\\';
            else
            {
                *p++ = '\\';
            }
        }
        else if (ch == '"')
        {
            ch = nextchar();
            if (ch != '"')
            {
                putchr(ch);
                break;
            }
        }

        *p++ = ch;
    }

    *p = '\0';

    s.sym.nchars = (p - s.sym.str);

    return tok;
}

/****************************************************************************
 *                                                                          *
 * Function: borland                                                        *
 *                                                                          *
 * Purpose : Handle Borland crap: ' hex hex hex hex hex ... '.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

static enum rctoken borland(enum rctoken tok)
{
    char *p;
    char ch;

    p = s.sym.str;
    while ((ch = nextchar()) != '\0' && ch != '\'')
        *p++ = ch;
    *p = '\0';
    s.sym.nchars = (p - s.sym.str);

    return tok;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup                                                         *
 *                                                                          *
 * Purpose : Binary search routine for keywords.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static enum rctoken lookup(const char *name)
{
    int lo = 0;
    int hi = NELEMS(kwtab)-1;

    while (hi >= lo)
    {
        int this = (lo + hi) / 2;
        int n;

        n = _stricmp(name, kwtab[this].name);

        if (n < 0)
            hi = this - 1;
        else if (n > 0)
            lo = this + 1;
        else
            return kwtab[this].tok;
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: setup_scanner                                                  *
 *                                                                          *
 * Purpose : Init the identifier lookup table.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void setup_scanner(void)
{
    int i;

    for (i = 0; i < 256; i++)
        idtab[i] = IsCharAlphaNumericA((uchar_t)i) ? 1 : 0;

    idtab['_'] =
    idtab['#'] =
    idtab['$'] =
    idtab['%'] =
    idtab['-'] =
    idtab['@'] =
    idtab['~'] =
    idtab['!'] =
    idtab['('] =
    idtab[')'] =
    /* idtab['{'] = */
    /* idtab['}'] = */
    idtab['^'] =
    idtab['&'] =
    /* idtab['+'] = */
    idtab['['] =
    idtab[']'] =
    idtab['.'] =
    idtab[':'] =
    idtab['\\'] = 1;
}

