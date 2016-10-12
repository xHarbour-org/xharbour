/****************************************************************************
 *                                                                          *
 * File    : cpp.h                                                          *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; preprocessor; constants & defs.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           03-06-27  New codes for EOB and EOFC (avoid using high bit).   *
 *                                                                          *
 ****************************************************************************/

#ifndef _CPP_H
#define _CPP_H

#ifdef __cplusplus
extern "C" {
#endif

#define INS   32768         /* input buffer size */
#define OBS   4096          /* outbut buffer size */
#define NARG  32            /* max number of arguments to a macro */
#define NIF   32            /* depth of nesting of #if */

#ifndef EOF
#define EOF  (-1)
#endif

#ifndef NULL
#define NULL  0
#endif

enum toktype
{
    END, UNCLASS, NAME, NUMBER, STRING, CCON, NL, WS, DSHARP,
    EQ, NEQ, LEQ, GEQ, LSH, RSH, LAND, LOR, PPLUS, MMINUS,
    ARROW, SBRA, SKET, LP, RP, DOT, AND, STAR, PLUS, MINUS,
    TILDE, NOT, SLASH, PCT, LT, GT, CIRC, OR, QUEST,
    COLON, ASGN, COMMA, SHARP, SEMIC, CBRA, CKET,
    ASPLUS, ASMINUS, ASSTAR, ASSLASH, ASPCT, ASCIRC, ASLSH,
    ASRSH, ASOR, ASAND, ELLIPS,
    DSHARP1, NAME1, DEFINED, UMINUS
};

enum kwtype
{
    KIF, KIFDEF, KIFNDEF, KELIF, KELSE, KENDIF, KINCLUDE, KDEFINE,
    KUNDEF, KLINE, KERROR, KPRAGMA, KDEFINED,
    KLINENO, KFILE, KDATE, KTIME, KSTDC, KEVAL
};

#define ISDEFINED   1       /* has #defined value */
#define ISKW        2       /* is PP keyword */
#define ISUNCHANGE  4       /* can't be #defined in PP */
#define ISMAC       8       /* builtin macro, e.g. __LINE__ */

#define EOB   27 /*0xFE*/   /* sentinel for end of input buffer */
#define EOFC  26 /*0xFD*/   /* sentinel for end of input file */
#define XPWS  1             /* token flag: white space to assure token sep. */

typedef struct _TOKEN
{
    uchar_t type;           /* enum toktype */
    uchar_t flag;           /* XPWS */
    ushort_t hideset;
    uint_t wslen;
    uint_t len;
    uchar_t *t;
} TOKEN;

typedef struct _TOKENROW
{
    TOKEN *tp;              /* current one to scan */
    TOKEN *bp;              /* base (allocated value) */
    TOKEN *lp;              /* last+1 token used */
    int max;                /* number allocated */
} TOKENROW;

typedef struct _SOURCE
{
    const char *filename;   /* name of the source file */
    int line;               /* current line number */
    int lineinc;            /* adjustment for \\n lines */
    uchar_t *inb;           /* input buffer */
    uchar_t *inp;           /* input pointer */
    uchar_t *inl;           /* end of input */
    HANDLE hf;              /* input source */
    int ifdepth;            /* conditional nesting in include */
    struct _SOURCE *next;   /* stack for #include */
} SOURCE;

typedef struct _NLIST
{
    struct _NLIST *next;    /* next hash bucket */
    uchar_t *name;          /* symbol name */
    int len;                /* length of symbol name */
    TOKENROW *vp;           /* value as macro */
    TOKENROW *ap;           /* list of argument names, if any */
    char val;               /* value as preprocessor name */
    char flag;              /* is defined, is pp name */
} NLIST;

typedef struct _INCLUDE
{
    struct _INCLUDE *next;  /* next include path */
    const char *path;       /* search path */
    uint_t flags;           /* INC_?? flags */
} INCLUDE;

/* INCLUDE flags */
#define INC_STDPLACE  0x01
#define INC_DEFAULT   0x02

#define new(t)  (t *)my_alloc(sizeof(t))
#define quicklook(a,b)  (namebit[(a) & 63] & (1 << ((b) & 31)))
#define quickset(a,b)   namebit[(a) & 63] |= (1 << ((b) & 31))
extern ulong_t namebit[63+1];

/* cpp.c */
void pp_init(void);
void pp_define(const char *);
void pp_start(const char *);
bool_t pp_process(TOKENROW *);
void pp_error(ulong_t, ...);

/* lex.c */
void setup_lexfsm(void);
void fixup_lexfsm(void);
int get_tokens(TOKENROW *, bool_t);
SOURCE *set_source(const char *, HANDLE, const char *);
void unset_source(void);

/* nlist.c */
void setup_kwtab(void);
NLIST *pp_lookup(TOKEN *, bool_t);

/* tokens.c */
void make_tokenrow(int, TOKENROW *);
TOKEN *grow_tokenrow(TOKENROW *);
int compare_tokens(TOKENROW *, TOKENROW *);
void insertrow(TOKENROW *, int, TOKENROW *);
void makespace(TOKENROW *);
void move_tokenrow(TOKENROW *, TOKENROW *);
void adjustrow(TOKENROW *, int);
TOKENROW *copy_tokenrow(TOKENROW *, TOKENROW *);
TOKENROW *norm_tokenrow(TOKENROW *);
void emptyrow(TOKENROW *);
void peek_tokens(TOKENROW *, char *);
bool_t put_tokens(TOKENROW *);
size_t pp_read(uchar_t *, size_t);
char *outnum(char *, int);

/* macro.c */
void define(TOKENROW *);
void doadefine(TOKENROW *);
void expandrow(TOKENROW *, const char *);

/* eval.c */
long eval(TOKENROW *, int);

/* include.c */
void include(TOKENROW *);
bool_t genline(void);
void setup_include(void);
void add_include(const char *, uint_t, bool_t);
void delete_includes(void);
char *search_include(const char *, bool_t);

/* hideset.c */
void setup_hideset(void);
bool_t check_hideset(int, NLIST *);
int new_hideset(int, NLIST *);
int union_hideset(int, int);
void print_hideset(int);

#define rowlen(tokrow)  ((tokrow)->lp - (tokrow)->bp)

/* Globals */
extern char outbuf[];
extern char *outpp;
extern SOURCE *cursource;
extern char *curtime;
extern int incdepth;
extern TOKENROW maintr;
extern NLIST *kwdefined;

#define tstrcpy(s)  (strlen(s) ? strcpy(my_alloc((strlen(s)+1) * sizeof(char)), (s)) : NULL)

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _CPP_H */
