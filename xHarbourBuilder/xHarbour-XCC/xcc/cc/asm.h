/****************************************************************************
 *                                                                          *
 * File    : asm.h                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic Assembler; Constants and definitions.  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-21  Machine specific stuff moved to X86.H and ARM.H      *
 *           01-02-23  Character mapping through ascmap[] added.            *
 *                                                                          *
 ****************************************************************************/

#ifndef _ASM_H
#define _ASM_H

#ifdef __cplusplus
extern "C" {
#endif

/* character mapping bits */
enum { ASNUM=1, ASCHR=2, ASID1=4, ASID2=8, ASHEX=16 };

/*
 * isidstart matches any character that may start an identifier, and
 * isidchar matches any character that may appear at places other than
 * the start of an identifier. E.g. a period may only appear at the
 * start of an identifier (for local labels), whereas a number may
 * appear anywhere *but* at the start.
 */
#define isidstart(c)  ((ascmap[(uchar_t)(c)] & ASID1) != 0)
#define isidchar(c)   ((ascmap[(uchar_t)(c)] & ASID2) != 0)

/*
 * Ditto for numeric constants.
 */
#define isnumstart(c)  ((ascmap[(uchar_t)(c)] & ASNUM) != 0)
#define isnumchar(c)   ((ascmap[(uchar_t)(c)] & (ASNUM|ASCHR)) != 0)

/* This returns the numeric value of a given 'digit' */
#define numvalue(c)  (((c) >= 'a') ? (c)-'a'+10 : ((c) >= 'A') ? (c)-'A'+10 : (c)-'0')

#define NO_SEG  -1L             /* null segment value */
#define SEG_ABS  0x40000000L    /* mask for far-absolute segments */
#define SEG_SYM  0x20000000L    /* it's not a segment, it's a symbol! */

/*
 * Output format driver interface.
 */
typedef struct _OUTFMT {
    uint_t binary_mode: 1;
    /**/
    void (*init)(void);
    void (*assemble)(void);
    /**/
    void (*filename)(char *, const char *);
    void (*filebeg)(void);
    void (*fileend)(bool_t);
    void (*output)(long, const void *, ulong_t, long);
    long (*symbol)(const char *, long, long, int);
    long (*segment)(const char *, const char *, int, int *);
    long (*segbase)(long);
    bool_t (*directive)(const char *, const char *, int);
    void (*lineno)(long, int);
} OUTFMT;

/*
 * Values for the 'type' parameter to an output function. Each one
 * must have the actual number of *bytes* added to it.
 *
 * Exceptions are OUT_RELxADR, which denote an x-byte relocation
 * which will be a relative jump. For this we need to know the
 * distance in bytes from the start of the relocated record until
 * the end of the containing instruction. *This* is what is stored
 * in the size part of the parameter, in this case.
 *
 * Also OUT_RESERVE denotes reservation of N bytes of BSS space,
 * and the contents of the "data" parameter is irrelevant.
 *
 * The "data" parameter for the output function points to a "long",
 * containing the address in question, unless the type is
 * OUT_RAWDATA, in which case it points to an "unsigned char"
 * array.
 */
#define OUT_RAWDATA  0x00000000UL
#define OUT_ADDRESS  0x10000000UL
#define OUT_REL2ADR  0x20000000UL
#define OUT_REL3ADR  0x30000000UL
#define OUT_REL4ADR  0x40000000UL
#define OUT_RESERVE  0x50000000UL
#define OUT_TYPMASK  0xF0000000UL
#define OUT_SIZMASK  0x0FFFFFFFUL

/*
 * Values for symbol type.
 */
enum {
    SYMDEF_LOCAL,
    SYMDEF_GLOBAL,
    SYMDEF_COMMON,
    SYMDEF_EXTERN
};

/*
 * Label description.
 */
typedef struct _LABELDEF {
    long segment;           /* segment (first!) */
    long offset;            /* offset */
    long symbol;            /* symbol tag from the output driver */
    char *name;             /* name */
    uint_t defined: 1;      /* true if defined */
    uint_t global: 1;       /* true if global */
    uint_t external: 1;     /* true if external */
    uint_t normal: 1;       /* true if not special */
    uint_t referenced: 1;   /* true if referenced */
} LABELDEF;

/*
 * The return value from the scanner is always a copy of the
 * 'type' field in the structure.
 */
typedef struct _TOKENVAL {
    int type;
    int inttwo;
    intmax_t integer;       /* must be >= 64 bits */
    char *charptr;
} TOKENVAL;

/*
 * Token types returned by the scanner, in addition to ordinary
 * ASCII character values, and zero for end-of-string.
 */
enum {
    /* token types, other than chars */
    TOK_INVALID = -1,                   /* a placeholder value */
    TOK_EOI = 0,                        /* end of input */
    TOK_ID = 256, TOK_NUM, TOK_REG, TOK_INSN,  /* major token types */
    TOK_ERRNUM,                         /* numeric constant with error in */
    TOK_HERE, TOK_BASE,                 /* $ and $$ */
    TOK_SPECIAL,                        /* BYTE, WORD, DWORD, FAR, NEAR */
    TOK_PREFIX,                         /* LOCK, REPNZ, TIMES */
    TOK_SHL, TOK_SHR,                   /* << and >> */
    TOK_SDIV, TOK_SMOD,                 /* // and %% */
    TOK_FLOAT,                          /* floating-point constant */
    TOK_SIZEOF                          /* SIZEOF */
};

typedef struct _LOCATION {
    long segment;
    long offset;
} LOCATION;

/*
 * Expression-evaluator datatype. Expressions, within the
 * evaluator, are stored as an array of these beasts, terminated by
 * a record with type==0. Mostly, it's a vector type: each type
 * denotes some kind of a component, and the value denotes the
 * multiple of that component present in the expression.
 */
typedef struct _EXPR {
    long type;          /* a register, or EXPR_xxx */
    void *vp;           /* NULL, LABELDEF* or SYMBOL* (inline assembler) */
    intmax_t value;     /* must be >= 64 bits */
} EXPR;

/*
 * The evaluator can also return hints about which of two registers
 * used in an expression should be the base register. See also the
 * 'operand' structure.
 */
typedef struct _EVAL_HINTS {
    int base;
    int type;
} EVAL_HINTS;

/*
 * Special values for expr->type. ASSUMPTION MADE HERE: the number
 * of distinct register names (i.e. possible "type" fields for an
 * expr structure) does not exceed 123 (EXPR_REG_START through
 * EXPR_REG_END).
 */
#define EXPR_REG_START  1
#define EXPR_REG_END    124
#define EXPR_UNKNOWN    125L            /* for forward references */
#define EXPR_SIMPLE     126L
#define EXPR_SEGBASE    127L

/* ? */
enum {
    EOT_NOTHING, EOT_DB_STRING, EOT_DB_NUMBER
};

/* special EA flags */
enum {
    EAF_BYTEOFFS = 1,               /* force offset part to byte size */
    EAF_WORDOFFS = 2                /* force offset part to [d]word size */
};

/* values for 'hinttype' */
enum {
    EAH_NOHINT = 0,                 /* no hint at all - our discretion */
    EAH_MAKEBASE = 1,               /* try to make given reg the base */
    EAH_NOTBASE = 2                 /* try *not* to make reg the base */
};

/* standard instruction operand */
typedef struct _STDOPAND {
    long type;                      /* type of operand */
    int addr_size;                  /* 0 means default; 16; 32 */
    int basereg, indexreg, scale;   /* registers and scale involved */
    int hintbase, hinttype;         /* hint as to real base register */
    long segment;                   /* immediate segment, if needed */
    long offset;                    /* any immediate number */
    void *vp;                       /* NULL, LABELDEF* or SYMBOL* */
    int eaflags;                    /* special EA flags */
    int opflags;                    /* see OPFLAG_* defines below */
    int shiftflag;                  /* ASR, LSL, LSR, ROR, RRX [ARM only] */
    bool_t bracket;                 /* For bracketed operands [ARM only] */
    bool_t minus;                   /* +/- reg operands [ARM only] */
} STDOPAND;

#define OPFLAG_FORWARD  1           /* operand is a forward reference */
#define OPFLAG_EXTERN   2           /* operand is an external reference */
#define OPFLAG_OFFSET   4           /* operand is an OFFSET (M$ style) */

/* extended instruction operand */
typedef struct _EXTOPAND {
    struct _EXTOPAND *next;         /* linked list */
    long type;                      /* defined above */
    char *stringval;                /* if it's a string, then here it is */
    int stringlen;                  /* ... and here's how long it is */
    long segment;                   /* if it's a number/address, then... */
    intmax_t offset;                /* ... it's given here ... */
    void *vp;                       /* NULL, LABELDEF* or SYMBOL* */
} EXTOPAND;

#define MAXPREFIX  4

/* instruction */
typedef struct _INSN {
    char *label;                    /* the label defined, or NULL */
    int prefixes[MAXPREFIX];        /* instruction prefixes, if any */
    int nprefix;                    /* number of entries in above */
    int opcode;                     /* the opcode - not just the string */
    int condition;                  /* the condition code */
    int nopands;                    /* how many operands? 0-4 (more if db et al) */
    STDOPAND aops[4];               /* the operands, defined as above */
    EXTOPAND *eops;                 /* extended operands */
    bool_t eops_float;              /* true if DD and floating */
    bool_t forw_ref;                /* is there a forward reference? */
    long times;                     /* repeat count (TIMES prefix) */
} INSN;

/* instruction template */
struct ITEMPLATE {
    int opcode;                     /* the token, passed from "xxxparse.c" */
    int nopands;                    /* number of operands */
    long aops[4];                   /* bit flags for operand types */
    char *code;                     /* the code it assembles to */
    ulong_t flags;                  /* some flags */
};

/*
 * Current Assembler interface.
 */
typedef struct _ASSEMBLER {
    struct {  /* scanner */
        int (*scan)(TOKENVAL *);
        const char * (*getptr)(void);
        void (*setptr)(const char *);
        void (*reset)(void);
        void (*cleanup)(void);
        char * (*asmtext)(INSN *, SYMBOL **, long *);
        int (*asmreg)(const char *);
    } scanner;
    struct {  /* parser */
        void (*parseinline)(const char *, char **, char **, SYMBOL **, long *);
        void (*parse)(int, const char *, INSN *);
        void (*cleanup)(INSN *);
    } parser;
    struct {  /* code emitter */
        bool_t (*cpulevel)(const char *);
        void (*emitinline)(const char *, const char *, SYMBOL *, long);
        long (*emit)(int, long, long, long, int, INSN *);
    } emitter;
} ASSEMBLER;

/* asm.c */
void x86_init(void);
void arm_init(void);
void assembler(void);
long seg_alloc(void);
intmax_t readnum(const char *, bool_t *);
intmax_t readstrnum(const char *, int);
bool_t is_simple(EXPR *);
bool_t is_really_simple(EXPR *);
bool_t is_reloc(EXPR *);
bool_t is_unknown(EXPR *);
bool_t is_just_unknown(EXPR *);
intmax_t reloc_value(EXPR *);
long reloc_seg(EXPR *);

/* evaluate.c */
EXPR *asmexp(TOKENVAL *, int *, int, EVAL_HINTS *, bool_t);
void asmexp_cleanup(void);

/* float.c */
int asmflt(const char *, long, uchar_t *, int);

/* labels.c */
void asmlab_init(void);
void asmlab_cleanup(void);
bool_t asmlab_lookup(const char *, long *, long *, LABELDEF **);
void asmlab_define(const char *, long, long, bool_t, bool_t);
void asmlab_redefine(const char *, long, long);
void asmlab_phase_check(const char *, long, long);
void asmlab_define_common(const char *, long, long);
void asmlab_declare_global(const char *);
bool_t asmlab_is_extern(const char *);

/* x86scan.c */
void x86_scanner_init(void);

/* x86parse.c */
void x86_parser_init(void);

/* x86emit.c */
void x86_emitter_init(void);

/* armscan.c */
void arm_scanner_init(void);

/* armparse.c */
void arm_parser_init(void);

/* armemit.c */
void arm_emitter_init(void);

/* Global variables */
extern const uchar_t ascmap[256];
extern ASSEMBLER as;
extern LOCATION location;
extern LIST *aslist;
extern LIST *curp;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _ASM_H */
