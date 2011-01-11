/****************************************************************************
 *                                                                          *
 * File    : lcc.h                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; constants and definitions.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-09-18  Changed interface flag mulops_calls into function.   *
 *           04-07-15  Rewritten support for intrinsic functions.           *
 *           04-10-06  Bugfix: optional argument can't be in SYMBOL union!  *
 *           04-12-18  Added interface flag wants_optb.                     *
 *                                                                          *
 ****************************************************************************/

#ifndef _LCC_H
#define _LCC_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __POCC__
#include <stdint.h>
#else /* MSC */
typedef signed __int64 intmax_t;
typedef unsigned __int64 uintmax_t;
#define INTMAX_MIN _I64_MIN
#define INTMAX_MAX _I64_MAX
#define UINTMAX_MAX _UI64_MAX
#endif

typedef unsigned short widechar_t;

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

typedef struct _NODE NODE;
typedef struct _TREE TREE;
typedef struct _LIST LIST;
typedef struct _CODE CODE;
typedef struct _SYMBOL SYMBOL;
typedef struct _TABLE TABLE;
typedef struct _FIELD FIELD;
typedef struct _TYPE TYPE;
typedef struct _SWTCH SWTCH;

#include "msg.h"
#include "cpp.h"
#include "asm.h"
#include "config.h"

/*
 * Definitions to manage a dynamic random access array of longs
 * which may grow in size to be more than the largest single
 * malloc'able chunk.
 */

#define RAA_BLKSIZE  4096       /* this many longs allocated at once */
#define RAA_LAYERSIZE  1024     /* this many *pointers* allocated */

typedef struct _RAA RAA;
typedef union _RAA_UNION RAA_UNION;
typedef struct _RAA_LEAF RAA_LEAF;
typedef struct _RAA_BRANCH RAA_BRANCH;

struct _RAA {
    /*
     * Number of layers below this one to get to the real data.
     * 0 means this structure is a leaf, holding RAA_BLKSIZE real
     * data items; 1 and above mean it's a branch, holding
     * RAA_LAYERSIZE pointers to the next level branch or leaf
     * structures.
     */
    int layers;
    /*
     * Number of real data items spanned by one position in the
     * 'data' array at this level. This number is 1, trivially,
     * for a leaf (level 0): for a level 1 branch it should be
     * RAA_BLKSIZE, and for a level 2 branch it's RAA_LAYERSIZE*RAA_BLKSIZE.
     */
    long stepsize;
    union _RAA_UNION {
        struct _RAA_LEAF {
            long data[RAA_BLKSIZE];
        } l;
        struct _RAA_BRANCH {
            struct _RAA *data[RAA_LAYERSIZE];
        } b;
    } u;
};

/*
 * Definitions to manage a dynamic sequential-access array, under the
 * same restriction on maximum mallocable block. This array may be
 * written to in two ways: a contiguous chunk can be reserved of a
 * given size, and a pointer returned, or single-byte data may be
 * written. The array can also be read back in the same two ways:
 * as a series of big byte-data blocks or as a list of structures
 * of a given size.
 */

typedef struct _SAA SAA;

struct _SAA {
    /*
     * members 'end' and 'elem_len' are only valid in first link
     * in list; 'rptr' and 'rpos' are used for reading.
     */
    SAA *next, *end, *rptr;
    long elem_len, length, posn, start, rpos;
    char *data;
};

#ifdef PROF
typedef struct _EVENTS {
    LIST *blockentry;
    LIST *blockexit;
    LIST *entry;
    LIST *exit;
    LIST *returns;
    LIST *points;
    LIST *calls;
    LIST *end;
} EVENTS;
#endif

/* SEH block information */
typedef struct _SEH {
    int index;              /* seh level */
    int previndex;          /* previous seh level */
    int level;              /* scope level */
    int label;              /* leave stmt label */
    int type;               /* type: SEH_EXCEPT or SEH_FINALLY */
    SYMBOL *sym;            /* symbol for _exception_code() value */
    struct {
        int try0lab;        /* start try block label (ARM) */
        int try1lab;        /* end try block label (ARM) */
        int evallab;        /* filter expression label */
        int xcptlab;        /* handler label */
        int exitlab;        /* common tail label */
    } x;
} SEH;

/* SEH intrinsic information */
typedef struct _SEHINFO {
    char *codename;         /* hashed "_exception_code" ptr */
    char *infoname;         /* hashed "_exception_info" ptr */
    uint_t acceptcode: 1;   /* true if _exception_code() is acceptable */
    uint_t acceptinfo: 1;   /* true if _exception_info() is acceptable */
    SYMBOL *codesym;        /* symbol for _exception_code() value */
    SYMBOL *infosym;        /* symbol for _exception_info() ptr */
} SEHINFO;

/* C runtime intrinsic information */
typedef struct _INTRINSIC {
    char *name;             /* hashed name ptr */
    TYPE **prototype;       /* prototype from C #include */
    TYPE *rty;              /* return type */
    enum {
        INTRIN_STRLEN = 1,
        INTRIN_STRCPY,
        INTRIN_STRCMP,
        INTRIN_MEMCPY,
        INTRIN_MEMSET,
        INTRIN_WCSLEN,
        INTRIN_WCSCPY,
        INTRIN_ROTL,
        INTRIN_ROTR,
        INTRIN_LROTL,
        INTRIN_LROTR,
        INTRIN_INP,
        INTRIN_INPD,
        INTRIN_INPW,
        INTRIN_OUTP,
        INTRIN_OUTPD,
        INTRIN_OUTPW,
        INTRIN_RDTSC,
        INTRIN_BSWAP,
        INTRIN_CPUID,
        INTRIN_ABS,
        INTRIN_LABS,
        INTRIN_FABS,
        INTRIN_FABSF,
        INTRIN_SQRT,
        INTRIN_SQRTF,
        INTRIN_TAN,
        INTRIN_TANF,
        INTRIN_ATAN,
        INTRIN_ATANF,
        INTRIN_EXP,
        INTRIN_EXPF,
        INTRIN_LOG,
        INTRIN_LOGF,
        INTRIN_LOG10,
        INTRIN_LOG10F,
        INTRIN_SIN,
        INTRIN_SINF,
        INTRIN_COS,
        INTRIN_COSF,
        INTRIN_LOG_LOG10_LOG2,
        INTRIN_LOGF_LOG10F_LOG2F,
        INTRIN_SIN_COS,
        INTRIN_SINF_COSF,
    } id;
    enum {
        INTRIN_MINSPACE = 0x0001,
        INTRIN_MAXSPEED = 0x0002,
        INTRIN_FLTOPT = 0x0004,
        INTRIN_SIDE_EFFECTS = 0x0008,
        INTRIN_ENABLED = 0x0010,        /* enabled by user */
        INTRIN_DISABLED = 0x0020,       /* disabled by user */
    } flags;
} INTRINSIC;

/* generic lists */
typedef struct _LIST {
    void *data;             /* element */
    LIST *link;             /* next node */
} LIST;

/* source coordinates */
typedef struct _COORDINATE {
    char *file;             /* file name */
    uint_t x;               /* column position in file */
    uint_t y;               /* row position in file */
} COORDINATE;

typedef struct _XTYPE {
    uint_t printed: 1;
    uint_t marked;
    ushort_t typeno;
    ushort_t xtypeno;
    void *xt;
} XTYPE;

/* data types */
typedef struct _TYPE {
    int op;                 /* FLOAT_/SHORT_/STRUCT/ENUM/... */
    TYPE *type;
    int align;              /* data alignment */
    int size;               /* data size */
    union {
        SYMBOL *sym;
        struct {
            uint_t calltype: 8;     /* calling convention */
            uint_t oldstyle: 1;     /* old-style parameter list? */
            TYPE **prototype;       /* prototype, i.e. function arguments */
            TREE **optparam;        /* optional argument values */
        } fcn;
        struct {
            SYMBOL *sym;            /* VLA temp symbol */
            TREE *e;                /* VLA size expression */
        } arr;
    } u;
    XTYPE x;
};

/* struct/union fields */
typedef struct _FIELD {
    char *name;             /* field name */
    TYPE *type;             /* data type */
    int offset;             /* field offset */
    int xoffset;            /* true field offset (adjusted for anonymous members) */
    short bitsize;
    short lsb;
    FIELD *link;            /* next field */
} FIELD;

/* constant values etc */
typedef union _VALUE {
    intmax_t i;             /* previously: signed long */
    uintmax_t u;            /* previously: unsigned long */
    long double d;
    void *p;
    void (*g)(void);
} VALUE;

/* extended attributes */
typedef struct _ATTR {
    uint_t inlined: 1;      /* true if function should be inlined */
    uint_t naked: 1;        /* true if no prolog/epilog */
    uint_t dllimport: 1;    /* true if imported */
    uint_t dllexport: 1;    /* true if exported */
    uint_t noreturn: 1;     /* true if function never returns */
} ATTR;

/* symbol table entries */
typedef struct _SYMBOL {
    char *name;             /* name */
    int scope;              /* scope level */
    COORDINATE src;         /* definition coordinate */
    SYMBOL *up;             /* next symbol in this or outer scope */
#ifdef XREF
    LIST *uses;             /* array of COORDINATE *'s for uses */
#endif
    ATTR attr;              /* extended attributes */
    int sclass;             /* storage class */
    uint_t structarg: 1;    /* true if special struct argument (!wants_argb) */
    uint_t addressed: 1;    /* true if its address is taken */
    uint_t computed: 1;     /* true if address computation identifier */
    uint_t temporary: 1;    /* true if a temporary */
    uint_t generated: 1;    /* true if a generated identifier */
    uint_t defined: 1;      /* true if defined */
    uint_t fetched: 1;      /* true if local is used or label has been walked (04-03-10) */
    uint_t assigned: 1;     /* true if variable is assigned (04-03-23) */
    uint_t intrinsic: 1;    /* true if SEH intrinsic version */
    uint_t init: 1;         /* true if special startup function (03-06-17) */
    uint_t exit: 1;         /* true if special exit function (03-06-17) */
    uint_t hasalloca: 1;    /* true if special _alloca function is used (03-09-01) */
    uint_t assembler: 1;    /* true if inline assembler is used (03-09-01) */
    TYPE *type;             /* data type */
    float ref;              /* weighted number of references */
    union {
        struct {            /* label: */
            int label;          /* id, from make_label() */
            SYMBOL *equatedto;  /* equated symbol */
            int funcvla;        /* seen VLA declarations - for jump check */
        } lab;
        struct {            /* struct/union: */
            uint_t cfields: 1;  /* const fields exist */
            uint_t vfields: 1;  /* volatile fields exist */
#ifdef XREF
            TABLE *ftab;        /* used by xref */
#endif
            FIELD *flist;       /* list of fields */
        } s;
        int value;              /* enum value */
        SYMBOL **idlist;        /* enum ID list */
        struct {            /* basic type limits: */
            VALUE min;          /* min */
            VALUE max;          /* max */
        } limits;
        struct {            /* constant: */
            VALUE v;            /* value */
            SYMBOL *loc;        /* out-of-line location */
        } c;
        struct {            /* function: */
            COORDINATE pt;      /* source code coordinate */
            int label;          /* label used to exit function */
            int ncalls;         /* number of calls from function */
            SYMBOL **callee;    /* list of arguments */
            LIST *sehlist;      /* list of SEH blocks */
        } fcn;
        int seg;            /* globals, statics: def segment */
        SYMBOL *alias;      /* aliased symbol */
        struct {            /* temporary: */
            NODE *cse;          /* common subexpression elemination */
            bool_t replace;     /* replace cse? */
        } t;
    } u;
    TREE *e;                /* optional argument expression */
    XSYMBOL x;              /* back-end's extension */
};

/*
 *  Dag operators;
 *
 *  Generic operator, valid type suffix, and number of kids and syms.
 *  The notations in the syms column give the number of syms values
 *  and a one-letter code that suggests their uses: 1V indicates that
 *  syms[0] points to a symbol for a variable, 1C indicates that
 *  syms[0] is a constant, and 1L indicates that syms[0] is a label.
 *  For 1S, syms[0] is a constant whose value is a size in bytes;
 *  2S adds syms[1], which is a constant whose value is an alignment.
 *  For most operators, the type suffix and size indicator denote the
 *  type and size of operation to perform and the type and size of
 *  the result.
 *
 *  syms kids Operator Types   Sizes                Operation
 *  1V   0    ADDRF    ...P..  p                    address of a parameter
 *  1V   0    ADDRG    ...P..  p                    address of a global
 *  1V   0    ADDRL    ...P..  p                    address of a local
 *  1C   0    CNST     FIUP..  fdx csilh p          constant
 *
 *       1    BCOM     .IU...  ilh                  bitwise complement
 *  1S   1    CVF      FI....  fdx ilh              convert from float
 *  1S   1    CVI      FIU...  fdx csilh csilhp     convert from signed integer
 *  1S   1    CVP      ..U...  p                    convert from pointer
 *  1S   1    CVU      .IUP..  csilh p              convert from unsigned integer
 *       1    INDIR    FIUP.B  fdx csilh p          fetch
 *       1    NEG      FI....  fdx ilh              negation
 *
 *       2    ADD      FIUP..  fdx ilh ilhp p       addition
 *       2    BAND     .IU...  ilh                  bitwise AND
 *       2    BOR      .IU...  ilh                  bitwise inclusive OR
 *       2    BXOR     .IU...  ilh                  bitwise exclusive OR
 *       2    DIV      FIU...  fdx ilh              division
 *       2    LSH      .IU...  ilh                  left shift
 *       2    MOD      .IU...  ilh                  modulus
 *       2    MUL      FIU...  fdx ilh              multiplication
 *       2    RSH      .IU...  ilh                  right shift
 *       2    SUB      FIUP..  fdx ilh ilhp p       subtraction
 *
 *  2S   2    ASGN     FIUP.B  fdx csilh p          assignment
 *  1L   2    EQ       FIU...  fdx ilh ilhp         jump if equal
 *  1L   2    GE       FIU...  fdx ilh ilhp         jump if greater than or equal
 *  1L   2    GT       FIU...  fdx ilh ilhp         jump if greater than
 *  1L   2    LE       FIU...  fdx ilh ilhp         jump if less than or equal
 *  1L   2    LT       FIU...  fdx ilh ilhp         jump if less than
 *  1L   2    NE       FIU...  fdx ilh ilhp         jump if not equal
 *
 *  2S   1    ARG      FIUP.B  fdx ilh p            argument
 *  1    1/2  CALL     FIUPVB  fdx ilh p            function call
 *       1    RET      FIUPV.  fdx ilh p            return from function
 *
 *       1    JUMP     ....V.                       unconditional jump
 *  1L   0    LABEL    ....V.                       label definition
 *
 *       1    CBOOL      U...  b                    convert to _Bool
 *       1    CEQ      FIU...  fdx ilh ilhp         true if equal (optimization)
 *       1    CGE      FIU...  fdx ilh ilhp         true if greater than or equal (optimization)
 *       1    CGT      FIU...  fdx ilh ilhp         true if greater than (optimization)
 *       1    CLE      FIU...  fdx ilh ilhp         true if less than or equal (optimization)
 *       1    CLT      FIU...  fdx ilh ilhp         true if less than (optimization)
 *       1    CNE      FIU...  fdx ilh ilhp         true if not equal (optimization)
 *
 *  1C   0/1  INTRIN1  FIUPV.  fdx csilh csilhp     intrinsic function, no or one argument, no side effects
 *  1C   0/1  INTRIN1S FIUPV.  fdx csilh csilhp     intrinsic function, no or one argument, side effects
 *  1C   2    INTRIN2  FIUPV.  fdx csilh csilhp     intrinsic function, two arguments, no side effects
 *  1C   2    INTRIN2S FIUPV.  fdx csilh csilhp     intrinsic function, two arguments, side effects
 *
 *  F=FLOAT, I=INT, U=UNSIGNED, P=POINTER, V=VOID, B=STRUCT
 *
 *  The entries in the Sizes column indicate sizes of the operators
 *  that back ends must implement. Letters denote the size of float (f),
 *  double (d), long double (x), character (c), short integer (s),
 *  integer (i), long integer (l), long long integer (h) and pointer (p).
 *
 *  These sizes are separated into sets for each type suffix, except that
 *  a single set is used for both I and U when the set for I is identical
 *  to the set for U.
 *
 *  The actual values for the size indicators, fdxcsilhbp, depend on the
 *  target. A specification like ADDFf denotes the operator ADD+F+sizeop(f),
 *  where "f" is replaced by a target-dependent value, e.g., ADDF4 and
 *  ADDF8.
 */

/* dag node */
typedef struct _NODE {
    short op;               /* dag operator; o+t+sizeop(s) */
    short count;            /* reference count */
    SYMBOL *syms[3];        /* symbols */
    NODE *kids[2];          /* operands */
    NODE *link;
    XNODE x;                /* back-ends type extension */
} NODE;

typedef struct _TREE {
    int op;
    TYPE *type;
    TREE *kids[2];
    NODE *node;
    union {
        VALUE v;
        SYMBOL *sym;
        FIELD *field;
    } u;
} TREE;

typedef struct _CODE {
    enum {
        CODE_BLOCKBEG = 0,
        CODE_BLOCKEND = 1,
        CODE_LOCAL = 2,
        CODE_ADDRESS = 3,
        CODE_DEFPOINT = 4,
        CODE_LABEL = 5,         /* check for < CODE_LABEL */
        CODE_START = 6,         /* check for > CODE_START */
        CODE_GEN = 7,
        CODE_JUMP = 8,
        CODE_SWITCH = 9,
        CODE_ASM = 10,
        CODE_UNWIND = 11
    } kind;
    CODE *prev;
    CODE *next;
    int count;                  /* track dead code */
    union {
        struct {
            int level;              /* scope level */
            SYMBOL **locals;
            TABLE *identifiers;
            TABLE *types;
            ENV x;
            struct {
                enum {
                    SEH_NOTHING = 0,    /* standard vanilla block */
                    SEH_TRY = 1,        /* SEH try block */
                    SEH_EXCEPT = -1,    /* SEH except block */
                    SEH_FINALLY = -2    /* SEH finally block */
                } type;
                SEH *data;              /* SEH block information */
            } seh;
        } block;
        CODE *begin;
        SYMBOL *var;            /* local variable */
        struct {
            SYMBOL *sym;
            SYMBOL *base;
            long offset;
        } addr;
        struct {
            COORDINATE src;
#ifdef PROF
            int point;
#endif
        } point;
        NODE *forest;
        struct {
            SYMBOL *sym;
            SYMBOL *table;
            SYMBOL *deflab;
            int size;
            intmax_t *values;
            SYMBOL **labels;
        } swtch;
        struct {
            char *text;         /* assembly code */
            SYMBOL *lab;        /* statement label */
            SYMBOL *sym;        /* symbol node */
            COORDINATE src;
            long cl;
        } asm;
        SEH *seh;
    } u;
} CODE;

typedef struct _SWTCH {
    SYMBOL *sym;                /* switch symbol */
    int lab;
    SYMBOL *deflab;             /* label for default case */
    int ncases;                 /* number of cases */
    int size;                   /* number of entries in values/labels */
    intmax_t *values;           /* value of each case */
    SYMBOL **labels;            /* label for each case */
} SWTCH;

typedef struct _METRICS {
    uchar_t size, align, outofline;
} METRICS;

typedef struct _INTERFACE {
    METRICS boolmetric;
    METRICS charmetric;
    METRICS shortmetric;
    METRICS intmetric;
    METRICS longmetric;
    METRICS longlongmetric;
    METRICS floatmetric;
    METRICS doublemetric;
    METRICS longdoublemetric;
    METRICS ptrmetric;
    METRICS structmetric;
    /**/
    uint_t little_endian: 1;
    uint_t wants_callb: 1;
    uint_t wants_argb: 1;
    uint_t wants_optb: 1;
    uint_t left_to_right: 1;
    uint_t wants_dag: 1;
    uint_t unsigned_char: 1;
    uint_t wants_intrinsic: 1;
    /**/
    void (*address)(SYMBOL *, SYMBOL *, long);
    void (*blockbeg)(ENV *);
    void (*blockend)(ENV *);
    void (*defaddress)(SYMBOL *);
    void (*defconst)(int, int, VALUE);
    void (*defstring)(int, char *);
    void (*defsymbol)(SYMBOL *);
    void (*emit)(NODE *);
    void (*export)(SYMBOL *);
    void (*function)(SYMBOL *, SYMBOL *[], SYMBOL *[], int);
    NODE *(*gen)(NODE *);
    void (*global)(SYMBOL *);
    void (*import)(SYMBOL *);
    void (*local)(SYMBOL *);
    void (*progbeg)(void);
    void (*progend)(void);
    void (*segment)(int);
    void (*space)(int);
    void (*sehbeg)(int, SEH *);
    void (*sehend)(int, SEH *);
    void (*unwind)(SEH *);
    int (*opcall)(int);
    void (*dbgblock)(int, int, SYMBOL **);
    void (*dbgend)(COORDINATE *, SYMBOL *, COORDINATE **, SYMBOL **, SYMBOL **);
    void (*dbgfend)(SYMBOL *, int);
    void (*dbginit)(char *, char *);
    void (*dbgline)(COORDINATE *);
    void (*dbgsym)(SYMBOL *);
    void (*dbgtype)(SYMBOL *);
    XINTERFACE x;
} INTERFACE;

typedef struct _BINDING {
    char *name;
    INTERFACE *ir;      /* back-end interface */
    OUTFMT *of;         /* output format */
} BINDING;

#ifndef PREPROCESSOR  /* avoid token clash */
enum {
#define xx(a,b,c,d,e,f,g) a=b,  /* symbol=value */
#define yy(a,b,c,d,e,f,g)
#include "token.h"
    LAST
};

enum {
    F=FLOAT_,
    I=INT_,
    U=UNSIGNED,
    P=POINTER,
    V=VOID_,
    B=STRUCT
};

#define gop(name,value) name=value<<4,
#define op(name,type,sizes)
enum {
    gop(CNST,1)
     op(CNST,F,fdx)
     op(CNST,I,bcsilh)
     op(CNST,P,p)
     op(CNST,U,bcsilh)
    gop(ARG,2)
     op(ARG,B,-)
     op(ARG,F,fdx)
     op(ARG,I,ilh)
     op(ARG,P,p)
     op(ARG,U,ilh)
    gop(ASGN,3)
     op(ASGN,B,-)
     op(ASGN,F,fdx)
     op(ASGN,I,bcsilh)
     op(ASGN,P,p)
     op(ASGN,U,bcsilh)
    gop(INDIR,4)
     op(INDIR,B,-)
     op(INDIR,F,fdx)
     op(INDIR,I,bcsilh)
     op(INDIR,P,p)
     op(INDIR,U,bcsilh)
    gop(CVF,7)
     op(CVF,F,fdx)
     op(CVF,I,ilh)
    gop(CVI,8)
     op(CVI,F,fdx)
     op(CVI,I,csilh)
     op(CVI,U,csilhp)
    gop(CVP,9)
     op(CVP,U,p)
    gop(CVU,11)
     op(CVU,I,csilh)
     op(CVU,P,p)
     op(CVU,U,csilh)
    gop(NEG,12)
     op(NEG,F,fdx)
     op(NEG,I,ilh)
    gop(CALL,13)
     op(CALL,B,-)
     op(CALL,F,fdx)
     op(CALL,I,ilh)
     op(CALL,P,p)
     op(CALL,U,ilh)
     op(CALL,V,-)
    gop(RET,15)
     op(RET,F,fdx)
     op(RET,I,ilh)
     op(RET,P,p)
     op(RET,U,ilh)
     op(RET,V,-)
    gop(ADDRG,16)
     op(ADDRG,P,p)
    gop(ADDRF,17)
     op(ADDRF,P,p)
    gop(ADDRL,18)
     op(ADDRL,P,p)
    gop(ADD,19)
     op(ADD,F,fdx)
     op(ADD,I,ilh)
     op(ADD,P,p)
     op(ADD,U,ilhp)
    gop(SUB,20)
     op(SUB,F,fdx)
     op(SUB,I,ilh)
     op(SUB,P,p)
     op(SUB,U,ilhp)
    gop(LSH,21)
     op(LSH,I,ilh)
     op(LSH,U,ilh)
    gop(MOD,22)
     op(MOD,I,ilh)
     op(MOD,U,ilh)
    gop(RSH,23)
     op(RSH,I,ilh)
     op(RSH,U,ilh)
    gop(BAND,24)
     op(BAND,I,ilh)
     op(BAND,U,ilh)
    gop(BCOM,25)
     op(BCOM,I,ilh)
     op(BCOM,U,ilh)
    gop(BOR,26)
     op(BOR,I,ilh)
     op(BOR,U,ilh)
    gop(BXOR,27)
     op(BXOR,I,ilh)
     op(BXOR,U,ilh)
    gop(DIV,28)
     op(DIV,F,fdx)
     op(DIV,I,ilh)
     op(DIV,U,ilh)
    gop(MUL,29)
     op(MUL,F,fdx)
     op(MUL,I,ilh)
     op(MUL,U,ilh)
    gop(EQ,30)
     op(EQ,F,fdx)
     op(EQ,I,ilh)
     op(EQ,U,ilhp)
    gop(GE,31)
     op(GE,F,fdx)
     op(GE,I,ilh)
     op(GE,U,ilhp)
    gop(GT,32)
     op(GT,F,fdx)
     op(GT,I,ilh)
     op(GT,U,ilhp)
    gop(LE,33)
     op(LE,F,fdx)
     op(LE,I,ilh)
     op(LE,U,ilhp)
    gop(LT,34)
     op(LT,F,fdx)
     op(LT,I,ilh)
     op(LT,U,ilhp)
    gop(NE,35)
     op(NE,F,fdx)
     op(NE,I,ilh)
     op(NE,U,ilhp)
    gop(JUMP,36)
     op(JUMP,V,-)
    gop(LABEL,37)
     op(LABEL,V,-)
    gop(CBOOL,38)
     op(CBOOL,U,b)
    gop(CEQ,39)
     op(CEQ,I,il)
     op(CEQ,U,il)
    gop(CGE,40)
     op(CGE,I,il)
     op(CGE,U,il)
    gop(CGT,41)
     op(CGT,I,il)
     op(CGT,U,il)
    gop(CLE,42)
     op(CLE,I,il)
     op(CLE,U,il)
    gop(CLT,43)
     op(CLT,I,il)
     op(CLT,U,il)
    gop(CNE,44)
     op(CNE,I,il)
     op(CNE,U,il)
    gop(INTRIN1,45)
     op(INTRIN1,F,fdx)
     op(INTRIN1,I,bcsilh)
     op(INTRIN1,P,p)
     op(INTRIN1,U,bcsilh)
     op(INTRIN1,V,-)
    gop(INTRIN1S,46)  /* intrinsic with side effects */
     op(INTRIN1S,F,fdx)
     op(INTRIN1S,I,bcsilh)
     op(INTRIN1S,P,p)
     op(INTRIN1S,U,bcsilh)
     op(INTRIN1S,V,-)
    gop(INTRIN2,47)
     op(INTRIN2,F,fdx)
     op(INTRIN2,I,bcsilh)
     op(INTRIN2,P,p)
     op(INTRIN2,U,bcsilh)
     op(INTRIN2,V,-)
    gop(INTRIN2S,48)  /* intrinsic with side effects */
     op(INTRIN2S,F,fdx)
     op(INTRIN2S,I,bcsilh)
     op(INTRIN2S,P,p)
     op(INTRIN2S,U,bcsilh)
     op(INTRIN2S,V,-)
    gop(LOAD,14)
     op(LOAD,B,-)
     op(LOAD,F,fdx)
     op(LOAD,I,bcsilh)
     op(LOAD,P,p)
     op(LOAD,U,bcsilhp)
    LASTOP
};
#undef gop
#undef op

/* operators that don't reach the back-end */
enum {
    AND=49<<4,
    NOT=50<<4,
    OR=51<<4,
    COND=52<<4,
    RIGHT=53<<4,
    AFIELD=54<<4
};

enum { TEXT=1, BSS, DATA, LIT, DRECTVE };
enum { PERM=0, FUNC, STMT };

enum { CONSTANTS=1, LABELS, GLOBAL, PARAM, LOCAL };

#endif  /* PREPROCESSOR */

#define UNREFERENCED_PARAMETER(P)  (P)

/* Matches GetLastError() type */
typedef DWORD WINERR;

#define NELEMS(arr)  (sizeof(arr) / sizeof(arr[0]))

#ifndef FALSE
#define FALSE  0
#endif
#ifndef TRUE
#define TRUE  !FALSE
#endif

#define isaddrop(op)  (specific(op) == ADDRG+P || specific(op) == ADDRL+P || specific(op) == ADDRF+P)

#define MAXLINE  4096
#define BUFSIZE  65536  /*4096*/
#define MAXPATH  260

#define istypename(tok,toksym)  (kind[tok] == CHAR_ || tok == ID && toksym && toksym->sclass == TYPEDEF)

#define sizeop(n) ((n) << 10)
#define generic(op)  ((op) & 0x3F0)
#define specific(op) ((op) & 0x3FF)
#define opindex(op) (((op) >> 4) & 0x3F)
#define opkind(op)  ((op) & ~0x3F0)
#define opsize(op)  ((op) >> 10)
#define optype(op)  ((op) & 0xF)

#undef roundup
#define roundup(x,n)  (((x)+((n)-1)) & (~((n)-1)))
#define mkop(op,ty)  (specific((op) + ttob(ty)))

#define extend(x,ty)  ((x) & (((uintmax_t)1)<<(8*(ty)->size-1)) ? (x) | ((~((uintmax_t)0))<<(8*(ty)->size-1)) : (x) & ones(8*(ty)->size))
#define ones(n)  ((n) >= 8*sizeof(uintmax_t) ? ~((uintmax_t)0) : ~((~((uintmax_t)0))<<(n)))

#define bits2bytes(n)  (((n)+7)/8)

/*
 * type-checking macros.
 * the operator codes are defined in token.h
 * to permit the range tests below; don't change them.
 */
#define isqual(t)     ((t)->op >= CONST_)
#define unqual(t)     (isqual(t) ? (t)->type : (t))

#define isvolatile(t) ((t)->op == VOLATILE_ || (t)->op == VOLATILE_+CONST_ || (t)->op == VOLATILE_+RESTRICT_ || (t)->op == VOLATILE_+CONST_+RESTRICT_)
#define isconst(t)    ((t)->op == CONST_ || (t)->op == CONST_+VOLATILE_ || (t)->op == CONST_+RESTRICT_ || (t)->op == CONST_+VOLATILE_+RESTRICT_)
#define isrestrict(t) ((t)->op == RESTRICT_ || (t)->op == RESTRICT_+CONST_ || (t)->op == RESTRICT_+VOLATILE_ || (t)->op == RESTRICT_+CONST_+VOLATILE_)
#define isarray(t)    (unqual(t)->op == ARRAY)
#define isvla(t)      (unqual(t)->op == ARRAY && unqual(t)->u.arr.e != 0)
#define isstruct(t)   (unqual(t)->op == STRUCT || unqual(t)->op == UNION)
#define isunion(t)    (unqual(t)->op == UNION)
#define isfunc(t)     (unqual(t)->op == FUNCTION)
#define isptr(t)      (unqual(t)->op == POINTER)
#define ischar(t)     ((t)->size == 1 && isint(t))  /* char or unsigned char */
#define isint(t)      (unqual(t)->op == INT_ || unqual(t)->op == UNSIGNED)
#define isfloat(t)    (unqual(t)->op == FLOAT_)
#define isarith(t)    (unqual(t)->op <= UNSIGNED)  /* i.e. FLOAT_, INT_ and _UNSIGNED */
#define isunsigned(t) (unqual(t)->op == UNSIGNED)
#define isscalar(t)   (unqual(t)->op <= POINTER || unqual(t)->op == ENUM)  /* i.e. FLOAT_, INT_, UNSIGNED and POINTER */
#define isenum(t)     (unqual(t)->op == ENUM)

#define iscdecl(t)    ((t)->u.fcn.calltype == CDECL_)
#define isfast(t)     ((t)->u.fcn.calltype == FASTCALL_)

#define fieldsize(p)  (p)->bitsize
#define fieldright(p) ((p)->lsb - 1)
#define fieldleft(p)  (8*(p)->type->size - fieldsize(p) - fieldright(p))
/* #define fieldmask(p)  (~(~(uint_t)0 << fieldsize(p))) */
#define fieldmask(p)  (~(fieldsize(p) < 8*unsignedtype->size ? ~0u<<fieldsize(p) : 0u))  /* LCC 4.2 */

/* Display name for this program */
#ifdef XHARBOUR
#define PROGRAM_NAME  "xCC"
#else
#define PROGRAM_NAME  "POCC"
#endif  /* XHARBOUR */

enum { DONTOPT=0, MINSPACE, MAXSPEED };

struct options {
    bool_t preproc;         /* preprocess only */
    bool_t assemble;        /* assemble source file */
    bool_t printproto;      /* print function prototypes */
#ifdef PROF
    bool_t prof;            /* emit basic block profiling code */
#endif
#ifdef PODEBUG
    bool_t debug;           /* debug mode */
#endif
    bool_t extensions;      /* enable Pelle's extensions */
    bool_t microsoft;       /* enable Micro$oft extensions */
    bool_t fltopt;          /* optimize floating-point calculations */
    bool_t fltuse;          /* emit Micro$oft floating-point flag */
    bool_t hookcall;        /* generate call to hook procedure */
    bool_t multithread;     /* enable multi-threading */
    bool_t defaultlib;      /* emit name of default library */
    bool_t maxopt;          /* maximum optimization */
    bool_t verbose;         /* verbose mode (undocumented) */
    bool_t oldnames;        /* use oldnames.lib and define __POCC__OLDNAMES */
    bool_t nomangle;        /* don't mangle exported STDCALL symbols */
    bool_t truenomangle;    /* don't mangle *any* STDCALL or FASTCALL symbols */
    bool_t crtdll;          /* link with runtime DLL */
    bool_t cbstring;        /* use funky Microsoft driver strings */
    int calltype;           /* default calling convention (CDECL_, STDCALL_ or FASTCALL_) */
    int infolevel;          /* verbosity level (0-nothing, 1-#include tree, 2-#include tree and progress) */
    int warnlevel;          /* warning level (0-nothing, 1-serious, 2-informative) */
    int dbglevel;           /* debugging info level (0-nothing, 1-line numbers, 2-line numbers and symbols) */
#ifdef XREF
    int xreflevel;          /* cross-reference level (0-nothing, 1-functions, 2-all) */
#endif
    int optimize;           /* optimization type */
    int pragmaopt;          /* last #pragma optimization type */
    int structalign;        /* structure field alignment (in bytes) */
    int codepage;           /* code page for NLS conversion */
    char codeseg[16];       /* current #pragma code_seg section */
    char dataseg[16];       /* current #pragma data_seg section */
    char litseg[16];        /* current #pragma const_seg section */
};

#ifndef APPLICATION_ERROR_MASK
#define APPLICATION_ERROR_MASK        0x20000000
#define ERROR_SEVERITY_SUCCESS        0x00000000
#define ERROR_SEVERITY_INFORMATIONAL  0x40000000
#define ERROR_SEVERITY_WARNING        0x80000000
#define ERROR_SEVERITY_ERROR          0xC0000000
#endif

/* Error definitions */
#define ERROR_SEVERITY_FATAL     0x8000U
#define ERROR_SEVERITY_WARNING2  0x4000U
#define ERROR_SEVERITY_WARNING1  0x0000U

#define RCFATAL(err)    (ERROR_SEVERITY_FATAL|(err))
#define RCERROR(err)    (ERROR_SEVERITY_ERROR|(err))
#define RCWARNING(err)  (ERROR_SEVERITY_WARNING|(err))

#define ISFATAL(err)    (((err) & ERROR_SEVERITY_FATAL) ? 1 : 0)
#define ISERROR(err)    (((err) & 0xC0000000) == ERROR_SEVERITY_ERROR)
#define ISWARNING(err)  (((err) & 0xC0000000) == ERROR_SEVERITY_WARNING)

#define ERRNUM(err)     ((err) & 0x3FFF)

/* Support for multiple warning levels */
#define RCWARNING1(err) (ERROR_SEVERITY_WARNING|ERROR_SEVERITY_WARNING1|(err))
#define RCWARNING2(err) (ERROR_SEVERITY_WARNING|ERROR_SEVERITY_WARNING2|(err))
#define WARNLEVEL(err)  (((err) & ERROR_SEVERITY_WARNING2) ? 2 : 1)

/* C language error and warning range (see MSG.MC) */
#define CLANG_ERRNUM_MIN  1000
#define CLANG_ERRNUM_MAX  4050

/* Private WINERR error codes */
#define ERROR_FILE_NOT_FOUND2           (APPLICATION_ERROR_MASK|MSG_FILE_NOT_FOUND)
#define ERROR_CANNOT_CREATE_FILE        (APPLICATION_ERROR_MASK|MSG_CANNOT_CREATE_FILE)
#define ERROR_OUT_OF_MEMORY             (APPLICATION_ERROR_MASK|MSG_OUT_OF_MEMORY)
#define ERROR_CANT_READ_CMDFILE         (APPLICATION_ERROR_MASK|MSG_CANT_READ_CMDFILE)
#define ERROR_INTERNAL                  (APPLICATION_ERROR_MASK|MSG_INTERNAL_ERROR)

#define ERROR_UNKNOWN_OPTION            (APPLICATION_ERROR_MASK|MSG_UNKNOWN_OPTION)
#define ERROR_OPTION_ARG_MISSING        (APPLICATION_ERROR_MASK|MSG_OPTION_ARG_MISSING)

#define ERROR_UNTERM_INCLUDE_COND       (APPLICATION_ERROR_MASK|MSG_PP_UNTERM_INCLUDE_COND)
#define ERROR_UNTERM_IF_DIRECT          (APPLICATION_ERROR_MASK|MSG_PP_UNTERM_IF_DIRECT)
#define ERROR_INVALID_CONTROL_LINE      (APPLICATION_ERROR_MASK|MSG_PP_INVALID_CONTROL_LINE)
#define ERROR_UNKNOWN_PP_CONTROL        (APPLICATION_ERROR_MASK|MSG_PP_UNKNOWN_PP_CONTROL)
#define ERROR_IF_TOO_DEEPLY_NESTED      (APPLICATION_ERROR_MASK|MSG_PP_IF_TOO_DEEPLY_NESTED)
#define ERROR_UNDEF_SYNTAX_ERROR        (APPLICATION_ERROR_MASK|MSG_PP_UNDEF_SYNTAX_ERROR)
#define ERROR_ELIF_WITHOUT_IF           (APPLICATION_ERROR_MASK|MSG_PP_ELIF_WITHOUT_IF)
#define ERROR_ELIF_AFTER_ELSE           (APPLICATION_ERROR_MASK|MSG_PP_ELIF_AFTER_ELSE)
#define ERROR_ELSE_WITHOUT_IF           (APPLICATION_ERROR_MASK|MSG_PP_ELSE_WITHOUT_IF)
#define ERROR_ELSE_AFTER_ELSE           (APPLICATION_ERROR_MASK|MSG_PP_ELSE_AFTER_ELSE)
#define ERROR_ELSE_SYNTAX_ERROR         (APPLICATION_ERROR_MASK|MSG_PP_ELSE_SYNTAX_ERROR)
#define ERROR_ENDIF_WITHOUT_IF          (APPLICATION_ERROR_MASK|MSG_PP_ENDIF_WITHOUT_IF)
#define ERROR_ENDIF_SYNTAX_ERROR        (APPLICATION_ERROR_MASK|MSG_PP_ENDIF_SYNTAX_ERROR)
#define ERROR_ERROR_DIRECT              (APPLICATION_ERROR_MASK|MSG_PP_ERROR_DIRECT)
#define ERROR_LINE_SYNTAX_ERROR         (APPLICATION_ERROR_MASK|MSG_PP_LINE_SYNTAX_ERROR)
#define ERROR_LINE_NUMBER_OUT_OF_RANGE  (APPLICATION_ERROR_MASK|MSG_PP_LINE_NUMBER_OUT_OF_RANGE)
#define ERROR_CONTROL_SYNTAX_ERROR      (APPLICATION_ERROR_MASK|MSG_PP_CONTROL_SYNTAX_ERROR)
#define ERROR_CONTROL_NOT_IMPLEMENTED   (APPLICATION_ERROR_MASK|MSG_PP_CONTROL_NOT_IMPLEMENTED)

#define ERROR_IF_SYNTAX_ERROR           (APPLICATION_ERROR_MASK|MSG_PP_IF_SYNTAX_ERROR)
#define ERROR_INVALID_OPERATOR_IN_IF    (APPLICATION_ERROR_MASK|MSG_PP_INVALID_OPERATOR_IN_IF)
#define ERROR_BAD_OPERATOR_IN_IF        (APPLICATION_ERROR_MASK|MSG_PP_BAD_OPERATOR_IN_IF)
#define ERROR_BOTCH_IN_IF               (APPLICATION_ERROR_MASK|MSG_PP_BOTCH_IN_IF)
#define ERROR_UNDEF_EXPRESSION_VALUE    (APPLICATION_ERROR_MASK|MSG_PP_UNDEF_EXPRESSION_VALUE)
#define ERROR_BAD_COND_OPERATOR_IN_IF   (APPLICATION_ERROR_MASK|MSG_PP_BAD_COND_OPERATOR_IN_IF)
#define ERROR_EVAL_BOTCH                (APPLICATION_ERROR_MASK|MSG_PP_EVAL_BOTCH)
#define ERROR_BAD_DIGIT_IN_NUMBER       (APPLICATION_ERROR_MASK|MSG_PP_BAD_DIGIT_IN_NUMBER)
#define ERROR_BAD_NUMBER_IN_IF          (APPLICATION_ERROR_MASK|MSG_PP_BAD_NUMBER_IN_IF)
#define ERROR_WIDE_CHAR_VALUE_UNDEF     (APPLICATION_ERROR_MASK|MSG_PP_WIDE_CHAR_VALUE_UNDEF)
#define ERROR_UNDEF_ESC_IN_CHAR_CONST   (APPLICATION_ERROR_MASK|MSG_PP_UNDEF_ESC_IN_CHAR_CONST)
#define ERROR_EMPTY_CHAR_CONST          (APPLICATION_ERROR_MASK|MSG_PP_EMPTY_CHAR_CONST)
#define ERROR_MULTIBYTE_VALUE_UNDEF     (APPLICATION_ERROR_MASK|MSG_PP_MULTIBYTE_VALUE_UNDEF)
#define ERROR_SIGNED_CHAR_CONST         (APPLICATION_ERROR_MASK|MSG_PP_SIGNED_CHAR_CONST)
#define ERROR_STRING_IN_IF              (APPLICATION_ERROR_MASK|MSG_PP_STRING_IN_IF)

#define ERROR_INCLUDE_TOO_DEEPLY_NESTED (APPLICATION_ERROR_MASK|MSG_PP_INCLUDE_TOO_DEEPLY_NESTED)
#define ERROR_INCLUDE_FILE_NOT_FOUND    (APPLICATION_ERROR_MASK|MSG_PP_INCLUDE_FILE_NOT_FOUND)
#define ERROR_INCLUDE_SYNTAX_ERROR      (APPLICATION_ERROR_MASK|MSG_PP_INCLUDE_SYNTAX_ERROR)
#define ERROR_MORE_THAN_X_INCLUDES      (APPLICATION_ERROR_MASK|MSG_PP_MORE_THAN_X_INCLUDES)

#define ERROR_LEXICAL_BOTCH             (APPLICATION_ERROR_MASK|MSG_PP_LEXICAL_BOTCH)
#define ERROR_NO_NEWLINE_AT_EOF         (APPLICATION_ERROR_MASK|MSG_PP_NO_NEWLINE_AT_EOF)
#define ERROR_UNTERM_STRING_OR_CHAR     (APPLICATION_ERROR_MASK|MSG_PP_UNTERM_STRING_OR_CHAR)
#define ERROR_EOF_IN_STRING_OR_CHAR     (APPLICATION_ERROR_MASK|MSG_PP_EOF_IN_STRING_OR_CHAR)
#define ERROR_EOF_INSIDE_COMMENT        (APPLICATION_ERROR_MASK|MSG_PP_EOF_INSIDE_COMMENT)
#define ERROR_INPUT_BUFFER_OVERFLOW     (APPLICATION_ERROR_MASK|MSG_PP_INPUT_BUFFER_OVERFLOW)
#define ERROR_TRIGRAPH_USED             (APPLICATION_ERROR_MASK|MSG_PP_TRIGRAPH_USED)

#define ERROR_DEF_TOKEN_IS_NOT_A_NAME   (APPLICATION_ERROR_MASK|MSG_PP_DEF_TOKEN_IS_NOT_A_NAME)
#define ERROR_DEF_TOKEN_CANT_BE_REDEF   (APPLICATION_ERROR_MASK|MSG_PP_DEF_TOKEN_CANT_BE_REDEF)
#define ERROR_DUP_MACRO_ARGUMENT        (APPLICATION_ERROR_MASK|MSG_PP_DUP_MACRO_ARGUMENT)
#define ERROR_RESERVED_MACRO_ARGUMENT   (APPLICATION_ERROR_MASK|MSG_PP_RESERVED_MACRO_ARGUMENT)

#define ERROR_MACRO_ARGS_SYNTAX_ERROR   (APPLICATION_ERROR_MASK|MSG_PP_MACRO_ARGS_SYNTAX_ERROR)
#define ERROR_MACRO_REDEFINITION        (APPLICATION_ERROR_MASK|MSG_PP_MACRO_REDEFINITION)
#define ERROR_INVALID_D_OR_U_ARG        (APPLICATION_ERROR_MASK|MSG_PP_INVALID_D_OR_U_ARG)
#define ERROR_BAD_DEFINED_SYNTAX        (APPLICATION_ERROR_MASK|MSG_PP_BAD_DEFINED_SYNTAX)
#define ERROR_MACRO_ARG_DISAGREEMENT    (APPLICATION_ERROR_MASK|MSG_PP_MACRO_ARG_DISAGREEMENT)
#define ERROR_EOF_IN_MACRO_ARGLIST      (APPLICATION_ERROR_MASK|MSG_PP_EOF_IN_MACRO_ARGLIST)
#define ERROR_TOO_MANY_MACRO_ARGS       (APPLICATION_ERROR_MASK|MSG_PP_TOO_MANY_MACRO_ARGS)
#define ERROR_SHARP_WITHOUT_MACRO_ARG   (APPLICATION_ERROR_MASK|MSG_PP_SHARP_WITHOUT_MACRO_ARG)
#define ERROR_DSHARP_AT_BORDER          (APPLICATION_ERROR_MASK|MSG_PP_DSHARP_AT_BORDER)
#define ERROR_BAD_TOKEN_FROM_DSHARP     (APPLICATION_ERROR_MASK|MSG_PP_BAD_TOKEN_FROM_DSHARP)
#define ERROR_STR_MACRO_ARG_TOO_LONG    (APPLICATION_ERROR_MASK|MSG_PP_STR_MACRO_ARG_TOO_LONG)
#define ERROR_INTERNAL_MACRO_BOTCH      (APPLICATION_ERROR_MASK|MSG_PP_INTERNAL_MACRO_BOTCH)

#define ERROR_CANT_OPEN_INPUT_FILE      (APPLICATION_ERROR_MASK|MSG_PP_CANT_OPEN_INPUT_FILE)
#define ERROR_CANT_OPEN_OUTPUT_FILE     (APPLICATION_ERROR_MASK|MSG_PP_CANT_OPEN_OUTPUT_FILE)

#define ERROR_EXPECTED_TOKEN_X          (APPLICATION_ERROR_MASK|MSG_CC_EXPECTED_TOKEN_X)
#define ERROR_NON_PORTABLE_STR_CHARS    (APPLICATION_ERROR_MASK|MSG_CC_NON_PORTABLE_STR_CHARS)
#define ERROR_NON_PORTABLE_CHR_CHARS    (APPLICATION_ERROR_MASK|MSG_CC_NON_PORTABLE_CHR_CHARS)
#define ERROR_NON_PORTABLE_USAGE        (APPLICATION_ERROR_MASK|MSG_CC_NON_PORTABLE_USAGE)
#define ERROR_NON_PORTABLE_ASSIGNMENT   (APPLICATION_ERROR_MASK|MSG_CC_NON_PORTABLE_ASSIGNMENT)
#define ERROR_NON_PORTABLE_CONVERSION   (APPLICATION_ERROR_MASK|MSG_CC_NON_PORTABLE_CONVERSION)
#define ERROR_NON_PORTABLE_ASSEMBLY     (APPLICATION_ERROR_MASK|MSG_CC_NON_PORTABLE_ASSEMBLY)
#define ERROR_NON_PORTABLE_SEH          (APPLICATION_ERROR_MASK|MSG_CC_NON_PORTABLE_SEH)
#define ERROR_CONFLICTING_FUNC_PARAM    (APPLICATION_ERROR_MASK|MSG_CC_CONFLICTING_FUNC_PARAM)
#define ERROR_DECLARATION_MISMATCH      (APPLICATION_ERROR_MASK|MSG_CC_DECLARATION_MISMATCH)
#define ERROR_DECLARED_PARAM_MISSING    (APPLICATION_ERROR_MASK|MSG_CC_DECLARED_PARAM_MISSING)
#define ERROR_DUPLICATE_DECLARATION     (APPLICATION_ERROR_MASK|MSG_CC_DUPLICATE_DECLARATION)
#define ERROR_DUPLICATE_FIELD_NAME      (APPLICATION_ERROR_MASK|MSG_CC_DUPLICATE_FIELD_NAME)
#define ERROR_EMPTY_DECLARATION         (APPLICATION_ERROR_MASK|MSG_CC_EMPTY_DECLARATION)
#define ERROR_UNKNOWN_EXTENDED_ATTRIB   (APPLICATION_ERROR_MASK|MSG_CC_UNKNOWN_EXTENDED_ATTRIB)
#define ERROR_ILLEGAL_EXTENDED_ATTRIB   (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_EXTENDED_ATTRIB)
#define ERROR_ILLEGAL_INLINE_USAGE      (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_INLINE_USAGE)
#define ERROR_ILLEGAL_NAKED_USAGE       (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_NAKED_USAGE)
#define ERROR_ILLEGAL_DLLIMPORT_USAGE   (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_DLLIMPORT_USAGE)
#define ERROR_ILLEGAL_DLLEXPORT_USAGE   (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_DLLEXPORT_USAGE)
#define ERROR_ILLEGAL_ALLOCA_USAGE      (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_ALLOCA_USAGE)
#define ERROR_ILLEGAL_VLA_USAGE         (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_VLA_USAGE)
#define ERROR_ILLEGAL_VLA_CLASS         (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_VLA_USAGE)
#define ERROR_STATIC_IMPORT_EXPORT      (APPLICATION_ERROR_MASK|MSG_CC_STATIC_IMPORT_EXPORT)
#define ERROR_EMPTY_INPUT_FILE          (APPLICATION_ERROR_MASK|MSG_CC_EMPTY_INPUT_FILE)
#define ERROR_EXPECTING_ENUM_IDENT      (APPLICATION_ERROR_MASK|MSG_CC_EXPECTING_ENUM_IDENT)
#define ERROR_EXPECTING_IDENT           (APPLICATION_ERROR_MASK|MSG_CC_EXPECTING_IDENT)
#define ERROR_MISSING_PROTOTYPE_FOR     (APPLICATION_ERROR_MASK|MSG_CC_MISSING_PROTOTYPE_FOR)
#define ERROR_MISSING_PROTOTYPE         (APPLICATION_ERROR_MASK|MSG_CC_MISSING_PROTOTYPE)
#define ERROR_MISSING_OPT_PARAM         (APPLICATION_ERROR_MASK|MSG_CC_MISSING_OPT_PARAM)
#define ERROR_USED_IN_COND_EXPRESSION   (APPLICATION_ERROR_MASK|MSG_CC_USED_IN_COND_EXPRESSION)
#define ERROR_ADDRESSABLE_OBJ_REQUIRED  (APPLICATION_ERROR_MASK|MSG_CC_ADDRESSABLE_OBJ_REQUIRED)
#define ERROR_ASSIGN_TO_CONST_IDENT     (APPLICATION_ERROR_MASK|MSG_CC_ASSIGN_TO_CONST_IDENT)
#define ERROR_ASSIGN_TO_CONST_LOC       (APPLICATION_ERROR_MASK|MSG_CC_ASSIGN_TO_CONST_LOC)
#define ERROR_ILLEGAL_BREAK_STMT        (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_BREAK_STMT)
#define ERROR_ILLEGAL_CASE_LABEL        (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_CASE_LABEL)
#define ERROR_ILLEGAL_CHAR              (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_CHAR)
#define ERROR_ILLEGAL_CONTINUE_STMT     (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_CONTINUE_STMT)
#define ERROR_ILLEGAL_DEFAULT_LABEL     (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_DEFAULT_LABEL)
#define ERROR_ILLEGAL_EXPRESSION        (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_EXPRESSION)
#define ERROR_ILLEGAL_FORMAL_PARAM_TYPE (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_FORMAL_PARAM_TYPE)
#define ERROR_ILLEGAL_INIT_OF_PARAM     (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_INIT_OF_PARAM)
#define ERROR_ILLEGAL_INIT_FOR          (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_INIT_FOR)
#define ERROR_ILLEGAL_INIT_OF_EXTERN    (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_INIT_OF_EXTERN)
#define ERROR_ILLEGAL_INIT_OF_VLA       (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_INIT_OF_VLA)
#define ERROR_CASE_LABEL_NOT_INTCONST   (APPLICATION_ERROR_MASK|MSG_CC_CASE_LABEL_NOT_INTCONST)
#define ERROR_EXPRESSION_WO_EFFECT      (APPLICATION_ERROR_MASK|MSG_CC_EXPRESSION_WO_EFFECT)
#define ERROR_EXPECTING_FIELD_NAME      (APPLICATION_ERROR_MASK|MSG_CC_EXPECTING_FIELD_NAME)
#define ERROR_UNDECLARED_IDENT          (APPLICATION_ERROR_MASK|MSG_CC_UNDECLARED_IDENT)
#define ERROR_CANNOT_INIT_UNDEFINED     (APPLICATION_ERROR_MASK|MSG_CC_CANNOT_INIT_UNDEFINED)
#define ERROR_ILLEGAL_CONST_CAST        (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_CONST_CAST)
#define ERROR_ILLEGAL_CAST              (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_CAST)
#define ERROR_UNDEFINED_CONVERSION      (APPLICATION_ERROR_MASK|MSG_CC_UNDEFINED_CONVERSION)
#define ERROR_DUPLICATE_CASE_LABEL      (APPLICATION_ERROR_MASK|MSG_CC_DUPLICATE_CASE_LABEL)
#define ERROR_EXTRA_DEFAULT_LABEL       (APPLICATION_ERROR_MASK|MSG_CC_EXTRA_DEFAULT_LABEL)
#define ERROR_EXCESS_MULTIBYTE_CHAR     (APPLICATION_ERROR_MASK|MSG_CC_EXCESS_MULTIBYTE_CHAR)
#define ERROR_EXCESS_WIDE_CHAR          (APPLICATION_ERROR_MASK|MSG_CC_EXCESS_WIDE_CHAR)
#define ERROR_EMPTY_MULTIBYTE_CHAR      (APPLICATION_ERROR_MASK|MSG_CC_EMPTY_MULTIBYTE_CHAR)
#define ERROR_EMPTY_WIDE_CHAR           (APPLICATION_ERROR_MASK|MSG_CC_EMPTY_WIDE_CHAR)
#define ERROR_ILLEGAL_RETURN_TYPE       (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_RETURN_TYPE)
#define ERROR_ILLEGAL_RETURN_TYPE2      (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_RETURN_TYPE2)
#define ERROR_ILLEGAL_STMT_TERMINATION  (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_STMT_TERMINATION)
#define ERROR_ILLEGAL_TYPE              (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_TYPE)
#define ERROR_ILLEGAL_TYPE_IN_SWITCH    (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_TYPE_IN_SWITCH)
#define ERROR_ILLEGAL_TYPE_ARRAY        (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_TYPE_ARRAY)
#define ERROR_ILLEGAL_USE_OF_TYPE       (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_USE_OF_TYPE)
#define ERROR_ILLEGAL_USE_OF_TYPENAME   (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_USE_OF_TYPENAME)
#define ERROR_ILLEGAL_USE_OF            (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_USE_OF)
#define ERROR_EXPECTING_FUNCTION        (APPLICATION_ERROR_MASK|MSG_CC_EXPECTING_FUNCTION)
#define ERROR_INIT_MUST_BE_CONSTANT     (APPLICATION_ERROR_MASK|MSG_CC_INIT_MUST_BE_CONSTANT)
#define ERROR_INSUFFICIENT_FUNC_ARGS    (APPLICATION_ERROR_MASK|MSG_CC_INSUFFICIENT_FUNC_ARGS)
#define ERROR_OVERFLOW_IN_CONST_EXPR    (APPLICATION_ERROR_MASK|MSG_CC_OVERFLOW_IN_CONST_EXPR)
#define ERROR_OVERFLOW_IN_CONSTANT      (APPLICATION_ERROR_MASK|MSG_CC_OVERFLOW_IN_CONSTANT)
#define ERROR_OVERFLOW_IN_CONVERSION    (APPLICATION_ERROR_MASK|MSG_CC_OVERFLOW_IN_CONVERSION)
#define ERROR_OVERFLOW_IN_ESC_SEQ       (APPLICATION_ERROR_MASK|MSG_CC_OVERFLOW_IN_ESC_SEQ)
#define ERROR_OVERFLOW_IN_FP_CONSTANT   (APPLICATION_ERROR_MASK|MSG_CC_OVERFLOW_IN_FP_CONSTANT)
#define ERROR_STRING_LITERAL_TOO_LONG   (APPLICATION_ERROR_MASK|MSG_CC_STRING_LITERAL_TOO_LONG)
#define ERROR_CHAR_LITERAL_TOO_LONG     (APPLICATION_ERROR_MASK|MSG_CC_CHAR_LITERAL_TOO_LONG)
#define ERROR_INVALID_FIELD_DECL        (APPLICATION_ERROR_MASK|MSG_CC_INVALID_FIELD_DECL)
#define ERROR_INVALID_FP_CONSTANT       (APPLICATION_ERROR_MASK|MSG_CC_INVALID_FP_CONSTANT)
#define ERROR_INVALID_HEX_CONSTANT      (APPLICATION_ERROR_MASK|MSG_CC_INVALID_HEX_CONSTANT)
#define ERROR_INVALID_OCT_CONSTANT      (APPLICATION_ERROR_MASK|MSG_CC_INVALID_OCT_CONSTANT)
#define ERROR_INVALID_INIT_TYPE         (APPLICATION_ERROR_MASK|MSG_CC_INVALID_INIT_TYPE)
#define ERROR_INVALID_UNARY_OPERAND     (APPLICATION_ERROR_MASK|MSG_CC_INVALID_UNARY_OPERAND)
#define ERROR_INVALID_STORAGE_CLASS     (APPLICATION_ERROR_MASK|MSG_CC_INVALID_STORAGE_CLASS)
#define ERROR_INVALID_SIZEOF_TYPE       (APPLICATION_ERROR_MASK|MSG_CC_INVALID_SIZEOF_TYPE)
#define ERROR_INVALID_TYPE_SPEC         (APPLICATION_ERROR_MASK|MSG_CC_INVALID_TYPE_SPEC)
#define ERROR_INVALID_USE_OF_TYPEDEF    (APPLICATION_ERROR_MASK|MSG_CC_INVALID_USE_OF_TYPEDEF)
#define ERROR_LVALUE_REQUIRED           (APPLICATION_ERROR_MASK|MSG_CC_LVALUE_REQUIRED)
#define ERROR_MISSING_CHAR              (APPLICATION_ERROR_MASK|MSG_CC_MISSING_CHAR)
#define ERROR_MISSING_TAG               (APPLICATION_ERROR_MASK|MSG_CC_MISSING_TAG)
#define ERROR_MISSING_ARRAY_SIZE        (APPLICATION_ERROR_MASK|MSG_CC_MISSING_ARRAY_SIZE)
#define ERROR_MISSING_IDENT             (APPLICATION_ERROR_MASK|MSG_CC_MISSING_IDENT)
#define ERROR_MISSING_LABEL_IN_GOTO     (APPLICATION_ERROR_MASK|MSG_CC_MISSING_LABEL_IN_GOTO)
#define ERROR_MISSING_PARAM_NAME        (APPLICATION_ERROR_MASK|MSG_CC_MISSING_PARAM_NAME)
#define ERROR_MISSING_PARAM_TYPE        (APPLICATION_ERROR_MASK|MSG_CC_MISSING_PARAM_TYPE)
#define ERROR_MISSING_RETURN_VALUE      (APPLICATION_ERROR_MASK|MSG_CC_MISSING_RETURN_VALUE)
#define ERROR_MISSING_QUOTE_IN_PP_LINE  (APPLICATION_ERROR_MASK|MSG_CC_MISSING_QUOTE_IN_PP_LINE)
#define ERROR_MISSING_BRACKET_IN_INIT   (APPLICATION_ERROR_MASK|MSG_CC_MISSING_BRACKET_IN_INIT)
#define ERROR_MISSING_TYPE_SPEC         (APPLICATION_ERROR_MASK|MSG_CC_MISSING_TYPE_SPEC)
#define ERROR_MORE_THAN_X_ENUM_CONST    (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_ENUM_CONST)
#define ERROR_MORE_THAN_X_FIELDS        (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_FIELDS)
#define ERROR_MORE_THAN_X_BLOCK_IDENT   (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_BLOCK_IDENT)
#define ERROR_MORE_THAN_X_NESTED_STMT   (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_NESTED_STMT)
#define ERROR_MORE_THAN_X_SWITCH_CASES  (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_SWITCH_CASES)
#define ERROR_MORE_THAN_X_CALL_PARAMS   (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_CALL_PARAMS)
#define ERROR_MORE_THAN_X_FUNC_PARAMS   (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_FUNC_PARAMS)
#define ERROR_MORE_THAN_X_BYTES         (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_BYTES)
#define ERROR_MORE_THAN_X_CHARS         (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_CHARS)
#define ERROR_MORE_THAN_X_EXTERN_IDENT  (APPLICATION_ERROR_MASK|MSG_CC_MORE_THAN_X_EXTERN_IDENT)
#define ERROR_INIT_EXCEEDS_FIELD_WIDTH  (APPLICATION_ERROR_MASK|MSG_CC_INIT_EXCEEDS_FIELD_WIDTH)
#define ERROR_INT_EXPR_MUST_BE_CONST    (APPLICATION_ERROR_MASK|MSG_CC_INT_EXPR_MUST_BE_CONST)
#define ERROR_BAD_LEFT_DEREF_TYPE       (APPLICATION_ERROR_MASK|MSG_CC_BAD_LEFT_DEREF_TYPE)
#define ERROR_BAD_LEFT_DOT_TYPE         (APPLICATION_ERROR_MASK|MSG_CC_BAD_LEFT_DOT_TYPE)
#define ERROR_UNREFERENCED_LOCAL        (APPLICATION_ERROR_MASK|MSG_CC_UNREFERENCED_LOCAL)
#define ERROR_UNUSED_LOCAL              (APPLICATION_ERROR_MASK|MSG_CC_UNUSED_LOCAL)
#define ERROR_UNASSIGNED_LOCAL          (APPLICATION_ERROR_MASK|MSG_CC_UNASSIGNED_LOCAL)
#define ERROR_OLD_STYLE_FUNC_DEF        (APPLICATION_ERROR_MASK|MSG_CC_OLD_STYLE_FUNC_DEF)
#define ERROR_UNREFERENCED_PARAM        (APPLICATION_ERROR_MASK|MSG_CC_UNREFERENCED_PARAM)
#define ERROR_REDECLARATION_SEE_DECL    (APPLICATION_ERROR_MASK|MSG_CC_REDECLARATION_SEE_DECL)
#define ERROR_REDECLARATION_SEE_DECLTY  (APPLICATION_ERROR_MASK|MSG_CC_REDECLARATION_SEE_DECLTY)
#define ERROR_REDECLARATION             (APPLICATION_ERROR_MASK|MSG_CC_REDECLARATION)
#define ERROR_REDEFINITION_OF_LABEL     (APPLICATION_ERROR_MASK|MSG_CC_REDEFINITION_OF_LABEL)
#define ERROR_REDEFINITION_SEE_DEF      (APPLICATION_ERROR_MASK|MSG_CC_REDEFINITION_SEE_DEF)
#define ERROR_REDEFINITION_OF_OPT_PARAM (APPLICATION_ERROR_MASK|MSG_CC_REDEFINITION_OF_OPT_PARAM)
#define ERROR_QUAL_FUNC_TYPE_IGNORED    (APPLICATION_ERROR_MASK|MSG_CC_QUAL_FUNC_TYPE_IGNORED)
#define ERROR_QUAL_TYPE_IGNORED         (APPLICATION_ERROR_MASK|MSG_CC_QUAL_TYPE_IGNORED)
#define ERROR_REFERENCE_ELIDED          (APPLICATION_ERROR_MASK|MSG_CC_REFERENCE_ELIDED)
#define ERROR_VOLATILE_REFERENCE_ELIDED (APPLICATION_ERROR_MASK|MSG_CC_VOLATILE_REFERENCE_ELIDED)
#define ERROR_REGISTER_DECL_IGNORED     (APPLICATION_ERROR_MASK|MSG_CC_REGISTER_DECL_IGNORED)
#define ERROR_UNSIGNED_COMP_IS_CONST    (APPLICATION_ERROR_MASK|MSG_CC_UNSIGNED_COMP_IS_CONST)
#define ERROR_SHIFTING_X_BITS_IS_UNDEF  (APPLICATION_ERROR_MASK|MSG_CC_SHIFTING_X_BITS_IS_UNDEF)
#define ERROR_SIZE_EXCEEDS_X_BYTES      (APPLICATION_ERROR_MASK|MSG_CC_SIZE_EXCEEDS_X_BYTES)
#define ERROR_ARRSIZE_EXCEEDS_X_BYTES   (APPLICATION_ERROR_MASK|MSG_CC_ARRSIZE_EXCEEDS_X_BYTES)
#define ERROR_INFINITE_LOOP             (APPLICATION_ERROR_MASK|MSG_CC_INFINITE_LOOP)
#define ERROR_UNREFERENCED_STATIC       (APPLICATION_ERROR_MASK|MSG_CC_UNREFERENCED_STATIC)
#define ERROR_HUGE_SWITCH_TABLE         (APPLICATION_ERROR_MASK|MSG_CC_HUGE_SWITCH_TABLE)
#define ERROR_NO_SWITCH_CASES           (APPLICATION_ERROR_MASK|MSG_CC_NO_SWITCH_CASES)
#define ERROR_TOO_MANY_FUNC_PARAMS      (APPLICATION_ERROR_MASK|MSG_CC_TOO_MANY_FUNC_PARAMS)
#define ERROR_TOO_MANY_INITIALIZERS     (APPLICATION_ERROR_MASK|MSG_CC_TOO_MANY_INITIALIZERS)
#define ERROR_TYPE_ERROR_IN_ARG_FOUND   (APPLICATION_ERROR_MASK|MSG_CC_TYPE_ERROR_IN_ARG_FOUND)
#define ERROR_TYPE_ERROR_IN_ARG         (APPLICATION_ERROR_MASK|MSG_CC_TYPE_ERROR_IN_ARG)
#define ERROR_TYPE_ERROR_NO_ARRAY       (APPLICATION_ERROR_MASK|MSG_CC_TYPE_ERROR_NO_ARRAY)
#define ERROR_TYPE_ERROR_NO_FUNCTION    (APPLICATION_ERROR_MASK|MSG_CC_TYPE_ERROR_NO_FUNCTION)
#define ERROR_TYPE_ERROR_NO_POINTER     (APPLICATION_ERROR_MASK|MSG_CC_TYPE_ERROR_NO_POINTER)
#define ERROR_ASSIGNMENT_OF_TYPES       (APPLICATION_ERROR_MASK|MSG_CC_ASSIGNMENT_OF_TYPES)
#define ERROR_UNCLOSED_COMMENT          (APPLICATION_ERROR_MASK|MSG_CC_UNCLOSED_COMMENT)
#define ERROR_UNDEFINED_LABEL           (APPLICATION_ERROR_MASK|MSG_CC_UNDEFINED_LABEL)
#define ERROR_UNDEFINED_SIZE_FOR_PARAM  (APPLICATION_ERROR_MASK|MSG_CC_UNDEFINED_SIZE_FOR_PARAM)
#define ERROR_UNDEFINED_SIZE_FOR        (APPLICATION_ERROR_MASK|MSG_CC_UNDEFINED_SIZE_FOR)
#define ERROR_UNDEFINED_STATIC          (APPLICATION_ERROR_MASK|MSG_CC_UNDEFINED_STATIC)
#define ERROR_UNKNOWN_ENUMERATION       (APPLICATION_ERROR_MASK|MSG_CC_UNKNOWN_ENUMERATION)
#define ERROR_UNKNOWN_FIELD             (APPLICATION_ERROR_MASK|MSG_CC_UNKNOWN_FIELD)
#define ERROR_UNKNOWN_SIZE_FOR_TYPE     (APPLICATION_ERROR_MASK|MSG_CC_UNKNOWN_SIZE_FOR_TYPE)
#define ERROR_UNREACHABLE_CODE          (APPLICATION_ERROR_MASK|MSG_CC_UNREACHABLE_CODE)
#define ERROR_UNRECOGNIZED_CTRL_LINE    (APPLICATION_ERROR_MASK|MSG_CC_UNRECOGNIZED_CTRL_LINE)
#define ERROR_UNRECOGNIZED_DECL         (APPLICATION_ERROR_MASK|MSG_CC_UNREGOCNIZED_DECL)
#define ERROR_UNRECOGNIZED_STMT         (APPLICATION_ERROR_MASK|MSG_CC_UNREGOCNIZED_STMT)
#define ERROR_UNDEFINED_ARRAY_DECL      (APPLICATION_ERROR_MASK|MSG_CC_UNDEFINED_ARRAY_DECL)
#define ERROR_ZERO_WIDTH_FIELD_IGNORED  (APPLICATION_ERROR_MASK|MSG_CC_ZERO_WIDTH_FIELD_IGNORED)
#define ERROR_EXTRANEOUS_IDENT          (APPLICATION_ERROR_MASK|MSG_CC_EXTRANEOUS_IDENT)
#define ERROR_OLD_STYLE_PARAM_LIST      (APPLICATION_ERROR_MASK|MSG_CC_OLD_STYLE_PARAM_LIST)
#define ERROR_EXTRANEOUS_RETURN_VALUE   (APPLICATION_ERROR_MASK|MSG_CC_EXTRANEOUS_RETURN_VALUE)
#define ERROR_INVALID_HEX_ESC_SEQ_X     (APPLICATION_ERROR_MASK|MSG_CC_INVALID_HEX_ESC_SEQ_X)
#define ERROR_INVALID_HEX_ESC_SEQ       (APPLICATION_ERROR_MASK|MSG_CC_INVALID_HEX_ESC_SEQ)
#define ERROR_IMPLICIT_DECL_MISMATCH    (APPLICATION_ERROR_MASK|MSG_CC_IMPLICIT_DECL_MISMATCH)
#define ERROR_INCONSISTENT_LINKAGE      (APPLICATION_ERROR_MASK|MSG_CC_INCONSISTENT_LINKAGE)
#define ERROR_INVALID_UNARY_TYPE        (APPLICATION_ERROR_MASK|MSG_CC_INVALID_UNARY_TYPE)
#define ERROR_INVALID_BINARY_TYPES      (APPLICATION_ERROR_MASK|MSG_CC_INVALID_BINARY_TYPES)
#define ERROR_OVERFLOW_IN_HEX_ESC_SEQ   (APPLICATION_ERROR_MASK|MSG_CC_OVERFLOW_IN_HEX_ESC_SEQ)
#define ERROR_OVERFLOW_IN_ENUM_CONST    (APPLICATION_ERROR_MASK|MSG_CC_OVERFLOW_IN_ENUM_CONST)
#define ERROR_ILLEGAL_RETURN_VALUE_P1   (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_RETURN_VALUE_P1)
#define ERROR_ILLEGAL_RETURN_VALUE_P2   (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_RETURN_VALUE_P2)
#define ERROR_ILLEGAL_RETURN_VALUE_L1   (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_RETURN_VALUE_L1)
#define ERROR_ILLEGAL_RETURN_VALUE_L2   (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_RETURN_VALUE_L2)
#define ERROR_UNNAMED_IN_PROTOTYPE      (APPLICATION_ERROR_MASK|MSG_CC_UNNAMED_IN_PROTOTYPE)
#define ERROR_INVALID_CHAR_ESC_SEQ_X    (APPLICATION_ERROR_MASK|MSG_CC_INVALID_CHAR_ESC_SEQ_X)
#define ERROR_INVALID_CHAR_ESC_SEQ      (APPLICATION_ERROR_MASK|MSG_CC_INVALID_CHAR_ESC_SEQ)
#define ERROR_UNSIGNED_NEGATION         (APPLICATION_ERROR_MASK|MSG_CC_UNSIGNED_NEGATION)
#define ERROR_ILLEGAL_ARRAY_SIZE        (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_ARRAY_SIZE)
#define ERROR_ILLEGAL_BIT_FIELD_SIZE    (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_BIT_FIELD_SIZE)
#define ERROR_NON_ISO_DEFINITION        (APPLICATION_ERROR_MASK|MSG_CC_NON_ISO_DEFINITION)
#define ERROR_PP_NUM_BUT_NO_INT_CONST   (APPLICATION_ERROR_MASK|MSG_CC_PP_NUM_BUT_NO_INT_CONST)
#define ERROR_PP_NUM_BUT_NO_FP_CONST    (APPLICATION_ERROR_MASK|MSG_CC_PP_NUM_BUT_NO_FP_CONST)
#define ERROR_NON_ISO_FUNC_DEFINITION   (APPLICATION_ERROR_MASK|MSG_CC_NON_ISO_FUNC_DEFINITION)
#define ERROR_NON_ISO_TYPE              (APPLICATION_ERROR_MASK|MSG_CC_NON_ISO_TYPE)
#define ERROR_ILLEGAL_BIT_FIELD_TYPE    (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_BIT_FIELD_TYPE)
#define ERROR_ILLEGAL_FIELD_TYPE        (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_FIELD_TYPE)
#define ERROR_CAST_ON_LVALUE            (APPLICATION_ERROR_MASK|MSG_CC_CAST_ON_LVALUE)
#define ERROR_USED_AS_LVALUE            (APPLICATION_ERROR_MASK|MSG_CC_USED_AS_LVALUE)
#define ERROR_SIZEOF_ON_BIT_FIELD       (APPLICATION_ERROR_MASK|MSG_CC_SIZEOF_ON_BIT_FIELD)
#define ERROR_INVALID_PACK_PRAGMA       (APPLICATION_ERROR_MASK|MSG_CC_INVALID_PACK_PRAGMA)
#define ERROR_INVALID_COMMENT_PRAGMA    (APPLICATION_ERROR_MASK|MSG_CC_INVALID_COMMENT_PRAGMA)
#define ERROR_INVALID_INITEXIT_PRAGMA   (APPLICATION_ERROR_MASK|MSG_CC_INVALID_INITEXIT_PRAGMA)
#define ERROR_INVALID_WARN_PRAGMA       (APPLICATION_ERROR_MASK|MSG_CC_INVALID_WARN_PRAGMA)
#define ERROR_INVALID_INTRINSIC_PRAGMA  (APPLICATION_ERROR_MASK|MSG_CC_INVALID_INTRINSIC_PRAGMA)
#define ERROR_UNRECOGNIZED_PRAGMA       (APPLICATION_ERROR_MASK|MSG_CC_UNRECOGNIZED_PRAGMA)
#define ERROR_NON_INT_BIT_FIELD_TYPE    (APPLICATION_ERROR_MASK|MSG_CC_NON_INT_BIT_FIELD_TYPE)
#define ERROR_ANONYMOUS_FIELD_NAME      (APPLICATION_ERROR_MASK|MSG_CC_ANONYMOUS_FIELD_NAME)
#define ERROR_ILLEGAL_FLEXIBLE_FIELD    (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_FLEXIBLE_FIELD)
#define ERROR_ILLEGAL_LEAVE_STMT        (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_LEAVE_STMT)
#define ERROR_BAD_INTRINSIC_CONTEXT     (APPLICATION_ERROR_MASK|MSG_CC_BAD_INTRINSIC_CONTEXT)
#define ERROR_ILLEGAL_WITH_NAKED_ATTRIB (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_WITH_NAKED_ATTRIB)
#define ERROR_FUNCTION_MUST_BE_CDECL    (APPLICATION_ERROR_MASK|MSG_CC_FUNCTION_MUST_BE_CDECL)
#define ERROR_FP_EXPRESSION_TOO_COMPLEX (APPLICATION_ERROR_MASK|MSG_CC_FP_EXPRESSION_TOO_COMPLEX)
#define ERROR_NON_PARSEABLE_ASSEMBLY    (APPLICATION_ERROR_MASK|MSG_CC_NON_PARSEABLE_ASSEMBLY)
#define ERROR_ASSEMBLY_NOT_SUPPORTED    (APPLICATION_ERROR_MASK|MSG_CC_ASSEMBLY_NOT_SUPPORTED)
#define ERROR_SEH_NOT_SUPPORTED         (APPLICATION_ERROR_MASK|MSG_CC_SEH_NOT_SUPPORTED)
#define ERROR_FEATURE_NOT_SUPPORTED     (APPLICATION_ERROR_MASK|MSG_CC_FEATURE_NOT_SUPPORTED)
#define ERROR_UNREACHABLE_CODE_REMOVED  (APPLICATION_ERROR_MASK|MSG_CC_UNREACHABLE_CODE_REMOVED)
#define ERROR_TOO_MANY_ERRORS           (APPLICATION_ERROR_MASK|MSG_CC_TOO_MANY_ERRORS)
#define ERROR_INVALID_JUMP_PAST_VLA     (APPLICATION_ERROR_MASK|MSG_CC_INVALID_JUMP_PAST_VLA)
#define ERROR_ILLEGAL_ARRAY_INDEX       (APPLICATION_ERROR_MASK|MSG_CC_ILLEGAL_ARRAY_INDEX)
#define ERROR_DUPLICATE_INITIALIZERS    (APPLICATION_ERROR_MASK|MSG_CC_DUPLICATE_INITIALIZERS)
#define ERROR_INVALID_CODEPAGE          (APPLICATION_ERROR_MASK|MSG_CC_INVALID_CODEPAGE)

#define ERROR_UNDEF_SECTION_NAME        (APPLICATION_ERROR_MASK|MSG_AS_UNDEF_SECTION_NAME)
#define ERROR_EXPECTING_IDENT_AFTER     (APPLICATION_ERROR_MASK|MSG_AS_EXPECTING_IDENT_AFTER)
#define ERROR_INVALID_COMMON_SIZE       (APPLICATION_ERROR_MASK|MSG_AS_INVALID_COMMON_SIZE)
#define ERROR_EQU_SYNTAX_ERROR          (APPLICATION_ERROR_MASK|MSG_AS_EQU_SYNTAX_ERROR)
#define ERROR_OVERFLOW_IN_INT_CONST     (APPLICATION_ERROR_MASK|MSG_AS_OVERFLOW_IN_INT_CONST)
#define ERROR_UNKNOWN_CPU_TYPE          (APPLICATION_ERROR_MASK|MSG_AS_UNKNOWN_CPU_TYPE)

#define ERROR_BADLY_FORMED_FP_CONSTANT  (APPLICATION_ERROR_MASK|MSG_AS_BADLY_FORMED_FP_CONSTANT)
#define ERROR_OPERATOR_NEEDS_SCALAR     (APPLICATION_ERROR_MASK|MSG_AS_OPERATOR_NEEDS_SCALAR)
#define ERROR_DIVISION_BY_ZERO          (APPLICATION_ERROR_MASK|MSG_AS_DIVISION_BY_ZERO)
#define ERROR_EXPECTING_RPAREN          (APPLICATION_ERROR_MASK|MSG_AS_EXPECTING_RPAREN)
#define ERROR_UNDEFINED_SYMBOL          (APPLICATION_ERROR_MASK|MSG_AS_UNDEFINED_SYMBOL)
#define ERROR_UNDEFINED_BEFORE_USE      (APPLICATION_ERROR_MASK|MSG_AS_UNDEFINED_BEFORE_USE)
#define ERROR_UNDEFINED_STRUCTURE       (APPLICATION_ERROR_MASK|MSG_AS_UNDEFINED_STRUCTURE)
#define ERROR_EXPR_SYNTAX_ERROR         (APPLICATION_ERROR_MASK|MSG_AS_EXPR_SYNTAX_ERROR)

#define ERROR_EXPECTING_LABEL_OR_INSN   (APPLICATION_ERROR_MASK|MSG_AS_EXPECTING_LABEL_OR_INSN)
#define ERROR_LABEL_WITHOUT_COLON       (APPLICATION_ERROR_MASK|MSG_AS_LABEL_WITHOUT_COLON)
#define ERROR_INVALID_TIMES_VALUE       (APPLICATION_ERROR_MASK|MSG_AS_INVALID_TIMES_VALUE)
#define ERROR_TOO_MANY_INSN_PREFIX      (APPLICATION_ERROR_MASK|MSG_AS_TOO_MANY_INSN_PREFIX)
#define ERROR_EXPECTING_INSN            (APPLICATION_ERROR_MASK|MSG_AS_EXPECTING_INSN)
#define ERROR_FP_CONSTANT_IN_DB_INSN    (APPLICATION_ERROR_MASK|MSG_AS_FP_CONSTANT_IN_DB_INSN)
#define ERROR_EXPRESSION_NOT_SIMPLE     (APPLICATION_ERROR_MASK|MSG_AS_EXPRESSION_NOT_SIMPLE)
#define ERROR_EXPECTING_COMMA           (APPLICATION_ERROR_MASK|MSG_AS_EXPECTING_COMMA)
#define ERROR_EXPECTING_COMMA_OR_EOL    (APPLICATION_ERROR_MASK|MSG_AS_EXPECTING_COMMA_OR_EOL)
#define ERROR_EXPECTING_RBRACKET        (APPLICATION_ERROR_MASK|MSG_AS_EXPECTING_RBRACKET)
#define ERROR_MISSING_DATA_OPERAND      (APPLICATION_ERROR_MASK|MSG_AS_MISSING_DATA_OPERAND)
#define ERROR_INVALID_OPERAND_SIZE      (APPLICATION_ERROR_MASK|MSG_AS_INVALID_OPERAND_SIZE)
#define ERROR_INVALID_OPERAND_TYPE      (APPLICATION_ERROR_MASK|MSG_AS_INVALID_OPERAND_TYPE)
#define ERROR_INVALID_EA_SIZE           (APPLICATION_ERROR_MASK|MSG_AS_INVALID_EA_SIZE)
#define ERROR_INVALID_EA_ADDR           (APPLICATION_ERROR_MASK|MSG_AS_INVALID_EA_ADDR)
#define ERROR_REG_SIZE_IGNORED          (APPLICATION_ERROR_MASK|MSG_AS_REG_SIZE_IGNORED)
#define ERROR_INVALID_SEG_OVERRIDE      (APPLICATION_ERROR_MASK|MSG_AS_INVALID_SEG_OVERRIDE)

#define ERROR_ONE_BYTE_RELOCATION       (APPLICATION_ERROR_MASK|MSG_AS_ONE_BYTE_RELOCATION)
#define ERROR_INT_CONST_IN_DT_INSN      (APPLICATION_ERROR_MASK|MSG_AS_INT_CONST_IN_DT_INSN)
#define ERROR_OPERAND_SIZE_MISSING      (APPLICATION_ERROR_MASK|MSG_AS_OPERAND_SIZE_MISSING)
#define ERROR_OPERAND_SIZE_MISMATCH     (APPLICATION_ERROR_MASK|MSG_AS_OPERAND_SIZE_MISMATCH)
#define ERROR_CPU_TYPE_MISMATCH         (APPLICATION_ERROR_MASK|MSG_AS_CPU_TYPE_MISMATCH)
#define ERROR_UNKNOWN_INSN              (APPLICATION_ERROR_MASK|MSG_AS_UNKNOWN_INSN)
#define ERROR_NON_CONSTANT_BSS_SIZE     (APPLICATION_ERROR_MASK|MSG_AS_NON_CONSTANT_BSS_SIZE)
#define ERROR_DATA_EXCEEDS_BOUNDS       (APPLICATION_ERROR_MASK|MSG_AS_DATA_EXCEEDS_BOUNDS)
#define ERROR_NON_RELOC_FAR_VALUE       (APPLICATION_ERROR_MASK|MSG_AS_NON_RELOC_FAR_VALUE)
#define ERROR_INTER_SEG_REFERENCE       (APPLICATION_ERROR_MASK|MSG_AS_INTER_SEG_REFERENCE)
#define ERROR_JUMP_OUT_OF_RANGE         (APPLICATION_ERROR_MASK|MSG_AS_JUMP_OUT_OF_RANGE)

#define ERROR_LABEL_REDEFINITION        (APPLICATION_ERROR_MASK|MSG_AS_LABEL_REDEFINITION)
#define ERROR_LOCAL_LABEL_TOO_SOON      (APPLICATION_ERROR_MASK|MSG_AS_LOCAL_LABEL_TOO_SOON)
#define ERROR_LOCAL_LABEL_AS_COMMON     (APPLICATION_ERROR_MASK|MSG_AS_LOCAL_LABEL_AS_COMMON)
#define ERROR_LABEL_ALREADY_LOCAL       (APPLICATION_ERROR_MASK|MSG_AS_LABEL_ALREADY_LOCAL)
#define ERROR_MISPLACED_GLOBAL          (APPLICATION_ERROR_MASK|MSG_AS_MISPLACED_GLOBAL)
#define ERROR_PHASE_ERROR               (APPLICATION_ERROR_MASK|MSG_AS_PHASE_ERROR)

#define ERROR_EXPECTING_REGISTER        (APPLICATION_ERROR_MASK|MSG_AS_EXPECTING_REGISTER)
#define ERROR_DUPLICATE_REGISTERS       (APPLICATION_ERROR_MASK|MSG_AS_DUPLICATE_REGISTERS)
#define ERROR_ILLEGAL_REGISTER          (APPLICATION_ERROR_MASK|MSG_AS_ILLEGAL_REGISTER)
#define ERROR_EXPECTING_CPSR_OR_SPSR    (APPLICATION_ERROR_MASK|MSG_AS_EXPECTING_CPSR_OR_SPSR)
#define ERROR_OFFSET_TOO_BIG            (APPLICATION_ERROR_MASK|MSG_AS_OFFSET_TOO_BIG)
#define ERROR_SHIFT_TOO_BIG             (APPLICATION_ERROR_MASK|MSG_AS_SHIFT_TOO_BIG)
#define ERROR_NON_CREATABLE_CONST       (APPLICATION_ERROR_MASK|MSG_AS_NON_CREATABLE_CONST)
#define ERROR_W_BIT_NOT_ALLOWED         (APPLICATION_ERROR_MASK|MSG_AS_W_BIT_NOT_ALLOWED)
#define ERROR_T_BIT_NOT_ALLOWED         (APPLICATION_ERROR_MASK|MSG_AS_T_BIT_NOT_ALLOWED)
#define ERROR_S_BIT_NOT_ALLOWED         (APPLICATION_ERROR_MASK|MSG_AS_S_BIT_NOT_ALLOWED)
#define ERROR_UNALIGNED_OFFSET          (APPLICATION_ERROR_MASK|MSG_AS_UNALIGNED_OFFSET)

#define ERROR_COFF_CODE_IN_ABS_SEGMENT  (APPLICATION_ERROR_MASK|MSG_COFF_CODE_IN_ABS_SEGMENT)
#define ERROR_COFF_DATA_IN_BSS_SEGMENT  (APPLICATION_ERROR_MASK|MSG_COFF_DATA_IN_BSS_SEGMENT)
#define ERROR_COFF_UNSUPPORTED_RELOC    (APPLICATION_ERROR_MASK|MSG_COFF_UNSUPPORTED_RELOC)
#define ERROR_COFF_NO_SEGMENT_BASE_REF  (APPLICATION_ERROR_MASK|MSG_COFF_NO_SEGMENT_BASE_REF)
#define ERROR_COFF_NO_REL_TO_ABS_REF    (APPLICATION_ERROR_MASK|MSG_COFF_NO_REL_TO_ABS_REF)
#define ERROR_COFF_UNKNOWN_SPECIAL      (APPLICATION_ERROR_MASK|MSG_COFF_UNKNOWN_SPECIAL)
#define ERROR_COFF_SECTNAME_TOO_LONG    (APPLICATION_ERROR_MASK|MSG_COFF_SECTNAME_TOO_LONG)
#define ERROR_COFF_UNKNOWN_SECT_ATTRIB  (APPLICATION_ERROR_MASK|MSG_COFF_UNKNOWN_SECT_ATTRIB)
#define ERROR_COFF_UNKNOWN_SEGMENT      (APPLICATION_ERROR_MASK|MSG_COFF_UNKNOWN_SEGMENT)
#define ERROR_COFF_TOO_MANY_RELOCATIONS (APPLICATION_ERROR_MASK|MSG_COFF_TOO_MANY_RELOCATIONS)

/* allocate.c */
void *memalloc(size_t, uint_t);
void memfree(uint_t);
void *memarray(size_t, size_t, uint_t);

/* bind.c */
bool_t select_binding(const char *);
void list_bindings(void);

/* dag.c */
NODE *new_node(int, NODE *, NODE *, SYMBOL *);
void new_forest(TREE *, int, int);
NODE *list_nodes(TREE *, int, int);
TREE *static_constant(TREE *);
void best_function_regsyms(SYMBOL *[], int);
void generate_function_code(SYMBOL *[], SYMBOL *[]);
void emit_function_code(void);
NODE *jump(int);
void printdag(NODE *);

/* decl.c */
void parse_program(void);
void parse_compound_statement(int, SWTCH *, SEH *, int);
CODE *parse_for_declaration(void);
void close_for_scope(CODE *);
TYPE *parse_typename(void);
void finalize(void);
TREE *inline_function(SYMBOL *, TYPE *, TREE *);
void inline_symbol_ref(SYMBOL *);

/* dynarray.c */
RAA *raa_init(void);
void raa_free(RAA *);
long raa_read(RAA *, long);
RAA *raa_write(RAA *, long, long);
SAA *saa_init(long);
void saa_free(SAA *);
void *saa_wstruct(SAA *);
void saa_wbytes(SAA *, const void *, long);
void saa_rewind(SAA *);
void *saa_rstruct(SAA *);
void *saa_rbytes(SAA *, long *);
void saa_rnbytes(SAA *, void *, long);
void saa_fread(SAA *, long, void *, long);
void saa_fwrite(SAA *, long, const void *, long);
void saa_fpwrite(SAA *, FILE *);

/* enode.c */
TREE *call(TREE *, TYPE *, COORDINATE);
TREE *vcall(SYMBOL *, TYPE *, ...);
TREE *vlasize_tree(TYPE *, int);
TREE *const_tree(uintmax_t, TYPE *);
TREE *cnst_tree(TYPE *, ...);
TREE *assignment_tree(int, TREE *, TREE *);
TREE *condexpr_tree(TREE *, TREE *, TREE *);
TREE *equality_tree(int, TREE *, TREE *);
TREE *bit_tree(int, TREE *, TREE *);
TREE *shift_tree(int, TREE *, TREE *);
TREE *optimized_struct_tree(TREE *);
bool_t is_callb(TREE *);
TYPE *check_assignment(TYPE *, TREE *);
TREE *assignment(SYMBOL *, TREE *);
TREE *addrof(TREE *);
void typeerror(int, TREE *, TREE *);

#ifdef PROF
/* event.c */
typedef void (*APPLYFN)(void *, void *, void *);
void attach(APPLYFN, void *, LIST **);
void apply(LIST *, void *, void *);
#endif

/* expr.c */
int intexpr(int, int);
TREE *constexpr(int);
TREE *expr(int);
TREE *expr0(int);
TREE *expr1(int);
bool_t is_compound_literal(TREE *);
TREE *field_tree(TREE *, const char *);
TREE *increment_tree(int, TREE *, TREE *);
TREE *id_tree(SYMBOL *);
TREE *rvalue(TREE *);
TREE *lvalue(TREE *);
TREE *condexpr(int);
TREE *cond(TREE *);
TREE *pointer(TREE *);
TREE *cast(TREE *, TYPE *);
TREE *retype(TREE *, TYPE *);
TREE *value(TREE *);
char *funcname(TREE *);

/* init.c */
void set_segment(int);
#ifdef PROF
void init_pointer(SYMBOL *);
#endif
TYPE *global_initializer(SYMBOL *, TYPE *);
TREE *local_initializer(SYMBOL *, TYPE *);

/* input.c */
void cc_input_init(void);
void cc_nextline(void);
void cc_fillbuf(void);

/* intrin.c */
void intrinsic_init(void);
TREE *intrinsic_call(TREE *, TYPE *, TYPE *, TREE *);
bool_t enable_or_disable_intrinsic(bool_t, const char *);

/* lex.c */
void expect(int);
void skipto(int, char []);
void follow(int, char []);
void puttok(int);
int gettok(void);
int getchr(void);
char *getinp(void);

/* list.c */
LIST *listappend(void *, LIST *);
LIST *listinsert(void *, LIST *, LIST *);
LIST *listdelete(LIST *, LIST *);
int listelems(LIST *);
void *listvector(LIST **, uint_t);

/* main.c */
void apperror(WINERR, ...);
void printmsg(int, ...);
void errorexit(int);

/* output.c */
void print(const char *, ...);
void fprint(FILE *, char *, const char *, ...);
void vfprint(FILE *, char *, const char *, va_list);

/* peephole.c */
void peephole_optimizer(const char *[]);

/* prof.c */
void prof_init(void);

/* simp.c */
TREE *simplify(int, TYPE *, TREE *, TREE *);

/* stmt.c */
CODE *new_code(int);
bool_t reachable_code(int);
void new_local_var(SYMBOL *);
void new_execution_point(COORDINATE *);
void parse_statement(int, SWTCH *, SEH *, int);
void return_value(TREE *);
void new_label(int);
void equate_labels(SYMBOL *, SYMBOL *);

/* string.c */
char *string(const char *);
char *stringd(intmax_t);
char *stringf(const char *, ...);
char *stringn(const char *, size_t);
widechar_t *wstringn(const widechar_t *, size_t);

/* sym.c */
TABLE *new_symbol_table(TABLE *, int);
SYMBOL *install_symbol(const char *, TABLE **, int, uint_t);
SYMBOL *relocate_symbol(const char *, TABLE *, TABLE *);
SYMBOL *lookup_symbol(const char *, TABLE *);
void for_each_symbol(TABLE *, int, void (*)(SYMBOL *, void *), void *);
SYMBOL *all_symbols(TABLE *);
void enter_scope(void);
void leave_scope(void);
SYMBOL *make_symbol(int, const char *, TYPE *);
SYMBOL *make_ident(int, TYPE *, int);
SYMBOL *make_temp_ident(int, TYPE *);
void define_global(SYMBOL *, int);
SYMBOL *define_local(int, int, int);
int make_label(int);
SYMBOL *find_label(int);
SYMBOL *constant(TYPE *, VALUE);
char *value_to_str(TYPE *, VALUE);
SYMBOL *intconst(int);
SYMBOL *strconst(const char *);
void add_locus(TABLE *, COORDINATE *);
void use_symbol(SYMBOL *, COORDINATE);
SYMBOL *find_type_symbol(TYPE *);

/* tree.c */
TREE *new_tree(int, TYPE *, TREE *, TREE *);
TREE *tree_to_arena(TREE *, uint_t);
TREE *expr_in_arena(TREE *(*)(int), int, uint_t);
TREE *root(TREE *);
char *opname(int);
int nodeid(TREE *);
bool_t *printed(int);
void printtree(TREE *);

/* types.c */
void setup_basic_types(void);
void remove_types_from_scope(int);
TYPE *new_array(TYPE *, int, int);
TYPE *new_struct(int, char *);
FIELD *new_struct_field(char *, TYPE *, TYPE *);
FIELD *struct_field_reference(const char *, TYPE *);
TYPE *optimized_struct_type(TYPE *ty);
TYPE *composite_type(TYPE *, TYPE *);
TYPE *qual(int, TYPE *);
TYPE *array_to_ptr(TYPE *);
TYPE *ptr(TYPE *);
TYPE *dereference_type(TYPE *);
TYPE *func(TYPE *, TYPE **, TREE **, int, int);
TYPE *func_return(TYPE *);
TYPE *func_type(TYPE *, ...);
bool_t has_varargs(TYPE *);
bool_t has_prototype(TYPE *);
bool_t is_same_type(TYPE *, TYPE *, bool_t);
TYPE *promote_type(TYPE *);
TYPE *signedint_type(TYPE *);
int ttob(TYPE *);
TYPE *btot(int, int);
void outtype(TYPE *, FILE *, char *);
void print_prototype(SYMBOL *, SYMBOL *[]);
void print_declaration(SYMBOL *, TYPE *);
char *typestring(TYPE *, char *);

/* utils.c */
#if defined(PODEBUG) && defined(__POCC__)
#define my_alloc(n)  my_trace_alloc((n), __FILE__, __LINE__)
#define my_realloc(p,n)  my_trace_realloc((p), (n), __FILE__, __LINE__)
void *my_trace_alloc(size_t, const char *, long);
void *my_trace_realloc(void *, size_t, const char *, long);
#else
void *my_alloc(size_t);
void *my_realloc(void *, size_t);
#endif
void my_free(void *);
int my_bsearch(const char *, const char **, int, int (__cdecl *)(const char *, const char *));
char *my_strdup(const char *);
char *my_strndup(const char *, size_t);
char *my_strncat(const char *, size_t, const char *, size_t);
char *outnum(char *, int);
int bitcount(uint_t);
void update_extension_in_file(char *, const char *);
size_t my_fullpath(char *, const char *, const char *);
const char *basename(const char *);
void my_getversion(short *, short *);
bool_t disabled_warning(WINERR);
void enable_or_disable_warning(bool_t, WINERR);
void *copy_warnings(void);
void restore_warnings(void *);

/* Global variables */
extern struct options options;
extern HANDLE hmod;
extern INTERFACE *IR;
extern OUTFMT *OF;
extern FILE *fdo;
extern int nerrs;
extern int nodecount;
extern SYMBOL *funcsym;
extern SYMBOL *retv;
extern SEHINFO sehinfo;
extern uint_t funca;
extern TREE *(*optree[])(int, TREE *, TREE *);
extern float refinc;
extern bool_t haslonglong;
extern bool_t optparam;
extern int funcvla;

#ifdef PROF
extern EVENTS events;
#endif

extern char kind[];
extern int tok;
extern char *tokstr;
extern SYMBOL *toksym;
extern COORDINATE src;

extern uchar_t *cp;
extern char *file;
extern char *firstfile;
extern uchar_t *limit;
extern char *line;
extern int lineno;

extern TABLE *constants;
extern TABLE *externals;
extern TABLE *globals;
extern TABLE *identifiers;
extern TABLE *labels;
extern TABLE *types;
extern int scope;
#ifdef XREF
extern LIST *loci;
extern LIST *symbols;
#endif

extern bool_t need_const;
extern bool_t explicit_cast;
extern bool_t no_const_check;

extern CODE codehead;
extern CODE *codelist;
extern TABLE *stmtlabs;

extern TYPE *booltype;
extern TYPE *chartype;
extern TYPE *doubletype;
extern TYPE *floattype;
extern TYPE *inttype;
extern TYPE *longdoubletype;
extern TYPE *longtype;
extern TYPE *longlongtype;
extern TYPE *shorttype;
extern TYPE *signedchartype;
extern TYPE *unsignedchartype;
extern TYPE *unsignedlongtype;
extern TYPE *unsignedlonglongtype;
extern TYPE *unsignedshorttype;
extern TYPE *unsignedtype;
extern TYPE *funcptype;
extern TYPE *charptype;
extern TYPE *voidptype;
extern TYPE *voidtype;
extern TYPE *unsignedptrtype;
extern TYPE *signedptrtype;
extern TYPE *widechartype;
#ifdef HAS_C99_COMPLEX
extern TYPE *complexdoubletype;
extern TYPE *complexfloattype;
extern TYPE *complexlongdoubletype;
#endif

#ifdef PROF
extern int ncalled;
extern int npoints;
extern int findfunc(char *, char *);
extern int findcount(char *, int, int);
#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _LCC_H */
