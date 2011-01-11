/*
 * LBURG definitions.
 */
typedef struct _XINTERFACE {
    uchar_t max_unaligned_load;
    SYMBOL *(*rmap)(int);
    /**/
    void (*blkfetch)(int, int, int, int);
    void (*blkstore)(int, int, int, int);
    void (*blkloop)(int, int, int, int, int, int []);
    void (*_label)(NODE *);
    int (*_rule)(void *, int);
    short **_nts;
    void (*_kids)(NODE *, int, NODE **);
    char **_string;
    char **_templates;
    char *_isinstruction;
    char **_ntname;
    void (*emit2)(NODE *);
    void (*doarg)(NODE *);
    void (*target)(NODE *);
    void (*clobber)(NODE *);
} XINTERFACE;

/* gen.c */
extern int register_variable(SYMBOL *, SYMBOL *);
extern int register_number(NODE *);
extern uint_t register_location(SYMBOL *);
extern void blkcopy(int, int, int, int, int, int[]);
extern void emitasm(NODE *, int);
extern bool_t may_recalc_temp(NODE *);
extern void mkauto(SYMBOL *);
extern int mkparm(int, int);
extern SYMBOL *mkreg(char *, int, int, int);
extern SYMBOL *mkwildcard(SYMBOL **);
extern int move(NODE *);
extern int notarget(NODE *);
extern int range(NODE *, int, int);
extern void target_register(NODE *, int, SYMBOL *);
extern void set_register(NODE *, SYMBOL *);
extern void spill(uint_t, int, NODE *);

/* Global variables */
extern int argoffset, maxargoffset;
extern int dflag;
extern int dalign, salign;
extern int framesize;
extern uint_t freemask[], usedmask[];
extern int offset, maxoffset;
extern int swap;
extern uint_t tmask[], vmask[];

typedef struct _XNODE {
    uint_t listed: 1;
    uint_t registered: 1;
    uint_t emitted: 1;      /* true if emitted */
    uint_t copy: 1;
    uint_t equatable: 1;
    uint_t spills: 1;
    uint_t may_recalc: 1;
    void *state;
    NODE *kids[3];
    NODE *prev;
    NODE *next;
    NODE *prevuse;
    short inst;
    short argno;
} XNODE;

typedef struct _REGNODE {
    SYMBOL *vbl;
    short set;
    short number;
    uint_t mask;
} *REGNODE;

enum { IREG=0, FREG=1 };

typedef struct _XSYMBOL {
    char *name;
    uint_t eaddr;  /* omit; MSVC crashes if we remove this! Que?! */
    int offset;
    NODE *lastuse;
    int usecount;
    REGNODE regnode;
    SYMBOL **wildcard;
} XSYMBOL;

enum { RX=2 };

typedef struct _ENV {
    int offset;
    uint_t freemask[2];
} ENV;

#define LBURG_MAX SHRT_MAX

enum { VREG=(55<<4) };

/* Exported for the front end */
extern void blockbeg(ENV *);
extern void blockend(ENV *);
extern void emit(NODE *);
extern NODE *gencode(NODE *);

#ifdef PODEBUG
#define debug(x) (void)(dflag&&((x),0))
#else
#define debug(x) (void)0
#endif

