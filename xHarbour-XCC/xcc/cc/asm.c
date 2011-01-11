/****************************************************************************
 *                                                                          *
 * File    : asm.c                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic Assembler; Main module.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-02-23  Character mapping through ascmap[] added.            *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

/* forward reference info */
typedef struct _FWDREF {
    int lineno;     /* source line */
    int operand;    /* operand index */
    long offset;
} FWDREF;

/*
 * Generic directives.
 */
static const char *keywords[] = {
    "align",
    "alignb",
    "bits",
    "common",
    "cpu",
    "extern",
    "global",
    "section",
    "segment"
};
enum {
    KW_ALIGN = 0,
    KW_ALIGNB,
    KW_BITS,
    KW_COMMON,
    KW_CPU,
    KW_EXTERN,
    KW_GLOBAL,
    KW_SECTION,
    KW_SEGMENT,
    KW_DRIVER
};

const uchar_t ascmap[256] = {
    /* 00 nul */        0,
    /* 01 soh */        0,
    /* 02 stx */        0,
    /* 03 etx */        0,
    /* 04 eot */        0,
    /* 05 enq */        0,
    /* 06 ack */        0,
    /* 07 bel */        0,
    /* 08 bs  */        0,
    /* 09 ht  */        0,
    /* 0A nl  */        0,
    /* 0B vt  */        0,
    /* 0C ff  */        0,
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
    /* 20 sp  */        0,
    /* 21 !   */        0,
    /* 22 "   */        0,
    /* 23 #   */        ASID2,
    /* 24 $   */        ASID2,
    /* 25 %   */        ASID2,
    /* 26 &   */        0,
    /* 27 '   */        0,
    /* 28 (   */        0,
    /* 29 )   */        0,
    /* 2A *   */        0,
    /* 2B +   */        0,
    /* 2C ,   */        0,
    /* 2D -   */        0,
    /* 2E .   */        ASID1|ASID2,
    /* 2F /   */        0,
    /* 30 0   */        ASNUM|ASID2,
    /* 31 1   */        ASNUM|ASID2,
    /* 32 2   */        ASNUM|ASID2,
    /* 33 3   */        ASNUM|ASID2,
    /* 34 4   */        ASNUM|ASID2,
    /* 35 5   */        ASNUM|ASID2,
    /* 36 6   */        ASNUM|ASID2,
    /* 37 7   */        ASNUM|ASID2,
    /* 38 8   */        ASNUM|ASID2,
    /* 39 9   */        ASNUM|ASID2,
    /* 3A :   */        0,
    /* 3B ;   */        0,
    /* 3C <   */        0,
    /* 3D =   */        0,
    /* 3E >   */        0,
    /* 3F ?   */        ASID1|ASID2,
    /* 40 @   */        ASID1|ASID2,
    /* 41 A   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 42 B   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 43 C   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 44 D   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 45 E   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 46 F   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 47 G   */        ASCHR|ASID1|ASID2,
    /* 48 H   */        ASCHR|ASID1|ASID2,
    /* 49 I   */        ASCHR|ASID1|ASID2,
    /* 4A J   */        ASCHR|ASID1|ASID2,
    /* 4B K   */        ASCHR|ASID1|ASID2,
    /* 4C L   */        ASCHR|ASID1|ASID2,
    /* 4D M   */        ASCHR|ASID1|ASID2,
    /* 4E N   */        ASCHR|ASID1|ASID2,
    /* 4F O   */        ASCHR|ASID1|ASID2,
    /* 50 P   */        ASCHR|ASID1|ASID2,
    /* 51 Q   */        ASCHR|ASID1|ASID2,
    /* 52 R   */        ASCHR|ASID1|ASID2,
    /* 53 S   */        ASCHR|ASID1|ASID2,
    /* 54 T   */        ASCHR|ASID1|ASID2,
    /* 55 U   */        ASCHR|ASID1|ASID2,
    /* 56 V   */        ASCHR|ASID1|ASID2,
    /* 57 W   */        ASCHR|ASID1|ASID2,
    /* 58 X   */        ASCHR|ASID1|ASID2,
    /* 59 Y   */        ASCHR|ASID1|ASID2,
    /* 5A Z   */        ASCHR|ASID1|ASID2,
    /* 5B [   */        0,
    /* 5C \   */        0,
    /* 5D ]   */        0,
    /* 5E ^   */        0,
    /* 5F _   */        ASID1|ASID2,
    /* 60 `   */        0,
    /* 61 a   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 62 b   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 63 c   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 64 d   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 65 e   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 66 f   */        ASCHR|ASID1|ASID2|ASHEX,
    /* 67 g   */        ASCHR|ASID1|ASID2,
    /* 68 h   */        ASCHR|ASID1|ASID2,
    /* 69 i   */        ASCHR|ASID1|ASID2,
    /* 6A j   */        ASCHR|ASID1|ASID2,
    /* 6B k   */        ASCHR|ASID1|ASID2,
    /* 6C l   */        ASCHR|ASID1|ASID2,
    /* 6D m   */        ASCHR|ASID1|ASID2,
    /* 6E n   */        ASCHR|ASID1|ASID2,
    /* 6F o   */        ASCHR|ASID1|ASID2,
    /* 70 p   */        ASCHR|ASID1|ASID2,
    /* 71 q   */        ASCHR|ASID1|ASID2,
    /* 72 r   */        ASCHR|ASID1|ASID2,
    /* 73 s   */        ASCHR|ASID1|ASID2,
    /* 74 t   */        ASCHR|ASID1|ASID2,
    /* 75 u   */        ASCHR|ASID1|ASID2,
    /* 76 v   */        ASCHR|ASID1|ASID2,
    /* 77 w   */        ASCHR|ASID1|ASID2,
    /* 78 x   */        ASCHR|ASID1|ASID2,
    /* 79 y   */        ASCHR|ASID1|ASID2,
    /* 7A z   */        ASCHR|ASID1|ASID2,
    /* 7B {   */        0,
    /* 7C |   */        0,
    /* 7D }   */        0,
    /* 7E ~   */        ASID2,
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
    /* C4 Ä   */        ASID1|ASID2,    /* ANSI */
    /* C5 Å   */        ASID1|ASID2,    /* ANSI */
    /* C6     */        0,
    /* C7     */        0,
    /* C8     */        0,
    /* C9 É   */        ASID1|ASID2,    /* ANSI */
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
    /* D6 Ö   */        ASID1|ASID2,    /* ANSI */
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
    /* E4 ä   */        ASID1|ASID2,    /* ANSI */
    /* E5 å   */        ASID1|ASID2,    /* ANSI */
    /* E6     */        0,
    /* E7     */        0,
    /* E8     */        0,
    /* E9 é   */        ASID1|ASID2,    /* ANSI */
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
    /* F6 ö   */        ASID1|ASID2,    /* ANSI */
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

ASSEMBLER as;
LOCATION location;                  /* current location */
LIST *aslist = NULL, *curp;

static int bits = 32;
static int pass = 0;
static RAA *offsets;                /* current offset for each segment */
static SAA *forwrefs;               /* keep track of forward references */
static long next_seg = 0;

/* Static function prototypes */
static void assemble_file(void);
static char *getline(void);
static void resynch(char *);
static int getkw(const char **, char **);
static void seg_init(void);

/****************************************************************************
 *                                                                          *
 * Function: x86_init                                                       *
 *                                                                          *
 * Purpose : Initialize the generic assembler (X86 mode).                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

void x86_init(void)
{
    x86_scanner_init();
    x86_parser_init();
    x86_emitter_init();
}

/****************************************************************************
 *                                                                          *
 * Function: arm_init                                                       *
 *                                                                          *
 * Purpose : Initialize the generic assembler (ARM mode).                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

void arm_init(void)
{
    arm_scanner_init();
    arm_parser_init();
    arm_emitter_init();
}

/****************************************************************************
 *                                                                          *
 * Function: assembler                                                      *
 *                                                                          *
 * Purpose : Assemble code and write the object file.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void assembler(void)
{
    seg_init();

    offsets = raa_init();
    forwrefs = saa_init(sizeof(FWDREF));

    /*
     * We must call asmlab_init() before OF->filebeg() since some
     * object formats might want to define labels in their init routines.
     */
    asmlab_init();
    (*OF->filebeg)();
    assemble_file();
    (*OF->fileend)(nerrs != 0);
    asmlab_cleanup();

    raa_free(offsets);
    saa_free(forwrefs);

    asmexp_cleanup();
    as.scanner.cleanup();
}

/****************************************************************************
 *                                                                          *
 * Function: assemble_file                                                  *
 *                                                                          *
 * Purpose : Main body of the 3-pass assembler.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-12-12  Bugfix: skip initial $ for COMMON symbols too.       *
 *           01-04-07  New directive added: [CPU level].                    *
 *           04-05-19  Added verbose output (pass count).                   *
 *                                                                          *
 ****************************************************************************/

#define get_curr_offset()   raa_read(offsets,location.segment)
#define set_curr_offset(x)  (void)(offsets = raa_write(offsets,location.segment,(x)))

static void assemble_file(void)
{
    FWDREF *forwref;
    INSN insn;
    const char *line;
    char *value;
    long segment, offset;
    int lineno;
    int i;

    /*
     * Pass number one.
     */
    pass = 1;
    curp = aslist;
    lineno = 0;

    location.segment = (*OF->segment)(NULL, NULL, pass, &bits);
    location.offset = offset = get_curr_offset();

    if (options.infolevel > 1)
        printmsg(MSG_PASS_COUNT, pass, 3);

    while ((line = getline()) != NULL)
    {
        lineno++;

        /*
         * Parse special directives.
         */
        if ((i = getkw(&line, &value)) != -1)
        {
            switch (i)
            {
                bool_t ok;
                char *p;

                case KW_BITS:  /* [BITS bits] */
                    break;

                case KW_CPU:  /* [CPU level] */
                    if (!as.emitter.cpulevel(value))
                        apperror(RCERROR(ERROR_UNKNOWN_CPU_TYPE), value);
                    break;

                case KW_SECTION:  /* [SECTION name [attributes]] */
                case KW_SEGMENT:
                    for (p = value; *p && !isspace(*p); p++)
                        ;
                    while (*p && isspace(*p))
                        *p++ = '\0';
                    segment = (*OF->segment)(value, p, pass, &bits);
                    if (segment == NO_SEG)
                        apperror(RCERROR(ERROR_UNDEF_SECTION_NAME), value);
                    else
                        location.segment = segment;
                    break;

                case KW_EXTERN:  /* [EXTERN label] */
                    if (*value == '$') value++;  /* skip initial $ */
                    for (p = value, ok = isidstart(*p); ok && *p; ok = isidchar(*p), p++)
                        ;
                    if (!ok)
                    {
                        apperror(RCERROR(ERROR_EXPECTING_IDENT_AFTER), "EXTERN");
                        break;
                    }
                    /* allow re-EXTERN to be ignored */
                    if (!asmlab_is_extern(value))
                    {
                        asmlab_declare_global(value);
                        asmlab_define(value, seg_alloc(), 0L, FALSE, TRUE);
                    }
                    break;

                case KW_GLOBAL:  /* [GLOBAL symbol] */
                    if (*value == '$') value++;  /* skip initial $ */
                    for (p = value, ok = isidstart(*p); ok && *p; ok = isidchar(*p), p++)
                        ;
                    if (!ok)
                    {
                        apperror(RCERROR(ERROR_EXPECTING_IDENT_AFTER), "GLOBAL");
                        break;
                    }
                    asmlab_declare_global(value);
                    break;

                case KW_COMMON:  /* [COMMON symbol size] */
                    if (*value == '$') value++;  /* skip initial $ */
                    for (p = value, ok = isidstart(*p); ok && *p && !isspace(*p); ok = isidchar(*p), p++)
                        ;
                    if (!ok)
                    {
                        apperror(RCERROR(ERROR_EXPECTING_IDENT_AFTER), "COMMON");
                        break;
                    }
                    if (*p)
                    {
                        bool_t error;
                        long size;

                        while (*p && isspace(*p))
                            *p++ = '\0';

                        size = (long)readnum(p, &error);
                        if (error)
                            apperror(RCERROR(ERROR_INVALID_COMMON_SIZE));
                        else
                            asmlab_define_common(value, seg_alloc(), size);
                    }
                    else
                    {
                        apperror(RCERROR(ERROR_INVALID_COMMON_SIZE));
                    }
                    break;

                case KW_DRIVER:
                default:
                    break;
            }
        }
        else  /* not a directive */
        {
            long size;

            as.parser.parse(pass, line, &insn);

            if (insn.forw_ref)
            {
                for (i = 0; i < insn.nopands; i++)
                {
                    if (insn.aops[i].opflags & OPFLAG_FORWARD)
                    {
                        FWDREF *forwref = (FWDREF *)saa_wstruct(forwrefs);
                        forwref->lineno = lineno;
                        forwref->operand = i;
                        forwref->offset = offset;
                    }
                }
            }

            size = as.emitter.emit(pass, location.segment, offset, 0, bits, &insn);
            if (size != -1)
            {
                offset += size;
                set_curr_offset(offset);
            }

            as.parser.cleanup(&insn);
        }

        location.offset = offset = get_curr_offset();
    }

    if (nerrs != 0)
        return;

    /*
     * Pass number two.
     */
    pass = 2;
    curp = aslist;
    lineno = 0;

    saa_rewind(forwrefs);
    forwref = saa_rstruct(forwrefs);

    raa_free(offsets);
    offsets = raa_init();
    location.segment = (*OF->segment)(NULL, NULL, pass, &bits);
    location.offset = offset = get_curr_offset();

    if (options.infolevel > 1)
        printmsg(MSG_PASS_COUNT, pass, 3);

    while ((line = getline()) != NULL)
    {
        lineno++;

        /*
         * Parse special directives.
         */
        if ((i = getkw(&line, &value)) != -1)
        {
            switch (i)
            {
                bool_t ok;
                char *p;

                case KW_CPU:  /* [CPU level] */
                    if (!as.emitter.cpulevel(value))
                        apperror(RCERROR(ERROR_UNKNOWN_CPU_TYPE), value);
                    break;

                case KW_SECTION:  /* [SECTION name [attributes]] */
                case KW_SEGMENT:
                    for (p = value; *p && !isspace(*p); p++)
                        ;
                    while (*p && isspace(*p))
                        *p++ = '\0';
                    segment = (*OF->segment)(value, p, pass, &bits);
                    if (segment == NO_SEG)
                        apperror(RCERROR(ERROR_UNDEF_SECTION_NAME), value);
                    else
                        location.segment = segment;
                    break;

                case KW_EXTERN:  /* [EXTERN label] */
                    if (*value == '$') value++;  /* skip initial $ */
                    for (p = value, ok = isidstart(*p); ok && *p; ok = isidchar(*p), p++)
                        ;
                    asmlab_redefine(value, seg_alloc(), 0L);
                    break;

                case KW_BITS:    /* [BITS bits] */
                case KW_GLOBAL:  /* [GLOBAL symbol] */
                case KW_COMMON:  /* [COMMON symbol size] */
                case KW_DRIVER:
                default:
                    break;
            }
        }
        else  /* not a directive */
        {
            long oldoffset = 0;
            long size;

            as.parser.parse(pass, line, &insn);

            insn.forw_ref = (forwref && forwref->lineno == lineno);
            if (insn.forw_ref)
            {
                do
                {
                    oldoffset = forwref->offset;
                    insn.aops[forwref->operand].opflags |= OPFLAG_FORWARD;
                    forwref = saa_rstruct(forwrefs);
                } while (forwref && forwref->lineno == lineno);
            }

            size = as.emitter.emit(pass, location.segment, offset, oldoffset, bits, &insn);
            if (size != -1)
            {
                offset += size;
                set_curr_offset(offset);
            }

            as.parser.cleanup(&insn);
        }

        location.offset = offset = get_curr_offset();
    }

    /*
     * Pass number three.
     */
    pass = 3;
    curp = aslist;
    lineno = 0;

    saa_rewind(forwrefs);
    forwref = saa_rstruct(forwrefs);

    raa_free(offsets);
    offsets = raa_init();
    location.segment = (*OF->segment)(NULL, NULL, pass, &bits);
    location.offset = offset = get_curr_offset();

    if (options.infolevel > 1)
        printmsg(MSG_PASS_COUNT, pass, 3);

    while ((line = getline()) != NULL)
    {
        lineno++;

        /*
         * Parse special directives.
         */
        if ((i = getkw(&line, &value)) != -1)
        {
            switch (i)
            {
                char *p;

                case KW_CPU:  /* [CPU level] */
                    if (!as.emitter.cpulevel(value))
                        apperror(RCERROR(ERROR_UNKNOWN_CPU_TYPE), value);
                    break;

                case KW_SECTION:  /* [SECTION name [attributes]] */
                case KW_SEGMENT:
                    for (p = value; *p && !isspace(*p); p++)
                        ;
                    while (*p && isspace(*p))
                        *p++ = '\0';
                    segment = (*OF->segment)(value, p, pass, &bits);
                    if (segment == NO_SEG)
                        apperror(RCERROR(ERROR_UNDEF_SECTION_NAME), value);
                    else
                        location.segment = segment;
                    break;

                case KW_BITS:    /* [BITS bits] */
                case KW_EXTERN:  /* [EXTERN label] */
                case KW_GLOBAL:  /* [GLOBAL symbol] */
                case KW_COMMON:  /* [COMMON symbol size] */
                case KW_DRIVER:
                default:
                    break;
            }
        }
        else  /* not a directive */
        {
            as.parser.parse(pass, line, &insn);

            insn.forw_ref = (forwref && forwref->lineno == lineno);
            if (insn.forw_ref)
            {
                do
                {
                    insn.aops[forwref->operand].opflags |= OPFLAG_FORWARD;
                    forwref = saa_rstruct(forwrefs);
                } while (forwref && forwref->lineno == lineno);
            }

            offset += as.emitter.emit(pass, location.segment, offset, 0, bits, &insn);
            set_curr_offset(offset);

            as.parser.cleanup(&insn);
        }

        location.offset = offset = get_curr_offset();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: getline                                                        *
 *                                                                          *
 * Purpose : Return a line of preprocessed source.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static char *getline(void)
{
    if (curp == aslist)  /* first line: reset lineno */
        src.y = 0;

    if (curp)
    {
        char *cp;

        curp = curp->link;
        cp = (char *)curp->data;
        if (curp == aslist) curp = NULL;

        if (options.assemble) src.y++;

        if (*cp == '#')  /* handle #line */
        {
            resynch(cp);
            cp = getline();
        }

        return cp;
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: resynch                                                        *
 *                                                                          *
 * Purpose : Set line number/filename in # n [ "file" ]                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void resynch(char *cp)
{
    for (cp += 1; *cp == ' ' || *cp == '\t'; cp++)
        ;

    if (strncmp(cp, "line", 4) == 0)
    {
        int lineno;

        for (cp += 4; *cp == ' ' || *cp == '\t'; cp++)
            ;

        for (lineno = 0; *cp >= '0' && *cp <= '9'; )
             lineno = 10 * lineno + *cp++ - '0';

        while (*cp == ' ' || *cp == '\t')
            cp++;

        if (*cp == '"')
        {
            file = (char *)++cp;

            while (*cp && *cp != '"' && *cp != '\n')
                cp++;

            file = stringn(file, (char *)cp - file);

            if (*cp == '\n')
                apperror(RCWARNING1(ERROR_MISSING_QUOTE_IN_PP_LINE));

            src.file = file;
            src.y = lineno;

            if (options.dbglevel > 0 && pass == 3)
                (*OF->lineno)(location.segment, lineno);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: getkw                                                          *
 *                                                                          *
 * Purpose : Look for assembler directives (keywords).                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int getkw(const char **line, char **value)
{
    static char argument[80];
    char keyword[20];
    char *q;
    char endchr;
    const char *p;
    int i;

    /*
     * Begin by determine if this looks like a valid directive:
     * " [directive args] ; comment "  *OR*
     * "  directive args  ; comment "
     */
    for (p = *line; isspace(*p); p++)
        ;

    if (*p == '[')
        endchr = ']', p++;
    else if (options.assemble)
        endchr = ';';
    else
        return -1;

    /*
     * Copy keyword to local buffer.
     */
    q = keyword;
    while (*p && *p != endchr && !isspace(*p) && q-keyword < sizeof(keyword))
        *q++ = *p++;

    if (q-keyword == sizeof(keyword))
        return -1;

    *q = '\0';

    /* Skip leading whitespace */
    while (isspace(*p)) p++;

    /*
     * Copy argument to local buffer.
     */
    q = argument;
    while (*p && *p != endchr && q-argument < sizeof(argument))
        *q++ = *p++;

    if (q-argument == sizeof(argument))
        return -1;

    *q = '\0';

    if (endchr != ';')
    {
        if (*p++ != endchr)
            return -1;

        while (isspace(*p)) p++;

        if (*p && *p != ';')
            return -1;
    }

    *value = argument;

    /*
     * See if it really was a directive.
     */
    i = my_bsearch(keyword, keywords, NELEMS(keywords), _stricmp);
    if (i == KW_ALIGN)
    {
        i = atoi(*value);
        *line = stringf("times ((($-$$)^%d)+1)&%d nop", i-1, i-1);
        i = -1;
    }
    else if (i == KW_ALIGNB)
    {
        i = atoi(*value);
        *line = stringf("resb ((($-$$)^%d)+1)&%d\n", i-1, i-1);
        i = -1;
    }
    else if (i != -1)
        ;
    else if ((*OF->directive)(keyword, argument, pass))
        i = KW_DRIVER;

    return i;
}

/****************************************************************************
 *                                                                          *
 * Function: seg_init                                                       *
 *                                                                          *
 * Purpose : Initialize the segment number generator.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void seg_init(void)
{
    next_seg = 0;
}

/****************************************************************************
 *                                                                          *
 * Function: seg_alloc                                                      *
 *                                                                          *
 * Purpose : Return a new segment number.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

long seg_alloc(void)
{
    return (next_seg += 2) - 2;
}

/****************************************************************************
 *                                                                          *
 * Function: readnum                                                        *
 *                                                                          *
 * Purpose : Convert a string into an integer.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

intmax_t readnum(const char *str, bool_t *error)
{
    const char *r = str, *q;
    intmax_t radix;
    uintmax_t result;
    uintmax_t checklimit;
    int digit;
    int last;
    int sign;
    int warn = FALSE;

    *error = FALSE;

    while (isspace(*r)) r++;  /* find start of number */

    sign = (*r == '-') ? r++, -1 : 1;

    q = r;

    while (isalnum(*q)) q++;  /* find end of number */

    /*
     * If it begins 0x or 0X, or ends in H, it's in hex. if it
     * ends in Q, it's octal. if it ends in B, it's binary.
     * Otherwise, it's ordinary decimal.
     */
    if (*r == '0' && (r[1] == 'x' || r[1] == 'X'))
        radix = 16, r += 2;
    else if (q[-1] == 'H' || q[-1] == 'h')
        radix = 16, q--;
    else if (q[-1] == 'Q' || q[-1] == 'q')
        radix = 8, q--;
    else if (q[-1] == 'B' || q[-1] == 'b')
        radix = 2, q--;
    else
        radix = 10;

    /*
     * If this number has been found for us by something other than
     * the ordinary scanners, then it might be malformed by having
     * nothing between the prefix and the suffix. Check this case
     * now.
     */
    if (r >= q)
    {
        *error = TRUE;
        return 0;
    }

    /*
     * 'checklimit' must be 2**64 / radix. We can't do that in
     * 64-bit arithmetic, which we're (probably) using, so we
     * cheat: since we know that all radices we use are even, we
     * can divide 2**63 by radix/2 instead.
     */
    checklimit = ((uintmax_t)INTMAX_MAX + 1) / (radix >> 1);

    /*
     * Calculate the highest allowable value for the last digit
     * of a 64 bit constant... in radix 10, it is 6, otherwise it is 0.
     */
    last = (radix == 10) ? 6 : 0;

    result = 0;
    while (*r && r < q)
    {
        if (*r < '0' || (*r > '9' && *r < 'A') || (digit = numvalue(*r)) >= radix)
        {
            *error = TRUE;
            return 0;
        }

        if (result > checklimit || (result == checklimit && digit >= last))
            warn = TRUE;

        result = radix * result + digit;
        r++;
    }

    if (warn)
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_INT_CONST));

    return result*sign;
}

/****************************************************************************
 *                                                                          *
 * Function: readstrnum                                                     *
 *                                                                          *
 * Purpose : Convert a numeric string constant into an integer.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

intmax_t readstrnum(const char *str, int length)
{
    intmax_t charconst = 0;
    int i;

    str += length;
    for (i = 0; i < length; i++)
    {
        /* we currently ignore overflows */
        charconst = (charconst << 8) + (uchar_t)*--str;
    }

    return charconst;
}

/****************************************************************************
 *                                                                          *
 * Function: is_simple                                                      *
 *                                                                          *
 * Purpose : Return true if the argument is a simple scalar (or a far-      *
 *           absolute, which counts.)                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t is_simple(EXPR *e)
{
    while (e->type && !e->value)
        e++;

    if (!e->type)
        return TRUE;

    if (e->type != EXPR_SIMPLE)
        return FALSE;

    do
        e++;
    while (e->type && !e->value);
    if (e->type && e->type < EXPR_SEGBASE+SEG_ABS)
        return FALSE;

    return TRUE;
}

/****************************************************************************
 *                                                                          *
 * Function: is_really_simple                                               *
 *                                                                          *
 * Purpose : Return true if the argument is a simple scalar,                *
 *           *NOT* a far-absolute.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t is_really_simple(EXPR *e)
{
    while (e->type && !e->value)
        e++;

    if (!e->type)
        return TRUE;

    if (e->type != EXPR_SIMPLE)
        return FALSE;

    do
        e++;
    while (e->type && !e->value);
    if (e->type) return FALSE;

    return TRUE;
}

/****************************************************************************
 *                                                                          *
 * Function: is_reloc                                                       *
 *                                                                          *
 * Purpose : Return true if the argument is relocatable (i.e. a simple      *
 *           scalar, plus at most one segment-base).                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t is_reloc(EXPR *e)
{
    /* skip initial value-0 terms */
    while (e->type && !e->value)
        e++;

    if (!e->type)
        return TRUE;

    /* FALSE if a register is present */
    if (e->type < EXPR_SIMPLE)
        return FALSE;

    if (e->type == EXPR_SIMPLE)
    {
        /* skip over a pure number term... */
        do
            e++;
        while (e->type && !e->value);
        if (!e->type) return TRUE;
    }

    /* segment base multiplier non-unity */
    if (e->value != 0 && e->value != 1)
        return FALSE;

    /* skip over *one* seg-base term... */
    do
        e++;
    while (e->type && !e->value);
    if (!e->type) return TRUE;

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: is_unknown                                                     *
 *                                                                          *
 * Purpose : Return true if the argument contains an 'unknown' part.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t is_unknown(EXPR *e)
{
    while (e->type && e->type < EXPR_UNKNOWN)
        e++;

    return (e->type == EXPR_UNKNOWN);
}

/****************************************************************************
 *                                                                          *
 * Function: is_just_unknown                                                *
 *                                                                          *
 * Purpose : Return true if the argument contains nothing but an 'unknown'  *
 *           part.                                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t is_just_unknown(EXPR *e)
{
    while (e->type && !e->value)
        e++;

    return (e->type == EXPR_UNKNOWN);
}

/****************************************************************************
 *                                                                          *
 * Function: reloc_value                                                    *
 *                                                                          *
 * Purpose : Return the scalar part of a relocatable vector (including      *
 *           simple scalar vectors - those qualify as relocatable.)         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

intmax_t reloc_value(EXPR *e)
{
    while (e->type && !e->value)
        e++;

    if (!e->type) return 0;

    if (e->type == EXPR_SIMPLE)
        return e->value;
    else
        return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: reloc_seg                                                      *
 *                                                                          *
 * Purpose : Return the segment number of a relocatable vector, or          *
 *           NO_SEG for simple scalars.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

long reloc_seg(EXPR *e)
{
    while (e->type && !e->value)
        e++;

    if (e->type == EXPR_SIMPLE)
    {
        do
            e++;
        while (e->type && !e->value);
    }

    if (!e->type)
        return NO_SEG;
    else
        return e->type - EXPR_SEGBASE;
}

