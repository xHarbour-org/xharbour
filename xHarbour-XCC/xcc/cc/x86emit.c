/****************************************************************************
 *                                                                          *
 * File    : x86emit.c                                                      *
 *                                                                          *
 * Purpose : ISO C Compiler; X86 Assembler; Code generation.                *
 *                                                                          *
 * Comment : There are lots of 16-bit support, which we don't need,         *
 *           but we keep it around for a while...                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-21  Function is_sbyte() added.                           *
 *           01-04-07  Handling of processor level added.                   *
 *                                                                          *
 ****************************************************************************/

/*
 * the actual codes (C syntax, i.e. octal):
 * \0             - terminates the code. (Unless it's a literal of course.)
 * \1, \2, \3     - that many literal bytes follow in the code stream
 * \4, \6         - the POP/PUSH (respectively) codes for CS, DS, ES, SS
 *                  (POP is never used for CS) depending on operand 0
 * \5, \7         - the second byte of POP/PUSH codes for FS, GS, depending
 *                  on operand 0
 * \10, \11, \12  - a literal byte follows in the code stream, to be added
 *                  to the register value of operand 0, 1 or 2
 * \17            - encodes the literal byte 0. (Some compilers don't take
 *                  kindly to a zero byte in the *middle* of a compile time
 *                  string constant, so I had to put this hack in.)
 * \14, \15, \16  - a signed byte immediate operand, from operand 0, 1 or 2
 * \20, \21, \22  - a byte immediate operand, from operand 0, 1 or 2
 * \24, \25, \26  - an unsigned byte immediate operand, from operand 0, 1 or 2
 * \30, \31, \32  - a word immediate operand, from operand 0, 1 or 2
 * \34, \35, \36  - select between \3[012] and \4[012] depending on 16/32 bit
 *                  assembly mode or the address-size override on the operand
 * \37            - a word constant, from the *segment* part of operand 0
 * \40, \41, \42  - a long immediate operand, from operand 0, 1 or 2
 * \50, \51, \52  - a byte relative operand, from operand 0, 1 or 2
 * \60, \61, \62  - a word relative operand, from operand 0, 1 or 2
 * \64, \65, \66  - select between \6[012] and \7[012] depending on 16/32 bit
 *                  assembly mode or the address-size override on the operand
 * \70, \71, \72  - a long relative operand, from operand 0, 1 or 2
 * \1ab           - a ModRM, calculated on EA in operand a, with the spare
 *                  field the register value of operand b.
 * \130,\131,\132 - an immediate word or signed byte for operand 0, 1, or 2
 * \133,\134,\135 - or 2 (s-field) into next opcode byte if operand 0, 1, or 2
 *                  is a signed byte rather than a word.
 * \140,\141,\142 - an immediate dword or signed byte for operand 0, 1, or 2
 * \143,\144,\145 - or 2 (s-field) into next opcode byte if operand 0, 1, or 2
 *                  is a signed byte rather than a dword.
 * \2ab           - a ModRM, calculated on EA in operand a, with the spare
 *                  field equal to digit b.
 * \30x           - might be an 0x67 byte, depending on the address size of
 *                  the memory reference in operand x.
 * \310           - indicates fixed 16-bit address size, i.e. optional 0x67.
 * \311           - indicates fixed 32-bit address size, i.e. optional 0x67.
 * \320           - indicates fixed 16-bit operand size, i.e. optional 0x66.
 * \321           - indicates fixed 32-bit operand size, i.e. optional 0x66.
 * \322           - indicates that this instruction is only valid when the
 *                  operand size is the default (instruction to disassembler,
 *                  generates no code in the assembler)
 * \330           - a literal byte follows in the code stream, to be added
 *                  to the condition code value of the instruction.
 * \331           - instruction not valid with REP prefix.  Hint for
 *                  disassembler only; for SSE instructions.
 * \332           - disassemble a rep (0xF3 byte) prefix as repe not rep.
 * \333           - REP prefix (0xF3 byte); for SSE instructions.  Not encoded
 *                  as a literal byte in order to aid the disassembler.
 * \340           - reserve <operand 0> bytes of uninitialised storage.
 *                  Operand 0 had better be a segmentless constant.
 */

#include "proj.h"
#pragma hdrstop

#include "lcc.h"
#include "x86.h"

/* instruction templates generated from x86.dat */
#include "x86insa.c"

/* effective address info */
typedef struct _EA {
    int sib_present;            /* is a SIB byte necessary? */
    int bytes;                  /* number of bytes of offset needed */
    int size;                   /* lazy - this is sib+bytes+1 */
    uchar_t modrm, sib;         /* the bytes themselves */
} EA;

/* return values from match_insn() */
#define MATCHES_MATCH       100
#define MATCHES_NO          0
#define MATCHES_WHICH_SIZE  1
#define MATCHES_NOT_SIZE    2
#define MATCHES_NOT_CPU     3

static ulong_t cpulevel = IF_PLEVEL;   /* highest level by default */

/* Static function prototypes */
static bool_t cpu_level(const char *);
static void inline_emitter(const char *, const char *, SYMBOL *, long);
static long emitter(int, long, long, long, int, INSN *);
static bool_t disp32_to_disp8(INSN *, long);
static long assemble(long, long, int, INSN *);
static long insnsize(long, long, int, INSN *);
static int match_insn(struct ITEMPLATE *, INSN *);
static long calc_insnsize(int, INSN *, char *);
static int is_sbyte(INSN *, int, int);
static void generate_insn(long, long, int, INSN *, char *, long);
static void getaddr(STDOPAND *, long *, long *);
static void out(long, long, void *, ulong_t, long);
static EA *process_ea(STDOPAND *, EA *, int, int, int);
static int regnum(STDOPAND *);
static int chaddrsize(STDOPAND *, int);
static void fltused(void);

/****************************************************************************
 *                                                                          *
 * Function: x86_emitter_init                                               *
 *                                                                          *
 * Purpose : Initialize the emitter module (X86 mode).                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

void x86_emitter_init(void)
{
    as.emitter.cpulevel = cpu_level;
    as.emitter.emitinline = inline_emitter;
    as.emitter.emit = emitter;
}

/****************************************************************************
 *                                                                          *
 * Function: cpu_level                                                      *
 *                                                                          *
 * Purpose : Parse and store requested processor level.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-07  Created                                              *
 *           04-04-18  Added Pentium 4 level (Willamette).                  *
 *                                                                          *
 ****************************************************************************/

static bool_t cpu_level(const char *name)
{
    if (strcmp(name, "386") == 0)  /* 80386 */
        cpulevel = IF_386;
    else if (strcmp(name, "486") == 0)  /* 80486 */
        cpulevel = IF_486;
    else if (_stricmp(name, "pentium") == 0)  /* Pentium */
        cpulevel = IF_PENT;
    else if (_stricmp(name, "p2") == 0 || _stricmp(name, "ppro") == 0)  /* Pentium II */
        cpulevel = IF_P6;
    else if (_stricmp(name, "p3") == 0 || _stricmp(name, "katmai") == 0)  /* Pentium III */
        cpulevel = IF_KATMAI;
    else if (_stricmp(name, "p4") == 0 || _stricmp(name, "willamette") == 0)  /* Pentium 4 */
        cpulevel = IF_WILLAMETTE;
    else
        cpulevel = 0;

    return cpulevel != 0;
}

/****************************************************************************
 *                                                                          *
 * Function: inline_emitter                                                 *
 *                                                                          *
 * Purpose : Emit previously parsed inline code.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-04-18  Maximum processor level is now Willamette.           *
 *                                                                          *
 ****************************************************************************/

static void inline_emitter(const char *name, const char *text, SYMBOL *sym, long cl)
{
    const char *p;

    /* set maximum processor level */
    print("[cpu willamette]\n");

    if (name) print("%s:\n", name);

    for (p = text; p && *p; p++)
    {
        if (*p == '?')  /* placeholder */
        {
            if (sym->sclass == AUTO && sym->scope >= PARAM)
                print("EBP+(%d)", sym->x.offset);
            else
                print("%s", sym->x.name);
        }
        else
        {
            print("%c", *p);
        }
    }

    /* set appropriate processor level (see x86.md) */
    print(options.maxopt ? "[cpu ppro]\n" : "[cpu 486]\n");
}

/****************************************************************************
 *                                                                          *
 * Function: emitter                                                        *
 *                                                                          *
 * Purpose : Emit assembler code (X86 mode).                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created (extracted from asm.c)                       *
 *                                                                          *
 ****************************************************************************/

static long emitter(int pass, long segment, long offset, long oldoffset, int bits, INSN *insn)
{
    long size = -1;

    if (pass == 1)
    {
        if (insn->opcode != I_EQU)  /* see below */
        {
            /*
             * Here we convert backward jumps to the shortest
             * possible form, i.e. to 8-bit displacement if the
             * target is less than -128 bytes away.
             */
            do
                size = insnsize(segment, offset, bits, insn);
            while (!insn->forw_ref && disp32_to_disp8(insn, offset + size));
        }
    }
    else if (pass == 2)
    {
        if (insn->opcode == I_EQU)
        {
            /*
             * Special '..' EQUs get processed in pass three.
             */
            if (!insn->label)
            {
                apperror(RCERROR(ERROR_EQU_SYNTAX_ERROR));
            }
            else if (insn->label[0] != '.' || insn->label[1] != '.')
            {
                if (insn->nopands == 1 && (insn->aops[0].type & A_IMMEDIATE))
                {
                    int external = insn->aops[0].opflags & OPFLAG_EXTERN;
                    asmlab_define(insn->label, insn->aops[0].segment, insn->aops[0].offset, FALSE, external);
                }
                else if (insn->nopands == 2 &&
                    (insn->aops[0].type & A_IMMEDIATE) && (insn->aops[0].type & A_COLON) && insn->aops[0].segment == NO_SEG &&
                    (insn->aops[1].type & A_IMMEDIATE) && insn->aops[1].segment == NO_SEG)
                {
                    asmlab_define(insn->label, insn->aops[0].offset | SEG_ABS, insn->aops[1].offset, FALSE, FALSE);
                }
                else
                {
                    apperror(RCERROR(ERROR_EQU_SYNTAX_ERROR));
                }
            }
        }
        else  /* instruction isn't an EQU */
        {
            /*
             * Here we convert forward jumps to the shortest
             * possible form, i.e. to 8-bit displacement if the
             * target is less than +127 bytes away.
             */
            do
                size = insnsize(segment, offset, bits, insn);
            while (insn->forw_ref && disp32_to_disp8(insn, oldoffset + size));
        }
    }
    else if (pass == 3)
    {
        if (insn->opcode == I_EQU)
        {
            /*
             * Special '..' EQUs get processed here.
             */
            if (insn->label[0] == '.' && insn->label[1] == '.')
            {
                if (insn->nopands == 1 && (insn->aops[0].type & A_IMMEDIATE))
                {
                    asmlab_define(insn->label, insn->aops[0].segment, insn->aops[0].offset, FALSE, FALSE);
                }
                else if (insn->nopands == 2 &&
                    (insn->aops[0].type & A_IMMEDIATE) && (insn->aops[0].type & A_COLON) && insn->aops[0].segment == NO_SEG &&
                    (insn->aops[1].type & A_IMMEDIATE) && insn->aops[1].segment == NO_SEG)
                {
                    asmlab_define(insn->label, insn->aops[0].offset | SEG_ABS, insn->aops[1].offset, FALSE, FALSE);
                }
                else
                {
                    apperror(RCERROR(ERROR_EQU_SYNTAX_ERROR));
                }
            }
        }

        size = assemble(segment, offset, bits, insn);
    }

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: disp32_to_disp8                                                *
 *                                                                          *
 * Purpose : Convert disp32 to disp8, when possible.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define FUDGE  10
static bool_t disp32_to_disp8(INSN *insn, long insn_end)
{
    if (!options.optimize)
        return FALSE;

    /* we totally ignore I_JECXZ... */
    if ((insn->opcode == I_Jcc || insn->opcode == I_JMP) &&
        (A_MEMORY & ~insn->aops[0].type) != 0 &&
        (A_NEAR & ~insn->aops[0].type) == 0)
    {
        long disp = insn->aops[0].offset - insn_end;
        if (disp >= -128 + FUDGE && disp <= 127 - FUDGE)
        {
            char buf[MAXLINE], *p;

            assert(curp);
            p = strstr(strcpy(buf, (char *)curp->data), "near");
            assert(p);
            strncpy(p, "shrt", 4);

            curp->data = string(buf);

            /* change it to recalc the instruction length */
            insn->aops[0].type &= ~A_NEAR;
            insn->aops[0].type |= A_SHORT;

            /* been here, done that */
            return TRUE;
        }
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: assemble                                                       *
 *                                                                          *
 * Purpose : Emit assembly code and data.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long assemble(long segment, long offset, int bits, INSN *insn)
{
    struct ITEMPLATE *template;
    int size_problem;
    long wsize;
    long start = offset;

    wsize = 0;
    switch (insn->opcode)
    {
        case -1: return 0;
        case I_DB: wsize = 1; break;
        case I_DW: wsize = 2; break;
        case I_DD: wsize = 4; break;
        case I_DQ: wsize = 8; break;
        case I_DT: wsize = 10; break;
    }

    if (wsize)
    {
        long times;

        if ((times = insn->times) < 0)
            apperror(RCFATAL(ERROR_INVALID_TIMES_VALUE));

        while (times--)  /* repeat TIMES times */
        {
            EXTOPAND *eop;

            for (eop = insn->eops; eop != NULL; eop = eop->next)
            {
                if (eop->type == EOT_DB_NUMBER)
                {
                    if (wsize == 1)
                    {
                        if (eop->segment != NO_SEG)
                        {
                            apperror(RCERROR(ERROR_ONE_BYTE_RELOCATION));
                        }
                        else
                        {
                            uchar_t out_byte = (uchar_t)eop->offset;
                            out(offset, segment, &out_byte, OUT_RAWDATA+1, NO_SEG);
                        }
                    }
                    else if (wsize == 8)
                    {
                        if (eop->segment != NO_SEG)
                            apperror(RCERROR(ERROR_ONE_BYTE_RELOCATION));
                        else
                            out(offset, segment, &eop->offset, OUT_RAWDATA+8, NO_SEG);
                    }
                    else if (wsize == 10)
                    {
                        apperror(RCERROR(ERROR_INT_CONST_IN_DT_INSN));
                    }
                    else  /* wsize == 2 || wsize == 4 */
                    {
                        LABELDEF *labdef = (LABELDEF *)eop->vp;
                        if (labdef && labdef->symbol)
                        {
                            eop->offset -= labdef->offset;
                            eop->segment = SEG_SYM | labdef->symbol;
                        }

                        out(offset, segment, &eop->offset, OUT_ADDRESS+wsize, eop->segment);
                    }
                    offset += wsize;
                }
                else if (eop->type == EOT_DB_STRING)
                {
                    int align;

                    out(offset, segment, eop->stringval, OUT_RAWDATA+eop->stringlen, NO_SEG);

                    align = eop->stringlen % wsize;
                    if (align)
                    {
                        align = wsize - align;
                        out(offset, segment, "\0\0\0\0\0\0\0\0", OUT_RAWDATA+align, NO_SEG);
                    }
                    offset += eop->stringlen + align;
                }
            }
        }

        return offset-start;
    }

    size_problem = 0;
    template = asm_instructions[insn->opcode];
    while (template->opcode != -1)
    {
        int m = match_insn(template, insn);
        if (m == MATCHES_MATCH)  /* matches! */
        {
            char *codes = template->code;
            long isize = calc_insnsize(bits, insn, codes);
            long itimes;

            if (isize < 0)  /* shouldn't be, on final pass */
                apperror(RCFATAL(ERROR_INTERNAL), "emitter()");

            if (options.fltuse && (template->flags & IF_FPU))
                options.fltuse = FALSE, fltused();

            itimes = insn->times;
            while (itimes--)
            {
                long insn_end;
                int j;

                insn_end = offset + isize;
                for (j = 0; j < insn->nprefix; j++)
                {
                    uchar_t c = 0;

                    switch (insn->prefixes[j])
                    {
                        case P_LOCK:
                            c = 0xF0; break;
                        case P_REPNE: case P_REPNZ:
                            c = 0xF2; break;
                        case P_REPE: case P_REPZ: case P_REP:
                            c = 0xF3; break;
                        case R_CS: c = 0x2E; break;
                        case R_DS: c = 0x3E; break;
                        case R_ES: c = 0x26; break;
                        case R_FS: c = 0x64; break;
                        case R_GS: c = 0x65; break;
                        case R_SS: c = 0x36; break;
                        default:
                            apperror(RCFATAL(ERROR_INTERNAL), "prefix?");
                            break;
                    }

                    if (c != 0)
                    {
                        out(offset, segment, &c, OUT_RAWDATA+1, NO_SEG);
                        offset++;
                    }
                }

                generate_insn(segment, offset, bits, insn, codes, insn_end);
                offset += isize;
            }

            return offset-start;
        }
        else if (m > MATCHES_NO)
        {
            size_problem = m;
        }

        template++;
    }

    if (template->opcode == -1)  /* didn't match any instruction */
    {
        if (size_problem == MATCHES_WHICH_SIZE)
            apperror(RCERROR(ERROR_OPERAND_SIZE_MISSING));
        else if (size_problem == MATCHES_NOT_SIZE)
            apperror(RCERROR(ERROR_OPERAND_SIZE_MISMATCH));
        else if (size_problem == MATCHES_NOT_CPU)
            apperror(RCERROR(ERROR_CPU_TYPE_MISMATCH));
        else
            apperror(RCERROR(ERROR_UNKNOWN_INSN));
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: insnsize                                                       *
 *                                                                          *
 * Purpose : Calculate the size of the given instruction.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long insnsize(long segment, long offset, int bits, INSN *insn)
{
    struct ITEMPLATE *template;

    if (insn->opcode == -1)
        return 0;

    if (insn->opcode == I_DB ||
        insn->opcode == I_DW ||
        insn->opcode == I_DD ||
        insn->opcode == I_DQ ||
        insn->opcode == I_DT)
    {
        long isize, osize, wsize = 0;
        EXTOPAND *eop;

        isize = 0;
        switch (insn->opcode)
        {
            case I_DB: wsize = 1; break;
            case I_DW: wsize = 2; break;
            case I_DD: wsize = 4; break;
            case I_DQ: wsize = 8; break;
            case I_DT: wsize = 10; break;
        }

        for (eop = insn->eops; eop != NULL; eop = eop->next)
        {
            long align;

            if (eop->type == EOT_DB_NUMBER)
                osize = 1;
            else if (eop->type == EOT_DB_STRING)
                osize = eop->stringlen;
            else
                osize = 0;

            align = (-osize) % wsize;
            if (align < 0) align += wsize;

            isize += osize + align;
        }

        return isize * insn->times;
    }

    template = asm_instructions[insn->opcode];
    while (template->opcode != -1)
    {
        if (match_insn(template, insn) == MATCHES_MATCH)
        {
            /* we've matched an instruction */
            char *codes = template->code;
            long isize;
            int j;

            isize = calc_insnsize(bits, insn, codes);
            if (isize < 0) return -1;

            for (j = 0; j < insn->nprefix; j++)
                isize++;

            return isize * insn->times;
        }

        template++;
    }

    return -1;
}

/****************************************************************************
 *                                                                          *
 * Function: match_insn                                                     *
 *                                                                          *
 * Purpose : Compare instruction with given template instruction.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-04-07  Handling of processor level added.                   *
 *                                                                          *
 ****************************************************************************/

static int match_insn(struct ITEMPLATE *template, INSN *insn)
{
    int i, size[3], asize, aops, ret;

    ret = MATCHES_MATCH;

    /*
     * Check the opcode.
     */
    if (template->opcode != insn->opcode)
        return MATCHES_NO;

    /*
     * Count the operands.
     */
    if (template->nopands != insn->nopands)
        return MATCHES_NO;

    /*
     * Check that no spurious colons or TOs are present.
     */
    for (i = 0; i < template->nopands; i++)
        if (insn->aops[i].type & ~template->aops[i] & (A_COLON|A_TO))
            return MATCHES_NO;

    /*
     * Check that the operand flags all match up.
     */
    for (i = 0; i < template->nopands; i++)
    {
        if (template->aops[i] & ~insn->aops[i].type ||
            ((template->aops[i] & A_SIZEMASK) &&
            ((template->aops[i] ^ insn->aops[i].type) & A_SIZEMASK)))
        {
            if ((template->aops[i] & ~insn->aops[i].type & A_TYPEMASK) ||
                (insn->aops[i].type & A_SIZEMASK))
                return MATCHES_NO;
            else
                ret = MATCHES_WHICH_SIZE;
        }
    }

    /*
     * Check operand sizes.
     */
    if (template->flags & IF_ARMASK)
    {
        size[0] = size[1] = size[2] = 0;

        switch (template->flags & IF_ARMASK)
        {
            case IF_AR0: i = 0; break;
            case IF_AR1: i = 1; break;
            case IF_AR2: i = 2; break;
            default: break;
        }

        if (template->flags & IF_SB)
            size[i] = A_BITS8;
        else if (template->flags & IF_SW)
            size[i] = A_BITS16;
        else if (template->flags & IF_SD)
            size[i] = A_BITS32;
    }
    else
    {
        asize = 0;

        if (template->flags & IF_SB)
        {
            asize = A_BITS8;
            aops = template->nopands;
        }
        else if (template->flags & IF_SW)
        {
            asize = A_BITS16;
            aops = template->nopands;
        }
        else if (template->flags & IF_SD)
        {
            asize = A_BITS32;
            aops = template->nopands;
        }

        size[0] = size[1] = size[2] = asize;
    }

    if (template->flags & (IF_SM|IF_SM2))
    {
        aops = (template->flags & IF_SM2) ? 2 : template->nopands;
        asize = 0;

        for (i = 0; i < aops; i++)
        {
            if ((asize = template->aops[i] & A_SIZEMASK) != 0)
            {
                int j;
                for (j = 0; j < aops; j++)
                    size[j] = asize;
                break;
            }
        }
    }
    else
    {
        aops = template->nopands;
    }

    for (i = 0; i < template->nopands; i++)
    {
        if (!(template->aops[i] & A_SIZEMASK) &&
            (insn->aops[i].type & A_SIZEMASK & ~size[i]))
            ret = MATCHES_NOT_SIZE;
    }

    if ((template->flags & IF_PLEVEL) > cpulevel)
        ret = MATCHES_NOT_CPU;

    return ret;
}

/****************************************************************************
 *                                                                          *
 * Function: calc_insnsize                                                  *
 *                                                                          *
 * Purpose : Return the size of the given instruction.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define ADDRSIZE(insn,i,bits)  (((insn)->aops[i].addr_size ? \
    (insn)->aops[i].addr_size : (bits)) == 16 ? 2 : 4)

static long calc_insnsize(int bits, INSN *insn, char *codes)
{
    long length = 0;
    uchar_t c;

    while (*codes) switch (c = *codes++)
    {
        case 01: case 02: case 03:
            codes += c, length += c; break;

        case 04: case 05: case 06: case 07:
            length++; break;

        case 010: case 011: case 012:
            codes++, length++; break;

        case 017:
            length++; break;

        case 014: case 015: case 016:
            length++; break;

        case 020: case 021: case 022:
            length++; break;

        case 024: case 025: case 026:
            length++; break;

        case 030: case 031: case 032:
            length += 2; break;

        case 034: case 035: case 036:
            length += ADDRSIZE(insn, c-034, bits); break;

        case 037:
            length += 2; break;

        case 040: case 041: case 042:
            length += 4; break;

        case 050: case 051: case 052:
            length++; break;

        case 060: case 061: case 062:
            length += 2; break;

        case 064: case 065: case 066:
            length += ADDRSIZE(insn, c-064, bits); break;

        case 070: case 071: case 072:
            length += 4; break;

        case 0130: case 0131: case 0132:
            length += is_sbyte(insn, c-0130, 16) ? 1 : 2; break;

        case 0133: case 0134: case 0135:
            codes += 2; length++; break;

        case 0140: case 0141: case 0142:
            length += is_sbyte(insn, c-0140, 32) ? 1 : 4; break;

        case 0143: case 0144: case 0145:
            codes += 2; length++; break;

        case 0300: case 0301: case 0302:
            length += chaddrsize(&insn->aops[c-0300], bits); break;

        case 0310:
            length += (bits == 32); break;

        case 0311:
            length += (bits == 16); break;

        case 0312:
            break;

        case 0320:
            length += (bits == 32); break;

        case 0321:
            length += (bits == 16); break;

        case 0322:
            break;

        case 0330:
            codes++, length++; break;

        case 0331:
        case 0332:
            break;

        case 0333:
            length++; break;

        case 0340: case 0341: case 0342:
            if (insn->aops[0].segment != NO_SEG)
                apperror(RCERROR(ERROR_NON_CONSTANT_BSS_SIZE));
            else
                length += insn->aops[0].offset << (c-0340);
            break;

        default:
            if (c >= 0100 && c <= 0277)  /* it's an EA */
            {
                EA ea_data;

                if (!process_ea(&insn->aops[(c>>3)&7], &ea_data, bits, 0, insn->forw_ref))
                {
                    apperror(RCERROR(ERROR_INVALID_EA_ADDR));
                    return -1;
                }
                else
                {
                    length += ea_data.size;
                }
            }
            else
            {
                apperror(RCFATAL(ERROR_INTERNAL), "calc_insnsize()");
            }
            break;
    }

    return length;
}

/****************************************************************************
 *                                                                          *
 * Function: is_sbyte                                                       *
 *                                                                          *
 * Purpose : Check if the operands value lies within a signed byte.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int is_sbyte(INSN *insn, int operand, int size)
{
    signed long v;
    int ret;

    /* dead in the water on forward reference or external */
    ret = !(insn->forw_ref && insn->aops[operand].opflags) && insn->aops[operand].segment == NO_SEG;
    v = insn->aops[operand].offset;
    if (size == 16) v = (signed short)v;  /* sign extend if 16 bits */

    return ret && v >= -128L && v <= 127L;
}

/****************************************************************************
 *                                                                          *
 * Function: generate_insn                                                  *
 *                                                                          *
 * Purpose : Generate an instruction.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void generate_insn(long segment, long offset, int bits, INSN *insn, char *codes, long insn_end)
{
    static char condval[] = {
        /* conditional opcodes */
        0x7, 0x3, 0x2, 0x6, 0x2, 0x4, 0xF, 0xD, 0xC, 0xE, 0x6, 0x2,
        0x3, 0x7, 0x3, 0x5, 0xE, 0xC, 0xD, 0xF, 0x1, 0xB, 0x9, 0x5,
        0x0, 0xA, 0xA, 0xB, 0x8, 0x4
    };
    uchar_t c;
    uchar_t bytes[4];
    long data, size, seg;

    while (*codes)
    {
        switch (c = *codes++)
        {
            case 01: case 02: case 03:
                out(offset, segment, codes, OUT_RAWDATA+c, NO_SEG);
                codes += c;
                offset += c;
                break;

            case 04: case 06:
                switch (insn->aops[0].basereg)
                {
                    case R_CS: bytes[0] = 0x0E + (c == 0x04 ? 1 : 0); break;
                    case R_DS: bytes[0] = 0x1E + (c == 0x04 ? 1 : 0); break;
                    case R_ES: bytes[0] = 0x06 + (c == 0x04 ? 1 : 0); break;
                    case R_SS: bytes[0] = 0x16 + (c == 0x04 ? 1 : 0); break;
                    default: apperror(RCFATAL(ERROR_INTERNAL), "8086 segreg?");
                }
                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                offset++;
                break;

            case 05: case 07:
                switch (insn->aops[0].basereg)
                {
                    case R_FS: bytes[0] = 0xA0 + (c == 0x05 ? 1 : 0); break;
                    case R_GS: bytes[0] = 0xA8 + (c == 0x05 ? 1 : 0); break;
                    default: apperror(RCFATAL(ERROR_INTERNAL), "386 segreg?");
                }
                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                offset++;
                break;

            case 010: case 011: case 012:
                bytes[0] = *codes++ + regnum(&insn->aops[c-010]);
                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                offset++;
                break;

            case 017:
                bytes[0] = 0;
                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                offset++;
                break;

            case 014: case 015: case 016:
                getaddr(&insn->aops[c-014], &data, &seg);
                if (data < -128 || data > +127)
                    apperror(RCWARNING1(ERROR_DATA_EXCEEDS_BOUNDS), "signed byte");
                if (seg != NO_SEG)
                {
                    out(offset, segment, &data, OUT_ADDRESS+1, seg);
                }
                else
                {
                    bytes[0] = (uchar_t)data;
                    out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                }
                offset++;
                break;

            case 020: case 021: case 022:
                getaddr(&insn->aops[c-020], &data, &seg);
                if (data < -256 || data > 255)
                    apperror(RCWARNING1(ERROR_DATA_EXCEEDS_BOUNDS), "byte");
                if (seg != NO_SEG)
                {
                    out(offset, segment, &data, OUT_ADDRESS+1, seg);
                }
                else
                {
                    bytes[0] = (uchar_t)data;
                    out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                }
                offset++;
                break;

            case 024: case 025: case 026:
                getaddr(&insn->aops[c-024], &data, &seg);
                if (data < 0 || data > 255)
                    apperror(RCWARNING1(ERROR_DATA_EXCEEDS_BOUNDS), "unsigned byte");
                if (seg != NO_SEG)
                {
                    out(offset, segment, &data, OUT_ADDRESS+1, seg);
                }
                else
                {
                    bytes[0] = (uchar_t)data;
                    out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                }
                offset++;
                break;

            case 030: case 031: case 032:
                getaddr(&insn->aops[c-030], &data, &seg);
                if (seg == NO_SEG && (data < -65536L || data > +65535L))
                    apperror(RCWARNING1(ERROR_DATA_EXCEEDS_BOUNDS), "word");
                out(offset, segment, &data, OUT_ADDRESS+2, seg);
                offset += 2;
                break;

            case 034: case 035: case 036:
                getaddr(&insn->aops[c-034], &data, &seg);
                size = ADDRSIZE(insn, c-034, bits);
                if (size == 2 && (data < -65536L || data > 65535L))
                    apperror(RCWARNING1(ERROR_DATA_EXCEEDS_BOUNDS), "word");
                out(offset, segment, &data, OUT_ADDRESS+size, seg);
                offset += size;
                break;

            case 037:
                if (insn->aops[0].segment == NO_SEG)
                    apperror(RCERROR(ERROR_NON_RELOC_FAR_VALUE));
                data = 0L;
                out(offset, segment, &data, OUT_ADDRESS+2,
                    (*OF->segbase)(1+insn->aops[0].segment));
                offset += 2;
                break;

            case 040: case 041: case 042:
                getaddr(&insn->aops[c-040], &data, &seg);
                out(offset, segment, &data, OUT_ADDRESS+4, seg);
                offset += 4;
                break;

            case 050: case 051: case 052:
                if (insn->aops[c-050].segment != segment)
                    apperror(RCERROR(ERROR_INTER_SEG_REFERENCE));
                data = insn->aops[c-050].offset - insn_end;
                if (data < -128 || data > 127)
                    apperror(RCERROR(ERROR_JUMP_OUT_OF_RANGE), data);
                bytes[0] = (uchar_t)data;
                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                offset++;
                break;

            case 060: case 061: case 062:
                getaddr(&insn->aops[c-060], &data, &seg);
                if (seg != segment)
                {
                    out(offset, segment, &data, OUT_REL2ADR+insn_end-offset, seg);
                }
                else
                {
                    data = data - insn_end;
                    out(offset, segment, &data, OUT_ADDRESS+2, NO_SEG);
                }
                offset += 2;
                break;

            case 064: case 065: case 066:
                getaddr(&insn->aops[c-064], &data, &seg);
                size = ADDRSIZE(insn, c-064, bits);
                if (seg != segment)
                {
                    size = (bits == 16 ? OUT_REL2ADR : OUT_REL4ADR);
                    out(offset, segment, &data, size+insn_end-offset, seg);
                    size = (bits == 16 ? 2 : 4);
                }
                else
                {
                    data = data - insn_end;
                    out(offset, segment, &data, OUT_ADDRESS+size, NO_SEG);
                }
                offset += size;
                break;

            case 070: case 071: case 072:
                getaddr(&insn->aops[c-070], &data, &seg);
                if (seg != segment)
                {
                    out(offset, segment, &data, OUT_REL4ADR+insn_end-offset, seg);
                }
                else
                {
                    data = data - insn_end;
                    out(offset, segment, &data, OUT_ADDRESS+4, NO_SEG);
                }
                offset += 4;
                break;

            case 0130: case 0131: case 0132:
                getaddr(&insn->aops[c-0130], &data, &seg);
                if (is_sbyte(insn, c-0130, 16))
                {
                    out(offset, segment, &data, OUT_RAWDATA+1, NO_SEG);
                    offset++;
                }
                else
                {
                    if (seg == NO_SEG && (data < -65536L || data > 65535L))
                        apperror(RCWARNING1(ERROR_DATA_EXCEEDS_BOUNDS), "word");
                    out(offset, segment, &data, OUT_ADDRESS+2, seg);
                    offset += 2;
                }
                break;

            case 0133: case 0134: case 0135:
                codes++;
                bytes[0] = *codes++;
                if (is_sbyte(insn, c-0133, 16)) bytes[0] |= 2;  /* s-bit */
                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                offset++;
                break;

            case 0140: case 0141: case 0142:
                getaddr(&insn->aops[c-0140], &data, &seg);
                if (is_sbyte(insn, c-0140, 32))
                {
                    out(offset, segment, &data, OUT_RAWDATA+1, NO_SEG);
                    offset++;
                }
                else
                {
                    out(offset, segment, &data, OUT_ADDRESS+4, seg);
                    offset += 4;
                }
                break;

            case 0143: case 0144: case 0145:
                codes++;
                bytes[0] = *codes++;
                if (is_sbyte(insn, c-0143, 32)) bytes[0] |= 2;  /* s-bit */
                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                offset++;
                break;

            case 0300: case 0301: case 0302:
                if (chaddrsize(&insn->aops[c-0300], bits))
                {
                    bytes[0] = 0x67;
                    out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                    offset++;
                }
                break;

            case 0310:
                if (bits == 32)
                {
                    bytes[0] = 0x67;
                    out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                    offset++;
                }
                break;

            case 0311:
                if (bits == 16)
                {
                    bytes[0] = 0x67;
                    out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                    offset++;
                }
                break;

            case 0312:
                break;

            case 0320:
                if (bits == 32)
                {
                    bytes[0] = 0x66;
                    out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                    offset++;
                }
                break;

            case 0321:
                if (bits == 16)
                {
                    bytes[0] = 0x66;
                    out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                    offset++;
                }
                break;

            case 0322:
                break;

            case 0330:
                bytes[0] = *codes++ + condval[insn->condition];
                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                offset++;
                break;

            case 0331:
            case 0332:
                break;

            case 0333:
                bytes[0] = 0xF3;
                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                offset++;
                break;

            case 0340: case 0341: case 0342:
                if (insn->aops[0].segment != NO_SEG)
                    apperror(RCERROR(ERROR_NON_CONSTANT_BSS_SIZE));
                else
                {
                    long size = insn->aops[0].offset << (c-0340);
                    if (size > 0) out(offset, segment, NULL, OUT_RESERVE+size, NO_SEG);
                    offset += size;
                }
                break;

            default:
                if (c >= 0100 && c <= 0277)  /* it's an EA */
                {
                    EA ea_data;
                    int rfield;
                    uchar_t *p;
                    long s;

                    if (c <= 0177)  /* pick rfield from operand b */
                        rfield = regnum(&insn->aops[c&7]);
                    else  /* rfield is constant */
                        rfield = (c&7);

                    if (!process_ea(&insn->aops[(c>>3)&7], &ea_data, bits, rfield, insn->forw_ref))
                        apperror(RCERROR(ERROR_INVALID_EA_ADDR));

                    p = bytes;
                    *p++ = ea_data.modrm;
                    if (ea_data.sib_present)
                        *p++ = ea_data.sib;

                    s = p - bytes;
                    out(offset, segment, bytes, OUT_RAWDATA + s, NO_SEG);

                    switch (ea_data.bytes)
                    {
                        case 0:
                            break;

                        case 1:
                            getaddr(&insn->aops[(c>>3)&7], &data, &seg);
                            if (seg != NO_SEG)
                            {
                                out(offset, segment, &data, OUT_ADDRESS+1, seg);
                            }
                            else
                            {
                                bytes[0] = (uchar_t)data;
                                out(offset, segment, bytes, OUT_RAWDATA+1, NO_SEG);
                            }
                            s++;
                            break;

                        case 2:
                        case 4:
                            getaddr(&insn->aops[(c>>3)&7], &data, &seg);
                            out(offset, segment, &data, OUT_ADDRESS+ea_data.bytes, seg);
                            s += ea_data.bytes;
                            break;
                    }
                    offset += s;
                }
                else
                {
                    apperror(RCFATAL(ERROR_INTERNAL), "generate_insn()");
                }
                break;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: getaddr                                                        *
 *                                                                          *
 * Purpose : Return real or symbol adjusted address.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void getaddr(STDOPAND *aop, long *offset, long *segment)
{
    LABELDEF *labdef = (LABELDEF *)aop->vp;
    if (labdef && labdef->symbol)
    {
        *offset = aop->offset - labdef->offset;
        *segment = SEG_SYM | labdef->symbol;
    }
    else
    {
        *offset = aop->offset;
        *segment = aop->segment;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: out                                                            *
 *                                                                          *
 * Purpose : Wrapper around the real output format's output routine,        *
 *           in case we need to do anything more at the same time.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void out(long offset, long segment, void *data, ulong_t type, long segref)
{
    (*OF->output)(segment, data, type, segref);
}

/****************************************************************************
 *                                                                          *
 * Function: process_ea                                                     *
 *                                                                          *
 * Purpose : Handle the effective address part of a instruction.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static EA *process_ea(STDOPAND *input, EA *output, int addrbits, int rfield, int forw_ref)
{
    if (!(A_REGISTER & ~input->type))  /* it's a single register */
    {
        static int regs[] = {
            R_AL,   R_CL,   R_DL,   R_BL,   R_AH,   R_CH,   R_DH,   R_BH,
            R_AX,   R_CX,   R_DX,   R_BX,   R_SP,   R_BP,   R_SI,   R_DI,
            R_EAX,  R_ECX,  R_EDX,  R_EBX,  R_ESP,  R_EBP,  R_ESI,  R_EDI,
            R_MM0,  R_MM1,  R_MM2,  R_MM3,  R_MM4,  R_MM5,  R_MM6,  R_MM7,
            R_XMM0, R_XMM1, R_XMM2, R_XMM3, R_XMM4, R_XMM5, R_XMM6, R_XMM7
        };
        int i;

        for (i = 0; i < NELEMS(regs); i++)
            if (input->basereg == regs[i]) break;

        if (i == NELEMS(regs))
            return NULL;

        output->sib_present = FALSE;
        output->bytes = 0;
        output->modrm = 0xC0|(rfield << 3)|(i & 7);
    }
    else  /* it's a memory reference */
    {
        if (input->basereg == -1 && (input->indexreg == -1 || input->scale == 0))
        {
            /* it's a pure offset */
            if (input->addr_size)
                addrbits = input->addr_size;

            output->sib_present = FALSE;
            output->bytes = (addrbits == 32) ? 4 : 2;
            output->modrm = ((addrbits == 32) ? 5 : 6)|(rfield << 3);
        }
        else  /* it's an indirection */
        {
            int i = input->indexreg, b = input->basereg, s = input->scale;
            long o = input->offset, seg = input->segment;
            int hb = input->hintbase, ht = input->hinttype;
            int t;

            if (s == 0) i = -1;  /* make this easy, at least */

            if (i == R_EAX || i == R_EBX || i == R_ECX || i == R_EDX ||
                i == R_EBP || i == R_ESP || i == R_ESI || i == R_EDI ||
                b == R_EAX || b == R_EBX || b == R_ECX || b == R_EDX ||
                b == R_EBP || b == R_ESP || b == R_ESI || b == R_EDI)
            {
                /* it must be a 32-bit memory reference. First we have
                 * to check that all registers involved are type Exx.
                 */
                if (i != -1 &&
                    i != R_EAX && i != R_EBX && i != R_ECX && i != R_EDX &&
                    i != R_EBP && i != R_ESP && i != R_ESI && i != R_EDI)
                    return NULL;

                if (b != -1 &&
                    b != R_EAX && b != R_EBX && b != R_ECX && b != R_EDX &&
                    b != R_EBP && b != R_ESP && b != R_ESI && b != R_EDI)
                    return NULL;

                /* while we're here, ensure the user didn't specify WORD. */
                if (input->addr_size == 16)
                    return NULL;

                /* now reorganize base/index */
                if (s == 1 && b != i && b != -1 && i != -1 &&
                    ((hb == b && ht == EAH_NOTBASE) ||
                     (hb == i && ht == EAH_MAKEBASE)))
                    t = b, b = i, i = t;   /* swap if hints say so */

                /* convert EAX+2*EAX to 3*EAX */
                if (b == i)
                    b = -1, s++;

                /* make single reg base, unless hint */
                if (b == -1 && s == 1 && !(hb == i && ht == EAH_NOTBASE))
                    b = i, i = -1;

                /* convert 3*EAX to EAX+2*EAX */
                if (((s == 2 && i != R_ESP) ||
                    s == 3 || s == 5 || s == 9) && b == -1)
                    b = i, s--;

                /* swap ESP into base if scale is 1 */
                if (s == 1 && i == R_ESP)
                    i = b, b = R_ESP;

                /* something wrong, for various reasons */
                if (i == R_ESP || (s != 1 && s != 2 && s != 4 && s != 8 && i != -1))
                    return NULL;

                if (i == -1 && b != R_ESP)  /* no SIB needed */
                {
                    int mod, rm;
                    switch (b)
                    {
                        case R_EAX: rm = 0; break;
                        case R_ECX: rm = 1; break;
                        case R_EDX: rm = 2; break;
                        case R_EBX: rm = 3; break;
                        case R_EBP: rm = 5; break;
                        case R_ESI: rm = 6; break;
                        case R_EDI: rm = 7; break;
                        case -1: rm = 5; break;
                        default:  /* should never happen */
                            return NULL;
                    }
                    if (b == -1 || (b != R_EBP && o == 0 &&
                        seg == NO_SEG && !forw_ref &&
                        !(input->eaflags & (EAF_BYTEOFFS|EAF_WORDOFFS))))
                        mod = 0;
                    else if (input->eaflags & EAF_BYTEOFFS ||
                        (o >= -128 && o <= 127 && seg == NO_SEG && !forw_ref &&
                        !(input->eaflags & EAF_WORDOFFS)))
                        mod = 1;
                    else
                        mod = 2;

                    output->sib_present = FALSE;
                    output->bytes = (b == -1 || mod == 2) ? 4 : mod;
                    output->modrm = (mod<<6)|(rfield<<3)|rm;
                }
                else  /* we need a SIB */
                {
                    int mod, scale, index, base;

                    switch (b)
                    {
                        case R_EAX: base = 0; break;
                        case R_ECX: base = 1; break;
                        case R_EDX: base = 2; break;
                        case R_EBX: base = 3; break;
                        case R_ESP: base = 4; break;
                        case R_EBP: case -1: base = 5; break;
                        case R_ESI: base = 6; break;
                        case R_EDI: base = 7; break;
                        default:  /* then what the smeg is it? */
                            return NULL;
                    }

                    switch (i)
                    {
                        case R_EAX: index = 0; break;
                        case R_ECX: index = 1; break;
                        case R_EDX: index = 2; break;
                        case R_EBX: index = 3; break;
                        case -1: index = 4; break;
                        case R_EBP: index = 5; break;
                        case R_ESI: index = 6; break;
                        case R_EDI: index = 7; break;
                        default:  /* then what the smeg is it? */
                            return NULL;
                    }

                    if (i == -1) s = 1;
                    switch (s)
                    {
                        case 1: scale = 0; break;
                        case 2: scale = 1; break;
                        case 4: scale = 2; break;
                        case 8: scale = 3; break;
                        default:  /* then what the smeg is it? */
                            return NULL;
                    }

                    if (b == -1 || (b != R_EBP && o == 0 &&
                        seg == NO_SEG && !forw_ref &&
                        !(input->eaflags & (EAF_BYTEOFFS|EAF_WORDOFFS))))
                        mod = 0;
                    else if (input->eaflags & EAF_BYTEOFFS ||
                        (o >= -128 && o <= 127 && seg == NO_SEG && !forw_ref &&
                        !(input->eaflags & EAF_WORDOFFS)))
                        mod = 1;
                    else
                        mod = 2;

                    output->sib_present = TRUE;
                    output->bytes = (b == -1 || mod == 2) ? 4 : mod;
                    output->modrm = (mod<<6)|(rfield<<3)|4;
                    output->sib = (scale<<6)|(index<<3)|base;
                }
            }
            else  /* it's 16-bit: yuck! */
            {
                int mod, rm;

                /* check all registers are BX, BP, SI or DI */
                if ((b != -1 && b != R_BP && b != R_BX && b != R_SI && b != R_DI) ||
                    (i != -1 && i != R_BP && i != R_BX && i != R_SI && i != R_DI))
                    return NULL;

                /* ensure the user didn't specify DWORD */
                if (input->addr_size == 32)
                    return NULL;

                /* no can do, in 16-bit EA */
                if (s != 1 && i != -1) return NULL;

                /* swap them round */
                if (b == -1 && i != -1) b ^= i ^= b ^= i;

                /* have BX/BP as base, SI/DI index */
                if ((b == R_SI || b == R_DI) && i != -1)
                    b ^= i ^= b ^= i;

                /* should never happen, in theory */
                if (b == i) return NULL;

                /* check invalid combinations */
                if (i != -1 && b != -1 && (i == R_BP || i == R_BX || b == R_SI || b == R_DI))
                    return NULL;

                /* pure offset: handled above */
                if (b == -1)
                    return NULL;

                rm = -1;
                if (i != -1)
                {
                    switch (i*256 + b)
                    {
                        case R_SI*256+R_BX: rm = 0; break;
                        case R_DI*256+R_BX: rm = 1; break;
                        case R_SI*256+R_BP: rm = 2; break;
                        case R_DI*256+R_BP: rm = 3; break;
                    }
                }
                else
                {
                    switch (b)
                    {
                        case R_SI: rm = 4; break;
                        case R_DI: rm = 5; break;
                        case R_BP: rm = 6; break;
                        case R_BX: rm = 7; break;
                    }
                }

                if (rm == -1)
                    return NULL;

                if (o == 0 && seg == NO_SEG && !forw_ref && rm != 6 &&
                    !(input->eaflags & (EAF_BYTEOFFS|EAF_WORDOFFS)))
                    mod = 0;
                else if (input->eaflags & EAF_BYTEOFFS ||
                    (o >= -128 && o <= 127 && seg == NO_SEG && !forw_ref &&
                    !(input->eaflags & EAF_WORDOFFS)))
                    mod = 1;
                else
                    mod = 2;

                output->sib_present = FALSE;  /* no SIB - it's 16-bit */
                output->bytes = mod;  /* bytes of offset needed */
                output->modrm = (mod<<6)|(rfield<<3)|rm;
            }
        }
    }

    output->size = 1 + output->sib_present + output->bytes;
    return output;
}

/****************************************************************************
 *                                                                          *
 * Function: regnum                                                         *
 *                                                                          *
 * Purpose : Convert to CPU register code.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int regnum(STDOPAND *input)
{
    switch (input->basereg)
    {
        case R_EAX: case R_AX: case R_AL: case R_ES: case R_CR0: case R_DR0: case R_ST0: case R_MM0: case R_XMM0:
            return 0;

        case R_ECX: case R_CX: case R_CL: case R_CS: case R_DR1: case R_ST1: case R_MM1: case R_XMM1:
            return 1;

        case R_EDX: case R_DX: case R_DL: case R_SS: case R_CR2: case R_DR2: case R_ST2: case R_MM2: case R_XMM2:
            return 2;

        case R_EBX: case R_BX: case R_BL: case R_DS: case R_CR3: case R_DR3: case R_TR3: case R_ST3: case R_MM3: case R_XMM3:
            return 3;

        case R_ESP: case R_SP: case R_AH: case R_FS: case R_CR4: case R_TR4: case R_ST4: case R_MM4: case R_XMM4:
            return 4;

        case R_EBP: case R_BP: case R_CH: case R_GS: case R_TR5: case R_ST5: case R_MM5: case R_XMM5:
            return 5;

        case R_ESI: case R_SI: case R_DH: case R_DR6: case R_TR6: case R_ST6: case R_MM6: case R_XMM6:
            return 6;

        case R_EDI: case R_DI: case R_BH: case R_DR7: case R_TR7: case R_ST7: case R_MM7: case R_XMM7:
            return 7;

        default:
            apperror(RCFATAL(ERROR_INTERNAL), "regnum()");
            return 0;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: chaddrsize                                                     *
 *                                                                          *
 * Purpose : Determine if a address size prefix is needed.                  *
 *                                                                          *
 * Comment : We return 0 or 1, not FALSE or TRUE!                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int chaddrsize(STDOPAND *input, int addrbits)
{
    if (!(A_MEMORY & ~input->type))
    {
        int i = input->indexreg;
        int b = input->basereg;

        if (input->scale == 0) i = -1;

        if (i == -1 && b == -1)  /* pure offset */
            return (input->addr_size != 0 && input->addr_size != addrbits);

        if (i == R_EAX || i == R_EBX || i == R_ECX || i == R_EDX ||
            i == R_EBP || i == R_ESP || i == R_ESI || i == R_EDI ||
            b == R_EAX || b == R_EBX || b == R_ECX || b == R_EDX ||
            b == R_EBP || b == R_ESP || b == R_ESI || b == R_EDI)
            return (addrbits == 16);
        else
            return (addrbits == 32);
    }
    else
    {
        return 0;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: fltused                                                        *
 *                                                                          *
 * Purpose : Signal floating point usage (Microsoft).                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define FLTUSED  "__fltused"
static void fltused(void)
{
    if (!asmlab_is_extern(FLTUSED))
    {
        long seg = seg_alloc();

        asmlab_declare_global(FLTUSED);
        asmlab_define(FLTUSED, seg, 0L, FALSE, TRUE);   /* install */
        asmlab_redefine(FLTUSED, seg, 0L);              /* emit */
    }
}
#undef FLTUSED

