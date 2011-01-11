/****************************************************************************
 *                                                                          *
 * File    : armemit.c                                                      *
 *                                                                          *
 * Purpose : ISO C Compiler; ARM Assembler; Code generation.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *           01-04-07  Handling of processor level added.                   *
 *           01-09-21  Inlined code emitter added.                          *
 *                                                                          *
 ****************************************************************************/

/*
 * Third field has a first byte indicating how to
 * put together the bits, and then some codes
 * that may be used at will (see armemit.c)
 *
 * \x1  - 24 bit pc-rel offset           [B, BL]
 * \x2  - 24 bit imm value               [SWI]
 * \x3  -  3 byte code                   [BX]
 *
 * \x4  - reg,reg,reg                    [AND,EOR,SUB,RSB,ADD,ADC,SBC,RSC,ORR,BIC]
 * \x5  - reg,reg,reg,<shift>reg         [-"-]
 * \x6  - reg,reg,reg,<shift>#imm        [-"-]
 * \x7  - reg,reg,#imm                   [-"-]
 *
 * \x8  - reg,reg                        [MOV,MVN]
 * \x9  - reg,reg,<shift>reg             [-"-]
 * \xA  - reg,reg,<shift>#imm            [-"-]
 * \xB  - reg,#imm                       [-"-]
 *
 * \xC  - reg,reg                        [CMP,CMN,TEQ,TST]
 * \xD  - reg,reg,<shift>reg             [-"-]
 * \xE  - reg,reg,<shift>#imm            [-"-]
 * \xF  - reg,#imm                       [-"-]
 */

#include "proj.h"
#pragma hdrstop

#include "lcc.h"
#include "arm.h"

/* all instructions are 4 bytes - RISC isn't it? */
#define INSNSIZE  4

/* instruction templates generated from arm.dat */
#include "arminsa.c"

/* return values from match_insn() */
#define MATCHES_MATCH  100
#define MATCHES_NO  0
/* add MATCHES_NOT_CPU later!!! */

static ulong_t cpulevel = IF_PMASK;   /* highest level by default */

/* Static function prototypes */
static bool_t cpu_level(const char *);
static void inline_emitter(const char *, const char *, SYMBOL *, long);
static long emitter(int, long, long, long, int, INSN *);
static long assemble(long, long, int, INSN *);
static long insnsize(long, long, int, INSN *);
static int match_insn(struct ITEMPLATE *, INSN *);
static void generate_insn(long, long, int, INSN *, char *, long);
static void getaddr(STDOPAND *, long *, long *);
static void out(long, long, void *, ulong_t, long);
static void outswap(long, long, uchar_t *, ulong_t, long);
static int regnum(STDOPAND *);
static int shiftval(STDOPAND *);
static int imm_shift(int);

/****************************************************************************
 *                                                                          *
 * Function: arm_emitter_init                                               *
 *                                                                          *
 * Purpose : Initialize the emitter module (ARM mode).                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

void arm_emitter_init(void)
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
 *                                                                          *
 ****************************************************************************/

static bool_t cpu_level(const char *name)
{
    if (_stricmp(name, "arm7") == 0)  /* ARM 7 */
        cpulevel = IF_ARM7;
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
 *           01-09-21  Created                                              *
 *           02-01-21  Always use variadic stack frame for arguments.       *
 *                                                                          *
 ****************************************************************************/

static void inline_emitter(const char *name, const char *text, SYMBOL *sym, long cl)
{
    const char *p;

    if (name) print("%s:\n", name);

    for (p = text; p && *p; p++)
    {
        if (*p == '?')  /* placeholder */
        {
            if (sym->sclass == AUTO && sym->scope >= PARAM)
            {
                print("fp,#%d", sym->x.offset);
            }
            else
            {
                print("%s", sym->x.name);
            }
        }
        else
        {
            print("%c", *p);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: emitter                                                        *
 *                                                                          *
 * Purpose : Emit assembler code (ARM mode).                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long emitter(int pass, long segment, long offset, long oldoffset, int bits, INSN *insn)
{
    long size = -1;

    if (pass == 1)
    {
        if (insn->opcode != I_EQU)  /* see below */
            size = insnsize(segment, offset, bits, insn);
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
            size = insnsize(segment, offset, bits, insn);
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
 * Function: assemble                                                       *
 *                                                                          *
 * Purpose : Emit assembly code and data.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long assemble(long segment, long offset, int bits, INSN *insn)
{
    struct ITEMPLATE *template;
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
                    else if (wsize > 4)
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

    template = asm_instructions[insn->opcode];
    while (template->opcode != -1)
    {
        if (match_insn(template, insn) == MATCHES_MATCH)
        {
            /* we've matched an instruction */
            char *codes = template->code;
            long itimes;

            itimes = insn->times;
            while (itimes--)
            {
                long insn_end = offset + INSNSIZE;
                generate_insn(segment, offset, bits, insn, codes, insn_end);
                if (insn->opcode == I_RESB)
                    offset += insn->aops[0].offset;
                else
                    offset += INSNSIZE;
            }

            return offset - start;
        }

        template++;
    }

    if (template->opcode == -1)  /* didn't match any instruction */
        apperror(RCERROR(ERROR_UNKNOWN_INSN));

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: insnsize                                                       *
 *                                                                          *
 * Purpose : Calculate the size of the given instruction.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
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
            if (insn->opcode == I_RESB)
                return insn->aops[0].offset * insn->times;
            else
                return INSNSIZE * insn->times;
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
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int match_insn(struct ITEMPLATE *template, INSN *insn)
{
    int i;

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
     * Check that the operand flags all match up.
     */
    for (i = 0; i < template->nopands; i++)
    {
        if (template->aops[i] & ~insn->aops[i].type)
            return MATCHES_NO;
    }

    return MATCHES_MATCH;
}

/****************************************************************************
 *                                                                          *
 * Function: generate_insn                                                  *
 *                                                                          *
 * Purpose : Generate an instruction.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *           02-01-21  Better check for invalid 'S','T','!' suffixes.       *
 *                                                                          *
 ****************************************************************************/

static void generate_insn(long segment, long offset, int bits, INSN *insn, char *codes, long insn_end)
{
    static char condval[] =
    {
        /* conditional opcodes */
        0x0E, 0x03, 0x02, 0x00,
        0x0A, 0x0C, 0x08, 0x0D,
        0x09, 0x0B, 0x04, 0x01,
        0x05, 0x07, 0x06
    };
    bool_t has_S_code;  /* S - setflag */
    bool_t has_B_code;  /* B - setflag */
    bool_t has_T_code;  /* T - setflag */
    bool_t has_W_code;  /* ! => W flag */
    bool_t has_F_code;  /* ^ => S flag */
    uchar_t bytes[4], c;
    long data, seg;
    int keep;

    has_S_code = (insn->condition & C_SSETFLAG);
    has_B_code = (insn->condition & C_BSETFLAG);
    has_T_code = (insn->condition & C_TSETFLAG);
    has_W_code = (insn->condition & C_WSETFLAG);
    has_F_code = (insn->condition & C_FSETFLAG);
    insn->condition = (insn->condition & 0x0F);

    /* First a condition code in upper nibble */
    if (insn->condition < C_NONE)
        c = condval[insn->condition] << 4;
    else
        c = condval[C_AL] << 4;  /* is often ALWAYS but not always */

    switch (keep = *codes++)
    {
        case 0x01: /* B, BL */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            getaddr(&insn->aops[0], &data, &seg);
            if (seg != segment)
            {
                bytes[1] = bytes[2] = bytes[3] = 0;  /* 24-bit relocatable value */
                outswap(offset, segment, bytes, OUT_REL3ADR+4, seg);
            }
            else
            {
                data = insn->aops[0].offset - (offset + 8);
                if (data % 4) apperror(RCERROR(ERROR_UNALIGNED_OFFSET), 4);
                if (data >= 0x1000000) apperror(RCERROR(ERROR_OFFSET_TOO_BIG), 0x1000000);
                data >>= 2;
                bytes[1] = (data >> 16) & 0xFF;
                bytes[2] = (data >> 8) & 0xFF;
                bytes[3] = (data) & 0xFF;
                outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            }
            break;

        case 0x02: /* SWI */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            data = insn->aops[0].offset;
            bytes[1] = (data >> 16) & 0xFF;
            bytes[2] = (data >> 8) & 0xFF;
            bytes[3] = (data) & 0xFF;
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x03: /* BX */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes++;
            bytes[2] = *codes++;
            bytes[3] = *codes++;
            c = regnum(&insn->aops[0]);
            if (c >= 15) apperror(RCERROR(ERROR_ILLEGAL_REGISTER));
            bytes[3] |= (c & 0x0F);
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x04: /* AND Rd,Rn,Rm */
        case 0x05: /* AND Rd,Rn,Rm,<shift>Rs */
        case 0x06: /* AND Rd,Rn,Rm,<shift>imm */
        case 0x07: /* AND Rd,Rn,<shift>imm */
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes;
            if (has_S_code) bytes[1] |= 0x10;
            bytes[1] |= regnum(&insn->aops[1]);  /* Rn in low nibble */
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd in high nibble */
            if (keep == 0x07)  /* reg,reg,imm */
            {
                int shimm;

                if ((shimm = imm_shift(insn->aops[2].offset)) == -1)
                    apperror(RCERROR(ERROR_NON_CREATABLE_CONST), insn->aops[2].offset);

                bytes[3] = shimm & 0xFF;
                bytes[2] |= (shimm & 0xF00) >> 8;
            }
            else
            {
                bytes[3] = regnum(&insn->aops[2]);  /* Rm in low nibble */

                if (keep == 0x05)  /* shift in bytes 2 and 3 */
                {
                    bytes[2] |= regnum(&insn->aops[3]);  /* Rs */
                    bytes[3] |= shiftval(&insn->aops[3]) << 5;  /* <shift> */
                    bytes[3] |= 0x10;
                }
                else if (keep == 0x06)
                {
                    c = (insn->aops[3].offset) & 0x1F;
                    if (c & 0x01) bytes[3] |= 0x80;
                    bytes[2] |= c >> 1;  /* #imm */
                    bytes[3] |= shiftval(&insn->aops[3]) << 5;  /* <shift> */
                }
            }
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x08: /* MOV Rd,Rm */
        case 0x09: /* MOV Rd,Rm,<shift>Rs */
        case 0x0A: /* MOV Rd,Rm,<shift>imm */
        case 0x0B: /* MOV Rd,<shift>imm */
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes;
            if (has_S_code) bytes[1] |= 0x10;
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd in high nibble */
            if (keep == 0x0B)  /* reg,imm */
            {
                int shimm;

                if ((shimm = imm_shift(insn->aops[1].offset)) == -1)
                    apperror(RCERROR(ERROR_NON_CREATABLE_CONST), insn->aops[1].offset);

                bytes[3] = shimm & 0xFF;
                bytes[2] |= (shimm & 0xF00) >> 8;
            }
            else
            {
                bytes[3] = regnum(&insn->aops[1]);  /* Rm in low nibble */

                if (keep == 0x09)  /* shift in bytes 2 and 3 */
                {
                    bytes[2] |= regnum(&insn->aops[2]);  /* Rs */
                    bytes[3] |= shiftval(&insn->aops[2]) << 5;  /* <shift> */
                    bytes[3] |= 0x10;
                }
                else if (keep == 0x0A)
                {
                    c = (insn->aops[2].offset) & 0x1F;
                    if (c & 0x01) bytes[3] |= 0x80;
                    bytes[2] |= c >> 1;  /* #imm */
                    bytes[3] |= shiftval(&insn->aops[2]) << 5;  /* <shift> */
                }
            }
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x0C: /* CMP Rn,Rm */
        case 0x0D: /* CMP Rn,Rm,<shift>Rs */
        case 0x0E: /* CMP Rn,Rm,<shift>imm */
        case 0x0F: /* CMP Rn,<shift>imm */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes;
            bytes[1] |= 0x10;  /* implicit S code */
            bytes[1] |= regnum(&insn->aops[0]);  /* Rn in low nibble */
            bytes[2] = 0;  /* No destination (Rd) */
            if (keep == 0x0F)  /* reg,imm */
            {
                int shimm;

                if ((shimm = imm_shift(insn->aops[1].offset)) == -1)
                    apperror(RCERROR(ERROR_NON_CREATABLE_CONST), insn->aops[1].offset);

                bytes[3] = shimm & 0xFF;
                bytes[2] |= (shimm & 0xF00) >> 8;
            }
            else
            {
                bytes[3] = regnum(&insn->aops[1]);  /* Rm in low nibble */

                if (keep == 0x0D)  /* shift in bytes 2 and 3 */
                {
                    bytes[2] |= regnum(&insn->aops[2]);  /* Rs */
                    bytes[3] |= shiftval(&insn->aops[2]) << 5;  /* <shift> */
                    bytes[3] |= 0x10;
                }
                else if (keep == 0x0E)
                {
                    c = (insn->aops[2].offset) & 0x1F;
                    if (c & 0x01) bytes[3] |= 0x80;
                    bytes[2] |= c >> 1;  /* #imm */
                    bytes[3] |= shiftval(&insn->aops[2]) << 5;  /* <shift> */
                }
            }
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x10: /* MRS Rd,<psr> */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes++;
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd */
            bytes[3] = 0;
            c = insn->aops[1].basereg;
            if (c == R_SPSR)
                bytes[1] |= 0x40;
            else if (c != R_CPSR)
                apperror(RCERROR(ERROR_EXPECTING_CPSR_OR_SPSR));
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x11: /* MSR <psr>,Rm */
        case 0x12: /* MSR <psrf>,Rm */
        case 0x13: /* MSR <psrf>,#expression */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes++;
            bytes[2] = *codes;
            if (keep == 0x13)  /* #imm */
            {
                int shimm;

                if ((shimm = imm_shift(insn->aops[1].offset)) == -1)
                    apperror(RCERROR(ERROR_NON_CREATABLE_CONST), insn->aops[1].offset);

                bytes[3] = shimm & 0xFF;
                bytes[2] |= (shimm & 0xF00) >> 8;
            }
            else
            {
                bytes[3] = regnum(&insn->aops[1]);  /* Rm */
            }
            c = insn->aops[0].basereg;
            if (keep == 0x11)
            {
                if (c == R_SPSR)
                    bytes[1] |= 0x40;
                else if (c != R_CPSR)
                    apperror(RCERROR(ERROR_EXPECTING_CPSR_OR_SPSR));
            }
            else
            {
                if (c == R_SPSR_FLG)
                    bytes[1] |= 0x40;
                else if (c != R_CPSR_FLG)
                    apperror(RCERROR(ERROR_EXPECTING_CPSR_OR_SPSR));
            }
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x14: /* MUL  Rd,Rm,Rs */
        case 0x15: /* MULA Rd,Rm,Rs,Rn */
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes++;
            bytes[1] |= regnum(&insn->aops[0]);  /* Rd */
            if (has_S_code) bytes[1] |= 0x10;
            bytes[2] = regnum(&insn->aops[2]);   /* Rs */
            if (keep == 0x15) bytes[2] |= regnum(&insn->aops[3]) << 4;  /* Rn */
            bytes[3] = *codes;
            bytes[3] |= regnum(&insn->aops[1]);  /* Rm */
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x16: /* SMLAL RdHi,RdLo,Rm,Rs */
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes++;
            bytes[1] |= regnum(&insn->aops[1]);  /* RdHi */
            if (has_S_code) bytes[1] |= 0x10;
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* RdLo */
            bytes[2] |= regnum(&insn->aops[3]);  /* Rs */
            bytes[3] = *codes;
            bytes[3] |= regnum(&insn->aops[2]);  /* Rm */
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x17: /* LDR Rd,expression */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes++;
            if (has_B_code) bytes[1] |= 0x40;
            bytes[1] |= 0xF;  /* Rn - implicit R15 */
            /**/
            if (insn->aops[1].segment != segment)
                apperror(RCERROR(ERROR_INTER_SEG_REFERENCE));
            /**/
            data = insn->aops[1].offset - (offset + 8);
            if (data < 0) data = -data; else bytes[1] |= 0x80;  /* add or subtract */
            if (data >= 0x1000) apperror(RCERROR(ERROR_OFFSET_TOO_BIG), 0x1000);
            /**/
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd */
            bytes[2] |= ((data & 0xF00) >> 8);
            bytes[3] = data & 0xFF;
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x18: /*  LDR Rd,[Rn] */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes++;
            if (has_B_code) bytes[1] |= 0x40;
            if (has_T_code) bytes[1] |= 0x20; /* write-back */ else bytes[0] |= 0x01; /* implicit pre-index mode */
            if (has_W_code) bytes[1] |= 0x20; /* write-back */
            /**/
            c = regnum(&insn->aops[1]);  /* Rn */
            bytes[1] |= c;
            data = (c == 15) ? -8 : 0;
            if (data < 0) data = -data; else bytes[1] |= 0x80;
            /**/
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd */
            bytes[2] |= ((data & 0xF00) >> 8);
            bytes[3] = data & 0xFF;
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x19: /* LDR Rd,[Rn,#expression] */
        case 0x20: /* LDR Rd,[Rn,Rm] */
        case 0x21: /* LDR Rd,[Rn,Rm,shift] */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = *codes++;
            bytes[1] |= regnum(&insn->aops[1]);  /* Rn */
            if (has_B_code) bytes[1] |= 0x40;
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd */
            /**/
            if (insn->aops[insn->nopands-1].bracket)
            {
                bytes[0] |= 0x01;  /* pre-index mode */
                if (has_W_code) bytes[1] |= 0x20;
                if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            }
            else
            {
                if (has_T_code) bytes[1] |= 0x20;  /* forced write-back in post-index mode */
                if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            }
            if (keep == 0x19)
            {
                data = insn->aops[2].offset;
                if (data < 0) data = -data; else bytes[1] |= 0x80;
                if (data >= 0x1000) apperror(RCERROR(ERROR_OFFSET_TOO_BIG), 0x1000);

                bytes[2] |= ((data & 0xF00) >> 8);
                bytes[3] = data & 0xFF;
            }
            else
            {
                if (!insn->aops[2].minus) bytes[1] |= 0x80;
                bytes[3] = regnum(&insn->aops[2]);

                if (keep == 0x21)
                {
                    c = (uchar_t)insn->aops[3].offset;
                    if (c > 0x1F)
                    {
                        apperror(RCERROR(ERROR_SHIFT_TOO_BIG), 0x1F);
                        c = c & 0x1F;
                    }

                    bytes[2] |= c >> 1;
                    if (c & 0x01) bytes[3] |= 0x80;
                    bytes[3] |= shiftval(&insn->aops[3]) << 5;
                }
            }
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x22:  /* LDRH Rd,expression */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|0x01;  /* implicit pre-index */
            bytes[1] = *codes++;
            bytes[1] |= 0xF;  /* Rn - implicit R15 */
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd */
            bytes[3] = *codes++;
            /**/
            if (insn->aops[1].segment != segment)
                apperror(RCERROR(ERROR_INTER_SEG_REFERENCE));
            /**/
            data = insn->aops[1].offset - (offset + 8);
            if (data < 0) data = -data; else bytes[1] |= 0x80;  /* add or subtract */
            if (data >= 0x100) apperror(RCERROR(ERROR_OFFSET_TOO_BIG), 0x100);
            /**/
            bytes[2] |= ((data & 0xF0) >> 4);
            bytes[3] |= data & 0xF;
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x23:  /* LDRH Rd,Rn */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|0x01;  /* implicit pre-index */
            bytes[1] = *codes++;
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd */
            bytes[3] = *codes++;
            /**/
            c = regnum(&insn->aops[1]);  /* Rn */
            bytes[1] |= c;
            data = (c == 15) ? -8 : 0;
            if (data < 0) data = -data; else bytes[1] |= 0x80;
            if (data >= 0x100) apperror(RCERROR(ERROR_OFFSET_TOO_BIG), 0x100);
            /**/
            bytes[2] |= ((data & 0xF0) >> 4);
            bytes[3] |= data & 0xF;
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x24:  /* LDRH Rd,Rn,expression */
        case 0x25:  /* LDRH Rd,Rn,Rm */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c;
            bytes[1] = *codes++;
            bytes[1] |= regnum(&insn->aops[1]);  /* Rn */
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd */
            bytes[3] = *codes++;
            /**/
            if (insn->aops[insn->nopands-1].bracket)  /* FIXME: Bracket on last operand -> pre-index */
            {
                bytes[0] |= 0x01;  /* pre-index mode */
                if (has_W_code) bytes[1] |= 0x20;
            }
            else
            {
                if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            }
            if (keep == 0x24)
            {
                data = insn->aops[2].offset;
                if (data < 0) data = -data; else bytes[1] |= 0x80;
                if (data >= 0x100) apperror(RCERROR(ERROR_OFFSET_TOO_BIG), 0x100);

                bytes[2] |= ((data & 0xF0) >> 4);
                bytes[3] |= data & 0xF;
            }
            else  /* keep == 0x25 */
            {
                if (insn->aops[2].minus == 0) bytes[1] |= 0x80;
                bytes[3] |= regnum(&insn->aops[2]);
            }
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x26:  /* LDM/STM Rn,{reg-list} */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c | ((*codes >> 4) & 0x0F);
            bytes[1] = ((*codes++ << 4) & 0xF0);
            bytes[1] |= regnum(&insn->aops[0]);  /* Rn */
            if (has_W_code) bytes[1] |= 0x20;
            if (has_F_code) bytes[1] |= 0x40;
            data = insn->aops[1].basereg;
            bytes[2] = ((data >> 8) & 0xFF);
            bytes[3] = (data & 0xFF);
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x27:  /* SWP Rd,Rm,[Rn] */
            if (has_S_code) apperror(RCERROR(ERROR_S_BIT_NOT_ALLOWED));
            if (has_T_code) apperror(RCERROR(ERROR_T_BIT_NOT_ALLOWED));
            if (has_W_code) apperror(RCERROR(ERROR_W_BIT_NOT_ALLOWED));
            /**/
            bytes[0] = c|*codes++;
            bytes[1] = regnum(&insn->aops[2]);  /* Rn */
            if (has_B_code) bytes[1] |= 0x40;
            bytes[2] = regnum(&insn->aops[0]) << 4;  /* Rd */
            bytes[3] = *codes++;
            bytes[3] |= regnum(&insn->aops[1]);  /* Rm */
            outswap(offset, segment, bytes, OUT_RAWDATA+4, NO_SEG);
            break;

        case 0x7F:  /* RESB imm */
            if (insn->aops[0].segment != NO_SEG)
                apperror(RCERROR(ERROR_NON_CONSTANT_BSS_SIZE));
            else if ((data = insn->aops[0].offset) > 0)
                out(offset, segment, NULL, OUT_RESERVE+data, NO_SEG);
            break;

        default:
            apperror(RCFATAL(ERROR_INTERNAL), "generate_insn()");
            break;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: getaddr                                                        *
 *                                                                          *
 * Purpose : Return real or symbol adjusted address.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
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
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void out(long offset, long segment, void *data, ulong_t type, long segref)
{
    (*OF->output)(segment, data, type, segref);
}

/****************************************************************************
 *                                                                          *
 * Function: outswap                                                        *
 *                                                                          *
 * Purpose : same as out(), but with swapped bytes.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void outswap(long offset, long segment, uchar_t *data, ulong_t type, long segref)
{
    uchar_t bytes[4];

    assert((type & OUT_SIZMASK) == 4);

    bytes[0] = data[3];
    bytes[1] = data[2];
    bytes[2] = data[1];
    bytes[3] = data[0];

    (*OF->output)(segment, bytes, type, segref);
}

/****************************************************************************
 *                                                                          *
 * Function: regnum                                                         *
 *                                                                          *
 * Purpose : Convert to CPU register code.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int regnum(STDOPAND *input)
{
    switch (input->basereg)
    {
        case R_A1: case R_R0: return 0;
        case R_A2: case R_R1: return 1;
        case R_A3: case R_R2: return 2;
        case R_A4: case R_R3: return 3;
        case R_V1: case R_R4: return 4;
        case R_V2: case R_R5: return 5;
        case R_V3: case R_R6: return 6;
        case R_V4: case R_R7: return 7;
        case R_V5: case R_R8: return 8;
        case R_V6: case R_R9: return 9;
        case R_V7: case R_R10: case R_SL: return 10;
        case R_V8: case R_R11: case R_FP: return 11;
        case R_IP: case R_R12: return 12;
        case R_SP: case R_R13: return 13;
        case R_LR: case R_R14: return 14;
        case R_PC: case R_R15: return 15;
        case R_CPSR: case R_CPSR_FLG:
            apperror(RCERROR(ERROR_ILLEGAL_REGISTER));
            return 16;
        case R_SPSR: case R_SPSR_FLG:
            apperror(RCERROR(ERROR_ILLEGAL_REGISTER));
            return 17;
        default:  /* panic */
            apperror(RCFATAL(ERROR_INTERNAL), "regnum()");
            return 0;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: shiftval                                                       *
 *                                                                          *
 * Purpose : Convert to CPU shift value.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int shiftval(STDOPAND *input)
{
    switch (input->shiftflag)
    {
        case S_LSL:
        case S_ASL:
            return 0;
        case S_LSR:
            return 1;
        case S_ASR:
            return 2;
        case S_ROR:
            return 3;
        case S_RRX:
        default:
            return 23456;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: imm_shift                                                      *
 *                                                                          *
 * Purpose : Convert to CPU (possibly shifted) immediate value.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int imm_shift(int value)
{
    uint_t val = value;
    int shift = 16;
    int ret_val = -1;

    if (val <= 0xFF)
    {
        /*
         * to be compatible with MS's armasm (or is it Codemist's ?) -
         * makes it easier to test bit-patterns.
         */
        return val;
    }

    /* chop of the 2 LSBits as long as they are zero */
    while ((val & 0x03) == 0)
    {
        val >>= 2;
        shift--;
    }

    if (shift == 16)
        shift = 0;

    if (val <= 0xFF)
    {
        ret_val = shift << 8;
        ret_val |= val;
    }

    return ret_val;
}

