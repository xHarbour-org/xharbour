/****************************************************************************
 *                                                                          *
 * File    : x86parse.c                                                     *
 *                                                                          *
 * Purpose : ISO C Compiler; X86 Assembler; Source code parser.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"
#include "x86.h"

#define scan  as.scanner.scan

/* maximum number of operands */
#define MAXOPAND  3

/* sizes and special flags */
static long reg_flags[] =
{
    0, A_REG8, A_REG_AL, A_REG_AX, A_REG8, A_REG8, A_REG16, A_REG16, A_REG8, A_REG_CL,
    A_REG_CREG, A_REG_CREG, A_REG_CREG, A_REG_CR4, A_REG_CS, A_REG_CX, A_REG8,
    A_REG16, A_REG8, A_REG_DREG, A_REG_DREG, A_REG_DREG, A_REG_DREG, A_REG_DREG,
    A_REG_DREG, A_REG_DESS, A_REG_DX, A_REG_EAX, A_REG32, A_REG32, A_REG_ECX,
    A_REG32, A_REG32, A_REG_DESS, A_REG32, A_REG32, A_REG_FSGS, A_REG_FSGS,
    A_MMXREG, A_MMXREG, A_MMXREG, A_MMXREG, A_MMXREG, A_MMXREG, A_MMXREG, A_MMXREG,
    A_REG16, A_REG16, A_REG_DESS, A_FPU0, A_FPUREG, A_FPUREG, A_FPUREG, A_FPUREG,
    A_FPUREG, A_FPUREG, A_FPUREG, A_REG_TREG, A_REG_TREG, A_REG_TREG, A_REG_TREG,
    A_REG_TREG,
    A_XMMREG, A_XMMREG, A_XMMREG, A_XMMREG, A_XMMREG, A_XMMREG, A_XMMREG, A_XMMREG
};

/* Static function prototypes */
static void inline_parser(const char *, char **, char **, SYMBOL **, long *);
static void parser(int, const char *, INSN *);
static void cleanup(INSN *);
static bool_t is_comma_next(void);

/****************************************************************************
 *                                                                          *
 * Function: x86_parser_init                                                *
 *                                                                          *
 * Purpose : Initialize the parser module (X86 mode).                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

void x86_parser_init(void)
{
    as.parser.parseinline = inline_parser;
    as.parser.parse = parser;
    as.parser.cleanup = cleanup;
}

/****************************************************************************
 *                                                                          *
 * Function: inline_parser                                                  *
 *                                                                          *
 * Purpose : Parse inline code and return info to the front-end.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-04-13  Accept the ALIGN directive.                          *
 *           04-08-11  Accept the _emit instruction in Microsoft mode.      *
 *                                                                          *
 ****************************************************************************/

static void inline_parser(const char *s, char **name, char **text, SYMBOL **symp, long *cl)
{
    INSN insn;
    int n;

    /* Microsoft accepts the ALIGN and EVEN directives */
    for (; isspace(*s); s++)
        ;
    if (_strnicmp(s, "align ", 6) == 0 && (n = atoi(s + 6)) > 0)
    {
        *name = NULL;
        *text = stringf("[align %d]\n", n);
        *symp = NULL;
        *cl = 0;
        return;
    }
    /* Microsoft accepts the _emit pseudo instruction */
    else if (options.microsoft && _strnicmp(s, "_emit ", 6) == 0)
    {
        *name = NULL;
        *text = stringf("db %s\n", s + 6);
        *symp = NULL;
        *cl = 0;
        return;
    }

    parser(0, s, &insn);

    *name = (insn.label) ? string(insn.label) : NULL;
    *text = (insn.opcode != -1) ? as.scanner.asmtext(&insn, symp, cl) : NULL;

    cleanup(&insn);
}

/****************************************************************************
 *                                                                          *
 * Function: parser                                                         *
 *                                                                          *
 * Purpose : Parse assembler code (X86 mode).                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-21  Better handling of short opcodes, if signed byte.    *
 *           04-04-21  Set OPFLAG_OFFSET for S_OFFSET case.                 *
 *           04-06-10  Changed 'Label without colon' into error.            *
 *           04-08-04  Added support for inline DB, DW, DD and DQ.          *
 *                                                                          *
 ****************************************************************************/

static void parser(int pass, const char *buffer, INSN *insn)
{
    static int token;
    static TOKENVAL tokval;
    int operand;
    int critical;
    EVAL_HINTS hints;
    bool_t inlined = (pass == 0);  /* C inline assembler mode */

    insn->forw_ref = FALSE;

    as.scanner.reset();
    as.scanner.setptr(buffer);
    token = scan(&tokval);

    insn->label = NULL;
    insn->eops = NULL;
    insn->nopands = 0;

    if (token == TOK_EOI)
    {
        /* blank line - ignore it */
        insn->opcode = -1;
        return;
    }

    if (token != TOK_ID && token != TOK_INSN && token != TOK_PREFIX &&
       (token != TOK_REG || (A_REG_SREG & ~reg_flags[tokval.integer])))
    {
        apperror(RCERROR(ERROR_EXPECTING_LABEL_OR_INSN));
        insn->opcode = -1;
        return;
    }

    if (token == TOK_ID)  /* there's a label here */
    {
        insn->label = tokval.charptr;

        token = scan(&tokval);
        if (token == ':')
            token = scan(&tokval);
        else
            apperror(RCERROR(ERROR_LABEL_WITHOUT_COLON), insn->label);

        if (token != TOK_INSN || tokval.integer != I_EQU)
        {
            switch (pass)
            {
                case 0: break;
                case 1: asmlab_define(insn->label, location.segment, location.offset, TRUE, FALSE); break;
                case 2: asmlab_redefine(insn->label, location.segment, location.offset); break;
                case 3: asmlab_phase_check(insn->label, location.segment, location.offset); break;
                default: assert (0);
            }
        }
    }

    if (token == TOK_EOI)
    {
        /* this line contains just a label... */
        insn->opcode = -1;
        return;
    }

    insn->nprefix = 0;
    insn->times = 1L;

    while (token == TOK_PREFIX || (token == TOK_REG && !(A_REG_SREG & ~reg_flags[tokval.integer])))
    {
        /*
         * Handle a special case: the TIMES prefix.
         */
        if (token == TOK_PREFIX && tokval.integer == P_TIMES)
        {
            EXPR *e;

            token = scan(&tokval);

            e = asmexp(&tokval, NULL, (pass < 3) ? 1 : 2, NULL, inlined);
            token = tokval.type;
            if (!e)
            {
                insn->opcode = -1;
                return;
            }

            if (!is_simple(e))
            {
                apperror(RCERROR(ERROR_INVALID_TIMES_VALUE));
                insn->times = 1L;
            }
            else
            {
                insn->times = (long)e->value;
                if (insn->times < 0)
                {
                    apperror(RCERROR(ERROR_INVALID_TIMES_VALUE));
                    insn->times = 0;
                }
            }
        }
        else
        {
            if (insn->nprefix == MAXPREFIX)
                apperror(RCERROR(ERROR_TOO_MANY_INSN_PREFIX), MAXPREFIX);
            else
                insn->prefixes[insn->nprefix++] = (int)tokval.integer;

            token = scan(&tokval);
        }
    }

    if (token != TOK_INSN)
    {
        if (insn->nprefix > 0 && token == TOK_EOI)
        {
            /*
             * Instruction prefixes are present, but no actual instruction.
             * This is allowed: at this point we invent a notional instruction
             * of RESB 0.
             */
            insn->opcode = I_RESB;
            insn->nopands = 1;
            insn->aops[0].type = A_IMMEDIATE;
            insn->aops[0].offset = 0L;
            insn->aops[0].segment = NO_SEG;
            return;
        }
        else
        {
            apperror(RCERROR(ERROR_EXPECTING_INSN));
            insn->opcode = -1;
            return;
        }
    }

    insn->opcode = (int)tokval.integer;
    insn->condition = tokval.inttwo;

    /*
     * We cannot accept these instructions when we are in the
     * inline assembler mode. Sorry...
     */
    if (inlined && (
        insn->opcode == I_DT ||  /* long double problems! */
        insn->opcode == I_RESB ||
        insn->opcode == I_RESW ||
        insn->opcode == I_RESD ||
        insn->opcode == I_RESQ ||
        insn->opcode == I_REST ||
        insn->opcode == I_EQU))
    {
        apperror(RCERROR(ERROR_EXPECTING_INSN));
        insn->opcode = -1;
        return;
    }

    /*
     * RESB, RESW and RESD cannot be satisfied with incorrectly
     * evaluated operands, since the correct values *must* be known
     * on the first pass. Hence, even in pass one, we set the
     * 'critical' flag on calling asmexp(), so that it will bomb
     * out on undefined symbols. Nasty, but there's nothing we can
     * do about it.
     *
     * For the moment, EQU has the same difficulty, so we'll
     * include that.
     */
    if (insn->opcode == I_RESB ||
        insn->opcode == I_RESW ||
        insn->opcode == I_RESD ||
        insn->opcode == I_RESQ ||
        insn->opcode == I_REST ||
        insn->opcode == I_EQU)
    {
        critical = (pass < 3) ? 1 : 2;
    }
    else
    {
        critical = (pass < 3) ? 0 : 2;
    }

    if (insn->opcode == I_DB ||
        insn->opcode == I_DW ||
        insn->opcode == I_DD ||
        insn->opcode == I_DQ ||
        insn->opcode == I_DT)
    {
        EXTOPAND *eop, **tail = &insn->eops, **fixptr;
        int oper_num = 0;

        insn->eops_float = FALSE;

        /*
         * Begin to read the DB/DW/DD/DQ/DT operands.
         */
        for (;;)
        {
            token = scan(&tokval);
            if (token == TOK_EOI) break;

            fixptr = tail;
            eop = *tail = my_alloc(sizeof(EXTOPAND));
            tail = &eop->next;
            eop->next = NULL;
            eop->type = EOT_NOTHING;
            oper_num++;

            if (token == TOK_NUM && tokval.charptr && is_comma_next())
            {
                eop->type = EOT_DB_STRING;
                eop->stringval = tokval.charptr;
                eop->stringlen = tokval.inttwo;
                eop->vp = NULL;
                token = scan(&tokval);  /* eat the comma; yum-yum */
                continue;
            }

            if ((token == TOK_FLOAT && is_comma_next()) || token == '-')
            {
                long sign = +1L;

                if (token == '-')
                {
                    const char *save = as.scanner.getptr();
                    token = scan(&tokval);
                    sign = -1L;

                    if (token != TOK_FLOAT || !is_comma_next())
                    {
                        as.scanner.setptr(save);
                        token = tokval.type = '-';
                    }
                }

                if (token == TOK_FLOAT)
                {
                    eop->type = EOT_DB_STRING;
                    insn->eops_float = TRUE;

                    if (insn->opcode == I_DD)
                        eop->stringlen = 4;
                    else if (insn->opcode == I_DQ)
                        eop->stringlen = 8;
                    else if (insn->opcode == I_DT)
                        eop->stringlen = 10;
                    else
                    {
                        apperror(RCERROR(ERROR_FP_CONSTANT_IN_DB_INSN), (insn->opcode == I_DW) ? 'W' : 'B');
                        eop->stringlen = 0;
                    }

                    eop = my_realloc(eop, sizeof(EXTOPAND) + eop->stringlen);
                    tail = &eop->next;
                    *fixptr = eop;
                    eop->stringval = (char *)eop + sizeof(EXTOPAND);
                    eop->vp = NULL;
                    if (eop->stringlen < 4 || !asmflt(tokval.charptr,
                        sign, (uchar_t *)eop->stringval, eop->stringlen))
                        eop->type = EOT_NOTHING;

                    token = scan(&tokval);  /* eat the comma */
                    continue;
                }
            }

            /**/
            {
                EXPR *e;

                e = asmexp(&tokval, NULL, critical, NULL, inlined);
                token = tokval.type;
                if (!e)
                {
                    insn->opcode = -1;
                    return;
                }

                if (is_unknown(e))
                {
                    eop->type = EOT_DB_NUMBER;
                    eop->offset = 0;  /* doesn't matter what we put */
                    eop->segment = NO_SEG;  /* ditto */
                    eop->vp = NULL;
                }
                else if (is_reloc(e))
                {
                    eop->type = EOT_DB_NUMBER;
                    eop->offset = reloc_value(e);
                    eop->segment = reloc_seg(e);
                    eop->vp = e->vp;
                }
                else
                {
                    apperror(RCERROR(ERROR_EXPRESSION_NOT_SIMPLE), oper_num);
                }
            }

            /*
             * We're about to call scan(), which will eat the
             * comma that we're currently sitting on between
             * arguments. However, we'd better check first that
             * it *is* a comma.
             */
            if (token == TOK_EOI) break;

            if (token != ',')
            {
                apperror(RCERROR(ERROR_EXPECTING_COMMA), oper_num);
                insn->opcode = -1;
                return;
            }
        }

        if (oper_num == 0 && pass <= 1)
            apperror(RCWARNING1(ERROR_MISSING_DATA_OPERAND));
        else if (oper_num != 0)
            insn->nopands = oper_num;

        return;
    }

    /*
     * Now we begin to parse the operands. There may be up to three
     * of these, separated by commas, and terminated by a zero token.
     */
    for (operand = 0; operand < MAXOPAND; operand++)
    {
        EXPR *e;
        bool_t mref;        /* is this going to be a memory ref? */
        bool_t bracket;     /* is it a [] mref, or a & mref? */
        bool_t setsize = FALSE;

        insn->aops[operand].addr_size = 0;
        insn->aops[operand].eaflags = 0;
        insn->aops[operand].opflags = 0;
        insn->aops[operand].vp = 0;

        token = scan(&tokval);
        if (token == TOK_EOI) break;

        insn->aops[operand].type = 0;
        while (token == TOK_SPECIAL)
        {
            switch ((int)tokval.integer)
            {
                case S_BYTE:
                    if (!setsize)  /* we only want to use the first */
                        insn->aops[operand].type |= A_BITS8;
                    setsize = TRUE;
                    break;

                case S_WORD:
                    if (!setsize)
                        insn->aops[operand].type |= A_BITS16;
                    setsize = TRUE;
                    break;

                case S_DWORD:
                case S_LONG:
                    if (!setsize)
                        insn->aops[operand].type |= A_BITS32;
                    setsize = TRUE;
                    break;

                case S_QWORD:
                    if (!setsize)
                        insn->aops[operand].type |= A_BITS64;
                    setsize = TRUE;
                    break;

                case S_TBYTE:
                    if (!setsize)
                        insn->aops[operand].type |= A_BITS80;
                    setsize = TRUE;
                    break;

                case S_PTR:
                    if (!setsize)
                        apperror(RCERROR(ERROR_INVALID_OPERAND_TYPE));
                    break;

                case S_TO:
                    insn->aops[operand].type |= A_TO;
                    break;

                case S_FAR:
                    insn->aops[operand].type |= A_FAR;
                    break;

                case S_NEAR:
                    insn->aops[operand].type |= A_NEAR;
                    break;

                case S_SHORT:
                case S_SHRT:
                    insn->aops[operand].type |= A_SHORT;
                    break;

                case S_OFFSET:
                    insn->aops[operand].opflags |= OPFLAG_OFFSET;
                    break;

                default:
                    apperror(RCERROR(ERROR_INVALID_OPERAND_SIZE));
                    break;
            }

            token = scan(&tokval);
        }

        if (token == '[' || token == '&')  /* memory reference */
        {
            mref = TRUE;
            bracket = (token == '[');

            token = scan(&tokval);
            if (token == TOK_SPECIAL)  /* check for address size override */
            {
                switch ((int)tokval.integer)
                {
                    case S_BYTE:
                        insn->aops[operand].eaflags |= EAF_BYTEOFFS;
                        break;

                    case S_WORD:
                        insn->aops[operand].addr_size = 16;
                        insn->aops[operand].eaflags |= EAF_WORDOFFS;
                        break;

                    case S_DWORD:
                    case S_LONG:
                        insn->aops[operand].addr_size = 32;
                        insn->aops[operand].eaflags |= EAF_WORDOFFS;
                        break;

                    default:
                        apperror(RCERROR(ERROR_INVALID_EA_SIZE));
                        break;
                }

                token = scan(&tokval);
            }
        }
        else  /* immediate operand, or register */
        {
            mref = FALSE;
            bracket = FALSE;
        }

        e = asmexp(&tokval, &insn->aops[operand].opflags, critical, &hints, inlined);
        token = tokval.type;

        if (insn->aops[operand].opflags & OPFLAG_FORWARD)
            insn->forw_ref = TRUE;

        if (!e)
        {
            insn->opcode = -1;
            return;
        }

        if (token == ':' && mref)  /* it was seg:offset */
        {
            /*
             * Process the segment override.
             */
            if (e[1].type != 0 || e->value != 1 || (A_REG_SREG & ~reg_flags[e->type]))
                apperror(RCERROR(ERROR_INVALID_SEG_OVERRIDE));
            else if (insn->nprefix == MAXPREFIX)
                apperror(RCERROR(ERROR_TOO_MANY_INSN_PREFIX), MAXPREFIX);
            else
                insn->prefixes[insn->nprefix++] = e->type;

            token = scan(&tokval);
            if (token == TOK_SPECIAL)
            {
                switch ((int)tokval.integer)
                {
                    case S_WORD:
                        insn->aops[operand].addr_size = 16;
                        break;

                    case S_DWORD:
                    case S_LONG:
                        insn->aops[operand].addr_size = 32;
                        break;

                    default:
                        apperror(RCERROR(ERROR_INVALID_EA_SIZE));
                        break;
                }

                token = scan(&tokval);
            }

            /*
             * Process the offset.
             */
            e = asmexp(&tokval, &insn->aops[operand].opflags, critical, &hints, inlined);
            token = tokval.type;

            if (insn->aops[operand].opflags & OPFLAG_FORWARD)
                insn->forw_ref = TRUE;

            if (!e)
            {
                insn->opcode = -1;
                return;
            }
        }

        if (mref && bracket)  /* find ] at the end */
        {
            if (token != ']')
            {
                apperror(RCERROR(ERROR_EXPECTING_RBRACKET));
                do
                    token = scan(&tokval);
                while (token != TOK_EOI && token != ',');
            }
            else
            {
                token = scan(&tokval);
            }
        }
        else  /* immediate operand */
        {
            if (token != TOK_EOI && token != ',' && token != ':')
            {
                apperror(RCERROR(ERROR_EXPECTING_COMMA_OR_EOL));
                do
                    token = scan(&tokval);
                while (token != TOK_EOI && token != ',');
            }
            else if (token == ':')
            {
                insn->aops[operand].type |= A_COLON;
            }
        }

        /*
         * Now convert the exprs returned from asmexp() into operand descriptions.
         */
        if (mref)
        {
            EXPR *f = e;
            int b, i, s;               /* basereg, indexreg, scale */
            long o;                    /* offset */
            SYMBOL *sym = NULL;

            b = i = -1, o = s = 0;
            insn->aops[operand].hintbase = hints.base;
            insn->aops[operand].hinttype = hints.type;

            if (f->type <= EXPR_REG_END)  /* this bit's a register */
            {
                if (f->value == 1)  /* in fact it can be basereg */
                    b = f->type;
                else  /* has to be indexreg */
                    i = f->type, s = (int)f->value;
                f++;
            }

            if (f->type && f->type <= EXPR_REG_END)  /* it's a 2nd register */
            {
                if (b != -1)  /* If the first was the base, ... */
                {
                    i = f->type, s = (int)f->value;  /* second has to be indexreg */
                }
                else if (f->value != 1)  /* if both want to be index */
                {
                    apperror(RCERROR(ERROR_INVALID_EA_ADDR));
                    insn->opcode = -1;
                    return;
                }
                else
                {
                    b = f->type;
                }
                f++;
            }

            if (f->type != 0)  /* is there an offset? */
            {
                if (f->type <= EXPR_REG_END)  /* in fact, is there an error? */
                {
                    apperror(RCERROR(ERROR_INVALID_EA_ADDR));
                    insn->opcode = -1;
                    return;
                }
                else
                {
                    if (f->type == EXPR_UNKNOWN)
                    {
                        o = 0;
                        insn->aops[operand].segment = NO_SEG;
                        while (f->type) f++;   /* go to end of line */
                    }
                    else
                    {
                        if (f->type == EXPR_SIMPLE)
                        {
                            o = (int)f->value; sym = (SYMBOL *)f->vp;
                            f++;
                        }
                        /*
                         * Look for a segment base type.
                         */
                        if (f->type && f->type < EXPR_SEGBASE)
                        {
                            apperror(RCERROR(ERROR_INVALID_EA_ADDR));
                            insn->opcode = -1;
                            return;
                        }

                        while (f->type && f->value == 0)
                            f++;

                        if (f->type && f->value != 1)
                        {
                            apperror(RCERROR(ERROR_INVALID_EA_ADDR));
                            insn->opcode = -1;
                            return;
                        }

                        if (f->type)
                        {
                            insn->aops[operand].segment = f->type - EXPR_SEGBASE;
                            f++;
                        }
                        else
                        {
                            insn->aops[operand].segment = NO_SEG;
                        }

                        while (f->type && f->value == 0)
                            f++;

                        if (f->type)
                        {
                            apperror(RCERROR(ERROR_INVALID_EA_ADDR));
                            insn->opcode = -1;
                            return;
                        }
                    }
                }
            }
            else
            {
                o = 0;
                insn->aops[operand].segment = NO_SEG;
            }

            if (f->type != 0)  /* there'd better be nothing left! */
            {
                apperror(RCERROR(ERROR_INVALID_EA_ADDR));
                insn->opcode = -1;
                return;
            }

            insn->aops[operand].type |= A_MEMORY;
            if (b == -1 && (i == -1 || s == 0))
                insn->aops[operand].type |= A_MEM_OFFS;
            insn->aops[operand].basereg = b;
            insn->aops[operand].indexreg = i;
            insn->aops[operand].scale = s;
            insn->aops[operand].offset = o;
            insn->aops[operand].vp = sym;
        }
        else  /* not a memory reference */
        {
            if (is_just_unknown(e))  /* immediate but unknown */
            {
                insn->aops[operand].type |= A_IMMEDIATE;
                insn->aops[operand].offset = 0;
                insn->aops[operand].segment = NO_SEG;
                insn->aops[operand].vp = NULL;
            }
            else if (is_reloc(e))  /* immediate */
            {
                insn->aops[operand].type |= A_IMMEDIATE;
                insn->aops[operand].offset = (long)reloc_value(e);
                insn->aops[operand].segment = reloc_seg(e);
                insn->aops[operand].vp = e->vp;

                if (is_simple(e))
                {
                    if (reloc_value(e) == 1)
                        insn->aops[operand].type |= A_UNITY;
                    if (reloc_value(e) >= -128 && reloc_value(e) <= 127)
                        insn->aops[operand].type |= A_SBYTE;
                }
            }
            else  /* a register */
            {
                int i;

                if (e->type >= EXPR_SIMPLE || e->value != 1)
                {
                    apperror(RCERROR(ERROR_INVALID_OPERAND_TYPE));
                    insn->opcode = -1;
                    return;
                }

                /*
                 * check that it's only one register, not an expression...
                 */
                for (i = 1; e[i].type; i++)
                {
                    if (e[i].value)
                    {
                        apperror(RCERROR(ERROR_INVALID_OPERAND_TYPE));
                        insn->opcode = -1;
                        return;
                    }
                }

                /* clear overrides, except TO which applies to FPU regs */
                if (insn->aops[operand].type & ~A_TO)
                {
                    /*
                     * we want to produce a warning if the specified size
                     * is different from the register size.
                     */
                    i = insn->aops[operand].type & A_SIZEMASK;
                }
                else
                {
                    i = 0;
                }

                insn->aops[operand].type &= A_TO;
                insn->aops[operand].type |= A_REGISTER;
                insn->aops[operand].type |= reg_flags[e->type];
                insn->aops[operand].basereg = e->type;

                if (i && (insn->aops[operand].type & A_SIZEMASK) != i && pass <= 1)
                    apperror(RCWARNING2(ERROR_REG_SIZE_IGNORED));
            }
        }
    }

    insn->nopands = operand;

    while (operand < MAXOPAND)
        insn->aops[operand++].type = 0;

    /*
     * Transform RESW, RESD, RESQ, REST into RESB.
     */
    switch (insn->opcode)
    {
        case I_RESW: insn->opcode = I_RESB; insn->aops[0].offset *= 2; break;
        case I_RESD: insn->opcode = I_RESB; insn->aops[0].offset *= 4; break;
        case I_RESQ: insn->opcode = I_RESB; insn->aops[0].offset *= 8; break;
        case I_REST: insn->opcode = I_RESB; insn->aops[0].offset *= 10; break;
    }

    return;
}

/****************************************************************************
 *                                                                          *
 * Function: cleanup                                                        *
 *                                                                          *
 * Purpose : Clean up!                                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void cleanup(INSN *insn)
{
    while (insn->eops)
    {
        EXTOPAND *eop = insn->eops;
        insn->eops = insn->eops->next;
        my_free(eop);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: is_comma_next                                                  *
 *                                                                          *
 * Purpose : Peek ahead during DB/DW/DD/DQ/DT operand parsing.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t is_comma_next(void)
{
    const char *save;
    TOKENVAL tokval;
    int token;

    save = as.scanner.getptr();
    token = scan(&tokval);
    as.scanner.setptr(save);

    return (token == ',' || token == ';' || token == TOK_EOI);
}

