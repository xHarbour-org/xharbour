/****************************************************************************
 *                                                                          *
 * File    : armparse.c                                                     *
 *                                                                          *
 * Purpose : ISO C Compiler; ARM Assembler; Source code parser.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *           01-09-21  C inline parser added.                               *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"
#include "arm.h"

#define scan  as.scanner.scan

/* maximum number of operands */
#define MAXOPAND  4

/* Static function prototypes */
static void inline_parser(const char *, char **, char **, SYMBOL **, long *);
static void parser(int, const char *, INSN *);
static void cleanup(INSN *);
static bool_t is_comma_next(void);
static int reglist_index(int);

/****************************************************************************
 *                                                                          *
 * Function: arm_parser_init                                                *
 *                                                                          *
 * Purpose : Initialize the parser module (ARM mode).                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

void arm_parser_init(void)
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
 *           01-09-21  Created                                              *
 *           04-08-11  Accept the _emit instruction in Microsoft mode.      *
 *                                                                          *
 ****************************************************************************/

static void inline_parser(const char *s, char **name, char **text, SYMBOL **symp, long *cl)
{
    INSN insn;

    /* Microsoft accepts the _emit pseudo instruction */
    if (options.microsoft && _strnicmp(s, "_emit ", 6) == 0)
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
 * Purpose : Parse assembler code (ARM mode).                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *           04-06-10  Changed 'Label without colon' into error.            *
 *           04-06-23  Bugfix: change reg+x to reg,x - still handling n+n.  *
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
    bool_t bracket;
    bool_t minus;
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

    if (token != TOK_ID && token != TOK_INSN && token != TOK_PREFIX && token != TOK_REG)
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

    while (token == TOK_PREFIX || token == TOK_REG)
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
        apperror(RCERROR(ERROR_EXPECTING_INSN));
        insn->opcode = -1;
        return;
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

                    eop = my_realloc(eop, sizeof(EXTOPAND)+eop->stringlen);
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
     * Now we begin to parse the operands. There may be up to four
     * of these, separated by commas, and terminated by a zero token.
     */
    bracket = FALSE; minus = FALSE;
    for (operand = 0; operand < MAXOPAND; operand++)
    {
        EXPR *e;

        insn->aops[operand].addr_size = 0;
        insn->aops[operand].eaflags = 0;
        insn->aops[operand].opflags = 0;
        insn->aops[operand].vp = 0;
        insn->aops[operand].shiftflag = 0;

        token = scan(&tokval);
        if (token == TOK_EOI) break;

        insn->aops[operand].type = 0;
        while (token == TOK_SPECIAL)
        {
            switch ((int)tokval.integer)
            {
                case S_ASL:
                case S_ASR:
                case S_LSL:
                case S_LSR:
                case S_ROR:
                case S_RRX:  /* RRX is ROR #0 and not impl like this <-- */
                    if (operand != 3 && operand != 2)
                        apperror(RCERROR(ERROR_INVALID_OPERAND_TYPE));
                    else
                        insn->aops[operand].shiftflag = (int)tokval.integer;
                    break;

                default:
                    apperror(RCERROR(ERROR_EXPECTING_INSN));
                    break;
            }

            token = scan(&tokval);
        }

        if (token == '[')  /* memory reference */
        {
            bracket = TRUE;
            token = scan(&tokval);
        }
        insn->aops[operand].bracket = bracket;

        if (token == TOK_REG)
        {
            int r;

            r = (int)tokval.integer;

            insn->aops[operand].type = A_REGISTER;
            insn->aops[operand].basereg = r;
            insn->aops[operand].minus = minus;

            switch (r)
            {
                case R_CPSR_FLG:
                case R_SPSR_FLG:
                    insn->aops[operand].type |= A_FLAG;
                    break;

                default:
                    insn->aops[operand].type |= A_BITS32;
                    break;
            }

            token = scan(&tokval);
            if (token == '+')  /* bugfix 04-06-23 */
                token = ',';
        }
        else if (token == '{')  /* register list */
        {
            bool_t shut_up = FALSE;
            int r, p = 0;

            insn->aops[operand].type |= A_REGLIST;
            insn->aops[operand].basereg = 0;

            token = scan(&tokval);
            while (token != '}')
            {
                if (token != TOK_REG)
                {
                    apperror(RCERROR(ERROR_EXPECTING_REGISTER));
                    break;  /* stop infinite recursion */
                }
                else
                {
                    r = reglist_index((int)tokval.integer);
                    if ((insn->aops[operand].basereg & r) != 0 && !shut_up)
                    {
                        apperror(RCWARNING(ERROR_DUPLICATE_REGISTERS));
                        shut_up = TRUE;
                    }
                    insn->aops[operand].basereg |= r;

                    if (p)
                    {
                        int lo, hi;

                        if (p > r)
                        {
                            hi = p >> 1;  /* don't take p twice */
                            lo = r << 1;  /* don't take r twice */
                        }
                        else  /* also handles p == r */
                        {
                            hi = r >> 1;
                            lo = p << 1;
                        }

                        while (lo <= hi)
                        {
                            if ((insn->aops[operand].basereg & lo) && !shut_up)
                            {
                                apperror(RCWARNING(ERROR_DUPLICATE_REGISTERS));
                                shut_up = TRUE;
                            }
                            insn->aops[operand].basereg |= lo;
                            lo = lo << 1;
                        }

                        p = 0;
                    }
                }

                token = scan(&tokval);
                if (token == '-')
                {
                    p = r;
                    token = scan(&tokval);
                }
                if (token == ',')
                    token = scan(&tokval);
            }

            token = scan(&tokval);
        }
        else
        {
            if (token == '#')  /* immediate operand */
                token = scan(&tokval);

            e = asmexp(&tokval, &insn->aops[operand].opflags, critical, &hints, inlined);
            token = tokval.type;

            if (insn->aops[operand].opflags & OPFLAG_FORWARD)
                insn->forw_ref = TRUE;

            if (!e)
            {
                insn->opcode = -1;
                return;
            }

            if (is_just_unknown(e))
            {
                /* it's immediate but unknown */
                insn->aops[operand].type |= A_IMMEDIATE;
                insn->aops[operand].type |= A_BITS32;
                insn->aops[operand].offset = 0;        /* don't care */
                insn->aops[operand].segment = NO_SEG;  /* don't care again */
            }
            else if (is_reloc(e))
            {
                /* it's immediate */
                insn->aops[operand].type |= A_IMMEDIATE;
                insn->aops[operand].type |= A_BITS32;
                insn->aops[operand].offset = (long)reloc_value(e);
                insn->aops[operand].segment = reloc_seg(e);
                insn->aops[operand].vp = e->vp;

                if (is_simple(e) && reloc_value(e) == 1)
                    insn->aops[operand].type |= A_UNITY;
            }
        }

        if (token == ']')
        {
            bracket = FALSE;
            token = scan(&tokval);
        }

        if (token == '!')
        {
            insn->condition |= C_WSETFLAG;
            token = scan(&tokval);
        }
        if (token == '^')
        {
            insn->condition |= C_FSETFLAG;
            token = scan(&tokval);
        }

        if (token != TOK_EOI && token != ',')
        {
            apperror(RCERROR(ERROR_EXPECTING_COMMA_OR_EOL));
            do
                token = scan(&tokval);
             while (token != TOK_EOI && token != ',');
        }
    }

    if (bracket)
        apperror(RCERROR(ERROR_EXPECTING_RBRACKET));

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
}

/****************************************************************************
 *                                                                          *
 * Function: cleanup                                                        *
 *                                                                          *
 * Purpose : Clean up!                                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
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
 *           00-11-21  Created                                              *
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

/****************************************************************************
 *                                                                          *
 * Function: reglist_index                                                  *
 *                                                                          *
 * Purpose : Return reglist index for the given register.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int reglist_index(int reg)
{
    switch (reg)
    {
        case R_A1: case R_R0: return 1;
        case R_A2: case R_R1: return 1 << 1;
        case R_A3: case R_R2: return 1 << 2;
        case R_A4: case R_R3: return 1 << 3;
        case R_V1: case R_R4: return 1 << 4;
        case R_V2: case R_R5: return 1 << 5;
        case R_V3: case R_R6: return 1 << 6;
        case R_V4: case R_R7: return 1 << 7;
        case R_V5: case R_R8: return 1 << 8;
        case R_V6: case R_R9: return 1 << 9;
        case R_V7: case R_R10: case R_SL: return 1 << 10;
        case R_V8: case R_R11: case R_FP: return 1 << 11;
        case R_IP: case R_R12: return 1 << 12;
        case R_SP: case R_R13: return 1 << 13;
        case R_LR: case R_R14: return 1 << 14;
        case R_PC: case R_R15: return 1 << 15;
        default:
            apperror(RCERROR(ERROR_ILLEGAL_REGISTER));
            return 0;
    }
}

