/****************************************************************************
 *                                                                          *
 * File    : armscan.c                                                      *
 *                                                                          *
 * Purpose : ISO C Compiler; ARM Assembler; Token scanner.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"
#include "arm.h"

#if MAX_ARMINSLEN > 9
#define MAX_KEYWORD  MAX_ARMINSLEN
#else
#define MAX_KEYWORD  9
#endif

#define STACK_INCREMENT  256

/* Instruction names generated from arm.dat */
#include "arminsn.c"

/* register names, as strings */
static const char *reg_names[] = {
    "a1", "a2", "a3", "a4",
    "cpsr", "cpsr_flg", "fp", "ip", "lr", "pc",
    "r0", "r1", "r10", "r11", "r12", "r13", "r14",
    "r15", "r2", "r3", "r4", "r5", "r6", "r7",
    "r8", "r9", "sl", "sp", "spsr","spsr_flg",
    "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8"
};

/* condition code names */
static const char *conditions[] = {
    "al", "cc", "cs", "eq",
    "ge", "gt", "hi", "le",
    "ls", "lt", "mi", "ne",
    "pl", "vc", "vs"
};

// ASL, LSL, LSR, ASR, ROR (ASL=LSL)
static const char *special_names[] = {
    "asl", "asr", "lsl", "lsr", "ror", "rrx",
};

/* instruction prefix names */
static const char *prefix_names[] = {
    "times"
};

/*
 * Scanner routine used by armparse.c and some output formats.
 * It keeps a succession of temporary-storage strings in
 * stack_tempstorage, which can be cleared using reset.
 */
static char **stack_tempstorage = NULL;
static int stack_maxcount = 0;
static int stack_count = 0;

static const char *bp = NULL;

#define lookup(s,arr)  my_bsearch((s),(arr),NELEMS(arr),strcmp)
#define lookupn(s,arr)  my_bsearch((s),(arr),NELEMS(arr),mystrcmp)

/* Static function prototypes */
static int scanner(TOKENVAL *);
static int __cdecl mystrcmp(const char *, const char *);
static const char *get_bufptr(void);
static void set_bufptr(const char *);
static char *push_copy(const char *, size_t);
static void reset(void);
static void cleanup(void);
static char *asmtext(INSN *, SYMBOL **, long *);
static int asmreg(const char *);

/****************************************************************************
 *                                                                          *
 * Function: arm_scanner_init                                               *
 *                                                                          *
 * Purpose : Initialize the scanner module (ARM mode).                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

void arm_scanner_init(void)
{
    as.scanner.scan = scanner;
    as.scanner.getptr = get_bufptr;
    as.scanner.setptr = set_bufptr;
    as.scanner.reset = reset;
    as.scanner.cleanup = cleanup;
    as.scanner.asmtext = asmtext;
    as.scanner.asmreg = asmreg;
}

/****************************************************************************
 *                                                                          *
 * Function: scanner                                                        *
 *                                                                          *
 * Purpose : Really *ugly* token scanner (ARM mode).                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *           04-05-01  Added support for SIZEOF keyword.                    *
 *                                                                          *
 ****************************************************************************/

static int scanner(TOKENVAL *tv)
{
    while (isspace(*bp))
        bp++;

    if (*bp == '\0')
        return tv->type = TOK_EOI;

    /* we have a token; either an id, a number or a char */
    if (isidstart(*bp) || (*bp == '$' && isidstart(bp[1])))
    {
        /* now we've got an identifier */
        char ourcopy[MAX_KEYWORD+1], *s, *t;
        bool_t is_sym = FALSE;
        const char *r;
        int i;

        if (*bp == '$')
        {
            is_sym = TRUE;
            bp++;
        }

        for (r = bp++; isidchar(*bp); bp++)
            ;

        tv->charptr = push_copy(r, bp - r);

        if (is_sym || (bp - r) > MAX_KEYWORD)
            return tv->type = TOK_ID;  /* bypass all other checks */

        for (s = tv->charptr, t = ourcopy; *s != '\0'; s++)
            *t++ = tolower(*s);
        *t = '\0';

        /*
         * Right. So we have an identifier sitting in temporary storage.
         * is it actually a register, a instruction name, a french
         * potato or what...?
         */
        if ((tv->integer = lookup(ourcopy, reg_names)) != -1)
        {
            tv->integer += EXPR_REG_START;
            return tv->type = TOK_REG;
        }
        if ((tv->integer = lookup(ourcopy, insn_names)) != -1)
        {
            /* Normal instruction - cc is NONE */
            tv->inttwo = C_NONE;
            return tv->type = TOK_INSN;
        }
        for (i = 0; i < NELEMS(cond_insn_names); i++)
        {
            /* This works as long as perl sort in this order: "ldrh", "ldrsb", "ldrsh", "ldr" */
            if (strncmp(ourcopy, cond_insn_names[i], strlen(cond_insn_names[i])) == 0)
            {
                char *p = ourcopy + strlen(cond_insn_names[i]);

                tv->integer = cond_insn_codes[i];

                /* Conditional instruction - set cc */
                if ((tv->inttwo = lookupn(p, conditions)) != -1)
                    p += 2;  /* always 2 characters */
                else
                    tv->inttwo = C_NONE;

                /* Since perl sort "bl" before "b", we get into trouble with "blt" etc */
                if (tv->integer == I_BLcc && isalpha(*p)) continue;

                if (*p == 's')
                {
                    p++;
                    tv->inttwo |= C_SSETFLAG;
                }
                if (*p == 'b')
                {
                    p++;
                    tv->inttwo |= C_BSETFLAG;
                }
                if (*p == 't')
                {
                    p++;
                    tv->inttwo |= C_TSETFLAG;
                }

                return tv->type = TOK_INSN;
            }
        }
        if ((tv->integer = lookup(ourcopy, prefix_names)) != -1)
        {
            tv->integer += PREFIX_ENUM_START;
            return tv->type = TOK_PREFIX;
        }
        if ((tv->integer = lookup(ourcopy, special_names)) != -1)
            return tv->type = TOK_SPECIAL;
        if (strcmp(ourcopy, "sizeof") == 0)
            return tv->type = TOK_SIZEOF;

        return tv->type = TOK_ID;
    }
    else if (*bp == '$')
    {
        /*
         * This is either a Here token ($), evaluating to the current
         * assembly location, or a Base token ($$), evaluating to
         * the base of the current segment.
         */
        bp++;
        if (*bp == '$')
        {
            bp++;
            return tv->type = TOK_BASE;
        }
        return tv->type = TOK_HERE;
    }
    else if (isnumstart(*bp))
    {
        /* a numeric constant */
        bool_t rn_error;
        const char *r;

        for (r = bp++; isnumchar(*bp); bp++)
            ;

        /* a floating point constant? */
        if (*bp == '.')
        {
            for (bp++; isnumchar(*bp) || ((bp[-1] == 'e' || bp[-1] == 'E') && (*bp == '-' || *bp == '+')); bp++)
                ;

            tv->charptr = push_copy(r, bp - r);
            return tv->type = TOK_FLOAT;
        }

        r = my_strndup(r, bp - r);
        tv->integer = readnum(r, &rn_error);
        my_free((char *)r);
        if (rn_error) return tv->type = TOK_ERRNUM;

        tv->charptr = NULL;
        return tv->type = TOK_NUM;
    }
    else if (*bp == '\'' || *bp == '"')
    {
        /* a char constant */
        char quote = *bp++;
        const char *r;

        tv->charptr = (char *)(r = bp);
        while (*bp && *bp != quote)
            bp++;

        tv->inttwo = bp - r;
        if (!*bp)
            return tv->type = TOK_ERRNUM;  /* unmatched quotes */

        bp++;
        tv->integer = readstrnum(r, tv->inttwo);
        return tv->type = TOK_NUM;
    }
    else if (*bp == ';')
    {
        /* a comment has just happened */
        return tv->type = TOK_EOI;
    }
    else if (bp[0] == '>' && bp[1] == '>')
    {
        bp += 2;
        return tv->type = TOK_SHR;
    }
    else if (bp[0] == '<' && bp[1] == '<')
    {
        bp += 2;
        return tv->type = TOK_SHL;
    }
    else if (bp[0] == '/' && bp[1] == '/')
    {
        bp += 2;
        return tv->type = TOK_SDIV;
    }
    else if (bp[0] == '%' && bp[1] == '%')
    {
        bp += 2;
        return tv->type = TOK_SMOD;
    }
    else
    {
        /* just an ordinary char :-( */
        return tv->type = (uchar_t)(*bp++);
    }
}

/****************************************************************************
 *                                                                          *
 * Subfunction: mystrcmp                                                    *
 *                                                                          *
 ****************************************************************************/

/* strncmp with strcmp signature */
static int __cdecl mystrcmp(const char *s1, const char *s2)
{
    return strncmp(s1, s2, strlen(s2));
}

/****************************************************************************
 *                                                                          *
 * Function: get_bufptr                                                     *
 *                                                                          *
 * Purpose : Return the current buffer position of the scanner.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static const char *get_bufptr(void)
{
    return bp;
}

/****************************************************************************
 *                                                                          *
 * Function: set_bufptr                                                     *
 *                                                                          *
 * Purpose : Set the current buffer position of the scanner.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void set_bufptr(const char *buffer)
{
    bp = buffer;
}

/****************************************************************************
 *                                                                          *
 * Function: push_copy                                                      *
 *                                                                          *
 * Purpose : Push a temporary string onto the stack.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static char *push_copy(const char *s, size_t len)
{
    char *text;

    if (stack_count == stack_maxcount)
    {
        stack_maxcount += STACK_INCREMENT;
        stack_tempstorage = my_realloc(stack_tempstorage, stack_maxcount * sizeof(char *));
    }

    text = my_strndup(s, len);
    stack_tempstorage[stack_count++] = text;

    return text;
}

/****************************************************************************
 *                                                                          *
 * Function: reset                                                          *
 *                                                                          *
 * Purpose : Reset the stack by popping all entries.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void reset(void)
{
    while (stack_count > 0)
        my_free(stack_tempstorage[--stack_count]);
}

/****************************************************************************
 *                                                                          *
 * Function: cleanup                                                        *
 *                                                                          *
 * Purpose : Unimportant cleanup is done to avoid confusing people who      *
 *           are trying to debug real memory leaks.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void cleanup(void)
{
    reset();
    my_free(stack_tempstorage);
    stack_tempstorage = NULL;
    stack_maxcount = 0;
}

/****************************************************************************
 *                                                                          *
 * Function: asmtext                                                        *
 *                                                                          *
 * Purpose : Convert parsed inline code back to readable form.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-21  Created                                              *
 *           04-08-04  Added support for DB, DW, DD and DQ.                 *
 *           04-08-11  Bugfix: must handle float's in DD and DQ.            *
 *                                                                          *
 ****************************************************************************/

static char *asmtext(INSN *insn, SYMBOL **symp, long *cl)
{
    char buf[MAXLINE], *p = buf;
    bool_t has_S_code;  /* S - setflag */
    bool_t has_B_code;  /* B - setflag */
    bool_t has_T_code;  /* T - setflag */
    bool_t has_W_code;  /* ! => W flag */
    bool_t has_F_code;  /* ^ => S flag */
    bool_t bracket = FALSE;
    int n;

    *symp = NULL;
    *cl = 0;

    /* TIMES prefix, if needed */
    if (insn->times > 1)
        p += sprintf(p, "%s %ld ", prefix_names[P_TIMES-PREFIX_ENUM_START], insn->times);

    has_S_code = (insn->condition & C_SSETFLAG);
    has_B_code = (insn->condition & C_BSETFLAG);
    has_T_code = (insn->condition & C_TSETFLAG);
    has_W_code = (insn->condition & C_WSETFLAG);
    has_F_code = (insn->condition & C_FSETFLAG);
    insn->condition = (insn->condition & 0x0F);

    /* instruction */
    assert(insn->opcode < NELEMS(cond_insn_names));
    p += sprintf(p, "%s%s%s%s%s", cond_insn_names[insn->opcode - NELEMS(insn_names)],
        (insn->condition < C_NONE) ? conditions[insn->condition] : "",
        (has_S_code ? "S" : ""), (has_B_code ? "B" : ""), (has_T_code ? "T" : ""));

    /* data definitions */
    if (insn->opcode == I_DB ||
        insn->opcode == I_DW ||
        insn->opcode == I_DD ||
        insn->opcode == I_DQ ||
        insn->opcode == I_DT)
    {
        EXTOPAND *eop;

        *p++ = ' ';
        for (eop = insn->eops; eop != NULL; eop = eop->next)
        {
            if (eop->vp != NULL)  /* avoid some trouble for now */
                apperror(RCERROR(ERROR_INVALID_EA_ADDR));

            if (insn->eops_float && eop->stringlen == 4)
                p += sprintf(p, "0%xH", *(uint_t *)eop->stringval);
            else if (insn->eops_float && eop->stringlen == 8)
                p += sprintf(p, "0%I64xH", *(uintmax_t *)eop->stringval);
            else if (eop->type == EOT_DB_NUMBER)
                p += sprintf(p, "0%xH", eop->offset);
            else if (eop->type == EOT_DB_STRING)
                p += sprintf(p, "\"%.*s\"", eop->stringlen, eop->stringval);

            if (eop->next != NULL)
                *p++ = ',';
        }
    }
    /* instruction operands */
    else for (n = 0; n < insn->nopands; n++)
    {
        SYMBOL *sym = (SYMBOL *)insn->aops[n].vp;
        int type = insn->aops[n].type;

        if (sym != NULL)
        {
            assert(*symp == NULL);
            *symp = sym;

            sym->ref++;  /* unconditional 03-08-27 */

            if (insn->opcode == I_BLcc || insn->opcode == I_BXcc || insn->opcode == I_Bcc)
                funcsym->u.fcn.ncalls++;
        }

        /* separator */
        *p++ = (n == 0) ? ' ' : ',';

        if (insn->aops[n].bracket && !bracket)
            *p++ = '[', bracket = TRUE;

        switch (insn->aops[n].shiftflag)
        {
            case S_LSL: p += sprintf(p, "lsl "); break;
            case S_LSR: p += sprintf(p, "lsr "); break;
            case S_ASR: p += sprintf(p, "asr "); break;
            case S_ROR: p += sprintf(p, "ror "); break;
            case S_RRX: p += sprintf(p, "rrx "); break;
            case S_ASL:  /* zero = default */
                break;
        }

        if (!(A_REGLIST & ~type))
        {
            int regidx[] = { R_R0, R_R1, R_R2, R_R3, R_R4, R_R5, R_R6, R_R7, R_R8, R_R9, R_R10, R_FP, R_IP, R_SP, R_LR, R_PC };
            int i;

            *p++ = '{';
            for (i = 0; i < 16; i++)
            {
                if (insn->aops[n].basereg & (1 << i))
                    p += sprintf(p, "%s,", reg_names[regidx[i] - EXPR_REG_START]);
            }
            p[-1] = '}';  /* replace last comma */
        }
        else if (!(A_REGISTER & ~type))
        {
            if (insn->aops[n].minus) *p++ = '-';
            p += sprintf(p, "%s", reg_names[insn->aops[n].basereg - EXPR_REG_START]);
            if (has_W_code && n == 0) *p++ = '!';
        }
        else if (!(A_IMMEDIATE & ~type))
        {
            if (sym != NULL)
            {
                if ((insn->opcode == I_BLcc || insn->opcode == I_BXcc || insn->opcode == I_Bcc) &&
                    (sym->sclass == STATIC || sym->sclass == EXTERN || sym->scope == LABELS))
                {
                    *p++ = '?';  /* placeholder */
                }
                else if (insn->opcode == I_LDRSBcc && sym->type->size == 1)
                {
                    *p++ = '?';  /* placeholder */
                }
                else if ((insn->opcode == I_LDRHcc || insn->opcode == I_STRHcc || insn->opcode == I_LDRSHcc) && sym->type->size == 2)
                {
                    *p++ = '?';  /* placeholder */
                }
                else if ((insn->opcode == I_LDRcc || insn->opcode == I_STRcc) && sym->type->size == 4)
                {
                    *p++ = '?';  /* placeholder */
                }
                else apperror(RCERROR(ERROR_INVALID_EA_ADDR));
            }
            else
            {
                p += sprintf(p, "#0x%x", insn->aops[n].offset);
            }
        }
        else assert(0);
    }

    if (bracket) *p++ = ']';
    if (has_F_code) *p++ = '^';

    *p++ = ';';  /* silly comment to fool the optimizer */
    *p++ = '\n';
    return stringn(buf, p-buf);
}

/****************************************************************************
 *                                                                          *
 * Function: asmreg                                                         *
 *                                                                          *
 * Purpose : Look up the string in the register name array.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int asmreg(const char *s)
{
    return lookup(s, reg_names);
}

