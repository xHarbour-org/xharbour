/****************************************************************************
 *                                                                          *
 * File    : x86scan.c                                                      *
 *                                                                          *
 * Purpose : ISO C Compiler; X86 Assembler; Token scanner.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"
#include "x86.h"

#if MAX_X86INSLEN > 9
#define MAX_KEYWORD  MAX_X86INSLEN
#else
#define MAX_KEYWORD  9
#endif

#define STACK_INCREMENT  256

/* Instruction names generated from x86.dat */
#include "x86insn.c"

/* register names, as strings */
static const char *reg_names[] = {
    "ah", "al", "ax", "bh", "bl", "bp", "bx", "ch", "cl",
    "cr0", "cr2", "cr3", "cr4", "cs", "cx", "dh", "di", "dl", "dr0",
    "dr1", "dr2", "dr3", "dr6", "dr7", "ds", "dx", "eax", "ebp",
    "ebx", "ecx", "edi", "edx", "es", "esi", "esp", "fs", "gs",
    "mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7", "si",
    "sp", "ss", "st0", "st1", "st2", "st3", "st4", "st5", "st6",
    "st7", "tr3", "tr4", "tr5", "tr6", "tr7",
    "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"
};

/* condition code names */
static const char *conditions[] = {
    "a", "ae", "b", "be", "c", "e", "g", "ge", "l", "le", "na", "nae",
    "nb", "nbe", "nc", "ne", "ng", "nge", "nl", "nle", "no", "np",
    "ns", "nz", "o", "p", "pe", "po", "s", "z"
};

/* special names */
static const char *special_names[] = {
    "byte", "dword", "far", "long", "near", "offset", "ptr",
    "qword", "short", "shrt", "tbyte", "to", "word"
};

/* instruction prefix names */
static const char *prefix_names[] = {
    "lock", "rep", "repe", "repne", "repnz", "repz", "times"
};

/*
 * Scanner routine used by x86parse.c and some output formats.
 * It keeps a succession of temporary-storage strings in
 * stack_tempstorage, which can be cleared using reset.
 */
static char **stack_tempstorage = NULL;
static int stack_maxcount = 0;
static int stack_count = 0;

static const char *bp = NULL;

#define lookup(s,arr)  my_bsearch((s),(arr),NELEMS(arr),strcmp)

/* Static function prototypes */
static int scanner(TOKENVAL *);
static const char *get_bufptr(void);
static void set_bufptr(const char *);
static char *push_copy(const char *, size_t);
static void reset(void);
static void cleanup(void);
static char *asmtext(INSN *, SYMBOL **, long *);
static int asmreg(const char *);

/****************************************************************************
 *                                                                          *
 * Function: x86_scanner_init                                               *
 *                                                                          *
 * Purpose : Initialize the scanner module.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

void x86_scanner_init(void)
{
    assert(strlen(special_names[S_SHRT]) == strlen(special_names[S_NEAR]));

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
 * Purpose : Really *ugly* token scanner.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
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
        else if ((tv->integer = lookup(ourcopy, insn_names)) != -1)
        {
            return tv->type = TOK_INSN;
        }
        for (i = 0; i < NELEMS(cond_insn_names); i++)
        {
            if (strncmp(ourcopy, cond_insn_names[i], strlen(cond_insn_names[i])) == 0)
            {
                char *p = ourcopy + strlen(cond_insn_names[i]);
                tv->integer = cond_insn_codes[i];
                if ((tv->inttwo = lookup(p, conditions)) != -1)
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
 *           00-06-06  Created                                              *
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
 *           00-06-06  Created                                              *
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
 *           00-06-06  Created                                              *
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
 *           00-06-06  Created                                              *
 *           01-08-24  Bugfix: check for I_LEA in A_IMMEDIATE case too.     *
 *           01-08-24  Bugfix: handle function symbols (imported too).      *
 *           02-10-03  Bugfix: handle array symbols.                        *
 *           04-03-14  Bugfix: check for I_JECXZ in A_IMMEDIATE case.       *
 *           04-04-13  Bugfix: too many commas in segment overrides.        *
 *           04-04-22  Added support for reg,OFFSET var.                    *
 *           04-04-13  Bugfix: offset wasn't emitted for A_IMMEDIATE case.  *
 *           04-08-04  Added support for DB, DW, DD and DQ.                 *
 *           04-08-11  Bugfix: must handle float's in DD and DQ.            *
 *                                                                          *
 ****************************************************************************/

static char *asmtext(INSN *insn, SYMBOL **symp, long *cl)
{
    char buf[MAXLINE], *p = buf;
    int n;

    *symp = NULL;
    *cl = 0;

    /* TIMES prefix, if needed */
    if (insn->times > 1)
        p += sprintf(p, "%s %ld ", prefix_names[P_TIMES-PREFIX_ENUM_START], insn->times);

    /* instruction prefixes */
    for (n = 0; n < insn->nprefix; n++)
    {
        switch (insn->prefixes[n])
        {
            case P_LOCK:
            case P_REP:
            case P_REPE:
            case P_REPNE:
            case P_REPNZ:
            case P_REPZ:
                p += sprintf(p, "%s ", prefix_names[insn->prefixes[n] - PREFIX_ENUM_START]);
                break;
        }
    }

    /* instruction */
    switch (insn->opcode)
    {
        case I_CMOVcc:
        case I_Jcc:
        case I_SETcc:
            for (n = 0; n < NELEMS(cond_insn_codes); n++)
            {
                if (cond_insn_codes[n] == insn->opcode)
                {
                    p += sprintf(p, "%s%s", cond_insn_names[n], conditions[insn->condition]);
                    break;
                }
            }
            break;

        default:
            assert(insn->opcode < NELEMS(insn_names));
            p += sprintf(p, "%s", insn_names[insn->opcode]);
            break;
    }

    /* data definitions */
    if (insn->opcode == I_DB ||
        insn->opcode == I_DW ||
        insn->opcode == I_DD ||
        insn->opcode == I_DQ)
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

            if (insn->opcode == I_CALL)  /* added 02-08-01 */
                funcsym->u.fcn.ncalls++;
        }

        /* separator */
        if (n == 0)
            *p++ = ' ';
        else if (p[-1] != ' ' && p[-1] != ',')  /* bugfix 04-04-13 */
            *p++ = ',';

        if (!(A_REGISTER & ~type))
        {
            assert(!sym);

            /* hack: convert Microsoft seg:[addr] to NASM [seg:addr] */
            if (!(A_REG_SREG & ~type) && n < insn->nopands-1 &&
                !(A_MEMORY & ~insn->aops[n+1].type) && insn->nprefix == 0)
            {
                insn->prefixes[insn->nprefix++] = insn->aops[n].basereg;
                continue;
            }

            p += sprintf(p, "%s", reg_names[insn->aops[n].basereg - EXPR_REG_START]);
        }
        else if (!(A_IMMEDIATE & ~type))
        {
            if (sym != NULL)
            {
                /* mov reg,variable --> mov reg,[variable] */

                if (sym->sclass != STATIC && sym->sclass != EXTERN &&
                    sym->sclass != AUTO && sym->sclass != 0)
                {
                    apperror(RCERROR(ERROR_INVALID_EA_ADDR));
                    continue;
                }

                /* handle mov reg,OFFSET var */
                if (insn->aops[n].opflags & OPFLAG_OFFSET)
                {
                    *p++ = '?';  /* placeholder */
                    if (insn->aops[n].offset != 0)
                        p += sprintf(p, "+0x%x", insn->aops[n].offset);
                    continue;
                }

                /* use explicit size or deduce from symbol */
                if (insn->opcode == I_LEA)
                    ;
                else if (insn->opcode == I_JECXZ)  /* bugfix 04-03-14 */
                    ;
                else if (type & A_BITS8)
                    p += sprintf(p, "%s ", special_names[S_BYTE]);
                else if (type & A_BITS16)
                    p += sprintf(p, "%s ", special_names[S_WORD]);
                else if (type & A_BITS32)
                    p += sprintf(p, "%s ", special_names[S_DWORD]);
                else if (type & A_BITS64)
                    p += sprintf(p, "%s ", special_names[S_QWORD]);
                else if (type & A_BITS80)
                    p += sprintf(p, "%s ", special_names[S_TBYTE]);
                else if (sym->type == NULL)
                    p += sprintf(p, "%s ", special_names[S_NEAR]);
                else if (isptr(sym->type))
                    ;
                else if (isfunc(sym->type))
                    p += sprintf(p, "%s ", special_names[S_DWORD]);
                else if (sym->type->size == 1)
                    p += sprintf(p, "%s ", special_names[S_BYTE]);
                else if (sym->type->size == 2)
                    p += sprintf(p, "%s ", special_names[S_WORD]);
                else if (sym->type->size == 4)
                    p += sprintf(p, "%s ", special_names[S_DWORD]);
                else if (sym->type->size == 8)
                    p += sprintf(p, "%s ", special_names[S_QWORD]);
                else if (sym->type->size != 0)
                    apperror(RCERROR(ERROR_INVALID_EA_ADDR));

                /* isarray() added 02-10-03; isstruct() added 04-03-27 */
                if (sym->type && (isarith(sym->type) || isptr(sym->type) || isstruct(sym->type) ||
                    isarray(sym->type) || (isfunc(sym->type) && sym->attr.dllimport)))
                {
                    *p++ = '[';
                    *p++ = '?';  /* placeholder */
                    if (insn->aops[n].offset != 0)
                        p += sprintf(p, "+0x%x", insn->aops[n].offset);
                    *p++ = ']';
                }
                else
                {
                    *p++ = '?';  /* placeholder */
                    if (insn->aops[n].offset != 0)
                        p += sprintf(p, "+0x%x", insn->aops[n].offset);
                }
            }
            else
            {
                if (insn->opcode == I_LEA)
                    ;
                else if (type & A_BITS8)
                    p += sprintf(p, "%s ", special_names[S_BYTE]);
                else if (type & A_BITS16)
                    p += sprintf(p, "%s ", special_names[S_WORD]);
                else if (type & A_BITS32)
                    p += sprintf(p, "%s ", special_names[S_DWORD]);
                else if (type & A_BITS64)
                    p += sprintf(p, "%s ", special_names[S_QWORD]);
                else if (type & A_BITS80)
                    p += sprintf(p, "%s ", special_names[S_TBYTE]);

                p += sprintf(p, "0x%x", insn->aops[n].offset);
            }
        }
        else if (!(A_MEMORY & ~type))
        {
            int b = insn->aops[n].basereg;
            int i = insn->aops[n].indexreg;
            int s = insn->aops[n].scale;
            int m;

            if (sym != NULL)
            {
                if (sym->sclass != STATIC && sym->sclass != EXTERN &&
                   (sym->sclass != AUTO || b != -1))
                {
                    apperror(RCERROR(ERROR_INVALID_EA_ADDR));
                    continue;
                }

                /* use explicit size or deduce from symbol */
                if (insn->opcode == I_LEA)
                    ;
                else if (type & A_BITS8)
                    p += sprintf(p, "%s ", special_names[S_BYTE]);
                else if (type & A_BITS16)
                    p += sprintf(p, "%s ", special_names[S_WORD]);
                else if (type & A_BITS32)
                    p += sprintf(p, "%s ", special_names[S_DWORD]);
                else if (type & A_BITS64)
                    p += sprintf(p, "%s ", special_names[S_QWORD]);
                else if (type & A_BITS80)
                    p += sprintf(p, "%s ", special_names[S_TBYTE]);
                else if (sym->type == NULL || isptr(sym->type) || isstruct(sym->type))
                    ;
                else if (sym->type->size == 1 || isarray(sym->type) && sym->type->type->size == 1)
                    p += sprintf(p, "%s ", special_names[S_BYTE]);
                else if (sym->type->size == 2 || isarray(sym->type) && sym->type->type->size == 2)
                    p += sprintf(p, "%s ", special_names[S_WORD]);
                else if (sym->type->size == 4 || isarray(sym->type) && sym->type->type->size == 4)
                    p += sprintf(p, "%s ", special_names[S_DWORD]);
                else if (sym->type->size == 8 || isarray(sym->type) && sym->type->type->size == 8)
                    p += sprintf(p, "%s ", special_names[S_QWORD]);
                else if (sym->type->size != 0)
                    apperror(RCERROR(ERROR_INVALID_EA_ADDR));
            }
            else
            {
                if (insn->opcode == I_LEA)
                    ;
                else if (type & A_BITS8)
                    p += sprintf(p, "%s ", special_names[S_BYTE]);
                else if (type & A_BITS16)
                    p += sprintf(p, "%s ", special_names[S_WORD]);
                else if (type & A_BITS32)
                    p += sprintf(p, "%s ", special_names[S_DWORD]);
                else if (type & A_BITS64)
                    p += sprintf(p, "%s ", special_names[S_QWORD]);
                else if (type & A_BITS80)
                    p += sprintf(p, "%s ", special_names[S_TBYTE]);
            }

            *p++ = '[';

            /* segment overrides */
            for (m = 0; m < insn->nprefix; m++)
            {
                switch (insn->prefixes[m])
                {
                    case R_CS:
                    case R_DS:
                    case R_ES:
                    case R_FS:
                    case R_GS:
                    case R_SS:
                        p += sprintf(p, "%s:", reg_names[insn->prefixes[m] - EXPR_REG_START]);
                        break;
                }
            }

            if (b != -1)
                p += sprintf(p, "%s+", reg_names[b - EXPR_REG_START]);

            if (i != -1 && s != 0)
                p += sprintf(p, "%s*%u+", reg_names[i - EXPR_REG_START], s);
            else if (i != -1 && s == 0)
                p += sprintf(p, "%s+", reg_names[i - EXPR_REG_START]);

            if (sym != NULL)
            {
                *p++ = '?';  /* placeholder */
                *p++ = '+';
            }

            if (insn->aops[n].offset != 0)
                p += sprintf(p, "0x%x", insn->aops[n].offset);
            else if (p[-1] == '+')
                p--;

            *p++ = ']';
        }
        else assert(0);
    }

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
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int asmreg(const char *s)
{
    return lookup(s, reg_names);
}
