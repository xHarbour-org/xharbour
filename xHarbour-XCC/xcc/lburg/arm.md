%{
/****************************************************************************
 *                                                                          *
 * File    : arm.md                                                         *
 *                                                                          *
 * Purpose : StrongARM machine description for Win32 (WinCE).               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           02-01-27  Extensive modifications for better code quality.     *
 *           02-01-27  Support for CRT function _alloca() added.            *
 *           03-09-14  Floating-point support added.                        *
 *           03-09-18  Changed interface flag mulops_calls into function.   *
 *           04-06-25  Bugfix: loading and storing doubles were wrong.      *
 *           04-07-22  Simplified and improved argument and local handling. *
 *           04-10-24  Bugfix: use addri for ldrb and strb instructions.    *
 *           04-11-24  64-bit long long support added (from 32-bit).        *
 *           04-11-26  Moved peephole patterns here.                        *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#define I(f) arm_##f

// #define USE_MEMCPY

/* rough instruction count: ~500 word * 4 = ~1 kB */
#define FLUSH_LIMIT  500

/*
 * NOTES:
 *
 * 1. ip is used as a temporary register.
 *
 * 2. r0-r3 and ip may be (both senses) destroyed by functions.
 *
 * 3. Use of ldr{h|sb|sh} and str{h|sb|sh} requires StrongARM or higher.
 *
 * 4. No debug support (only line numbers).
 *
 * 5. We have __u64tos() and __u64tod(), but the front-end will
 *    always generate a conversion expression - we can't use them!
 */

/*
 * r0     - argument 1, return value, temporary
 * r1     - argument 2, second 32-bit if double/int return value, temporary
 * r2-r3  - arguments, temporaries
 * r4-r10 - permanent registers
 * r11    - (fp) frame pointer, permanent
 * r12    - (ip) temporary
 * r13    - (sp) stack pointer, permanent
 * r14    - (lr) link register, permanent
 * r15    - (pc) program counter
 */

/*
 * Win CE differences from ARM's Procedure Calling Standard:
 *
 * 1. Windows CE uses R9 and R10 as callee saved registers, and not as a static
 *    base and a stack limit register.
 *
 * 2. ARM’s compiler saves register arguments in the callee’s stack frame after
 *    the stack, link register, frame, and permanent registers, and passes
 *    additional arguments beginning at the start of the caller’s stack frame.
 *    This allows saving all the registers with one instruction in the prolog,
 *    but the resulting stack cannot support varargs as mentioned; arguments
 *    must be stored differently when varargs is used or when structures need
 *    to be passed partially in registers.
 *
 * 3. ARM’s compiler uses a frame pointer by default and pushes outgoing arguments
 *    on the stack frame. Except when alloca and/or SEH are used, clarm and clthumb
 *    use a fixed stack and no frame pointer, and arguments are not pushed.
 *
 * 4. ARM’s compiler returns some 4-byte structures in R0, clarm and clthumb
 *    always returns structures in the caller’s stack space which is pointed to
 *    by the first argument.
 */

enum { ARM_R0 = 0, ARM_R1 = 1, ARM_R2 = 2, ARM_R3 = 3, ARM_R4 = 4, ARM_R5 = 5,
       ARM_R6 = 6, ARM_R7 = 7, ARM_R8 = 8, ARM_R9 = 9, ARM_R10 = 10,
       ARM_FP = 11, ARM_IP = 12, ARM_SP = 13, ARM_LR = 14, ARM_PC = 15, ARM_DBLSTK = 16 };

enum { RT_MCPY = 0, RT_SDIV, RT_SDIV64, RT_UDIV, RT_UDIV64, RT_SMOD64, RT_UMOD64, RT_SRSH64,
       RT_DTOS, RT_STOD, RT_STOI, RT_STOI64, RT_DTOI, RT_DTOI64, RT_ITOS, RT_I64TOS, RT_ITOD, RT_I64TOD,
       RT_NEGS, RT_NEGD, RT_ADDS, RT_ADDD, RT_SUBS, RT_SUBD, RT_DIVS, RT_DIVD, RT_MULS, RT_MULD,
       RT_EQS, RT_EQD, RT_GES, RT_GED, RT_GTS, RT_GTD, RT_LES, RT_LED, RT_LTS, RT_LTD, RT_NES, RT_NED,
       RT_MAXCOUNT
};

typedef struct _OUTOFLINE {
    SYMBOL *sym;
    int lab;
} OUTOFLINE;

#define INTTMP  (1<<ARM_R1|1<<ARM_R2|1<<ARM_R3|1<<ARM_R4|1<<ARM_R5|1<<ARM_R6|1<<ARM_R7|1<<ARM_R8|1<<ARM_R9|1<<ARM_R10|1<<ARM_IP)
#define INTVAR  (1<<ARM_R7|1<<ARM_R8|1<<ARM_R9|1<<ARM_R10)

#define INTVARLEAF  (1<<ARM_R1|1<<ARM_R2|1<<ARM_R3)

#define INTSAVE  (1<<ARM_R4|1<<ARM_R5|1<<ARM_R6|1<<ARM_R7|1<<ARM_R8|1<<ARM_R9|1<<ARM_R10)

#define INTRET  (1<<ARM_R0)
#define INTCLOBBER  (1<<ARM_R1|1<<ARM_R2|1<<ARM_R3|1<<ARM_IP)

#define INTRET2  (1<<ARM_R0|1<<ARM_R1)
#define INTCLOBBER2  (1<<ARM_R2|1<<ARM_R3|1<<ARM_IP)

#define NODEPTR_TYPE NODE*
#define OP_LABEL(p) ((p)->op)
#define LEFT_CHILD(p) ((p)->kids[0])
#define RIGHT_CHILD(p) ((p)->kids[1])
#define STATE_LABEL(p) ((p)->x.state)

/* private segment IDs */
#define FCNRECS  98
#define XCPTRECS  97
#define INITFN  96
#define EXITFN  95

static void fatal(const char *fcn, const char *fmt, int arg)
{
    char buf1[100];
    char buf2[200];

    sprintf(buf1, fmt, arg);
    sprintf(buf2, "function %s: %s", fcn, buf1);
    apperror(RCFATAL(ERROR_INTERNAL), buf2);
}

/* Static function prototypes */
static int offrange(NODE *p, int, int);
static int imm12(NODE *, int);
static SYMBOL *regmap(int);
static void target(NODE *);
static void clobber(NODE *);
static void doarg(NODE *);
static void emit2(NODE *);
static void store_argument(int, int);
static void blkfetch(int, int, int, int);
static void blkstore(int, int, int, int);
static void blkloop(int, int, int, int, int, int[]);
#ifdef USE_MEMCPY
static void memcopy(int, int, int, int, int);
#endif
static void I(progbeg)(void);
static void I(progend)(void);
static void finalize_initexit(SYMBOL *, void *);
static void I(function)(SYMBOL *, SYMBOL *[], SYMBOL *[], int);
static void function_seh(SYMBOL *);
static void function_seh_unnest(LIST *);
static void I(defsymbol)(SYMBOL *);
static void I(address)(SYMBOL *, SYMBOL *, long);
static void I(defaddress)(SYMBOL *);
static void I(defconst)(int, int, VALUE);
static void I(defstring)(int, char *);
static void I(export)(SYMBOL *);
static void I(import)(SYMBOL *);
static void I(global)(SYMBOL *);
static void I(local)(SYMBOL *);
static void I(segment)(int);
static void I(space)(int);
static void I(sehbeg)(int, SEH *);
static void I(sehend)(int, SEH *);
static void I(unwind)(SEH *);
static int I(opcall)(int);
static void I(dbgline)(COORDINATE *);
static void movcon(int, int);
static void addcon(int, int, int);
static SYMBOL *argreg(int, int);
static const char *reglist(uint_t);
static int newcnst(SYMBOL *);
static void flushcnst(bool_t);
static void flushcnst_with_branch(void);

/* Register information */
static SYMBOL *intreg[32], *intreg2[32], *dreg12, *dreg3X;
static SYMBOL *intregw, *intreg2w;
#ifndef USE_MEMCPY
static SYMBOL *blkreg;
static int tmpregs[] = { ARM_LR, ARM_R5, ARM_R6 };
#endif

static LIST *cnstlist;  /* current out-of-line constants */
static int cseg;        /* current segment */
static int argstack;    /* sp offset to stack arguments (arg #5 and above) */
static bool_t noresb;
static bool_t hasfp;    /* use frame pointer (fp) */
static char rtimp[RT_MAXCOUNT];
static int nelems = 0;

/*
 * ARM nonterminals
 *
 * NAME         WHAT IT MATCHES
 * ----------------------------
 * reg          computations that yield a result in a register
 * stmt         computations done for side-effect
 * con          constants
 * con8         constants which can be used by short datatrans instructions (8 bits) - LDRH and STRH
 * con12        constants which can be used by datatrans instructions (12 bits) - LDR and STR
 * neg12        con12 with negative value - MVN
 * addr         address calculations for instructions that read and write memory
 * addri        address calculations for shorts (8 bits)
 * rc           registers and constants
 * rc5          registers and constants that fit in 5 bits
 * arc          registers, constants, register shifts for datatrans instructions
 * alab         labels
 */

%}
%start stmt

%term CNSTF4=4113
%term CNSTF8=8209
%term CNSTI1=1045
%term CNSTI2=2069
%term CNSTI4=4117
%term CNSTI8=8213
%term CNSTP4=4119
%term CNSTU1=1046
%term CNSTU2=2070
%term CNSTU4=4118
%term CNSTU8=8214

%term ARGB=41
%term ARGF4=4129
%term ARGF8=8225
%term ARGI4=4133
%term ARGI8=8229
%term ARGP4=4135
%term ARGU4=4134
%term ARGU8=8230

%term ASGNB=57
%term ASGNF4=4145
%term ASGNF8=8241
%term ASGNI1=1077
%term ASGNI2=2101
%term ASGNI4=4149
%term ASGNI8=8245
%term ASGNP4=4151
%term ASGNU1=1078
%term ASGNU2=2102
%term ASGNU4=4150
%term ASGNU8=8246

%term INDIRB=73
%term INDIRF4=4161
%term INDIRF8=8257
%term INDIRI1=1093
%term INDIRI2=2117
%term INDIRI4=4165
%term INDIRI8=8261
%term INDIRP4=4167
%term INDIRU1=1094
%term INDIRU2=2118
%term INDIRU4=4166
%term INDIRU8=8262

%term CVFF4=4209
%term CVFF8=8305
%term CVFI4=4213
%term CVFI8=8309

%term CVIF4=4225
%term CVIF8=8321
%term CVII1=1157
%term CVII2=2181
%term CVII4=4229
%term CVII8=8325
%term CVIU1=1158
%term CVIU2=2182
%term CVIU4=4230
%term CVIU8=8326

%term CVPU4=4246

%term CVUI1=1205
%term CVUI2=2229
%term CVUI4=4277
%term CVUI8=8373
%term CVUP4=4279
%term CVUU1=1206
%term CVUU2=2230
%term CVUU4=4278
%term CVUU8=8374

%term NEGF4=4289
%term NEGF8=8385
%term NEGI4=4293
%term NEGI8=8389

%term CALLB=217
%term CALLF4=4305
%term CALLF8=8401
%term CALLI4=4309
%term CALLI8=8405
%term CALLP4=4311
%term CALLU4=4310
%term CALLU8=8406
%term CALLV=216

%term RETF4=4337
%term RETF8=8433
%term RETI4=4341
%term RETI8=8437
%term RETP4=4343
%term RETU4=4342
%term RETU8=8438
%term RETV=248

%term ADDRGP4=4359

%term ADDRFP4=4375

%term ADDRLP4=4391

%term ADDF4=4401
%term ADDF8=8497
%term ADDI4=4405
%term ADDI8=8501
%term ADDP4=4407
%term ADDU4=4406
%term ADDU8=8502

%term SUBF4=4417
%term SUBF8=8513
%term SUBI4=4421
%term SUBI8=8517
%term SUBP4=4423
%term SUBU4=4422
%term SUBU8=8518

%term LSHI4=4437
%term LSHI8=8533
%term LSHU4=4438
%term LSHU8=8534

%term MODI4=4453
%term MODI8=8549
%term MODU4=4454
%term MODU8=8550

%term RSHI4=4469
%term RSHI8=8565
%term RSHU4=4470
%term RSHU8=8566

%term BANDI4=4485
%term BANDI8=8581
%term BANDU4=4486
%term BANDU8=8582

%term BCOMI4=4501
%term BCOMI8=8597
%term BCOMU4=4502
%term BCOMU8=8598

%term BORI4=4517
%term BORI8=8613
%term BORU4=4518
%term BORU8=8614

%term BXORI4=4533
%term BXORI8=8629
%term BXORU4=4534
%term BXORU8=8630

%term DIVF4=4545
%term DIVF8=8641
%term DIVI4=4549
%term DIVI8=8645
%term DIVU4=4550
%term DIVU8=8646

%term MULF4=4561
%term MULF8=8657
%term MULI4=4565
%term MULI8=8661
%term MULU4=4566
%term MULU8=8662

%term EQF4=4577
%term EQF8=8673
%term EQI4=4581
%term EQI8=8677
%term EQU4=4582
%term EQU8=8678

%term GEF4=4593
%term GEF8=8689
%term GEI4=4597
%term GEI8=8693
%term GEU4=4598
%term GEU8=8694

%term GTF4=4609
%term GTF8=8705
%term GTI4=4613
%term GTI8=8709
%term GTU4=4614
%term GTU8=8710

%term LEF4=4625
%term LEF8=8721
%term LEI4=4629
%term LEI8=8725
%term LEU4=4630
%term LEU8=8726

%term LTF4=4641
%term LTF8=8737
%term LTI4=4645
%term LTI8=8741
%term LTU4=4646
%term LTU8=8742

%term NEF4=4657
%term NEF8=8753
%term NEI4=4661
%term NEI8=8757
%term NEU4=4662
%term NEU8=8758

%term JUMPV=584

%term LABELV=600

%term CBOOLU1=1638

%term CEQI4=4725
%term CEQU4=4726

%term CGEI4=4741
%term CGEU4=4742

%term CGTI4=4757
%term CGTU4=4758

%term CLEI4=4773
%term CLEU4=4774

%term CLTI4=4789
%term CLTU4=4790

%term CNEI4=4805
%term CNEU4=4806

;; we don't support INTRIN1, INTRIN2, INTRIN1S, INTRIN2S (wants_intrinsic == 0)

%term LOADB=233
%term LOADF4=4321
%term LOADF8=8417
%term LOADI1=1253
%term LOADI2=2277
%term LOADI4=4325
%term LOADI8=8421
%term LOADP4=4327
%term LOADU1=1254
%term LOADU2=2278
%term LOADU4=4326
%term LOADU8=8422

%term VREGP=887
%%
stmt: reg  ""

reg: INDIRI1(VREGP)  "# read register\n"
reg: INDIRU1(VREGP)  "# read register\n"
reg: INDIRI2(VREGP)  "# read register\n"
reg: INDIRU2(VREGP)  "# read register\n"
reg: INDIRF4(VREGP)  "# read register\n"
reg: INDIRI4(VREGP)  "# read register\n"
reg: INDIRU4(VREGP)  "# read register\n"
reg: INDIRP4(VREGP)  "# read register\n"
reg: INDIRF8(VREGP)  "# read register\n"
reg: INDIRI8(VREGP)  "# read register\n"
reg: INDIRU8(VREGP)  "# read register\n"

stmt: ASGNI1(VREGP,reg)  "# write register\n"
stmt: ASGNU1(VREGP,reg)  "# write register\n"
stmt: ASGNI2(VREGP,reg)  "# write register\n"
stmt: ASGNU2(VREGP,reg)  "# write register\n"
stmt: ASGNF4(VREGP,reg)  "# write register\n"
stmt: ASGNI4(VREGP,reg)  "# write register\n"
stmt: ASGNU4(VREGP,reg)  "# write register\n"
stmt: ASGNP4(VREGP,reg)  "# write register\n"
stmt: ASGNF8(VREGP,reg)  "# write register\n"
stmt: ASGNI8(VREGP,reg)  "# write register\n"
stmt: ASGNU8(VREGP,reg)  "# write register\n"

con: CNSTI1  "%a"
con: CNSTU1  "%a"
con: CNSTI2  "%a"
con: CNSTU2  "%a"
con: CNSTI4  "%a"
con: CNSTU4  "%a"
con: CNSTP4  "%a"
con: CNSTF4  "%a"

con12: CNSTI1  "%a"  imm12(a,0)
con12: CNSTU1  "%a"  imm12(a,0)
con12: CNSTI2  "%a"  imm12(a,0)
con12: CNSTU2  "%a"  imm12(a,0)
con12: CNSTI4  "%a"  imm12(a,0)
con12: CNSTU4  "%a"  imm12(a,0)
con12: CNSTP4  "%a"  imm12(a,0)

neg12: CNSTI1  "%a"  imm12(a,1)
neg12: CNSTU1  "%a"  imm12(a,1)
neg12: CNSTI2  "%a"  imm12(a,1)
neg12: CNSTU2  "%a"  imm12(a,1)
neg12: CNSTI4  "%a"  imm12(a,1)
neg12: CNSTU4  "%a"  imm12(a,1)
neg12: CNSTP4  "%a"  imm12(a,1)

con8: CNSTI1  "%a"  range(a,-255,255)
con8: CNSTU1  "%a"  range(a,-255,255)
con8: CNSTI2  "%a"  range(a,-255,255)
con8: CNSTU2  "%a"  range(a,-255,255)
con8: CNSTI4  "%a"  range(a,-255,255)
con8: CNSTU4  "%a"  range(a,-255,255)
con8: CNSTP4  "%a"  range(a,-255,255)

;; address of global at %a.
acon: ADDRGP4  "#"

;; out-of-line constant.
mcon: con  "#"  10

;; address calculations for shorts (8 bits).
addri: reg  "%0,0"
addri: ADDI4(reg,arci)  "%0,%1"
addri: ADDU4(reg,arci)  "%0,%1"
addri: ADDP4(reg,arci)  "%0,%1"
addri: ADDI4(arci,reg)  "%1,%0"
addri: ADDU4(arci,reg)  "%1,%0"
addri: ADDP4(arci,reg)  "%1,%0"
addri: ADDRFP4  "#"               (hasfp ? LBURG_MAX : range(a,-255,255-32))
addri: ADDRFP4  "fp,(%a)"         (hasfp ? range(a,-255,255) : LBURG_MAX)
addri: ADDRFP4  "fp,((%a)+(%f))"  (hasfp ? offrange(a,-255,-1) : LBURG_MAX)
addri: ADDRLP4  "sp,((%a)+(%F))"  (hasfp ? LBURG_MAX : offrange(a,-255,255))
addri: ADDRLP4  "fp,((%a)+(%F))"  (hasfp ? offrange(a,-255,255) : LBURG_MAX)

;; address calculations for doubles.
addrd: reg  "%0,0"

;; address calculations for instructions that read and write memory.
addr: reg  "%0,0"
addr: ADDI4(reg,arc)  "%0,%1"
addr: ADDU4(reg,arc)  "%0,%1"
addr: ADDP4(reg,arc)  "%0,%1"
addr: ADDI4(arc,reg)  "%1,%0"
addr: ADDU4(arc,reg)  "%1,%0"
addr: ADDP4(arc,reg)  "%1,%0"
addr: ADDRFP4  "#"               (hasfp ? LBURG_MAX : range(a,-4095,4095))
addr: ADDRFP4  "fp,(%a)"         (hasfp ? range(a,-4095,4095) : LBURG_MAX)
addr: ADDRFP4  "fp,((%a)+(%F))"  (hasfp ? offrange(a,-4095,-1) : LBURG_MAX)
addr: ADDRLP4  "sp,((%a)+(%F))"  (hasfp ? LBURG_MAX : offrange(a,-4095,4095))
addr: ADDRLP4  "fp,((%a)+(%F))"  (hasfp ? offrange(a,-4095,4095) : LBURG_MAX)

;; important that "reg: addr" has higher cost that "reg: ADDRLP4".
reg: ADDRFP4  "#\n"  1
reg: ADDRLP4  "#\n"  1
reg: addr  "add %c,%0\n"  2
reg: con12  "mov %c,%0\n"  1
reg: neg12  "mvn %c,~(%0)\n"  1
reg: acon  "ldr %c,%0\n"  4
reg: mcon  "ldr %c,%0\n"  4
;; must load in *two* registers.
reg: CNSTI8  "#\n"
reg: CNSTU8  "#\n"
reg: CNSTF8  "#\n"

;; registers, constants for short datatrans instructions (04-10-24).
arci: reg  "%0"
arci: con8  "%0"

;; registers, constants, register shifts for datatrans instructions.
arc: reg  "%0"
arc: con12  "%0"
arc: LSHI4(reg,rc5)  " %0,lsl %1"
arc: LSHU4(reg,rc5)  " %0,lsl %1"
arc: RSHI4(reg,rc5)  " %0,asr %1"
arc: RSHU4(reg,rc5)  " %0,lsr %1"

;; registers and constants that fit in 5 bits.
rc5: CNSTI1  "%a"  range(a,1,31)
rc5: CNSTU1  "%a"  range(a,1,31)
rc5: CNSTI2  "%a"  range(a,1,31)
rc5: CNSTU2  "%a"  range(a,1,31)
rc5: CNSTI4  "%a"  range(a,1,31)
rc5: CNSTU4  "%a"  range(a,1,31)
rc5: CNSTP4  "%a"  range(a,1,31)
rc5: reg  "%0"

;; registers and constants.
rc: reg  "%0"
rc: LSHI4(reg,rc5)  " %0,lsl %1"
rc: LSHU4(reg,rc5)  " %0,lsl %1"
rc: RSHI4(reg,rc5)  " %0,asr %1"
rc: RSHU4(reg,rc5)  " %0,lsr %1"
rc: con12  "%0"

stmt: ASGNI1(addri,reg)  "strb %1,[%0]\n"  4
stmt: ASGNU1(addri,reg)  "strb %1,[%0]\n"  4
stmt: ASGNI2(addri,reg)  "strh %1,[%0]\n"  4
stmt: ASGNU2(addri,reg)  "strh %1,[%0]\n"  4
stmt: ASGNI4(addr,reg)  "str %1,[%0]\n"  4
stmt: ASGNU4(addr,reg)  "str %1,[%0]\n"  4
stmt: ASGNP4(addr,reg)  "str %1,[%0]\n"  4
stmt: ASGNF4(addr,reg)  "str %1,[%0]\n"  4
stmt: ASGNI8(addrd,reg)  "# ASGNI8\n"
stmt: ASGNU8(addrd,reg)  "# ASGNU8\n"
stmt: ASGNF8(addrd,reg)  "# ASGNF8\n"

;; avoid unnecessary move (04-08-06).
stmt: ASGNI1(addri,LOADI1(reg))  "strb %1,[%0]\n"  3
stmt: ASGNU1(addri,LOADU1(reg))  "strb %1,[%0]\n"  3
stmt: ASGNI2(addri,LOADI2(reg))  "strh %1,[%0]\n"  3
stmt: ASGNU2(addri,LOADU2(reg))  "strh %1,[%0]\n"  3

;; need INDIRB(addr) for dllimport of arrays ?
reg: INDIRB(addr)  "ldr %c,[%0]\n"  4
reg: INDIRI1(addri)  "ldrsb %c,[%0]\n"  4
reg: INDIRU1(addri)  "ldrb %c,[%0]\n"  4
reg: INDIRI2(addri)  "ldrsh %c,[%0]\n"  4
reg: INDIRU2(addri)  "ldrh %c,[%0]\n"  4
reg: INDIRI4(addr)  "ldr %c,[%0]\n"  4
reg: INDIRU4(addr)  "ldr %c,[%0]\n"  4
reg: INDIRP4(addr)  "ldr %c,[%0]\n"  4
reg: INDIRF4(addr)  "ldr %c,[%0]\n"  4
reg: INDIRI8(addrd)  "# INDIRI8\n"
reg: INDIRU8(addrd)  "# INDIRU8\n"
reg: INDIRF8(addrd)  "# INDIRF8\n"

reg: CVII4(reg)  "mov %c,%0,lsl 8*(4-%a)\nmov %c,%c,asr 8*(4-%a)\n"  2
reg: CVUU4(reg)  "and %c,%0,0xFF\n"  (a->syms[0]->u.c.v.i == 1 ? 1 : LBURG_MAX)
reg: CVUU4(reg)  "mov %c,%0,lsl 8*(4-%a)\nmov %c,%c,lsr 8*(4-%a)\n"  2
reg: CVUI4(reg)  "and %c,%0,0xFF\n"  (a->syms[0]->u.c.v.i == 1 ? 1 : LBURG_MAX)
reg: CVUI4(reg)  "mov %c,%0,lsl 8*(4-%a)\nmov %c,%c,lsr 8*(4-%a)\n"  2
reg: CVIU4(reg)  "mov %c,%0,lsl 8*(4-%a)\nmov %c,%c,asr 8*(4-%a)\n"  2
reg: CVPU4(reg)  "mov %c,%0\n"  move(a)
reg: CVUP4(reg)  "mov %c,%0\n"  move(a)
reg: CVFF4(reg)  "bl __dtos\n"  (rtimp[RT_DTOS] = 20)
reg: CVFF8(reg)  "bl __stod\n"  (rtimp[RT_STOD] = 20)
reg: CVFI4(reg)  "bl __stoi\n"  (a->syms[0]->u.c.v.i == 4 ? (rtimp[RT_STOI] = 20) : LBURG_MAX)
reg: CVFI4(reg)  "bl __dtoi\n"  (a->syms[0]->u.c.v.i == 8 ? (rtimp[RT_DTOI] = 20) : LBURG_MAX)
reg: CVFI8(reg)  "bl __stoi64\n"  (a->syms[0]->u.c.v.i == 4 ? (rtimp[RT_STOI64] = 20) : LBURG_MAX)
reg: CVFI8(reg)  "bl __dtoi64\n"  (a->syms[0]->u.c.v.i == 8 ? (rtimp[RT_DTOI64] = 20) : LBURG_MAX)
reg: CVIF4(reg)  "bl __itos\n"  (a->syms[0]->u.c.v.i == 4 ? (rtimp[RT_ITOS] = 20) : LBURG_MAX)
reg: CVIF4(reg)  "bl __i64tos\n"  (a->syms[0]->u.c.v.i == 8 ? (rtimp[RT_I64TOS] = 20) : LBURG_MAX)
reg: CVIF8(reg)  "bl __itod\n"  (a->syms[0]->u.c.v.i == 4 ? (rtimp[RT_ITOD] = 20) : LBURG_MAX)
reg: CVIF8(reg)  "bl __i64tod\n"  (a->syms[0]->u.c.v.i == 8 ? (rtimp[RT_I64TOD] = 20) : LBURG_MAX)

reg: CVII4(INDIRI1(addri))  "ldrsb %c,[%0]\n"  4
reg: CVII4(INDIRI2(addri))  "ldrsh %c,[%0]\n"  4
reg: CVUU4(INDIRU1(addri))  "ldrb %c,[%0]\n"  4
reg: CVUU4(INDIRU2(addri))  "ldrh %c,[%0]\n"  4
reg: CVIU4(INDIRI1(addri))  "ldrsb %c,[%0]\n"  4
reg: CVIU4(INDIRI2(addri))  "ldrsh %c,[%0]\n"  4
reg: CVUI4(INDIRU1(addri))  "ldrb %c,[%0]\n"  4
reg: CVUI4(INDIRU2(addri))  "ldrh %c,[%0]\n"  4

reg: CVII8(reg)  "# CVII8\n"
reg: CVIU8(reg)  "# CVIU8\n"
reg: CVUI8(reg)  "# CVUI8\n"
reg: CVUU8(reg)  "# CVUU8\n"

;; special _Bool conversion.
;; ( reg: CBOOLU1(reg)  "cmp %0,0\nmoveq %c,0\nmovne %c,1\n" )
reg: CBOOLU1(reg)  "# CBOOLU1\n"

reg: LOADI1(reg)  "mov %c,%0\n"  move(a)
reg: LOADU1(reg)  "mov %c,%0\n"  move(a)
reg: LOADI2(reg)  "mov %c,%0\n"  move(a)
reg: LOADU2(reg)  "mov %c,%0\n"  move(a)
reg: LOADI4(reg)  "mov %c,%0\n"  move(a)
reg: LOADU4(reg)  "mov %c,%0\n"  move(a)
reg: LOADP4(reg)  "mov %c,%0\n"  move(a)
reg: LOADF4(reg)  "mov %c,%0\n"  move(a)
reg: LOADI8(reg)  "# LOADI8\n"  move(a)
reg: LOADU8(reg)  "# LOADU8\n"  move(a)
reg: LOADF8(reg)  "# LOADF8\n"  move(a)

reg: NEGI4(reg)  "rsb %c,%0,0\n"  1
reg: NEGI8(reg)  "# NEGI8\n"
reg: NEGF4(reg)  "bl __negs\n"  (rtimp[RT_NEGS] = 20)
reg: NEGF8(reg)  "bl __negd\n"  (rtimp[RT_NEGD] = 20)

reg: CVFF4(NEGF8(CVFF8(reg)))  "bl __negs\n"  (rtimp[RT_NEGS] = 20)

reg: BCOMI4(reg)  "mvn %c,%0\n"  1
reg: BCOMU4(reg)  "mvn %c,%0\n"  1
reg: BCOMI8(reg)  "# BCOMI8\n"
reg: BCOMU8(reg)  "# BCOMU8\n"

reg: BANDI4(reg,rc)  "and %c,%0,%1\n"  1
reg: BANDU4(reg,rc)  "and %c,%0,%1\n"  1
reg: BANDI4(reg,neg12)  "bic %c,%0,~(%1)\n"  1
reg: BANDU4(reg,neg12)  "bic %c,%0,~(%1)\n"  1
reg: BORI4(reg,rc)  "orr %c,%0,%1\n"  1
reg: BORU4(reg,rc)  "orr %c,%0,%1\n"  1
reg: BXORI4(reg,rc)  "eor %c,%0,%1\n"  1
reg: BXORU4(reg,rc)  "eor %c,%0,%1\n"  1

reg: BANDI8(reg,reg)  "# BANDI8\n"
reg: BANDU8(reg,reg)  "# BANDU8\n"
reg: BORI8(reg,reg)  "# BORI8\n"
reg: BORU8(reg,reg)  "# BORU8\n"
reg: BXORI8(reg,reg)  "# BXORI8\n"
reg: BXORU8(reg,reg)  "# BXORU8\n"

reg: LSHI4(reg,rc5)  "mov %c,%0,lsl %1\n"  1
reg: LSHU4(reg,rc5)  "mov %c,%0,lsl %1\n"  1
reg: RSHI4(reg,rc5)  "mov %c,%0,asr %1\n"  1
reg: RSHU4(reg,rc5)  "mov %c,%0,lsr %1\n"  1

reg: LSHI8(reg,rc)  "# LSHI8\n"
reg: LSHU8(reg,rc)  "# LSHU8\n"
reg: RSHI8(reg,rc)  "bl __rt_srsh\n"  (rtimp[RT_SRSH64] = 20)
reg: RSHU8(reg,rc)  "# RSHU8\n"

reg: ADDI4(reg,rc)  "add %c,%0,%1\n"  1
reg: ADDU4(reg,rc)  "add %c,%0,%1\n"  1
reg: ADDP4(reg,rc)  "add %c,%0,%1\n"  1
reg: ADDI4(reg,neg12)  "sub %c,%0,0-(%1)\n"  1
reg: ADDU4(reg,neg12)  "sub %c,%0,0-(%1)\n"  1
reg: ADDP4(reg,neg12)  "sub %c,%0,0-(%1)\n"  1
reg: ADDF4(reg,reg)  "bl __adds\n"  (rtimp[RT_ADDS] = 20)
reg: ADDI8(reg,reg)  "# ADDI8\n"
reg: ADDU8(reg,reg)  "# ADDU8\n"
reg: ADDF8(reg,reg)  "bl __addd\n"  (rtimp[RT_ADDD] = 20)

reg: SUBI4(reg,rc)  "sub %c,%0,%1\n"  1
reg: SUBU4(reg,rc)  "sub %c,%0,%1\n"  1
reg: SUBP4(reg,rc)  "sub %c,%0,%1\n"  1
reg: SUBI4(reg,neg12)  "sub %c,%0,0-(%1)\n"  1
reg: SUBU4(reg,neg12)  "sub %c,%0,0-(%1)\n"  1
reg: SUBP4(reg,neg12)  "sub %c,%0,0-(%1)\n"  1
reg: SUBF4(reg,reg)  "bl __subs\n"  (rtimp[RT_SUBS] = 20)
reg: SUBI8(reg,reg)  "# SUBI8\n"
reg: SUBU8(reg,reg)  "# SUBU8\n"
reg: SUBF8(reg,reg)  "bl __subd\n"  (rtimp[RT_SUBD] = 20)

;; added 04-08-06.
reg: ADDU4(LOADU4(reg),LOADU4(reg))  "add %c,%0,%1\n"
reg: SUBU4(LOADU4(reg),LOADU4(reg))  "sub %c,%0,%1\n"

reg: MULI4(reg,reg)  "# MULI4\n"  10
reg: MULU4(reg,reg)  "# MULU4\n"  10
reg: MULF4(reg,reg)  "bl __muls\n"  (rtimp[RT_MULS] = 20)
reg: MULI8(reg,reg)  "# MULI8\n"  10
reg: MULU8(reg,reg)  "# MULU8\n"  10
reg: MULF8(reg,reg)  "bl __muld\n"  (rtimp[RT_MULD] = 20)

reg: DIVI4(reg,reg)  "bl __rt_sdiv\n"  (rtimp[RT_SDIV] = 20)
reg: DIVU4(reg,reg)  "bl __rt_udiv\n"  (rtimp[RT_UDIV] = 20)
reg: DIVF4(reg,reg)  "bl __divs\n"  (rtimp[RT_DIVS] = 20)
reg: DIVI8(reg,reg)  "bl __rt_sdiv64by64\n"  (rtimp[RT_SDIV64] = 20)
reg: DIVU8(reg,reg)  "bl __rt_udiv64by64\n"  (rtimp[RT_UDIV64] = 20)
reg: DIVF8(reg,reg)  "bl __divd\n"  (rtimp[RT_DIVD] = 20)

reg: MODI4(reg,reg)  "bl __rt_sdiv\n"  (rtimp[RT_SDIV] = 20)
reg: MODU4(reg,reg)  "bl __rt_udiv\n"  (rtimp[RT_UDIV] = 20)
reg: MODI8(reg,reg)  "bl __rt_srem64by64\n"  (rtimp[RT_SMOD64] = 20)
reg: MODU8(reg,reg)  "bl __rt_urem64by64\n"  (rtimp[RT_UMOD64] = 20)

stmt: LABELV  "%a:\n"
stmt: JUMPV(alab)  "# JUMPV\n"  3
stmt: JUMPV(reg)  "mov pc,%0\n"  3

stmt: EQI4(reg,rc)  "cmp %0,%1\nbeq %a\n"  4
stmt: EQU4(reg,rc)  "cmp %0,%1\nbeq %a\n"  4
stmt: GEI4(reg,rc)  "cmp %0,%1\nbge %a\n"  4
stmt: GEU4(reg,rc)  "cmp %0,%1\nbcs %a\n"  4
stmt: GTI4(reg,rc)  "cmp %0,%1\nbgt %a\n"  4
stmt: GTU4(reg,rc)  "cmp %0,%1\nbhi %a\n"  4
stmt: LEI4(reg,rc)  "cmp %0,%1\nble %a\n"  4
stmt: LEU4(reg,rc)  "cmp %0,%1\nbls %a\n"  4
stmt: LTI4(reg,rc)  "cmp %0,%1\nblt %a\n"  4
stmt: LTU4(reg,rc)  "cmp %0,%1\nbcc %a\n"  4
stmt: NEI4(reg,rc)  "cmp %0,%1\nbne %a\n"  4
stmt: NEU4(reg,rc)  "cmp %0,%1\nbne %a\n"  4

;; special versions of the above
stmt: EQI4(reg,neg12)  "cmn %0,0-(%1)\nbeq %a\n"  4
stmt: EQU4(reg,neg12)  "cmn %0,0-(%1)\nbeq %a\n"  4
stmt: GEI4(reg,neg12)  "cmn %0,0-(%1)\nbge %a\n"  4
stmt: GEU4(reg,neg12)  "cmn %0,0-(%1)\nbcs %a\n"  4
stmt: GTI4(reg,neg12)  "cmn %0,0-(%1)\nbgt %a\n"  4
stmt: GTU4(reg,neg12)  "cmn %0,0-(%1)\nbhi %a\n"  4
stmt: LEI4(reg,neg12)  "cmn %0,0-(%1)\nble %a\n"  4
stmt: LEU4(reg,neg12)  "cmn %0,0-(%1)\nbls %a\n"  4
stmt: LTI4(reg,neg12)  "cmn %0,0-(%1)\nblt %a\n"  4
stmt: LTU4(reg,neg12)  "cmn %0,0-(%1)\nbcc %a\n"  4
stmt: NEI4(reg,neg12)  "cmn %0,0-(%1)\nbne %a\n"  4
stmt: NEU4(reg,neg12)  "cmn %0,0-(%1)\nbne %a\n"  4

;; avoid loading temps before NULL compare (04-03-25)
stmt: EQU4(LOADU4(reg),con12)  "cmp %0,%1\nbeq %a\n"  4
stmt: NEU4(LOADU4(reg),con12)  "cmp %0,%1\nbne %a\n"  4

;; avoid loading temps before compare (04-03-25)
stmt: EQU4(LOADU4(reg),LOADU4(reg))  "cmp %0,%1\nbeq %a\n"  4
stmt: GEU4(LOADU4(reg),LOADU4(reg))  "cmp %0,%1\nbcs %a\n"  4
stmt: GTU4(LOADU4(reg),LOADU4(reg))  "cmp %0,%1\nbhi %a\n"  4
stmt: LEU4(LOADU4(reg),LOADU4(reg))  "cmp %0,%1\nbls %a\n"  4
stmt: LTU4(LOADU4(reg),LOADU4(reg))  "cmp %0,%1\nbcc %a\n"  4
stmt: NEU4(LOADU4(reg),LOADU4(reg))  "cmp %0,%1\nbne %a\n"  4

stmt: EQI8(reg,reg)  "#\n"
stmt: GEI8(reg,reg)  "#\n"
stmt: GTI8(reg,reg)  "#\n"
stmt: LEI8(reg,reg)  "#\n"
stmt: LTI8(reg,reg)  "#\n"
stmt: NEI8(reg,reg)  "#\n"
stmt: EQU8(reg,reg)  "#\n"
stmt: GEU8(reg,reg)  "#\n"
stmt: GTU8(reg,reg)  "#\n"
stmt: LEU8(reg,reg)  "#\n"
stmt: LTU8(reg,reg)  "#\n"
stmt: NEU8(reg,reg)  "#\n"

stmt: EQF4(reg,reg)  "bl __nes\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_NES] = 20)
stmt: EQF8(reg,reg)  "bl __ned\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_NED] = 20)
stmt: GEF4(reg,reg)  "bl __lts\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_LTS] = 20)
stmt: GEF8(reg,reg)  "bl __ltd\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_LTD] = 20)
stmt: GTF4(reg,reg)  "bl __les\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_LES] = 20)
stmt: GTF8(reg,reg)  "bl __led\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_LED] = 20)
stmt: LEF4(reg,reg)  "bl __gts\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_GTS] = 20)
stmt: LEF8(reg,reg)  "bl __gtd\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_GTD] = 20)
stmt: LTF4(reg,reg)  "bl __ges\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_GES] = 20)
stmt: LTF8(reg,reg)  "bl __ged\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_GED] = 20)
stmt: NEF4(reg,reg)  "bl __eqs\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_EQS] = 20)
stmt: NEF8(reg,reg)  "bl __eqd\nmovs r3,r0\nbeq %a\n"  (rtimp[RT_EQD] = 20)

reg: CEQI4(reg,rc)  "cmp %0,%1\nmoveq ip,1\nmovne ip,0\n"
reg: CEQU4(reg,rc)  "cmp %0,%1\nmoveq ip,1\nmovne ip,0\n"
reg: CGEI4(reg,rc)  "cmp %0,%1\nmovge ip,1\nmovlt ip,0\n"
reg: CGEU4(reg,rc)  "cmp %0,%1\nmovcs ip,1\nmovcc ip,0\n"
reg: CGTI4(reg,rc)  "cmp %0,%1\nmovgt ip,1\nmovle ip,0\n"
reg: CGTU4(reg,rc)  "cmp %0,%1\nmovhi ip,1\nmovls ip,0\n"
reg: CLEI4(reg,rc)  "cmp %0,%1\nmovle ip,1\nmovgt ip,0\n"
reg: CLEU4(reg,rc)  "cmp %0,%1\nmovls ip,1\nmovhi ip,0\n"
reg: CLTI4(reg,rc)  "cmp %0,%1\nmovlt ip,1\nmovge ip,0\n"
reg: CLTU4(reg,rc)  "cmp %0,%1\nmovcc ip,1\nmovcs ip,0\n"
reg: CNEI4(reg,rc)  "cmp %0,%1\nmovne ip,1\nmoveq ip,0\n"
reg: CNEU4(reg,rc)  "cmp %0,%1\nmovne ip,1\nmoveq ip,0\n"

alab: ADDRGP4  "%a"

;; special handling of the _alloca() function.
reg: CALLP4(alab)  "add r0,r0,3\nbic r0,r0,3\nsub sp,sp,r0\nmov r0,sp\n"  (a->syms[0]->u.c.v.i == 4 && a->kids[0]->syms[0] && strcmp(a->kids[0]->syms[0]->name, "_alloca") == 0 ? 0 : LBURG_MAX)

reg: CALLI4(alab)  "bl %0\n"  3
reg: CALLU4(alab)  "bl %0\n"  3
reg: CALLP4(alab)  "bl %0\n"  3
reg: CALLF4(alab)  "bl %0\n"  3
reg: CALLI8(alab)  "bl %0\n"  3
reg: CALLU8(alab)  "bl %0\n"  3
reg: CALLF8(alab)  "bl %0\n"  3
stmt: CALLV(alab)  "bl %0\n"  3
reg: CALLI4(reg)  "mov lr,pc\nmov pc,%0\n"  4
reg: CALLU4(reg)  "mov lr,pc\nmov pc,%0\n"  4
reg: CALLP4(reg)  "mov lr,pc\nmov pc,%0\n"  4
reg: CALLF4(reg)  "mov lr,pc\nmov pc,%0\n"  4
reg: CALLI8(reg)  "mov lr,pc\nmov pc,%0\n"  4
reg: CALLU8(reg)  "mov lr,pc\nmov pc,%0\n"  4
reg: CALLF8(reg)  "mov lr,pc\nmov pc,%0\n"  4
stmt: CALLV(reg)  "mov lr,pc\nmov pc,%0\n"  4

stmt: RETI4(reg)  "# ret\n"
stmt: RETU4(reg)  "# ret\n"
stmt: RETP4(reg)  "# ret\n"
stmt: RETF4(reg)  "# ret\n"
stmt: RETI8(reg)  "# ret\n"
stmt: RETU8(reg)  "# ret\n"
stmt: RETF8(reg)  "# ret\n"
stmt: RETV(reg)   "# ret\n"

stmt: ARGI4(reg)  "# arg\n"  4
stmt: ARGU4(reg)  "# arg\n"  4
stmt: ARGP4(reg)  "# arg\n"  4
stmt: ARGF4(reg)  "# arg\n"  4
stmt: ARGI8(reg)  "# arg\n"  4
stmt: ARGU8(reg)  "# arg\n"  4
stmt: ARGF8(reg)  "# arg\n"  4
stmt: ARGB(INDIRB(reg))  "# argb %0\n"  10
stmt: ASGNB(reg,INDIRB(reg))  "# asgnb %0 %1\n"  10

%%

/*
 * Generic peephole rules - make code smaller and/or faster.
 */
static const char *generic_rules[] = {
    "mov %1,%1", NULL,                                      NULL,
    "mov %1,%2", "mov %2,%1", NULL,                         "mov %1,%2", NULL,
    "str %1,$2", "ldr %1,$2", NULL,                         "str %1,$2", NULL,
    "and %1,$2", "cmp %1,0", NULL,                          "ands %1,$2", NULL,  /* 02-01-20 */
    "mov %1,$2", "cmp %1,0", NULL,                          "movs %1,$2", NULL,  /* 02-01-20 */
    "mov %1,%2", "cmp %2,0", NULL,                          "movs %1,%2", NULL,  /* 04-03-25 */
    "add %1,%2,0", NULL,                                    "mov %1,%2", NULL,   /* 04-07-22 (zero sp offset) */
    "beq $1", "mov %2,$3", "b $4", "$1:", NULL,             "movne %2,$3", "bne $4", "$1:", NULL,
    "bge $1", "mov %2,$3", "b $4", "$1:", NULL,             "movlt %2,$3", "blt $4", "$1:", NULL,
    "bgt $1", "mov %2,$3", "b $4", "$1:", NULL,             "movle %2,$3", "ble $4", "$1:", NULL,
    "ble $1", "mov %2,$3", "b $4", "$1:", NULL,             "movgt %2,$3", "bgt $4", "$1:", NULL,
    "blt $1", "mov %2,$3", "b $4", "$1:", NULL,             "movge %2,$3", "bge $4", "$1:", NULL,
    "bne $1", "mov %2,$3", "b $4", "$1:", NULL,             "moveq %2,$3", "beq $4", "$1:", NULL,
    /* added 04-03-25 */
    "bcs $1", "mov %2,$3", "b $4", "$1:", NULL,             "movcc %2,$3", "bcc $4", "$1:", NULL,
    "bhi $1", "mov %2,$3", "b $4", "$1:", NULL,             "movls %2,$3", "bls $4", "$1:", NULL,
    "bls $1", "mov %2,$3", "b $4", "$1:", NULL,             "movhi %2,$3", "bhi $4", "$1:", NULL,
    "bcc $1", "mov %2,$3", "b $4", "$1:", NULL,             "movcs %2,$3", "bcs $4", "$1:", NULL,
    /* added 01-12-31 */
    "beq $1", "b $2", "$1:", NULL,                          "bne $2", "$1:", NULL,
    "bge $1", "b $2", "$1:", NULL,                          "blt $2", "$1:", NULL,
    "bgt $1", "b $2", "$1:", NULL,                          "ble $2", "$1:", NULL,
    "ble $1", "b $2", "$1:", NULL,                          "bgt $2", "$1:", NULL,
    "blt $1", "b $2", "$1:", NULL,                          "bge $2", "$1:", NULL,
    "bne $1", "b $2", "$1:", NULL,                          "beq $2", "$1:", NULL,
    /* added 04-03-25 */
    "bcs $1", "b $2", "$1:", NULL,                          "bcc $2", "$1:", NULL,
    "bhi $1", "b $2", "$1:", NULL,                          "bls $2", "$1:", NULL,
    "bls $1", "b $2", "$1:", NULL,                          "bhi $2", "$1:", NULL,
    "bcc $1", "b $2", "$1:", NULL,                          "bcs $2", "$1:", NULL,
    /* added 02-01-05 (_Bool conversion) */
    "cmp %1,0", "moveq %1,0", "movne %1,1", NULL,           "cmp %1,0", "movne %1,1", NULL,
    /* help the inline assembler (note ';' in ldr instruction, but not in mov instruction) */
    "ldr %1,[%2];", NULL,                                   "mov %1,%2", NULL,
    /* added 02-02-23 */
    "mov %1,%1,lsl 8*(4-2)", "mov %1,%1,lsr 8*(4-2)", "mov %2,%1,lsl 8*(4-2)", NULL, "mov %2,%1,lsl 8*(4-2)", NULL,
    /* added 02-02-27 */
    "beq $1", "mov %2,$3", "$1:", NULL,                     "movne %2,$3", "$1:", NULL,
    "bge $1", "mov %2,$3", "$1:", NULL,                     "movlt %2,$3", "$1:", NULL,
    "bgt $1", "mov %2,$3", "$1:", NULL,                     "movle %2,$3", "$1:", NULL,
    "ble $1", "mov %2,$3", "$1:", NULL,                     "movgt %2,$3", "$1:", NULL,
    "blt $1", "mov %2,$3", "$1:", NULL,                     "movge %2,$3", "$1:", NULL,
    "bne $1", "mov %2,$3", "$1:", NULL,                     "moveq %2,$3", "$1:", NULL,
    /* added 04-03-25 */
    "bcs $1", "mov %2,$3", "$1:", NULL,                     "movcc %2,$3", "$1:", NULL,
    "bhi $1", "mov %2,$3", "$1:", NULL,                     "movls %2,$3", "$1:", NULL,
    "bls $1", "mov %2,$3", "$1:", NULL,                     "movhi %2,$3", "$1:", NULL,
    "bcc $1", "mov %2,$3", "$1:", NULL,                     "movcs %2,$3", "$1:", NULL,
    // added 04-07-22 (bitcount) */
    "ands %1,$2", "beq $3", "add %4,$5", "$3:", NULL,       "ands %1,$2", "addne %4,$5", "$3:", NULL,
    "ands %1,$2", "bne $3", "add %4,$5", "$3:", NULL,       "ands %1,$2", "addeq %4,$5", "$3:", NULL,
    "ands %1,$2", "beq $3", "sub %4,$5", "$3:", NULL,       "ands %1,$2", "subne %4,$5", "$3:", NULL,
    "ands %1,$2", "bne $3", "sub %4,$5", "$3:", NULL,       "ands %1,$2", "subeq %4,$5", "$3:", NULL,
    /* added 03-09-17 */
    "mov %3,%1", "mov %4,%2", "mov %1,%3", "mov %2,%4", NULL,  "mov %3,%1", "mov %4,%2", NULL,
    /* added 04-03-16 (clean up after dead code elimination) */
    "b $1", "$1:", NULL,                                    "$1:", NULL,
    NULL
};

/****************************************************************************
 *                                                                          *
 * Function: offrange                                                       *
 *                                                                          *
 * Purpose : Confirm that the offset is within the given bounds.            *
 *                                                                          *
 * Comment : We could use range() too, but this might be more obvious.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int offrange(NODE *p, int lo, int hi)
{
    SYMBOL *sym = p->syms[0];
    int offset;

    switch (specific(p->op))
    {
        case ADDRF+P: offset = framesize + (int)sym->x.offset + 32; break;  /* add fudge for saved regs */
        case ADDRL+P: offset = framesize + (int)sym->x.offset; break;
        default: assert(0);
    }

    return (offset >= lo && offset <= hi) ? 0 : LBURG_MAX;
}

/****************************************************************************
 *                                                                          *
 * Function: imm12                                                          *
 *                                                                          *
 * Purpose : Confirm that the constant fits in a datatrans instruction.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           04-11-25  Bugfix: always use 32-bit mask (uintmax_t -> uint_t).*
 *                                                                          *
 ****************************************************************************/

#ifndef __POCC__
/* MSVC has big problems here */
#pragma optimize("", off)
#endif

static int imm12(NODE *p, int negate)
{
    SYMBOL *sym = p->syms[0];
    uint_t n;
    int i;

    switch (specific(p->op))
    {
        case CNST+I: n = (uint_t)(negate ? ~sym->u.c.v.i : sym->u.c.v.i); break;
        case CNST+U: n = (uint_t)(negate ? ~sym->u.c.v.u : sym->u.c.v.u); break;
        case CNST+P: n = (uint_t)(negate ? (void *)~0 : sym->u.c.v.p); break;
        default: assert(0);
    }

    for (i = 0; i < 16; i++)
    {
        if (n <= 255) return 0;
        if (n & 3) break;
        n >>= 2;
    }

    return LBURG_MAX;
}

#ifndef __POCC__
#pragma optimize("", on)
#endif

/****************************************************************************
 *                                                                          *
 * Function: XINTERFACE::regmap                                             *
 *                                                                          *
 * Purpose : Map a data type to a register set.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           03-09-14  Floating-point support added.                        *
 *           04-11-24  64-bit long long support added.                      *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *regmap(int op)
{
    switch (optype(op))
    {
        case P: case B:
            return intregw;
        case I: case U: case F:
            return opsize(op) == 4 ? intregw : intreg2w;
        default:
            return NULL;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: XINTERFACE::target                                             *
 *                                                                          *
 * Purpose : Set target register for an operation.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           03-09-14  Floating-point support added.                        *
 *           04-03-08  Added operators CEQ, CGE, CGT, CLE, CLT, CNE.        *
 *           04-11-24  64-bit long long support added.                      *
 *                                                                          *
 ****************************************************************************/

static void target(NODE *p)
{
    assert(p);
    switch (p->op)
    {
        case CNST+F+sizeop(8):
            /* bugfix 04-12-29: need this for expressions like: return (a - a) / (b - b); */
            set_register(p, intreg2[ARM_R4]);  // ????
            break;

        case CEQ+I+sizeop(4):
        case CGE+I+sizeop(4):
        case CGT+I+sizeop(4):
        case CLE+I+sizeop(4):
        case CLT+I+sizeop(4):
        case CNE+I+sizeop(4):
        case CEQ+U+sizeop(4):
        case CGE+U+sizeop(4):
        case CGT+U+sizeop(4):
        case CLE+U+sizeop(4):
        case CLT+U+sizeop(4):
        case CNE+U+sizeop(4):
            set_register(p, intreg[ARM_IP]);
            break;

        case ARG+I+sizeop(8):
        case ARG+U+sizeop(8):
        case ARG+F+sizeop(8):
        case ARG+I+sizeop(4):
        case ARG+U+sizeop(4):
        case ARG+P+sizeop(4):
        case ARG+F+sizeop(4):
        {
            SYMBOL *rsym = argreg(p->x.argno, opsize(p->op));
            if (rsym != NULL)
            {
                target_register(p, 0, rsym);
                if ((rsym->x.regnode->mask & (1<<ARM_DBLSTK)) == 0)
                    p->op = LOAD + opkind(p->op);  /* see book */
                set_register(p, rsym);
            }
            break;
        }

        case CALL+I+sizeop(8):
        case CALL+U+sizeop(8):
        case CALL+F+sizeop(8):
            set_register(p, intreg2[ARM_R0]);
            break;

        case CALL+I+sizeop(4):
        case CALL+U+sizeop(4):
        case CALL+P+sizeop(4):
        case CALL+F+sizeop(4):
        case CALL+V:
            set_register(p, intreg[ARM_R0]);
            break;

        case RET+I+sizeop(8):
        case RET+U+sizeop(8):
        case RET+F+sizeop(8):
            target_register(p, 0, intreg2[ARM_R0]);
            p->kids[0]->x.registered = 1;
            break;

        case RET+I+sizeop(4):
        case RET+U+sizeop(4):
        case RET+P+sizeop(4):
        case RET+F+sizeop(4):
            target_register(p, 0, intreg[ARM_R0]);
            p->kids[0]->x.registered = 1;
            break;

        case NEG+F+sizeop(8):
            target_register(p, 0, intreg2[ARM_R0]);
            set_register(p, intreg2[ARM_R0]);
            break;

        case NEG+F+sizeop(4):
            target_register(p, 0, intreg[ARM_R0]);
            set_register(p, intreg[ARM_R0]);
            break;

        case DIV+I+sizeop(8):
        case DIV+U+sizeop(8):
        case MOD+I+sizeop(8):
        case MOD+U+sizeop(8):
        case ADD+F+sizeop(8):
        case SUB+F+sizeop(8):
        case MUL+F+sizeop(8):
        case DIV+F+sizeop(8):
            target_register(p, 0, intreg2[ARM_R0]);
            target_register(p, 1, intreg2[ARM_R2]);
            set_register(p, intreg2[ARM_R0]);
            break;

        case DIV+I+sizeop(4):
        case DIV+U+sizeop(4):
            target_register(p, 1, intreg[ARM_R0]);
            target_register(p, 0, intreg[ARM_R1]);
            set_register(p, intreg[ARM_R0]);
            break;

        case MOD+I+sizeop(4):
        case MOD+U+sizeop(4):
            target_register(p, 1, intreg[ARM_R0]);
            target_register(p, 0, intreg[ARM_R1]);
            set_register(p, intreg[ARM_R1]);
            break;

        case ADD+F+sizeop(4):
        case SUB+F+sizeop(4):
        case MUL+F+sizeop(4):
        case DIV+F+sizeop(4):
            target_register(p, 0, intreg[ARM_R0]);
            target_register(p, 1, intreg[ARM_R1]);
            set_register(p, intreg[ARM_R0]);
            break;

        case RSH+I+sizeop(8):
            target_register(p, 0, intreg2[ARM_R0]);
            target_register(p, 1, intreg[ARM_R2]);
            set_register(p, intreg2[ARM_R0]);
            break;

        case EQ+F+sizeop(8):
        case NE+F+sizeop(8):
        case GE+F+sizeop(8):
        case GT+F+sizeop(8):
        case LE+F+sizeop(8):
        case LT+F+sizeop(8):
            target_register(p, 0, intreg2[ARM_R0]);
            target_register(p, 1, intreg2[ARM_R2]);
            break;

        case EQ+F+sizeop(4):
        case NE+F+sizeop(4):
        case GE+F+sizeop(4):
        case GT+F+sizeop(4):
        case LE+F+sizeop(4):
        case LT+F+sizeop(4):
            target_register(p, 0, intreg[ARM_R0]);
            target_register(p, 1, intreg[ARM_R1]);
            break;

        case CVF+F+sizeop(8):
            target_register(p, 0, intreg[ARM_R0]);
            set_register(p, intreg2[ARM_R0]);
            break;

        case CVF+F+sizeop(4):
            target_register(p, 0, intreg2[ARM_R0]);
            set_register(p, intreg[ARM_R0]);
            break;

        case CVF+I+sizeop(4):
            target_register(p, 0, intreg[ARM_R0]);
            set_register(p, intreg[ARM_R0]);
            break;

        case CVF+I+sizeop(8):
        case CVI+F+sizeop(8):
            if (p->syms[0]->u.c.v.i == 4)
            {
                target_register(p, 0, intreg[ARM_R0]);
                set_register(p, intreg2[ARM_R0]);
            }
            else
            {
                assert(p->syms[0]->u.c.v.i == 8);
                target_register(p, 0, intreg2[ARM_R0]);
                set_register(p, intreg2[ARM_R0]);
            }
            break;

        case CVI+F+sizeop(4):
            if (p->syms[0]->u.c.v.i == 4)
            {
                target_register(p, 0, intreg[ARM_R0]);
                set_register(p, intreg[ARM_R0]);
            }
            else
            {
                assert(p->syms[0]->u.c.v.i == 8);
                target_register(p, 0, intreg2[ARM_R0]);
                set_register(p, intreg[ARM_R0]);
            }
            break;

#ifndef USE_MEMCPY
        case ASGN+B:
            target_register(p->kids[1], 0, blkreg);
            break;

        case ARG+B:
            target_register(p->kids[0], 0, blkreg);
            break;
#endif
    }
}

/****************************************************************************
 *                                                                          *
 * Function: XINTERFACE::clobber                                            *
 *                                                                          *
 * Purpose : Spill and reload busy registers.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           03-09-14  Floating-point support added.                        *
 *           04-11-24  64-bit long long support added.                      *
 *                                                                          *
 ****************************************************************************/

static void clobber(NODE *p)
{
    assert(p);
    switch (p->op)
    {
        case DIV+I+sizeop(4):
        case DIV+U+sizeop(4):
        case ADD+F+sizeop(4):
        case SUB+F+sizeop(4):
        case MUL+F+sizeop(4):
        case DIV+F+sizeop(4):
        case NEG+F+sizeop(4):
        case CALL+I+sizeop(4):
        case CALL+U+sizeop(4):
        case CALL+P+sizeop(4):
        case CALL+F+sizeop(4):
        case CVF+F+sizeop(4):
        case CVF+I+sizeop(4):
        case CVI+F+sizeop(4):
            spill(INTCLOBBER, IREG, p);
            break;

        case CALL+V:
        case EQ+F+sizeop(4):
        case NE+F+sizeop(4):
        case GE+F+sizeop(4):
        case GT+F+sizeop(4):
        case LE+F+sizeop(4):
        case LT+F+sizeop(4):
        case EQ+F+sizeop(8):
        case NE+F+sizeop(8):
        case GE+F+sizeop(8):
        case GT+F+sizeop(8):
        case LE+F+sizeop(8):
        case LT+F+sizeop(8):
            spill(INTCLOBBER|INTRET, IREG, p);
            break;

        case MOD+I+sizeop(4):  /* result in r1 */
        case MOD+U+sizeop(4):  /* result in r1 */
        case ADD+F+sizeop(8):
        case SUB+F+sizeop(8):
        case MUL+F+sizeop(8):
        case DIV+F+sizeop(8):
        case RSH+I+sizeop(8):
        case NEG+F+sizeop(8):
        case CVF+F+sizeop(8):
        case CVF+I+sizeop(8):
        case CVI+F+sizeop(8):
        case CALL+F+sizeop(8):
            spill(INTCLOBBER2, IREG, p);
            break;

        case MUL+I+sizeop(8):
        case MUL+U+sizeop(8):
        case LSH+I+sizeop(8):
        case LSH+U+sizeop(8):
            spill((1<<ARM_IP|1<<ARM_LR), IREG, p);
            break;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: XINTERFACE::doarg                                              *
 *                                                                          *
 * Purpose : Define a new argument to a function.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void doarg(NODE *p)
{
    switch (argoffset)
    {
        case  0: p->x.argno = 1; break;
        case  4: p->x.argno = 2; break;
        case  8: p->x.argno = 3; break;
        case 12: p->x.argno = 4; break;
        default: p->x.argno = 0; break;
    }
    p->syms[2] = intconst(mkparm(4 /*align*/, (int)p->syms[0]->u.c.v.i /*size*/) - 16);
}

/****************************************************************************
 *                                                                          *
 * Function: XINTERFACE::emit2                                              *
 *                                                                          *
 * Purpose : Escape hatch to generate machine specific code.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           02-01-27  Rewritten                                            *
 *           03-09-14  Floating-point support added.                        *
 *           03-09-19  Flush out-of-line constants after JUMP+V added.      *
 *           04-06-25  Flush out-of-line constants in more places added.    *
 *           04-06-25  Bugfix: loading and storing doubles were wrong.      *
 *           04-11-24  64-bit long long support added.                      *
 *           04-12-28  CBOOL operator will now handle long long's.          *
 *                                                                          *
 ****************************************************************************/

#define FLUSH_CONSTANTS(cl)  flushcnst_with_branch();

static void emit2(NODE *p)
{
    switch (p->op)
    {
        case CNST+I+sizeop(1):
        case CNST+I+sizeop(2):
        case CNST+I+sizeop(4):
        case CNST+U+sizeop(1):
        case CNST+U+sizeop(2):
        case CNST+U+sizeop(4):
        case CNST+P+sizeop(4):
        case CNST+F+sizeop(4):
        {
            /*
             * out-of-line constants are put right after the
             * functions body and referenced through pc + offset.
             */
            int lab = newcnst(p->syms[0]);
            print("[pc,(..?C%d-8-$)]", lab);
            break;
        }

        case CNST+I+sizeop(8):
        case CNST+U+sizeop(8):
        case CNST+F+sizeop(8):  /* bugfix 04-06-25 */
        {
            /*
             * out-of-line constants are put right after the
             * functions body and referenced through pc + offset.
             */
            int dst = register_number(p);
            int lab = newcnst(p->syms[0]);
            print("ldr %s,[pc,(..?C%d-8-$)]\n", intreg[dst]->x.name, lab);
            print("ldr %s,[pc,(..?C%d-8-$)+4]\n", intreg[dst+1]->x.name, lab);
            break;
        }

        case ADDRG+P+sizeop(4):
        {
            /*
             * address of global pointer is referenced indirectly
             * through dd-constant after the functions body.
             */
            int lab = newcnst(p->syms[0]);
            print("[pc,(..?C%d-8-$)]", lab);
            break;
        }

        case ADDRF+P+sizeop(4):
            /*
             * address of formal parameter.
             */
            if (!p->syms[RX] || !p->syms[RX]->x.regnode)
            {
                assert(!hasfp);
                if (p->syms[0]->x.offset >= 0)
                    print("sp,%d", argstack + p->syms[0]->x.offset);
                else  /* for saving incoming args */
                    print("sp,%d", framesize + p->syms[0]->x.offset);
                break;
            }
            else if (p->syms[0]->x.offset >= 0)
            {
                if (hasfp)
                    addcon(register_number(p), ARM_FP, p->syms[0]->x.offset);
                else
                    addcon(register_number(p), ARM_SP, argstack + p->syms[0]->x.offset);
                FLUSH_CONSTANTS(cnstlist);
                break;
            }
            /* fall through */

        case ADDRL+P+sizeop(4):
            /*
             * address of local pointer.
             */
            if (hasfp)
                addcon(register_number(p), ARM_FP, framesize + p->syms[0]->x.offset);
            else
                addcon(register_number(p), ARM_SP, framesize + p->syms[0]->x.offset);
            FLUSH_CONSTANTS(cnstlist);
            break;

        case ARG+I+sizeop(4):
        case ARG+U+sizeop(4):
        case ARG+P+sizeop(4):
        case ARG+F+sizeop(4):
            /*
             * normal arg: target() will have arranged for those passed in registers
             * to be in regs, so we only have to save other args onto the stack.
             */
            if (!argreg(p->x.argno, opsize(p->op)))
            {
                int src = register_number(p->x.kids[0]);
                store_argument(src, (int)p->syms[2]->u.c.v.i);
                FLUSH_CONSTANTS(cnstlist);
            }
            break;

        case ARG+I+sizeop(8):
        case ARG+U+sizeop(8):
        case ARG+F+sizeop(8):
        {
            /*
             * double arg: target() will have arranged for those passed in registers
             * to be in regs, so we only have to save other args onto the stack.
             */
            SYMBOL *rsym = argreg(p->x.argno, opsize(p->op));
            if (!rsym)
            {
                int src = register_number(p->x.kids[0]);
                store_argument(src, (int)p->syms[2]->u.c.v.i);
                store_argument(src+1, (int)p->syms[2]->u.c.v.i + 4);
                FLUSH_CONSTANTS(cnstlist);
            }
            else if (rsym->x.regnode->mask & (1<<ARM_DBLSTK))
            {
                /* store upper half of incoming double in outgoing build area */
                int src = register_number(p->x.kids[0]);
                store_argument(src+1, (int)p->syms[2]->u.c.v.i);  /* store high word */
                FLUSH_CONSTANTS(cnstlist);
            }
            break;
        }

        case ARG+B:
        {
            /*
             * structure argument: some parts may be passed in r0-r3, the rest
             * on the stack. blkcopy() requires a non-negative size, so check that.
             */
            int size = (int)p->syms[0]->u.c.v.i;  /* size */
            int offset = (int)p->syms[2]->u.c.v.i;  /* offset */
            if (offset < 0)
            {
                /* in registers, maybe on stack too */
                int lim = offset + size;
                int cnt = offset;
                int dst, off;

                /* in registers */
                for (off = 0, dst = ARM_R0; cnt < 0 && cnt < lim; cnt += 4, off += 4, dst++)
                    print("ldr %s,[%s,%d]\n", intreg[dst]->x.name, intreg[register_number(p->x.kids[0])]->x.name, off);

                /* on stack too? */
                if ((size += offset) > 0)  /* offset is negative! */
                {
                    dalign = 4; salign = (int)p->syms[1]->u.c.v.i;  /* alignment */
#ifdef USE_MEMCPY
                    memcopy(ARM_SP, 0, register_number(p->x.kids[0]), off, size);
#else
                    blkcopy(ARM_SP, 0, register_number(p->x.kids[0]), off, size, tmpregs);
#endif
                }
            }
            else
            {
                if (size > 0)
                {
                    dalign = 4; salign = (int)p->syms[1]->u.c.v.i;  /* alignment */

#ifdef USE_MEMCPY
                    memcopy(ARM_SP, (int)p->syms[2]->u.c.v.i, register_number(p->x.kids[0]), 0, size);
#else
                    blkcopy(ARM_SP, (int)p->syms[2]->u.c.v.i, register_number(p->x.kids[0]), 0, size, tmpregs);
#endif
                }
                else assert(0);
            }
            break;
        }

        case ASGN+B:
        {
            /*
             * structure assignment: simple copy.
             */
            int size = (int)p->syms[0]->u.c.v.i;  /* size */
            if (size > 0)
            {
                dalign = salign = (int)p->syms[1]->u.c.v.i;  /* alignment */
#ifdef USE_MEMCPY
                memcopy(register_number(p->x.kids[0]), 0, register_number(p->x.kids[1]), 0, size);
#else
                blkcopy(register_number(p->x.kids[0]), 0, register_number(p->x.kids[1]), 0, size, tmpregs);
#endif
            }
            else assert(0);
            break;
        }

        case ASGN+I+sizeop(8):
        case ASGN+U+sizeop(8):
        case ASGN+F+sizeop(8):  /* bugfix 04-06-25 */
            if (generic(p->kids[0]->op) != VREG)
            {
                /* rules will have arranged for str reg,[reg] - very important for +4 to work! */
                int dst = register_number(p->x.kids[0]);
                if (p->x.kids[1]->syms[RX]->x.regnode->mask & (1<<ARM_DBLSTK))
                {
                    /* store incoming double - half in r3, half on stack */
                    print("str %s,[%s]\n", intreg[ARM_R3]->x.name, intreg[dst]->x.name);
                    /* register lr should be unused */
                    print("ldr lr,[sp,%d]\n", argstack + 0);
                    print("str lr,[%s,4]\n", intreg[dst]->x.name);
                    usedmask[IREG] |= (1<<ARM_LR);
                }
                else
                {
                    int src = register_number(p->x.kids[1]);
                    print("str %s,[%s]\n", intreg[src]->x.name, intreg[dst]->x.name);
                    print("str %s,[%s,4]\n", intreg[src+1]->x.name, intreg[dst]->x.name);
                }
                FLUSH_CONSTANTS(cnstlist);
            }
            break;

        case LOAD+I+sizeop(8):
        case LOAD+U+sizeop(8):
        case LOAD+F+sizeop(8):
        {
            int dst = register_number(p);
            int src = register_number(p->x.kids[0]);
            print("mov %s,%s\n", intreg[dst]->x.name, intreg[src]->x.name);
            print("mov %s,%s\n", intreg[dst+1]->x.name, intreg[src+1]->x.name);
            FLUSH_CONSTANTS(cnstlist);
            break;
        }

        case INDIR+I+sizeop(8):
        case INDIR+U+sizeop(8):
        case INDIR+F+sizeop(8):  /* bugfix 04-06-25 */
            if (generic(p->kids[0]->op) != VREG)
            {
                /* rules will have arranged for str reg,[reg] - very important for +4 to work! */
                int dst = register_number(p);
                int src = register_number(p->x.kids[0]);
                print("ldr %s,[%s]\n", intreg[dst]->x.name, intreg[src]->x.name);
                print("ldr %s,[%s,4]\n", intreg[dst+1]->x.name, intreg[src]->x.name);
                FLUSH_CONSTANTS(cnstlist);
            }
            break;

        case MUL+I+sizeop(4):
        case MUL+U+sizeop(4):
        {
            /*
             * Fix the instruction "MUL <Rd>,<Rm>,<Rs>". ARM ARM says that specifying
             * the same register for <Rd> and <Rm> has unpredictable results.
             */
            int dst = register_number(p);
            int mul = register_number(p->x.kids[0]);
            int src = register_number(p->x.kids[1]);
            if (dst == mul)
            {
                int tmp = mul; mul = src; src = tmp;
                /* opcall() == 1 should assure this */
                assert(mul != src);
            }
            print("mul %s,%s,%s\n", intreg[dst]->x.name, intreg[mul]->x.name, intreg[src]->x.name);
            break;
        }

        case MUL+I+sizeop(8):
        case MUL+U+sizeop(8):
        {
            int dst = register_number(p);
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            print("mul ip,%s,%s\n", intreg[op1]->x.name, intreg[op2+1]->x.name);
            print("mul lr,%s,%s\n", intreg[op1+1]->x.name, intreg[op2]->x.name);
            print("add ip,ip,lr\n");
            print("umull %s,%s,%s,%s\n", intreg[dst]->x.name, intreg[dst+1]->x.name, intreg[op1]->x.name, intreg[op2]->x.name);
            print("add %s,%s,ip\n", intreg[dst+1]->x.name, intreg[dst+1]->x.name);
            break;
        }

        case ADD+I+sizeop(8):
        case ADD+U+sizeop(8):
        {
            int dst = register_number(p);
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            print("adds %s,%s,%s\n", intreg[dst]->x.name, intreg[op1]->x.name, intreg[op2]->x.name);
            print("adc %s,%s,%s\n", intreg[dst+1]->x.name, intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            break;
        }

        case SUB+I+sizeop(8):
        case SUB+U+sizeop(8):
        {
            int dst = register_number(p);
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            print("subs %s,%s,%s\n", intreg[dst]->x.name, intreg[op1]->x.name, intreg[op2]->x.name);
            print("sbc %s,%s,%s\n", intreg[dst+1]->x.name, intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            break;
        }

        case BAND+I+sizeop(8):
        case BAND+U+sizeop(8):
        {
            int dst = register_number(p);
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            print("and %s,%s,%s\n", intreg[dst]->x.name, intreg[op1]->x.name, intreg[op2]->x.name);
            print("and %s,%s,%s\n", intreg[dst+1]->x.name, intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            break;
        }

        case BOR+I+sizeop(8):
        case BOR+U+sizeop(8):
        {
            int dst = register_number(p);
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            print("orr %s,%s,%s\n", intreg[dst]->x.name, intreg[op1]->x.name, intreg[op2]->x.name);
            print("orr %s,%s,%s\n", intreg[dst+1]->x.name, intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            break;
        }

        case BXOR+I+sizeop(8):
        case BXOR+U+sizeop(8):
        {
            int dst = register_number(p);
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            print("eor %s,%s,%s\n", intreg[dst]->x.name, intreg[op1]->x.name, intreg[op2]->x.name);
            print("eor %s,%s,%s\n", intreg[dst+1]->x.name, intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            break;
        }

        case LSH+I+sizeop(8):
        case LSH+U+sizeop(8):
        {
            /*
             * This required *much* head-scratching!
             * - dst might be same as src.
             * - IP can't be part of dst or src.
             * - only IP or LR will be destroyed after this.
             */
            int dst = register_number(p);
            int src = register_number(p->x.kids[0]);
            if (generic(p->kids[1]->op) == CNST)  /* shift value is constant */
            {
                uint_t sh = (uint_t)p->kids[1]->syms[0]->u.c.v.u;
                if (sh <= 32)
                {
                    print("mov %s,%s,lsl %u\n", intreg[dst+1]->x.name, intreg[src+1]->x.name, sh);
                    print("orr %s,%s,%s,lsr %u\n", intreg[dst+1]->x.name, intreg[dst+1]->x.name, intreg[src]->x.name, (32 - sh));
                    print("mov %s,%s,lsl %u\n", intreg[dst]->x.name, intreg[src]->x.name, sh);
                }
                else
                {
                    print("mov %s,%s,lsl %u\n", intreg[dst+1]->x.name, intreg[src]->x.name, (sh - 32));
                    print("mov %s,0\n", intreg[dst]->x.name);
                }
            }
            else
            {
                int sh = register_number(p->x.kids[1]);
                int tmp = (sh == ARM_IP) ? ARM_LR : ARM_IP;
                print("mov %s,%s,lsl %s\n", intreg[dst+1]->x.name, intreg[src+1]->x.name, intreg[sh]->x.name);
                print("rsb %s,%s,32\n", intreg[tmp]->x.name, intreg[sh]->x.name);
                print("orr %s,%s,%s,lsr %s\n", intreg[dst+1]->x.name, intreg[dst+1]->x.name, intreg[src]->x.name, intreg[tmp]->x.name);
                print("sub %s,%s,32\n", intreg[tmp]->x.name, intreg[sh]->x.name);
                print("orr %s,%s,%s,lsl %s\n", intreg[dst+1]->x.name, intreg[dst+1]->x.name, intreg[src]->x.name, intreg[tmp]->x.name);
                print("mov %s,%s,lsl %s\n", intreg[dst]->x.name, intreg[src]->x.name, intreg[sh]->x.name);
            }
            break;
        }

        case RSH+U+sizeop(8):
        {
            /*
             * This required *much* head-scratching too!
             * - dst might be same as src.
             * - IP can't be part of dst or src.
             * - only IP or LR will be destroyed after this.
             */
            int dst = register_number(p);
            int src = register_number(p->x.kids[0]);
            if (generic(p->kids[1]->op) == CNST)  /* shift value is constant */
            {
                uint_t sh = (uint_t)p->kids[1]->syms[0]->u.c.v.u;
                if (sh <= 32)
                {
                    print("mov %s,%s,lsr %u\n", intreg[dst]->x.name, intreg[src]->x.name, sh);
                    print("orr %s,%s,%s,lsl %u\n", intreg[dst]->x.name, intreg[dst]->x.name, intreg[src+1]->x.name, (32 - sh));
                    print("mov %s,%s,lsl %u\n", intreg[dst+1]->x.name, intreg[src+1]->x.name, sh);
                }
                else
                {
                    print("mov %s,%s,lsr %u\n", intreg[dst]->x.name, intreg[src+1]->x.name, (sh - 32));
                    print("mov %s,0\n", intreg[dst+1]->x.name);
                }
            }
            else
            {
                int sh = register_number(p->x.kids[1]);
                int tmp = (sh == ARM_IP) ? ARM_LR : ARM_IP;
                print("mov %s,%s,lsr %s\n", intreg[dst]->x.name, intreg[src]->x.name, intreg[sh]->x.name);
                print("rsb %s,%s,32\n", intreg[tmp]->x.name, intreg[sh]->x.name);
                print("orr %s,%s,%s,lsl %s\n", intreg[dst]->x.name, intreg[dst]->x.name, intreg[src+1]->x.name, intreg[tmp]->x.name);
                print("sub %s,%s,32\n", intreg[tmp]->x.name, intreg[sh]->x.name);
                print("orr %s,%s,%s,lsr %s\n", intreg[dst]->x.name, intreg[dst]->x.name, intreg[src+1]->x.name, intreg[tmp]->x.name);
                print("mov %s,%s,lsl %s\n", intreg[dst+1]->x.name, intreg[src+1]->x.name, intreg[sh]->x.name);
            }
            break;
        }

        case NEG+I+sizeop(8):
        {
            int dst = register_number(p);
            int src = register_number(p->x.kids[0]);
            print("rsbs %s,%s,0\n", intreg[dst]->x.name, intreg[src]->x.name);
            print("rsc %s,%s,0\n", intreg[dst+1]->x.name, intreg[src+1]->x.name);
            break;
        }

        case BCOM+I+sizeop(8):
        case BCOM+U+sizeop(8):
        {
            int dst = register_number(p);
            int src = register_number(p->x.kids[0]);
            print("mvn %s,%s\n", intreg[dst]->x.name, intreg[src]->x.name);
            print("mvn %s,%s\n", intreg[dst+1]->x.name, intreg[src+1]->x.name);
            break;
        }

        case EQ+I+sizeop(8):
        case EQ+U+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            int lab = make_label(1);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("bne @%d\n", lab);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("beq %s\n", p->syms[0]->x.name);
            print("@%d:\n", lab);
            break;
        }

        case NE+I+sizeop(8):
        case NE+U+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("bne %s\n", p->syms[0]->x.name);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("bne %s\n", p->syms[0]->x.name);
            break;
        }

        case GE+I+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            int lab = make_label(1);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("bgt %s\n", p->syms[0]->x.name);
            print("blt @%d\n", lab);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("bcs %s\n", p->syms[0]->x.name);
            print("@%d:\n", lab);
            break;
        }

        case GE+U+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            int lab = make_label(1);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("bhi %s\n", p->syms[0]->x.name);
            print("bcc @%d\n", lab);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("bcs %s\n", p->syms[0]->x.name);
            print("@%d:\n", lab);
            break;
        }

        case GT+I+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            int lab = make_label(1);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("bgt %s\n", p->syms[0]->x.name);
            print("blt @%d\n", lab);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("bhi %s\n", p->syms[0]->x.name);
            print("@%d:\n", lab);
            break;
        }

        case GT+U+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            int lab = make_label(1);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("bhi %s\n", p->syms[0]->x.name);
            print("bcc @%d\n", lab);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("bhi %s\n", p->syms[0]->x.name);
            print("@%d:\n", lab);
            break;
        }

        case LE+I+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            int lab = make_label(1);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("blt %s\n", p->syms[0]->x.name);
            print("bgt @%d\n", lab);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("bls %s\n", p->syms[0]->x.name);
            print("@%d:\n", lab);
            break;
        }

        case LE+U+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            int lab = make_label(1);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("bcc %s\n", p->syms[0]->x.name);
            print("bhi @%d\n", lab);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("bls %s\n", p->syms[0]->x.name);
            print("@%d:\n", lab);
            break;
        }

        case LT+I+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            int lab = make_label(1);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("blt %s\n", p->syms[0]->x.name);
            print("bgt @%d\n", lab);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("bcc %s\n", p->syms[0]->x.name);
            print("@%d:\n", lab);
            break;
        }

        case LT+U+sizeop(8):
        {
            int op1 = register_number(p->x.kids[0]);
            int op2 = register_number(p->x.kids[1]);
            int lab = make_label(1);
            print("cmp %s,%s\n", intreg[op1+1]->x.name, intreg[op2+1]->x.name);
            print("bcc %s\n", p->syms[0]->x.name);
            print("bhi @%d\n", lab);
            print("cmp %s,%s\n", intreg[op1]->x.name, intreg[op2]->x.name);
            print("bcc %s\n", p->syms[0]->x.name);
            print("@%d:\n", lab);
            break;
        }

        case CVI+I+sizeop(8):
        case CVI+U+sizeop(8):
        {
            int dst = register_number(p);
            int src = register_number(p->x.kids[0]);
            assert (opsize(p->x.kids[0]->op) == 4);
            /* sign extend */
            print("mov %s,%s\n", intreg[dst]->x.name, intreg[src]->x.name);
            print("mov %s,%s,asr 31\n", intreg[dst+1]->x.name, intreg[dst]->x.name);
            break;
        }

        case CVU+I+sizeop(8):
        case CVU+U+sizeop(8):
        {
            int dst = register_number(p);
            int src = register_number(p->x.kids[0]);
            assert (opsize(p->x.kids[0]->op) == 4);
            /* zero extend */
            print("mov %s,%s\n", intreg[dst]->x.name, intreg[src]->x.name);
            print("mov %s,0\n", intreg[dst+1]->x.name);
            break;
        }

        case JUMP+V:
            print("b %s\n", p->kids[0]->syms[0]->x.name);
            /* flush out-of-line constants */
            flushcnst(FALSE);
            break;


        case CBOOL+U+sizeop(1):
        {
            int src = register_number(p->x.kids[0]);
            if (opsize(p->x.kids[0]->op) == 8)
                print("orrs %s,%s,%s\nmovne %s,1\n", p->syms[RX]->x.name, intreg[src]->x.name, intreg[src+1]->x.name, p->syms[RX]->x.name);
            else /* 1, 2 or 4 */
                print("cmp %s,0\nmoveq %s,0\nmovne %s,1\n", intreg[src]->x.name, p->syms[RX]->x.name, p->syms[RX]->x.name);
            break;
        }

        default:
            break;
    }
}

#undef FLUSH_CONSTANTS

/****************************************************************************
 *                                                                          *
 * Function: store_argument                                                 *
 *                                                                          *
 * Purpose : Store outgoing argument in build area at given offset.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-22  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void store_argument(int src, int offset)
{
    if (hasfp)
    {
        /* the argument build area is always on top (bottom) of the stack */
        offset -= framesize;
        if (offset > -4096)
        {
            /* limited offset, fewer instructions */
            print("str %s,[fp,%d]\n", intreg[src]->x.name, offset);
        }
        else
        {
            /* arbitrary offset */
            addcon(ARM_LR, ARM_FP, offset);
            print("str %s,[lr,0]\n", intreg[src]->x.name);
            usedmask[IREG] |= (1<<ARM_LR);
        }
    }
    else
    {
        /* the argument build area is always on top (bottom) of the stack */
        if (offset < 4096)
        {
            /* limited offset, fewer instructions */
            print("str %s,[sp,%d]\n", intreg[src]->x.name, offset);
        }
        else
        {
            /* arbitrary offset */
            addcon(ARM_LR, ARM_SP, offset);
            print("str %s,[lr,0]\n", intreg[src]->x.name);
            usedmask[IREG] |= (1<<ARM_LR);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: XINTERFACE::blkfetch                                           *
 *                                                                          *
 * Purpose : Load register from the given cell.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void blkfetch(int size, int off, int reg, int tmp)
{
#ifndef USE_MEMCPY
    assert(size == 1 || size == 2 || size == 4);
    assert(salign >= size);

    if (size == 1)
        print("ldrb %s,[%s,%d]\n", intreg[tmp]->x.name, intreg[reg]->x.name, off);
    else if (size == 2)
        print("ldrh %s,[%s,%d]\n", intreg[tmp]->x.name, intreg[reg]->x.name, off);
    else
        print("ldr %s,[%s,%d]\n", intreg[tmp]->x.name, intreg[reg]->x.name, off);
#endif
}

/****************************************************************************
 *                                                                          *
 * Function: XINTERFACE::blkstore                                           *
 *                                                                          *
 * Purpose : Store register into the given cell.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void blkstore(int size, int off, int reg, int tmp)
{
#ifndef USE_MEMCPY
    assert(size == 1 || size == 2 || size == 4);
    assert(dalign >= size);

    if (size == 1)
        print("strb %s,[%s,%d]\n", intreg[tmp]->x.name, intreg[reg]->x.name, off);
    else if (size == 2)
        print("strh %s,[%s,%d]\n", intreg[tmp]->x.name, intreg[reg]->x.name, off);
    else
        print("str %s,[%s,%d]\n", intreg[tmp]->x.name, intreg[reg]->x.name, off);
#endif
}

/****************************************************************************
 *                                                                          *
 * Function: XINTERFACE::blkloop                                            *
 *                                                                          *
 * Purpose : Copy a block in memory.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void blkloop(int dreg, int doff, int sreg, int soff, int size, int tmps[])
{
#ifndef USE_MEMCPY
    int lab = make_label(1);

    print("add %s,%s,%d\n", intreg[sreg]->x.name, intreg[sreg]->x.name, size & ~7);
    print("add %s,%s,%d\n", intreg[tmps[2]]->x.name, intreg[dreg]->x.name, size & ~7);
    if ((size & 7) == 2)
    {
        blkcopy(tmps[2], doff, sreg, soff, 1, tmps);
        blkcopy(tmps[2], doff+1, sreg, soff+1, 1, tmps);
    }
    else
    {
        blkcopy(tmps[2], doff, sreg, soff, size & 7, tmps);
    }
    print("@%d:\n", lab);
    print("sub %s,%s,%d\n", intreg[sreg]->x.name, intreg[sreg]->x.name, 8);
    print("sub %s,%s,%d\n", intreg[tmps[2]]->x.name, intreg[tmps[2]]->x.name, 8);
    if (soff == 0 && doff == 0)
    {
        print("ldmia %s,{%s,%s}\n", intreg[sreg]->x.name, intreg[tmps[0]]->x.name, intreg[tmps[1]]->x.name);
        print("stmia %s,{%s,%s}\n", intreg[tmps[2]]->x.name, intreg[tmps[0]]->x.name, intreg[tmps[1]]->x.name);
    }
    else
    {
        blkcopy(tmps[2], doff, sreg, soff, 8, tmps);
    }
    print("cmp %s,%s\nbne @%d\n", intreg[dreg]->x.name, intreg[tmps[2]]->x.name, lab);
#endif
}

/****************************************************************************
 *                                                                          *
 * Function: memcopy                                                        *
 *                                                                          *
 * Purpose : Replacement for blkcopy(): use CRT memcpy() instead.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-17  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef USE_MEMCPY
static void memcopy(int dreg, int doff, int sreg, int soff, int size)
{
    uint_t mask = (1<<ARM_R0|1<<ARM_R1|1<<ARM_R2|1<<ARM_R3)|(usedmask[IREG] & (1<<ARM_LR));
    int cnt = 4 * bitcount(mask);
    print("stmfd sp!,{%s}\n", reglist(mask));
    if (dreg == ARM_SP) doff += cnt;  /* for the saved registers */
    if (sreg == ARM_SP) soff += cnt;  /* for the saved registers */
    addcon(ARM_R0, dreg, doff);
    addcon(ARM_R1, sreg, soff);
    movcon(ARM_R2, size);
    print("bl memcpy\n");
    print("ldmfd sp!,{%s}\n", reglist(mask));
    rtimp[RT_MCPY] = 1;
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::progbeg                                             *
 *                                                                          *
 * Purpose : Initialize the code generation (back end).                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           03-09-14  Floating-point support added.                        *
 *                                                                          *
 ****************************************************************************/

static void I(progbeg)(void)
{
    /* define the standard registers */
    intreg[ARM_R0] = mkreg("r0", ARM_R0, 1, IREG);
    intreg[ARM_R1] = mkreg("r1", ARM_R1, 1, IREG);
    intreg[ARM_R2] = mkreg("r2", ARM_R2, 1, IREG);
    intreg[ARM_R3] = mkreg("r3", ARM_R3, 1, IREG);
    intreg[ARM_R4] = mkreg("r4", ARM_R4, 1, IREG);
    intreg[ARM_R5] = mkreg("r5", ARM_R5, 1, IREG);
    intreg[ARM_R6] = mkreg("r6", ARM_R6, 1, IREG);
    intreg[ARM_R7] = mkreg("r7", ARM_R7, 1, IREG);
    intreg[ARM_R8] = mkreg("r8", ARM_R8, 1, IREG);
    intreg[ARM_R9] = mkreg("r9", ARM_R9, 1, IREG);
    intreg[ARM_R10] = mkreg("r10", ARM_R10, 1, IREG);
    intreg[ARM_FP] = mkreg("fp", ARM_FP, 1, IREG);
    intreg[ARM_IP] = mkreg("ip", ARM_IP, 1, IREG);
    intreg[ARM_SP] = mkreg("sp", ARM_SP, 1, IREG);
    intreg[ARM_LR] = mkreg("lr", ARM_LR, 1, IREG);
    intreg[ARM_PC] = mkreg("pc", ARM_PC, 1, IREG);

    /* define the pair registers */
    intreg2[ARM_R0] = mkreg("r0", ARM_R0, 1, IREG);
    intreg2[ARM_R0]->x.regnode->mask |= 1<<ARM_R1;      /* r0,r1 */
    intreg2[ARM_R2] = mkreg("r2", ARM_R2, 1, IREG);
    intreg2[ARM_R2]->x.regnode->mask |= 1<<ARM_R3;      /* r2,r3 */
    intreg2[ARM_R4] = mkreg("r4", ARM_R4, 1, IREG);
    intreg2[ARM_R4]->x.regnode->mask |= 1<<ARM_R5;      /* r4,r5 */
    intreg2[ARM_R6] = mkreg("r6", ARM_R6, 1, IREG);
    intreg2[ARM_R6]->x.regnode->mask |= 1<<ARM_R7;      /* r6,r7 */
    intreg2[ARM_R8] = mkreg("r8", ARM_R8, 1, IREG);
    intreg2[ARM_R8]->x.regnode->mask |= 1<<ARM_R9;      /* r8,r9 */

    /* define the pair registers for odd arguments */
    dreg12 = mkreg("r1", ARM_R1, 1, IREG);
    dreg12->x.regnode->mask |= 1<<ARM_R2;               /* r1,r2 */
    dreg3X = mkreg("r3", ARM_R3, 1, IREG);
    dreg3X->x.regnode->mask |= 1<<ARM_R4|1<<ARM_DBLSTK;           /* r3,stack */

#ifndef USE_MEMCPY
    blkreg = mkreg("r4", ARM_R4, 7, IREG);
#endif

    intregw = mkwildcard(intreg);
    intreg2w = mkwildcard(intreg2);

    tmask[IREG] = INTTMP;
    vmask[IREG] = INTVAR;

    cseg = 0;

    /* set appropriate processor level */
    print("[cpu arm7]\n");
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::progend                                             *
 *                                                                          *
 * Purpose : Finalize the code generation (back end).                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           03-09-14  Floating-point support added.                        *
 *           04-11-24  64-bit long long support added.                      *
 *                                                                          *
 ****************************************************************************/

static void I(progend)(void)
{
#ifdef USE_MEMCPY
    if (rtimp[RT_MCPY]) print("[extern memcpy]\n");
#endif
    if (rtimp[RT_SDIV]) print("[extern __rt_sdiv]\n");
    if (rtimp[RT_UDIV]) print("[extern __rt_udiv]\n");
    /* long long support functions */
    if (rtimp[RT_SDIV64]) print("[extern __rt_sdiv64by64]\n");
    if (rtimp[RT_UDIV64]) print("[extern __rt_udiv64by64]\n");
    if (rtimp[RT_SMOD64]) print("[extern __rt_srem64by64]\n");
    if (rtimp[RT_UMOD64]) print("[extern __rt_urem64by64]\n");
    if (rtimp[RT_SRSH64]) print("[extern __rt_srsh]\n");
    if (rtimp[RT_STOI64]) print("[extern __stoi64]\n");
    if (rtimp[RT_DTOI64]) print("[extern __dtoi64]\n");
    if (rtimp[RT_I64TOS]) print("[extern __i64tos]\n");
    if (rtimp[RT_I64TOD]) print("[extern __i64tod]\n");
    /* floating-point support functions */
    if (rtimp[RT_DTOS]) print("[extern __dtos]\n");
    if (rtimp[RT_STOD]) print("[extern __stod]\n");
    if (rtimp[RT_STOI]) print("[extern __stoi]\n");
    if (rtimp[RT_DTOI]) print("[extern __dtoi]\n");
    if (rtimp[RT_ITOS]) print("[extern __itos]\n");
    if (rtimp[RT_ITOD]) print("[extern __itod]\n");
    if (rtimp[RT_NEGS]) print("[extern __negs]\n");
    if (rtimp[RT_NEGD]) print("[extern __negd]\n");
    if (rtimp[RT_ADDS]) print("[extern __adds]\n");
    if (rtimp[RT_ADDD]) print("[extern __addd]\n");
    if (rtimp[RT_SUBS]) print("[extern __subs]\n");
    if (rtimp[RT_SUBD]) print("[extern __subd]\n");
    if (rtimp[RT_DIVS]) print("[extern __divs]\n");
    if (rtimp[RT_DIVD]) print("[extern __divd]\n");
    if (rtimp[RT_MULS]) print("[extern __muls]\n");
    if (rtimp[RT_MULD]) print("[extern __muld]\n");
    if (rtimp[RT_EQS]) print("[extern __eqs]\n");
    if (rtimp[RT_EQD]) print("[extern __eqd]\n");
    if (rtimp[RT_GES]) print("[extern __ges]\n");
    if (rtimp[RT_GED]) print("[extern __ged]\n");
    if (rtimp[RT_GTS]) print("[extern __gts]\n");
    if (rtimp[RT_GTD]) print("[extern __gtd]\n");
    if (rtimp[RT_LES]) print("[extern __les]\n");
    if (rtimp[RT_LED]) print("[extern __led]\n");
    if (rtimp[RT_LTS]) print("[extern __lts]\n");
    if (rtimp[RT_LTD]) print("[extern __ltd]\n");
    if (rtimp[RT_NES]) print("[extern __nes]\n");
    if (rtimp[RT_NED]) print("[extern __ned]\n");

    /* finalize startup and exit procedures */
    for_each_symbol(identifiers, GLOBAL, finalize_initexit, NULL);

    if (options.defaultlib)
    {
        (*IR->segment)(DRECTVE);
        print("db \" -defaultlib:crtce\"\n");
        /* We use MS libs for now... */
        print("db \" -defaultlib:coredll\"\n");
        print("db \" -defaultlib:corelibc\"\n");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: finalize_initexit                                              *
 *                                                                          *
 * Purpose : Finalize the startup and exit procedures.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void finalize_initexit(SYMBOL *sym, void *cl)
{
    if (sym->init)
    {
        (*IR->segment)(INITFN);
        print("dd %s\n", sym->x.name);
    }
    if (sym->exit)
    {
        (*IR->segment)(EXITFN);
        print("dd %s\n", sym->x.name);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::function                                            *
 *                                                                          *
 * Purpose : Generate and emit code for a function.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           02-01-27  Rewritten                                            *
 *           03-09-01  Changed test to flags for _alloca and assembler.     *
 *           03-09-14  Floating-point support added.                        *
 *           04-03-25  Bugfix: check addressed flag before using REGISTER.  *
 *           04-07-09  Added warning about unsupported fastcall option.     *
 *           04-07-22  Simplified and improved argument and local handling. *
 *           04-11-26  Call peephole optimizer from here.                   *
 *                                                                          *
 ****************************************************************************/

static void I(function)(SYMBOL *f, SYMBOL *caller[], SYMBOL *callee[], int ncalls)
{
    int i;
    int argno;
    int stacksize;
    int argsave;
    bool_t sehfunc;
    bool_t varargs;
    LIST *savelist;

    /* remember instruction list, clear it for this function */
    savelist = aslist; aslist = NULL;

    /* no __fastcall support in this backend */
    if (isfast(f->type))
        apperror(RCWARNING1(ERROR_FEATURE_NOT_SUPPORTED), "__fastcall");

    usedmask[IREG] = usedmask[FREG] = 0;
    freemask[IREG] = freemask[FREG] = ~(unsigned)0;

    sehfunc = (f->u.fcn.sehlist != NULL);
    /* use conservative approach for assembler - see inline_emitter() */
    varargs = has_varargs(f->type) || f->assembler;
    hasfp = sehfunc || f->hasalloca || varargs;

    /* remove possible trash from last visit */
    intreg[ARM_R0]->x.regnode->vbl = NULL;
    intreg[ARM_R1]->x.regnode->vbl = NULL;
    intreg[ARM_R2]->x.regnode->vbl = NULL;
    intreg[ARM_R3]->x.regnode->vbl = NULL;

    for (i = 0; callee[i]; i++)
    {
        SYMBOL *p = callee[i];

        if (p->ref != 0)
        {
            if (!p->addressed && isscalar(p->type))  /* this is a RISC! */
                p->sclass = REGISTER;
            else if (isstruct(p->type))  /* need varargs frame */
                varargs = TRUE;
        }
    }
    vmask[IREG] = (ncalls == 0 && i <= 2) ? INTVARLEAF : INTVAR;
    best_function_regsyms(callee, bitcount(vmask[IREG]));

    argsave = offset = 0; argno = 1;
    for (i = 0; callee[i]; i++)
    {
        SYMBOL *p = callee[i];
        SYMBOL *q = caller[i];
        SYMBOL *rsym;
        assert(q);

        q->type = p->type;

        rsym = argreg(argno, q->type->size);
        if (rsym != NULL && !varargs)
        {
            /* initialize incoming register */
            q->sclass = REGISTER;
            q->x.regnode = rsym->x.regnode;
            q->x.name = rsym->x.name;

            if (p->ref == 0)
            {
                /* make sure generate_function_code() won't touch it */
                p->sclass = REGISTER;
                p->x.regnode = rsym->x.regnode;
                p->x.name = rsym->x.name;
            }
            else if (ncalls == 0 && !isstruct(q->type) && !p->addressed)
            {
                /* the argument can stay in place */
                p->sclass = REGISTER;
                p->x.regnode = rsym->x.regnode;
                p->x.regnode->vbl = p;
                p->x.name = rsym->x.name;
                /* mark the register as being used, and not free */
                freemask[IREG] &= ~rsym->x.regnode->mask;
                usedmask[IREG] |= rsym->x.regnode->mask;
            }
            else
            {
                if (!register_variable(p, (*IR->x.rmap)(ttob(p->type))))
                {
                    /* need to spill the incoming argument */
                    argsave += roundup(p->type->size, 4);
                    p->x.offset = -argsave;
                    p->x.name = stringf("%d", p->x.offset);
                }
            }
            argno += roundup(q->type->size, 4) / 4;
            continue;
        }

        /* variadic routines keep all arguments on the stack */
        if (varargs) p->sclass = AUTO;

        p->x.offset = q->x.offset = offset;
        p->x.name = q->x.name = stringd(offset);

        register_variable(p, regmap(ttob(p->type)));
        offset += roundup(q->type->size, 4);
    }
    assert(caller[i] == 0);

    offset = maxoffset = (sehfunc ? argsave + 4 : argsave);
    maxargoffset = 0;
    generate_function_code(caller, callee);

    usedmask[IREG] &= INTSAVE;

    maxoffset = roundup(maxoffset, 4);  /* needed! */
    maxargoffset = roundup(maxargoffset, 4);  /* needed? */

    stacksize = maxoffset;
    if (maxargoffset > 16)  /* more than 4 outgoing args? */
        stacksize += (maxargoffset - 16);

    /* emit .PDATA function record */
    {
        int oldseg = cseg;
        (*IR->segment)(FCNRECS);
        print("dd %s\n", f->x.name);
        print("dd ((..?S%s-%s)>>2)|(((..?X%s-%s)>>2)<<8)|(1<<30)|%u\n", f->x.name, f->x.name, f->x.name, f->x.name, sehfunc ? (1<<31) : 0);
        (*IR->segment)(oldseg);
    }

    (*IR->segment)(TEXT);

    /* initialize SEH handling */
    if (sehfunc) function_seh(f);

    print("[function %s]\n", f->x.name);
    print("%s:\n", f->x.name);

    if (options.hookcall)
        print("bl _penter\n");

    if (f->attr.naked)
    {
        print("..?S%s:\n", f->x.name);  /* always needed for .PDATA section */
        emit_function_code();
        if (options.dbglevel > 1) print("..?E%s:\n", f->x.name);
    }
    else
    {
        if (hasfp)
        {
            print("mov ip,sp\n");
            if (varargs) print("stmfd sp!,{r0,r1,r2,r3}\n");  /* all arguments on the stack */
            print("stmfd sp!,{%s}\n", reglist(usedmask[IREG]|(1<<ARM_FP|1<<ARM_IP|1<<ARM_LR)));
            addcon(ARM_FP, ARM_IP, varargs ? -16 /*r0-r3*/ : 0);
            framesize = (hasfp ? -4 * bitcount(usedmask[IREG]) - 12 /*fp,ip,lr*/ : stacksize);
        }
        else
        {
            framesize = argstack = stacksize;
            if (usedmask[IREG] || ncalls)
            {
                print("stmfd sp!,{%s}\n", reglist(usedmask[IREG]|(1<<ARM_LR)));
                argstack += 4 * bitcount(usedmask[IREG]) + 4 /*lr*/;
            }
        }

        addcon(ARM_SP, ARM_SP, -stacksize);

        print("..?S%s:\n", f->x.name);  /* always needed for .PDATA section */

        emit_function_code();
        if (options.dbglevel > 1) print("..?E%s:\n", f->x.name);

        addcon(ARM_SP, ARM_SP, stacksize);

        if (hasfp)
        {
            print("ldmea fp,{%s}\n", reglist(usedmask[IREG]|(1<<ARM_FP|1<<ARM_SP|1<<ARM_PC)));
        }
        else
        {
            if (usedmask[IREG] || ncalls)
                print("ldmfd sp!,{%s}\n", reglist(usedmask[IREG]|(1<<ARM_PC)));
            else
                print("mov pc,lr\n");
        }
    }

    print("..?X%s:\n", f->x.name);

    /* flush out-of-line constants */
    flushcnst(TRUE);

    /* optimize instructions for this function */
    if (options.pragmaopt)
        peephole_optimizer(generic_rules);

    /* append this function to the instruction list */
    if (savelist != NULL)
    {
        LIST *t = savelist->link; savelist->link = aslist->link; aslist->link = t;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: function_seh                                                   *
 *                                                                          *
 * Purpose : Initialize "Structured Exception Handling" for a function.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void function_seh(SYMBOL *f)
{
    LIST *lp = f->u.fcn.sehlist;
    int lab = make_label(1);

    print("[extern __C_specific_handler]\n");
    print("dd __C_specific_handler\n");
    print("dd @%d\n", lab);

    (*IR->segment)(XCPTRECS);

    print("@%d:\n", lab);
    print("dd %d\n", listelems(lp));
    do
    {
        lp = lp->link;
        /**/
        {
            if (((SEH *)lp->data)->level == 1)
                function_seh_unnest(lp);
        }
    } while (lp != f->u.fcn.sehlist);

    (*IR->segment)(TEXT);
}

/****************************************************************************
 *                                                                          *
 * Function: function_seh_unnest                                            *
 *                                                                          *
 * Purpose : Write SEH-blocks in decending order.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void function_seh_unnest(LIST *lp)
{
    SEH *seh = (SEH *)lp->data;

    /* make sure that we write the highest scope level first */
    if (((SEH *)lp->link->data)->level > seh->level)
        function_seh_unnest(lp->link);

    seh->x.try0lab = make_label(5);       /* beginning of try block */
    seh->x.try1lab = seh->x.try0lab + 1;  /* end of try block */
    seh->x.evallab = seh->x.try0lab + 2;  /* filter */
    seh->x.xcptlab = seh->x.try0lab + 3;  /* handler */
    seh->x.exitlab = seh->x.try0lab + 4;  /* common tail */

    if (seh->type == SEH_EXCEPT)
    {
        print("dd @%d\n", seh->x.try0lab);
        print("dd @%d\n", seh->x.try1lab);
        print("dd @%d\n", seh->x.evallab);
        print("dd @%d\n", seh->x.xcptlab);
    }
    else
    {
        print("dd @%d\n", seh->x.try0lab);
        print("dd @%d\n", seh->x.try1lab);
        print("dd @%d\n", seh->x.xcptlab);
        print("dd 0\n");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::defsymbol                                           *
 *                                                                          *
 * Purpose : Build the back-end's name of a symbol.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(defsymbol)(SYMBOL *sym)
{
    if (sym->scope >= LOCAL && sym->sclass == STATIC)
        sym->x.name = stringf("@%d", make_label(1));
    else if (sym->generated)
        sym->x.name = stringf("@%s", sym->name);
    else if (sym->scope == GLOBAL || sym->sclass == EXTERN)
    {
        if (sym->attr.dllimport)
            sym->x.name = stringf("__imp__%s", sym->name);
        else if (sym->attr.dllexport)
            sym->x.name = stringf("%s", sym->name);
        else
            sym->x.name = stringf("$%s", sym->name);  /* avoid name clashes by using the $-prefix */
    }
    else if (sym->scope == CONSTANTS && (isint(sym->type) || isptr(sym->type)) &&
        sym->name[0] == '0' && sym->name[1] == 'x')
        sym->x.name = stringf("0%sH", &sym->name[2]);
    else
        sym->x.name = sym->name;
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::address                                             *
 *                                                                          *
 * Purpose : Define an address relative to a symbol.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(address)(SYMBOL *q, SYMBOL *p, long n)
{
    q->x.offset = p->x.offset + n;

    if (p->scope == GLOBAL || p->sclass == STATIC || p->sclass == EXTERN)
        q->x.name = stringf("%s%s%D", p->x.name, n >= 0 ? "+" : "", (intmax_t)n);
    else
        q->x.name = stringd(q->x.offset);
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::defaddress                                          *
 *                                                                          *
 * Purpose : Initialize and address constant.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(defaddress)(SYMBOL *sym)
{
    print("dd %s\n", sym->x.name);
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::defconst                                            *
 *                                                                          *
 * Purpose : Initialize a arithmetic constant.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           04-11-24  64-bit long long support added.                      *
 *                                                                          *
 ****************************************************************************/

static void I(defconst)(int suffix, int size, VALUE v)
{
    if (suffix == I && size == 1)
        print("db 0%xH\n", (uint_t)v.i);
    else if (suffix == I && size == 2)
        print("dw 0%xH\n", (uint_t)v.i);
    else if (suffix == I && size == 4)
        print("dd 0%xH\n", (uint_t)v.i);
    else if (suffix == I && size == 8)
    {
        uint_t *p = (uint_t *)&v.i;
        print("dd 0%xH,0%xH\n", p[0], p[1]);
    }
    else if (suffix == U && size == 1)
        print("db 0%xH\n", (uint_t)v.u);
    else if (suffix == U && size == 2)
        print("dw 0%xH\n", (uint_t)v.u);
    else if (suffix == U && size == 4)
        print("dd 0%xH\n", (uint_t)v.u);
    else if (suffix == U && size == 8)
    {
        uint_t *p = (uint_t *)&v.u;
        print("dd 0%xH,0%xH\n", p[0], p[1]);
    }
    else if (suffix == P && size == 4)
        print("dd 0%xH\n", (uint_t)v.p);
    else if (suffix == F && size == 4)
    {
        float f = (float)(double)v.d;
        print("dd 0%xH\n", *(uint_t *)&f);
    }
    else if (suffix == F && size == 8)
    {
        uint_t *p = (uint_t *)&v.d;
        print("dd 0%xH,0%xH\n", p[0], p[1]);
    }
    else assert(0);
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::defstring                                           *
 *                                                                          *
 * Purpose : Initialize a string constant.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(defstring)(int n, char *str)
{
    bool_t inquote = TRUE;
    char *s;

    print("db '");
    for (s = str; s < str + n; s++)
    {
        if (isprint(*s) && *s != '\'')
        {
            if (!inquote)
            {
                print(",'");
                inquote = TRUE;
            }
            print("%c", *s);
        }
        else
        {
            if (inquote)
            {
                print("'");
                inquote = FALSE;
            }
            print(",%d", *s);
        }
    }
    if (inquote) print("'");
    print("\n");
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::export                                              *
 *                                                                          *
 * Purpose : Define a public symbol.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           03-08-27  Condition for inline functions removed.              *
 *                                                                          *
 ****************************************************************************/

static void I(export)(SYMBOL *sym)
{
    print("[global %s]\n", sym->x.name);

    if (sym->attr.dllexport)
    {
        int oldseg = cseg;
        (*IR->segment)(DRECTVE);
        print("db \" -export:%s%s \"\n", sym->x.name, isfunc(sym->type) ? "" : ",data");
        (*IR->segment)(oldseg);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::import                                              *
 *                                                                          *
 * Purpose : Define an external symbol.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(import)(SYMBOL *sym)
{
    if (sym->ref != 0 && !sym->intrinsic)
        print("[extern %s]\n", sym->x.name);
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::global                                              *
 *                                                                          *
 * Purpose : Define a global symbol.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(global)(SYMBOL *sym)
{
    if (sym->u.seg == BSS && sym->sclass != STATIC)
    {
        print("[common %s %d]\n", sym->x.name, sym->type->size);
        noresb = TRUE;  /* don't reserve space */
    }
    else
    {
        print("[alignb %d]\n", sym->type->align);
        print("%s:\n", sym->x.name);
        noresb = FALSE;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::local                                               *
 *                                                                          *
 * Purpose : Define a local symbol.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           02-01-27  Support for _exception_info added.                   *
 *                                                                          *
 ****************************************************************************/

static void I(local)(SYMBOL *sym)
{
    if (sym->intrinsic)
    {
        /*
         * The only intrinsic local variable currently
         * supported is the value from _exception_info().
         */
        sym->x.offset = -4;
        sym->x.name = stringd(-4);
        return;
    }

    if (!register_variable(sym, (*IR->x.rmap)(ttob(sym->type))))
        mkauto(sym);
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::segment                                             *
 *                                                                          *
 * Purpose : Switch logical segments.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           02-01-27  Section ".xdata" added.                              *
 *           03-09-18  Section .CRT$XIC and .CRT$XTC added.                 *
 *           04-03-13  Support added for code_seg, data_seg pragmas.        *
 *                                                                          *
 ****************************************************************************/

static void I(segment)(int n)
{
    if (n == cseg)
        return;

    cseg = n;

    if (cseg == TEXT && *options.codeseg)
        print("[section %s ER]\n", options.codeseg);  /* E(xecute) R(ead) */
    else if (cseg == DATA && *options.dataseg)
        print("[section %s WR]\n", options.dataseg);  /* W(rite), R(ead) */
    else if (cseg == LIT && *options.litseg)
        print("[section %s R]\n", options.litseg);    /* R(ead) */
    else if (cseg == TEXT)
        print("[section .text]\n");
    else if (cseg == DATA || cseg == LIT)
        print("[section .data]\n");
    else if (cseg == BSS)
        print("[section .bss]\n");
    else if (cseg == DRECTVE)
        print("[section .drectve]\n");
    else if (cseg == FCNRECS)
        print("[section .pdata]\n");
    else if (cseg == XCPTRECS)
        print("[section .xdata]\n");
    else if (cseg == INITFN)
        print("[section .CRT$XIC]\n");  /* between .CRT$XIA and .CRT$XIZ */
    else if (cseg == EXITFN)
        print("[section .CRT$XTC]\n");  /* between .CRT$XTA and .CRT$XTZ */
    else
        assert(0);
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::space                                               *
 *                                                                          *
 * Purpose : Define an uninitialized block.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(space)(int n)
{
    if (cseg != BSS)
        print("times %d db 0\n", n);
    else if (!noresb)
        print("resb %d\n", n);
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::sehbeg                                              *
 *                                                                          *
 * Purpose : Handle the initialization of a SEH block.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(sehbeg)(int type, SEH *seh)
{
    if (type == SEH_TRY)
    {
        print("@%d:\n", seh->x.try0lab);
    }
    else if (type == SEH_EXCEPT)
    {
        print("ldr pc,[sp],4\n");
        print("@%d:\n", seh->x.xcptlab);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::sehend                                              *
 *                                                                          *
 * Purpose : Handle the termination of a SEH block.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(sehend)(int type, SEH *seh)
{
    if (type == SEH_TRY)
    {
        if (seh->type == SEH_FINALLY)
        {
            print("@%d:\n", seh->x.try1lab);
            print("bl @%d\n", seh->x.xcptlab);
            print("b @%d\n", seh->x.exitlab);
            print("@%d:\n", seh->x.xcptlab);
            print("str lr,[sp,-4]!\n");
        }
        else
        {
            print("@%d:\n", seh->x.try1lab);
            print("b @%d\n", seh->x.exitlab);
            print("@%d:\n", seh->x.evallab);
            print("str lr,[sp,-4]!\n");

            print("str r0,[fp,%d]\n", framesize + (-4));  /* _exception_info() */
            if (seh->sym)
            {
                print("ldr r0,[r0,0]\n");  /* EXCEPTION_RECORD* */
                print("ldr r0,[r0,0]\n");  /* ExceptionCode */
                print("str r0,[fp,%d]\n", framesize + seh->sym->x.offset);  /* _exception_code() */
            }
        }
    }
    else if (type == SEH_EXCEPT)
    {
        print("@%d:\n", seh->x.exitlab);
    }
    else if (type == SEH_FINALLY)
    {
        print("ldr pc,[sp],4\n");
        print("@%d:\n", seh->x.exitlab);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::unwind                                              *
 *                                                                          *
 * Purpose : Handle unwinding of a SEH block.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(unwind)(SEH *seh)
{
    apperror(RCFATAL(ERROR_FEATURE_NOT_SUPPORTED), "unwind");
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::opcall                                              *
 *                                                                          *
 * Purpose : Return non-zero if the operator calls a library routine.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-18  Created                                              *
 *           04-03-08  Added operators CEQ, CGE, CGT, CLE, CLT, CNE.        *
 *           04-11-24  Use specific operators - more readable.              *
 *                                                                          *
 ****************************************************************************/

static int I(opcall)(int op)
{
    switch (op)
    {
        case CVF+F+sizeop(4):
        case CVF+F+sizeop(8):
        case CVF+I+sizeop(4):
        case CVF+I+sizeop(8):
        case CVI+F+sizeop(4):
        case CVI+F+sizeop(8):
        case NEG+F+sizeop(4):
        case NEG+F+sizeop(8):
        case ADD+F+sizeop(4):
        case ADD+F+sizeop(8):
        case SUB+F+sizeop(4):
        case SUB+F+sizeop(8):
        case MUL+F+sizeop(4):
        case MUL+F+sizeop(8):
        case DIV+F+sizeop(4):
        case DIV+F+sizeop(8):
        case DIV+I+sizeop(4):
        case DIV+I+sizeop(8):
        case DIV+U+sizeop(4):
        case DIV+U+sizeop(8):
        case RSH+I+sizeop(8):
        case MOD+I+sizeop(4):
        case MOD+I+sizeop(8):
        case MOD+U+sizeop(4):
        case MOD+U+sizeop(8):
        case EQ+F+sizeop(4):
        case EQ+F+sizeop(8):
        case GE+F+sizeop(4):
        case GE+F+sizeop(8):
        case GT+F+sizeop(4):
        case GT+F+sizeop(8):
        case LE+F+sizeop(4):
        case LE+F+sizeop(8):
        case LT+F+sizeop(4):
        case LT+F+sizeop(8):
        case NE+F+sizeop(4):
        case NE+F+sizeop(8):
            return 1;
        default:
            return 0;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::dbgline                                             *
 *                                                                          *
 * Purpose : Emit a debug info entry for the source coordinate *cp.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void I(dbgline)(COORDINATE *cp)
{
    print("#line %d \"%s\"\n", cp->y, cp->file);
}

/****************************************************************************
 *                                                                          *
 * Function: movcon                                                         *
 *                                                                          *
 * Purpose : Move arbitrary constant into register dst.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef USE_MEMCPY
static void movcon(int dst, int con)
{
    bool_t inv = bitcount(con) > 16;
    int n = 24;
    uint_t v;

    if (inv) con = ~con;

    while (n > 0 && (con & (0xC0 << n)) == 0)
        n -= 2;
    v = con & (0xFF << n);

    if (inv)
    {
        print("mvn %s,%d\n", intreg[dst]->x.name, v);
        con = (~con) - (~v);
    }
    else
    {
        print("mov %s,%d\n", intreg[dst]->x.name, v);
        con -= v;
    }

    addcon(dst, dst, con);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: addcon                                                         *
 *                                                                          *
 * Purpose : Add arbitrary constant to register src, put result in dst.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void addcon(int dst, int src, int con)
{
    int n = 0;

    if (con == 0)
    {
        if (dst != src)
            print("mov %s,%s\n", intreg[dst]->x.name, intreg[src]->x.name);
    }
    else do
    {
        for (; (con & 3) == 0; con >>= 2, n += 2)
            ;
        if (!(con & 0x100))
        {
            print("add %s,%s,0x%x\n", intreg[dst]->x.name, intreg[src]->x.name, (con & 0xFF) << n);
            con -= con & 0xFF;
        }
        else
        {
            print("sub %s,%s,0x%x\n", intreg[dst]->x.name, intreg[src]->x.name, (-con & 0xFF) << n);
            con += -con & 0xFF;
        }
        src = dst;
    } while (con);
}

/****************************************************************************
 *                                                                          *
 * Function: argreg                                                         *
 *                                                                          *
 * Purpose : Decide which register to assign to this argument, if any.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *           02-01-27  Rewritten                                            *
 *           03-09-14  Floating-point support added.                        *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *argreg(int argno, int sz)
{
    if (argno == 1 && sz <= 4) return intreg[ARM_R0];
    if (argno == 1 && sz == 8) return intreg2[ARM_R0];

    if (argno == 2 && sz <= 4) return intreg[ARM_R1];
    if (argno == 2 && sz == 8) return dreg12;

    if (argno == 3 && sz <= 4) return intreg[ARM_R2];
    if (argno == 3 && sz == 8) return intreg2[ARM_R2];

    if (argno == 4 && sz <= 4) return intreg[ARM_R3];
    if (argno == 4 && sz == 8) return dreg3X;  /* half in r3, half on stack */

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: reglist                                                        *
 *                                                                          *
 * Purpose : Build a register list from the given bitmask.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static const char *reglist(uint_t mask)
{
    static char list[16 * 4 + 1];
    int i;

    list[0] = '\0';
    for (i = 0; i < 16; i++)
    {
        if (mask & (1<<i))
        {
            if (list[0] != '\0')
                strcat(list, ",");
            strcat(list, intreg[i]->x.name);
        }
    }

    return list;
}

/****************************************************************************
 *                                                                          *
 * Function: newcnst                                                        *
 *                                                                          *
 * Purpose : Add a constant to the literal pool.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-12-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int newcnst(SYMBOL *sym)
{
    OUTOFLINE *cnst;

    if (cnstlist)
    {
        /* already used this constant? */
        LIST *lp = cnstlist;
        do
        {
            cnst = (OUTOFLINE *)lp->data;
            if (cnst->sym == sym) return cnst->lab;
        } while ((lp = lp->link) != cnstlist);
    }

    /* allocate a new out-of-line constant */
    cnst = memalloc(sizeof(*cnst), funca);
    cnst->sym = sym;
    cnst->lab = make_label(1);
    cnstlist = listappend(cnst, cnstlist);

    return cnst->lab;
}

/****************************************************************************
 *                                                                          *
 * Function: flushcnst                                                      *
 *                                                                          *
 * Purpose : Flush all constants in the literal pool.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-27  Created (moved from function).                       *
 *           03-09-14  Floating-point support added.                        *
 *           04-03-25  Emit constants through defconst, not as strings.     *
 *           04-11-24  Use file position as flush marker (not const count). *
 *                                                                          *
 ****************************************************************************/

static void flushcnst(bool_t force)
{
    /* use output file position as "address approximation" */
    if (cnstlist && (force || (listelems(aslist) - nelems) > FLUSH_LIMIT))
    {
        /* dump all current out-of-line constants */
        LIST *lp = cnstlist;
        do
        {
            lp = lp->link;
            /**/
            {
                OUTOFLINE *cnst = (OUTOFLINE *)lp->data;
                if (cnst->sym->scope == CONSTANTS)
                {
                    print("..?C%d:\n", cnst->lab);
                    (*IR->defconst)(cnst->sym->type->op, cnst->sym->type->size, cnst->sym->u.c.v);
                }
                else  /* symbol reference */
                {
                    print("..?C%d:\ndd %s\n", cnst->lab, cnst->sym->x.name);
                }
            }
        } while (lp != cnstlist);
        cnstlist = NULL;

        /* remember position in output file */
        nelems = listelems(aslist);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: flushcnst                                                      *
 *                                                                          *
 * Purpose : Really flush all constants in the literal pool.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-06-25  Created                                              *
 *           04-11-24  Use file position as flush marker (not const count). *
 *                                                                          *
 ****************************************************************************/

static void flushcnst_with_branch(void)
{
    /* use output file position as "address approximation" */
    if (cnstlist && (listelems(aslist) - nelems) > FLUSH_LIMIT)
    {
        /* jump around a dump of all current out-of-line constants */
        int lab = make_label(1);
        print("b @%d\n", lab);
        flushcnst(TRUE);
        print("@%d:\n", lab);
    }
}


INTERFACE armIR = {
    /* size, align, outofline */
    1, 1, 0,  /* _Bool */
    1, 1, 0,  /* char */
    2, 2, 0,  /* short */
    4, 4, 0,  /* int */
    4, 4, 0,  /* long */
    8, 4, 0,  /* long long; 4->8 bytes 04-11-24 */
    4, 4, 0,  /* float */
    8, 4, 0,  /* double */
    8, 4, 0,  /* long double */
    4, 4, 0,  /* T* (ptr) */
    0, 4, 0,  /* struct; so that ARGB keeps stack aligned */
    1,        /* little_endian */
    0,        /* wants_callb */
    1,        /* wants_argb */
    0,        /* wants_optb */
    1,        /* left_to_right */
    0,        /* wants_dag */
    0,        /* unsigned_char (default=no) */
    0,        /* wants_intrinsic */
    I(address),
    blockbeg,       /* generic */
    blockend,       /* generic */
    I(defaddress),
    I(defconst),
    I(defstring),
    I(defsymbol),
    emit,           /* generic */
    I(export),
    I(function),
    gencode,
    I(global),
    I(import),
    I(local),
    I(progbeg),
    I(progend),
    I(segment),
    I(space),
    I(sehbeg),
    I(sehend),
    I(unwind),
    I(opcall),
    0,  /* dbgblock */
    0,  /* dbgend */
    0,  /* dbgfend */
    0,  /* dbginit */
    I(dbgline),
    0,  /* dbgsym */
    0,  /* dbgtype */
    {
        1,  /* max_unaligned_load */
        regmap,
        blkfetch, blkstore, blkloop,
        _label,
        _rule,
        _nts,
        _kids,
        _string,
        _templates,
        _isinstruction,
        _ntname,
        emit2,
        doarg,
        target,
        clobber,
    }
};
