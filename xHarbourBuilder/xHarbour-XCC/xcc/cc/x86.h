/****************************************************************************
 *                                                                          *
 * File    : x86.h                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; X86 Assembler; Constants and definitions.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created from definitions in ASM.H                    *
 *           01-03-21  Added flags for IF_SS2 and IF_WILLAMETTE.            *
 *           01-04-08  Changed S_TWORD to S_TBYTE.                          *
 *                                                                          *
 ****************************************************************************/

#ifndef _X86_H
#define _X86_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Codes returned from the parser, for registers and instructions.
 */
enum {
    R_AH = EXPR_REG_START, R_AL, R_AX, R_BH, R_BL, R_BP, R_BX, R_CH,
    R_CL, R_CR0, R_CR2, R_CR3, R_CR4, R_CS, R_CX, R_DH, R_DI, R_DL,
    R_DR0, R_DR1, R_DR2, R_DR3, R_DR6, R_DR7, R_DS, R_DX, R_EAX,
    R_EBP, R_EBX, R_ECX, R_EDI, R_EDX, R_ES, R_ESI, R_ESP, R_FS,
    R_GS, R_MM0, R_MM1, R_MM2, R_MM3, R_MM4, R_MM5, R_MM6, R_MM7,
    R_SI, R_SP, R_SS, R_ST0, R_ST1, R_ST2, R_ST3, R_ST4, R_ST5,
    R_ST6, R_ST7, R_TR3, R_TR4, R_TR5, R_TR6, R_TR7,
    R_XMM0, R_XMM1, R_XMM2, R_XMM3, R_XMM4, R_XMM5, R_XMM6, R_XMM7,
    REG_ENUM_LIMIT
};

/* condition code names */
enum {
    C_A, C_AE, C_B, C_BE, C_C, C_E, C_G, C_GE, C_L, C_LE, C_NA, C_NAE,
    C_NB, C_NBE, C_NC, C_NE, C_NG, C_NGE, C_NL, C_NLE, C_NO, C_NP,
    C_NS, C_NZ, C_O, C_P, C_PE, C_PO, C_S, C_Z
};

/* special tokens */
enum {
    S_BYTE, S_DWORD, S_FAR, S_LONG, S_NEAR, S_OFFSET, S_PTR,
    S_QWORD, S_SHORT, S_SHRT, S_TBYTE, S_TO, S_WORD
};

/*
 * Note that because segment registers may be used as instruction
 * prefixes, we must ensure the enumerations for prefixes and
 * register names do not overlap.
 */
enum {
    PREFIX_ENUM_START = REG_ENUM_LIMIT,
    P_LOCK = PREFIX_ENUM_START, P_REP, P_REPE, P_REPNE, P_REPNZ, P_REPZ, P_TIMES
};

/*
 * Here we define the operand types. These are implemented as bit
 * masks, since some are subsets of others; e.g. AX in a MOV
 * instruction is a special operand type, whereas AX in other
 * contexts is just another 16-bit register. (Also, consider CL in
 * shift instructions, DX in OUT, etc.)
 */

/* size, and other attributes, of the operand */
#define A_BITS8     0x00000001L
#define A_BITS16    0x00000002L
#define A_BITS32    0x00000004L
#define A_BITS64    0x00000008L         /* FPU only */
#define A_BITS80    0x00000010L         /* FPU only */
#define A_FAR       0x00000020L         /* grotty: this means 16:16 or */
                                        /* 16:32, like in CALL/JMP */
#define A_NEAR      0x00000040L
#define A_SHORT     0x00000080L         /* and this means what it says :) */

#define A_SIZEMASK  0x000000FFL         /* all the size attributes */
#define A_TYPEMASK  (~A_SIZEMASK)

#define A_TO        0x00000100L         /* reverse effect in FADD, FSUB &c */
#define A_COLON     0x00000200L         /* operand is followed by a colon */

/* type of operand: memory reference, register, etc. */
#define A_MEMORY    0x00204000L
#define A_REGISTER  0x00001000L         /* register number in 'basereg' */
#define A_IMMEDIATE 0x00002000L

#define A_REGMEM    0x00200000L         /* for r/m, ie EA, operands */
#define A_REGNORM   0x00201000L         /* 'normal' reg, qualifies as EA */
#define A_REG8      0x00201001L
#define A_REG16     0x00201002L
#define A_REG32     0x00201004L
#define A_MMXREG    0x00201008L         /* MMX registers */
#define A_XMMREG    0x00201010L         /* XMM Katmai reg */
#define A_FPUREG    0x01000000L         /* floating point stack registers */
#define A_FPU0      0x01000800L         /* FPU stack register zero */

/* special register operands: these may be treated differently */
#define A_REG_SMASK 0x00070000L         /* a mask for the following */
#define A_REG_ACCUM 0x00211000L         /* accumulator: AL, AX or EAX */
#define A_REG_AL    0x00211001L         /* REG_ACCUM | BITSxx */
#define A_REG_AX    0x00211002L         /* ditto */
#define A_REG_EAX   0x00211004L         /* and again */
#define A_REG_COUNT 0x00221000L         /* counter: CL, CX or ECX */
#define A_REG_CL    0x00221001L         /* REG_COUNT | BITSxx */
#define A_REG_CX    0x00221002L         /* ditto */
#define A_REG_ECX   0x00221004L         /* another one */
#define A_REG_DX    0x00241002L
#define A_REG_SREG  0x00081002L         /* any segment register */
#define A_REG_CS    0x01081002L         /* CS */
#define A_REG_DESS  0x02081002L         /* DS, ES, SS (non-CS 86 registers) */
#define A_REG_FSGS  0x04081002L         /* FS, GS (386 extended registers) */
#define A_REG_CDT   0x00101004L         /* CRn, DRn and TRn */
#define A_REG_CREG  0x08101004L         /* CRn */
#define A_REG_CR4   0x08101404L         /* CR4 (Pentium only) */
#define A_REG_DREG  0x10101004L         /* DRn */
#define A_REG_TREG  0x20101004L         /* TRn */

/* special type of EA */
#define A_MEM_OFFS  0x00604000L         /* simple [address] offset */

/* special type of immediate operand */
#define A_ONENESS   0x00800000L         /* so UNITY == IMMEDIATE | ONENESS */
#define A_UNITY     0x00802000L         /* for shift/rotate instructions */
#define A_BYTENESS  0x80000000L         /* so SBYTE == IMMEDIATE | BYTENESS */
#define A_SBYTE     0x80002000L         /* for op r16/32,immediate insns */

/*
 * Instruction template flags. These specify which processor
 * targets the instruction is eligible for, whether it is
 * privileged or undocumented, and also specify extra error
 * checking on the matching of the instruction.
 *
 * IF_SM stands for Size Match: any operand whose size is not
 * explicitly specified by the template is 'really' intended to be
 * the same size as the first size-specified operand.
 * Non-specification is tolerated in the input instruction, but
 * *wrong* specification is not.
 *
 * IF_SM2 invokes Size Match on only the first *two* operands, for
 * three-operand instructions such as SHLD: it implies that the
 * first two operands must match in size, but that the third is
 * required to be *unspecified*.
 *
 * IF_SB invokes Size Byte: operands with unspecified size in the
 * template are really bytes, and so no non-byte specification in
 * the input instruction will be tolerated. IF_SW similarly invokes
 * Size Word, and IF_SD invokes Size Doubleword.
 *
 * (The default state if neither IF_SM nor IF_SM2 is specified is
 * that any operand with unspecified size in the template is
 * required to have unspecified size in the instruction too...)
 */

#define IF_SM       0x00000001UL        /* size match */
#define IF_SM2      0x00000002UL        /* size match first two operands */
#define IF_SB       0x00000004UL        /* unsized operands can't be non-byte */
#define IF_SW       0x00000008UL        /* unsized operands can't be non-word */
#define IF_SD       0x00000010UL        /* unsized operands can't be nondword */
#define IF_AR0      0x00000020UL        /* SB, SW, SD applies to argument 0 */
#define IF_AR1      0x00000040UL        /* SB, SW, SD applies to argument 1 */
#define IF_AR2      0x00000060UL        /* SB, SW, SD applies to argument 2 */
#define IF_ARMASK   0x00000060UL        /* mask for unsized argument spec */
#define IF_PRIV     0x00000100UL        /* it's a privileged instruction */
#define IF_SMM      0x00000200UL        /* it's only valid in SMM */
#define IF_PROT     0x00000400UL        /* it's protected mode only */
#define IF_UNDOC    0x00001000UL        /* it's an undocumented instruction */
#define IF_FPU      0x00002000UL        /* it's an FPU instruction */
#define IF_MMX      0x00004000UL        /* it's an MMX instruction */
#define IF_3DNOW    0x00008000UL        /* it's a 3DNow! instruction */
#define IF_SSE      0x00010000UL        /* it's a SSE (KNI, MMX2) instruction */
#define IF_SSE2     0x00020000UL        /* it's a SSE2 instruction */
#define IF_PMASK    0xFF000000UL        /* the mask for processor types */
#define IF_PLEVEL   0x0F000000UL        /* the mask for processor instruction level */

#define IF_8086     0x00000000UL        /* 8086 instruction */
#define IF_186      0x01000000UL        /* 186+ instruction */
#define IF_286      0x02000000UL        /* 286+ instruction */
#define IF_386      0x03000000UL        /* 386+ instruction */
#define IF_486      0x04000000UL        /* 486+ instruction */
#define IF_PENT     0x05000000UL        /* Pentium instruction */
#define IF_P6       0x06000000UL        /* P6 instruction */
#define IF_KATMAI   0x07000000UL        /* Katmai instruction */
#define IF_WILLAMETTE 0x08000000UL      /* Willamette instruction */
#define IF_CYRIX    0x10000000UL        /* Cyrix-specific instruction */
#define IF_AMD      0x20000000UL        /* AMD-specific instruction */

/* Instruction names generated from x86.dat */
#include "x86insi.h"

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _X86_H */
