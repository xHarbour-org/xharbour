/****************************************************************************
 *                                                                          *
 * File    : arm.h                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; ARM Assembler; Constants and definitions.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifndef _ARM_H
#define _ARM_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Codes returned from the parser, for registers and instructions.
 */
enum {
    R_A1 = EXPR_REG_START, R_A2, R_A3, R_A4,
    R_CPSR, R_CPSR_FLG, R_FP, R_IP, R_LR, R_PC,
    R_R0, R_R1, R_R10, R_R11, R_R12, R_R13, R_R14, R_R15, R_R2, R_R3,
    R_R4, R_R5, R_R6, R_R7, R_R8, R_R9, R_SL, R_SP, R_SPSR, R_SPSR_FLG,
    R_V1, R_V2, R_V3, R_V4, R_V5, R_V6, R_V7, R_V8,
    REG_ENUM_LIMIT
};

/* condition code names */
enum {
    C_AL, C_CC, C_CS, C_EQ,
    C_GE, C_GT, C_HI, C_LE,
    C_LS, C_LT, C_MI, C_NE,
    C_PL, C_VC, C_VS, C_NONE,
    C_SSETFLAG = 0x0100,        /* S-flag */
    C_BSETFLAG = 0x0200,        /* B-flag */
    C_TSETFLAG = 0x0400,        /* T-flag */
    C_WSETFLAG = 0x0800,        /* ! write-back-flag */
    C_FSETFLAG = 0x1000,        /* ^ LDM/STM */
};

/* special tokens */
enum {
    S_ASL, S_ASR, S_LSL, S_LSR, S_ROR, S_RRX
};

/*
 * Note that because segment registers may be used as instruction
 * prefixes, we must ensure the enumerations for prefixes and
 * register names do not overlap.
 */
enum {
    PREFIX_ENUM_START = REG_ENUM_LIMIT,
    P_TIMES = PREFIX_ENUM_START
};

/*
 * Here we define the operand types. These are implemented as bit
 * masks, since some are subsets of others.
 */

/* size, and other attributes, of the operand */
#define A_BITS8     0x00000001L
#define A_BITS16    0x00000002L
#define A_BITS32    0x00000004L
#define A_BITS64    0x00000008L         /* FPU only */
#define A_FLAG      0x00000020L         /* CPSR_flg or SPSR_flg */

#define A_NEAR      0x00000040L
#define A_SHORT     0x00000080L         /* and this means what it says :) */

#define A_SIZEMASK  0x000000FFL         /* all the size attributes */
#define A_TYPEMASK  (~A_SIZEMASK)

#define A_COLON     0x00000200L         /* operand is followed by a colon */

/* type of operand: register or immediate */
#define A_REGISTER  0x00001000L         /* register number in 'basereg' */
#define A_IMMEDIATE 0x00002000L

#define A_REGF      0x00001020L         /* Reg_flg */
#define A_REG32     0x00001004L
#define A_REGLIST   0x00001008L         /* {reg-list} */

/* special type of immediate operand */
#define A_ONENESS   0x00800000L         /* so UNITY == IMMEDIATE | ONENESS */
#define A_UNITY     0x00802000L         /* for shift/rotate instructions */

/*
 * Instruction template flags. These specify which processor
 * targets the instruction is eligible for, whether it is
 * privileged or undocumented, and also specify extra error
 * checking on the matching of the instruction.
 *
 * IF_SM stands for Size Match: any operand whose size is not
 * explicitly specified by the template is `really' intended to be
 * the same size as the first size-specified operand.
 * Non-specification is tolerated in the input instruction, but
 * _wrong_ specification is not.
 *
 * IF_SM2 invokes Size Match on only the first _two_ operands, for
 * three-operand instructions such as SHLD: it implies that the
 * first two operands must match in size, but that the third is
 * required to be _unspecified_.
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
#define IF_SM     0x00000001UL         /* ARM7 */
#define IF_SM2    0x00000002UL         /* size match first two operands */
#define IF_SB     0x00000004UL         /* unsized operands can't be non-byte */
#define IF_SW     0x00000008UL         /* unsized operands can't be non-word */
#define IF_SD     0x00000010UL         /* unsized operands can't be nondword */
#define IF_AR0    0x00000020UL         /* SB, SW, SD applies to argument 0 */
#define IF_AR1    0x00000040UL         /* SB, SW, SD applies to argument 1 */
#define IF_AR2    0x00000060UL         /* SB, SW, SD applies to argument 2 */
#define IF_ARMASK 0x00000060UL         /* mask for unsized argument spec */
#define IF_PMASK  0xFF000000UL         /* the mask for processor types */
#define IF_ARM7   0x03000000UL         /* ARM7+ instruction */

/* Instruction names generated from arm.dat */
#include "arminsi.h"

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _ARM_H */
