/* This file auto-generated from arm.dat by arm.pl - don't edit it */

/* This file is included by armemit.c */

static struct ITEMPLATE instrux_DB[] = {
    {-1}
};

static struct ITEMPLATE instrux_DD[] = {
    {-1}
};

static struct ITEMPLATE instrux_DQ[] = {
    {-1}
};

static struct ITEMPLATE instrux_DT[] = {
    {-1}
};

static struct ITEMPLATE instrux_DW[] = {
    {-1}
};

static struct ITEMPLATE instrux_EQU[] = {
    {I_EQU, 1, {A_IMMEDIATE,0,0,0}, "\0", IF_ARM7},
    {I_EQU, 2, {A_IMMEDIATE|A_COLON,A_IMMEDIATE,0,0}, "\0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_RESB[] = {
    {I_RESB, 1, {A_IMMEDIATE,0,0,0}, "\x7F", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_RESD[] = {
    {-1}
};

static struct ITEMPLATE instrux_RESQ[] = {
    {-1}
};

static struct ITEMPLATE instrux_REST[] = {
    {-1}
};

static struct ITEMPLATE instrux_RESW[] = {
    {-1}
};

static struct ITEMPLATE instrux_ADCcc[] = {
    {I_ADCcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x0\xA0", IF_ARM7},
    {I_ADCcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x0\xA0", IF_ARM7},
    {I_ADCcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x0\xA0", IF_ARM7},
    {I_ADCcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x2\xA0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_ADDcc[] = {
    {I_ADDcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x0\x80", IF_ARM7},
    {I_ADDcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x0\x80", IF_ARM7},
    {I_ADDcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x0\x80", IF_ARM7},
    {I_ADDcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x2\x80", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_ANDcc[] = {
    {I_ANDcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x0\x00", IF_ARM7},
    {I_ANDcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x0\x00", IF_ARM7},
    {I_ANDcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x0\x00", IF_ARM7},
    {I_ANDcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x2\x00", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_BICcc[] = {
    {I_BICcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x1\xC0", IF_ARM7},
    {I_BICcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x1\xC0", IF_ARM7},
    {I_BICcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x1\xC0", IF_ARM7},
    {I_BICcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x3\xC0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_BLcc[] = {
    {I_BLcc, 1, {A_IMMEDIATE|A_BITS32,0,0,0}, "\1\x0B", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_BXcc[] = {
    {I_BXcc, 1, {A_REG32,0,0,0}, "\3\x01\x2F\xFF\x10", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_Bcc[] = {
    {I_Bcc, 1, {A_IMMEDIATE|A_BITS32,0,0,0}, "\1\x0A", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_CMNcc[] = {
    {I_CMNcc, 2, {A_REG32,A_REG32,0,0}, "\xC\x1\x60", IF_ARM7},
    {I_CMNcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\xD\x1\x60", IF_ARM7},
    {I_CMNcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\xE\x1\x60", IF_ARM7},
    {I_CMNcc, 2, {A_REG32,A_IMMEDIATE,0,0}, "\xF\x3\x60", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_CMPcc[] = {
    {I_CMPcc, 2, {A_REG32,A_REG32,0,0}, "\xC\x1\x40", IF_ARM7},
    {I_CMPcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\xD\x1\x40", IF_ARM7},
    {I_CMPcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\xE\x1\x40", IF_ARM7},
    {I_CMPcc, 2, {A_REG32,A_IMMEDIATE,0,0}, "\xF\x3\x40", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_EORcc[] = {
    {I_EORcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x0\x20", IF_ARM7},
    {I_EORcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x0\x20", IF_ARM7},
    {I_EORcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x0\x20", IF_ARM7},
    {I_EORcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x2\x20", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDMDAcc[] = {
    {I_LDMDAcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x81", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDMDBcc[] = {
    {I_LDMDBcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x91", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDMEAcc[] = {
    {I_LDMEAcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x91", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDMEDcc[] = {
    {I_LDMEDcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x99", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDMFAcc[] = {
    {I_LDMFAcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x81", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDMFDcc[] = {
    {I_LDMFDcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x89", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDMIAcc[] = {
    {I_LDMIAcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x89", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDMIBcc[] = {
    {I_LDMIBcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x99", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDMcc[] = {
    {-1}
};

static struct ITEMPLATE instrux_LDRHcc[] = {
    {I_LDRHcc, 2, {A_REG32,A_IMMEDIATE|A_BITS32,0,0}, "\x22\x50\xB0", IF_ARM7},
    {I_LDRHcc, 2, {A_REG32,A_REG32,0,0}, "\x23\x50\xB0", IF_ARM7},
    {I_LDRHcc, 3, {A_REG32,A_REG32,A_IMMEDIATE|A_BITS32,0}, "\x24\x50\xB0", IF_ARM7},
    {I_LDRHcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x25\x10\xB0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDRSBcc[] = {
    {I_LDRSBcc, 2, {A_REG32,A_IMMEDIATE|A_BITS32,0,0}, "\x22\x50\xD0", IF_ARM7},
    {I_LDRSBcc, 2, {A_REG32,A_REG32,0,0}, "\x23\x50\xD0", IF_ARM7},
    {I_LDRSBcc, 3, {A_REG32,A_REG32,A_IMMEDIATE|A_BITS32,0}, "\x24\x50\xD0", IF_ARM7},
    {I_LDRSBcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x25\x10\xD0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDRSHcc[] = {
    {I_LDRSHcc, 2, {A_REG32,A_IMMEDIATE|A_BITS32,0,0}, "\x22\x50\xF0", IF_ARM7},
    {I_LDRSHcc, 2, {A_REG32,A_REG32,0,0}, "\x23\x50\xF0", IF_ARM7},
    {I_LDRSHcc, 3, {A_REG32,A_REG32,A_IMMEDIATE|A_BITS32,0}, "\x24\x50\xF0", IF_ARM7},
    {I_LDRSHcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x25\x10\xF0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_LDRcc[] = {
    {I_LDRcc, 2, {A_REG32,A_IMMEDIATE|A_BITS32,0,0}, "\x17\x05\x10", IF_ARM7},
    {I_LDRcc, 2, {A_REG32,A_REG32,0,0}, "\x18\x04\x10", IF_ARM7},
    {I_LDRcc, 3, {A_REG32,A_REG32,A_IMMEDIATE|A_BITS32,0}, "\x19\x04\x10", IF_ARM7},
    {I_LDRcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x20\x06\x10", IF_ARM7},
    {I_LDRcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE|A_BITS32}, "\x21\x06\x10", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_MLAcc[] = {
    {I_MLAcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\x15\x00\x20\x90", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_MOVcc[] = {
    {I_MOVcc, 2, {A_REG32,A_REG32,0,0}, "\x8\x1\xA0", IF_ARM7},
    {I_MOVcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x9\x1\xA0", IF_ARM7},
    {I_MOVcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\xA\x1\xA0", IF_ARM7},
    {I_MOVcc, 2, {A_REG32,A_IMMEDIATE,0,0}, "\xB\x3\xA0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_MRScc[] = {
    {I_MRScc, 2, {A_REG32,A_REG32,0,0}, "\x10\x01\x0F", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_MSRcc[] = {
    {I_MSRcc, 2, {A_REG32,A_REG32,0,0}, "\x11\x01\x29\xF0", IF_ARM7},
    {I_MSRcc, 2, {A_REGF,A_REG32,0,0}, "\x12\x01\x28\xF0", IF_ARM7},
    {I_MSRcc, 2, {A_REGF,A_IMMEDIATE,0,0}, "\x13\x03\x28\xF0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_MULcc[] = {
    {I_MULcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x14\x00\x00\x90", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_MVNcc[] = {
    {I_MVNcc, 2, {A_REG32,A_REG32,0,0}, "\x8\x1\xE0", IF_ARM7},
    {I_MVNcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x9\x1\xE0", IF_ARM7},
    {I_MVNcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\xA\x1\xE0", IF_ARM7},
    {I_MVNcc, 2, {A_REG32,A_IMMEDIATE,0,0}, "\xB\x3\xE0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_ORRcc[] = {
    {I_ORRcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x1\x80", IF_ARM7},
    {I_ORRcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x1\x80", IF_ARM7},
    {I_ORRcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x1\x80", IF_ARM7},
    {I_ORRcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x3\x80", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_RSBcc[] = {
    {I_RSBcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x0\x60", IF_ARM7},
    {I_RSBcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x0\x60", IF_ARM7},
    {I_RSBcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x0\x60", IF_ARM7},
    {I_RSBcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x2\x60", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_RSCcc[] = {
    {I_RSCcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x0\xE0", IF_ARM7},
    {I_RSCcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x0\xE0", IF_ARM7},
    {I_RSCcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x0\xE0", IF_ARM7},
    {I_RSCcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x2\xE0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_SBCcc[] = {
    {I_SBCcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x0\xC0", IF_ARM7},
    {I_SBCcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x0\xC0", IF_ARM7},
    {I_SBCcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x0\xC0", IF_ARM7},
    {I_SBCcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x2\xC0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_SMLALcc[] = {
    {I_SMLALcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\x16\x00\xE0\x90", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_SMULLcc[] = {
    {I_SMULLcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\x16\x00\xC0\x90", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STMDAcc[] = {
    {I_STMDAcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x80", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STMDBcc[] = {
    {I_STMDBcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x90", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STMEAcc[] = {
    {I_STMEAcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x88", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STMEDcc[] = {
    {I_STMEDcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x80", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STMFAcc[] = {
    {I_STMFAcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x98", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STMFDcc[] = {
    {I_STMFDcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x90", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STMIAcc[] = {
    {I_STMIAcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x88", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STMIBcc[] = {
    {I_STMIBcc, 2, {A_REG32,A_REGLIST,0,0}, "\x26\x98", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STMcc[] = {
    {-1}
};

static struct ITEMPLATE instrux_STRHcc[] = {
    {I_STRHcc, 2, {A_REG32,A_IMMEDIATE|A_BITS32,0,0}, "\x22\x40\xB0", IF_ARM7},
    {I_STRHcc, 2, {A_REG32,A_REG32,0,0}, "\x23\x40\xB0", IF_ARM7},
    {I_STRHcc, 3, {A_REG32,A_REG32,A_IMMEDIATE|A_BITS32,0}, "\x24\x40\xB0", IF_ARM7},
    {I_STRHcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x25\x00\xB0", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_STRcc[] = {
    {I_STRcc, 2, {A_REG32,A_IMMEDIATE|A_BITS32,0,0}, "\x17\x05\x00", IF_ARM7},
    {I_STRcc, 2, {A_REG32,A_REG32,0,0}, "\x18\x04\x00", IF_ARM7},
    {I_STRcc, 3, {A_REG32,A_REG32,A_IMMEDIATE|A_BITS32,0}, "\x19\x04\x00", IF_ARM7},
    {I_STRcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x20\x06\x00", IF_ARM7},
    {I_STRcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE|A_BITS32}, "\x21\x06\x00", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_SUBcc[] = {
    {I_SUBcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\4\x0\x40", IF_ARM7},
    {I_SUBcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\5\x0\x40", IF_ARM7},
    {I_SUBcc, 4, {A_REG32,A_REG32,A_REG32,A_IMMEDIATE}, "\6\x0\x40", IF_ARM7},
    {I_SUBcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\7\x2\x40", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_SWIcc[] = {
    {I_SWIcc, 1, {A_IMMEDIATE,0,0,0}, "\2\x0F", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_SWPcc[] = {
    {I_SWPcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\x27\x01\x90", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_TEQcc[] = {
    {I_TEQcc, 2, {A_REG32,A_REG32,0,0}, "\xC\x1\x20", IF_ARM7},
    {I_TEQcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\xD\x1\x20", IF_ARM7},
    {I_TEQcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\xE\x1\x20", IF_ARM7},
    {I_TEQcc, 2, {A_REG32,A_IMMEDIATE,0,0}, "\xF\x3\x20", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_TSTcc[] = {
    {I_TSTcc, 2, {A_REG32,A_REG32,0,0}, "\xC\x1\x00", IF_ARM7},
    {I_TSTcc, 3, {A_REG32,A_REG32,A_REG32,0}, "\xD\x1\x00", IF_ARM7},
    {I_TSTcc, 3, {A_REG32,A_REG32,A_IMMEDIATE,0}, "\xE\x1\x00", IF_ARM7},
    {I_TSTcc, 2, {A_REG32,A_IMMEDIATE,0,0}, "\xF\x3\x00", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_UMLALcc[] = {
    {I_UMLALcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\x16\x00\xA0\x90", IF_ARM7},
    {-1}
};

static struct ITEMPLATE instrux_UMULLcc[] = {
    {I_UMULLcc, 4, {A_REG32,A_REG32,A_REG32,A_REG32}, "\x16\x00\x80\x90", IF_ARM7},
    {-1}
};

static struct ITEMPLATE *asm_instructions[] = {
    instrux_DB,
    instrux_DD,
    instrux_DQ,
    instrux_DT,
    instrux_DW,
    instrux_EQU,
    instrux_RESB,
    instrux_RESD,
    instrux_RESQ,
    instrux_REST,
    instrux_RESW,
    instrux_ADCcc,
    instrux_ADDcc,
    instrux_ANDcc,
    instrux_BICcc,
    instrux_BLcc,
    instrux_BXcc,
    instrux_Bcc,
    instrux_CMNcc,
    instrux_CMPcc,
    instrux_EORcc,
    instrux_LDMDAcc,
    instrux_LDMDBcc,
    instrux_LDMEAcc,
    instrux_LDMEDcc,
    instrux_LDMFAcc,
    instrux_LDMFDcc,
    instrux_LDMIAcc,
    instrux_LDMIBcc,
    instrux_LDMcc,
    instrux_LDRHcc,
    instrux_LDRSBcc,
    instrux_LDRSHcc,
    instrux_LDRcc,
    instrux_MLAcc,
    instrux_MOVcc,
    instrux_MRScc,
    instrux_MSRcc,
    instrux_MULcc,
    instrux_MVNcc,
    instrux_ORRcc,
    instrux_RSBcc,
    instrux_RSCcc,
    instrux_SBCcc,
    instrux_SMLALcc,
    instrux_SMULLcc,
    instrux_STMDAcc,
    instrux_STMDBcc,
    instrux_STMEAcc,
    instrux_STMEDcc,
    instrux_STMFAcc,
    instrux_STMFDcc,
    instrux_STMIAcc,
    instrux_STMIBcc,
    instrux_STMcc,
    instrux_STRHcc,
    instrux_STRcc,
    instrux_SUBcc,
    instrux_SWIcc,
    instrux_SWPcc,
    instrux_TEQcc,
    instrux_TSTcc,
    instrux_UMLALcc,
    instrux_UMULLcc,
};
