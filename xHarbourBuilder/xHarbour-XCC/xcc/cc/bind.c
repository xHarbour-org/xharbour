/****************************************************************************
 *                                                                          *
 * File    : bind.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Bindings for supported targets.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

extern INTERFACE nullIR;
extern INTERFACE x86IR;
extern INTERFACE armIR;
extern INTERFACE bytecodeIR;

extern OUTFMT x86coffOF;
extern OUTFMT x86asmOF;
extern OUTFMT x86debugOF;
extern OUTFMT armcoffOF;
extern OUTFMT armasmOF;
extern OUTFMT armdebugOF;

static BINDING bindings[] = {
    "null",         &nullIR,        NULL,           /* always first! */
    "x86-coff",     &x86IR,         &x86coffOF,
    "x86-asm",      &x86IR,         &x86asmOF,
    "arm-coff",     &armIR,         &armcoffOF,
    "arm-asm",      &armIR,         &armasmOF,
#if defined(PODEBUG) || defined(PRERELEASE)
    "x86-debug",    &x86IR,         &x86debugOF,
    "arm-debug",    &armIR,         &armdebugOF,
    "bytecode",     &bytecodeIR,    NULL,
#endif
    NULL,           NULL,           NULL
};

/****************************************************************************
 *                                                                          *
 * Function: select_binding                                                 *
 *                                                                          *
 * Purpose : Select target machine and output driver by name.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t select_binding(const char *name)
{
    int i;

    for (i = 1; bindings[i].name; i++)  /* "null" always first */
    {
        if (_stricmp(name, bindings[i].name) == 0)
        {
            IR = bindings[i].ir;
            OF = bindings[i].of;
            return TRUE;
        }
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: list_bindings                                                  *
 *                                                                          *
 * Purpose : Display a list of supported targets.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void list_bindings(void)
{
    int i;

    for (i = 1; bindings[i].name; i++)  /* "null" always first */
    {
        printf("%c /T%s\n", (bindings[i].ir == IR &&
            bindings[i].of == OF) ? '*' : ' ', bindings[i].name);
    }
}

