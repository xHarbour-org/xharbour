/****************************************************************************
 *                                                                          *
 * File    : null.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Null (no output) target.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#define I(f) null_##f

static NODE *I(gen)(NODE *p) { return p; }
static void I(address)(SYMBOL *sym, SYMBOL *base, long offset) {}
static void I(blockbeg)(ENV *e) {}
static void I(blockend)(ENV *e) {}
static void I(defaddress)(SYMBOL *sym) {}
static void I(defconst)(int suffix, int size, VALUE v) {}
static void I(defstring)(int len, char *str) {}
static void I(defsymbol)(SYMBOL *sym) {}
static void I(emit)(NODE *p) {}
static void I(export)(SYMBOL *sym) {}
static void I(function)(SYMBOL *f, SYMBOL *caller[], SYMBOL *callee[], int ncalls) {}
static void I(global)(SYMBOL *sym) {}
static void I(import)(SYMBOL *sym) {}
static void I(local)(SYMBOL *sym) {}
static void I(progbeg)(void) {}
static void I(progend)(void) {}
static void I(segment)(int n) {}
static void I(space)(int n) {}
static void I(sehbeg)(int n, SEH *seh) {}
static void I(sehend)(int n, SEH *seh) {}
static void I(unwind)(SEH *seh) {}
static int I(opcall)(int op) { return 0; }
static void I(dbgblock)(int brace, int lev, SYMBOL **locals) {}
static void I(dbgend)(COORDINATE *cp, SYMBOL *sym, COORDINATE **cpp, SYMBOL **sp, SYMBOL **dbg) {}
static void I(dbgfend)(SYMBOL *f, int lineno) {}
static void I(dbginit)(char *srcfile, char *outfile) {}
static void I(dbgline)(COORDINATE *cp) {}
static void I(dbgsym)(SYMBOL *sym) {}
static void I(dbgtype)(SYMBOL *sym) {}

INTERFACE nullIR = {
    /* size, align, outofline */
    1, 1, 0,  /* _Bool */
    1, 1, 0,  /* char */
    2, 2, 0,  /* short */
    4, 4, 0,  /* int */
    4, 4, 0,  /* long */
    8, 8, 1,  /* long long */
    4, 4, 1,  /* float */
    8, 8, 1,  /* double */
    8, 8, 1,  /* long double */
    4, 4, 0,  /* T* */
    0, 4, 0,  /* struct */
    1,        /* little_endian */
    0,        /* wants_callb */
    0,        /* wants_argb */
    0,        /* wants_optb */
    1,        /* left_to_right */
    0,        /* wants_dag */
    0,        /* unsigned_char */
    0,        /* wants_intrinsic */
    I(address),
    I(blockbeg),
    I(blockend),
    I(defaddress),
    I(defconst),
    I(defstring),
    I(defsymbol),
    I(emit),
    I(export),
    I(function),
    I(gen),
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
    I(dbgblock),
    I(dbgend),
    I(dbgfend),
    I(dbginit),
    I(dbgline),
    I(dbgsym),
    I(dbgtype)
};
