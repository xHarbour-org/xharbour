/****************************************************************************
 *                                                                          *
 * File    : intrin.c                                                       *
 *                                                                          *
 * Purpose : ISO C Compiler; Intrinsic function support.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

/* Locals */
static LIST *intrinsics;
static bool_t is_strconst;

/* Static function prototypes */
static void unref_strconst(SYMBOL *, void *);

/****************************************************************************
 *                                                                          *
 * Function: intrinsic_init                                                 *
 *                                                                          *
 * Purpose : Initialize the intrinsic function list.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-15  Created                                              *
 *           04-08-20  Added functions _rdtsc() and _bswap().               *
 *           04-11-20  Added function _cpuid().                             *
 *           04-12-09  Added functions wcslen() and wcscpy().               *
 *                                                                          *
 ****************************************************************************/

void intrinsic_init(void)
{
    INTRINSIC *intr;

    /* size_t __cdecl strlen(const char *) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("strlen");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = ptr(qual(CONST_, chartype));
    intr->prototype[1] = NULL;
    intr->rty = unsignedtype;
    intr->id = INTRIN_STRLEN;
    intr->flags = INTRIN_MAXSPEED;
    intrinsics = listappend(intr, intrinsics);

    /* char * __cdecl strcpy(char * restrict, const char * restrict) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("strcpy");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = qual(RESTRICT_, charptype);
    intr->prototype[1] = qual(RESTRICT_, ptr(qual(CONST_, chartype)));
    intr->prototype[2] = NULL;
    intr->rty = charptype;
    intr->id = INTRIN_STRCPY;
    intr->flags = INTRIN_MAXSPEED|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* int __cdecl strcmp(const char *, const char *) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("strcmp");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = ptr(qual(CONST_, chartype));
    intr->prototype[1] = ptr(qual(CONST_, chartype));
    intr->prototype[2] = NULL;
    intr->rty = inttype;
    intr->id = INTRIN_STRCMP;
    intr->flags = INTRIN_MAXSPEED;
    intrinsics = listappend(intr, intrinsics);

    /* void * __cdecl memcpy(void * restrict, const void * restrict, size_t) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("memcpy");
    intr->prototype = memarray(4, sizeof(*intr->prototype), PERM);  /* special case, OK with 3 arguments */
    intr->prototype[0] = qual(RESTRICT_, voidptype);
    intr->prototype[1] = qual(RESTRICT_, ptr(qual(CONST_, voidtype)));
    intr->prototype[2] = unsignedtype;
    intr->prototype[3] = NULL;
    intr->rty = voidptype;
    intr->id = INTRIN_MEMCPY;
    intr->flags = INTRIN_MAXSPEED;
    intrinsics = listappend(intr, intrinsics);

    /* void * __cdecl memset(void *, int, size_t) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("memset");
    intr->prototype = memarray(4, sizeof(*intr->prototype), PERM);  /* special case, OK with 3 arguments */
    intr->prototype[0] = voidptype;
    intr->prototype[1] = inttype;
    intr->prototype[2] = unsignedtype;
    intr->prototype[3] = NULL;
    intr->rty = voidptype;
    intr->id = INTRIN_MEMSET;
    intr->flags = INTRIN_MAXSPEED;
    intrinsics = listappend(intr, intrinsics);

    /* size_t __cdecl wcslen(const wchar_t *) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("wcslen");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = ptr(qual(CONST_, widechartype));
    intr->prototype[1] = NULL;
    intr->rty = unsignedtype;
    intr->id = INTRIN_WCSLEN;
    intr->flags = INTRIN_MAXSPEED;
    intrinsics = listappend(intr, intrinsics);

    /* wchar_t * __cdecl wcscpy(wchar_t * restrict, const wchar_t * restrict) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("wcscpy");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = qual(RESTRICT_, ptr(widechartype));
    intr->prototype[1] = qual(RESTRICT_, ptr(qual(CONST_, widechartype)));
    intr->prototype[2] = NULL;
    intr->rty = ptr(widechartype);
    intr->id = INTRIN_WCSCPY;
    intr->flags = INTRIN_MAXSPEED|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned int __cdecl _rotl(unsigned int, int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_rotl");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedtype;
    intr->prototype[1] = inttype;
    intr->prototype[2] = NULL;
    intr->rty = unsignedtype;
    intr->id = INTRIN_ROTL;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned int __cdecl _rotr(unsigned int, int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_rotr");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedtype;
    intr->prototype[1] = inttype;
    intr->prototype[2] = NULL;
    intr->rty = unsignedtype;
    intr->id = INTRIN_ROTR;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned long __cdecl _lrotl(unsigned long, int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_lrotl");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedlongtype;
    intr->prototype[1] = inttype;
    intr->prototype[2] = NULL;
    intr->rty = unsignedlongtype;
    intr->id = INTRIN_LROTL;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned long __cdecl _lrotr(unsigned long, int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_lrotr");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedlongtype;
    intr->prototype[1] = inttype;
    intr->prototype[2] = NULL;
    intr->rty = unsignedlongtype;
    intr->id = INTRIN_LROTR;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE;
    intrinsics = listappend(intr, intrinsics);

    /* int __cdecl _inp(unsigned short) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_inp");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedshorttype;
    intr->prototype[1] = NULL;
    intr->rty = inttype;
    intr->id = INTRIN_INP;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned long __cdecl _inpd(unsigned short) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_inpd");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedshorttype;
    intr->prototype[1] = NULL;
    intr->rty = unsignedlongtype;
    intr->id = INTRIN_INPD;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned short __cdecl _inpw(unsigned short) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_inpw");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedshorttype;
    intr->prototype[1] = NULL;
    intr->rty = unsignedshorttype;
    intr->id = INTRIN_INPW;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* int __cdecl _outp(unsigned short, int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_outp");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedshorttype;
    intr->prototype[1] = inttype;
    intr->prototype[2] = NULL;
    intr->rty = inttype;
    intr->id = INTRIN_OUTP;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned long __cdecl _outpd(unsigned short, unsigned long) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_outpd");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedshorttype;
    intr->prototype[1] = unsignedlongtype;
    intr->prototype[2] = NULL;
    intr->rty = unsignedlongtype;
    intr->id = INTRIN_OUTPD;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned short __cdecl _outpw(unsigned short, unsigned short) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_outpw");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedshorttype;
    intr->prototype[1] = unsignedshorttype;
    intr->prototype[2] = NULL;
    intr->rty = unsignedshorttype;
    intr->id = INTRIN_OUTPW;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned long long __cdecl _rdtsc(void) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_rdtsc");
    intr->prototype = memarray(1, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = NULL;
    intr->rty = unsignedlonglongtype;
    intr->id = INTRIN_RDTSC;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* unsigned int __cdecl _bswap(unsigned int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_bswap");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = unsignedtype;
    intr->prototype[1] = NULL;
    intr->rty = unsignedtype;
    intr->id = INTRIN_BSWAP;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE;
    intrinsics = listappend(intr, intrinsics);

    /* void __cdecl _cpuid(int *, int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("_cpuid");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = ptr(inttype);
    intr->prototype[1] = inttype;
    intr->prototype[2] = NULL;
    intr->rty = voidtype;
    intr->id = INTRIN_CPUID;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_SIDE_EFFECTS;
    intrinsics = listappend(intr, intrinsics);

    /* int __cdecl abs(int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("abs");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = inttype;
    intr->prototype[1] = NULL;
    intr->rty = inttype;
    intr->id = INTRIN_ABS;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE;
    intrinsics = listappend(intr, intrinsics);

    /* long __cdecl labs(long) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("labs");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longtype;
    intr->prototype[1] = NULL;
    intr->rty = longtype;
    intr->id = INTRIN_LABS;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE;
    intrinsics = listappend(intr, intrinsics);

    /* double __cdecl fabs(double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("fabs");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_FABS;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* float __cdecl fabsf(float) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("fabsf");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_FABSF;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* long double __cdecl fabsl(long double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("fabsl");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_FABS;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* double __cdecl sqrt(double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("sqrt");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_SQRT;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* float __cdecl sqrtf(float) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("sqrtf");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_SQRTF;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* long double __cdecl sqrtl(long double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("sqrtl");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_SQRT;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* double __cdecl tan(double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("tan");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_TAN;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* float __cdecl tanf(float) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("tanf");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_TANF;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* long double __cdecl tanl(long double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("tanl");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_TAN;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* double __cdecl atan(double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("atan");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_ATAN;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* float __cdecl atanf(float) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("atanf");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_ATANF;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* long double __cdecl atanl(long double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("atanl");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_ATAN;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* double __cdecl exp(double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("exp");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_EXP;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* float __cdecl expf(float) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("expf");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_EXPF;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* long double __cdecl expl(long double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("expl");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_EXP;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* double __cdecl log(double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("log");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_LOG;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* float __cdecl logf(float) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("logf");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_LOGF;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* long double __cdecl logl(long double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("logl");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_LOG;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* double __cdecl log10(double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("log10");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_LOG10;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* float __cdecl log10f(float) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("log10f");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_LOG10F;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* long double __cdecl log10l(long double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("log10l");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_LOG10;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* log(x) and log10(x) macro override: double __cdecl __log(double, int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("__log");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = inttype;
    intr->prototype[2] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_LOG_LOG10_LOG2;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* logf(x) and log10f(x) macro override: float __cdecl __logf(float, int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("__logf");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = inttype;
    intr->prototype[2] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_LOGF_LOG10F_LOG2F;
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* logl(x) and log10l(x) macro override: long double __cdecl __logl(long double, int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("__logl");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = inttype;
    intr->prototype[2] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_LOG_LOG10_LOG2;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* double __cdecl sin(double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("sin");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_SIN;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* float __cdecl sinf(float) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("sinf");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_SINF;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* long double __cdecl sinl(long double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("sinl");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_SIN;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* double __cdecl cos(double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("cos");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_COS;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* float __cdecl cosf(float) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("cosf");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_COSF;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* long double __cdecl cosl(long double) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("cosl");
    intr->prototype = memarray(2, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_COS;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* cos(x) and sin(x) macro override: double __cdecl __sin(double, unsigned int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("__sin");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = doubletype;
    intr->prototype[1] = unsignedtype;
    intr->prototype[2] = NULL;
    intr->rty = doubletype;
    intr->id = INTRIN_SIN_COS;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* cosf(x) and sinf(x) macro override: float __cdecl __sin(float, unsigned int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("__sinf");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = floattype;
    intr->prototype[1] = unsignedtype;
    intr->prototype[2] = NULL;
    intr->rty = floattype;
    intr->id = INTRIN_SINF_COSF;
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);

    /* cosl(x) and sinl(x) macro override: long double __cdecl __sinl(long double, unsigned int) */
    intr = memalloc(sizeof(*intr), PERM);
    intr->name = string("__sinl");
    intr->prototype = memarray(3, sizeof(*intr->prototype), PERM);
    intr->prototype[0] = longdoubletype;
    intr->prototype[1] = unsignedtype;
    intr->prototype[2] = NULL;
    intr->rty = longdoubletype;
    intr->id = INTRIN_SIN_COS;  /* long double == double! */
    intr->flags = INTRIN_MAXSPEED|INTRIN_MINSPACE|INTRIN_FLTOPT;
    intrinsics = listappend(intr, intrinsics);
}

/****************************************************************************
 *                                                                          *
 * Function: intrinsic_call                                                 *
 *                                                                          *
 * Purpose : Map function call to intrinsic version, when possible.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-15  Created                                              *
 *           04-08-10  Bugfix: make sure 3rd arg to memset is ARG+U.        *
 *           04-08-11  Added support for functions without arguments.       *
 *           04-12-12  Check for zero bytes in memset and memcpy calls.     *
 *                                                                          *
 ****************************************************************************/

#define has_side_effects(x)  ((x)->flags & INTRIN_SIDE_EFFECTS)

TREE *intrinsic_call(TREE *f, TYPE *fty, TYPE *rty, TREE *args)
{
    if (intrinsics && IR->wants_intrinsic)
    {
        int optim = options.pragmaopt == MAXSPEED ? INTRIN_MAXSPEED : INTRIN_MINSPACE;
        LIST *lp = intrinsics;
        do
        {
            lp = lp->link;
            /**/
            {
                INTRINSIC *intr = (INTRINSIC *)lp->data;

                /* use explicitly enabled, or filter out disabled or inappropriate intrinsics */
                if ((intr->flags & INTRIN_ENABLED) == 0 && ((intr->flags & INTRIN_DISABLED) != 0 ||
                    (intr->flags & optim) == 0 || ((intr->flags & INTRIN_FLTOPT) && !options.fltopt)))
                    continue;

                /* check name, storage class, calling convention, return type */
                if (f->u.sym->name == intr->name && f->u.sym->sclass != STATIC &&
                    f->u.sym->type->u.fcn.calltype == CDECL_ && rty == intr->rty &&
                    fty->u.fcn.prototype && !fty->u.fcn.oldstyle)
                {
                    TYPE **prototype = fty->u.fcn.prototype;
                    int n;

                    /* check prototype (and count arguments) */
                    for (n = 0; prototype[n] && prototype[n] != voidtype; n++)
                        if (prototype[n] != intr->prototype[n]) break;

                    /* do we have a winner? */
                    if ((!prototype[n] || prototype[n] == voidtype) && !intr->prototype[n])
                    {
                        /* perform some special mappings */
                        if (intr->id == INTRIN_STRLEN && args && args->op == ARG+P && args->kids[0]->op == ADDRG+P)
                        {
                            /* map strlen("constant") to integer constant */
                            is_strconst = FALSE;
                            for_each_symbol(constants, CONSTANTS, unref_strconst, args->kids[0]->u.sym);
                            if (is_strconst) return const_tree(args->kids[0]->u.sym->type->size-1, rty);
                        }
                        else if (intr->id == INTRIN_MEMCPY)
                        {
                            if (args && args->op == ARG+U && args->kids[0]->op == CNST+U)
                            {
                                TREE *e2 = args->kids[1]->kids[0];
                                TREE *e1 = args->kids[1]->kids[1]->kids[0];
                                if ((int)args->kids[0]->u.v.u == 0)
                                {
                                    /* don't copy zero bytes */
                                    return e1;
                                }
                                else
                                {
                                    /* use fake array, and everything happens automatically :-) */
                                    TYPE *ty = new_array(chartype, (int)args->kids[0]->u.v.u, 1);
                                    assert(generic(args->kids[1]->op) == ARG && generic(args->kids[1]->kids[1]->op) == ARG);
                                    return new_tree(RIGHT, voidptype, new_tree(ASGN+B, ty, e1, new_tree(INDIR+B, ty, e2, NULL)), e1);
                                }
                            }
                        }
                        else if (intr->id == INTRIN_MEMSET)
                        {
                            /* we only support clearing memory, but this should be the most common case */
                            if (args && args->op == ARG+U && args->kids[1]->op == ARG+I && args->kids[1]->kids[0]->op == CNST+I && args->kids[1]->kids[0]->u.v.i == 0)
                            {
                                TREE *e3 = args->kids[0];
                                TREE *e1 = args->kids[1]->kids[1]->kids[0];
                                int size = (int)e3->u.v.u;
                                assert(generic(args->kids[1]->kids[1]->op) == ARG);
                                if (e3->op == CNST+U && size == 0)
                                {
                                    /* don't clear zero bytes */
                                    return e1;
                                }
                                else if (e3->op == CNST+U && (size == chartype->size || size == shorttype->size ||
                                    size == inttype->size || size == longlongtype->size))
                                {
                                    /* turn small blocks into assignments */
                                    TYPE *ty = btot(I, size);
                                    TREE *e = retype(rvalue(e1), ty);
                                    return new_tree(RIGHT, voidptype, assignment_tree(ASGN, e, const_tree(0, ty)), lvalue(e));
                                }
                                else
                                {
                                    /* turn function call into *zero* memory call */
                                    TREE *e = new_tree(mkop(INTRIN2S, rty), rty, e1, e3);
                                    e->u.sym = intconst(intr->id);
                                    return e;
                                }
                            }
                        }
                        else if (intr->id == INTRIN_LOG_LOG10_LOG2 || intr->id == INTRIN_LOGF_LOG10F_LOG2F)
                        {
                            /* we might have a special CRT call: __log(0,x) or __log(1,x) */
                            if (args && args->op == ARG+I && args->kids[0]->op == CNST+I)
                            {
                                assert(generic(args->kids[1]->op) == ARG);
                                if (args->kids[0]->u.v.i == 0)  /* log() */
                                {
                                    /* turn function call into a log() intrinsic call */
                                    TREE *e = new_tree(mkop(INTRIN1, rty), rty, args->kids[1]->kids[0], NULL);
                                    e->u.sym = intconst(intr->id == INTRIN_LOG_LOG10_LOG2 ? INTRIN_LOG : INTRIN_LOGF);
                                    return e;
                                }
                                else if (args->kids[0]->u.v.i == 1)  /* log10() */
                                {
                                    /* turn function call into a log10() intrinsic call */
                                    TREE *e = new_tree(mkop(INTRIN1, rty), rty, args->kids[1]->kids[0], NULL);
                                    e->u.sym = intconst(intr->id == INTRIN_LOG_LOG10_LOG2 ? INTRIN_LOG10 : INTRIN_LOG10F);
                                    return e;
                                }
                            }
                        }
                        else if (intr->id == INTRIN_SIN_COS || intr->id == INTRIN_SINF_COSF)
                        {
                            /* we might have a special CRT call: __sin(0,x) or __sin(1,x) */
                            if (args && args->op == ARG+U && args->kids[0]->op == CNST+U)
                            {
                                assert(generic(args->kids[1]->op) == ARG);
                                if (args->kids[0]->u.v.u == 0)  /* sin() */
                                {
                                    /* turn function call into a sin() intrinsic call */
                                    TREE *e = new_tree(mkop(INTRIN1, rty), rty, args->kids[1]->kids[0], NULL);
                                    e->u.sym = intconst(intr->id == INTRIN_SIN_COS ? INTRIN_SIN : INTRIN_SINF);
                                    return e;
                                }
                                else if (args->kids[0]->u.v.u == 1)  /* cos() */
                                {
                                    /* turn function call into a cos() intrinsic call */
                                    TREE *e = new_tree(mkop(INTRIN1, rty), rty, args->kids[1]->kids[0], NULL);
                                    e->u.sym = intconst(intr->id == INTRIN_SIN_COS ? INTRIN_COS : INTRIN_COSF);
                                    return e;
                                }
                            }
                        }
                        else if (n == 0)
                        {
                            /* turn function call into an intrinsic call */
                            TREE *e;
                            e = new_tree(mkop(has_side_effects(intr) ? INTRIN1S : INTRIN1, rty), rty, NULL, NULL);
                            e->u.sym = intconst(intr->id);
                            return e;
                        }
                        else if (n == 1 && args)
                        {
                            /* turn function call into an intrinsic call */
                            TREE *e;
                            assert(generic(args->op) == ARG);
                            e = new_tree(mkop(has_side_effects(intr) ? INTRIN1S : INTRIN1, rty), rty, args->kids[0], NULL);
                            e->u.sym = intconst(intr->id);
                            return e;
                        }
                        else if (n == 2 && args)
                        {
                            /* turn function call into an intrinsic call */
                            TREE *e;
                            assert(generic(args->op) == ARG && generic(args->kids[1]->op) == ARG);
                            e = new_tree(mkop(has_side_effects(intr) ? INTRIN2S : INTRIN2, rty),
                                rty, args->kids[1]->kids[0], args->kids[0]);
                            e->u.sym = intconst(intr->id);
                            return e;
                        }
                    }
                    break;
                }
            }
        } while (lp != intrinsics);
    }

    return NULL;
}

#undef has_side_effects

/****************************************************************************
 *                                                                          *
 * Function: unref_strconst                                                 *
 *                                                                          *
 * Purpose : Decrease the reference count for a string constant.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void unref_strconst(SYMBOL *sym, void *cl)
{
    if (sym->u.c.loc == (SYMBOL *)cl)
    {
        is_strconst = TRUE;
        sym->ref--;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: enable_or_disable_intrinsic                                    *
 *                                                                          *
 * Purpose : Enable or disable the specified intrinsic function.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t enable_or_disable_intrinsic(bool_t enable, const char *name)
{
    if (intrinsics)
    {
        LIST *lp = intrinsics;
        do
        {
            INTRINSIC *intr = (INTRINSIC *)lp->data;
            if (strcmp(intr->name, name) == 0)
            {
                intr->flags &= ~(INTRIN_DISABLED|INTRIN_ENABLED);
                intr->flags |= (enable) ? INTRIN_ENABLED : INTRIN_DISABLED;
                return TRUE;
            }
        } while ((lp = lp->link) != intrinsics);
    }
    return FALSE;
}
