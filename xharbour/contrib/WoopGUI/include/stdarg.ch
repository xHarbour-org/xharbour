* Automatically translated from stdarg.h by hConvert.EXE
* (Copyright PC Wise Technology) AJ Wos (andrwos@global.co.za) 1998-2000
* Fitness for any particular purpose is not guaranteed nor implied.
* It is recommended to verify the correctness of the file before use.

/*  stdarg.h

    Definitions for accessing parameters in functions that accept
    a variable number of arguments.

*/

/*
 *      C/C++ Run Time Library - Version 10.0
 *
 *      Copyright (c) 1987, 2000 by Inprise Corporation
 *      All Rights Reserved.
 *
 */

// $Revision:   9.2  $

#ifndef __STDARG_H
#define __STDARG_H
#define _INC_STDARG  // MSC Guard name

#ifdef __VARARGS_H
#error Can't include both STDARG.H and VARARGS.H
#endif

#ifndef ___STDDEF_H
#include "_stddef.ch"
#endif

#ifdef __cplusplus

#endif // __cplusplus


#define __size(x) ((sizeof(x)+sizeof(int)-1) * ~(sizeof(int)-1))

#define va_start(ap, parmN) ((void)((ap) = (va_list)((char _FAR *)(&parmN)+__size(parmN))))
#define va_arg(ap, type) (*(type _FAR *)(((*(char _FAR *_FAR *)&(ap))+=__size(type))-(__size(type))))
#define va_end(ap)          ((void)0)

#ifndef __STDC__
#define _va_ptr             (...)
#endif

