* Automatically translated from _defs.h by hConvert.EXE
* (Copyright PC Wise Technology) AJ Wos (andrwos@global.co.za) 1998-2000
* Fitness for any particular purpose is not guaranteed nor implied.
* It is recommended to verify the correctness of the file before use.

/*  _defs.h

    Common definitions for pointer size and calling conventions.

    Calling conventions:
    _RTLENTRY       Specifies the calling convention used by the RTL

    _USERENTRY      Specifies the calling convention the RTL expects user
                    compiled functions to use (for callbacks)

    Export (and size for DOS) information:
    _EXPCLASS       Exports class if building DLL version of library
                    For DOS16 also provides size information

    _EXPDATA        Exports data if building DLL version of library

    _EXPFUNC        Exports function if building DLL version of library
                    For DOS16 also provides size information

    _FAR            Promotes data pointers to far in DLLs (DOS16 only)

    Obsolete versions:
    _Cdecl          Use _RTLENTRY
    _CLASSTYPE      Use _EXPCLASS
    _FARFUNC        Use _EXPFUNC
    _FARCALL        Use _EXPFUNC and declare function explicity __far
*/

/*
 *      C/C++ Run Time Library - Version 10.0
 *
 *      Copyright (c) 1991, 2000 by Inprise Corporation
 *      All Rights Reserved.
 *
 */

// $Revision:   9.4  $

#ifndef ___DEFS_H
#define ___DEFS_H

#define _RTLENTRY  __cdecl
#define _USERENTRY __cdecl

#ifdef __PAS__


#else


#endif

#define _FAR
#ifdef _BUILDRTLDLL


#else


#endif


#define _EXPFUNC32  _EXPFUNC
#define _EXPFUNC16


#define _Cdecl      _RTLENTRY
#define _CType      _RTLENTRYF
#define _CLASSTYPE  _EXPCLASS
#define _FARFUNC    _EXPFUNC
#define _FARCALL    _EXPFUNC __far


#ifdef __cplusplus


#if 0
//********** Obsolete definitions for OWL 1.0 ************

           
#endif // 0
#endif

#endif  // ___DEFS_H
