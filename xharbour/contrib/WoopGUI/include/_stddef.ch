* Automatically translated from _stddef.h by hConvert.EXE
* (Copyright PC Wise Technology) AJ Wos (andrwos@global.co.za) 1998-2000
* Fitness for any particular purpose is not guaranteed nor implied.
* It is recommended to verify the correctness of the file before use.

/*  _stddef.h

    multi-includable Definitions for common types, and NULL

*/

/*
 *      C/C++ Run Time Library - Version 10.0
 *
 *      Copyright (c) 1987, 2000 by Inprise Corporation
 *      All Rights Reserved.
 *
 */

// $Revision:   9.2  $

#ifndef ___STDDEF_H
#define ___STDDEF_H

#ifndef ___DEFS_H
#include "_defs.ch"
#endif

#ifndef NULL
#include "_null.ch"
#endif

/* Full locale support is on by default now.  To get the previous behavior,
   define __SIMPLE_LOCALES__
*/
#ifndef __SIMPLE_LOCALES__
#define __USELOCALES__
#endif

#ifdef __cplusplus

#endif // __cplusplus

// Define __STD to expand to std:: or nothing depending on being in C++.
#ifdef __cplusplus

#else

#endif

/*
   Define the size_t type in the std namespace if in C++ or globally if in C.
   If we're in C++, make the _SIZE_T macro expand to std::size_t
*/


#ifndef _PTRDIFF_T
#define _PTRDIFF_T

#endif


#define offsetof( s_name, m_name )  (_SIZE_T)&(((s_name _FAR *)0)->m_name)

#ifndef __cplusplus

/*
   Define the wint_t type in the std namespace if in C++ or globally if in C.
   If we're in C++, make the _WINT_T macro expand to std::wint_t
*/


#ifndef _WCTYPE_T_DEFINED

#define _WCTYPE_T_DEFINED
#endif


#define _threadid (__threadid())



