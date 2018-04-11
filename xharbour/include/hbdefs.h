/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for compiler and runtime basic type declarations
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_DEFS_H_
#define HB_DEFS_H_

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hbsetup.h"

/* Alternative memcpy */
#if defined( HB_NO_DV_MEMCPY )
   #define HB_MEMCPY  memcpy
#else
   #define HB_MEMCPY  dv_memcpy
#endif

#if defined( __XCC__ ) || defined( __POCC__ ) || defined( __LCC__ ) || \
    defined( __MINGW32__ ) || defined( __DMC__ ) || defined( __TINYC__ ) || \
    ( defined( _MSC_VER ) && _MSC_VER >= 1600 ) || \
    ( defined( __BORLANDC__ ) && __BORLANDC__ >= 0x582 ) || \
    ( defined( __WATCOMC__ ) && __WATCOMC__ >= 1270 ) || \
    ( ( defined( __GNUC__ ) || defined( __SUNPRO_C ) || defined( __SUNPRO_CC ) ) && \
      ( defined( _ISOC99_SOURCE ) || defined( _STDC_C99 ) || \
        ( defined( __STDC_VERSION__ ) && __STDC_VERSION__ >= 199901L ) || \
        ( defined( __DJGPP__ ) && \
          ( __DJGPP__ > 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ >= 4 ) ) ) || \
        defined( HB_OS_LINUX ) || defined( HB_OS_DARWIN ) || \
        defined( HB_OS_BSD ) || defined( HB_OS_SUNOS ) || \
        defined( HB_OS_BEOS ) || defined( HB_OS_QNX ) || \
        defined( HB_OS_VXWORKS ) || defined( HB_OS_MINIX ) ) )
   #undef  HAVE_INTTYPES_H
   #define HAVE_INTTYPES_H           1
   #undef  HAVE_STDINT_H
   #define HAVE_STDINT_H             1

   #  include <stdint.h>
   #if defined( _MSC_VER )
      #  if defined( _MSC_VER ) && _MSC_VER >= 1400
         #  include <intrin.h>
      #  endif
      #undef  HAVE_INTTYPES_H
      #define HAVE_INTTYPES_H        0
      #if ( _MSC_VER >= 1600 )
         #undef  HAVE_STDINT_H
         #define HAVE_STDINT_H       1
      #else
         #undef  HAVE_STDINT_H
         #define HAVE_STDINT_H       0
         #if ( _MSC_VER > 1400 )
            #define HAVE_INTSAFE_H   1
         #endif
      #endif
   #else
      #undef  HAVE_STDINT_H
      #define HAVE_STDINT_H          0
      #undef  HAVE_INTTYPES_H
      #define HAVE_INTTYPES_H        0
      #undef  INT64_MAX

   #endif
   /* NOTE: Hack to avoid collision between stdint.h and unistd.h. */
   #  if defined( HB_OS_VXWORKS ) && defined( _INTPTR ) && ! defined( _INTPTR_T )
   #     define _INTPTR_T
   #  endif

/* for backward compatibility */
#define s_defaultGT     hb_gt_szNameDefault

/* Compatibility. Do not use HB_OS_WIN_32_USED anymore. */
#ifdef HB_LEGACY_LEVEL2
   #if defined( HB_OS_WIN_32_USED ) && ! defined( HB_OS_WIN_USED )
      #define HB_OS_WIN_USED
   #endif
#endif


   /* workaround for BCC 5.8 bug */
   #if ( defined( __BORLANDC__ ) && __BORLANDC__ >= 0x582 )
      #undef INT32_MIN
      #define INT32_MIN ((int32_t) (-INT32_MAX-1))
      #undef INT64_MIN
      #define INT64_MIN (9223372036854775807i64-1)
      #undef INT64_MAX
      #define INT64_MAX 9223372036854775807i64
   #endif
#endif

#  if defined(__BORLANDC__) && __BORLANDC__ < 0x560

    typedef __int32 int32_t;
    typedef __int64 int64_t;

    typedef unsigned __int8 uint8_t;
    typedef unsigned __int16 uint16_t;
    typedef unsigned __int32 uint32_t;
    typedef unsigned __int64 uint64_t;
    typedef int32_t intptr_t;
    typedef uint32_t uintptr_t;

#  endif

#if (_MSC_VER < 1300) && !defined(HAVE_STDINT_H)


   #ifdef _WIN64 // [
      typedef signed __int64    intptr_t;
      typedef unsigned __int64  uintptr_t;
   #else // _WIN64 ][
      typedef  signed int   intptr_t;
      typedef  unsigned int uintptr_t;
   #endif // _WIN64 ]
#endif

/*
#define HB_CLIPPER_INT_ITEMS
#define HB_LONG_LONG_OFF
*/

#if defined( HB_OS_WIN ) || defined( HB_OS_WIN_64 )
   #if defined( HB_OS_WIN_64 )
      #undef HB_LONG_LONG_OFF
      #define HB_STRICT_ALIGNMENT
      #if !defined( HB_OS_WIN )
         #define HB_OS_WIN
      #endif
   #endif

   #if !defined( HB_WIN32_IO_OFF )
      #define HB_WIN32_IO
   #endif
   #if defined( HB_WIN32_IO ) && !defined( HB_OS_WIN_USED )
      /* disabled to avoid problems with windows.h */
      /* #define HB_OS_WIN_USED */
   #endif
#else
   #undef HB_WIN32_IO
   #undef HB_OS_WIN_USED
#endif

/* Include windows.h if applicable and requested */
#if defined( HB_OS_WIN_USED ) && defined( HB_OS_WIN )

   #define WIN32_LEAN_AND_MEAN
//    #define _WINSOCKAPI_  /* Prevents inclusion of Winsock.h in Windows.h */
// Don't move, must precede any #include of Windows to fix CINTERFACE support in guiddef.h and propkeydef.h!!!
   #include "cinterface.h"
   #include <windows.h>
   #if defined( __GNUC__ )
      #define HB_DONT_DEFINE_BASIC_TYPES
   #endif

#elif defined( HB_OS_OS2 )

   /* With the exception of WORD, the IBM Visual Age C++ compiler has
      its own definitions of the Harbour types most of which conflict with the
      Harbour #undefs, due to typedef being the prevalent method of
      defining the types in IBMCPP, whereas Harbour assumes that the
      definitions that it is replacing have been defined using
      #define. Therefore, it is necessary to skip the Harbour
      definition section when using the IBMCPP compiiler, include
      the IBMCPP type definitions, and then add the definition for WORD

      NOTE: This only applies to the common types that most C compilers
            define. Any new types, particulary those that start with
            HB_, must be placed AFTER the #endif __IBMCPP__ line!
   */
   /* 28/03/2000 - maurilio.longo@libero.it
      The same holds true when using GCC under OS/2
   */
   #define INCL_BASE
   #define INCL_DOS
   #define INCL_DOSMISC

   #include <os2.h>
   #define HB_DONT_DEFINE_BASIC_TYPES

#elif defined( HB_OS_DOS )

   #include <dos.h>

   #if defined( __WATCOMC__ ) && defined( __386__ ) && !defined( __WINDOWS_386__ )
      #define HB_DOS_INT86 int386
      #define HB_DOS_INT86X int386x
      #define HB_XREGS w
   #elif defined( __RSX32__ )
      #define HB_DOS_INT86 _int86
      #define HB_DOS_INT86X _int86x
      #define HB_XREGS x
   #elif defined( __DJGPP__ )
      #define HB_DOS_INT86 int86
      #define HB_DOS_INT86X int86x
      #define HB_XREGS w
   #else
      #define HB_DOS_INT86 int86
      #define HB_DOS_INT86X int86x
      #define HB_XREGS x
   #endif

#elif defined( HB_OS_DARWIN )

   /* Detect if it is Darwin < 6.x */
   #include <pthread.h>
   #ifndef PTHREAD_MUTEX_RECURSIVE
      #define HB_OS_DARWIN_5
   #endif

#endif

#if ! defined( HB_DONT_DEFINE_BASIC_TYPES )

   #if ! defined( HB_DONT_DEFINE_BOOL )
      #undef BOOL                         /* boolean */
      typedef int BOOL;
   #endif

   #undef UINT                            /* varies with platform */
   typedef unsigned int UINT;

   #undef SCHAR                           /* 1 byte signed */
   typedef signed char SCHAR;

   #undef UCHAR                           /* 1 byte unsigned */
   typedef unsigned char UCHAR;

   #if ! defined( HB_DONT_DEFINE_BYTE )
      #undef BYTE                            /* 1 byte unsigned */
      typedef unsigned char BYTE;
   #endif

   #undef SHORT                           /* 2 bytes signed */
   typedef signed short int SHORT;

   #undef USHORT                          /* 2 bytes unsigned */
   typedef unsigned short int USHORT;

   #if ! defined( HB_DONT_DEFINE_LONG )
      #undef LONG                         /* 4 or 8 bytes signed */
      typedef long LONG;
   #endif

   #undef ULONG                           /* 4 or 8 bytes unsigned */
   typedef unsigned long ULONG;

   #undef FALSE
   #define FALSE  0
   #undef TRUE
   #define TRUE   (!0)

#else  /* HB_DONT_DEFINE_BASIC_TYPES */

   /* 21/02/07 - <maurilio.longo@libero.it>
                 Needed when HB_DONT_DEFINE_BASIC_TYPES is defined but
                 some definition is missing from the include files which
                 require the use of HB_DONT_DEFINE_BASIC_TYPES in the first place.
                 Here SCHAR is needed using GCC on OS/2
   */

   /* SCHAR is needed using GCC on OS/2 */
   #if ! defined( SCHAR )
      typedef signed char SCHAR;          /* 1 byte signed */
   #endif

#endif /* HB_DONT_DEFINE_BASIC_TYPES */

#ifndef HB_LONG_LONG_OFF

 /*  #if ! defined( HB_DONT_DEFINE_BASIC_TYPES ) && ! defined( _WINNT_H )
      #if !defined( LONGLONG )
         #if defined( __GNUC__ ) || defined( __XCC__ )
            typedef signed long long LONGLONG;
         #else
            typedef __int64 LONGLONG;
         #endif
      #endif
      #if !defined( ULONGLONG )
         #if defined( __GNUC__ ) || defined( __XCC__ )
            typedef unsigned long long ULONGLONG;
         #else
            typedef unsigned __int64 ULONGLONG;
         #endif
      #endif
   #endif
*/
   #if defined( HB_OS_WIN ) && ! defined( __GNUC__ )
      typedef __int64            LONGLONG;
      typedef unsigned __int64   ULONGLONG;
   #else
      typedef signed long long   LONGLONG;
      typedef unsigned long long ULONGLONG;
   #endif

   #if !defined( ULONGLONG_MAX )
      #if defined( _UI64_MAX )
         #define ULONGLONG_MAX      _UI64_MAX
      #elif defined( ULLONG_MAX )
         #define ULONGLONG_MAX      ULLONG_MAX
      #elif defined( ULONG_LONG_MAX )
         #define ULONGLONG_MAX      ULONG_LONG_MAX
      #else
         #define ULONGLONG_MAX      18446744073709551615ULL
      #endif
   #endif
   #if !defined( LONGLONG_MAX )
      #if defined( _I64_MAX )
         #define LONGLONG_MAX       _I64_MAX
      #elif defined( LLONG_MAX )
         #define LONGLONG_MAX       LLONG_MAX
      #elif defined( LONG_LONG_MAX )
         #define LONGLONG_MAX       LONG_LONG_MAX
      #else
         #define LONGLONG_MAX       9223372036854775807LL
      #endif
   #endif
   #if !defined( LONGLONG_MIN )
      #if defined( _I64_MIN )
         #define LONGLONG_MIN       _I64_MIN
      #elif defined(LLONG_MIN)
         #define LONGLONG_MIN       LLONG_MIN
      #elif defined(LONG_LONG_MIN)
         #define LONGLONG_MIN       LONG_LONG_MIN
      #else
         #define LONGLONG_MIN       (-LONGLONG_MAX - 1LL)
      #endif
   #endif

#endif /* HB_LONG_LONG_OFF */

/*
 * below are some hacks which don't have to be true on some machines
 * please update it if necessary
 */
#if defined( HB_OS_WIN_64 )
#  define HB_ARCH_64BIT
#elif ULONG_MAX > UINT_MAX && UINT_MAX > USHRT_MAX
#  define HB_ARCH_64BIT
#elif ULONG_MAX == UINT_MAX && UINT_MAX > USHRT_MAX
#  define HB_ARCH_32BIT
#elif ULONG_MAX > UINT_MAX && UINT_MAX == USHRT_MAX
#  define HB_ARCH_16BIT
#endif

#if USHRT_MAX == 0xffff
#  if !defined( UINT16 )
      typedef USHORT       UINT16;
#  endif
#  if !defined( INT16 )
      typedef SHORT        INT16;
#  endif
#  if !defined( UINT16_MAX )
#     define UINT16_MAX    USHRT_MAX
#  endif
#  if !defined( INT16_MAX )
#     define INT16_MAX     SHRT_MAX
#  endif
#  if !defined( INT16_MIN )
#     define INT16_MIN     SHRT_MIN
#  endif
#endif

#if UINT_MAX == 0xFFFFFFFF
#  if !defined( UINT32 )
      typedef UINT         UINT32;
#  endif
#  if !defined( INT32 )
      typedef signed int   INT32;
#  endif
#  if !defined( UINT32_MAX )
#     define UINT32_MAX    UINT_MAX
#  endif
#  if !defined( INT32_MAX )
#     define INT32_MAX     INT_MAX
#  endif
#  if !defined( INT32_MIN )
#     define INT32_MIN     INT_MIN
#  endif
#elif ULONG_MAX == 0xFFFFFFFF
#  if !defined( UINT32 )
      typedef ULONG        UINT32;
#  endif
#  if !defined( INT32 )
      typedef LONG         INT32;
#  endif
#  if !defined( UINT32_MAX )
#     define UINT32_MAX    ULONG_MAX
#  endif
#  if !defined( INT32_MAX )
#     define INT32_MAX     LONG_MAX
#  endif
#  if !defined( INT32_MIN )
#     define INT32_MIN     LONG_MIN
#  endif
#endif

#if !defined( UCHAR_MAX )
#  define UCHAR_MAX     0x0FF
#endif
#if !defined( UINT24_MAX )
#  define UINT24_MAX    0x0FFFFFFL
#endif
#if !defined( INT24_MAX )
#  define INT24_MAX     8388607L
#endif
#if !defined( INT24_MIN )
#  define INT24_MIN     -8388608L
#endif

#if defined( HB_ARCH_64BIT ) && !defined( HB_OS_WIN_64 )
#  if !defined( UINT64 )
     typedef unsigned long          UINT64;
#  endif
#  if !defined( INT64 )
     typedef signed long          INT64;
#  endif
#  if !defined( UINT64_MAX )
#     define UINT64_MAX    ULONG_MAX
#  endif
#  if !defined( INT64_MAX )
#     define INT64_MAX     LONG_MAX
#  endif
#  if !defined( INT64_MIN )
#     define INT64_MIN     LONG_MIN
#  endif
#elif !defined( HB_LONG_LONG_OFF )
#  if !defined( UINT64 )
     typedef ULONGLONG     UINT64;
#  endif
#  if !defined( INT64 )
     typedef LONGLONG      INT64;
#  endif
#  if !defined( UINT64_MAX )
#     define UINT64_MAX     ULONGLONG_MAX
#  endif
#  if !defined( INT64_MAX )
#     define INT64_MAX      LONGLONG_MAX
#  endif
#  if !defined( INT64_MIN )
#     define INT64_MIN      LONGLONG_MIN
#  endif
#endif

#ifndef HB_LONG_DOUBLE_OFF
   typedef long double  HB_MAXDBL;
#else
   typedef double       HB_MAXDBL;
#endif

#if defined( HB_CLIPPER_INT_ITEMS )
#  define HB_INT_MAX             SHRT_MAX
#  define HB_INT_MIN             SHRT_MIN
#  define HB_UINT_MAX            USHRT_MAX
#  define HB_LONG_MAX            LONG_MAX
#  define HB_LONG_MIN            LONG_MIN
#  define HB_ULONG_MAX           ULONG_MAX
   typedef long                          HB_LONG;
   typedef unsigned long                 HB_ULONG;
#  define PFHL                   "l"
#elif !defined( HB_LONG_LONG_OFF ) && ULONG_MAX == UINT_MAX
#  define HB_INT_MAX             INT_MAX
#  define HB_INT_MIN             INT_MIN
#  define HB_UINT_MAX            UINT_MAX
#  define HB_LONG_MAX            LONGLONG_MAX
#  define HB_LONG_MIN            LONGLONG_MIN
#  define HB_ULONG_MAX           ULONGLONG_MAX
   typedef LONGLONG              HB_LONG;
   typedef ULONGLONG             HB_ULONG;
#else
#  define HB_INT_MAX             INT_MAX
#  define HB_INT_MIN             INT_MIN
#  define HB_UINT_MAX            UINT_MAX
#  define HB_LONG_MAX            LONG_MAX
#  define HB_LONG_MIN            LONG_MIN
#  define HB_ULONG_MAX           ULONG_MAX
   typedef long                          HB_LONG;
   typedef unsigned long                 HB_ULONG;
#  define PFHL                   "l"
#endif

#define HB_DBL_LIM_INT(d)     ( HB_INT_MIN <= (d) && (d) <= HB_INT_MAX )
#define HB_DBL_LIM_LONG(d)    ( (HB_MAXDBL) HB_LONG_MIN <= (HB_MAXDBL) (d) && (HB_MAXDBL) (d) <= (HB_MAXDBL) HB_LONG_MAX )
#define HB_LIM_INT(l)         ( HB_INT_MIN <= (l) && (l) <= HB_INT_MAX )
#define HB_LIM_LONG(l)        ( HB_LONG_MIN <= (l) && (l) <= HB_LONG_MAX )

#define HB_DBL_LIM_INT8(d)    ( -128 <= (d) && (d) <= 127 )
#define HB_DBL_LIM_INT16(d)   ( INT16_MIN <= (d) && (d) <= INT16_MAX )
#define HB_DBL_LIM_INT24(d)   ( INT24_MIN <= (d) && (d) <= INT24_MAX )
#define HB_DBL_LIM_INT32(d)   ( INT32_MIN <= (d) && (d) <= INT32_MAX )
#define HB_DBL_LIM_INT64(d)   ( (HB_MAXDBL) INT64_MIN <= (HB_MAXDBL) (d) && (HB_MAXDBL) (d) <= (HB_MAXDBL) INT64_MAX )
#define HB_LIM_INT8(l)        ( -128 <= (l) && (l) <= 127 )
#define HB_LIM_INT16(l)       ( INT16_MIN <= (l) && (l) <= INT16_MAX )
#define HB_LIM_INT24(l)       ( INT24_MIN <= (l) && (l) <= INT24_MAX )
#define HB_LIM_INT32(l)       ( INT32_MIN <= (l) && (l) <= INT32_MAX )
#define HB_LIM_INT64(l)       ( INT64_MIN <= (l) && (l) <= INT64_MAX )

/*
 * It's a hack for compilers which don't support LL suffix for LONGLONG
 * numeric constant. This suffix is necessary for some compilers -
 * without it they cut the number to LONG
 */
#if defined( __BORLANDC__ )
#  if __BORLANDC__ >= 0x530
#     define HB_LL( num )           num##i64
#     define HB_ULL( num )          num##ui64
#  else
#     define HB_LL( num )           num
#     define HB_ULL( num )          num
#  endif
#elif defined( _MSC_VER )
#  define HB_LL( num )           num
#  define HB_ULL( num )          num
#else
#  define HB_LL( num )           num##LL
#  define HB_ULL( num )          num##ULL
#endif


/* HB_*_EXPLENGTH() macros are used by HVM to set the size of
 * math operations, HB_*_LENGTH() macros are used when new
 * item is created. [druzus]
 */
/* NOTE: the positive number limit 999999999 in HB_INT_LENGTH()
 *       (HB_LONG_LENGTH() on 16-bit platforms) below is not
 *       compatible with other limits. Clipper have such limit
 *       but IMHO it's result of some typo or wrong compiler
 *       warnings cleanup when someone removed one digit from
 *       upper limit instead of removing the whole limit.
 *       It's also possible that it comes from DBASE and was
 *       intentionally replicated. I think we should keep it
 *       only in strict compatibility mode. [druzus]
 */
#if HB_INT_MIN < -999999999
#  define HB_INT_LENGTH( i )        ( ( (i) < -999999999 || (i) > 999999999 ) ? 20 : 10 )
#else
#  define HB_INT_LENGTH( i )        10
#  define HB_INT_EXPLENGTH( i )     10
#  if HB_LONG_MIN < -999999999
#     define HB_LONG_LENGTH( i )    ( ( (i) < -999999999 || (i) > 999999999 ) ? 20 : 10 )
#  endif
#endif

#if !defined( HB_LONG_LONG_OFF )
#  if HB_LONG_MAX > HB_LL( 9999999999 )
#     define HB_LONG_LENGTH( l )    ( ( (l) < -999999999 || (l) > HB_LL( 9999999999 ) ) ? 20 : 10 )
#  endif
#  if HB_INT_MAX > HB_LL( 9999999999 )
#     define HB_INT_EXPLENGTH( i )  HB_LONG_LENGTH( i )
#  endif
#endif

#if !defined( HB_LONG_LENGTH )
#  define HB_LONG_LENGTH( l )       ( ( (l) < -999999999 ) ? 20 : 10 )
#endif
#if !defined( HB_INT_EXPLENGTH )
#  define HB_INT_EXPLENGTH( i )     ( ( (i) < -999999999 ) ? 20 : 10 )
#endif
#if !defined( HB_LONG_EXPLENGTH )
#  define HB_LONG_EXPLENGTH( l ) HB_LONG_LENGTH( l )
#endif

/* HB_DBL_LENGTH() is used by VAL() for strings longer then 10 characters
 * (counted to '.') and to set the size of math operations and new
 * double item - it's CA-Cl*pper compatible range. For doubles we do
 * not have separated limit for result of math operations. [druzus]
 */
#define HB_DBL_LENGTH( d ) ( ( (d) > 9999999999.0 || (d) < -999999999.0 ) ? 20 : 10 )

/* uncomment this if you need strict Clipper compatibility */
/* #define PCODE_LONG_LIM(l)     HB_LIM_INT32( l ) */

/* #define PCODE_LONG_LIM(l)     HB_LIM_LONG( l ) */

/* type of HB_ITEM */
/* typedef USHORT HB_TYPE; */
typedef UINT32 HB_TYPE;
typedef UINT32 HB_FATTR;

/* type of reference counter */

#if defined( HB_OS_WIN_64 )
   typedef ULONGLONG    HB_COUNTER;
#  define HB_COUNTER_SIZE  8
#else
   typedef unsigned long   HB_COUNTER;
#  if ULONG_MAX <= UINT32_MAX
#     define HB_COUNTER_SIZE  4
#  else
#     define HB_COUNTER_SIZE  8
#  endif
#endif


/* type for memory pointer diff */
#if defined( HB_OS_WIN_64 )
   typedef LONGLONG HB_PTRDIFF;
   typedef ULONGLONG HB_PTRUINT;
#else
   typedef long HB_PTRDIFF;
   typedef unsigned long HB_PTRUINT;
#endif

#if defined( HB_LONG_LONG_OFF ) || ULONG_MAX == ULONGLONG_MAX
   typedef LONG HB_FOFFSET;
   /* we can add hack with double as work around what should
      effectively give 52bit file size limit */
#else
   typedef LONGLONG HB_FOFFSET;
#endif

#if defined( HB_WIN32_IO ) || defined( HB_OS_WIN )
   typedef HB_PTRDIFF HB_FHANDLE;
   typedef HB_PTRDIFF HB_NHANDLE;
#  define hb_numToHandle( h )   ( ( HB_FHANDLE ) ( HB_NHANDLE ) ( h ) )
#else
   typedef int HB_FHANDLE;
   typedef int HB_NHANDLE;
#  define hb_numToHandle( h )   ( ( int ) ( h ) )
#endif

#define FHANDLE                 HB_FHANDLE


/* maximum index size */
#if defined( HB_OS_WIN_64 )
#  if defined( HB_SIZE_SIGNED )
#     define HB_SIZE_MAX    LONGLONG_MAX
#  else
#     define HB_SIZE_MAX    ULONGLONG_MAX
#  endif
#else
#  if defined( HB_SIZE_SIGNED )
#     define HB_SIZE_MAX    LONG_MAX
#  else
#     define HB_SIZE_MAX    ULONG_MAX
#  endif
#endif
/* maximum length of double number in decimal representation:
   log10(2^1024) ~ 308.25 */
#define HB_MAX_DOUBLE_LENGTH 320

/* This value is used to hack the double FL value in round/int
   operation - similar thing is done by CL5.3 - I do not know
   only the exact factor value but it should be close to this one.
   When HB_C52_STRICT is set this macro is not used.
*/
#define HB_DBLFL_PREC_FACTOR 1.0000000000000002;

/* try to detect byte order if not explicitly set */
#if ! defined( HB_PDP_ENDIAN ) && ! defined( HB_BIG_ENDIAN ) && \
    ! defined( HB_LITTLE_ENDIAN )

   /* I intentionaly move the first two #if/#elif to the begining
      to avoid compiler error when this macro will be defined as
      empty statement in next conditions, F.e. SunOS
    */
#  if ( defined( __LITTLE_ENDIAN__ ) && ! defined( __BIG_ENDIAN__ ) ) || \
      ( defined( __LITTLE_ENDIAN ) && ! defined( __BIG_ENDIAN ) ) || \
      ( defined( _LITTLE_ENDIAN ) && ! defined( _BIG_ENDIAN ) ) || \
      ( defined( LITTLE_ENDIAN ) && ! defined( BIG_ENDIAN ) )

#     define HB_LITTLE_ENDIAN

#  elif ( ! defined( __LITTLE_ENDIAN__ ) && defined( __BIG_ENDIAN__ ) ) || \
        ( ! defined( __LITTLE_ENDIAN ) && defined( __BIG_ENDIAN ) ) || \
        ( ! defined( _LITTLE_ENDIAN ) && defined( _BIG_ENDIAN ) ) || \
        ( ! defined( LITTLE_ENDIAN ) && defined( BIG_ENDIAN ) )

#     define HB_BIG_ENDIAN

#  elif ( defined( __BYTE_ORDER ) && defined( __LITTLE_ENDIAN ) && __BYTE_ORDER == __LITTLE_ENDIAN ) || \
        ( defined( _BYTE_ORDER ) && defined( _LITTLE_ENDIAN ) && _BYTE_ORDER == _LITTLE_ENDIAN ) || \
        ( defined( BYTE_ORDER ) && defined( LITTLE_ENDIAN ) && BYTE_ORDER == LITTLE_ENDIAN )

#     define HB_LITTLE_ENDIAN

#  elif ( defined( __BYTE_ORDER ) && defined( __BIG_ENDIAN ) && __BYTE_ORDER == __BIG_ENDIAN ) || \
        ( defined( _BYTE_ORDER ) && defined( _BIG_ENDIAN ) && _BYTE_ORDER == _BIG_ENDIAN ) || \
        ( defined( BYTE_ORDER ) && defined( BIG_ENDIAN ) && BYTE_ORDER == BIG_ENDIAN )

#     define HB_BIG_ENDIAN

#  elif ( defined( __BYTE_ORDER ) && defined( __PDP_ENDIAN ) && __BYTE_ORDER == __PDP_ENDIAN ) || \
        ( defined( _BYTE_ORDER ) && defined( _PDP_ENDIAN ) && _BYTE_ORDER == _PDP_ENDIAN ) || \
        ( defined( BYTE_ORDER ) && defined( PDP_ENDIAN ) && BYTE_ORDER == PDP_ENDIAN )

#     define HB_PDP_ENDIAN

#  else /* We cannot detect byte order, we will have to guess */

#     if defined( HB_OS_DARWIN ) || defined( HB_OS_SUNOS ) || defined( HB_OS_HPUX )
#        define HB_BIG_ENDIAN
#     else
#        define HB_LITTLE_ENDIAN
#     endif

#  endif

#endif

#define HB_MAX( a, b )          ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )
#define HB_MIN( a, b )          ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )

#define HB_LOBYTE( w )          ( ( BYTE ) ( w ) )
#define HB_HIBYTE( w )          ( ( BYTE ) ( ( ( w ) >>  8 ) & 0xFF ) )
#define HB_ULBYTE( w )          ( ( BYTE ) ( ( ( w ) >> 16 ) & 0xFF ) )
#define HB_UHBYTE( w )          ( ( BYTE ) ( ( ( w ) >> 24 ) & 0xFF ) )
#define HB_LOWORD( l )          ( ( UINT16 ) ( l ) )
#define HB_HIWORD( l )          ( ( UINT16 ) ( ( ( l ) >> 16 ) & 0xFFFF ) )
#define HB_MKSHORT( lo, hi )    ( ( SHORT ) ( ( ( INT16 ) ( hi ) ) << 8 ) | ( lo ) )
#define HB_MKUSHORT( lo, hi )   ( ( USHORT ) ( ( ( UINT16 ) ( hi ) ) << 8 ) | ( lo ) )
#define HB_MKLONG( b1, b2, b3, b4 )  ( ( LONG ) \
                                       ( ( ( ( INT32 ) ( b4 ) ) << 24 ) | \
                                         ( ( ( INT32 ) ( b3 ) ) << 16 ) | \
                                         ( ( ( INT32 ) ( b2 ) ) <<  8 ) | \
                                         ( ( ( INT32 ) ( b1 ) ) ) ) )
#define HB_MKULONG( b1, b2, b3, b4 ) ( ( ULONG ) \
                                       ( ( ( ( UINT32 ) ( b4 ) ) << 24 ) | \
                                         ( ( ( UINT32 ) ( b3 ) ) << 16 ) | \
                                         ( ( ( UINT32 ) ( b2 ) ) <<  8 ) | \
                                         ( ( ( UINT32 ) ( b1 ) ) ) ) )

#define HB_SWAP_UINT16( w )     ( ( UINT16 ) ( ( ( ( UINT16 ) ( w ) & 0xFF00 ) >> 8 ) | \
                                               ( ( ( UINT16 ) ( w ) & 0x00FF ) << 8 ) ) )
#define HB_SWAP_UINT32( w )     ( ( UINT32 ) ( ( ( ( UINT32 ) ( w ) & 0x000000FF ) << 24 ) | \
                                               ( ( ( UINT32 ) ( w ) & 0x0000FF00 ) <<  8 ) | \
                                               ( ( ( UINT32 ) ( w ) & 0x00FF0000 ) >>  8 ) | \
                                               ( ( ( UINT32 ) ( w ) & 0xFF000000 ) >> 24 ) ) )


#ifndef PFLL
#  if defined( __BORLANDC__ ) || defined( _MSC_VER ) || defined( __MINGW32__ )
#     define PFLL    "I64"
#  else
#     define PFLL    "ll"
#  endif
#endif
#ifndef PFHL
#  define PFHL    PFLL
#endif

#ifndef HB_PF64
#  define HB_PF64 PFLL
#endif

#if defined( HB_OS_WIN_64 )
#  define HB_PFS  PFLL
#else
#  define HB_PFS  "l"
#endif

#define HB_SWAP_UINT64( w )      ( ( UINT64 ) ( ( ( ( UINT64 ) ( w ) & HB_LL( 0x00000000000000FF ) ) << 56 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x000000000000FF00 ) ) << 40 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x0000000000FF0000 ) ) << 24 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x00000000FF000000 ) ) <<  8 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x000000FF00000000 ) ) >>  8 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x0000FF0000000000 ) ) >> 24 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x00FF000000000000 ) ) >> 40 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0xFF00000000000000 ) ) >> 56 ) ) )

/*
 * on some machines it's not safe to directly access pointers stored
 * at byte buffer they have to be stored at odd (or other alignment)
 * addresses.
 * For example SPARC which needs 4 byte alignment for pointers
 * and 8 byte alignment for doubles and structures (when GCC is used)
 * IMHO need HB_ARCH_<arch> macro yet - the same OS can be used with
 * different architectures - SPARC + LINUX, ALPHA + LINUX
 */
#if !defined( HB_STRICT_ALIGNMENT )
#  if ! defined( HB_CPU_X86 ) && \
      ! defined( HB_CPU_X86_64 )
#     define HB_STRICT_ALIGNMENT
#  endif
#endif

#if defined( HB_STRICT_ALIGNMENT )
#  if ! defined( HB_ALLOC_ALIGNMENT ) || ( HB_ALLOC_ALIGNMENT + 1 == 1 )
#     define HB_ALLOC_ALIGNMENT     8
#  endif
#endif

#if defined( HB_ALLOC_ALIGNMENT ) && HB_COUNTER_SIZE < HB_ALLOC_ALIGNMENT + 0
#  define HB_COUNTER_OFFSET   HB_ALLOC_ALIGNMENT
#else
#  define HB_COUNTER_OFFSET   HB_COUNTER_SIZE
#endif

#define HB_COUNTER_PTR( p )         ((HB_COUNTER*) ((BYTE *) (p)-HB_COUNTER_OFFSET))

#if defined( HB_PDP_ENDIAN )
   #error PDP-Endian support unimplemented. If you have such machine do it yourself.
#endif

/*
 * These macros are necessary for architectures which need
 * strict alignment for pointers.
 */
#if defined( __GNUC__ )
#  define   HB_PUT_PTR( p, v )     _hb_put_ptr( ( BYTE * ) ( p ), v )
#  define   HB_GET_PTR( p )        _hb_get_ptr( ( const BYTE * ) ( p ) )
#elif ! defined( HB_STRICT_ALIGNMENT )
#  define   HB_PUT_PTR( p, v )      do { *( void ** ) ( p ) = ( void * ) ( v ); } while( 0 )
#  define   HB_GET_PTR( p )         ( *( void ** ) ( p ) )
#else
#if defined( HB_BIG_ENDIAN )
#  if defined( HB_ARCH_64BIT )
#        define   HB_PUT_PTR( p, v )   HB_PUT_BE_UINT64( p, ( UINT64 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_BE_UINT64( p ) )
#     define   HB_PUT_LONG( p, v )  HB_PUT_BE_UINT64( p, ( UINT64 ) ( v ) )
#     define   HB_GET_LONG( p )     HB_GET_BE_UINT64( p )
#  else
#        define   HB_PUT_PTR( p, v )   HB_PUT_BE_UINT32( p, ( UINT32 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_BE_UINT32( p ) )
#     define   HB_PUT_LONG( p, v )  HB_PUT_BE_UINT32( p, ( UINT32 ) ( v ) )
#     define   HB_GET_LONG( p )     HB_GET_BE_UINT32( p )

#  endif
#else
#  if defined( HB_ARCH_64BIT )
#     define   HB_PUT_PTR( p, v )   HB_PUT_LE_UINT64( p, ( UINT64 ) ( v ) )
#     define   HB_GET_PTR( p )      ( ( void * ) HB_GET_LE_UINT64( p ) )
#     define   HB_PUT_LONG( p, v )  HB_PUT_LE_UINT64( p, ( UINT64 ) ( v ) )
#     define   HB_GET_LONG( p )     HB_GET_LE_UINT64( p )

#  else
#     define   HB_PUT_PTR( p, v )   HB_PUT_LE_UINT32( p, ( UINT32 ) ( v ) )
#     define   HB_GET_PTR( p )      ( ( void * ) HB_GET_LE_UINT32( p ) )
#     define   HB_PUT_LONG( p, v )  HB_PUT_LE_UINT32( p, ( UINT32 ) ( v ) )
#     define   HB_GET_LONG( p )     HB_GET_LE_UINT32( p )
#  endif
#  endif
#  endif
#if defined( HB_BIG_ENDIAN )
#  define   HB_PUT_UINT32( p, v )   HB_PUT_BE_UINT32( p, ( UINT32 ) ( v ) )
#  define   HB_GET_UINT32( p )      HB_GET_BE_UINT32( p )
#else
#  define   HB_PUT_UINT32( p, v )   HB_PUT_LE_UINT32( p, ( UINT32 ) ( v ) )
#  define   HB_GET_UINT32( p )      HB_GET_LE_UINT32( p )
#endif

/* Macros to store/retrieve integer and double values at/from byte address */
#if defined( __GNUC__ ) || ( defined( _MSC_VER ) && ( _MSC_VER >= 1400 ) )

#  if ( __GNUC__ > 4 || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 3 ) ) && \
      ! defined( __ICC ) && ! defined( __OPENCC__ ) && ! defined( __PCC__ )
#     define HB_BUILTIN_BSWAP32( n )   __builtin_bswap32( n )
#     define HB_BUILTIN_BSWAP64( n )   __builtin_bswap64( n )
#  elif defined( _MSC_VER )
#     define HB_BUILTIN_BSWAP32( n )   _byteswap_ulong( n )
#     define HB_BUILTIN_BSWAP64( n )   _byteswap_uint64( n )
#  endif

#  if defined( _MSC_VER )
#     define _HB_CAST16 ( UINT16 )
#     define _HB_CAST32 ( UINT32 )
#     define _HB_CAST64 ( UINT64 )
#else
#     define _HB_CAST16
#     define _HB_CAST32
#     define _HB_CAST64
#endif

   typedef union
   {
      void *   val;
#  if defined( HB_ARCH_64BIT )
      BYTE  buf[ 8 ];
#else
      BYTE  buf[ 4 ];
#endif
   } HB_PTRCAST, * PHB_PTRCAST;

   typedef union
   {
      UINT16   val;
      BYTE  buf[ 2 ];
   } UINT16CAST, * PUINT16CAST;

   typedef union
   {
      UINT32   val;
      BYTE  buf[ 4 ];
   } UINT32CAST, * PUINT32CAST;

#  if ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT )
   typedef union
   {
      UINT64   val;
      BYTE  buf[ 8 ];
   } UINT64CAST, * PUINT64CAST;
#  endif

   typedef union
   {
      double   val;
      BYTE  buf[ 8 ];
#  if ( ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT ) ) && \
      defined( HB_BUILTIN_BSWAP64 )
      UINT64   i64;
#  endif
   } HB_DBLCAST, * PHB_DBLCAST;

   static HB_FORCEINLINE void * _hb_get_ptr( const BYTE * buf )
   {
      HB_PTRCAST u;
      memcpy( u.buf, buf, sizeof( void * ) );
      return u.val;
   }

   static HB_FORCEINLINE void _hb_put_ptr( BYTE * buf, void * val )
   {
      HB_PTRCAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( void * ) );
   }

   static HB_FORCEINLINE UINT16 _hb_get_std_uint16( const BYTE * buf )
   {
      UINT16CAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static HB_FORCEINLINE void _hb_put_std_uint16( BYTE * buf, UINT16 val )
   {
      UINT16CAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static HB_FORCEINLINE UINT16 _hb_get_rev_uint16( const BYTE * buf )
   {
      UINT16CAST u;
      u.buf[ 0 ] = buf[ 1 ];
      u.buf[ 1 ] = buf[ 0 ];
      return u.val;
   }

   static HB_FORCEINLINE void _hb_put_rev_uint16( BYTE * buf, UINT16 val )
   {
      UINT16CAST u;
      u.val = val;
      buf[ 0 ] = u.buf[ 1 ];
      buf[ 1 ] = u.buf[ 0 ];
   }

   static HB_FORCEINLINE UINT32 _hb_get_std_uint32( const BYTE * buf )
   {
      UINT32CAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static HB_FORCEINLINE void _hb_put_std_uint32( BYTE * buf, UINT32 val )
   {
      UINT32CAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static HB_FORCEINLINE UINT32 _hb_get_rev_uint32( const BYTE * buf )
   {
      UINT32CAST u;
#  if defined( HB_BUILTIN_BSWAP32 )
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return HB_BUILTIN_BSWAP32( u.val );
#  else
      u.buf[ 0 ] = buf[ 3 ];
      u.buf[ 1 ] = buf[ 2 ];
      u.buf[ 2 ] = buf[ 1 ];
      u.buf[ 3 ] = buf[ 0 ];
      return u.val;
#  endif
   }

   static HB_FORCEINLINE void _hb_put_rev_uint32( BYTE * buf, UINT32 val )
   {
      UINT32CAST u;
#  if defined( HB_BUILTIN_BSWAP32 )
      u.val = HB_BUILTIN_BSWAP32( val );
      memcpy( buf, u.buf, sizeof( u.buf ) );
#  else
      u.val = val;
      buf[ 0 ] = u.buf[ 3 ];
      buf[ 1 ] = u.buf[ 2 ];
      buf[ 2 ] = u.buf[ 1 ];
      buf[ 3 ] = u.buf[ 0 ];
#  endif
   }

#  if ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT )
      static HB_FORCEINLINE UINT64 _hb_get_std_uint64( const BYTE * buf )
      {
         UINT64CAST u;
         memcpy( u.buf, buf, sizeof( u.buf ) );
         return u.val;
      }

      static HB_FORCEINLINE void _hb_put_std_uint64( BYTE * buf, UINT64 val )
      {
         UINT64CAST u;
         u.val = val;
         memcpy( buf, u.buf, sizeof( u.buf ) );
      }

      static HB_FORCEINLINE UINT64 _hb_get_rev_uint64( const BYTE * buf )
      {
         UINT64CAST u;
#     if defined( HB_BUILTIN_BSWAP64 )
         memcpy( u.buf, buf, sizeof( u.buf ) );
         return HB_BUILTIN_BSWAP64( u.val );
#     else
         u.buf[ 0 ] = buf[ 7 ];
         u.buf[ 1 ] = buf[ 6 ];
         u.buf[ 2 ] = buf[ 5 ];
         u.buf[ 3 ] = buf[ 4 ];
         u.buf[ 4 ] = buf[ 3 ];
         u.buf[ 5 ] = buf[ 2 ];
         u.buf[ 6 ] = buf[ 1 ];
         u.buf[ 7 ] = buf[ 0 ];
         return u.val;
#     endif
      }

      static HB_FORCEINLINE void _hb_put_rev_uint64( BYTE * buf, UINT64 val )
      {
         UINT64CAST u;
#     if defined( HB_BUILTIN_BSWAP64 )
         u.val = HB_BUILTIN_BSWAP64( val );
         memcpy( buf, u.buf, sizeof( u.buf ) );
#     else
         u.val = val;
         buf[ 0 ] = u.buf[ 7 ];
         buf[ 1 ] = u.buf[ 6 ];
         buf[ 2 ] = u.buf[ 5 ];
         buf[ 3 ] = u.buf[ 4 ];
         buf[ 4 ] = u.buf[ 3 ];
         buf[ 5 ] = u.buf[ 2 ];
         buf[ 6 ] = u.buf[ 1 ];
         buf[ 7 ] = u.buf[ 0 ];
#     endif
      }
#  endif

   static HB_FORCEINLINE double _hb_get_std_double( const BYTE * buf )
   {
      HB_DBLCAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static HB_FORCEINLINE void _hb_put_std_double( BYTE * buf, double val )
   {
      HB_DBLCAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static HB_FORCEINLINE double _hb_get_rev_double( const BYTE * buf )
   {
      HB_DBLCAST u;
#  if ( ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT ) ) && \
      defined( HB_BUILTIN_BSWAP64 )
      memcpy( u.buf, buf, sizeof( u.buf ) );
      u.i64 = HB_BUILTIN_BSWAP64( u.i64 );
      return u.val;
#  else
      u.buf[ 0 ] = buf[ 7 ];
      u.buf[ 1 ] = buf[ 6 ];
      u.buf[ 2 ] = buf[ 5 ];
      u.buf[ 3 ] = buf[ 4 ];
      u.buf[ 4 ] = buf[ 3 ];
      u.buf[ 5 ] = buf[ 2 ];
      u.buf[ 6 ] = buf[ 1 ];
      u.buf[ 7 ] = buf[ 0 ];
      return u.val;
#endif
   }

   static HB_FORCEINLINE void _hb_put_rev_double( BYTE * buf, double val )
   {
      HB_DBLCAST u;
#  if ( ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT ) ) && \
      defined( HB_BUILTIN_BSWAP64 )
      u.val = val;
      u.i64 = HB_BUILTIN_BSWAP64( u.i64 );
      memcpy( buf, u.buf, sizeof( u.buf ) );
#  else
      u.val = val;
      buf[ 0 ] = u.buf[ 7 ];
      buf[ 1 ] = u.buf[ 6 ];
      buf[ 2 ] = u.buf[ 5 ];
      buf[ 3 ] = u.buf[ 4 ];
      buf[ 4 ] = u.buf[ 3 ];
      buf[ 5 ] = u.buf[ 2 ];
      buf[ 6 ] = u.buf[ 1 ];
      buf[ 7 ] = u.buf[ 0 ];
#  endif
   }

#  define HB_GET_STD_DOUBLE( p )       _hb_get_std_double( ( const BYTE * ) ( p ) )
#  define HB_GET_REV_DOUBLE( p )       _hb_get_rev_double( ( const BYTE * ) ( p ) )
#  define HB_PUT_STD_DOUBLE( p, d )    _hb_put_std_double( ( BYTE * ) ( p ), d )
#  define HB_PUT_REV_DOUBLE( p, d )    _hb_put_rev_double( ( BYTE * ) ( p ), d )

#  if defined( HB_BIG_ENDIAN )

#     define HB_GET_BE_UINT16( p )        _hb_get_std_uint16( ( const BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT16( p, w )     _hb_put_std_uint16( ( BYTE * ) ( p ), _HB_CAST16 ( w ) )
#     define HB_GET_BE_UINT32( p )        _hb_get_std_uint32( ( const BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT32( p, l )     _hb_put_std_uint32( ( BYTE * ) ( p ), _HB_CAST32 ( l ) )
#     define HB_GET_BE_UINT64( p )        _hb_get_std_uint64( ( const BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT64( p, q )     _hb_put_std_uint64( ( BYTE * ) ( p ), _HB_CAST64 ( q ) )

#     define HB_GET_LE_UINT16( p )        _hb_get_rev_uint16( ( const BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT16( p, w )     _hb_put_rev_uint16( ( BYTE * ) ( p ), _HB_CAST16 ( w ) )
#     define HB_GET_LE_UINT32( p )        _hb_get_rev_uint32( ( const BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT32( p, l )     _hb_put_rev_uint32( ( BYTE * ) ( p ), _HB_CAST32 ( l ) )
#     define HB_GET_LE_UINT64( p )        _hb_get_rev_uint64( ( const BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT64( p, q )     _hb_put_rev_uint64( ( BYTE * ) ( p ), _HB_CAST64 ( q ) )

#  else /* HB_LITTLE_ENDIAN */

#     define HB_GET_BE_UINT16( p )        _hb_get_rev_uint16( ( const BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT16( p, w )     _hb_put_rev_uint16( ( BYTE * ) ( p ), _HB_CAST16 ( w ) )
#     define HB_GET_BE_UINT32( p )        _hb_get_rev_uint32( ( const BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT32( p, l )     _hb_put_rev_uint32( ( BYTE * ) ( p ), _HB_CAST32 ( l ) )
#     define HB_GET_BE_UINT64( p )        _hb_get_rev_uint64( ( const BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT64( p, q )     _hb_put_rev_uint64( ( BYTE * ) ( p ), _HB_CAST64 ( q ) )

#     define HB_GET_LE_UINT16( p )        _hb_get_std_uint16( ( const BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT16( p, w )     _hb_put_std_uint16( ( BYTE * ) ( p ), _HB_CAST16 ( w ) )
#     define HB_GET_LE_UINT32( p )        _hb_get_std_uint32( ( const BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT32( p, l )     _hb_put_std_uint32( ( BYTE * ) ( p ), _HB_CAST32 ( l ) )
#     define HB_GET_LE_UINT64( p )        _hb_get_std_uint64( ( const BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT64( p, q )     _hb_put_std_uint64( ( BYTE * ) ( p ), _HB_CAST64 ( q ) )

#  endif

#else /* ! __GNUC__ || _MSC_VER < 1400 */

#  define HB_GET_STD_DOUBLE( p )    hb_get_std_double( ( const BYTE * ) ( p ) )
#  define HB_GET_REV_DOUBLE( p )    hb_get_rev_double( ( const BYTE * ) ( p ) )
#define HB_PUT_REV_DOUBLE( p, d )    \
         do { \
            union { \
               double dbl; \
               BYTE buffer[ 8 ]; \
            } u; \
            u.dbl = ( double ) ( d ); \
            (( BYTE * )( p ))[ 7 ] = u.buffer[ 0 ]; \
            (( BYTE * )( p ))[ 6 ] = u.buffer[ 1 ]; \
            (( BYTE * )( p ))[ 5 ] = u.buffer[ 2 ]; \
            (( BYTE * )( p ))[ 4 ] = u.buffer[ 3 ]; \
            (( BYTE * )( p ))[ 3 ] = u.buffer[ 4 ]; \
            (( BYTE * )( p ))[ 2 ] = u.buffer[ 5 ]; \
            (( BYTE * )( p ))[ 1 ] = u.buffer[ 6 ]; \
            (( BYTE * )( p ))[ 0 ] = u.buffer[ 7 ]; \
         } while( 0 )
#define HB_PUT_STD_DOUBLE( p, d )    \
         do { \
            union { \
               double dbl; \
               BYTE buffer[ 8 ]; \
            } u; \
            u.dbl = ( double ) ( d ); \
            (( BYTE * )( p ))[ 0 ] = u.buffer[ 0 ]; \
            (( BYTE * )( p ))[ 1 ] = u.buffer[ 1 ]; \
            (( BYTE * )( p ))[ 2 ] = u.buffer[ 2 ]; \
            (( BYTE * )( p ))[ 3 ] = u.buffer[ 3 ]; \
            (( BYTE * )( p ))[ 4 ] = u.buffer[ 4 ]; \
            (( BYTE * )( p ))[ 5 ] = u.buffer[ 5 ]; \
            (( BYTE * )( p ))[ 6 ] = u.buffer[ 6 ]; \
            (( BYTE * )( p ))[ 7 ] = u.buffer[ 7 ]; \
         } while( 0 )

#  if ! defined( HB_STRICT_ALIGNMENT ) && defined( HB_LITTLE_ENDIAN )

   #define HB_GET_LE_UINT16( p )    ( *( const UINT16 * )( p ) )
   #define HB_PUT_LE_UINT16( p, w ) ( *( UINT16 * )( p ) = ( UINT16 ) ( w ) )
   #define HB_GET_LE_UINT32( p )    ( *( const UINT32 * )( p ) )
   #define HB_PUT_LE_UINT32( p, l ) ( *( UINT32 * )( p ) = ( UINT32 ) ( l ) )
   #define HB_GET_LE_UINT64( p )    ( *( const UINT64 * )( p ) )
   #define HB_PUT_LE_UINT64( p, q ) ( *( UINT64 * )( p ) = ( UINT64 ) ( q ) )

#else

   #define HB_GET_LE_UINT16( p )    ( ( UINT16 ) \
                                      ( ( ( UINT16 ) (( const BYTE * )( p ))[ 0 ] ) | \
                                        ( ( UINT16 ) (( const BYTE * )( p ))[ 1 ] <<  8 ) ) )
   #define HB_GET_LE_UINT32( p )    ( ( UINT32 ) \
                                      ( ( ( UINT32 ) (( const BYTE * )( p ))[ 0 ] ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[ 2 ] << 16 ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[ 3 ] << 24 ) ) )
   #define HB_GET_LE_UINT64( p )    ( ( UINT64 ) \
                                      ( ( ( UINT64 ) (( const BYTE * )( p ))[ 0 ] ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[ 2 ] << 16 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[ 3 ] << 24 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[ 4 ] << 32 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[ 5 ] << 40 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[ 6 ] << 48 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[ 7 ] << 56 ) ) )

   #define HB_PUT_LE_UINT16( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( w ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (w) >>  8 ); \
                                       } while( 0 )
   #define HB_PUT_LE_UINT32( p, l )    do { \
                                         (( BYTE * )( p ))[ 0 ] = ( BYTE )( l ); \
                                         (( BYTE * )( p ))[ 1 ] = ( BYTE )( (l) >>  8 ); \
                                         (( BYTE * )( p ))[ 2 ] = ( BYTE )( (l) >> 16 ); \
                                         (( BYTE * )( p ))[ 3 ] = ( BYTE )( (l) >> 24 ); \
                                       } while( 0 )
   #define HB_PUT_LE_UINT64( p, q )    do { \
                                         (( BYTE * )( p ))[ 0 ] = ( BYTE )( q ); \
                                         (( BYTE * )( p ))[ 1 ] = ( BYTE )( (q) >>  8 ); \
                                         (( BYTE * )( p ))[ 2 ] = ( BYTE )( (q) >> 16 ); \
                                         (( BYTE * )( p ))[ 3 ] = ( BYTE )( (q) >> 24 ); \
                                         (( BYTE * )( p ))[ 4 ] = ( BYTE )( (q) >> 32 ); \
                                         (( BYTE * )( p ))[ 5 ] = ( BYTE )( (q) >> 40 ); \
                                         (( BYTE * )( p ))[ 6 ] = ( BYTE )( (q) >> 48 ); \
                                         (( BYTE * )( p ))[ 7 ] = ( BYTE )( (q) >> 56 ); \
                                       } while( 0 )
#endif

#  if ! defined( HB_STRICT_ALIGNMENT ) && defined( HB_BIG_ENDIAN )

   #define HB_GET_BE_UINT16( p )    ( *( const UINT16 * )( p ) )
   #define HB_PUT_BE_UINT16( p, w ) ( *( UINT16 * )( p ) = ( UINT16 ) ( w ) )
   #define HB_GET_BE_UINT32( p )    ( *( const UINT32 * )( p ) )
   #define HB_PUT_BE_UINT32( p, l ) ( *( UINT32 * )( p ) = ( UINT32 ) ( l ) )
   #define HB_GET_BE_UINT64( p )    ( *( const UINT64 * )( p ) )
   #define HB_PUT_BE_UINT64( p, q ) ( *( UINT64 * )( p ) = ( UINT64 ) ( q ) )

#  else

   #define HB_GET_BE_UINT16( p )    ( ( UINT16 ) \
                                      ( ( ( UINT16 ) (( const BYTE * )( p ))[0] << 8 ) | \
                                        ( ( UINT16 ) (( const BYTE * )( p ))[1] ) ) )
   #define HB_GET_BE_UINT32( p )    ( ( UINT32 ) \
                                      ( ( ( UINT32 ) (( const BYTE * )( p ))[0] << 24 ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[1] << 16 ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[2] <<  8 ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[3] ) ) )
   #define HB_GET_BE_UINT64( p )    ( ( UINT64 ) \
                                      ( ( ( UINT64 ) (( const BYTE * )( p ))[0] << 56 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[1] << 48 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[2] << 40 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[3] << 32 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[4] << 24 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[5] << 16 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[6] <<  8 ) | \
                                        ( ( UINT64 ) (( const BYTE * )( p ))[7] ) ) )

   #define HB_PUT_BE_UINT16( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( (w) >>  8 ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( w ); \
                                       } while( 0 )
   #define HB_PUT_BE_UINT32( p, l )    do { \
                                         (( BYTE * )( p ))[ 0 ] = ( BYTE )( (l) >> 24 ); \
                                         (( BYTE * )( p ))[ 1 ] = ( BYTE )( (l) >> 16 ); \
                                         (( BYTE * )( p ))[ 2 ] = ( BYTE )( (l) >>  8 ); \
                                         (( BYTE * )( p ))[ 3 ] = ( BYTE )( l ); \
                                       } while( 0 )
   #define HB_PUT_BE_UINT64( p, q )    do { \
                                         (( BYTE * )( p ))[ 0 ] = ( BYTE )( (q) >> 56 ); \
                                         (( BYTE * )( p ))[ 1 ] = ( BYTE )( (q) >> 48 ); \
                                         (( BYTE * )( p ))[ 2 ] = ( BYTE )( (q) >> 40 ); \
                                         (( BYTE * )( p ))[ 3 ] = ( BYTE )( (q) >> 32 ); \
                                         (( BYTE * )( p ))[ 4 ] = ( BYTE )( (q) >> 24 ); \
                                         (( BYTE * )( p ))[ 5 ] = ( BYTE )( (q) >> 16 ); \
                                         (( BYTE * )( p ))[ 6 ] = ( BYTE )( (q) >>  8 ); \
                                         (( BYTE * )( p ))[ 7 ] = ( BYTE )( q ); \
                                       } while( 0 )
#endif

#endif /* ! __GNUC__ */

/*
 * HB_FORCE_IEEE754_DOUBLE will can be used on platforms which use different
 * double format and we want to force storing double number as IEEE754
 * double value for sharing binary data (f.e. PCODE in .hrb files or CDX
 * indexes or DBFs with "B" fields.
 */
#if defined( HB_FORCE_IEEE754_DOUBLE )

#  define HB_GET_LE_DOUBLE( p )     hb_get_ieee754( ( const BYTE * ) ( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  hb_put_ieee754( ( BYTE * ) ( p ), ( d ) )
#  define HB_DBL2ORD( d, o )        hb_put_ord_ieee754( ( o ), *( d ) )
#  define HB_ORD2DBL( o, d )  do { \
                                 *( d ) = hb_get_ord_ieee754( ( const BYTE * ) ( o ) ); \
                              } while( 0 )

#elif defined( HB_BIG_ENDIAN )

#  define HB_GET_LE_DOUBLE( p )     HB_GET_REV_DOUBLE( ( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  HB_PUT_REV_DOUBLE( ( p ), ( d ) )

#elif defined( HB_STRICT_ALIGNMENT ) || defined( __GNUC__ )

#  define HB_GET_LE_DOUBLE( p )     HB_GET_STD_DOUBLE( ( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  HB_PUT_STD_DOUBLE( ( p ), ( d ) )

#else

#  define HB_GET_LE_DOUBLE( p )     ( *( const double * )( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  ( *( double * )( p ) = ( double ) ( d ) )

#  endif

#if ! defined( HB_FORCE_IEEE754_DOUBLE )
#  if defined( HB_BIG_ENDIAN )

   #define HB_ORD2DBL( o, d )       do { \
      if ( ( ( const BYTE * ) ( o ) )[ 0 ] & 0x80 ) { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( const BYTE * ) ( o ) )[ 0 ]; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( const BYTE * ) ( o ) )[ 1 ]; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( const BYTE * ) ( o ) )[ 2 ]; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( const BYTE * ) ( o ) )[ 3 ]; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( const BYTE * ) ( o ) )[ 4 ]; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( const BYTE * ) ( o ) )[ 5 ]; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( const BYTE * ) ( o ) )[ 6 ]; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( const BYTE * ) ( o ) )[ 7 ] ^ ( BYTE ) 0x80; \
      } else { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( const BYTE * ) ( o ) )[ 0 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( const BYTE * ) ( o ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( const BYTE * ) ( o ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( const BYTE * ) ( o ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( const BYTE * ) ( o ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( const BYTE * ) ( o ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( const BYTE * ) ( o ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( const BYTE * ) ( o ) )[ 7 ] ^ ( BYTE ) 0xFF; \
      } } while( 0 )

   #define HB_DBL2ORD( d, o )       do { \
      if ( *( d ) >= 0.0 ) { \
         if( *( d ) == -0.0 ) *( d ) = 0.0; \
         ( ( BYTE * ) ( o ) )[ 0 ] = ( ( const BYTE * ) ( d ) )[ 0 ] ^ ( BYTE ) 0x80; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( const BYTE * ) ( d ) )[ 1 ]; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( const BYTE * ) ( d ) )[ 2 ]; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( const BYTE * ) ( d ) )[ 3 ]; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( const BYTE * ) ( d ) )[ 4 ]; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( const BYTE * ) ( d ) )[ 5 ]; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( const BYTE * ) ( d ) )[ 6 ]; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( const BYTE * ) ( d ) )[ 7 ]; \
      } else { \
         ( ( BYTE * ) ( o ) )[ 0 ] = ( ( const BYTE * ) ( d ) )[ 0 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( const BYTE * ) ( d ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( const BYTE * ) ( d ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( const BYTE * ) ( d ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( const BYTE * ) ( d ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( const BYTE * ) ( d ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( const BYTE * ) ( d ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( const BYTE * ) ( d ) )[ 7 ] ^ ( BYTE ) 0xFF; \
      } } while( 0 )

#else /* HB_LITTLE_ENDIAN */

   #define HB_ORD2DBL( o, d )       do { \
      if ( ( ( const BYTE * ) ( o ) )[ 0 ] & 0x80 ) { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( const BYTE * ) ( o ) )[ 7 ]; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( const BYTE * ) ( o ) )[ 6 ]; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( const BYTE * ) ( o ) )[ 5 ]; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( const BYTE * ) ( o ) )[ 4 ]; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( const BYTE * ) ( o ) )[ 3 ]; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( const BYTE * ) ( o ) )[ 2 ]; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( const BYTE * ) ( o ) )[ 1 ]; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( const BYTE * ) ( o ) )[ 0 ] ^ ( BYTE ) 0x80; \
      } else { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( const BYTE * ) ( o ) )[ 7 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( const BYTE * ) ( o ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( const BYTE * ) ( o ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( const BYTE * ) ( o ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( const BYTE * ) ( o ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( const BYTE * ) ( o ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( const BYTE * ) ( o ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( const BYTE * ) ( o ) )[ 0 ] ^ ( BYTE ) 0xFF; \
      } } while( 0 )

   #define HB_DBL2ORD( d, o )       do { \
      if ( *( d ) >= 0.0 ) { \
         if( *( d ) == -0.0 ) *( d ) = 0.0; \
         ( ( BYTE * ) ( o ) )[ 0 ] = ( ( const BYTE * ) ( d ) )[ 7 ] ^ ( BYTE ) 0x80; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( const BYTE * ) ( d ) )[ 6 ]; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( const BYTE * ) ( d ) )[ 5 ]; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( const BYTE * ) ( d ) )[ 4 ]; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( const BYTE * ) ( d ) )[ 3 ]; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( const BYTE * ) ( d ) )[ 2 ]; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( const BYTE * ) ( d ) )[ 1 ]; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( const BYTE * ) ( d ) )[ 0 ]; \
      } else { \
         ( ( BYTE * ) ( o ) )[ 0 ] = ( ( const BYTE * ) ( d ) )[ 7 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( const BYTE * ) ( d ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( const BYTE * ) ( d ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( const BYTE * ) ( d ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( const BYTE * ) ( d ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( const BYTE * ) ( d ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( const BYTE * ) ( d ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( const BYTE * ) ( d ) )[ 0 ] ^ ( BYTE ) 0xFF; \
      } } while( 0 )
#  endif

#endif /* ! defined( HB_FORCE_IEEE754_DOUBLE ) */


/* Now the rest of endian macros */

/*
 * 24 bit integers are not directly supported by any processor we used so far
 * so we always have to build them from BYTEs and cannot use C casting
 */
#define HB_GET_LE_INT24( p )        ( ( INT32 ) \
                                      ( ( ( INT32 ) (( const BYTE * )( p ))[ 0 ] ) | \
                                        ( ( INT32 ) (( const BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( INT32 ) (( const BYTE * )( p ))[ 2 ] << 16 ) | \
                                        ( ( INT32 ) ((( const BYTE * )( p ))[ 2 ] & 0x80 ? 0xFF : 0x00 ) << 24 ) ) )
#define HB_GET_LE_UINT24( p )       ( ( UINT32 ) \
                                      ( ( ( UINT32 ) (( const BYTE * )( p ))[ 0 ] ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[ 2 ] << 16 ) ) )
#define HB_PUT_LE_UINT24( p, u )    do { \
                                       (( BYTE * )( p ))[ 0 ] = ( BYTE )( u ); \
                                       (( BYTE * )( p ))[ 1 ] = ( BYTE )( (u) >>  8 ); \
                                       (( BYTE * )( p ))[ 2 ] = ( BYTE )( (u) >> 16 ); \
                                    } while( 0 )
#define HB_GET_BE_INT24( p )        ( ( INT32 ) \
                                      ( ( ( INT32 ) (( const BYTE * )( p ))[ 2 ] ) | \
                                        ( ( INT32 ) (( const BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( INT32 ) (( const BYTE * )( p ))[ 0 ] << 16 ) | \
                                        ( ( INT32 ) ((( const BYTE * )( p ))[ 0 ] & 0x80 ? 0xFF : 0x00 ) << 24 ) ) )
#define HB_GET_BE_UINT24( p )       ( ( UINT32 ) \
                                      ( ( ( UINT32 ) (( const BYTE * )( p ))[ 2 ] ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[ 1 ] <<  8 ) | \
                                        ( ( UINT32 ) (( const BYTE * )( p ))[ 0 ] << 16 ) ) )
#define HB_PUT_BE_UINT24( p, u )    do { \
                                       (( BYTE * )( p ))[ 2 ] = ( BYTE )( u ); \
                                       (( BYTE * )( p ))[ 1 ] = ( BYTE )( (u) >>  8 ); \
                                       (( BYTE * )( p ))[ 0 ] = ( BYTE )( (u) >> 16 ); \
                                    } while( 0 )


#define HB_GET_LE_INT16( p )        (( INT16 ) HB_GET_LE_UINT16( p ))
#define HB_GET_LE_INT32( p )        (( INT32 ) HB_GET_LE_UINT32( p ))
#define HB_GET_LE_INT64( p )        (( INT64 ) HB_GET_LE_UINT64( p ))

#define HB_PCODE_MKSHORT( p )       (( SHORT )     HB_GET_LE_INT16( p ))
#define HB_PCODE_MKUSHORT( p )      (( USHORT )    HB_GET_LE_UINT16( p ))
#define HB_PCODE_MKLONG( p )        (( LONG )      HB_GET_LE_INT32( p ))
#define HB_PCODE_MKULONG( p )       (( ULONG )     HB_GET_LE_UINT32( p ))
#define HB_PCODE_MKLONGLONG( p )    (( LONGLONG )  HB_GET_LE_INT64( p ))
#define HB_PCODE_MKULONGLONG( p )   (( ULONGLONG ) HB_GET_LE_UINT64( p ))
#define HB_PCODE_MKDOUBLE( p )      (( double )    HB_GET_LE_DOUBLE( p ))
#define HB_PCODE_MKINT24( p )       (( LONG )      HB_GET_LE_INT24( p ))
#define HB_PCODE_MKUINT24( p )      (( ULONG )     HB_GET_LE_UINT24( p ))

/*
 * Below are hacked version of INT64 macros which operates on double
 * when INT64 is not supported - they are necessary for PCODE and
 * database access
 */
#if defined( HB_LONG_LONG_OFF ) && !defined( HB_ARCH_64BIT )
   #undef HB_GET_LE_INT64
   #undef HB_GET_LE_UINT64
   #undef HB_PUT_LE_UINT64
   #undef HB_PCODE_MKLONGLONG
   #undef HB_PCODE_MKULONGLONG
   #undef HB_DBL_LIM_INT64
   #define UINT64_MAXDBL               ( (( double ) UINT32_MAX + 1.0) * \
                                         (( double ) UINT32_MAX + 1.0) - 1.0 )
   #define HB_GET_LE_INT64( p )        hb_get_le_int64( ( const BYTE * ) ( p ) )
   #define HB_GET_LE_UINT64( p )       hb_get_le_uint64( ( const BYTE * ) ( p ) )
   #define HB_PUT_LE_UINT64( p, d )    hb_put_le_uint64( ( BYTE * ) ( p ), \
                                                         ( double ) ( d ) )
   #define HB_PCODE_MKLONGLONG( p )    (( double ) HB_GET_LE_INT64( p ))
   #define HB_PCODE_MKULONGLONG( p )   (( double ) HB_GET_LE_UINT64( p ))
   #define HB_DBL_LIM_INT64(d)         ( (HB_MAXDBL) -UINT64_MAXDBL / 2 - 1 <= \
                                         (HB_MAXDBL) (d) && (HB_MAXDBL) (d) <= \
                                         (HB_MAXDBL) UINT64_MAXDBL / 2 )
#endif

#define HB_MACRO2STRING( macro )    HB_MACRO2STRING_( macro )
#define HB_MACRO2STRING_( macro )   #macro

#define HB_MACRONAME_JOIN( m1, m2 )       HB_MACRONAME_JOIN_( m1, m2 )
#define HB_MACRONAME_JOIN_( m1, m2 )      m1 ## m2

#define HB_SIZEOFARRAY( var )       ( sizeof( var ) / sizeof( *var ) )

#define HB_UNCONST( p )       ( ( void * ) ( HB_PTRUINT ) ( const void * ) ( p ) )
#define HB_DECONST( c, p )    ( ( c ) HB_UNCONST( p ) )


//#if defined( __POCC__ ) || defined( __XCC__ )
//   #define HB_SYMBOL_UNUSED( symbol )  do if( symbol ) {;} while( 0 )
//#else
   #define HB_SYMBOL_UNUSED( symbol )  ( void ) symbol
//#endif

/* ***********************************************************************
 * The name of starting procedure
 * Note: You have to define it in case when Harbour cannot find the proper
 * starting procedure (due to incorrect order of static data initialization)
 *
 * The list of compilers that require it:
 * - Watcom C/C++ 10.0
 * - GCC on Linux
 * - 06/nov/2004 - <maurilio.longo@libero.it>
 *                 GCC on OS/2 needs this definition, I've found it playing with harbour.dll on OS/2
 *                 Right now I've simply commented out && !defined(HB_OS_OS2_GCC), to be removed
 *                 if there are no problems in the near future.
 *
 * By default we are using automatic lookup (symbol not defined)
 *
 * - 07/jan/2008 - Andi Jahja <harbour/AT/cbn/net/id>
 *                 No longer requires starting procedure:
 *                 OpenWatcom C/C++ 1.2 and above
 *                 MinGW 3.4.5
 *
 * - 18/feb/2008 - Phil Krylov <phil a t newstar.rinet.ru>
 *                 Most MinGW versions still need it, including some 3.4.5 builds.
 */
#if ( defined(__WATCOMC__) && (__WATCOMC__<1220) ) || \
    ( defined(__GNUC__) && !defined(__DJGPP__) /* && !defined(HB_OS_OS2_GCC)*/ )
   #define HARBOUR_START_PROCEDURE "MAIN"
#endif

#if defined(HB_FUNC_CALLCONV)
   #define HARBOUR void HB_FUNC_CALLCONV
#else
   #define HARBOUR void
#endif

#if ! defined(__HARBOUR__)
   #define __HARBOUR__
#endif
#if ! defined(__XHARBOUR__)
   #define __XHARBOUR__
#endif

typedef HARBOUR ( * PHB_FUNC )( void );

#if defined( __EXPORT__ )
   #if defined( __RSXNT__ )
      /* RSXNT does not support any type of export keyword.
         Exported (i.e., public) names can be obtained via
         the emxexp utility and the output can be used for
         input to a module definition file. See emxdev.doc
         in the RSXNT doc/ directory for more information. */
      #define HB_EXPORT

   #elif defined( __GNUC__ ) && defined( HB_OS_WIN )
      #define HB_EXPORT __attribute__ (( dllexport ))

   #elif defined( __GNUC__ ) && defined( HB_OS_LINUX )
      #define HB_EXPORT __attribute__ ((visibility ("default")))

   #elif defined( __BORLANDC__ )
      #define HB_EXPORT __declspec( dllexport )

   #elif defined( __WATCOMC__ )
      #define HB_EXPORT __declspec( dllexport )

   #elif defined( ASANLM ) || defined( ASANT )
      #define HB_EXPORT

   #elif defined( HB_OS_WIN )
      #define HB_EXPORT _declspec( dllexport )

   #else
      #define HB_EXPORT

   #endif
#else
   #define HB_EXPORT
#endif

#if defined( __IMPORT__ )
#if defined( __RSXNT__ )
   /* RSXNT does not support any type of export keyword.
      Exported (i.e., public) names can be obtained via
      the emxexp utility and the output can be used for
      input to a module definition file. See emxdev.doc
      in the RSXNT doc/ directory for more information. */
   #define HB_IMPORT

#elif defined( __GNUC__ ) && defined( HB_OS_WIN )
   #define HB_IMPORT __attribute__ (( dllimport ))

#elif defined( __BORLANDC__ )
   #define HB_IMPORT __declspec( dllimport )

#elif defined( __WATCOMC__ )
   #define HB_IMPORT __declspec( dllimport )

#elif defined( ASANLM ) || defined( ASANT )
   #define HB_IMPORT

#elif defined( HB_OS_WIN )
   #define HB_IMPORT _declspec( dllimport )

#else
   #define HB_IMPORT

#endif
#else
   #define HB_IMPORT
#endif

#if defined( HB_OS_WIN )
   #include "hbwince.h"
#endif

/* Function declaration macros */

/* NOTE: The prefix is "HB_FUN_" currently, this is needed to
         avoid collision with any other declared symbol.
         Note that "HB_" is not enough, since the Harbour internals
         are also prefixed with HB_. */

#define HB_FUNCNAME( funcname )        HB_FUN_##funcname
#define HB_INIT_FUNCNAME( funcname )   HB_FUN_init_##funcname
#define HB_EXIT_FUNCNAME( funcname )   HB_FUN_exit_##funcname
#define HB_INITSTATICS_FUNCNAME()      hb_INITSTATICS

#if defined( __cplusplus ) && !defined( HB_FUNC_USE_DECORATION )
   #define HB_EXTERN_C_ extern "C"
   #define HB_EXTERN_
#else
   #define HB_EXTERN_C_
   #define HB_EXTERN_   extern
#endif

#define HB_NAMESPACE_NAME( namespaceid, funcname )             _##namespaceid##_##funcname
#define HB_NAMESPACE_FUNCNAME( namespaceid, funcname )          HB_NSF_##namespaceid##_##funcname
#define HB_OPTIONAL_NAMESPACE_FUNCNAME( namespaceid, funcname ) HB_FUNCNAME( funcname )
#define HB_EXTERNAL_NAMESPACE_FUNCNAME( namespaceid, funcname ) HB_NAMESPACE_FUNCNAME( namespaceid, funcname )

#define HB_FUNC_NAMESPACE( namespaceid, funcname )              _HB_NS_FUNC_STATIC( HB_NAMESPACE_NAME( namespaceid, funcname ) )
#define HB_FUNC_EXTERNAL_NAMESPACE( namespaceid, funcname )     _HB_NS_FUNC( HB_NAMESPACE_NAME( namespaceid, funcname ) )
#define HB_FUNC_OPTIONAL_NAMESPACE( namespace, funcname )       HB_FUNC( funcname )

#define _HB_NS_FUNC_STATIC( expandedmacro )                     HB_NS_FUNC_STATIC( expandedmacro )
#define _HB_NS_FUNC( expandedmacro )                            HB_NS_FUNC( expandedmacro )

#define HB_NS_FUNC( funcname )                                  HB_EXTERN_C_ HB_EXPORT HARBOUR HB_NSF##funcname ( void )
#define HB_NS_FUNC_STATIC( funcname )                           static HARBOUR HB_NSF##funcname ( void )

#define HB_FUNC_EXEC( funcname )                          HB_FUN_##funcname();
#define HB_FUNC( funcname )                               HB_EXTERN_C_            HB_EXPORT HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_EXTERN( funcname )                        HB_EXTERN_C_ HB_EXTERN_ HB_IMPORT HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_STATIC( funcname )                        static HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_INIT( funcname )                          static HARBOUR HB_FUN_init_##funcname ( void )
#define HB_FUNC_EXIT( funcname )                          static HARBOUR HB_FUN_exit_##funcname ( void )
#define HB_FUNC_INITSTATICS( )                            static HARBOUR hb_INITSTATICS( void )
#define HB_FUNC_INITLINES( )                              static HARBOUR hb_INITLINES( void )
#define HB_FUNC_INITGLOBALS( )                            static HARBOUR hb_INITGLOBALS( void )
#define HB_FUNC_REGISTERGLOBAL( )                         static HARBOUR hb_REGISTERGLOBALS( void )
#define HB_FUNC_TRANSLATE( w, o )  HB_FUNC_EXTERN( o ); HB_FUNC( w ) { HB_FUNC_EXEC( o ); }
typedef ULONG HB_HANDLE;        /* handle to memvar value */
typedef SHORT HB_SYMBOLSCOPE;   /* stores symbol's scope */

typedef BYTE HB_CHAR;
typedef BYTE HB_ATTR;

/* Some common character constants */

#define HB_CHAR_NUL             '\0'    /*   0 - NUL */
#define HB_CHAR_EOS             HB_CHAR_NUL
#define HB_CHAR_BEL             '\a'    /*   7 - Bell */
#define HB_CHAR_BS              '\b'    /*   8 - Backspace */
#define HB_CHAR_HT              '\t'    /*   9 - Tab horizontal */
#define HB_CHAR_LF              '\n'    /*  10 - Linefeed */
#define HB_CHAR_VT              '\v'    /*  11 - Tab vertical */
#define HB_CHAR_FF              '\f'    /*  12 - Formfeed */
#define HB_CHAR_CR              '\r'    /*  13 - Carriage return */
#define HB_CHAR_EOF             '\x1A'  /*  26 - End of file marker */

/* Harbour specific character constants */

#define HB_CHAR_HARD1           HB_CHAR_CR
#define HB_CHAR_HARD2           HB_CHAR_LF

#define HB_CHAR_SOFT1           '\x8D'  /* 141 */
#define HB_CHAR_SOFT2           HB_CHAR_LF

#define HB_ISUPPER( c )         ( ( c ) >= 'A' && ( c ) <= 'Z' )
#define HB_ISLOWER( c )         ( ( c ) >= 'a' && ( c ) <= 'z' )
#define HB_TOUPPER( c )         ( ( c ) >= 'a' && ( c ) <= 'z' ? ( c ) - ( 'a' - 'A' ) : ( c ) )
#define HB_TOLOWER( c )         ( ( c ) >= 'A' && ( c ) <= 'Z' ? ( c ) + ( 'a' - 'A' ) : ( c ) )
#define HB_ISDIGIT( c )         ( ( c ) >= '0' && ( c ) <= '9' )
#define HB_ISALPHA( c )         ( HB_ISUPPER( c ) || HB_ISLOWER( c ) )
#define HB_ISALNUM( c )         ( HB_ISALPHA( c ) || HB_ISDIGIT( c ) )
#define HB_ISXDIGIT( c )        ( HB_ISDIGIT(c) || \
                                  ( (c) >= 'A' && (c) <= 'F' ) || \
                                  ( (c) >= 'a' && (c) <= 'f' ) )
#define HB_ISSPACE( c )         ( ( c ) == ' ' || \
                                  ( c ) == HB_CHAR_HT || \
                                  ( c ) == HB_CHAR_LF || \
                                  ( c ) == HB_CHAR_CR )
#define HB_ISFIRSTIDCHAR( c )   ( HB_ISALPHA( c ) || ( c ) == '_' )
#define HB_ISNEXTIDCHAR( c )    ( HB_ISFIRSTIDCHAR(c) || HB_ISDIGIT( c ) )

// UGLY hack
#include "hbtrace.h"


/* Harbour size type */
#if defined( HB_OS_WIN_64 )
#  if defined( HB_SIZE_SIGNED )
      typedef LONGLONG        HB_SIZE;
#  else
      typedef ULONGLONG       HB_SIZE;        /* TODO: Currently 'unsigned', to be changed 'signed' */
#  endif
   typedef LONGLONG           HB_ISIZ;        /* TODO: Change to HB_SIZE, after HB_SIZE has been converted to signed type. TEMPORARY type. */
   typedef ULONGLONG          HB_USIZ;        /* TEMPORARY type. Do not use it. */
#else
#  if defined( HB_SIZE_SIGNED )
//    typedef HB_LONG         HB_SIZE;
      typedef LONG            HB_SIZE;
#  else
//    typedef HB_ULONG        HB_SIZE;        /* TODO: Currently 'unsigned', to be changed 'signed' */
      typedef ULONG           HB_SIZE;        /* TODO: Currently 'unsigned', to be changed 'signed' */
#  endif
   typedef LONG               HB_ISIZ;        /* TODO: Change to HB_SIZE, after HB_SIZE has been converted to signed type. TEMPORARY type. */
   typedef ULONG              HB_USIZ;        /* TEMPORARY type. Do not use it. */
// typedef HB_LONG            HB_ISIZ;        /* TODO: Change to HB_SIZE, after HB_SIZE has been converted to signed type. TEMPORARY type. */
// typedef HB_ULONG           HB_USIZ;        /* TEMPORARY type. Do not use it. */
#endif

#include "hbcompat.h"

#endif /* HB_DEFS_H_ */
