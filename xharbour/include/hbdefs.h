/*
 * $Id: hbdefs.h,v 1.34 2004/04/06 09:58:23 andijahja Exp $
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
#include "hbtrace.h"

#if defined( HB_OS_WIN_32 )
   #if defined( X__WIN32__ ) && !defined( HB_WIN32_IO )
      #define HB_WIN32_IO
   #endif
#else
   #if defined( HB_WIN32_IO )
      #undef HB_WIN32_IO
   #endif
#endif

/* Include windows.h if applicable and requested */
#if defined( HB_OS_WIN_32_USED ) && defined( HB_OS_WIN_32 )

   #define WIN32_LEAN_AND_MEAN
   #define _WINSOCKAPI_  /* Prevents inclusion of Winsock.h in Windows.h */
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
   #undef INT
   #undef UINT
   #define HB_DONT_DEFINE_BASIC_TYPES

#elif defined( HB_OS_DOS )

   #include <dos.h>

   #if defined(__WATCOMC__) && defined(__386__) && !defined(__WINDOWS_386__)
      #define HB_DOS_INT86 int386
      #define HB_DOS_INT86X int386x
      #define HB_XREGS w
   #elif defined(__RSX32__)
      #define HB_DOS_INT86 _int86
      #define HB_DOS_INT86X _int86x
      #define HB_XREGS x
   #else
      #define HB_DOS_INT86 int86
      #define HB_DOS_INT86X int86x
      #define HB_XREGS x
   #endif

#endif

#if ! defined( HB_DONT_DEFINE_BASIC_TYPES )

   #undef BOOL                            /* boolean */
   typedef int BOOL;

   #undef UINT                            /* varies with platform */
   typedef unsigned int UINT;

   #undef BYTE                            /* 1 byte unsigned */
   typedef unsigned char BYTE;

   #undef SHORT                           /* 2 bytes signed */
   typedef short int SHORT;

   #undef USHORT                          /* 2 bytes unsigned */
   typedef unsigned short int USHORT;

#if UINT_MAX > USHRT_MAX && ULONG_MAX > UINT_MAX
   /* It's a temporary hack for 64bit machines, xHarbour use [U]LONG type
    * as 32bit integer and this type has to be redefined in such way
    * In the future we should clean it.
    */
   #undef LONG                            /* 4 bytes signed */
   typedef int LONG;

   #undef ULONG                           /* 4 bytes unsigned */
   typedef unsigned int ULONG;

   #undef ULONG_MAX
   #define ULONG_MAX    (~(ULONG) 0)
   #undef LONG_MAX
   #define LONG_MAX     ((LONG)( ULONG_MAX >> 1 ))
   #undef LONG_MIN
   #define LONG_MIN     (-LONG_MAX - 1)
#else
   #undef LONG                            /* 4 bytes signed */
   typedef long LONG;

   #undef ULONG                           /* 4 bytes unsigned */
   typedef unsigned long ULONG;
#endif

   #undef FALSE
   #define FALSE  (BOOL) 0
   #undef TRUE
   #define TRUE   (BOOL) !0

#endif /* HB_DONT_DEFINE_BASIC_TYPES */

#ifndef HB_LONG_LONG_OFF
   #ifndef __GNUC__
      #if !defined(LONGLONG_MIN)
         #define LONGLONG_MIN       _I64_MIN
      #endif
      #if !defined(LONGLONG_MAX)
         #define LONGLONG_MAX       _I64_MAX
      #endif
      #if !defined(ULONGLONG_MAX)
         #define ULONGLONG_MAX      _UI64_MAX
      #endif
   #else
      #if defined(ULLONG_MAX)
         #define ULONGLONG_MAX      ULLONG_MAX
      #elif defined(ULONG_LONG_MAX)
         #define ULONGLONG_MAX      ULONG_LONG_MAX
      #else
         #define ULONGLONG_MAX      (~0ULL)
      #endif
      #if defined(LLONG_MAX)
         #define LONGLONG_MAX       LLONG_MAX
      #elif defined(LONG_LONG_MAX)
         #define LONGLONG_MAX       LONG_LONG_MAX
      #else
         #define LONGLONG_MAX       ((LONGLONG) ( ULONGLONG_MAX >> 1 ))
      #endif
      #if defined(LLONG_MIN)
         #define LONGLONG_MIN       LLONG_MIN
      #elif defined(LONG_LONG_MIN)
         #define LONGLONG_MIN       LONG_LONG_MIN
      #else
         #define LONGLONG_MIN       (-LONGLONG_MAX - 1LL)
      #endif
   #endif
#endif

/* try to detect byte order if not explicitly set */
#if !defined( HB_PDP_ENDIAN ) && !defined( HB_BIG_ENDIAN ) && \
    !defined( HB_LITTLE_ENDIAN ) && \
    defined( __BYTE_ORDER ) && defined( __LITTLE_ENDIAN ) && \
    defined( __BIG_ENDIAN ) && defined( __PDP_ENDIAN )

#  if __BYTE_ORDER == __LITTLE_ENDIAN
#    define HB_LITTLE_ENDIAN
#  elif __BYTE_ORDER == __BIG_ENDIAN
#    define HB_BIG_ENDIAN
#  elif __BYTE_ORDER == __BIG_ENDIAN
#    define HB_PDP_ENDIAN
#  endif

#endif

/* maximum length of double number in decimal representation:
   log10(2^1024) ~ 308.25 */
#define HB_MAX_DOUBLE_LENGTH 320

#define HB_MAX( a, b )          ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )
#define HB_MIN( a, b )          ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )

#define HB_LOBYTE( w )          ( ( BYTE ) ( w ) )
#define HB_HIBYTE( w )          ( ( BYTE ) ( ( ( USHORT ) ( w ) >> 8 ) & 0xFF ) )
#define HB_LOWORD( l )     ( ( USHORT ) ( l ) )
#define HB_HIWORD( l )     ( ( USHORT ) ( ( ( l ) >> 16 ) & 0xFFFF ) )
#define HB_MKSHORT( lo, hi )    ( ( SHORT ) ( ( ( SHORT ) ( hi ) ) << 8 ) | ( lo ) )
#define HB_MKUSHORT( lo, hi )   ( ( USHORT ) ( ( ( USHORT ) ( hi ) ) << 8 ) | ( lo ) )
#define HB_MKLONG( b1, b2, b3, b4 )  ( ( ( ( LONG ) ( b4 ) ) << 24 ) | \
                                       ( ( ( LONG ) ( b3 ) ) << 16 ) | \
                                       ( ( ( LONG ) ( b2 ) ) <<  8 ) | \
                                       ( ( ( LONG ) ( b1 ) ) ) )
#define HB_MKULONG( b1, b2, b3, b4 ) ( ( ( ( ULONG ) ( b4 ) ) << 24 ) | \
                                       ( ( ( ULONG ) ( b3 ) ) << 16 ) | \
                                       ( ( ( ULONG ) ( b2 ) ) <<  8 ) | \
                                       ( ( ( ULONG ) ( b1 ) ) ) )

#define HB_SWAP_USHORT( w )     ( ( USHORT ) ( ( ( ( USHORT ) ( w ) & 0xFF00 ) >> 8 ) | \
                                               ( ( ( USHORT ) ( w ) & 0x00FF ) << 8 ) ) )
#define HB_SWAP_ULONG( w )      ( ( ULONG ) ( ( ( ( ULONG ) ( w ) & 0x000000FFL ) << 24 ) | \
                                              ( ( ( ULONG ) ( w ) & 0x0000FF00L ) <<  8 ) | \
                                              ( ( ( ULONG ) ( w ) & 0x00FF0000L ) >>  8 ) | \
                                              ( ( ( ULONG ) ( w ) & 0xFF000000L ) >> 24 ) ) )

#if defined( HB_PDP_ENDIAN )
   #error PDP-Endian support unimplemented. If you have such machine do it yourself.
#elif !defined( HB_BIG_ENDIAN )
   /* We use Little-Endian here */

   #define HB_GET_LE_USHORT( p )    ( *( USHORT * )( p ) )
   #define HB_PUT_LE_USHORT( p, w ) ( *( USHORT * )( p ) = ( USHORT ) ( w ) )
   #define HB_GET_LE_ULONG( p )     ( *( ULONG * )( p ) )
   #define HB_PUT_LE_ULONG( p, l )  ( *( ULONG * )( p ) = ( ULONG ) ( l ) )

   #define HB_GET_BE_USHORT( p )    HB_SWAP_USHORT( *( USHORT * )( p ) )
   #define HB_PUT_BE_USHORT( p, w ) ( *( USHORT * )( p ) = HB_SWAP_USHORT( w ) )
   #define HB_GET_BE_ULONG( p )     HB_SWAP_ULONG( *( ULONG * )( p ) )
   #define HB_PUT_BE_ULONG( p, l )  ( *( ULONG * )( p ) = HB_SWAP_ULONG( l ) )

   #define HB_GET_LE_DOUBLE( p )    ( *( double * )( p ) )
   #define HB_PUT_LE_DOUBLE( p, d ) ( *( double * )( p ) = ( double ) ( d ) )

   #define HB_USHORT_FROM_LE( w )   ( ( USHORT )( w ) )
   #define HB_ULONG_FROM_LE( l )    ( ( ULONG )( l ) )
   #define HB_USHORT_TO_LE( w )     ( ( USHORT )( w ) )
   #define HB_ULONG_TO_LE( l )      ( ( ULONG )( l ) )
   #define HB_DOUBLE_TO_LE( d )     ( ( double )( d ) )

   #define HB_PCODE_MKSHORT( p )    ( *( SHORT * )( p ) )
   #define HB_PCODE_MKUSHORT( p )   ( *( USHORT * )( p ) )
   #define HB_PCODE_MKLONG( p )     ( *( LONG * )( p ) )
   #define HB_PCODE_MKULONG( p )    ( *( ULONG * )( p ) )
   #define HB_PCODE_MKDOUBLE( p )   ( *( double * )( p ) )

   #define HB_ORD2DBL( o, d ) { \
      if ( ( ( BYTE * ) ( o ) )[ 0 ] & 0x80 ) { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( BYTE * ) ( o ) )[ 7 ]; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( BYTE * ) ( o ) )[ 6 ]; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( BYTE * ) ( o ) )[ 5 ]; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( BYTE * ) ( o ) )[ 4 ]; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( BYTE * ) ( o ) )[ 3 ]; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( BYTE * ) ( o ) )[ 2 ]; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( BYTE * ) ( o ) )[ 1 ]; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( BYTE * ) ( o ) )[ 0 ] ^ ( BYTE ) 0x80; \
      } else { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( BYTE * ) ( o ) )[ 7 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( BYTE * ) ( o ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( BYTE * ) ( o ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( BYTE * ) ( o ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( BYTE * ) ( o ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( BYTE * ) ( o ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( BYTE * ) ( o ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( BYTE * ) ( o ) )[ 0 ] ^ ( BYTE ) 0xFF; \
      } }

   #define HB_DBL2ORD( d, o ) { \
      if ( *( double * )( d ) >= 0.0 ) { \
      ( ( BYTE * ) ( o ) )[ 0 ] = ( ( BYTE * ) ( d ) )[ 7 ] ^ ( BYTE ) 0x80; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( BYTE * ) ( d ) )[ 6 ]; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( BYTE * ) ( d ) )[ 5 ]; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( BYTE * ) ( d ) )[ 4 ]; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( BYTE * ) ( d ) )[ 3 ]; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( BYTE * ) ( d ) )[ 2 ]; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( BYTE * ) ( d ) )[ 1 ]; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( BYTE * ) ( d ) )[ 0 ]; \
      } else { \
         ( ( BYTE * ) ( o ) )[ 0 ] = ( ( BYTE * ) ( d ) )[ 7 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( BYTE * ) ( d ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( BYTE * ) ( d ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( BYTE * ) ( d ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( BYTE * ) ( d ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( BYTE * ) ( d ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( BYTE * ) ( d ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( BYTE * ) ( d ) )[ 0 ] ^ ( BYTE ) 0xFF; \
      } }

#else
   /* We use Big-Endian here */

   #define HB_GET_LE_USHORT( p )    HB_SWAP_USHORT( *( USHORT * )( p ) )
   #define HB_PUT_LE_USHORT( p, w ) ( *( USHORT * )( p ) = HB_SWAP_USHORT( w ) )
   #define HB_GET_LE_ULONG( p )     HB_SWAP_ULONG( *( ULONG * )( p ) )
   #define HB_PUT_LE_ULONG( p, l )  ( *( ULONG * )( p ) = HB_SWAP_ULONG( l ) )
   #define HB_GET_BE_USHORT( p )    ( *( USHORT * )( p ) )
   #define HB_PUT_BE_USHORT( p, w ) ( *( USHORT * )( p ) = ( USHORT ) ( w ) )
   #define HB_GET_BE_ULONG( p )     ( *( ULONG * )( p ) )
   #define HB_PUT_BE_ULONG( p, l )  ( *( ULONG * )( p ) = ( ULONG ) ( l ) )

   #define HB_USHORT_FROM_LE( w )   HB_MKUSHORT( HB_HIBYTE( w ), HB_LOBYTE( w ) )
   #define HB_ULONG_FROM_LE( l )    HB_MKULONG( HB_HIBYTE( HB_HIWORD( l ) ), HB_LOBYTE( HB_HIWORD( l ) ), HB_HIBYTE( l ), HB_LOBYTE( l ) )
   #define HB_USHORT_TO_LE( w )     HB_USHORT_FROM_LE( w )
   #define HB_ULONG_TO_LE( l )      HB_ULONG_FROM_LE( l )

   #define HB_PCODE_MKSHORT( p )    HB_MKSHORT( *( BYTE * )( p ), ( ( BYTE * )( p ) )[ 1 ] )
   #define HB_PCODE_MKUSHORT( p )   HB_MKUSHORT( *( BYTE * )( p ), ( ( BYTE * )( p ) )[ 1 ] )
   #define HB_PCODE_MKLONG( p )     HB_MKLONG( *( BYTE * )( p ), ( ( BYTE * )( p ) )[ 1 ], ( ( BYTE * )( p ) )[ 2 ], ( ( BYTE * )( p ) )[ 3 ] )
   #define HB_PCODE_MKULONG( p )    HB_MKULONG( *( BYTE * )( p ), ( ( BYTE * )( p ) )[ 1 ], ( ( BYTE * )( p ) )[ 2 ], ( ( BYTE * )( p ) )[ 3 ] )

   #define HB_ORD2DBL( o, d ) { \
         *( double * )( d ) = *( double * )( o ); \
         if ( ( ( BYTE * ) ( d ) )[ 0 ] & 0x80 ) { \
            ( ( BYTE * ) ( d ) )[ 0 ] ^= 0x80; \
         } else { \
            ( ( LONG * ) ( d ) )[ 0 ] ^= 0xFFFFFFFFL; \
            ( ( LONG * ) ( d ) )[ 1 ] ^= 0xFFFFFFFFL; \
         } }
   #define HB_DBL2ORD( d, o ) { \
         *( double * )( o ) = *( double * )( d ); \
         if ( *( double * )( o ) >= 0.0 ) { \
            ( ( BYTE * ) ( o ) )[ 0 ] ^= 0x80; \
         } else { \
            ( ( LONG * ) ( o ) )[ 0 ] ^= 0xFFFFFFFFL; \
            ( ( LONG * ) ( o ) )[ 1 ] ^= 0xFFFFFFFFL; \
         } }

#if defined( __GNUC__ )
/* Be careful with double conversion. Some machines can use mixed form
   (Little/Big) for BYTE ORDER and WORD ORDER or even completely differ
   internal representation */

   #define HB_GET_LE_DOUBLE( p )    HB_PCODE_MKDOUBLE( p )
   #define HB_PUT_LE_DOUBLE( p, d ) ( *( double * )( p ) = HB_DOUBLE_TO_LE( d ) )

   #define HB_DOUBLE_TO_LE( d )     HB_DOUBLE_FROM_LE( d )
   #define HB_DOUBLE_FROM_LE( d )   \
   ( { \
      BYTE double_var[ 8 ]; \
      *( double * )double_var = d; \
      HB_PCODE_MKDOUBLE( double_var ); \
   } )
   #define HB_PCODE_MKDOUBLE( p )   \
   ( { \
      union { \
         double d; \
         BYTE buffer[ 8 ]; \
      } u; \
      u.buffer[ 0 ] = ( p )[ 7 ]; \
      u.buffer[ 1 ] = ( p )[ 6 ]; \
      u.buffer[ 2 ] = ( p )[ 5 ]; \
      u.buffer[ 3 ] = ( p )[ 4 ]; \
      u.buffer[ 4 ] = ( p )[ 3 ]; \
      u.buffer[ 5 ] = ( p )[ 2 ]; \
      u.buffer[ 6 ] = ( p )[ 1 ]; \
      u.buffer[ 7 ] = ( p )[ 0 ]; \
      u.d; \
   } )
#else
   #error Little-Endian IEEE 754 double type conversion unimplemented with a non-GCC compiler
#endif

#endif

#define HB_PCODE_MK24BIT( p )       HB_MKLONG( *( BYTE * )( p ), ( ( BYTE * )( p ) )[ 1 ], ( ( BYTE * )( p ) )[ 2 ], 0 )

#define HB_SYMBOL_UNUSED( symbol )  ( void ) symbol

/* ***********************************************************************
 * The name of starting procedure
 * Note: You have to define it in case when Harbour cannot find the proper
 * starting procedure (due to incorrect order of static data initialization)
 *
 * The list of compilers that require it:
 * - Watcom C/C++ 10.0
 * - GCC on Linux
 *
 * By default we are using automatic lookup (symbol not defined)
*/
#if defined(__WATCOMC__) || ( defined(__GNUC__) && !defined(__DJGPP__) && !defined(HARBOUR_GCC_OS2) )
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
typedef PHB_FUNC HB_FUNC_PTR;

#if defined( __EXPORT__ )
   #if defined( __RSXNT__ )
      /* RSXNT does not support any type of export keyword.
         Exported (i.e., public) names can be obtained via
         the emxexp utility and the output can be used for
         input to a module definition file. See emxdev.doc
         in the RSXNT doc/ directory for more information. */
      #define HB_EXPORT

   #elif defined( __GNUC__ ) && defined( HB_OS_WIN_32 )
      #define HB_EXPORT __attribute__ (( dllexport ))

   #elif defined( __BORLANDC__ )
      #define HB_EXPORT _declspec( dllexport )

   #elif defined( WIN32 ) && !defined( ASANT ) && !defined( __WATCOMC__ )
      #define HB_EXPORT _declspec( dllexport )

   #elif defined( ASANLM ) || defined( ASANT )
      #define HB_EXPORT

   #elif defined( __WATCOMC__ )
      #define HB_EXPORT __declspec( dllexport )

   #else
      #define HB_EXPORT

   #endif
#else
   #define HB_EXPORT
#endif

/* Function declaration macros */

/* NOTE: The prefix is "HB_FUN_" currently, this is needed to
         avoid collision with any other declared symbol.
         Note that "HB_" is not enough, since the Harbour internals
         are also prefixed with HB_. [vszakats] */

#define HB_FUNCNAME( funcname )    HB_FUN_##funcname

#if ( defined( _MSC_VER ) || defined( __WATCOMC__ ) ) && defined( HB_FUNC_NO_DECORATION )
   #define HB_EXTERN_C_ extern "C"
#else
   #define HB_EXTERN_C_
#endif

#define HB_FUNC( funcname )        HB_EXTERN_C_ HARBOUR HB_EXPORT HB_FUN_##funcname ( void )
#define HB_FUNC_STATIC( funcname ) HB_EXTERN_C_ static HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_EXTERN( funcname ) HB_EXTERN_C_ extern HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_INIT( funcname )   HB_EXTERN_C_ static HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_EXIT( funcname )   HB_EXTERN_C_ static HARBOUR HB_FUN_##funcname ( void )

typedef ULONG HB_HANDLE;        /* handle to memvar value */
typedef char  HB_SYMBOLSCOPE;   /* stores symbol's scope */

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

#define HB_CHAR_HARD1           ( ( char ) HB_CHAR_CR )
#define HB_CHAR_HARD2           ( ( char ) HB_CHAR_LF )

#define HB_CHAR_SOFT1           ( ( char ) 141 )
#define HB_CHAR_SOFT2           ( ( char ) HB_CHAR_LF )

#ifndef HB_LONG_LONG_OFF
#if !defined(LONGLONG) && !defined(_WINNT_H)
#if defined(__GNUC__)
  typedef long long LONGLONG;
#else
  typedef __int64 LONGLONG;
#endif
#endif

#if !defined(ULONGLONG) && !defined(_WINNT_H)
#if defined(__GNUC__)
  typedef unsigned long long ULONGLONG;
#else
  typedef unsigned __int64 ULONGLONG;
#endif
#endif
#endif /* HB_LONG_LONG_OFF */

#endif /* HB_DEFS_H_ */
