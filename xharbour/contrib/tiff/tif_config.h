/* $Id$
   For Creating DLL
*/

#ifndef __TIF_CONGIG_H
#define __TIF_CONGIG_H

#include "hbdefs.h"  /* UINT64 and INT64 */

#if defined(__EXPORT__)
   #if defined( __RSXNT__ )
      #define ___EXPORT___
   #elif defined( __GNUC__ ) && defined(__WIN32__)
      #define ___EXPORT___ __attribute__ (( dllexport ))
   #elif defined( __GNUC__ ) && defined( HB_OS_LINUX )
      #define ___EXPORT___ __attribute__ ((visibility ("default")))
   #elif defined( __BORLANDC__ )
      #define ___EXPORT___ __declspec( dllexport )
   #elif defined( __WATCOMC__ )
      #define ___EXPORT___ __declspec( dllexport )
   #elif defined( ASANLM ) || defined( ASANT )
      #define ___EXPORT___
   #elif defined( HB_OS_WIN ) || defined(__WIN32__)
      #define ___EXPORT___ _declspec( dllexport )
   #else
      #define ___EXPORT___
   #endif
#else
   #define ___EXPORT___
#endif

/* Pacify WatcomC warnings */
#if defined(__WATCOMC__)
   #pragma disable_message ( 201 )
   #pragma disable_message ( 124 )
   #pragma disable_message ( 136 )
#elif defined( __POCC__ )
   #pragma warn(push)
   #pragma warn(disable:2154)
   #pragma warn(disable:2030)
#elif defined(__BORLANDC__)
   #pragma warn -aus
   #pragma warn -pia
   #pragma warn -csu
   #pragma warn -sig
   #pragma warn -prc
   #pragma warn -spa
   #pragma warn -inl
#endif

/* Pacify MSVS2005 and above */
#if defined(_MSC_VER) && (_MSC_VER>=1400)
   #define _CRT_SECURE_NO_WARNINGS
   #define _CRT_SECURE_NO_DEPRECATE
#endif

/* Support Deflate compression */
#define ZIP_SUPPORT 1

/* Define to 1 if you have the <assert.h> header file. */
#define HAVE_ASSERT_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define as 0 or 1 according to the floating point format suported by the
   machine */
#define HAVE_IEEEFP 1

/* Define to 1 if you have the `jbg_newlen' function. */
#define HAVE_JBG_NEWLEN 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <io.h> header file. */
#define HAVE_IO_H 1

/* Define to 1 if you have the <search.h> header file. */
#if !defined( __XCC__ )
#define HAVE_SEARCH_H 1
#endif

/* Define to 1 if you have the `setmode' function. */
#define HAVE_SETMODE 1

/* The size of a `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of a `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* Signed 64-bit type formatter */
#define TIFF_INT64_FORMAT "%I64d"

/* Signed 64-bit type */
#define TIFF_INT64_T signed __int64

/* Unsigned 64-bit type formatter */
#define TIFF_UINT64_FORMAT "%I64u"

/* Unsigned 64-bit type */
#define TIFF_UINT64_T unsigned __int64

/* Set the native cpu bit order */
#define HOST_FILLORDER FILLORDER_LSB2MSB

#define snprintf _snprintf

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
# ifndef inline
#  define inline __inline
# endif
#endif

#define lfind _lfind
/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * fill-column: 78
 * End:
 */

#endif /* __TIF_CONGIG_H */
