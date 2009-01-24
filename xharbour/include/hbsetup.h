/*
 * $Id: hbsetup.h,v 1.53 2009/01/15 08:41:13 enricomaria Exp $
 */

/*
 * Harbour Project source code:
 * Header file for compiler and runtime configuration
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#ifndef HB_SETUP_H_
#define HB_SETUP_H_

#define HB_PCODE_VER 10
#define HB_HRB_VER 3

/* Don't change - this is the default for older compiled prgs.!!! */
#define HB_PRG_PCODE_VER 0

#define __STR__(x) #x
#define __STR(x) __STR__(x)

#include <limits.h>

/* ***********************************************************************
 * Include settings common for .prg and .c files
 */
#include "hbsetup.ch"

/* ***********************************************************************
 * NOTE: You can select the default language modul used by Harbour, by
 *       defining this to a valid language modul identifier.
 */

#ifndef HB_LANG_DEFAULT
   #define HB_LANG_DEFAULT       EN
#endif

/* ***********************************************************************
 * NOTE: You can select the default codepage used by Harbour, by
 *       defining this to a valid codepage modul identifier.
 */

#ifndef HB_CODEPAGE_DEFAULT
   #define HB_CODEPAGE_DEFAULT   EN
#endif

/* ***********************************************************************
 * If you turn this on, the memory subsystem will collect trace information
 * and several statistical data about memory management, it will show
 * these on exit if memory seem to have been leaked. The memory subsystem
 * will also do pointer checks. [vszakats]
 * This should be normally turned off in a final release.
 *
 * Note that if you turn this on, Harbour will be slighlty slower, larger
 * and will consume more memory.
 *
 * By default this is turned off.
 */
/*#define HB_FM_STATISTICS*/

/* ***********************************************************************
 * Enable profiler support in HVM
 * By default this is turned off. Define HB_USE_PROFILER to turn it on.
 */

#ifndef HB_USE_PROFILER
   #define HB_NO_PROFILER
#endif

/* ***********************************************************************
 * This symbol defines if Harbour is compiled using C compiler
 * that support strict ANSI C only
 *
 * The only non ANSI C feature that we are using is an ability
 * to call functions before the 'main' module is called.
 * This trick is used to automatically join all symbol tables defined
 * in run-time support modules and in user defined modules.
 *   If strict ANSI C compability is required then all symbol tables
 * have to be joined manually by calling special function named
 * hb_vm_SymbolInit_<module_name>
 * (for example for myfirst.prg it will be: 'hb_vm_SymbolInit_MYFIRST'
 * The generation of this function is performed by the macro called
 * HB_CALL_ON_STARTUP that is defined in 'hbinit.h'
 *
 * By default we are using extensions to ANSI C (symbol is not defined)
 */
/*#define HARBOUR_STRICT_ANSI_C */

/* ***********************************************************************
 * This symbol defines the calling convention used for Harbour level
 * functions.
 *
 * To use "pascal" mode, you should define this macro to "pascal".
 *
 * By default we are not using any special calling conventions.
 */
/*#define HB_FUNC_CALLCONV*/

/* ***********************************************************************
 * Define this option if you want the /y YACC trace option to be available
 * in the Harbour compiler.
 *
 * Note that if you turn this on, the compiler will slighly grow in size.
 *
 * By default this is turned off.
 * TODO: This should be disabled, when the parser has matured.
 */
/*#define HARBOUR_YYDEBUG*/


/* ***********************************************************************
 *
 */
#ifndef HB_ARRAY_USE_COUNTER_OFF
#  define HB_ARRAY_USE_COUNTER
#endif

/* *********************************************************************** */

/* ***********************************************************************
 * This macros cause that string shared (and static) items are always
 * unshared before pushing on VM stack if they are passed by reference.
 * It's unnecessary to have it as long as you are not using buggy 3-rd party
 * C code which writes directly to string buffer of items passed by reference
 * without checking if the buffer is shared or static.
 * Any 3-rd party code should always use xHarbour API function to store
 * values in parameters passed by reference. But if someone doesn't want
 * to do that and agree to update his code after modification in our VM
 * which we can make at any time then he _must_ to make at least the
 * following operation before writing to string buffer:
 *    pItem = hb_intemUnShare( pItem );
 *
 * In valid code this macro cause only slowness so it's disabled by default.
 */

/* #define HB_UNSHARE_REFERENCES */


/* ***********************************************************************
 * Use native Windows memory allocation functions (HB_OS_WIN_32)
 * This option can disable compiler memory allocation optimization
 * so you should really have a good reason to enable it
 */

/* #define HB_FM_WIN32_ALLOC */

/* ***********************************************************************
 * This symbol defines if we want an ability to create and link OBJ files
 * generated by Harbour compiler
 *
 * Note that the Virtual Machine support need a platform/compiler specific
 * assembler module, so you will be able to use this only with 32 bits
 * Borland C/C++ compilers.
 *
 * By default it is disabled (symbol is not defined)
 */
/*#define HARBOUR_OBJ_GENERATION*/

/* ***********************************************************************
 * You can select here, what type of main entry will be used in the
 * application (main() or WinMain()).
 *
 * By default the standard C main() function will be used.
 */
/*#define HARBOUR_MAIN_STD*/
/*#define HARBOUR_MAIN_WIN*/

/* ***********************************************************************
 * You can set here the maximum symbol name length handled by Harbour
 * compiler and runtime. You can override this setting in the make process.
 *
 * By default this value is 63
 */
#ifndef HB_SYMBOL_NAME_LEN
/* NOTE: For complete CA-Cl*pper compatibility you can set the maximum
         symbol name to 10. Sometimes this can be useful for compiling legacy
         code. [vszakats] */
/*
   #ifdef HB_C52_STRICT
      #define HB_SYMBOL_NAME_LEN   10
   #else
*/
      #define HB_SYMBOL_NAME_LEN   63
/*
   #endif
*/
#endif

/* ***********************************************************************
 * You can select here, if the preprocessor should be linked
 * for commands preprocessing passed to the macro compiler.
 * (Note, that if it is linked then commands preprocessing can be
 * disabled/enabled at runtime using HB_SETMACRO() function
 *
 * By default we do not support commands in the macro compiler.
 */
/* #define HB_MACRO_STATEMENTS */


/* ***********************************************************************
 * This fixes a bug in Clipper that allowed for copy array elements
 * beyond the destination array size
 *
 * By default we are 100% Clipper compatible
 */
/* #define HB_FIX_ACOPY_BUG */

/* ***********************************************************************
 * This controls an optimisation in ASORT() function
 *
 * If this is defined the item copying is optimized, in a way that
 * instead of calling the official hb_itemCopy(), the item structures
 * will be directly copied with memcpy(), this means that the related
 * data areas (string space for example) will never be moved. This can be
 * safely done here, because it's guaranteed by the nature of sorting
 * that the set of items doesn't change (there're no deleted or new
 * items, just swapping) in this functions.
 * Using this option makes sorting *much* faster, but if you have a
 * problem, or the low level stuff changes, turn it off. [vszakats]
 */
#define HB_ASORT_OPT_ITEMCOPY

/* ***********************************************************************
 * You can select here faster but less secure behaviour of STOD() function
 * There is no data validation if this is enabled.
 *
 * By default we are using secure method.
*/
/* #define HB_FAST_STOD */



/* ***********************************************************************
 * Operating system specific definitions
 */
#if ( defined(__GNUC__) && \
      ! ( defined(__DJGPP__) || defined(__EMX__) || defined(__RSXNT__) || \
          defined(_Windows) || defined(_WIN32) || defined(_WINCE) ) ) || \
    ( defined(__WATCOMC__) && defined(__LINUX__) )
   #define HOST_OS_UNIX_COMPATIBLE
   #define HB_OS_UNIX_COMPATIBLE
   #define HB_OS_PATH_LIST_SEP_CHR      ':'
   #define HB_OS_PATH_DELIM_CHR         '/'
   #define HB_OS_PATH_DELIM_CHR_STRING  "/"
   #define HB_OS_PATH_DELIM_CHR_LIST    "/"
   #define HB_OS_ALLFILE_MASK           "*"
   #undef  HB_OS_DRIVE_DELIM_CHR
   #undef  HB_OS_HAS_DRIVE_LETTER
   #define HB_OS_OPT_DELIM_LIST         "-"
   #define HB_OS_EOL_LEN                1
#else
   /* we are assuming here the DOS compatible OS */
   #define HB_OS_DOS_COMPATIBLE
   #define HB_OS_PATH_LIST_SEP_CHR      ';'
   #define HB_OS_PATH_DELIM_CHR         '\\'
   #define HB_OS_PATH_DELIM_CHR_STRING  "\\"
   #define HB_OS_PATH_DELIM_CHR_LIST    "\\/:"
   #define HB_OS_ALLFILE_MASK           "*.*"
   #define HB_OS_DRIVE_DELIM_CHR        ':'
   #define HB_OS_HAS_DRIVE_LETTER
   #define HB_OS_OPT_DELIM_LIST         "/-"
   #define HB_OS_EOL_LEN                2  /* # of bytes in End of Line marker */
#endif

#ifndef _POSIX_PATH_MAX
    #if defined( MAX_PATH )
       #define _POSIX_PATH_MAX    MAX_PATH  // 260
    #else
        #if defined( _MAX_PATH )
           #define _POSIX_PATH_MAX    _MAX_PATH // 260
        #else
           #define _POSIX_PATH_MAX    255
        #endif
    #endif
#endif

#define HB_ISOPTSEP( c ) ( strchr( HB_OS_OPT_DELIM_LIST, ( c ) ) != NULL )

/* NOTE:
   Compiler                                _MSC_VER value
   --------                                --------------
   C Compiler version 6.0                  600
   C/C++ compiler version 7.0              700
   Visual C++, Windows, version 1.0        800
   Visual C++, 32-bit, version 1.0         800
   Visual C++, Windows, version 2.0        900
   Visual C++, 32-bit, version 2.x         900
   Visual C++, 32-bit, version 4.0         1000
   Visual C++, 32-bit, version 5.0         1100
   Visual C++, 32-bit, version 6.0         1200
   Visual Studio .NET, version 7.0         1300
   Visual Studio .NET 2003, version 7.1    1310
   Visual Studio 2005, version 8.0         1400
   Visual Studio 2008, version 9.0         1500
*/

/* ***********************************************************************
 * Platform detection
 */

#if defined(__WATCOMC__)
   #if defined(__OS2__)
      #define HB_OS_OS2
   #elif defined(__NT__) || defined(__WINDOWS_386__) || defined(__WINDOWS__)
      #define HB_OS_WIN_32
   #elif defined(__LINUX__)
      #define HB_OS_LINUX
   #elif defined(__386__)
      #define HB_OS_DOS
      #define HB_OS_DOS_32
   #else
      #define HB_OS_DOS
      #define HB_OS_DOS_16
   #endif
#endif

#ifndef HB_OS_DOS
   #if defined(DOS) || defined(_QC) || defined(__DOS__) || defined(MSDOS) || defined(__MSDOS__) || defined(__RSX32__)
      #define HB_OS_DOS
      #if defined(__386__) || defined(__DJGPP__)
         #define HB_OS_DOS_32
      #else
         #define HB_OS_DOS_16
      #endif
   #endif
#endif

#if defined(__EMX__) && ! defined(__RSXNT__)
   #define HB_OS_OS2_GCC
#endif
#ifndef HB_OS_OS2
   #if defined(OS2) || defined(__OS2__) || defined(OS_2) || defined(HB_OS_OS2_GCC)
      #define HB_OS_OS2
   #endif
#endif

#ifndef HB_OS_WIN_32
   #if defined(WINNT) || defined(_Windows) || defined(__NT__) || defined(_WIN32) || defined(_WINDOWS_) || defined(__WINDOWS_386__) || defined(__WIN32__) || defined(_MSC_VER) || defined(__CYGWIN__)
      #define HB_OS_WIN_32
   #endif
#endif

/* Sub-option inside HB_OS_WIN_32 */
#ifndef HB_OS_WIN_64
   #if defined(_WIN64)
      #define HB_OS_WIN_64
   #endif
#endif

/* Sub-option inside HB_OS_WIN_32 */
#ifndef HB_WINCE
   #if defined(_WINCE) || defined(__CEGCC__) || defined(__MINGW32CE__) || (defined(__POCC_TARGET__) && __POCC_TARGET__ == 2)
      #define HB_WINCE
   #endif
#endif

#ifndef HB_OS_LINUX
   #if defined(linux) || defined(__linux) || defined(__linux__) || defined(__gnu_linux__)
      #define HB_OS_LINUX
   #endif
#endif

#ifndef HB_OS_SUNOS
   #if defined(sun) || defined(__sun) || defined(__sun__)
      #define HB_OS_SUNOS
   #endif
#endif

#ifndef HB_OS_HPUX
   /* HP cc in ANSI mode defines __hpux. GCC defines __hpux__ */
   #if defined(__hpux) || defined(__hpux__)
      #define HB_OS_HPUX
   #endif
#endif

#ifndef HB_OS_DARWIN
   #if defined(__APPLE__)
      #define HB_OS_DARWIN
   #endif
#endif

#ifndef HB_OS_BSD
   #if defined( __FreeBSD__ ) || defined( __NetBSD__ ) || defined( __OpenBSD__ ) || \
       defined( HB_OS_DARWIN ) || defined( __APPLE__ )
      #define HB_OS_BSD
   #endif
#endif

#ifndef HB_OS_UNIX
   #if defined(HB_OS_UNIX_COMPATIBLE) || \
       defined(HB_OS_LINUX) || \
       defined(HB_OS_DARWIN) || \
       defined(HB_OS_BSD) || \
       defined(HB_OS_SUNOS) || \
       defined(HB_OS_HPUX)
      #define HB_OS_UNIX
   #endif
#endif

#ifndef HB_OS_MAC
   #if defined(__MPW__)
      #define HB_OS_MAC
   #endif
#endif

/* ***********************************************************************
 * Here you can force the EOL string to be CRLF
 *
 * By default, the EOL string depends upon the detected platform.
 */
/* #define HB_EOL_CRLF */
#ifdef HB_EOL_CRLF
   #undef HB_OS_EOL_LEN
   #define HB_OS_EOL_LEN 2
#endif

/* Compatibility #defines. These will be removed, so 
   please use the new names in your code. */
#ifdef HB_LEGACY_LEVEL
   #define OS_PATH_DELIMITER            HB_OS_PATH_DELIM_CHR
   #ifdef HB_OS_UNIX_COMPATIBLE
      #define OS_UNIX_COMPATIBLE
   #endif
#endif

/* Compatibility #defines. These will be removed, so 
   please use the new names in your code. */
#ifdef HB_LEGACY_LEVEL
   #undef HB_NO_PROFILER
#endif

/* ***********************************************************************
 * See also the following files for task specific definitions/settings
 *
 * hbmath.h    - math errors handling
 */

/* ***********************************************************************
 * some fixes in compiler header files
 */

#if defined( __DJGPP__ )
   /* Fix DJGPP in call to: toupper(), tolower(), is...() */
   #include "hbfixdj.h"
#elif defined(__XCC__)
   #if !defined(isascii)
      #define isascii(c)   ((unsigned)(c)<=0x7f)
   #endif
   #if !defined(NEED_DUMMY_RETURN)
      #define NEED_DUMMY_RETURN
   #endif
#endif

/* ***********************************************************************
 * Extern "C" detection
 */

#if defined(__cplusplus) && !defined(__IBMCPP__)
   #define HB_EXTERN_C
   #define HB_EXTERN_BEGIN    extern "C" {
   #define HB_EXTERN_END      }
#else
   #define HB_EXTERN_BEGIN
   #define HB_EXTERN_END
#endif

#if defined( __GNUC__ )
   #define HB_PRINTF_FORMAT( _nStr, _nParam ) \
                     __attribute__ (( format (printf, _nStr, _nParam)))
   #define HB_MALLOC_ATTR \
                     __attribute__ (( malloc ))
   #define HB_HOT_ATTR \
                     __attribute__ (( hot ))
   #define HB_COLD_ATTR \
                     __attribute__ (( cold ))
#if 0
   #define HB_NORETURN_ATTR \
                     __attribute__ (( noreturn ))
#  else
   #define HB_NORETURN_ATTR
#  endif
#  if ( ( __GNUC__ > 4 ) || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 1 ) )
   #define HB_FLATTEN_ATTR \
                     __attribute__ (( flatten ))
#  else
   #define HB_FLATTEN_ATTR
#  endif
#  if ( ( __GNUC__ > 4 ) || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 3 ) )
   #define HB_ALLOC_SIZE_ATTR( _nParam ) \
                     __attribute__ (( alloc_size (_nParam)))
#  else
   #define HB_ALLOC_SIZE_ATTR( _nParam )
#  endif
#else
   #define HB_PRINTF_FORMAT( _nStr, _nParam )
   #define HB_MALLOC_ATTR
   #define HB_NORETURN_ATTR
   #define HB_HOT_ATTR
   #define HB_COLD_ATTR
   #define HB_FLATTEN_ATTR
   #define HB_ALLOC_SIZE_ATTR( _nParam )
#endif

/*
 * Auto SYNCH our HB_THREAD_SUPPORT flag with Compiler MT mode.
 */
#if defined(__XCC__) || defined(__POCC__) || defined(_MSC_VER) || defined(__BORLANDC__)
   #if defined(__MT__) || defined(_MT)
      #ifndef HB_THREAD_SUPPORT
         //#pragma message( "Warning: MT C compilation but HB_THREAD_SUPPORT not defined." )
      #endif
   #else
       #ifdef HB_THREAD_SUPPORT
          #error HB_THREAD_SUPPORT can not be used without MT support of C compiler!
          #undef HB_THREAD_SUPPORT
       #endif
   #endif
#endif


#endif /* HB_SETUP_H_ */
