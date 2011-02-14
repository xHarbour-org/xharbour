/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for automatic static initialization
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#ifndef HB_INIT_H_
#define HB_INIT_H_

#include "hbsetup.h"

HB_EXTERN_BEGIN

extern HB_EXPORT PSYMBOLS hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char *szModule, int iPCodeVer, PHB_ITEM *pGlobals ); /* statics symbols initialization */

#if defined( __EXPORT__ ) && defined( __cplusplus ) && defined( _MSC_VER ) && ( _MSC_VER < 1400 )
   #if !defined( HB_STATIC_STARTUP )
      #define HB_STATIC_STARTUP
   #endif
#endif

#if defined( _MSC_VER ) && !defined(HB_OS_WIN_64) && \
    !defined( __LCC__ ) && !defined( __POCC__ ) && !defined( __XCC__ ) && \
    !defined(HARBOUR_STRICT_ANSI_C) && !defined(HB_STATIC_STARTUP) && \
    !defined( HB_PRAGMA_STARTUP ) && !defined( HB_MSC_STARTUP )

   /* In order to maintain compatibility with other products, MSVC should
      always use this startup.  If you know that you can use HB_STATIC_STARTUP
      below, then all you need to do is define HB_STATIC_STARTUP to the
      compiler.

      Sat 07 Maj 2005 02:46:38 CEST
      This is only necessary when you want to create binary libs using
      MSC in C++ mode (-TP switch) and later this binaries will be linked
      by standard C linker with [x]Harbour programs. I strongly suggest
      to for 3-rd party developers to use MSC in standard C mode to create
      libraries which can be used with standard C compilers. This will
      eliminate the problem and we will be able to set C++ initialization
      as default for MSC in C++ mode. Druzus.
   */

   #define HB_MSC_STARTUP

#endif


#define HB_MODULE_GLOBALS NULL
#define HB_MODULE_NAMESPACES NULL

#if defined(HARBOUR_STRICT_ANSI_C)

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static PSYMBOLS pModuleSymbols; \
      static HB_DYNS ModuleFakeDyn; \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_END( func ) \
      }; \
      static PHB_SYMB symbols; \
      void func( void ) \
      { \
         pModuleSymbols = hb_vmProcessSymbols( symbols_table, (USHORT) ( sizeof( symbols_table ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER, HB_MODULE_GLOBALS ); \
         pModuleSymbols->pNamespaces = HB_MODULE_NAMESPACES; \
         symbols = pModuleSymbols->pSymbolTable; \
         ModuleFakeDyn.pModuleSymbols = pModuleSymbols; \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
      }

#elif defined( __GNUC__ )

   #if defined( HB_PRAGMA_STARTUP ) || defined( HB_MSC_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static PSYMBOLS pModuleSymbols; \
      static HB_DYNS ModuleFakeDyn; \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_END( func ) \
      }; \
      static PHB_SYMB symbols; \
      static void __attribute__ ((constructor)) func( void ) \
      { \
         pModuleSymbols = hb_vmProcessSymbols( symbols_table, (USHORT) ( sizeof( symbols_table ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER, HB_MODULE_GLOBALS ); \
         pModuleSymbols->pNamespaces = HB_MODULE_NAMESPACES; \
         symbols = pModuleSymbols->pSymbolTable; \
         ModuleFakeDyn.pModuleSymbols = pModuleSymbols; \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static void __attribute__ ((constructor)) func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
      }

#elif defined( HB_MSC_STARTUP )

   #if defined( HB_PRAGMA_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define HB_DATASEG_STARTUP

   typedef int (* HB_$INITSYM)( void );

   #if _MSC_VER >= 1010
      #define HB_STARTUP_SEGMENT ".CRT$XIY"
   #else
      #define HB_STARTUP_SEGMENT "XIY"
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static PSYMBOLS pModuleSymbols; \
      static HB_DYNS ModuleFakeDyn; \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_END( func ) \
      }; \
      static PHB_SYMB symbols; \
      static int func( void ) \
      { \
         pModuleSymbols = hb_vmProcessSymbols( symbols_table, (USHORT) ( sizeof( symbols_table ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER, HB_MODULE_GLOBALS ); \
         pModuleSymbols->pNamespaces = HB_MODULE_NAMESPACES; \
         symbols = pModuleSymbols->pSymbolTable; \
         ModuleFakeDyn.pModuleSymbols = pModuleSymbols; \
         return 0; \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static int func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
         return 0; \
      }

   #define HB_DATASEG_FUNC( func )     HB_DATASEG_FUNC_( func )
   #define HB_DATASEG_FUNC_( func ) \
      static HB_$INITSYM _s_init_func_##func = func;

   /*  After each '_END' symbol, additional 'hooks' are required See the C
       output of a generated prg for example
   */

#elif defined( HB_STATIC_STARTUP ) || defined( __cplusplus )

   #if defined( HB_PRAGMA_STARTUP ) || defined( HB_MSC_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static PSYMBOLS pModuleSymbols; \
      static HB_DYNS ModuleFakeDyn; \
      static HB_SYMB symbols_table[] = {

   /* this allows any macros to be preprocessed first
      so that token pasting is handled correctly */
   #define HB_INIT_SYMBOLS_END( func ) \
          _HB_INIT_SYMBOLS_END( func )

   #define _HB_INIT_SYMBOLS_END( func ) \
      }; \
      static PHB_SYMB symbols; \
      static int func( void ) \
      { \
         pModuleSymbols = hb_vmProcessSymbols( symbols_table, (USHORT) ( sizeof( symbols_table ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER, HB_MODULE_GLOBALS ); \
         pModuleSymbols->pNamespaces = HB_MODULE_NAMESPACES; \
         symbols = pModuleSymbols->pSymbolTable; \
         ModuleFakeDyn.pModuleSymbols = pModuleSymbols; \
         return 0; \
      } \
      static int DUMMY_RegisterSymbols_##func = func();

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static int func( void ) \
      {

   /* this allows any macros to be preprocessed first
      so that token pasting is handled correctly */
   #define HB_CALL_ON_STARTUP_END( func ) \
          _HB_CALL_ON_STARTUP_END( func )

   #define _HB_CALL_ON_STARTUP_END( func ) \
         return 0; \
      } \
      static int DUMMY_CallOnStart_##func = func();

#elif defined( HB_PRAGMA_STARTUP ) || \
      defined( __BORLANDC__ ) || defined( __LCC__ ) || defined( __POCC__ ) || defined( __XCC__ )

   #if defined( HB_MSC_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #if !defined( HB_PRAGMA_STARTUP )
      #define HB_PRAGMA_STARTUP
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static PSYMBOLS pModuleSymbols; \
      static HB_DYNS ModuleFakeDyn; \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_END( func ) \
      }; \
      static PHB_SYMB symbols; \
      static void func( void ) \
      { \
         pModuleSymbols = hb_vmProcessSymbols( symbols_table, (USHORT) ( sizeof( symbols_table ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER, HB_MODULE_GLOBALS ); \
         pModuleSymbols->pNamespaces = HB_MODULE_NAMESPACES; \
         symbols = pModuleSymbols->pSymbolTable; \
         ModuleFakeDyn.pModuleSymbols = pModuleSymbols; \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static void func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
      }

#elif defined( __WATCOMC__ )

   #if defined( HB_PRAGMA_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static PSYMBOLS pModuleSymbols; \
      static HB_DYNS ModuleFakeDyn; \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_END( func ) \
      }; \
      static PHB_SYMB symbols; \
      static void func( void ) \
      { \
         pModuleSymbols = hb_vmProcessSymbols( symbols_table, (USHORT) ( sizeof( symbols_table ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER, HB_MODULE_GLOBALS ); \
         pModuleSymbols->pNamespaces = HB_MODULE_NAMESPACES; \
         symbols = pModuleSymbols->pSymbolTable; \
         ModuleFakeDyn.pModuleSymbols = pModuleSymbols; \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static void func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
      }

   #define HB_DATASEG_STARTUP
   #define HB_STARTUP_SEGMENT          "XI"

   #define HB_WATCOM_STARTUP_ID        0x00
   #define HB_WATCOM_STARTUP_PRIORITY  0x40  /* default "program" priority */

   #pragma pack( __push, 1 )
   struct _s_init_info_
   {
      unsigned char     id;
      unsigned char     priority;
      void ( * func ) ( void );
   };
   #pragma pack( __pop )

   #define HB_DATASEG_FUNC( func )     HB_DATASEG_FUNC_( func )

   #define HB_DATASEG_FUNC_( func ) \
         static struct _s_init_info_ _s_init_info_##func = \
                  { HB_WATCOM_STARTUP_ID, HB_WATCOM_STARTUP_PRIORITY, func };
#else
   #error Unknown initialization method.
#endif

#include "pragma.h"

HB_EXTERN_END

#endif /* HB_INIT_H_ */
