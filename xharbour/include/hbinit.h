/*
 * $Id: hbinit.h,v 1.16 2005/03/04 17:18:33 druzus Exp $
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

extern void HB_EXPORT hb_vmProcessSymbols( PHB_SYMB pSymbols, ... ); /* statics symbols initialization */

#if defined(HARBOUR_STRICT_ANSI_C)

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols[] = {

   #define HB_INIT_SYMBOLS_END( func ) }; \
      void func( void ) \
      { \
         hb_vmProcessSymbols( symbols, (USHORT) ( sizeof( symbols ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER ); \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      func( void ) {

   #define HB_CALL_ON_STARTUP_END( func ) }

#elif defined(__GNUC__)

   #if defined(HB_PRAGMA_STARTUP) || defined(HB_MSC_STARTUP)
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols[] = {

   #define HB_INIT_SYMBOLS_END( func )  }; \
      static void __attribute__ ((constructor)) func( void ) \
      { \
         hb_vmProcessSymbols( symbols, (USHORT) ( sizeof( symbols ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER ); \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static void __attribute__ ((constructor)) func( void ) {

   #define HB_CALL_ON_STARTUP_END( func ) }

#elif defined(HB_STATIC_STARTUP) || defined(__cplusplus)

   #if defined(HB_PRAGMA_STARTUP) || defined(HB_MSC_STARTUP)
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols[] = {

   #define HB_INIT_SYMBOLS_END( func ) }; \
      static int func( void ) \
      { \
         hb_vmProcessSymbols( symbols, (USHORT) ( sizeof( symbols ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER ); \
         return 0; \
      } \
      static int hb_vm_auto_##func = func();

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static int func( void ) {

   /* this allows any macros to be preprocessed first
      so that token pasting is handled correctly */
   #define HB_CALL_ON_STARTUP_END( func ) \
          _HB_CALL_ON_STARTUP_END( func )

   #define _HB_CALL_ON_STARTUP_END( func ) \
      return 0; } \
      static int static_int_##func = func();

#elif defined(HB_PRAGMA_STARTUP) || \
      defined(__BORLANDC__) || defined(__LCC__) || defined(__POCC__)

   #if defined(HB_MSC_STARTUP)
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #if !defined(HB_PRAGMA_STARTUP)
      #define HB_PRAGMA_STARTUP
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols[] = {

   #define HB_INIT_SYMBOLS_END( func )  }; \
      static void func( void ) \
      { \
         hb_vmProcessSymbols( symbols, (USHORT) ( sizeof( symbols ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER ); \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static void func( void ) {

   #define HB_CALL_ON_STARTUP_END( func ) }

#elif defined(HB_MSC_STARTUP) || defined(_MSC_VER)

   /* This section is used for MSC in C mode. C++ mode will
      use HB_STATIC_STARTUP above.
   */

   #if !defined(HB_MSC_STARTUP)
      #define HB_MSC_STARTUP
   #endif

   typedef int (* HB_$INITSYM)( void );

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols[] = {

   #define HB_INIT_SYMBOLS_END( func ) }; \
      static int func( void ) \
      { \
         hb_vmProcessSymbols( symbols, (USHORT) ( sizeof( symbols ) / sizeof( HB_SYMB ) ), __PRG_SOURCE__, (int) HB_PRG_PCODE_VER ); \
         return 0; \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static int func( void ) {

   #define HB_CALL_ON_STARTUP_END( func ) \
         return 0; \
      }

   /*  After each '_END' simbol, additional 'hooks' are required See the C
       output of a generated prg for example
   */

#else
   #error Unknown initialization method.
#endif

HB_EXTERN_END

#endif /* HB_INIT_H_ */
