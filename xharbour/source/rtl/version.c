/*
 * $Id: version.c,v 1.11 2005/11/16 12:16:45 druzus Exp $
 */

/*
 * Harbour Project source code:
 * OS(), VERSION(), HB_COMPILER() functions
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    HB_COMPILER()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbinit.h"
#include "hbapilng.h"
#include "hbver.h"

extern char * hb_verPCode( void );
extern void hb_ParseLine( PHB_ITEM pReturn, char * szText, int iDelimiter, int * iWord );
extern char *hb_credits( void );
extern int hb_arrayMode( void );

HB_FUNC( OS )
{
   char * pszPlatform = hb_verPlatform();
   hb_retcAdopt( pszPlatform );
}

HB_FUNC( HB_COMPILER )
{
   char * pszCompiler = hb_verCompiler();
   hb_retcAdopt( pszCompiler );
}

HB_FUNC( VERSION )
{
   char * pszVersion = hb_verHarbour();
   hb_retcAdopt( pszVersion );
}

HB_FUNC( HB_PCODEVER )
{
   char * pszPCodeVersion = hb_verPCode();
   hb_retcAdopt( pszPCodeVersion );
}

HB_FUNC( HB_BUILDDATE )
{
   char *szBldDate = hb_builddate() ;
   hb_retcAdopt( szBldDate );
}

HB_FUNC( HB_BUILDINFO )
{
   PHB_ITEM pQuery = hb_param( 1, HB_IT_INTEGER );
   HB_ITEM hbInfo;
   HB_ITEM Return;
   int iWords = 0;
   int ui;
   HB_ITEM Temp;
   char * pszBuildInfo = hb_verBuildInfo( FALSE );

   ( &hbInfo )->type = HB_IT_NIL;
   ( &Return )->type = HB_IT_NIL;
   ( &Temp   )->type = HB_IT_NIL;

   hb_arrayNew( &hbInfo, 0 );

   hb_ParseLine( &hbInfo, pszBuildInfo, '\t', &iWords );
   hb_xfree( pszBuildInfo );

   hb_arrayNew( &Return, iWords );

   for ( ui=0; ui<iWords; ui++ )
   {
      char * szInfo = hb_arrayGetC( &hbInfo, ui + 1 );
      int iLen = strlen( szInfo );

      if( hb_strnicmp( szInfo, "yes", 3 ) == 0 )
      {
         hb_arraySetForward( &Return, ui + 1, hb_itemPutL( &Temp, TRUE ) );
      }
      else if( hb_strnicmp( szInfo, "no", 2 ) == 0 )
      {
         hb_arraySetForward( &Return, ui + 1, hb_itemPutL( &Temp, FALSE ) );
      }
      else if ( iLen > 5 && ( szInfo[iLen-1] == ')' && szInfo[iLen-2] == 'm' && szInfo[iLen-3] == 'u' && szInfo[iLen-4] == 'n' && szInfo[iLen-5] == '(') )
      {
         szInfo[ iLen - 5 ] = 0;
         hb_arraySetForward( &Return, ui + 1, hb_itemPutNI( &Temp, atoi( szInfo ) ) );
      }
      else
      {
         hb_arraySetForward( &Return, ui + 1, hb_itemPutC( &Temp, szInfo ) );
      }

      hb_xfree( szInfo );
   }

   // add info on MT and VM Optimization
   {
      PHB_ITEM pMT = hb_itemDoC( "HB_MULTITHREAD", 0, NULL, NULL );
      BOOL lMT = pMT->item.asLogical.value;
      PHB_ITEM pOpt = hb_itemDoC( "HB_VMMODE", 0, NULL, NULL );
      int iOpt = pOpt->item.asInteger.value;

      hb_arrayAddForward( &Return, hb_itemPutL( &Temp, lMT ) );
      hb_arrayAddForward( &Return, hb_itemPutNI( &Temp, iOpt ) );

      hb_itemRelease( pMT );
      hb_itemRelease( pOpt );
   }

   // Default Language
   hb_arrayAddForward( &Return, hb_itemPutC( &Temp, hb_langID() ) );

   // Array Mode, 0 = Counter, 1 = Owner
   hb_arrayAddForward( &Return, hb_itemPutNI( &Temp, hb_arrayMode() ) );

   // Contributors
   {
      HB_ITEM Credits;
      char *szCredits = hb_credits();

      ( &Credits )->type = HB_IT_NIL;
      hb_arrayNew( &Credits, 0 );
      hb_ParseLine( &Credits, szCredits, '\n', &iWords );
      hb_arrayAddForward( &Return, &Credits );
   }

   if( pQuery )
   {
      int iQuery = pQuery->item.asInteger.value;

      if( iQuery < _HB_VER_LAST )
      {
         HB_ITEM Query;
         ( &Query )->type = HB_IT_NIL;
         hb_arrayGet( &Return, iQuery, &Query );
         hb_itemReturnForward( &Query );
      }
      hb_itemClear( &Return );
   }
   else
   {
      hb_itemReturnForward( &Return );
   }

   hb_itemClear( &hbInfo );

}

#define __PRG_SOURCE__ __FILE__

HB_FUNC_EXTERN( HB_VMMODE );
HB_FUNC_EXTERN( HB_MULTITHREAD );

#undef HB_PRG_PCODE_VER
#define HB_PRG_PCODE_VER HB_PCODE_VER

static PHB_ITEM *pGlobals = NULL;

HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_HBVER )
{ "HB_VMMODE",      {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VMMODE )},      NULL },
{ "HB_MULTITHREAD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MULTITHREAD )}, NULL }
HB_INIT_SYMBOLS_END( hb_vm_SymbolInit_HBVER )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup hb_vm_SymbolInit_HBVER
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_SymbolInit_HBVER = hb_vm_SymbolInit_HBVER;
   #pragma data_seg()
#endif
