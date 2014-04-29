/*
 * $Id: win_misc.c 9983 2014-02-16 15:00:39Z zsaulius $
 */

/*
 * xHarbour Project source code:
 * Misc Windows API functions
 *
 * Copyright 2014 Saulius Zrelskis <labitas@gmail.com>
 * www - http://www.xharbour.org
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
 * As a special exception, the xHarbour Project gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * xHarbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

//---------------------------------------------------------------------------//

#define HB_OS_WIN_USED

#if defined( HB_OS_WIN )

#include "hbapi.h"
#include <windows.h>


//---------------------------------------------------------------------------//
HB_FUNC( ANSITOWIDE )  // ( cAnsiStr ) -> cWideStr
{
   const char * szString = hb_parc( 1 );

   if( szString )
   {
      int iLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, szString, -1, NULL, 0 );

      if( iLen )
      {
         LPWSTR szWide = ( LPWSTR ) hb_xgrab( iLen * sizeof( wchar_t ) );

         if( MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, szString, -1, szWide, iLen ) )
         {
            hb_retclenAdoptRaw( ( char * ) szWide, ( wcslen( szWide ) + 1 ) * sizeof( wchar_t ) );
            return;
         }
         else
         {
            hb_xfree( szWide );
         }
      }
   }

   hb_ret();
}

//---------------------------------------------------------------------------//
HB_FUNC( WIDETOANSI )  // ( cWideStr ) -> cAnsiStr
{
   LPWSTR szWide = ( LPWSTR ) hb_parc( 1 );

   if( szWide )
   {
      int iLen = WideCharToMultiByte( CP_ACP, 0, szWide, -1, NULL, 0, NULL, NULL );

      if( iLen )
      {
         char * szString = ( char * ) hb_xgrab( iLen );

         if( WideCharToMultiByte( CP_ACP, 0, szWide, -1, szString, iLen, NULL, NULL ) )
         {
            hb_retclenAdopt( szString, strlen( szString ) );
            return;
         }
         else
         {
            hb_xfree( szString );
         }
      }
   }

   hb_ret();
}

//---------------------------------------------------------------------------//
HB_FUNC( MESSAGEBOX )  // ( hWnd, cText, cCaption, nType ) -> nID
{
   hb_retni( MessageBox( ( HWND ) hb_parns( 1 ), hb_parcx( 2 ), hb_parcx( 3 ),
                         ISNIL( 4 ) ? MB_OK : hb_parni( 4 ) ) );
}


#endif  //defined( HB_OS_WIN )
