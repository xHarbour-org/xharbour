/*
 * HWGUI - Harbour Win32 GUI library source code:
 * C level media functions
 *
 * Copyright 2003 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define OEMRESOURCE
#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

/*
 *  PlaySound( cName, lSync, lLoop )
 */
HB_FUNC ( PLAYSOUND )
{
   LPCSTR pszSound = ( hb_pcount()>0 && ISCHAR(1) )? hb_parc(1):NULL;
   HMODULE hmod = NULL;
   DWORD fdwSound = SND_NODEFAULT | SND_FILENAME;

   if( hb_pcount()>1 && ISLOG(2) && hb_parl(2) )
      fdwSound |= SND_SYNC;
   else
      fdwSound |= SND_ASYNC;
   if( hb_pcount()>2 && ISLOG(3) && hb_parl(3) )
      fdwSound |= SND_LOOP;
   if( !pszSound )
      fdwSound |= SND_PURGE;

   hb_retl( PlaySound( pszSound, hmod, fdwSound ) );

}

HB_FUNC ( MCISENDSTRING )
{
   BYTE cBuffer[128];

   hb_retnl( (LONG) mciSendString( (LPSTR) hb_parc(1), (LPSTR) cBuffer, 127,
               ( ISNIL(3) )? GetActiveWindow() : (HWND)hb_parnl(3) ) );
   if( !ISNIL(2) )
      hb_storc( (char*) cBuffer,2 );
}



/* Functions bellow for play video's and wav's*/

HB_FUNC(  MCISENDCOMMAND )       // ()
{
   hb_retnl( mciSendCommand( hb_parni( 1 ),      // Device ID
                           hb_parni( 2 ),      // Command Message
                           hb_parnl( 3 ),      // Flags
                           ( DWORD ) hb_parc( 4 ) ) );   // Parameter Block
}

//----------------------------------------------------------------------------//


   HB_FUNC(  MCIGETERRORSTRING )  // ()
{
   BYTE bBuffer[ 200 ];

   hb_retl( mciGetErrorString( hb_parnl( 1 ),       // Error Code
                             ( LPSTR ) bBuffer,
                             200 ) );
   hb_storc( ( char * ) bBuffer, 2 );
}

//----------------------------------------------------------------------------//

HB_FUNC(  NMCIOPEN )
{
   MCI_OPEN_PARMS mciOpenParms;
   DWORD dwFlags = 0;

   mciOpenParms.lpstrDeviceType = hb_parc( 1 );
   dwFlags = MCI_OPEN_ELEMENT;

   if( ISCHAR( 2 ) )
   {
      mciOpenParms.lpstrElementName = hb_parc( 2 );
      dwFlags |= MCI_OPEN_TYPE;
   }

   hb_retnl( mciSendCommand( 0, MCI_OPEN, dwFlags,
                           ( DWORD ) ( LPMCI_OPEN_PARMS ) &mciOpenParms ) );


   hb_storni( mciOpenParms.wDeviceID, 3 );
}

//----------------------------------------------------------------------------//

HB_FUNC(  NMCIPLAY )
{
   MCI_PLAY_PARMS mciPlayParms;
   DWORD dwFlags = 0;

   if( hb_parnl( 2 ) )
   {
      mciPlayParms.dwFrom = hb_parnl( 2 );
      dwFlags |= MCI_FROM;
   }

   if( hb_parnl( 3 ) )
   {
      mciPlayParms.dwTo = hb_parnl( 3 );
      dwFlags |= MCI_TO;
   }

   if( hb_parni( 4 ) )
   {
      mciPlayParms.dwCallback = ( DWORD ) ( LPVOID ) hb_parni( 4 );
      dwFlags |= MCI_NOTIFY;
   }

   hb_retnl( mciSendCommand( hb_parni( 1 ),         // Device ID
                           MCI_PLAY, dwFlags,
                           ( DWORD ) ( LPMCI_PLAY_PARMS ) &mciPlayParms ) );
}

//----------------------------------------------------------------------------//

HB_FUNC(  NMCIWINDOW )
{
   MCI_ANIM_WINDOW_PARMS mciWindowParms;
   MCI_ANIM_RECT_PARMS mciRectParms;
   HWND hWnd = ( HWND ) hb_parnl( 2 );

   mciWindowParms.hWnd = hWnd;

   hb_retnl( mciSendCommand( hb_parni( 1 ), MCI_WINDOW, MCI_ANIM_WINDOW_HWND | MCI_ANIM_WINDOW_DISABLE_STRETCH,
                   ( LONG ) ( LPMCI_ANIM_WINDOW_PARMS ) &mciWindowParms ) );
}


