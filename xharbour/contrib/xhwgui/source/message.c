/*
 *$Id: message.c,v 1.3 2004/02/07 15:24:22 lculik Exp $
 *
 * HWGUI - Harbour Win32 GUI library source code:
 * C level messages functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#include <windows.h>
#include "hbapi.h"

HB_FUNC( MSGINFO )
{
   char* cTitle = ( hb_pcount() == 1 )? "":hb_parc( 2 );

   MessageBox( GetActiveWindow(), hb_parc(1), cTitle, MB_OK | MB_ICONINFORMATION );
}

HB_FUNC( MSGSTOP )
{
   char* cTitle;

   cTitle = ( hb_pcount() == 1 )? "":hb_parc( 2 );

   MessageBox( GetActiveWindow(), hb_parc(1), cTitle, MB_OK | MB_ICONSTOP );
}

HB_FUNC( MSGOKCANCEL )
{
   char* cTitle;

   cTitle = ( hb_pcount() == 1 )? "":hb_parc( 2 );

   hb_retni( MessageBox( GetActiveWindow(), hb_parc(1), cTitle, MB_OKCANCEL | MB_ICONQUESTION ) );
}

HB_FUNC( MSGYESNO )
{
   char* cTitle;
   HWND h = GetActiveWindow();

   cTitle = ( hb_pcount() == 1 )? "":hb_parc( 2 );

   hb_retl( MessageBox( h, hb_parc(1), cTitle, MB_YESNO | MB_ICONQUESTION ) == IDYES );
}

HB_FUNC( MSGYESNOCANCEL )
{
   char* cTitle;
   HWND h = GetActiveWindow();

   cTitle = ( hb_pcount() == 1 )? "":hb_parc( 2 );

   hb_retni( MessageBox( h, hb_parc(1), cTitle, MB_YESNOCANCEL | MB_ICONQUESTION ) );
}

HB_FUNC( MSGEXCLAMATION )
{
   char* cTitle = ( hb_pcount() == 1 )? "":hb_parc( 2 );
   HWND h = GetActiveWindow();

   MessageBox( h, hb_parc(1), cTitle, MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL  );
}

HB_FUNC( MSGRETRYCANCEL )
{
   char* cTitle;
   HWND h = GetActiveWindow();

   cTitle = ( hb_pcount() == 1 )? "":hb_parc( 2 );

   hb_retni( MessageBox( h, hb_parc(1), cTitle, MB_RETRYCANCEL | MB_ICONQUESTION | MB_ICONQUESTION ) );
}

HB_FUNC( MSGBEEP )
{
   MessageBeep( ( hb_pcount() == 0 )? 0xFFFFFFFF:hb_parnl(1) );
}


#include <commctrl.h>
#include <richedit.h>
HB_FUNC( MSGTEMP )
{
   char cres[60];

   sprintf( cres,"WS_OVERLAPPEDWINDOW: %lu NM_FIRST: %lu ",WS_OVERLAPPEDWINDOW,NM_FIRST );
   hb_retni( MessageBox( GetActiveWindow(), cres, "DialogBaseUnits", MB_OKCANCEL | MB_ICONQUESTION ) );
}


