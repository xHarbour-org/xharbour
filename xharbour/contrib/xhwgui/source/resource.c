/*
 * HWGUI - Harbour Win32 GUI library source code:
 * C level resource functions
 *
 * Copyright 2003 Luiz Rafael Culik Guimaraes <culikr@brtrubo.com>
 * www - http://sites.uol.com.br/culikr/

*/


#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
// #define OEMRESOURCE
#include <windows.h>
#if defined(__MINGW32__)
   #include <prsht.h>
#endif
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "item.api"
#include "guilib.h"
HMODULE hModule ;
void hb_resourcemodules( void );

HB_FUNC(GETRESOURCES)
{
   hb_retnl( ( LONG ) hModule );
}

HB_FUNC(LOADSTRING)
{
   char Buffer[ 2048 ];
   int  BuffRet ;
   BuffRet = LoadString( ( HINSTANCE ) hModule , ( UINT ) hb_parnl( 2 ), Buffer, 2048 );
   hb_retclen(Buffer, BuffRet);
}

HB_FUNC(LOADRESOURCE)
{
if ( ISCHAR( 1 ) )
    hModule = GetModuleHandle( hb_parc( 1 ) );
else
    hModule = GetModuleHandle( NULL ) ;
}


#pragma startup hb_resourcemodules

void hb_resourcemodules( void )
{
    hModule = GetModuleHandle( NULL ) ;
}
