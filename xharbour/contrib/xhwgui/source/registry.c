/*
 * HWGUI - Harbour Win32 GUI library source code:
 * Registry handling functions
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define OEMRESOURCE
#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

/*
 * Regcreatekey( handle, cKeyName ) --> handle
*/

HB_FUNC( REGCREATEKEY )
{
   HKEY hkResult = NULL;
   DWORD dwDisposition;

   if( RegCreateKeyEx( (HKEY)hb_parnl(1), hb_parc(2), 0, NULL, 0, KEY_ALL_ACCESS, 
          NULL, &hkResult, &dwDisposition ) == ERROR_SUCCESS )
   {
      hb_retnl( (ULONG) hkResult );
   }
   else
      hb_retnl( -1 );
}

/*
 * RegOpenKey( handle, cKeyName ) --> handle
*/

HB_FUNC( REGOPENKEY )
{
   HKEY hkResult = NULL;

   if( RegOpenKeyEx( (HKEY)hb_parnl(1), hb_parc(2), 0, KEY_ALL_ACCESS, 
                &hkResult ) == ERROR_SUCCESS )
   {
      hb_retnl( (ULONG) hkResult );
   }
   else
      hb_retnl( -1 );
}

/*
 * RegCloseKey( handle )
*/

HB_FUNC( REGCLOSEKEY )
{
   RegCloseKey( (HKEY)hb_parnl(1) );
}

/*
 * RegSetString( handle, cKeyName, cKeyValue ) --> 0 (Success) or -1 (Error)
*/

HB_FUNC( REGSETSTRING )
{
   if( RegSetValueEx( (HKEY)hb_parnl(1), hb_parc(2), 0, REG_SZ, 
           (BYTE*)hb_parc(3), hb_parclen(3)+1 ) == ERROR_SUCCESS )
      hb_retnl( 0 );
   else
      hb_retnl( -1 );
}

HB_FUNC( REGSETBINARY )
{
   if( RegSetValueEx( (HKEY)hb_parnl(1), hb_parc(2), 0, REG_BINARY, 
           (BYTE*)hb_parc(3), hb_parclen(3)+1 ) == ERROR_SUCCESS )
      hb_retnl( 0 );
   else
      hb_retnl( -1 );
}

HB_FUNC( REGGETVALUE )
{
   HKEY hKey = (HKEY)hb_parnl(1);
   LPTSTR lpValueName = (LPTSTR)hb_parc(2);
   DWORD lpType = 0;
   LPBYTE lpData;
   DWORD lpcbData;
   int length;

   if( RegQueryValueEx( hKey, lpValueName, NULL,NULL,NULL,&lpcbData ) == ERROR_SUCCESS )
   {
      length = (int) lpcbData;
      lpData = (LPBYTE)hb_xgrab( length+1 );
      if( RegQueryValueEx( hKey, lpValueName, NULL,&lpType,lpData,&lpcbData ) == ERROR_SUCCESS )
      {
         hb_retclen( (char*)lpData,(lpType==REG_SZ || lpType==REG_MULTI_SZ || lpType==REG_EXPAND_SZ)? length-1:length );
         if( hb_pcount() > 2 )
            hb_stornl( (LONG) lpType,3 );
      }
      else
         hb_ret();
      hb_xfree( lpData );
   }
   else
      hb_ret();
}
