
// What32.Lib
// DateTimePicker functions

#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400
#include <shlobj.h>

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "winreg.h"
#include "tchar.h"

//-----------------------------------------------------------------------------

//
// The GetSystemTime function retrieves the current system date and time.
// The system time is expressed in Coordinated Universal Time (UTC).
//
// SYNTAX:
// GetSystemtime() --> aSystemTime
//
//

HB_FUNC( GETSYSTEMTIME )
{
   SYSTEMTIME SysTime ;
   PHB_ITEM aSysTime ;
   PHB_ITEM temp ;
   long nRet;


   GetSystemTime( &SysTime );     // Pointer to a SYSTEMTIME structure.

   aSysTime = hb_itemArrayNew( 8 ) ;

       temp = hb_itemPutNL( NULL, SysTime.wYear );
       hb_arraySet( aSysTime, 1, temp );
       hb_itemRelease( temp );

       temp = hb_itemPutNL( NULL, SysTime.wMonth );
       hb_arraySet( aSysTime, 2, temp );
       hb_itemRelease( temp );

       temp = hb_itemPutNL( NULL, SysTime.wDayOfWeek );
       hb_arraySet( aSysTime, 3, temp );
       hb_itemRelease( temp );

       temp = hb_itemPutNL( NULL, SysTime.wDay );
       hb_arraySet( aSysTime, 4, temp );
       hb_itemRelease( temp );

       temp = hb_itemPutNL( NULL, SysTime.wHour );
       hb_arraySet( aSysTime, 5, temp );
       hb_itemRelease( temp );

       temp = hb_itemPutNL( NULL, SysTime.wMinute );
       hb_arraySet( aSysTime, 6, temp );
       hb_itemRelease( temp );

       temp = hb_itemPutNL( NULL, SysTime.wSecond );
       hb_arraySet( aSysTime, 7, temp );
       hb_itemRelease( temp );

       temp = hb_itemPutNL( NULL, SysTime.wMilliseconds );
       hb_arraySet( aSysTime, 8, temp );
       hb_itemRelease( temp );

   hb_itemReturn( aSysTime );
   hb_itemRelease( aSysTime );

}

//-----------------------------------------------------------------------------

// Sets a date and time picker (DTP) control to a given date and time.
// You can use this macro or send the DTM_SETSYSTEMTIME message explicitly.
//
// SYNTAX:
// SetSystemtime( aSystemTime ) --> lOk
//

HB_FUNC( SETSYSTEMTIME )
{
   SYSTEMTIME *SysTime ;

   if ( ISARRAY( 1 ) ) // array
   {
      SysTime->wYear         = (WORD)  hb_parnl( 1, 1 );
      SysTime->wMonth        = (WORD)  hb_parnl( 1, 2 );
      SysTime->wDayOfWeek    = (WORD)  hb_parnl( 1, 3 );
      SysTime->wDay          = (WORD)  hb_parnl( 1, 4 );
      SysTime->wHour         = (WORD)  hb_parnl( 1, 5 );
      SysTime->wMinute       = (WORD)  hb_parnl( 1, 6 );
      SysTime->wSecond       = (WORD)  hb_parnl( 1, 7 );
      SysTime->wMilliseconds = (WORD)  hb_parnl( 1, 8 );
   }
   else
   {
     if ( ISCHAR( 1 ) )  // xHarbour structure
     {
        SysTime =( SYSTEMTIME *) hb_param( 1, HB_IT_STRING)->item.asString.value;
     }
     else
     {
      hb_retl(0);
      return;
     }
   }

   hb_retl( SetSystemTime(
                      SysTime                  // Pointer to SYSTEMTIME structures
                         ) );

}

