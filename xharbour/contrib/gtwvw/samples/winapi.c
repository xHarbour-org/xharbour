/*
   WINAPI.C
   collection of usefull WinAPI wrappers
   and low level routines
*/

#define _WIN32_WINNT   0x0400

#include <windows.h>
#include <shlobj.h>

#define HB_OS_WIN_32_USED

#include <math.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

//********************************************************************

HB_FUNC ( MESSAGEBOX )
{
 //param:
 //1: IN : window handle of caller (parent)
 //2: IN : message
 //3: IN : caption
 //4: IN : flags (buttons+icons etc)

  hb_retnl( MessageBox( ISNIL(1) ? NULL : (HWND) hb_parnl(1) ,
                        (LPCSTR) hb_parc(2),
                        ISNIL(3) ? NULL : (LPCSTR) hb_parc(3) ,
                        ISNIL(4) ? 0 : (UINT) hb_parnl(4) ) ) ;
}

HB_FUNC( LISWINNT )
{
  OSVERSIONINFO osvi ;
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx (&osvi);
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT ); // && osvi.dwMajorVersion >= 4);
}


//********************************************************************

HB_FUNC ( GETSYSCOLOR )
{
  // param: i
  // where 0 <= i <= 28
  // returns sys color number i
  int i = ISNIL(1) ? 0 : hb_parni(1);

  if ( i >= 0 && i < 29 )  /* Test bound error */
  {
    hb_retnl( (LONG) GetSysColor(i) );
  }
}

//********************************************************************


//********************************************************************
// SUPPORTING FUNCTIONS
//********************************************************************

HB_FUNC( LOWORD )
{
   hb_retni( (int) ( hb_parnl( 1 ) & 0xFFFF ) );
}

//-------------------------------------------------------------------//

HB_FUNC( HIWORD )
{
   hb_retni( (int) ( ( hb_parnl( 1 ) >> 16 ) & 0xFFFF ) );
}
