/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *     Copyright 2004 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.

*/

/*
 *
 *  Operating system functions for Win32
 *
 *  Program to check and set Windows Registry settings
 *  for safe networking - all versions of Windows to XP SP2
 *
 *
 *  Also includes check for buggy VREDIR.VXD under Win95
 *  and if the correct patch file is found - run it.

*/

#include "directry.ch"

FUNCTION OS_NETREGOK( lSetIt, lDoVista )

   LOCAL rVal := .T. , cKeySrv, cKeyWks

   IF lSetIt == NIL
      lSetIt := .F.
   ENDIF
   IF lDoVista == NIL
      lDoVista := .T.
   ENDIF
   IF !lDoVista .AND. OS_ISWINVISTA_OR_LATER() // 06/12/09 changed from OS_ISWINVISTA()
      //
   ELSEIF OS_ISWIN9X()
      rVal := QueryRegistry( 0, "System\CurrentControlSet\Services\VxD\VREDIR", "DiscardCacheOnOpen", 1, lSetIt )
   ELSE
      cKeySrv := "System\CurrentControlSet\Services\LanmanServer\Parameters"
      cKeyWks := "System\CurrentControlSet\Services\LanmanWorkStation\Parameters"
      lSetIt := lSetIt .AND. ( OS_ISWINNT() .OR. OS_ISUSERANADMIN() ) // 06/12/09 Only Try to set registry if Admin authority for Win2000 or later
      // Server settings
      rVal := rVal .AND. QueryRegistry( 0, cKeySrv, "CachedOpenLimit", 0, lSetIt )
      rVal := rVal .AND. QueryRegistry( 0, cKeySrv, "EnableOpLocks", 0, lSetIt ) // Q124916
      rVal := rVal .AND. QueryRegistry( 0, cKeySrv, "EnableOpLockForceClose", 1, lSetIt )
      rVal := rVal .AND. QueryRegistry( 0, cKeySrv, "SharingViolationDelay", 0, lSetIt )
      rVal := rVal .AND. QueryRegistry( 0, cKeySrv, "SharingViolationRetries", 0, lSetIt )
      IF OS_ISWINVISTA_OR_LATER()
         // // 06/12/09 If SMB2 is enabled then turning off oplocks does not work so SMB2 is required to be turned off on Server
         rVal := rVal .AND. QueryRegistry( 0, cKeySrv, "SMB2", 0, lSetIt )
      ENDIF

      // Workstation settings
      rVal := rVal .AND. QueryRegistry( 0, cKeyWks, "UseOpportunisticLocking", 0, lSetIt )
      rVal := rVal .AND. QueryRegistry( 0, cKeyWks, "EnableOpLocks", 0, lSetIt )
      rVal := rVal .AND. QueryRegistry( 0, cKeyWks, "EnableOpLockForceClose", 1, lSetIt )
      rVal := rVal .AND. QueryRegistry( 0, cKeyWks, "UtilizeNtCaching", 0, lSetIt )
      rVal := rVal .AND. QueryRegistry( 0, cKeyWks, "UseLockReadUnlock", 0, lSetIt )

      IF OS_ISWIN2000_OR_LATER()
         rVal := rVal .AND. QueryRegistry( 0, "System\CurrentControlSet\Services\MRXSmb\Parameters", "OpLocksDisabled", 1, lSetIt )
      ENDIF
   ENDIF

   RETURN( rVal )

FUNCTION OS_NETVREDIROK( nResult )

   LOCAL cWinDir, cFile, a

   nResult := 0
   IF OS_ISWIN9X()
      cWinDir := GetEnv( "WINDIR" )  // Get the folder that Windows is installed in
      IF Empty( cWinDir )
         cWinDir := "C:\WINDOWS"
      ENDIF
      cFile := cWinDir + "\SYSTEM\VREDIR.VXD"
      a := Directory( cFile )  // Check for faulty files.
      IF !Empty( a )
         IF a[ 1, F_SIZE ] == 156749 .AND. a[ 1, F_TIME ] == "11:11:10"
            nResult := 1111
         ELSEIF a[ 1, F_SIZE ] == 140343 .AND. a[ 1, F_TIME ] == "09:50:00"
            nResult := 950
         ENDIF
      ENDIF
   ENDIF

   RETURN( Empty( nResult ) )


#pragma BEGINDUMP
#if defined(HB_OS_WIN) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))

#include <windows.h>
#define HB_OS_WIN_USED
#include "hbapiitm.h"

static void getwinver(  OSVERSIONINFO * pOSvi )
{
  pOSvi->dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
  GetVersionEx ( pOSvi );
}

HB_FUNC( OS_ISWINNT )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT );
}

HB_FUNC( OS_ISWIN9X )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
}

HB_FUNC( OS_ISWIN95 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( OS_ISWIN98 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10 );
}

HB_FUNC( OS_ISWINME )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90 );
}

HB_FUNC( OS_ISWINNT351 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT
        && osvi.dwMajorVersion == 3 && osvi.dwMinorVersion == 51 );
}

HB_FUNC( OS_ISWINNT4 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( OS_ISWIN2000 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( OS_ISWINXP )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 );
}

HB_FUNC( OS_ISWIN2003 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2 );
}

HB_FUNC( OS_ISWINVISTA )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( OS_ISWIN7 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 1 );
}

HB_FUNC( OS_ISWIN8 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 2 );
}

HB_FUNC( OS_ISWIN81 )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 3 );
}

HB_FUNC( OS_ISWTSCLIENT )
{
  int iResult = FALSE;
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  if ( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion >= 4 )
  {
    // Only supported on NT Ver 4.0 SP3 & higher
    #ifndef SM_REMOTESESSION
       #define SM_REMOTESESSION        0x1000
    #endif
    iResult = GetSystemMetrics(SM_REMOTESESSION) ;
  }
  hb_retl( iResult );
}

HB_FUNC( OS_ISWIN2000_OR_LATER )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion >= 5 );
}

HB_FUNC( OS_ISWINXP_OR_LATER )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion > 5 || (osvi.dwMajorVersion==5 && osvi.dwMinorVersion >= 1) );
}
HB_FUNC( OS_ISWINVISTA_OR_LATER )
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion >= 6 );
}

HB_FUNC( OS_VERSIONINFO )
{
  OSVERSIONINFO osvi;
  PHB_ITEM pArray = hb_itemArrayNew( 5 );
  getwinver( &osvi );
  hb_arraySetNL( pArray, 1, osvi.dwMajorVersion );
  hb_arraySetNL( pArray, 2, osvi.dwMinorVersion );
  if ( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
  {
    osvi.dwBuildNumber = LOWORD( osvi.dwBuildNumber );
  }
  hb_arraySetNL( pArray, 3, osvi.dwBuildNumber  );
/*
  #define VER_PLATFORM_WIN32s             0
  #define VER_PLATFORM_WIN32_WINDOWS      1
  #define VER_PLATFORM_WIN32_NT           2
*/
  hb_arraySetNL( pArray, 4, osvi.dwPlatformId   );
  hb_arraySetC( pArray, 5, osvi.szCSDVersion   );
  hb_itemRelease( hb_itemReturn( pArray) );
}

HB_FUNC( OS_ISUSERANADMIN )  // 24/11/09 11:43
{
  BOOL iResult = FALSE ;
  typedef int (WINAPI *USERADMIN)( void );
  HINSTANCE hLib;
  USERADMIN ProcAdd;
  hLib = LoadLibrary("shell32.dll");
  if (hLib != NULL)
  {
    ProcAdd = ( USERADMIN ) GetProcAddress(hLib, "IsUserAnAdmin");
    if (NULL != ProcAdd)
    {
      iResult = (ProcAdd)() ;
    }
    FreeLibrary( hLib );
  }
  hb_retl( iResult ) ;
}
#else
HB_FUNC( OS_ISWINNT )
{
  hb_retl( 0 );
}

HB_FUNC( OS_ISWIN9X )
{
  hb_retl( 0 );
}

HB_FUNC( OS_ISWIN95 )
{
  hb_retl( 0 );
}

HB_FUNC( OS_ISWIN98 )
{
  hb_retl( 0 );
}

HB_FUNC( OS_ISWINME )
{
  hb_retl( 0 );
}

HB_FUNC( OS_ISWINNT351 )
{
  hb_retl( 0);
}

HB_FUNC( OS_ISWINNT4 )
{
  hb_retl( 0);
}

HB_FUNC( OS_ISWIN2000 )
{
  hb_retl( 0);
}

HB_FUNC( OS_ISWINXP )
{
  hb_retl( 0);
}

HB_FUNC( OS_ISWIN2003 )
{
  hb_retl( 0);
}

HB_FUNC( OS_ISWINVISTA )
{
  hb_retl( 0 );
}

HB_FUNC( OS_ISWIN7 )
{
  hb_retl( 0);
}

HB_FUNC( OS_ISWIN81 )
{
  hb_retl( 0 );
}

HB_FUNC( OS_ISWIN8 )
{
  hb_retl( 0 );
}

HB_FUNC( OS_ISWTSCLIENT )
{
  hb_retl( 0 );
}

HB_FUNC( OS_ISWIN2000_OR_LATER )
{
  hb_retl( 0);
}

HB_FUNC( OS_ISWINXP_OR_LATER )
{
  hb_retl( 0);
}
HB_FUNC( OS_ISWINVISTA_OR_LATER )
{
  hb_retl( 0);
}

HB_FUNC( OS_VERSIONINFO )
{
}

HB_FUNC( OS_ISUSERANADMIN )  // 24/11/09 11:43
{
hb_retl( 0 ) ;
}

#endif

#pragma ENDDUMP
