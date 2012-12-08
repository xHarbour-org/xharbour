/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DIRCHANGE(), MAKEDIR(), DIRREMOVE(), ISDISK(), DISKCHANGE(), DISKNAME() functions
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

/* NOTE: Clipper 5.3 functions */

#include <ctype.h>

#include "hbapi.h"
#include "hbapifs.h"

#if defined( HB_OS_WIN )
#include "windows.h"

#define DRIVE_UNKNOWN     0
#define DRIVE_NO_ROOT_DIR 1
#define DRIVE_REMOVABLE   2
#define DRIVE_FIXED       3
#define DRIVE_REMOTE      4
#define DRIVE_CDROM       5
#define DRIVE_RAMDISK     6

static char* szGetRootName( PHB_ITEM pRootName )
{
   if( *pRootName->item.asString.value && pRootName->item.asString.length < 3 )
   {
      static char szRoot[ 4 ];

      szRoot[ 0 ] = pRootName->item.asString.value[ 0 ];
      szRoot[ 1 ] = ':';
      szRoot[ 2 ] = '\\';
      szRoot[ 3 ] = '\0';

      return szRoot;
   }

   return pRootName->item.asString.value;
}

HB_FUNC( HB_ISREMOTEDISK )
{
   PHB_ITEM pRootName = hb_param( 1, HB_IT_STRING );

   if( pRootName )
   {
      hb_retl( GetDriveType( szGetRootName( pRootName ) ) == DRIVE_REMOTE );
      return;
   }

   hb_retl( GetDriveType( NULL ) == DRIVE_REMOTE );
}

HB_FUNC( HB_ISRAMDISK )
{
   PHB_ITEM pRootName = hb_param( 1, HB_IT_STRING );

   if( pRootName )
   {
      hb_retl( GetDriveType( szGetRootName( pRootName ) ) == DRIVE_RAMDISK );
      return;
   }

   hb_retl( GetDriveType( NULL ) == DRIVE_RAMDISK );
}

HB_FUNC( HB_ISHARDDISK )
{
   PHB_ITEM pRootName = hb_param( 1, HB_IT_STRING );

   if( pRootName )
   {
      hb_retl( GetDriveType( szGetRootName( pRootName ) ) == DRIVE_FIXED );
      return;
   }

   hb_retl( GetDriveType( NULL ) == DRIVE_FIXED );
}

HB_FUNC( HB_ISCDROM )
{
   PHB_ITEM pRootName = hb_param( 1, HB_IT_STRING );

   if( pRootName )
   {
      hb_retl( GetDriveType( szGetRootName( pRootName ) ) == DRIVE_CDROM );
      return;
   }

   hb_retl( GetDriveType( NULL ) == DRIVE_CDROM );
}

HB_FUNC( HB_ISDISKETTE )
{
   PHB_ITEM pRootName = hb_param( 1, HB_IT_STRING );

   if( pRootName )
   {
      hb_retl( GetDriveType( szGetRootName( pRootName ) ) == DRIVE_REMOVABLE );
      return;
   }

   hb_retl( GetDriveType( NULL ) == DRIVE_REMOVABLE );
}

HB_FUNC( HB_DISKREADY )
{
   PHB_ITEM pRootName = hb_param( 1, HB_IT_STRING );

   if ( pRootName )
   {
      char *szRoot = szGetRootName( pRootName );

      if ( GetDriveType( szRoot ) == DRIVE_REMOVABLE  )
      {
         char   szTmp[ 64 ];
         FILE * fHandle;

         hb_snprintf( szTmp, sizeof( szTmp ), "%s%s", szRoot, "___tmp.tmp" );

         fHandle = hb_fopen( szTmp, "w" );

         if( fHandle )
         {
            fclose( fHandle );
            remove( szTmp );
            hb_retl( TRUE );
            return;
         }
      }
   }

   hb_retl( FALSE );
}

HB_FUNC( HB_DISKREADYW )
{
   HB_FUNC_EXEC( HB_DISKREADY );
}
#endif

#ifdef HB_COMPAT_C53

HB_FUNC( DIRCHANGE )
{
   USHORT uiErrorOld = hb_fsError();

   if( ISCHAR( 1 ) )
      hb_retni( hb_fsChDir( hb_parc( 1 ) ) ? 0 : hb_fsError() );
   else
      hb_retni( -1 );

   hb_fsSetError( uiErrorOld );
}

/* NOTE: Clipper 5.3 NG incorrectly states that the name of this function is
         DIRMAKE(), in reality it's not. */

HB_FUNC( MAKEDIR )
{
   USHORT uiErrorOld = hb_fsError();

   if( ISCHAR( 1 ) )
      hb_retni( hb_fsMkDir( hb_parcx( 1 ) ) ? 0 : hb_fsError() );
   else
      hb_retni( -1 );

   hb_fsSetError( uiErrorOld );
}

HB_FUNC( DIRREMOVE )
{
   USHORT uiErrorOld = hb_fsError();

   if( ISCHAR( 1 ) )
      hb_retni( hb_fsRmDir( hb_parcx( 1 ) ) ? 0 : hb_fsError() );
   else
      hb_retni( -1 );

   hb_fsSetError( uiErrorOld );
}

/* NOTE: Clipper 5.3 undocumented */

HB_FUNC( ISDISK )
{
   USHORT         uiErrorOld  = hb_fsError();

   BOOL           fResult     = FALSE;
   const char *   szDrive     = hb_parc( 1 );

   if( szDrive )
   {
      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         fResult = hb_fsIsDrv( *szDrive - 'A' ) == 0;
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         fResult = hb_fsIsDrv( *szDrive - 'a' ) == 0;
   }
   hb_retl( fResult );

   hb_fsSetError( uiErrorOld );
}

HB_FUNC( DISKCHANGE )
{
   USHORT         uiErrorOld  = hb_fsError();

   BOOL           fResult     = FALSE;
   const char *   szDrive     = hb_parc( 1 );

   if( szDrive )
   {
      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         fResult = hb_fsChDrv( *szDrive - 'A' ) == 0;
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         fResult = hb_fsChDrv( *szDrive - 'a' ) == 0;
   }
   hb_retl( fResult );

   hb_fsSetError( uiErrorOld );
}

HB_FUNC( DISKNAME )
{
#if defined( HB_OS_HAS_DRIVE_LETTER )
   USHORT   uiErrorOld = hb_fsError();
   char     szDrive[ 1 ];

   szDrive[ 0 ] = ( ( char ) hb_fsCurDrv() ) + 'A';
   hb_retclen( szDrive, 1 );

   hb_fsSetError( uiErrorOld );
#else
   hb_retc_null();
#endif
}

#endif

