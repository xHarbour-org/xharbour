/*
 * $Id: disk.c,v 1.5 2005/03/15 04:05:43 andijahja Exp $
 */
/*
 * xHarbour Project source code:
 * CT (Clipper Tools) Disk, File and Directory management.
 *
 * Copyright 2004-2005 Eduardo Fernandes <modalsist@yahoo.com.br>
 *
 * DeleteFile()  - Ready. Source is in "disk.c"
 * DirMake()     - Ready. Already exist a MakeDir() function in xHarbour RTL Lib,
 *                        but DirMake returns a more compatible error codes.
 * DirName()     - Ready.
 * DriveType()   - Ready.  corrected <ptucker@sympatico.ca>
 * FileMove()    - Ready.
 * Volume()      - Ready.
 * GetVolInfo()  - Ready.  This function is new.
 *
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
 * NUMDISKL()
 *
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or ( at your option )
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
 * Boston, MA 02111-1307 USA ( or visit the web site http://www.gnu.org/ ).
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

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapifs.h"

#if defined(HB_OS_WIN_32)

#include <windows.h>
#include <winbase.h>
#include <shellapi.h>

#define HB_OS_WIN_32_USED

#elif defined(HB_OS_DOS)

#include <dos.h>

#endif

HB_FUNC ( DELETEFILE )
{

   BYTE * pFileName  = ( BYTE *) hb_parcx( 1 ) ;
   int iRet;

   if ( hb_fsDelete( (BYTE*) pFileName) )
   {
      hb_retni ( 0 );
   }
   else
   {
      iRet = (int) hb_fsOsError();
      hb_retni ( iRet * (-1) );
   }
}

HB_FUNC ( DIRMAKE )
{

   BYTE * pFileName  = ( BYTE *) hb_parcx( 1 ) ;
   int iRet;

   if ( hb_fsMkDir( pFileName ) )
   {
      hb_retni ( 0 );
   }
   else
   {
      iRet = (int) hb_fsOsError();
      hb_retni ( iRet * (-1) );
   }
}

HB_FUNC ( DIRNAME )
{
   USHORT uiErrorOld = hb_fsError();
   BYTE * pbyBuffer = ( BYTE * ) hb_xgrab( _POSIX_PATH_MAX + 1 );

   hb_fsCurDirBuff( hb_fsCurDrv(), pbyBuffer + 1, _POSIX_PATH_MAX  );
   pbyBuffer[0] = OS_PATH_DELIMITER;

   hb_retcAdopt( ( char * ) pbyBuffer );

   hb_fsSetError( uiErrorOld );
}


HB_FUNC ( DRIVETYPE )
{
   #if defined(HB_OS_WIN_32)
      unsigned int uiType;
      char * pDrive = (char *) hb_xgrab( hb_parclen( 1 )+3 ); // allow space for '\0' & ":\"
      strcpy( pDrive, (char *) hb_parcx(1) );

      if ( strstr( pDrive, ":" ) == NULL )
      {
         strcat( pDrive, ":" ) ;
      }

      if ( strstr( pDrive, "\\" ) == NULL )
      {
         strcat( pDrive, "\\" ) ;
      }

      uiType = GetDriveType( pDrive );

      if ( uiType  == DRIVE_RAMDISK )
      {
         hb_retni( 0 );  // RAM Drive - Clipper compatible
      }
      else if (uiType == DRIVE_REMOVABLE )
      {
         hb_retni( 2 );  // Floppy Drive - Clipper compatible
      }
      else if (uiType == DRIVE_FIXED )
      {
         hb_retni( 3 );  // Hard Drive  - Clipper compatible
      }
      else if (uiType == DRIVE_CDROM )
      {
         hb_retni( 4 );  // CD-Rom Drive - xHarbour extension
      }
      else if (uiType == DRIVE_REMOTE )
      {
         hb_retni( 5 );  // Network Drive - xHarbour extension
      }
      else
      {
         hb_retni( 9 );  // Unknow Drive - xHarbour extension
      }
      hb_xfree( pDrive );
   #else
      hb_retni(9);
   #endif

}

HB_FUNC ( FILEMOVE )
{
   BYTE * pSourceFile = ( BYTE *) hb_parcx( 1 );
   BYTE *  pTargetFile = ( BYTE *) hb_parcx( 2 );
   int iRet;

   if (hb_fsRename( pSourceFile , pTargetFile ) )
   {
      hb_retni ( 0 );
   }
   else
   {
      iRet = (int) hb_fsOsError();
      hb_retni ( iRet * (-1) );
   }
}


HB_FUNC( NUMDISKL )
#if defined( HB_OS_DOS )
#if defined( __DJGPP__ )
{
   unsigned cur_drive, n_drives;

   _dos_getdrive( &cur_drive );
   _dos_setdrive( cur_drive, &n_drives );
   hb_retni( n_drives );
}
#else
{
   /* should be easily implementable somehow similar to DJGPP */
   hb_retni( 26 );
}
#endif
#elif defined( HB_OS_WIN_32 )
{
   /* LASTDRIVE does not affect Win32 apps, they always have 26 letters avail */
   hb_retni( 26 );
}
#else
{
   /* For Unix, return the most harmless value... or not? */
   hb_retni( 1 );
}
#endif



/*
Volume() depends of the CSETSAFETY() setting and, if is true, does not overwrite an 
existing label. 
Syntax is: Volume("x:test") or Volume("x:\test"), where "x" is the
any drive letter and "test" will be the new volume name. 
Note:
1) if the drive letter is not suplied, then the current drive will 
   be used to change voloume name.
2) if Volume("x:") or Volume("x:\") then the volume name of the drive
   "x:" will be erased.
3) if Volume("") or Volume() then the volume name of the current drive 
   will be erased.

*/

extern BOOL ct_getsafety( void );  // this function is in ct/strfile.c

HB_FUNC( VOLUME )
{
   BOOL bReturn = FALSE;

if ( !ct_getsafety() ) 
{                                             
   PHB_FNAME fname;
   BYTE * sDiskName;
   char * sRoot=NULL;
   char * sVolName=NULL;
   
   if( ISCHAR(1) && hb_parclen(1) > 0 )
   {
     sDiskName = hb_fileNameConv( hb_strdup( hb_parcx(1) ) );

     if( ( fname = hb_fsFNameSplit( (char*) sDiskName ) ) != NULL )
     {
        if( fname->szPath )
        {
         sRoot = ""; 
         strncat( sRoot, fname->szPath, 2 ); // truncate in 2 positions: trailing backslash isn't needed.
         strcat( sRoot , "\0" );
        }
        if( fname->szName )
        {   
         sVolName="           ";
         strncpy( sVolName, fname->szName, 11 );
         strcat( sVolName, "\0" );
        }     

        hb_xfree( fname );
     }
     else
     {
       sVolName="           ";
       strncpy( sVolName, (char *) sDiskName, 11 );
       strcat( sVolName, "\0" );
     }
   }
#if defined(HB_OS_WIN_32)
   bReturn = SetVolumeLabel( sRoot, sVolName );
#endif
}  
   hb_retl( bReturn );
}

/*
 GetVolInfo() is a new function. It returns the volume name of a Floppy, CD, 
 Hard-disk or network drive.
 Sintax is: GetVolInfo("x:\")
 Note that the trailing backslash is required.
 In the near future, this function will be increased to return serial number also.
*/
HB_FUNC( GETVOLINFO )
{
    int retval=0;
    char * sDrive="";
    char * sVolName[255];

    strncat( sDrive, (char*) hb_parcx(1), 3 );
    strcat( sDrive, "\0" );

#if defined(HB_OS_WIN_32)

    retval = GetVolumeInformation( sDrive,
                                   (char*) &sVolName,
                                   256,
                                   NULL,
                                   NULL,
                                   NULL,
                                   NULL,
                                   0 );
#endif
    if ( retval!=0 )
      hb_retc( (char *) sVolName );
    else  
      hb_retc("");

}

