/*
 * $Id: disksphb.c,v 1.10 2007/08/25 17:42:53 paultucker Exp $
 */

/*
 * Harbour Project source code:
 * HB_DISKSPACE() function
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

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_BASE
#define INCL_DOSERRORS

#define HB_OS_WIN_32_USED

#include <ctype.h>

#include "hbapierr.h"
#include "hbapifs.h"

#if defined(HB_OS_BSD)
#  include <sys/param.h>
#  include <sys/mount.h>
#elif defined(HB_OS_SUNOS)
#  include <sys/statvfs.h>
#elif defined(HB_OS_UNIX)
#  if defined(__WATCOMC__)
#     include <sys/stat.h>
#  else
#     include <sys/statvfs.h>
#  endif
#endif

#ifdef HB_EXTENSION

HB_FUNC( HB_DISKSPACE )
{
   char * szPath;
   char szPathBuf[4];
   USHORT uiType = ISNUM( 2 ) ? hb_parni( 2 ) : HB_DISK_AVAIL;
   USHORT uiDrive= 0;
   double dSpace = 0.0;

   if( uiType > HB_DISK_TOTAL )
      uiType = HB_DISK_AVAIL;

   szPathBuf[ 0 ] = '@';

   if( ISCHAR( 1 ))
   {
      szPath = hb_parcx( 1 );
#ifdef OS_HAS_DRIVE_LETTER
      if( hb_parclen( 1 ) < 3 )
      {
         *szPathBuf = *szPath;
         szPath = szPathBuf;
      }
#endif
   }
   else
   {
      szPath = szPathBuf;

#ifdef OS_HAS_DRIVE_LETTER
      if( ISNUM( 1 ))
      {
         uiDrive = hb_parni( 1 );
         szPathBuf[ 0 ] = ( char ) uiDrive + 'A' - 1;
      }
#else
      szPathBuf[ 0 ] = OS_PATH_DELIMITER;
      szPathBuf[ 1 ] = '\0';
#endif
   }


#ifdef OS_HAS_DRIVE_LETTER
   szPathBuf[ 1 ] = OS_DRIVE_DELIMITER;
   szPathBuf[ 2 ] = OS_PATH_DELIMITER;
   szPathBuf[ 3 ] = '\0';
#endif

#if defined(HB_OS_DOS)

   {
      char cDrive = toupper( szPath[ 0 ] );

      if( uiDrive == 0 )
         uiDrive = szPath[ 1 ] != OS_DRIVE_DELIMITER ? 0 :
                   cDrive >= 'A' && cDrive <= 'Z' ? cDrive - 'A' + 1 : 0;

      if( uiDrive == 0 )
      {
         /* Since the code that follows can accept a '0' uiDrive,
            I am presuming that this prevents an invalid current drive
            from presenting an error condition or setting errorlevel?
            (ie, removed floppy)
         */
         USHORT uiErrorOld = hb_fsError();

         uiDrive = hb_fsCurDrv() + 1;

         hb_fsSetError( uiErrorOld );
      }

      while( TRUE )
      {
         union REGS regs;

         regs.HB_XREGS.dx = uiDrive;
         regs.h.ah = 0x36;
         HB_DOS_INT86( 0x21, &regs, &regs );

         if( regs.HB_XREGS.ax != 0xFFFF )
         {
            USHORT uiClusterTotal  = regs.HB_XREGS.dx;
            USHORT uiClusterFree   = regs.HB_XREGS.bx;
            USHORT uiSecPerCluster = regs.HB_XREGS.ax;
            USHORT uiSectorSize    = regs.HB_XREGS.cx;

            switch( uiType )
            {
               case HB_DISK_AVAIL:
               case HB_DISK_FREE:
                  dSpace = ( double ) uiClusterFree *
                           ( double ) uiSecPerCluster *
                           ( double ) uiSectorSize;
                  break;

               case HB_DISK_USED:
               case HB_DISK_TOTAL:
                  dSpace = ( double ) uiClusterTotal *
                           ( double ) uiSecPerCluster *
                           ( double ) uiSectorSize;

                  if( uiType == HB_DISK_USED )
                     dSpace -= ( double ) uiClusterFree *
                               ( double ) uiSecPerCluster *
                               ( double ) uiSectorSize;
                  break;
            }
         }
         else
         {
            if( hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, ( EF_CANDEFAULT | EF_CANRETRY ), 2, hb_paramError( 1 ), hb_paramError( 2 ) ) == E_RETRY )
               continue;
         }
         break;
      }
   }

#elif defined(HB_OS_WIN_32)

   {

      if( uiDrive == 0 )
      {
         char cDrive = toupper( szPath[ 0 ] );

         uiDrive = szPath[ 1 ] != OS_DRIVE_DELIMITER ? 0 :
                   cDrive >= 'A' && cDrive <= 'Z' ? cDrive - 'A' + 1 : 0;
      }

      /* NOTE: Use of '0' meaning 'default drive' is _very_ _strongly_
         discouraged in a modern day application.
      */

      if( uiDrive == 0 )
      {
         USHORT uiErrorOld = hb_fsError();

         uiDrive = hb_fsCurDrv() + 1;

         hb_fsSetError( uiErrorOld );
      }

      szPath = ( char * ) hb_strdup( szPath );
      szPath [ 0 ] = ( char ) uiDrive + 'A' - 1;

      while( TRUE )
      {
         typedef BOOL ( WINAPI * P_GDFSE )( LPCTSTR, PULARGE_INTEGER,
                                            PULARGE_INTEGER, PULARGE_INTEGER );

         P_GDFSE pGetDiskFreeSpaceEx;
         UINT uiErrMode;

         uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );

         SetLastError( 0 );

         pGetDiskFreeSpaceEx = ( P_GDFSE ) GetProcAddress( GetModuleHandle( "kernel32.dll" ),
                                                           "GetDiskFreeSpaceExA");

         if( pGetDiskFreeSpaceEx )
         {
            ULARGE_INTEGER i64FreeBytesToCaller,
                           i64TotalBytes,
                           i64FreeBytes,
                           i64RetVal;

            if( pGetDiskFreeSpaceEx( szPath,
                                     ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                     ( PULARGE_INTEGER ) &i64TotalBytes,
                                     ( PULARGE_INTEGER ) &i64FreeBytes ) )
            {
               switch( uiType )
               {
                  case HB_DISK_AVAIL:
                     memcpy( &i64RetVal, &i64FreeBytesToCaller, sizeof( ULARGE_INTEGER ) );
                     break;

                  case HB_DISK_FREE:
                     memcpy( &i64RetVal, &i64FreeBytes, sizeof( ULARGE_INTEGER ) );
                     break;

                  case HB_DISK_USED:
                  case HB_DISK_TOTAL:
                     memcpy( &i64RetVal, &i64TotalBytes, sizeof( ULARGE_INTEGER ) );
               }

               #if (defined(__GNUC__) || defined(_MSC_VER) || defined(__LCC__)) && !defined(__RSXNT__)

                  dSpace  = ( double ) i64RetVal.LowPart +
                            ( double ) i64RetVal.HighPart +
                            ( double ) i64RetVal.HighPart *
                            ( double ) 0xFFFFFFFF;

                  if( uiType == HB_DISK_USED )
                  {
                     dSpace -= ( double ) i64FreeBytes.LowPart +
                               ( double ) i64FreeBytes.HighPart +
                               ( double ) i64FreeBytes.HighPart *
                               ( double ) 0xFFFFFFFF;
                  }

               #else

                  /* NOTE: Borland doesn't seem to deal with the un-named
                           struct that is part of ULARGE_INTEGER
                           [pt] */

                  dSpace  = ( double ) i64RetVal.u.LowPart +
                            ( double ) i64RetVal.u.HighPart +
                            ( double ) i64RetVal.u.HighPart *
                            ( double ) 0xFFFFFFFF;

                  if( uiType == HB_DISK_USED )
                  {
                     dSpace -= ( double ) i64FreeBytes.u.LowPart +
                               ( double ) i64FreeBytes.u.HighPart +
                               ( double ) i64FreeBytes.u.HighPart *
                               ( double ) 0xFFFFFFFF;
                  }

               #endif
            }
         }
         else
         {
            DWORD dwSectorsPerCluster;
            DWORD dwBytesPerSector;
            DWORD dwNumberOfFreeClusters;
            DWORD dwTotalNumberOfClusters;

            SetLastError( 0 );

            if( GetDiskFreeSpace( szPath,
                                  &dwSectorsPerCluster,
                                  &dwBytesPerSector,
                                  &dwNumberOfFreeClusters,
                                  &dwTotalNumberOfClusters ) )
            {
               switch( uiType )
               {
                  case HB_DISK_AVAIL:
                  case HB_DISK_FREE:
                     dSpace = ( double ) dwNumberOfFreeClusters *
                              ( double ) dwSectorsPerCluster *
                              ( double ) dwBytesPerSector;
                     break;

                  case HB_DISK_USED:
                  case HB_DISK_TOTAL:
                     dSpace  = ( double ) dwTotalNumberOfClusters *
                               ( double ) dwSectorsPerCluster *
                               ( double ) dwBytesPerSector;

                     if( uiType == HB_DISK_USED )
                        dSpace -= ( double ) dwNumberOfFreeClusters *
                                  ( double ) dwSectorsPerCluster *
                                  ( double ) dwBytesPerSector;
                     break;

               }
            }
         }

         SetErrorMode( uiErrMode );

         if( GetLastError() != 0 )
         {
            if( hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, (EF_CANDEFAULT | EF_CANRETRY), 2, hb_paramError( 1 ), hb_paramError( 2 ) ) == E_RETRY )
               continue;
         }
         break;
      }
      hb_xfree( szPath );
   }

#elif defined(HB_OS_OS2)

   {
      struct _FSALLOCATE fsa;
      USHORT rc;
      char cDrive = toupper( szPath[ 0 ] );

      if( uiDrive == 0 )
         uiDrive = szPath[ 1 ] != OS_DRIVE_DELIMITER ? 0 :
                   cDrive >= 'A' && cDrive <= 'Z' ? cDrive - 'A' + 1 : 0;

      /* Query level 1 info from filesystem */
      while( ( rc = DosQueryFSInfo( uiDrive, 1, &fsa, sizeof( fsa ) ) ) != 0 )
      {
         if( hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, (EF_CANDEFAULT | EF_CANRETRY), 2, hb_paramError( 1 ), hb_paramError( 2 ) ) != E_RETRY )
            break;
      }

      if( rc == 0 )
      {
         switch( uiType )
         {
            case HB_DISK_AVAIL:
            case HB_DISK_FREE:
               dSpace = ( double ) fsa.cUnitAvail *
                        ( double ) fsa.cSectorUnit *
                        ( double ) fsa.cbSector;
               break;

            case HB_DISK_USED:
            case HB_DISK_TOTAL:
               dSpace = ( double ) fsa.cUnit *
                        ( double ) fsa.cSectorUnit *
                        ( double ) fsa.cbSector;

               if( uiType == HB_DISK_USED )
                  dSpace -= ( double ) fsa.cUnitAvail *
                            ( double ) fsa.cSectorUnit *
                            ( double ) fsa.cbSector;
               break;
         }
      }
      hb_xfree( szPath );
   }

#elif defined(HB_OS_UNIX) && !defined(__WATCOMC__)

   {
      struct statvfs sf;

      szPath = ( char * ) hb_fileNameConv( hb_strdup( szPath ) );
      if ( statvfs( szPath, &sf ) == 0 )
      {
         switch( uiType )
         {
            case HB_DISK_AVAIL:
               dSpace = ( double ) sf.f_bavail * ( double ) sf.f_bsize;
               break;

            case HB_DISK_FREE:
               dSpace = ( double ) sf.f_bfree * ( double ) sf.f_bsize;
               break;

            case HB_DISK_USED:
                dSpace = ( double ) ( sf.f_blocks - sf.f_bfree ) *
                         ( double ) sf.f_bsize;
                break;

            case HB_DISK_TOTAL:
               dSpace = ( double ) sf.f_blocks * ( double ) sf.f_bsize;
               break;
         }
      }
      hb_xfree( szPath );
   }


#else

   {
      HB_SYMBOL_UNUSED( szPath );
      HB_SYMBOL_UNUSED( uiType );
   }

#endif

   hb_retnlen( dSpace, -1, 0 );
}

#endif
