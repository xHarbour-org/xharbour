/*
 * $Id$
 */

/*
 * CABINET SDK Project source code:
 *
 * Copyright 2012 Andi Jahja <xharbour@telkom.net.id>
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
 */

#define __DECOMPRESS_C
#include "cabinet.h"

static char dest_dir[ 256 ];
static char * _decompressCab( char * cabinet_fullpath, void * vCallBack, BOOL bExtract );

/* Retrieve CAB file info */
HB_FUNC( HB_CABINFO ) //  ( cCabFile )
{
   if( ISCHAR( 1 ) )
   {
      _decompressCab( ( char * ) hb_parc( 1 ), NULL, FALSE );
   }
}

/* Extract CAB file to a destination folder */
HB_FUNC( HB_DECOMPRESSCAB ) //  ( cCabFile, cDestDir )
{
   dest_dir[ 0 ] = 0;

   if( ISCHAR( 1 ) )
   {
      void *   vCallBack         = NULL;
      char *   cabinet_fullpath  = NULL;

      cabinet_fullpath = ( char * ) hb_parc( 1 );  /* CAB to decompress */

      if( ISCHAR( 2 ) )
      {
         int i = ( int ) hb_parclen( 2 );

         if( i )
         {
            strcpy( dest_dir, hb_parc( 2 ) );            /* destination dir */
            if( ! ( dest_dir[ i ] == '\\' ) )
            {
               dest_dir[ i++ ]   = '\\';
               dest_dir[ i ]     = '\0';
            }
         }
      }

      if( dest_dir[ 0 ] == 0 )
      {
         BYTE        bBufferEXE[ 250 ];
         PHB_FNAME   pFileName;
         GetModuleFileName( NULL, ( char * ) bBufferEXE, 249 );
         pFileName = hb_fsFNameSplit( ( const char * ) bBufferEXE );
         hb_snprintf( dest_dir, sizeof( dest_dir ), "%s", pFileName->szPath );
         hb_xfree( pFileName );
      }

      /* User Callback Function */
      if( ISBLOCK( 3 ) )
         vCallBack = ( void * ) hb_param( 3, HB_IT_BLOCK );

      hb_retc( _decompressCab( cabinet_fullpath, vCallBack, TRUE ) );
      return;
   }

   hb_retc( "Parameter error" );
   return;
}

static char * _decompressCab( char * cabinet_fullpath, void * vCallBack, BOOL bExtract )
{
   static char    errMsg[ 256 ];
   HFDI           hfdi;
   ERF            erf;
   FDICABINETINFO fdici;
   int            hf;
   char *         p;
   char           cabinet_name[ 256 ];
   char           cabinet_path[ 256 ];
   int            fdiflag     = 0;
   PFNFDIDECRYPT  fnDecrypt   = NULL;

   hfdi = hb_FDICreate(
      mem_alloc,
      mem_free,
      file_open,
      file_read,
      file_write,
      file_close,
      file_seek,
      cpu80386,
      &erf
      );

   if( ! hfdi )
   {
      sprintf( errMsg, "FDICreate() failed: code %d [%s]\n",
               erf.erfOper, return_fdi_error_string( erf.erfOper )
               );

      if( ! bExtract )
         hb_reta( 0 );

      return errMsg;
   }

   /*
    * Is this file really a cabinet?
    */
   hf = ( int ) file_open(
      cabinet_fullpath,
      _O_BINARY | _O_RDONLY | _O_SEQUENTIAL,
      0
      );

   if( hf == -1 )
   {
      hb_FDIDestroy( hfdi );
      sprintf( errMsg, "Unable to open '%s' for input\n", cabinet_fullpath );
      if( ! bExtract )
         hb_reta( 0 );
      return errMsg;
   }

   if( ! hb_FDIIsCabinet(
          hfdi,
          hf,
          &fdici ) )
   {
      _close( hf );

      sprintf( errMsg,
               "FDIIsCabinet() failed: '%s' is not a cabinet\n",
               cabinet_fullpath
               );

      hb_FDIDestroy( hfdi );
      if( ! bExtract )
         hb_reta( 0 );
      return errMsg;
   }
   else
   {
      _close( hf );

      if( ! bExtract )
      {
         hb_reta( 9 );
         HB_STORC( cabinet_fullpath, -1, 1 );
         HB_STORNI( fdici.cbCabinet, -1, 2 );
         HB_STORNI( fdici.cFolders, -1, 3 );
         HB_STORNI( fdici.cFiles, -1, 4 );
         HB_STORNI( fdici.setID, -1, 5 );
         HB_STORNI( fdici.iCabinet, -1, 6 );
         HB_STORNI( fdici.fReserve, -1, 7 );
         HB_STORNI( fdici.hasprev, -1, 8 );
         HB_STORNI( fdici.hasnext, -1, 9 );
         hb_FDIDestroy( hfdi );
         return "OK";
      }
   }

   p = strrchr( cabinet_fullpath, '\\' );

   if( ! p )
   {
      strcpy( cabinet_name, cabinet_fullpath );
      strcpy( cabinet_path, "" );
   }
   else
   {
      strcpy( cabinet_name, p + 1 );
      strncpy( cabinet_path, cabinet_fullpath, ( int ) ( p - cabinet_fullpath ) + 1 );
      cabinet_path[ ( int ) ( p - cabinet_fullpath ) + 1 ] = 0;
   }

   if( ! hb_FDICopy(
          hfdi,
          cabinet_name,
          cabinet_path,
          fdiflag,
          notification_function,
          fnDecrypt,
          vCallBack ) )
   {
      sprintf( errMsg,
               "FDICopy() failed: code %d [%s]\n",
               erf.erfOper, return_fdi_error_string( erf.erfOper )
               );
      hb_FDIDestroy( hfdi );
      return errMsg;
   }

   if( ! hb_FDIDestroy( hfdi ) )
   {
      sprintf( errMsg,
               "hb_FDIDestroy() failed: code %d [%s]\n",
               erf.erfOper, return_fdi_error_string( erf.erfOper )
               );
      return errMsg;
   }

   return "OK";
}

static void * mem_alloc( ULONG cb )
{
   return malloc( cb );
}

static void __cdecl mem_free( void * pv )
{
   free( pv );
}

static INT_PTR file_open( LPSTR pszFile, int oflag, int pmode )
{
   return _OPEN( pszFile, oflag, pmode );
}

static UINT file_read( INT_PTR hf, void * pv, UINT cb )
{
   return _read( ( int ) hf, pv, cb );
}

static UINT file_write( INT_PTR hf, void * pv, UINT cb )
{
   return _write( ( int ) hf, pv, cb );
}

static int file_close( INT_PTR hf )
{
   return _close( ( int ) hf );
}

static long file_seek( INT_PTR hf, long dist, int seektype )
{
   return _lseek( ( int ) hf, dist, seektype );
}

static INT_PTR notification_function( FDINOTIFICATIONTYPE fdint, PFDINOTIFICATION pfdin )
{
   PHB_ITEM bBlock = ( PHB_ITEM ) pfdin->pv;

   if( bBlock && hb_itemType( bBlock ) == HB_IT_BLOCK )
   {
      HB_PUSHEVALSYM();
      hb_vmPush( bBlock );
      hb_vmPushLong( fdint );
      hb_vmPushString( pfdin->psz1, strlen( pfdin->psz1 ) );
      hb_vmPushString( pfdin->psz2, strlen( pfdin->psz2 ) );
      hb_vmPushString( pfdin->psz3, strlen( pfdin->psz3 ) );
      hb_vmPushLong( pfdin->setID );
      hb_vmPushLong( pfdin->iCabinet );
      hb_vmPushLong( pfdin->cb );
      hb_vmSend( 7 );
   }

   /* passed values to PRG callback function */
   switch( fdint )
   {
      case fdintCABINET_INFO:
      case fdintPARTIAL_FILE:
      case fdintNEXT_CABINET:
      case fdintENUMERATE:
         break;

      case fdintCOPY_FILE:
      {
         int   handle;
         char  destination[ 256 ];

         sprintf(
            destination,
            "%s%s",
            dest_dir,
            pfdin->psz1
            );

         handle = ( int ) file_open(
            destination,
            _O_BINARY | _O_CREAT | _O_WRONLY | _O_SEQUENTIAL,
            _S_IREAD | _S_IWRITE
            );

         return handle;
      }

      case fdintCLOSE_FILE_INFO:
      {
         HANDLE   handle;
         DWORD    attrs;
         char     destination[ 256 ];

         sprintf( destination, "%s%s", dest_dir, pfdin->psz1 );

         file_close( pfdin->hf );

         /*
          * Set date/time
          *
          * Need Win32 type handle for to set date/time
          */
         handle = CreateFile(
            destination,
            GENERIC_READ | GENERIC_WRITE,
            FILE_SHARE_READ,
            NULL,
            OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL,
            NULL
            );

         if( handle != INVALID_HANDLE_VALUE )
         {
            FILETIME datetime;

            if( TRUE == DosDateTimeToFileTime( pfdin->date, pfdin->time, &datetime ) )
            {
               FILETIME local_filetime;

               if( TRUE == LocalFileTimeToFileTime( &datetime, &local_filetime ) )
               {
                  SetFileTime( handle, &local_filetime, NULL, &local_filetime );
               }
            }

            CloseHandle( handle );
         }

         /*
          * Mask out attribute bits other than readonly,
          * hidden, system, and archive, since the other
          * attribute bits are reserved for use by
          * the cabinet format.
          */
         attrs = pfdin->attribs;

         attrs &= ( _A_RDONLY | _A_HIDDEN | _A_SYSTEM | _A_ARCH );

         SetFileAttributes( destination, attrs );

         return 1;
      }
   }

   return 0;
}

static char * return_fdi_error_string( FDIERROR err )
{
   switch( err )
   {
      case FDIERROR_NONE:
         return "No error";

      case FDIERROR_CABINET_NOT_FOUND:
         return "Cabinet not found";

      case FDIERROR_NOT_A_CABINET:
         return "Not a cabinet";

      case FDIERROR_UNKNOWN_CABINET_VERSION:
         return "Unknown cabinet version";

      case FDIERROR_CORRUPT_CABINET:
         return "Corrupt cabinet";

      case FDIERROR_ALLOC_FAIL:
         return "Memory allocation failed";

      case FDIERROR_BAD_COMPR_TYPE:
         return "Unknown compression type";

      case FDIERROR_MDI_FAIL:
         return "Failure decompressing data";

      case FDIERROR_TARGET_FILE:
         return "Failure writing to target file";

      case FDIERROR_RESERVE_MISMATCH:
         return "Cabinets in set have different RESERVE sizes";

      case FDIERROR_WRONG_CABINET:
         return "Cabinet returned on fdintNEXT_CABINET is incorrect";

      case FDIERROR_USER_ABORT:
         return "User aborted";

      default:
         return "Unknown error";
   }
}
