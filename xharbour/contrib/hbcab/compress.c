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

#define __COMPRESS_C
#include "cabinet.h"

HB_FUNC( HB_CREATECAB )
{
   static char errMsg[ 256 ];
   PHB_ITEM    paFile = hb_param( 1, HB_IT_ARRAY );
   HFCI        hfci;
   ERF         erf;
   CCAB        cab_parms;
   ULONG       uL, uArrayLen;
   char *      szFileName;
   char        szDestinationDir[ 256 ];
   int         u;
   ULONG       uSize       = MEDIA_SIZE;
   ULONG       uTreshHold  = FOLDER_THRESHOLD;
   void *      vCallBack   = NULL;
   USHORT      iCabID      = 12345;
   char *      szCabStrID  = "MyCabFile";

   if( ! paFile )
   {
      hb_retc( "Parameter error" );
      return;
   }

   iFile          = 1;
   szMaskFileName = NULL;

   /* Mask Name -> TEST become TEST1.CAB, TEST2.CAB */
   if( ISCHAR( 2 ) )
      szMaskFileName = hb_parc( 2 );

   /* Destination Directory */
   if( ISCHAR( 3 ) )
   {
      int i = ( int ) hb_parclen( 3 );
      strcpy( szDestinationDir, hb_parc( 3 ) );

      if( ! ( szDestinationDir[ i ] == '\\' ) )
      {
         szDestinationDir[ i++ ] = '\\';
         szDestinationDir[ i ]   = '\0';
      }
   }
   else
   {
      BYTE        bBufferEXE[ 250 ];
      PHB_FNAME   pFileName;
      GetModuleFileName( NULL, ( char * ) bBufferEXE, 249 );
      pFileName = hb_fsFNameSplit( ( const char * ) bBufferEXE );
      hb_snprintf( szDestinationDir, sizeof( szDestinationDir ), "%s", pFileName->szPath );
      hb_xfree( pFileName );
   }

   /* CAB size per file */
   if( ISNUM( 4 ) )
      uSize = hb_parnl( 4 );

   /* Threshhold to Flush */
   if( ISNUM( 5 ) )
      uTreshHold = hb_parnl( 5 );

   /* User Callback Function */
   if( ISBLOCK( 6 ) )
      vCallBack = ( void * ) hb_param( 6, HB_IT_BLOCK );

   /* CAB Id */
   if( ISNUM( 7 ) )
      iCabID = ( USHORT ) hb_parnl( 7 );

   /* CAB String */
   if( ISCHAR( 8 ) )
      szCabStrID = ( char * ) hb_parc( 8 );

   uArrayLen = ( ULONG ) hb_arrayLen( paFile );

   set_cab_parameters( &cab_parms, szDestinationDir, uSize, uTreshHold, iCabID, szCabStrID );

   hfci = hb_FCICreate(
      &erf,
      file_placed,
      mem_alloc,
      mem_free,
      fci_open,
      fci_read,
      fci_write,
      fci_close,
      fci_seek,
      fci_delete,
      get_temp_file,
      &cab_parms,
      vCallBack
      );

   if( ! hfci )
   {
      sprintf( errMsg, "hb_FCICreate() failed: code %d [%s]\n",
               erf.erfOper, return_fci_error_string( erf.erfOper )
               );

      hb_retc( errMsg );
      return;
   }

   for( uL = 1; uL <= uArrayLen; uL++ )
   {
      char stripped_name[ 256 ];

      szFileName = hb_arrayGetCPtr( paFile, uL );
      strip_path( szFileName, stripped_name );

      if( ! hb_FCIAddFile(
             hfci,
             szFileName,         /* file to add */
             stripped_name,      /* file name in cabinet file */
             FALSE,              /* file is not executable */
             get_next_cabinet,
             progress,
             get_open_info,
             COMPRESSION_TYPE ) )
      {
         sprintf( errMsg, "hb_FCIAddFile() failed: code %d [%s]\n",
                  erf.erfOper, return_fci_error_string( erf.erfOper )
                  );

         hb_FCIDestroy( hfci );

         hb_retc( errMsg );
         return;
      }
   }

   if( ! hb_FCIFlushCabinet(
          hfci,
          0,
          get_next_cabinet,
          progress ) )
   {
      sprintf( errMsg, "hb_FCIFlushCabinet() failed: code %d [%s]\n",
               erf.erfOper, return_fci_error_string( erf.erfOper )
               );

      hb_FCIDestroy( hfci );

      hb_retc( errMsg );
      return;
   }

   if( hb_FCIDestroy( hfci ) != 1 )
   {
      sprintf( errMsg, "hb_FCIDestroy() failed: code %d [%s]\n",
               erf.erfOper, return_fci_error_string( erf.erfOper )
               );

      hb_retc( errMsg );
      return;
   }

   for( u = 1; u <= iFile; u++ )
   {
      char szResult[ 256 ];

      sprintf( szResult, "%s%s%d.CAB", szDestinationDir, szMaskFileName, u );
      SetFileAttributes( szResult, GetFileAttributes( szResult ) & ~FILE_ATTRIBUTE_READONLY );
   }

   hb_retc( "OK" );
}

static void strip_path( char * filename, char * stripped_name )
{
   char * p;

   p = strrchr( filename, '\\' );

   if( p == ( ( void * ) 0 ) )
      strcpy( stripped_name, filename );
   else
      strcpy( stripped_name, p + 1 );
}

static char * return_fci_error_string( FCIERROR err )
{
   switch( err )
   {
      case FCIERR_NONE:
         return "No error";

      case FCIERR_OPEN_SRC:
         return "Failure opening file to be stored in cabinet";

      case FCIERR_READ_SRC:
         return "Failure reading file to be stored in cabinet";

      case FCIERR_ALLOC_FAIL:
         return "Insufficient memory in FCI";

      case FCIERR_TEMP_FILE:
         return "Could not create a temporary file";

      case FCIERR_BAD_COMPR_TYPE:
         return "Unknown compression type";

      case FCIERR_CAB_FILE:
         return "Could not create cabinet file";

      case FCIERR_USER_ABORT:
         return "Client requested abort";

      case FCIERR_MCI_FAIL:
         return "Failure compressing data";

      default:
         return "Unknown error";
   }
}

static void * mem_alloc( ULONG cb )
{
   return malloc( cb );
}

static void mem_free( void * memory )
{
   free( memory );
}

static INT_PTR fci_open( LPSTR pszFile, int oflag, int pmode, int * err, void * pv )
{
   int result;

   (void) pv;
   result = _OPEN( pszFile, oflag, pmode );

   if( result == -1 )
      *err = ERROR_NO;

   return result;
}

static UINT fci_read( INT_PTR hf, void * memory, UINT cb, int * err, void * pv )
{
   unsigned int result;

   (void) pv;
   result = ( unsigned int ) _read( (int) hf, memory, cb );

   if( result != cb )
      *err = ERROR_NO;

   return result;
}

static UINT fci_write( INT_PTR hf, void * memory, UINT cb, int * err, void * pv )
{
   unsigned int result;

   (void) pv;
   result = ( unsigned int ) _write( ( int ) hf, memory, cb );

   if( result != cb )
      *err = ERROR_NO;

   return result;
}

static int fci_close( INT_PTR hf, int * err, void * pv )
{
   int result;

   (void) pv;
   result = _close( ( int ) hf );

   if( result != 0 )
      *err = ERROR_NO;

   return result;
}

static long fci_seek( INT_PTR hf, long dist, int seektype, int * err, void * pv )
{
   long result;

   (void) pv;
   result = _lseek( ( int ) hf, dist, seektype );

   if( result == -1 )
      *err = ERROR_NO;

   return result;
}

static int fci_delete( LPSTR pszFile, int * err, void * pv )
{
   int result;

   (void) pv;
   SetFileAttributes( pszFile, FA_ARCH );
   result = hb_fsDelete( pszFile );

   if( result != 0 )
      *err = ERROR_NO;

   return result;
}

static int file_placed( PCCAB pccab, char * pszFile, long cbFile, BOOL fContinuation, void * pv )
{
   PHB_ITEM bBlock = ( PHB_ITEM ) pv;

   if( bBlock && ( hb_itemType( bBlock ) == HB_IT_BLOCK ) )
   {
      HB_PUSHEVALSYM();
      hb_vmPush( bBlock );
      hb_vmPushString( pccab->szCab, strlen( pccab->szCab ) );
      hb_vmPushString( pszFile, strlen( pszFile ) );
      hb_vmPushLong( cbFile );
      hb_vmPushLogical( fContinuation );
      hb_vmPushLong( 1 );
      hb_vmSend( 5 );
   }

   return 0;
}

static BOOL get_temp_file( char * pszTempName, int cbTempName, void * pv )
{
   char * psz = TEMPNAME();            // Get a name

   (void) pv;
   if( ( psz != NULL ) && ( strlen( psz ) < ( unsigned ) cbTempName ) )
   {
      strcpy( pszTempName, psz );      // Copy to caller's buffer
      free( psz );                     // Free temporary name buffer
      return TRUE;                     // Success
   }
   //** Failed
   if( psz )
   {
      free( psz );
   }

   return FALSE;
}

static long progress( UINT typeStatus, ULONG cb1, ULONG cb2, void * pv )
{
   PHB_ITEM bBlock = ( PHB_ITEM ) pv;

   if( bBlock && ( hb_itemType( bBlock ) == HB_IT_BLOCK ) )
   {
      HB_PUSHEVALSYM();
      hb_vmPush( bBlock );
      hb_vmPushLong( typeStatus );
      hb_vmPushLong( cb1        );
      hb_vmPushLong( cb2        );
      hb_vmPushLogical(  FALSE  );
      hb_vmPushLong( 2          );
      hb_vmSend( 5 );
   }

   return 0;
}

static void store_cab_name( char * cabname, int iCab )
{
   if( szMaskFileName )
      sprintf( cabname, "%s%d.CAB", szMaskFileName, iCab );
   else
      sprintf( cabname, "TEST%d.CAB", iCab );
}

static BOOL get_next_cabinet( PCCAB pccab, ULONG cbPrevCab, void * pv )
{
   (void) pv;
   (void) cbPrevCab;
   iFile++;
   store_cab_name( pccab->szCab, pccab->iCab );
   return 1;
}

static INT_PTR get_open_info( char * pszName, USHORT * pdate, USHORT * ptime, USHORT * pattribs, int * err, void * pv )
{
   BY_HANDLE_FILE_INFORMATION finfo;
   FILETIME                   filetime;
   HANDLE                     handle;
   DWORD                      attrs;
   int                        hf;
   ( void ) err;
   ( void ) pv;
   handle = CreateFile(
      pszName,
      GENERIC_READ,
      FILE_SHARE_READ,
      NULL,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN,
      NULL
      );

   if( handle == INVALID_HANDLE_VALUE )
   {
      return -1;
   }

   if( GetFileInformationByHandle( handle, &finfo ) == FALSE )
   {
      CloseHandle( handle );
      return -1;
   }

   FileTimeToLocalFileTime(
      &finfo.ftLastWriteTime,
      &filetime
      );

   FileTimeToDosDateTime(
      &filetime,
      pdate,
      ptime
      );

   attrs = GetFileAttributes( pszName );

   if( attrs == 0xFFFFFFFF )
   {
      *pattribs = 0;
   }
   else
   {
      /*
       * Mask out all other bits except these four, since other
       * bits are used by the cabinet format to indicate a
       * special meaning.
       */
      *pattribs = ( int ) ( attrs & ( _A_RDONLY | _A_SYSTEM | _A_HIDDEN | _A_ARCH ) );
   }

   CloseHandle( handle );

   hf = _open( pszName, _O_RDONLY | _O_BINARY );

   if( hf == -1 )
      return -1;

   return hf;
}

static void set_cab_parameters(
   PCCAB cab_parms,
   char * szDestinationDir,
   ULONG uSize,
   ULONG uTreshHold,
   USHORT setCabID,
   char * szCabDisk )
{
   memset( cab_parms, 0, sizeof( CCAB ) );

   cab_parms->cb                 = uSize;
   cab_parms->cbFolderThresh     = uTreshHold;

   /*
    * Don't reserve space for any extensions
    */
   cab_parms->cbReserveCFHeader  = 0;
   cab_parms->cbReserveCFFolder  = 0;
   cab_parms->cbReserveCFData    = 0;

   /*
    * We use this to create the cabinet name
    */
   cab_parms->iCab               = 1;

   /*
    * If you want to use disk names, use this to
    * count disks
    */
   cab_parms->iDisk              = 0;

   /*
    * Choose your own number
    */
   cab_parms->setID              = setCabID;

   /*
    * Only important if CABs are spanning multiple
    * disks, in which case you will want to use a
    * real disk name.
    *
    * Can be left as an empty string.
    */
   strcpy( cab_parms->szDisk, szCabDisk );

   /* where to store the created CAB files */
   // strcpy(cab_parms->szCabPath, "c:\\");
   strcpy( cab_parms->szCabPath, szDestinationDir );

   /* store name of first CAB file */
   store_cab_name( cab_parms->szCab, cab_parms->iCab );
}
