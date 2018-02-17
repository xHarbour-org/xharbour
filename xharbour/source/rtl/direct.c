/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DIRECTORY() function
 *
 * Copyright 1999 Leslee Griffith <les.griffith@vantagesystems.ca>
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

/*
 * Notes from the fringe... <ptucker@sympatico.ca>
 *
 * Clipper is a bit schizoid with the treatment of file attributes, but we've
 * emulated that weirdness here for your viewing amusement.
 *
 * In Clippers' homeworld of DOS, there are 5 basic attributes: 'A'rchive,
 * 'H'idden, 'S'ystem, 'R'eadonly and 'D'irectory.  In addition, a file can
 * have no attributes, and only 1 file per physical partition can have the
 * 'V'olume label.
 *
 * For a given file request, it is implied that the attribute mask includes
 * all attributes except 'H'idden, 'S'ystem, 'D'irectory and 'V'olume.
 * The returned file list will always include (for instance) 'R'eadOnly files
 * unless they also happen to be 'H'idden and that attribute was not requested.
 *
 * "V" is a special case - you will get back the entry that describes the
 * volume label for the drive implied by the filemask.
 *
 * Differences from the 'standard' (where supported):
 * - Filenames will be returned in the same case as they are stored in the
 *   directory.  Clipper (and VO too) will convert the names to upper case
 * - Filenames will be the full filename as supported by the OS in use.
 * - There are a number of additional file attributes returned.
 *   They are:
 *       'I' - DEVICE      File is a device
 *       'T' - TEMPORARY   File is a Temporary file
 *       'P' - SPARSE      File is Sparse
 *       'L' - REPARSE     File/Dir is a reparse point
 *       'C' - COMPRESSED  File/Dir is compressed
 *       'O' - OFFLINE     File/Dir is not online
 *       'X' - NOTINDEXED  Exclude File/Dir from Indexing Service
 *       'E' - ENCRYPTED   File/Dir is Encrypted
 *       'M' - VOLCOMP     Volume Supports Compression
 * - Clipper can sometimes drop the ReadOnly indication of directories.
 *   Harbour detects this correctly.
 *
 * TODO: - Under an MS Windows implimentation, an optional 3rd parameter to
 *         Directory to allow you to receive the compatible '8.3' filename.
 *       - check that path support vis stat works on all platforms
 *       - UNC Support? ie: dir \\myserver\root
 *
 * TOFIX:- Volume label support
 *
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbregex.h"
#include "hbtrace.h"

#include "directry.ch"

static void hb_fsGrabDirectory( PHB_ITEM pDir, const char * szDirSpec, HB_FATTR uiMask, PHB_FNAME fDirSpec, BOOL bFullPath, BOOL bDirOnly )
{
   PHB_FFIND ffind;
   PHB_ITEM Subarray = hb_itemNew(NULL);
   /* Get the file list */
   if( ( ffind = hb_fsFindFirst( ( const char * ) szDirSpec, uiMask ) ) != NULL )
   {
      do
      {
         if( ! ( ( ( uiMask & HB_FA_HIDDEN ) == 0 && ( ffind->attr & HB_FA_HIDDEN ) != 0 ) ||
                 ( ( uiMask & HB_FA_SYSTEM ) == 0 && ( ffind->attr & HB_FA_SYSTEM ) != 0 ) ||
                 ( ( uiMask & HB_FA_LABEL ) == 0 && ( ffind->attr & HB_FA_LABEL ) != 0 ) ||
                 ( ( uiMask & HB_FA_DIRECTORY ) == 0 && ( ffind->attr & HB_FA_DIRECTORY ) != 0 ) ) )
         {
            char  buffer[ 32 ];
            BOOL  bAddEntry = TRUE;

            //HB_ITEM_NEW( Subarray );
            hb_arrayNew( Subarray, 5 );
            //hb_arrayNew( &Subarray, 5 );

            if( bFullPath )
            {
               char * szFullName = hb_xstrcpy( NULL, fDirSpec->szPath ? fDirSpec->szPath : "", ffind->szName, NULL );
               hb_arraySetC( Subarray, F_NAME, szFullName );
               hb_xfree( szFullName );
            }
            else
            {
               hb_arraySetC( Subarray, F_NAME, ffind->szName );
            }

            hb_arraySetNInt( Subarray, F_SIZE, ffind->size );
            hb_arraySetDL( Subarray, F_DATE, ffind->lDate );
            hb_arraySetC( Subarray, F_TIME, ffind->szTime );
            hb_arraySetC( Subarray, F_ATTR, hb_fsAttrDecode( ffind->attr, buffer ) );

            /* Don't exit when array limit is reached */
            if( bDirOnly )
            {
               /* 30/07/04 - <maurilio.longo@libero.it>
                             why do we issue a hb_fsIsDirectory() when hb_fsFindFirst() already returns
                             such info for every entry?
                  bAddEntry = hb_fsIsDirectory( ( BYTE * ) ffind->szName );
                */
               bAddEntry = ( ( ffind->attr & HB_FA_DIRECTORY ) == HB_FA_DIRECTORY );
            }

            if( bAddEntry )
            {
               hb_arrayAddForward( pDir, Subarray );
            }
            else
               hb_itemClear( Subarray );

         }
      }
      while( hb_fsFindNext( ffind ) );

      hb_fsFindClose( ffind );

   }
}

void hb_fsDirectory( PHB_ITEM pDir, const char * szSkleton, const char * szAttributes, BOOL bDirOnly, BOOL bFullPath )
{
   HB_FATTR         uiMask, uiMaskNoLabel;
   const char *   szDirSpec;
   char *         pszFree = NULL;

/*
   #if defined(__MINGW32__) || ( defined(_MSC_VER) && _MSC_VER >= 910 )
   PHB_ITEM pEightDotThree = hb_param( 3, HB_IT_LOGICAL );
   BOOL     bEightDotThree;

   // Do we want 8.3 support?
   bEightDotThree = ( pEightDotThree ? hb_itemGetL( pEightDotThree ) : FALSE );
   #endif
 */

   PHB_FNAME fDirSpec = NULL;

   /* Get the passed attributes and convert them to Harbour Flags */

   uiMask = HB_FA_ARCHIVE
            | HB_FA_READONLY
            | HB_FA_NORMAL
            | HB_FA_DEVICE
            | HB_FA_TEMPORARY
            | HB_FA_SPARSE
            | HB_FA_REPARSE
            | HB_FA_COMPRESSED
            | HB_FA_OFFLINE
            | HB_FA_NOTINDEXED
            | HB_FA_ENCRYPTED
            | HB_FA_VOLCOMP
			| HB_FA_PINNED
			| HB_FA_UNPINNED;

   uiMaskNoLabel = uiMask;

   hb_arrayNew( pDir, 0 );

   if( bDirOnly )
   {
      szAttributes = "D";
   }

   if( szAttributes && strlen( szAttributes ) > 0 )
   {
      if( ( uiMask |= hb_fsAttrEncode( szAttributes ) ) & HB_FA_LABEL )
      {
         /* NOTE: This is Clipper Doc compatible. (not operationally) */
         uiMask = HB_FA_LABEL;
      }
   }

   if( szSkleton && strlen( szSkleton ) > 0 )
      szDirSpec = hb_fsNameConv( szSkleton, &pszFree );
   else
      szDirSpec = HB_OS_ALLFILE_MASK;

   if( bDirOnly || bFullPath )
   {
      if( ( fDirSpec = hb_fsFNameSplit( szDirSpec ) ) != NULL )
      {
         if( fDirSpec->szDrive )
         {
            hb_fsChDrv( ( BYTE ) ( fDirSpec->szDrive[ 0 ] - 'A' ) );
         }

         if( fDirSpec->szPath )
         {
            hb_fsChDir( fDirSpec->szPath );
         }
      }
   }

   /* Get the file list */
   hb_fsGrabDirectory( pDir, szDirSpec, uiMask, fDirSpec, bFullPath, bDirOnly );

   if( uiMask == HB_FA_LABEL )
   {
      uiMaskNoLabel  |= hb_fsAttrEncode( szAttributes );
      uiMaskNoLabel  &= ~HB_FA_LABEL;
      hb_fsGrabDirectory( pDir, szDirSpec, uiMaskNoLabel, fDirSpec, bFullPath, bDirOnly );
   }

   if( fDirSpec != NULL )
   {
      hb_xfree( fDirSpec );
   }

   if( pszFree )
      hb_xfree( pszFree );
}

static BOOL hb_strMatchRegExpDir( const char * szString, const char * szMask, BOOL bInit )
{
   static HB_REGEX   RegEx;
   static BOOL       bInitReg = FALSE;

   if( bInit || ! bInitReg )
   {
      /* compile only once here */
      bInitReg = hb_regexCompile( &RegEx, szMask, 0, 0 );
   }

   if( ! szString && ! szMask )
   {
      /* resetting initialization flag */
      hb_regexFree( &RegEx );
      bInitReg = FALSE;
      return FALSE;
   }

   if( bInitReg )
   {
      return hb_regexMatch( &RegEx, szString, TRUE );
   }

   return FALSE;
}

static void hb_fsDirectoryCrawler( PHB_ITEM pRecurse, PHB_ITEM pResult, char * szFName, char * szAttributes, char * sRegEx )
{
   HB_SIZE ui, uiLen = hb_arrayLen( pRecurse ) ;

   for( ui = 0; ui < uiLen; ui++ )
   {
      PHB_ITEM pEntry   = hb_arrayGetItemPtr( pRecurse, ui + 1 );
      char *   szEntry  = hb_arrayGetC( pEntry, 1 );

      if( szEntry[ strlen( szEntry ) - 1 ] != '.' )
      {
         if( hb_fsIsDirectory( szEntry ) )
         {
            char * szSubdir = hb_xstrcpy( NULL, szEntry, HB_OS_PATH_DELIM_CHR_STRING, HB_OS_ALLFILE_MASK, NULL );
			PHB_ITEM SubDir = hb_itemNew(NULL);
            //HB_ITEM_NEW( SubDir );

            hb_fsDirectory( SubDir, szSubdir, szAttributes, FALSE, TRUE );

            hb_fsDirectoryCrawler( SubDir, pResult, szFName, szAttributes, sRegEx );

            hb_xfree( szSubdir );

            hb_itemClear( SubDir );
         }
         else
         {
            char * sFileName = strrchr( szEntry, HB_OS_PATH_DELIM_CHR );

            if( sFileName == NULL )
            {
               sFileName = szEntry;
            }
            else
            {
               sFileName++;
            }

            if( hb_strMatchRegExpDir( ( const char * ) sFileName, ( const char * ) sRegEx, FALSE ) )
            {
               hb_arrayAddForward( pResult, pEntry );
            }
         }
      }

      hb_xfree( szEntry );
   }
}

void hb_fsDirectoryRecursive( PHB_ITEM pResult, char * szSkleton, char * szFName, char * szAttributes, BOOL bMatchCase )
{
   static BOOL s_bTop = TRUE;
   char        cCurDsk;
   char *      pCurDir;

   //HB_ITEM_NEW( Dir );
   PHB_ITEM Dir = hb_itemNew(NULL);
   /* An arbitrary value which should be enough */
   char sRegEx[ HB_PATH_MAX + HB_PATH_MAX ];

   Wild2RegEx( szFName, sRegEx, bMatchCase );

   if( s_bTop )
   {
      cCurDsk  = hb_fsCurDrv();
      pCurDir  = hb_strdup( hb_fsCurDir( ( char ) cCurDsk ) );
      s_bTop   = FALSE;
   }
   else
   {
      cCurDsk  = 0;
      pCurDir  = NULL;
   }

   hb_fsDirectory( Dir, szSkleton, szAttributes, FALSE, TRUE );

   hb_arrayNew( pResult, 0 );

   /* initialize regex */
   hb_strMatchRegExpDir( ( const char * ) szFName, ( const char * ) sRegEx, TRUE );

   hb_fsDirectoryCrawler( Dir, pResult, szFName, szAttributes, sRegEx );

   hb_itemClear( Dir );

   /* reset regex for next loop */
   hb_strMatchRegExpDir( NULL, NULL, FALSE );

   if( pCurDir )
   {
      char sRoot[ 2 ];

      sRoot[ 0 ]  = HB_OS_PATH_DELIM_CHR;
      sRoot[ 1 ]  = '\0';

      hb_fsChDrv( ( BYTE ) cCurDsk );
      hb_fsChDir( sRoot );
      hb_fsChDir( pCurDir );

      hb_xfree( pCurDir );
      // For next run.
      s_bTop = TRUE;
   }
}

HB_FUNC( DIRECTORYRECURSE )
{
   PHB_ITEM    pDirSpec    = hb_param( 1, HB_IT_STRING );
   PHB_ITEM    pAttribute  = hb_param( 2, HB_IT_STRING );
   BOOL        bMatchCase  = hb_parl( 3 );
   char *      szRecurse   = NULL;
   PHB_FNAME   fDirSpec;

  // HB_ITEM_NEW( Dir );
  PHB_ITEM Dir=hb_itemNew(NULL);
   char *      szFName     = NULL;
#if defined( HB_OS_HAS_DRIVE_LETTER )
   BOOL        bAddDrive   = TRUE;
#endif
   char *      szAttributes;

   if( pDirSpec && hb_itemGetCLen( pDirSpec ) < HB_PATH_MAX )
   {
      szAttributes      = ( char * ) hb_xgrab( 3 + 1 );  // DHS
      hb_xmemset( szAttributes, 0, 4 );
      szAttributes[ 0 ] = 'D';                           // Compulsory

      if( pAttribute )
      {
         if( strpbrk( hb_itemGetCPtr( pAttribute ), "hH" ) != NULL )
         {
            hb_xstrcat( szAttributes, "H", 0 );
         }

         if( strpbrk( hb_itemGetCPtr( pAttribute ), "sS" ) != NULL )
         {
            hb_xstrcat( szAttributes, "S", 0 );
         }
      }

      if( ( fDirSpec = hb_fsFNameSplit( hb_itemGetCPtr( pDirSpec ) ) ) != NULL )
      {
#if defined( HB_OS_HAS_DRIVE_LETTER )
         if( fDirSpec->szDrive == NULL )
         {
            char szDrive[ 1 ];
            szDrive[ 0 ]      = ( ( char ) hb_fsCurDrv() ) + 'A';
            fDirSpec->szDrive = ( char * ) hb_szAscii[ ( UCHAR ) szDrive[ 0 ] ];
         }
         else
         {
            bAddDrive = FALSE;
         }
#endif
         if( fDirSpec->szPath == NULL )
         {
            #if defined( HB_OS_UNIX )
            fDirSpec->szPath  = ( char * ) hb_fsCurDirEx( hb_fsCurDrv() );
            #else
            fDirSpec->szPath  = ( char * ) hb_fsCurDir( hb_fsCurDrv() );
            #endif
         }

#if defined( HB_OS_HAS_DRIVE_LETTER )
         if( bAddDrive )
         {
            char * szDrvDelim[ 2 ];
#if defined( __XCC__ )
            szDrvDelim[ 0 ]   = ( char * ) HB_OS_DRIVE_DELIM_CHR;
#else
            szDrvDelim[ 0 ]   = ( char * ) HB_OS_DRIVE_DELIM_CHR;
#endif
            szDrvDelim[ 1 ]   = '\0';

            szRecurse         = hb_xstrcpy( NULL, fDirSpec->szDrive, szDrvDelim, HB_OS_PATH_DELIM_CHR_STRING, fDirSpec->szPath, HB_OS_PATH_DELIM_CHR_STRING, HB_OS_ALLFILE_MASK, NULL );
         }
         else
#endif
         {
#if defined( __DMC__ )
            /*
               AJ: 11-30-2007
               I found these funny lines have to be added for DMC 8.50.4
               otherwise GPF on tests/dirtest4.prg.
               Anybody to find where the bug may hide please... ?
             */
            char szDMCBug[ 1 ];
            sprintf( szDMCBug, "%s", "" );
#endif
            szRecurse = hb_xstrcpy( NULL, fDirSpec->szPath, HB_OS_ALLFILE_MASK, NULL );
         }
         /*
            AJ: 2011-02-27
            Fixing *.* not pulling file with no extension
          */
         if( ( strlen( fDirSpec->szName ) == 1 ) && ( hb_strnicmp( fDirSpec->szName, "*", 1 ) == 0 ) &&
             ( strlen( fDirSpec->szExtension ) == 2 ) && ( hb_strnicmp( fDirSpec->szExtension, ".*", 2 ) == 0 ) )
         {
            ( ( char * ) fDirSpec->szExtension )[ 0 ] = 0;
         }
         szFName = hb_xstrcpy( NULL, fDirSpec->szName, fDirSpec->szExtension, NULL );
      }

      hb_fsDirectoryRecursive( Dir, szRecurse, szFName, szAttributes, bMatchCase );

      hb_itemReturnForward( Dir );

      if( fDirSpec )
      {
         hb_xfree( fDirSpec );
      }

      if( szFName )
      {
         hb_xfree( szFName );
      }

      if( szRecurse )
      {
         hb_xfree( szRecurse );
      }

      if( szAttributes )
      {
         hb_xfree( szAttributes );
      }
   }
   else
   {
      hb_reta( 0 );
   }
}

HB_FUNC( DIRECTORY )
{
   PHB_ITEM pDir = hb_itemNew( NULL );

   hb_fsDirectory( pDir, hb_parcx( 1 ), hb_parcx( 2 ), hb_parl( 3 ), hb_parl( 4 ) );
   hb_itemRelease( hb_itemReturnForward( pDir ) );
}
