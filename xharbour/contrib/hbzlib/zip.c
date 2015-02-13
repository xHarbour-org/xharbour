/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour zip file compress function,
 *
 * Copyright 2000-2001 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2,  or ( at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not,  write to
 * the Free Software Foundation,  Inc.,  59 Temple Place,  Suite 330,
 * Boston,  MA 02111-1307 USA ( or visit the web site http://www.gnu.org/ ).
 *
 * As a special exception,  the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that,  if you link the Harbour libraries with other
 * files to produce an executable,  this does not by itself cause the
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
 * Harbour,  as the General Public License permits,  the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files,  you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour,  it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that,  delete this exception notice.
 *
 */
#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif



#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbinit.h"
#if ! defined( HB_OS_UNIX ) 
#  undef _LARGEFILE64_SOURCE
#endif

#if defined(HB_OS_WIN)
   #include <windows.h>
#endif     

#include <hbzip2.h>
#if defined( HB_OS_UNIX )
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <dirent.h>
#endif
#if ! defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and effectively enables lseek64/flock64/ftruncate64 functions
       * on 32bit machines.
       */
      #define HB_USE_LARGEFILE64
   #elif defined( HB_OS_UNIX ) && defined( O_LARGEFILE )
      #define HB_USE_LARGEFILE64
   #endif
#endif

extern PHB_ITEM   ZipArray;
static PHB_ITEM   FileToZip;
static PHB_ITEM   ExcludeFile;
static PHB_ITEM   UnzipFiles;
static PHB_ITEM   DelZip;
static PHB_ITEM   FileAttribs;
PHB_ITEM          ChangeDiskBlock;
#define FA_RDONLY 1              /* R */
#define FA_HIDDEN 2              /* H */
#define FA_SYSTEM 4              /* S */
#define FA_LABEL  8              /* V */
#define FA_DIREC  16             /* D */
#define FA_ARCH   32             /* A */
#define FA_NORMAL 128

extern int Wild2RegEx( const char * sWild, char * sRegEx, BOOL bMatchCase );
extern void hb_fsDirectory( PHB_ITEM pDir, const char * szSkleton, const char * szAttributes, BOOL bDirOnly, BOOL bFullPath );

#if defined(HB_OS_UNIX)
extern int GetFileAttributes( char * szEntry );
extern void SetFileAttributes( char * szEntry, ULONG ulAttr );
#endif


static void ResetAttribs( void )
{
   ULONG ulAtt, ulZipLen = hb_arrayLen( FileToZip );

   for( ulAtt = 0; ulAtt < ulZipLen; ulAtt++ )
   {
      char *   szFile   = hb_arrayGetC( FileToZip, ulAtt + 1 );
      int      iAttr    = hb_arrayGetNI( FileAttribs, ulAtt + 1 );
      SetFileAttributes( szFile, iAttr  );
      hb_xfree( szFile );
   }

   hb_itemRelease( FileAttribs );
   hb_itemRelease( FileToZip );
}

static void UnzipCreateArray( char * szZipFileName, char * szSkleton, int uiOption )
{
   int      ul;
   char *   szEntry;
   PHB_ITEM pZipEntry;
   PHB_ITEM Temp;
   BOOL     bOkAdd;
   int      ulLen = hb_arrayLen( ZipArray );
   char     sRegEx[ HB_PATH_MAX + HB_PATH_MAX ];

   HB_SYMBOL_UNUSED( szZipFileName );

   Wild2RegEx( szSkleton, sRegEx, FALSE );


   for( ul = 0; ul < ulLen; ul++ )
   {
      bOkAdd      = TRUE;
      pZipEntry   = hb_arrayGetItemPtr( ZipArray, ul + 1 );
      szEntry     = hb_arrayGetC( pZipEntry, 1 );

      if( szSkleton )
      {
         bOkAdd = hb_strMatchRegExp( ( const char * ) szEntry, ( const char * ) sRegEx );
      }

      if( ! bOkAdd )
      {
         PHB_FNAME pFileName = hb_fsFNameSplit( szEntry );

         if( pFileName->szName )
         {
            char * szFile = ( char * ) hb_xgrab( HB_PATH_MAX );
            pFileName->szPath = ( char * ) "";
            hb_fsFNameMerge( szFile, pFileName );
            bOkAdd            = ( hb_stricmp( szSkleton, szFile ) == 0  ? 1 : 0 );
            hb_xfree( szFile );
            if( ! bOkAdd )
            {
               bOkAdd = ( hb_stricmp( szSkleton, szEntry ) == 0  ? 1 : 0 );
            }

         }
         hb_xfree( pFileName );
      }


      if( bOkAdd )
      {
         if( uiOption == 1 )
         {
            Temp = hb_itemNew( NULL );
            hb_arrayAddForward( UnzipFiles, hb_itemPutC( Temp, szEntry ) );
            hb_itemRelease( Temp );
         }
         else
         {
            Temp = hb_itemNew( NULL );
            hb_arrayAddForward( DelZip, hb_itemPutC( Temp, szEntry ) );
            hb_itemRelease( Temp );
         }
      }

      hb_xfree( szEntry );
   }
}

static BOOL ZipTestExclude( char * szEntry )
{
   int   uiEx;
   BOOL  bNotFound   = TRUE;
   int   uiExLen     = hb_arrayLen( ExcludeFile );

   for( uiEx = 0; uiEx < uiExLen; uiEx++ )
   {
      char * szExclude = hb_arrayGetC( ExcludeFile, uiEx + 1 );
      if( strcmp( szExclude, hb_strupr( szEntry ) ) == 0 )
      {
         hb_xfree( szExclude );
         bNotFound = FALSE;
         break;
      }
      hb_xfree( szExclude );
   }

   return bNotFound;
}

static void ZipCreateExclude( PHB_ITEM pExclude )
{
   PHB_ITEM ExTmp;

   ExcludeFile = hb_itemArrayNew( 0 );

   if( pExclude == NULL )
   {
      return;
   }

   if( HB_IS_STRING( pExclude ) )
   {
      if( hb_itemGetCLen( pExclude ) == 0 )
      {
         return;
      }

      if( strchr( hb_itemGetCPtr( pExclude ), '*' ) != NULL || strchr( hb_itemGetCPtr( pExclude ), '?' ) != NULL )
      {
         PHB_ITEM WildFile;
         PHB_ITEM pDirEntry;
         int      uiLen;
         int      ui;

         WildFile = hb_itemNew( NULL );

         hb_fsDirectory( WildFile, hb_itemGetCPtr( pExclude ), NULL, 0, TRUE );
         uiLen    = hb_arrayLen( WildFile );

         for( ui = 0; ui < uiLen; ui++ )
         {
            char * szEntry;
            pDirEntry   = hb_arrayGetItemPtr( WildFile, ui + 1 );
            szEntry     = hb_arrayGetC( pDirEntry, 1 );

            if( szEntry )
            {
               ExTmp = hb_itemPutC( NULL, hb_strupr( szEntry ) );
               hb_arrayAddForward( ExcludeFile, ExTmp );
               hb_xfree( szEntry );
               hb_itemRelease( ExTmp );
            }
         }

         hb_itemRelease( WildFile );
      }
      else
      {
         ExTmp = hb_itemPutC( NULL, hb_itemGetCPtr( pExclude ) );
         hb_arrayAddForward( ExcludeFile, ExTmp );
         hb_itemRelease( ExTmp );
      }
   }
   else if( HB_IS_ARRAY( pExclude ) )
   {
      int      ux;
      int      ufx = hb_arrayLen( pExclude );
      char *   szExclude;
      PHB_ITEM WildFile;
      PHB_ITEM pDirEntry;

      WildFile = hb_itemNew( NULL );

      if( ufx == 0 )
      {
         return;
      }

      for( ux = 0; ux < ufx; ux++ )
      {
         szExclude = hb_arrayGetC( pExclude, ux + 1 );

         if( szExclude )
         {
            if( strchr( szExclude, '*' ) != NULL || strchr( szExclude, '?' ) != NULL )
            {
               int      uiW, uiWLen;
               char *   szEntry;

               hb_fsDirectory( WildFile, szExclude, NULL, 0, TRUE );
               uiWLen = hb_arrayLen( WildFile );

               for( uiW = 0; uiW < uiWLen; uiW++ )
               {
                  pDirEntry   = hb_arrayGetItemPtr( WildFile, uiW + 1 );
                  szEntry     = hb_arrayGetC( pDirEntry, 1 );
                  ExTmp       = hb_itemNew( NULL );
                  hb_arrayAddForward( ExcludeFile, hb_itemPutC( ExTmp, szEntry ) );
                  hb_itemRelease( ExTmp );
                  hb_xfree( szEntry );
               }
            }
            else
            {
               ExTmp = hb_itemNew( NULL );
               hb_arrayAddForward( ExcludeFile, hb_itemPutC( ExTmp, szExclude ) );
               hb_itemRelease( ExTmp );
            }

            hb_xfree( szExclude );
         }
      }

      hb_itemRelease( WildFile );
   }
}

static void ZipCreateArray( PHB_ITEM pParam, BYTE * pCurDir, BOOL bFullPath )    /* bFullPath by JGS */
{
   PHB_ITEM pDirEntry, Temp, TempArray;
   PHB_ITEM WildFile = hb_itemNew( NULL );
   int      ul, ulLen, ulArr, ulLenArr;

   FileToZip   = hb_itemArrayNew( 0 );
   FileAttribs = hb_itemArrayNew( 0 );

   if( pParam->type == HB_IT_STRING )
   {
      TempArray   = hb_itemArrayNew( 0 );
      Temp        = hb_itemPutC( NULL, hb_itemGetCPtr( pParam ) );
      hb_arrayAddForward( TempArray, Temp );
      hb_itemRelease( Temp );
   }
   else
   {
      TempArray = hb_arrayClone( pParam, NULL );
   }

   ulLenArr = hb_arrayLen( TempArray );

   for( ulArr = 0; ulArr < ulLenArr; ulArr++ )
   {
      char * szArrEntry = hb_arrayGetC( TempArray, ulArr + 1 );

      if( szArrEntry )
      {
         if( strchr( szArrEntry, '*' ) != NULL || strchr( szArrEntry, '?' ) != NULL )
         {
         #if defined( HB_WIN32_IO )
            /* by JGS if don't gave path or there is a relative path add current dir ! */
            PHB_FNAME fDirSpec = hb_fsFNameSplit( ( char * ) szArrEntry );

            if( ( pCurDir ) && ( fDirSpec != NULL ) &&
                ! ( fDirSpec->szDrive ) && ( fDirSpec->szPath ) && ( fDirSpec->szPath[ 0 ] != HB_OS_PATH_DELIM_CHR ) )
         #else
            /* if don't gave path add current dir ! */
            if( ( pCurDir ) && ( ! strchr( szArrEntry, HB_OS_PATH_DELIM_CHR ) ) )
         #endif
            {
               char * szTemp = szArrEntry;
               szArrEntry = hb_xstrcpy( NULL, ( char * ) pCurDir, HB_OS_PATH_DELIM_CHR_STRING, szTemp, NULL );
               hb_xfree( szTemp );
            }

            hb_fsDirectory( WildFile, szArrEntry, NULL, 0, bFullPath ); /* bFullPath by JGS */
            ulLen = hb_arrayLen( WildFile );

            for( ul = 0; ul < ulLen; ul++ )
            {
               char * szEntry;
               pDirEntry   = hb_arrayGetItemPtr( WildFile, ul + 1 );
               szEntry     = hb_arrayGetC( pDirEntry, 1 );

               /* by JGS */
               #if defined( HB_WIN32_IO )
               if( ! ( bFullPath ) && ( fDirSpec != NULL ) && ( fDirSpec->szPath ) )
               {
                  char * szFile = szEntry;
                  szEntry = hb_xstrcpy( NULL, fDirSpec->szPath, szFile, NULL );
                  hb_xfree( szFile );
               }
               #endif
               /* by JGS */

               if( ZipTestExclude( szEntry ) )
               {
                  Temp  = hb_itemNew( NULL );
                  hb_arrayAddForward( FileToZip, hb_itemPutC( Temp, szEntry ) );
                  hb_itemRelease( Temp );
                  Temp  = hb_itemNew( NULL );
                  hb_arrayAddForward( FileAttribs, hb_itemPutNI( Temp, GetFileAttributes( szEntry ) ) );
                  hb_itemRelease( Temp );
                  #if defined( HB_OS_UNIX ) 
                  SetFileAttributes( szEntry, 0777 );
                  #else
                  SetFileAttributes( szEntry, FA_ARCH );
                  #endif
               }

               if( szEntry )
               {
                  hb_xfree( szEntry );
               }
            }

            /* by JGS */
            #if defined( HB_WIN32_IO )
            if( fDirSpec )
            {
               hb_xfree( fDirSpec );
            }
            #endif

            hb_itemClear( WildFile );
            /* by JGS */
         }
         else
         {
            Temp  = hb_itemPutC( NULL, szArrEntry );
            hb_arrayAddForward( FileToZip, Temp );
            hb_itemRelease( Temp );
            Temp  = hb_itemPutNI( NULL, GetFileAttributes( szArrEntry ) );
            hb_arrayAddForward( FileAttribs, Temp );
            hb_itemRelease( Temp );

                  #if defined( HB_OS_UNIX )
            SetFileAttributes( szArrEntry, 0777 );
                  #else
            SetFileAttributes( szArrEntry, FA_ARCH );
                  #endif
         }

         hb_xfree( szArrEntry );
      }
   }

   hb_itemRelease( WildFile );
   hb_itemRelease( TempArray );
}

HB_FUNC( HB_ZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if( pParam )
      {
         char     szFile[ HB_PATH_MAX ];
         PHB_ITEM pExclude = hb_param( 10, HB_IT_STRING | HB_IT_ARRAY );
         BYTE *   pCurDir;
         char *   szZipFileName;

         /* by JGS */
         BOOL     bFullPath = TRUE;
         #if defined( HB_WIN32_IO )
         if( ISLOG( 11 ) )
         {
            bFullPath = hb_parl( 11 );
         }
         #endif
         /* by JGS */

         if( ! ISNIL( 4 ) && ! ISBLOCK( 4 ) )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 2017, "Invalid Codeblock ", "hb_zipfile",
                                  4, hb_paramError( 1 ),
                                  hb_paramError( 2 ),
                                  hb_paramError( 3 ),
                                  hb_paramError( 4 ) );
            return;
         }

         pCurDir = ( BYTE * ) hb_xstrcpy( NULL, HB_OS_PATH_DELIM_CHR_STRING, ( const char * ) hb_fsCurDir( 0 ), NULL );

         /* Always needs to create an array */
         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam, pCurDir, bFullPath );  /* bFullPath by JGS */

         hb_fsChDir( ( const char * ) pCurDir );

         if( ! strchr( hb_parc( 1 ), HB_OS_PATH_DELIM_CHR ) )
         {
            hb_xstrcpy( szFile, ( char * ) pCurDir, 0 );
            hb_xstrcat( szFile, HB_OS_PATH_DELIM_CHR_STRING, hb_parc( 1 ), 0 );
         }
         else
         {
            hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
         }

         hb_xfree( pCurDir );
         szZipFileName = hb___CheckFile( szFile, FALSE );

         if( szZipFileName && hb_arrayLen( FileToZip ) > 0 )
         {
            PHB_ITEM pProgress = ISBLOCK( 9 ) ? hb_itemNew( hb_param( 9, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
            bRet = hb_CompressFile( szZipFileName,
                                    FileToZip,
                                    ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                    hb_param( 4, HB_IT_BLOCK ),
                                    ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                    ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                    ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                    ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                    pProgress );
            ResetAttribs();
            hb_itemRelease( pProgress );
            hb_xfree( szZipFileName );
         }

         hb_itemRelease( ExcludeFile );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_GETFILESINZIP )
{
   if( ISCHAR( 1 ) )
   {
      char     szFile[ HB_PATH_MAX ];
      char *   szZipFileName;

      hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
      szZipFileName = hb___CheckFile( szFile, TRUE );;

      if( szZipFileName )
      {
         PHB_ITEM pArray;

         pArray = hb___GetFileNamesFromZip( szZipFileName,
                                            ISLOG( 2 ) ? hb_parl( 2 ) : 0 );

         hb_itemReturn( pArray );
         hb_itemRelease( pArray );
         hb_xfree( szZipFileName );
      }
   }
}

HB_FUNC( HB_ZIPWITHPASSWORD )
{
   hb_retl( hb_IsPassWord( hb_parc( 1 ) ) );
}

HB_FUNC( HB_GETFILECOUNT )
{
   int iRet = 0;

   if( ISCHAR( 1 ) )
   {
      char     szFile[ HB_PATH_MAX ];
      char *   szZipFileName;

      hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
      szZipFileName = hb___CheckFile( szFile, TRUE );

      if( szZipFileName )
      {
         iRet = hb___GetNumberofFilestoUnzip( szZipFileName );
         hb_xfree( szZipFileName );
      }
   }

   hb_retni( iRet );
}

HB_FUNC( HB_ZIPFILEBYTDSPAN )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if( pParam )
      {
         char     szFile[ HB_PATH_MAX ];
         PHB_ITEM pExclude = hb_param( 11, HB_IT_STRING | HB_IT_ARRAY );
         char *   szZipFileName;
         BYTE *   pCurDir;

         /* by JGS */
         BOOL     bFullPath = TRUE;
         #if defined( HB_WIN32_IO )
         if( ISLOG( 12 ) )
         {
            bFullPath = hb_parl( 12 );
         }
         #endif
         /* by JGS */

         pCurDir = ( BYTE * ) hb_xstrcpy( NULL, HB_OS_PATH_DELIM_CHR_STRING, ( const char * ) hb_fsCurDir( 0 ), NULL );

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam, pCurDir, bFullPath );  /* bFullPath by JGS */

         hb_fsChDir( ( const char * ) pCurDir );
         /* by JGS, wait until adding the directory to the file name if not specified
            hb_xfree( pCurDir );
          */
         if( ! strchr( hb_parc( 1 ), HB_OS_PATH_DELIM_CHR ) )
         {
            hb_xstrcpy( szFile, ( char * ) pCurDir, 0 );
            hb_xstrcat( szFile, HB_OS_PATH_DELIM_CHR_STRING, hb_parc( 1 ), 0 );
         }
         else
         {
            hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
         }
         hb_xfree( pCurDir );   /* by JGS */
         szZipFileName = hb___CheckFile( szFile, TRUE );

         if( szZipFileName && hb_arrayLen( FileToZip ) > 0 )
         {
            PHB_ITEM pProgress = ISBLOCK( 10 ) ? hb_itemNew( hb_param( 10, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
            bRet = hb_CmpTdSpan( szZipFileName,
                                 FileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 hb_param( 4, HB_IT_BLOCK ),
                                 ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISNUM( 7 ) ? hb_parni( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                 ISLOG( 9 ) ? hb_parl( 9 ) : 0,
                                 pProgress );
            ResetAttribs();
            hb_itemRelease( pProgress );
            hb_xfree( szZipFileName );
         }

         hb_itemRelease( ExcludeFile );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_ZIPFILEBYPKSPAN )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if( pParam )
      {
         char     szFile[ HB_PATH_MAX ];
         PHB_ITEM pExclude = hb_param( 10, HB_IT_STRING | HB_IT_ARRAY );
         char *   szZipFileName;
         BYTE *   pCurDir;

         /* by JGS */
         BOOL     bFullPath = TRUE;
         #if defined( HB_WIN32_IO )
         if( ISLOG( 11 ) )
         {
            bFullPath = hb_parl( 11 );
         }
         #endif
         /* by JGS */

         pCurDir = ( BYTE * ) hb_xstrcpy( NULL, HB_OS_PATH_DELIM_CHR_STRING, ( const char * ) hb_fsCurDir( 0 ), NULL );

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam, pCurDir, bFullPath );  /* bFullPath by JGS */

         hb_fsChDir( ( const char * ) pCurDir );
         /* by JGS, wait until adding the directory to the file name if not specified
            hb_xfree( pCurDir ) ;
            hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
          */
         if( ! strchr( szFile, HB_OS_PATH_DELIM_CHR ) )
         {
            hb_xstrcpy( szFile, ( char * ) pCurDir, 0 );
            hb_xstrcat( szFile, HB_OS_PATH_DELIM_CHR_STRING, hb_parc( 1 ), 0 );
         }
         else
         {
            hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
         }
         hb_xfree( pCurDir );
         /* by JGS */
         szZipFileName = hb___CheckFile( szFile, TRUE );

         if( szZipFileName && hb_arrayLen( FileToZip ) > 0 )
         {
            PHB_ITEM pProgress = ISBLOCK( 9 ) ? hb_itemNew( hb_param( 9, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
            bRet = hb_CmpPkSpan( szZipFileName,
                                 FileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 hb_param( 4, HB_IT_BLOCK ),
                                 ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                 pProgress );
            ResetAttribs();
            hb_itemRelease( pProgress );
            hb_xfree( szZipFileName );
         }

         hb_itemRelease( ExcludeFile );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_UNZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      char *   szZipFileName;
      BYTE *   pCurDir;
      PHB_ITEM pUnzip;
      char     szFile[ HB_PATH_MAX ];

      hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
      szZipFileName = hb___CheckFile( szFile, TRUE );

      if( szZipFileName )
      {
         BYTE bCurDrv = hb_fsCurDrv();

         if( ISARRAY( 6 ) || ISCHAR( 6 ) )
            pUnzip = hb_param( 6, HB_IT_ANY );

         pCurDir     = ( BYTE * ) hb_xstrcpy( NULL, HB_OS_PATH_DELIM_CHR_STRING, ( const char * ) hb_fsCurDir( 0 ), NULL );
         UnzipFiles  = hb_itemArrayNew( 0 );

         if( hb_TestForPKS( szZipFileName ) <= 0 )
         {
            hb___GetFileNamesFromZip( szZipFileName, TRUE );

            if( pUnzip )
            {
               if( HB_IS_STRING( pUnzip ) )
               {
                  UnzipCreateArray( szZipFileName, hb_itemGetCPtr( pUnzip ), 1 );
               }
               else if( HB_IS_ARRAY( pUnzip ) )
               {
                  int      uiZ, uiZLen = hb_arrayLen( pUnzip );


                  for( uiZ = 0; uiZ < uiZLen; uiZ++ )
                  {
	                 char *   szUnzip = hb_arrayGetC( pUnzip, uiZ + 1 );

                     if( szUnzip )
                     {
                        UnzipCreateArray( szZipFileName, szUnzip, 1 );
                        //hb_xfree( szUnzip );
                     }
                  }
               }
            }
         }
         else
         {
            UnzipCreateArray( szZipFileName, ( char * ) "*", 1 );
         }

         if( hb_arrayLen( UnzipFiles ) > 0 )
         {
            PHB_ITEM pProgress = ISBLOCK( 7 ) ? hb_itemNew( hb_param( 7, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
            bRet = hb_UnzipSel( szZipFileName,
                                hb_param( 2, HB_IT_BLOCK ),
                                ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                                ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                                ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                                UnzipFiles,
                                pProgress );
            hb_itemRelease( pProgress );
         }

         hb_xfree( szZipFileName );
         hb_itemRelease( UnzipFiles );
         hb_fsChDrv( bCurDrv );
         hb_fsChDir( ( const char * ) pCurDir );
         hb_xfree( pCurDir );
         hb_itemClear( ZipArray );
         hb_itemRelease( ZipArray );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_SETDISKZIP )
{
   hb_retl( hb___SetCallbackFunc( hb_param( 1, HB_IT_BLOCK ) ) );
}

HB_FUNC( HB_ZIPDELETEFILES )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pDelZip = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY | HB_IT_NUMERIC );

      if( pDelZip )
      {
         char     szFile[ HB_PATH_MAX ];
         char *   szZipFileName;
         int      ulLen;

         DelZip         = hb_itemArrayNew( 0 );
         hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
         szZipFileName  = hb___CheckFile( szFile, TRUE );

         if( szZipFileName )
         {
            hb___GetFileNamesFromZip( szZipFileName, TRUE );
            ulLen = hb_arrayLen( ZipArray );

            if( ! ulLen )
            {
               hb_xfree( szZipFileName );
               hb_itemClear( ZipArray );
               hb_itemRelease( ZipArray );
               hb_retl( bRet );
               return;
            }

            if( HB_IS_STRING( pDelZip ) )
            {
               if( hb_itemGetCLen( pDelZip ) > 0 )
               {
                  UnzipCreateArray( szZipFileName, hb_itemGetCPtr( pDelZip ), 2 );
               }
            }
            else if( HB_IS_ARRAY( pDelZip ) )
            {
               int uiInLen = hb_arrayLen( pDelZip );

               if( uiInLen > 0 )
               {
                  int      uiIn;
                  char *   szInput;

                  for( uiIn = 0; uiIn < uiInLen; uiIn++ )
                  {
                     szInput = hb_arrayGetC( pDelZip, uiIn + 1 );

                     if( szInput )
                     {
                        UnzipCreateArray( szZipFileName, szInput, 2 );
                        hb_xfree( szInput );
                     }
                  }

               }
            }
            else if( HB_IS_NUMERIC( pDelZip ) )
            {
               int      iIndex = hb_itemGetNI( pDelZip );
               PHB_ITEM Temp;


               if( iIndex > 0 && iIndex <= ulLen )
               {
                  PHB_ITEM pZipEntry   = hb_arrayGetItemPtr( ZipArray, iIndex );
                  char *   szEntry     = hb_arrayGetC( pZipEntry, 1 );
                  Temp = hb_itemNew( NULL );
                  hb_arrayAddForward( DelZip, hb_itemPutC( Temp, szEntry ) );
                  hb_xfree( szEntry );
                  hb_itemRelease( Temp );
               }
            }

            if( hb_arrayLen( DelZip ) > 0 )
            {
               bRet = hb_DeleteSel( szZipFileName,
                                    DelZip,
                                    ISLOG( 3 ) ? hb_parl( 3 ) : 0 );
            }

            hb_xfree( szZipFileName );
            hb_itemClear( ZipArray );
            hb_itemRelease( ZipArray );
         }

         hb_itemRelease( DelZip );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_ZIPTESTPK )
{
   char     szFile[ HB_PATH_MAX ];
   char *   szZipFileName;

   hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
   szZipFileName = hb___CheckFile( szFile, TRUE );

   if( szZipFileName )
   {
      hb_retni( hb_TestForPKS( szZipFileName ) );
      hb_xfree( szZipFileName );
   }
}

HB_FUNC( HB_SETBUFFER )
{
   hb_SetZipBuff( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

HB_FUNC( HB_SETZIPCOMMENT )
{
   hb_SetZipComment( hb_parc( 1 ) );
}

HB_FUNC( HB_GETZIPCOMMENT )
{
   char * szComment = ( char * ) hb_GetZipComment( hb_parc( 1 ) );

   hb_retcAdopt( szComment );
}

HB_FUNC( HB_UNZIPFILEINDEX )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pDelZip = hb_param( 6, HB_IT_NUMERIC | HB_IT_ARRAY );

      if( pDelZip )
      {
         char     szFile[ HB_PATH_MAX ];
         PHB_ITEM Temp, DelZip;
         char *   szZipFileName;
         int      ulLen;

         hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
         szZipFileName = hb___CheckFile( szFile, TRUE );

         if( szZipFileName )
         {
            DelZip   = hb_itemArrayNew( 0 );
            hb___GetFileNamesFromZip( szZipFileName, TRUE );
            ulLen    = hb_arrayLen( ZipArray );

            if( HB_IS_NUMERIC( pDelZip ) )
            {
               int iIndex = hb_itemGetNI( pDelZip );

               if( iIndex > 0 && iIndex <= ulLen )
               {
                  Temp = hb_itemNew( NULL );
                  hb_arrayAddForward( DelZip, hb_itemPutNI( Temp, iIndex ) );
                  hb_itemRelease( Temp );
               }
            }
            else
            {
               int ui, iIndex;

               for( ui = 0; ui < ulLen; ui++ )
               {
                  iIndex = hb_arrayGetNI( pDelZip, ui + 1 );
                  if( iIndex && iIndex > 0 && iIndex <= ulLen )
                  {
                     Temp = hb_itemNew( NULL );
                     hb_arrayAddForward( DelZip, hb_itemPutNI( Temp, iIndex ) );
                     hb_itemRelease( Temp );
                  }
               }

            }

            if( hb_arrayLen( DelZip ) > 0 )
            {
               PHB_ITEM pProgress = ISBLOCK( 7 ) ? hb_itemNew( hb_param( 7, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
               bRet = hb_UnzipSelIndex( szZipFileName,
                                        hb_param( 2, HB_IT_BLOCK ),
                                        ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                                        ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                                        hb_parc( 5 ),
                                        DelZip,
                                        pProgress );
               hb_itemRelease( pProgress );
            }

            hb_itemRelease( DelZip );
            hb_xfree( szZipFileName );
            hb_itemClear( ZipArray );
            hb_itemRelease( ZipArray );
         }

      }
   }

   hb_retl( bRet );
}

/*
   HB_FUNC( HB_ZIPINMEMORY )
   {
    hb_retl( hb_CreateZipInMemory( hb_parc( 1 ), hb_parc( 2 ) ) );
   }

   HB_FUNC( HB_SAVEZIPFROMMEMORY )
   {
    hb_retl( hb_SaveZipFileFromMemory( ) );
   }
 */

HB_FUNC( TRANSFERFROMZIP )
{
   hb_retl( hb_TransferFilesFromzip( hb_parc( 1 ),
                                     hb_parc( 2 ),
                                     hb_param( 3, HB_IT_ARRAY ) ) );
}

HB_FUNC( SETZIPREADONLY )
{
   hb_SetZipReadOnly( hb_parl( 1 ) );
}

HB_FUNC( HB_UNZIPALLFILE )
{
   if( ! ISCHAR( 6 ) && ! ISARRAY( 6 ) )
   {
      char     szFile[ HB_PATH_MAX ];
      char *   szZipFile;

      hb_xstrcpy( szFile, hb_parc( 1 ), 0 );
      szZipFile = hb___CheckFile( szFile, TRUE );

      if( szZipFile )
      {
         PHB_ITEM pProgress = ISBLOCK( 7 ) ? hb_itemNew( hb_param( 7, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
         hb_retl( hb_UnzipAll( szZipFile,
                               hb_param( 2, HB_IT_BLOCK ),
                               ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                               hb_parc( 4 ),
                               ISCHAR( 5 ) ? hb_parc( 5 ) : NULL,
                               hb_param( 6, HB_IT_BLOCK ),
                               pProgress ) );
         hb_xfree( szZipFile );
         hb_itemRelease( pProgress );
      }
   }
}

HB_FUNC_EXIT( HBZIPCLEANUP )
{
   if( ChangeDiskBlock )
   {
      hb_itemRelease( ChangeDiskBlock );
      ChangeDiskBlock = NULL;
   }
}

#if defined( HB_OS_UNIX )

int GetFileAttributes( char * szEntry )
{
#     if defined( HB_USE_LARGEFILE64 )
         struct stat64 sStat;
         stat64( szEntry, &sStat );
#     else	
   struct stat sStat;

   stat( szEntry, &sStat );
#endif   
   return ( int ) sStat.st_mode;
}
void SetFileAttributes( char * szEntry, ULONG ulAttr )
{
   chmod( szEntry, ulAttr );
}
#endif

#define __PRG_SOURCE__     ( char * ) "zip.c"
#ifdef HB_PCODE_VER
#  undef HB_PRG_PCODE_VER
#  define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

HB_INIT_SYMBOLS_BEGIN( hbzip_CLEANUP )
{
   ( char * ) "HBZIPCLEANUP$", { HB_FS_EXIT | HB_FS_LOCAL }, { HB_EXIT_FUNCNAME( HBZIPCLEANUP ) }, &ModuleFakeDyn
}
HB_INIT_SYMBOLS_END( hbzip_CLEANUP )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hbzip_CLEANUP
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY HB_DATASEG_FUNC( hbzip_CLEANUP )
   #include "hbiniseg.h"
#endif
