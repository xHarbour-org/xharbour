/*
 * $Id: zip.c,v 1.13 2004/02/25 14:30:07 andijahja Exp $
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

#include <hbzip2.h>

extern HB_ITEM ZipArray;
static HB_ITEM FileToZip;

static void ZipCreateArray( PHB_ITEM pParam )
{
   PHB_ITEM pDirEntry;
   PHB_ITEM pWildFile;
   HB_ITEM Temp;
   int ul, ulLen;
   char *szEntry;

   FileToZip.type = HB_IT_NIL;
   Temp.type = HB_IT_NIL;
   pWildFile = hb_fsDirectory(pParam->item.asString.value,NULL,NULL,TRUE);
   hb_arrayNew( &FileToZip, 0);
   ulLen = pWildFile->item.asArray.value->ulLen;

   for ( ul = 0; ul < ulLen ; ul ++ )
   {
      pDirEntry = hb_arrayGetItemPtr( pWildFile, ul + 1 );
      szEntry = hb_arrayGetC( pDirEntry, 1 );
      hb_arrayAddForward( &FileToZip, hb_itemPutC( &Temp, szEntry ) );
      hb_xfree( szEntry );
   }

   hb_itemRelease( pWildFile );
}

HB_FUNC( HB_ZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if ( pParam )
      {
         char szFile[ _POSIX_PATH_MAX ];
         PHB_ITEM pProgress = hb_param( 9, HB_IT_BLOCK );
         HB_ITEM iProgress;

         iProgress.type = HB_IT_NIL;

         if ( hb_param( 2, HB_IT_STRING ) )
         {
            ZipCreateArray( hb_param( 2, HB_IT_STRING ) );
         }
         else
         {
            hb_itemCopy( &FileToZip, pParam );
         }

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

         strcpy( szFile, hb_parc( 1 ) );

         bRet = hb_CompressFile( hb___CheckFile( szFile ),
                                 &FileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 hb_param( 4, HB_IT_BLOCK ),
                                 ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                 &iProgress );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_GETFILESINZIP )
{
   if( ISCHAR( 1 ) )
   {
      char szFile[ _POSIX_PATH_MAX ];
      strcpy( szFile, hb_parc( 1 ) );
      hb___GetFileNamesFromZip( hb___CheckFile( szFile ),
                                ISLOG( 2 ) ? hb_parl( 2 ) : 0 );
      hb_itemReturn( &ZipArray );
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
      char szFile[ _POSIX_PATH_MAX ];
      strcpy( szFile, hb_parc( 1 ) );

      iRet = hb___GetNumberofFilestoUnzip( hb___CheckFile( szFile ) );
   }

   hb_retni( iRet );
}

HB_FUNC( HB_ZIPFILEBYTDSPAN )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if ( pParam )
      {
         char szFile[ _POSIX_PATH_MAX ];
         PHB_ITEM pProgress = hb_param( 10, HB_IT_BLOCK );
         HB_ITEM iProgress;

         iProgress.type = HB_IT_NIL;

         if ( hb_param( 2, HB_IT_STRING ) )
         {
            ZipCreateArray( hb_param( 2, HB_IT_STRING ) );
         }
         else
         {
            hb_itemCopy( &FileToZip, pParam );
         }

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

         strcpy( szFile, hb_parc( 1 ) );

         bRet = hb_CmpTdSpan( hb___CheckFile( szFile ),
                              &FileToZip,
                              ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                              hb_param( 4, HB_IT_BLOCK ),
                              ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                              ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                              ISNUM( 7 ) ? hb_parni( 7 ) : 0,
                              ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                              ISLOG( 9 ) ? hb_parl( 9 ) : 0,
                              &iProgress );
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

      if ( pParam )
      {
         char szFile[ _POSIX_PATH_MAX ];
         PHB_ITEM pProgress = hb_param( 9, HB_IT_BLOCK );
         HB_ITEM iProgress;

         iProgress.type = HB_IT_NIL;

         if ( hb_param( 2, HB_IT_STRING ) )
         {
            ZipCreateArray( hb_param( 2, HB_IT_STRING ) );
         }
         else
         {
            hb_itemCopy( &FileToZip, pParam );
         }

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

         strcpy( szFile, hb_parc( 1 ) );

         bRet = hb_CmpPkSpan( hb___CheckFile( szFile ),
                              &FileToZip,
                              ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                              hb_param( 4, HB_IT_BLOCK ),
                              ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                              ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                              ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                              ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                              &iProgress );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_UNZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      char szFile[ _POSIX_PATH_MAX ];
      PHB_ITEM pProgress = hb_param( 7, HB_IT_BLOCK );
      HB_ITEM iProgress;

      iProgress.type = HB_IT_NIL;

      if( pProgress )
      {
        hb_itemCopy( &iProgress, pProgress );
      }

      strcpy( szFile, hb_parc( 1 ) );

      if( ISCHAR( 6 ) )
      {
         bRet = hb_UnzipOne( hb___CheckFile( szFile ),
                             hb_param( 2, HB_IT_BLOCK ),
                             ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                             ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                             ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                             hb_parc( 6 ),
                             &iProgress);
      }
      else if( ISARRAY( 6 ) )
      {
         bRet = hb_UnzipSel( hb___CheckFile( szFile ),
                             hb_param( 2, HB_IT_BLOCK ),
                             ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                             ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                             ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                             hb_param( 6, HB_IT_ARRAY ),
                             &iProgress );
      }
      else
      {
         bRet = hb_UnzipAll( hb___CheckFile( szFile ),
                             hb_param( 2, HB_IT_BLOCK ),
                             ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                             ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                             ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                             &iProgress);
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
      char szFile[ _POSIX_PATH_MAX ];
      strcpy( szFile, hb_parc( 1 ) );

      if( ISCHAR( 2 ) )
         bRet = hb_DeleteOne( hb___CheckFile( szFile ),
                              hb_parc( 2 ) );
      else if( ISARRAY( 2 ) )
         bRet = hb_DeleteSel( hb___CheckFile( szFile ),
                              hb_param( 2, HB_IT_ARRAY ),
                              ISLOG( 3 ) ? hb_parl( 3 ) : 0 );
      else if( ISNUM( 2 ) )
         bRet = hb_DeleteOneIndex( hb___CheckFile( szFile ),
                                   hb_parni( 2 ) );
   }

   hb_retl( bRet );
}

HB_FUNC( HB_ZIPTESTPK )
{
   char szFile[ _POSIX_PATH_MAX ];
   strcpy( szFile, hb_parc( 1 ) );

   hb_retni( hb_TestForPKS( hb___CheckFile( szFile ) ) );
}

HB_FUNC( HB_SETBUFFER )
{
   hb_SetZipBuff( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ));
}

HB_FUNC( HB_SETZIPCOMMENT )
{
   hb_SetZipComment( hb_parc( 1 ) );
}

HB_FUNC( HB_GETZIPCOMMENT )
{
   char *szComment=( char* )hb_GetZipComment( hb_parc( 1 ) );
   hb_retcAdopt( szComment );
}

HB_FUNC( HB_UNZIPFILEINDEX )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      char szFile[ _POSIX_PATH_MAX ];
      PHB_ITEM pProgress = hb_param( 7, HB_IT_BLOCK );
      HB_ITEM iProgress;

      iProgress.type = HB_IT_NIL;

      if( pProgress )
      {
        hb_itemCopy( &iProgress, pProgress );
      }

      strcpy( szFile, hb_parc( 1 ) );

      if( ISNUM( 6 ) )
      {
         bRet = hb_UnzipOneIndex( hb___CheckFile( szFile ),
                                  hb_param( 2, HB_IT_BLOCK ),
                                  ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                                  ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                                  hb_parc( 5 ),
                                  hb_parni( 6 ),
                                  &iProgress );
      }
      else if( ISARRAY( 6 ) )
      {
         bRet = hb_UnzipSelIndex( hb___CheckFile( szFile ),
                                  hb_param( 2, HB_IT_BLOCK ),
                                  ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                                  ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                                  hb_parc( 5 ),
                                  hb_param( 6, HB_IT_ARRAY ),
                                  &iProgress );
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

HB_FUNC(SETZIPREADONLY)
{
   hb_SetZipReadOnly( hb_parl( 1 ) );
}
