/*
 * $Id: zip.c,v 1.20 2004/02/29 17:09:41 andijahja Exp $
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
static HB_ITEM ExcludeFile;
static HB_ITEM UnzipFiles;
static HB_ITEM DelZip;
static HB_ITEM FileAttribs;

#define FA_RDONLY           1   /* R */
#define FA_HIDDEN           2   /* H */
#define FA_SYSTEM           4   /* S */
#define FA_LABEL            8   /* V */
#define FA_DIREC           16   /* D */
#define FA_ARCH            32   /* A */
#define FA_NORMAL         128

static BOOL ZipAttribute( char *szFile, int iAttr )
{
   DWORD dwFlags=FILE_ATTRIBUTE_ARCHIVE;
   BOOL lSuccess;

   if(  iAttr & FA_RDONLY  )
      dwFlags |= FILE_ATTRIBUTE_READONLY;

   if(  iAttr & FA_HIDDEN  )
      dwFlags |= FILE_ATTRIBUTE_HIDDEN;

   if(  iAttr & FA_SYSTEM  )
      dwFlags |= FILE_ATTRIBUTE_SYSTEM;

   if(  iAttr & FA_NORMAL  )
      dwFlags |=    FILE_ATTRIBUTE_NORMAL;

   lSuccess=SetFileAttributes( szFile, dwFlags );

   return lSuccess;
}

static void ResetAttribs()
{
   ULONG ulAtt, ulZipLen = FileToZip.item.asArray.value->ulLen;
   char *szFile;
   int iAttr;

   for( ulAtt = 0; ulAtt < ulZipLen; ulAtt ++ )
   {
     szFile = hb_arrayGetC( &FileToZip, ulAtt + 1 );
     iAttr  = hb_arrayGetNI( &FileAttribs, ulAtt + 1 );
     ZipAttribute( szFile, iAttr );
     hb_xfree( szFile );
   }

   hb_itemClear( &FileAttribs );
   hb_itemClear( &FileToZip );
}

static void UnzipCreateArray( char *szZipFileName, char *szSkleton, int uiOption )
{
   int ul;
   char * szEntry;
   PHB_ITEM pZipEntry;
   HB_ITEM Temp;
   BOOL bOkAdd;
   int ulLen = ZipArray.item.asArray.value->ulLen;

   Temp.type = HB_IT_NIL;

   for ( ul = 0 ; ul < ulLen; ul ++ )
   {
      bOkAdd = TRUE;
      pZipEntry = hb_arrayGetItemPtr( &ZipArray, ul + 1 );
      szEntry = hb_arrayGetC( pZipEntry, 1 );

      if ( szSkleton )
      {
         bOkAdd = hb_strMatchRegExp( (const char *) szEntry, (const char *) szSkleton );
      }

      if ( bOkAdd )
      {
         if ( uiOption == 1 )
         {
            hb_arrayAddForward( &UnzipFiles, hb_itemPutC( &Temp, szEntry ) );
         }
         else
         {
            hb_arrayAddForward( &DelZip, hb_itemPutC( &Temp, szEntry ) );
         }
      }

      hb_xfree( szEntry );
   }
}

static BOOL ZipTestExclude ( char *szEntry )
{
   int uiEx;
   BOOL bNotFound = TRUE;
   int uiExLen = ExcludeFile.item.asArray.value->ulLen;

   for ( uiEx = 0; uiEx < uiExLen; uiEx ++ )
   {
      char *szExclude = hb_arrayGetC( &ExcludeFile, uiEx + 1 );
      if ( strcmp ( szExclude, hb_strupr( szEntry ) ) == NULL )
      {
         hb_xfree( szExclude );
         bNotFound = FALSE;
         break;
      }
      hb_xfree( szExclude );
   }

   return ( bNotFound );
}

static void ZipCreateExclude( PHB_ITEM pExclude )
{
   HB_ITEM ExTmp;

   ExTmp.type = HB_IT_NIL;
   ExcludeFile.type = HB_IT_NIL;

   hb_arrayNew( &ExcludeFile, 0 );

   if( pExclude == NULL )
   {
      return;
   }

   if ( HB_IS_STRING( pExclude ) )
   {
      if ( pExclude->item.asString.length == 0 )
      {
         return;
      }

      if ( strchr( pExclude->item.asString.value, '*') != NULL )
      {
         HB_ITEM WildFile;
         PHB_ITEM pDirEntry;
         int uiLen;
         int ui;

         WildFile.type = HB_IT_NIL;
         hb_fsDirectory( &WildFile, pExclude->item.asString.value,NULL,NULL,TRUE);
         uiLen = WildFile.item.asArray.value->ulLen;

         for ( ui = 0 ; ui < uiLen; ui ++ )
         {
            char * szEntry;
            pDirEntry = hb_arrayGetItemPtr( &WildFile, ui + 1 );
            szEntry = hb_arrayGetC( pDirEntry, 1 );

            if( szEntry )
            {
               hb_arrayAddForward( &ExcludeFile, hb_itemPutC( &ExTmp, hb_strupr( szEntry ) ) );
               hb_xfree( szEntry );
            }
         }
      }
      else
      {
         hb_arrayAddForward( &ExcludeFile, hb_itemPutC( &ExTmp, pExclude->item.asString.value ) );
      }
   }
   else if ( HB_IS_ARRAY( pExclude ) )
   {
      int ux;
      int ufx = pExclude->item.asArray.value->ulLen;
      char * szExclude;
      HB_ITEM WildFile;
      PHB_ITEM pDirEntry;

      WildFile.type = HB_IT_NIL;

      if ( ufx == 0 )
      {
         return;
      }

      for ( ux = 0 ; ux < ufx ; ux ++  )
      {
         szExclude = hb_arrayGetC( pExclude, ux + 1 );

         if( szExclude )
         {
            if ( strchr( szExclude, '*') != NULL )
            {
               int uiW, uiWLen;
               char *szEntry;

               hb_fsDirectory(&WildFile,szExclude,NULL,NULL,TRUE);
               uiWLen = WildFile.item.asArray.value->ulLen;

               for ( uiW = 0; uiW < uiWLen; uiW ++ )
               {
                  pDirEntry = hb_arrayGetItemPtr( &WildFile, uiW + 1 );
                  szEntry = hb_arrayGetC( pDirEntry, 1 );
                  hb_arrayAddForward( &ExcludeFile, hb_itemPutC( &ExTmp, szEntry ));
                  hb_xfree( szEntry );
               }
            }
            else
            {
               hb_arrayAddForward( &ExcludeFile, hb_itemPutC( &ExTmp, szExclude ));
            }

            hb_xfree( szExclude );
         }
      }
   }
}

static void ZipCreateArray( PHB_ITEM pParam )
{
   PHB_ITEM pDirEntry;
   HB_ITEM Temp, TempArray, WildFile;
   int ul, ulLen, ulArr, ulLenArr;

   WildFile.type = HB_IT_NIL;
   FileAttribs.type = HB_IT_NIL;
   FileToZip.type = HB_IT_NIL;
   Temp.type = HB_IT_NIL;
   TempArray.type = HB_IT_NIL;

   hb_arrayNew( &FileAttribs, 0 );
   hb_arrayNew( &TempArray, 0 );

   if( pParam->type == HB_IT_STRING )
   {
      hb_arrayAddForward( &TempArray, hb_itemPutC( &Temp, pParam->item.asString.value ) );
   }
   else
   {
      PHB_ITEM pClone = hb_arrayClone( pParam, NULL );
      hb_itemCopy( &TempArray, pClone );
      hb_itemRelease( pClone );
   }

   ulLenArr = (&TempArray)->item.asArray.value->ulLen;

   hb_arrayNew( &FileToZip, 0 );

   for ( ulArr = 0; ulArr < ulLenArr ; ulArr ++ )
   {
      char *szArrEntry = hb_arrayGetC( &TempArray, ulArr + 1 );

      if ( szArrEntry )
      {
         if ( strchr( szArrEntry, '*') != NULL )
         {
            hb_fsDirectory(&WildFile,szArrEntry,NULL,NULL,TRUE);
            ulLen = WildFile.item.asArray.value->ulLen;

            for ( ul = 0; ul < ulLen ; ul ++ )
            {
               char * szEntry;
               pDirEntry = hb_arrayGetItemPtr( &WildFile, ul + 1 );
               szEntry = hb_arrayGetC( pDirEntry, 1 );

               if ( ZipTestExclude ( szEntry ) )
               {
                  hb_arrayAddForward( &FileToZip, hb_itemPutC( &Temp, szEntry ) );
                  hb_arrayAddForward( &FileAttribs, hb_itemPutNI( &Temp, GetFileAttributes( szEntry ) ) );
                  ZipAttribute( szEntry, FA_ARCH );
               }

               hb_xfree( szEntry );
            }
         }
         else
         {
            hb_arrayAddForward( &FileToZip, hb_itemPutC( &Temp, szArrEntry ) );
            hb_arrayAddForward( &FileAttribs, hb_itemPutNI( &Temp, GetFileAttributes( szArrEntry ) ) );
            ZipAttribute( szArrEntry, FA_ARCH );
         }

         hb_xfree( szArrEntry );
      }
   }

   hb_itemClear( &TempArray );
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
         PHB_ITEM pExclude = hb_param( 10, HB_IT_STRING | HB_IT_ARRAY );
         HB_ITEM iProgress;
         char *szZipFileName;

         iProgress.type = HB_IT_NIL;

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam );

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

         strcpy( szFile, hb_parc( 1 ) );
         szZipFileName = hb___CheckFile( szFile );

         if ( FileToZip.item.asArray.value->ulLen > 0 )
         {
            bRet = hb_CompressFile( szZipFileName,
                                    &FileToZip,
                                    ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                    hb_param( 4, HB_IT_BLOCK ),
                                    ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                    ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                    ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                    ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                    &iProgress );

            ResetAttribs();
         }

         hb_xfree( szZipFileName );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_GETFILESINZIP )
{
   if( ISCHAR( 1 ) )
   {
      char szFile[ _POSIX_PATH_MAX ];
      char *szZipFileName;
      strcpy( szFile, hb_parc( 1 ) );

      szZipFileName = hb___CheckFile( szFile );

      hb___GetFileNamesFromZip( szZipFileName,
                                ISLOG( 2 ) ? hb_parl( 2 ) : 0 );

      hb_itemReturn( &ZipArray );
      hb_xfree( szZipFileName );
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
      char *szZipFileName;
      strcpy( szFile, hb_parc( 1 ) );

      szZipFileName = hb___CheckFile( szFile );

      iRet = hb___GetNumberofFilestoUnzip( szZipFileName );

      hb_xfree( szZipFileName );
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
         PHB_ITEM pExclude = hb_param( 11, HB_IT_STRING | HB_IT_ARRAY );
         HB_ITEM iProgress;
         char *szZipFileName;

         iProgress.type = HB_IT_NIL;

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam );

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

         strcpy( szFile, hb_parc( 1 ) );
         szZipFileName = hb___CheckFile( szFile );

         if ( FileToZip.item.asArray.value->ulLen > 0 )
         {
            bRet = hb_CmpTdSpan( szZipFileName,
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

         hb_xfree( szZipFileName );
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
         PHB_ITEM pExclude = hb_param( 10, HB_IT_STRING | HB_IT_ARRAY );
         HB_ITEM iProgress;
         char *szZipFileName;

         iProgress.type = HB_IT_NIL;

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam );

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

         strcpy( szFile, hb_parc( 1 ) );
         szZipFileName = hb___CheckFile( szFile );

         if ( FileToZip.item.asArray.value->ulLen > 0 )
         {
            bRet = hb_CmpPkSpan( szZipFileName,
                                 &FileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 hb_param( 4, HB_IT_BLOCK ),
                                 ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                 &iProgress );
         }

         hb_xfree( szZipFileName );
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
      PHB_ITEM pUnzip = hb_param( 6, HB_IT_ANY );
      HB_ITEM iProgress, Temp;
      char *szZipFileName;

      Temp.type = HB_IT_NIL;
      iProgress.type = HB_IT_NIL;

      if( pProgress )
      {
        hb_itemCopy( &iProgress, pProgress );
      }

      strcpy( szFile, hb_parc( 1 ) );
      szZipFileName = hb___CheckFile( szFile );

      UnzipFiles.type = HB_IT_NIL;
      hb_arrayNew( &UnzipFiles, 0 );

      hb___GetFileNamesFromZip( szZipFileName, TRUE );

      if( pUnzip )
      {
         if( HB_IS_STRING( pUnzip ) )
         {
            UnzipCreateArray( szZipFileName, pUnzip->item.asString.value, 1 );
         }
         else if( HB_IS_ARRAY( pUnzip ) )
         {
            int uiZ, uiZLen = pUnzip->item.asArray.value->ulLen;
            char *szUnzip;
            HB_ITEM Temp;

            Temp.type = HB_IT_NIL;

            for ( uiZ = 0; uiZ < uiZLen; uiZ ++ )
            {
               szUnzip = hb_arrayGetC( pUnzip, uiZ + 1 );

               if ( szUnzip )
               {
                  UnzipCreateArray( szZipFileName, szUnzip, 1 );
                  hb_xfree( szUnzip );
               }
            }
         }
         else
         {
            hb_xfree( szZipFileName );
            hb_itemClear( &UnzipFiles );
            hb_retl( bRet );
            return;
         }
      }
      else
      {
         UnzipCreateArray( szZipFileName, (char*) NULL, 1 );
      }

      if ( UnzipFiles.item.asArray.value->ulLen > 0 )
      {
         bRet = hb_UnzipSel( szZipFileName,
                             hb_param( 2, HB_IT_BLOCK ),
                             ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                             ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                             ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                             &UnzipFiles,
                             &iProgress );
      }

      hb_xfree( szZipFileName );
      hb_itemClear( &UnzipFiles );
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

      DelZip.type = HB_IT_NIL;
      hb_arrayNew( &DelZip, 0 );

      if ( pDelZip )
      {
         char szFile[ _POSIX_PATH_MAX ];
         char *szZipFileName;
         int ulLen;

         strcpy( szFile, hb_parc( 1 ) );
         szZipFileName = hb___CheckFile( szFile );

         hb___GetFileNamesFromZip( szZipFileName, TRUE );
         ulLen = ZipArray.item.asArray.value->ulLen;

         if ( !ulLen )
         {
            hb_xfree( szZipFileName );
            hb_retl ( bRet );
            return;
         }

         if ( HB_IS_STRING( pDelZip ) )
         {
            if ( pDelZip->item.asString.length > 0 )
            {
               UnzipCreateArray( szZipFileName, pDelZip->item.asString.value, 2 );
            }
         }
         else if ( HB_IS_ARRAY( pDelZip ) )
         {
            int uiInLen = pDelZip->item.asArray.value->ulLen;

            if ( uiInLen > 0 )
            {
               int uiIn;
               char *szInput;

               for ( uiIn = 0; uiIn < uiInLen; uiIn ++ )
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
         else if ( HB_IS_NUMERIC( pDelZip ) )
         {
            int iIndex = hb_itemGetNI( pDelZip );
            HB_ITEM Temp;
            Temp.type = HB_IT_NIL;

            if( iIndex > 0 && iIndex <= ulLen )
            {
               PHB_ITEM pZipEntry = hb_arrayGetItemPtr( &ZipArray, iIndex );
               char* szEntry = hb_arrayGetC( pZipEntry, 1 );
               hb_arrayAddForward( &DelZip, hb_itemPutC( &Temp, szEntry ) );
               hb_xfree( szEntry );
               hb_itemClear( &Temp );
            }
         }

         if ( DelZip.item.asArray.value->ulLen > 0 )
         {
            bRet = hb_DeleteSel( szZipFileName,
                                 &DelZip,
                                 ISLOG( 3 ) ? hb_parl( 3 ) : 0 );
         }

         hb_xfree(szZipFileName);
      }

      hb_itemClear( &DelZip );
   }

   hb_retl( bRet );
}

HB_FUNC( HB_ZIPTESTPK )
{
   char szFile[ _POSIX_PATH_MAX ];
   char *szZipFileName;

   strcpy( szFile, hb_parc( 1 ) );
   szZipFileName = hb___CheckFile( szFile );

   hb_retni( hb_TestForPKS( szZipFileName ) );

   hb_xfree(szZipFileName);
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
      PHB_ITEM pDelZip = hb_param( 6, HB_IT_NUMERIC | HB_IT_ARRAY );

      if ( pDelZip )
      {
         char szFile[ _POSIX_PATH_MAX ];
         PHB_ITEM pProgress = hb_param( 7, HB_IT_BLOCK );
         HB_ITEM iProgress, DelZip, Temp;
         char* szZipFileName;
         int ulLen;

         Temp.type = HB_IT_NIL;
         DelZip.type = HB_IT_NIL;
         iProgress.type = HB_IT_NIL;

         hb_arrayNew( &DelZip, 0 );

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

         strcpy( szFile, hb_parc( 1 ) );
         szZipFileName = hb___CheckFile( szFile );

         hb___GetFileNamesFromZip( szZipFileName, TRUE );
         ulLen = ZipArray.item.asArray.value->ulLen;

         if ( HB_IS_NUMERIC ( pDelZip ) )
         {
            int iIndex = hb_itemGetNI( pDelZip );

            if ( iIndex > 0 && iIndex <= ulLen )
            {
               hb_arrayAddForward( &DelZip, hb_itemPutNI( &Temp, iIndex ) );
            }
         }
         else
         {
            int ui, iIndex;

            for ( ui = 0 ; ui < ulLen; ui ++ )
            {
               iIndex = hb_arrayGetNI( pDelZip, ui + 1 );
               if ( iIndex && iIndex > 0 && iIndex <= ulLen )
               {
                  hb_arrayAddForward( &DelZip, hb_itemPutNI( &Temp, iIndex ) );
               }
            }

         }

         if( DelZip.item.asArray.value->ulLen > 0 )
         {
            bRet = hb_UnzipSelIndex( szZipFileName,
                                     hb_param( 2, HB_IT_BLOCK ),
                                     ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                                     ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                                     hb_parc( 5 ),
                                     &DelZip,
                                     &iProgress );
         }

         hb_xfree( szZipFileName );

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
