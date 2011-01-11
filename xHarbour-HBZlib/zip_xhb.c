/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Luiz Rafael Culik Guimaraes culikr@brturbo.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/

#define HB_APIZLIB_H_
#include "hbzip.h"
//#include "tchar.h"
#include "time.h"

/* Xharbour CallAble functions */

HB_FUNC( HB_ZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      char szFile[ HB_PATH_MAX - 1 ] = { 0 };

      strcpy( szFile, hb_parc( 1 ) );

      if( ISCHAR( 2 ) )
      {
         bRet = hb_CompressFileStd( hb___CheckFile( szFile ),
                                    hb_parc( 2 ),
                                    ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                    ISBLOCK( 4 ) ? hb_param( 4, HB_IT_BLOCK ) : NULL,
                                    ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                    ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                    ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                    ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                    ISBLOCK( 9 ) ? hb_itemNew( hb_param( 9 , HB_IT_BLOCK )) : NULL,
                                    ISCHAR( 10 ) ? hb_parc( 10 ) : NULL );
      }
      else if( ISARRAY( 2 ) )
      {
         bRet = hb_CompressFile( hb___CheckFile( szFile ),
                                 hb_param( 2, HB_IT_ARRAY ),
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 ISBLOCK( 4 ) ? hb_param( 4, HB_IT_BLOCK ) : NULL,
                                 ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                 ISBLOCK( 9 ) ? hb_itemNew( hb_param( 9 , HB_IT_BLOCK ) ) : NULL,
                                 ISCHAR( 10 ) ? hb_parc( 10 ) : NULL );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_ZIPFILEBYTDSPAN )
{

   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {

      char szFile[ HB_PATH_MAX - 1 ] = { 0 } ;
      strcpy( szFile, hb_parc( 1 ) );

      if( ISCHAR( 2 ) )
      {

         bRet = hb_CmpTdSpanStd( hb___CheckFile( szFile ),
                                 hb_parc( 2 ),
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 ISBLOCK( 4 ) ? hb_param( 4, HB_IT_BLOCK ) : NULL,
                                 ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISNUM( 7 ) ? hb_parni( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                 ISLOG( 9 ) ? hb_parl( 9 ) : 0,
                                 ISBLOCK( 10 ) ? hb_itemNew( hb_param( 10 , HB_IT_BLOCK ) ) : NULL,
                                 ISCHAR( 11 ) ? hb_parc( 11 ) : NULL );
      }
      else if( ISARRAY( 2 ) )
      {
         bRet = hb_CmpTdSpan( hb___CheckFile( szFile ),
                              hb_param( 2, HB_IT_ARRAY ),
                              ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                              ISBLOCK( 4 ) ? hb_param( 4, HB_IT_BLOCK ) : NULL,
                              ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                              ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                              ISNUM( 7 ) ? hb_parni( 7 ) : 0,
                              ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                              ISLOG( 9 ) ? hb_parl( 9 ) : 0,
                              ISBLOCK( 10 ) ? hb_itemNew( hb_param( 10 , HB_IT_BLOCK ) ) : NULL,
                              ISCHAR( 11 ) ? hb_parc( 11 ) : NULL );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_ZIPFILEBYPKSPAN )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      char szFile[ HB_PATH_MAX - 1 ] = { 0 } ;
      strcpy( szFile, hb_parc( 1 ) );

      if( ISCHAR( 2 ) )
      {
         bRet = hb_CmpPkSpanStd( hb___CheckFile( szFile ),
                                 hb_parc( 2 ),
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 ISBLOCK( 4 ) ? hb_param( 4, HB_IT_BLOCK ) : NULL,
                                 ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                 ISBLOCK( 9 ) ? hb_itemNew( hb_param( 9 , HB_IT_BLOCK ) ) : NULL,
                                 ISCHAR( 10) ? hb_parc( 10) : NULL);
      }
      else if( ISARRAY( 2 ) )
      {
         bRet = hb_CmpPkSpan( hb___CheckFile( szFile ),
                              hb_param( 2, HB_IT_ARRAY ),
                              ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                              hb_param( 4, HB_IT_BLOCK ),
                              ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                              ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                              ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                              ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                              ISBLOCK( 9 ) ? hb_itemNew( hb_param( 9, HB_IT_BLOCK ) ) : NULL,
                              ISCHAR( 10) ? hb_parc( 10) : NULL);

      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_ZIPWITHPASSWORD )
{
   hb_retl( hb_IsPassWord( (char * ) hb_parc( 1 ) ) );
}

HB_FUNC( HB_GETFILECOUNT )
{
   int iRet = 0;

   if( ISCHAR( 1 ) )
   {
      char szFile[ HB_PATH_MAX - 1 ] = { 0 } ;
      strcpy( szFile, hb_parc( 1 ) );

      iRet = hb___GetNumberofFilestoUnzip( hb___CheckFile( szFile ) );
   }

   hb_retni( iRet );
}

HB_FUNC( HB_GETFILESINZIP )
{
   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pArray;
      char szFile[ HB_PATH_MAX - 1 ] = { 0 } ;
      strcpy( szFile, hb_parc( 1 ) );

      pArray = hb___GetFileNamesFromZip( hb___CheckFile( szFile ),
                                ISLOG( 2 ) ? hb_parl( 2 ) : 0 );
      hb_itemReturnForward( pArray );
//      hb_itemReturn( pArray );
      hb_itemRelease( pArray );
   }
}


HB_FUNC( HB_UNZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pProgress = hb_param( 7, HB_IT_BLOCK );
      PHB_ITEM  iProgress;
      BYTE *pCurDir;

      char szFile[ HB_PATH_MAX - 1 ] = { 0 } ;


      pCurDir = ( BYTE * )hb_xstrcpy( NULL, HB_OS_PATH_DELIM_CHR_STRING, ( const char * )hb_fsCurDir( 0 ) , NULL );

      if( pProgress )
      {
        //hb_itemCopy( &iProgress, pProgress );
        iProgress=hb_itemNew( pProgress);
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
                             iProgress);
      }
      else if( ISARRAY( 6 ) )
      {
         bRet = hb_UnzipSel( hb___CheckFile( szFile ),
                             hb_param( 2, HB_IT_BLOCK ),
                             ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                             ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                             ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                             hb_param( 6, HB_IT_ARRAY ),
                             iProgress);
      }
      else
      {
         bRet = hb_UnzipAll( hb___CheckFile( szFile ),
                             hb_param( 2, HB_IT_BLOCK ),
                             ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                             ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                             ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                             iProgress);
      }

      hb_fsChDir( (const char *) pCurDir ) ;
      hb_xfree( pCurDir ) ;
//      hb_itemClear( &iProgress );
   }


   hb_retl( bRet );
}

HB_FUNC( HB_SETDISKZIP )
{
   hb_retl( hb___SetCallbackFunc( hb_itemParam( 1 ) ) );
}


/* Xharbour interface between Xharbour Level and ZipArchive level */

/* Standart Zip */

PHB_ITEM pDiskCallBackBlock = NULL ;

int GetZipMode(char * szFile)
{
   return hb_CheckSpanMode( szFile );
}

int hb_CompressFile( char *szFile, PHB_ITEM pArray, int iCompLevel,
                     PHB_ITEM pBlock, BOOL bOverWrite, const char *szPassWord,
                     BOOL bPath, BOOL bDrive, PHB_ITEM pProgress,
                     const char * szComment )
{
   ULONG ulCount;
   char *szFileName;
   BOOL bFileExist = hb_fsFile( (const char *) szFile );
   BOOL bReturn    = TRUE;

   void * pZip;


   pZip = ZipAchive_New( szFile, ( char * ) szPassWord, bOverWrite, bFileExist );

   if ( ! pZip )
   {
      hb_retl( FALSE );
   }
   else
   {
      bReturn = TRUE ;
   }

   if ( bReturn )
   {
      if ( szComment != NULL )
      {
         ZipAchive_SetGlobalcomment( pZip, szComment) ;
      }

      if ( pProgress  )
      {
         ZipArchive_SetActCallBack( pZip, ActCallback, ( void * ) pProgress);
      }

      for ( ulCount = 1; ( ulCount <= hb_arrayLen( pArray ) ) ;ulCount++ )
      {
         szFileName = ( char * ) hb_arrayGetCPtr( pArray, ulCount ) ;

         if( pBlock )
         {
            PHB_ITEM FileName  = hb_itemPutC(  NULL, hb_arrayGetCPtr( pArray, ulCount ) );
            PHB_ITEM FilePos   = hb_itemPutNI( NULL, ulCount );

            hb_vmEvalBlockV( pBlock, 2, FileName, FilePos );

            hb_itemRelease( FileName );
            hb_itemRelease( FilePos );
         }

            ZipArchive_AddnNewFile(  pZip , szFileName, bPath, bDrive, iCompLevel);

      }
   }

   bReturn = ZipArchive_Close( pZip );

   if ( pProgress )
   {
      hb_itemRelease( pProgress );
   }


   return ( int ) bReturn;
}


int  hb_CompressFileStd( char *szFile, const char *szFiletoCompress,int iCompLevel,
                         PHB_ITEM pBlock,BOOL bOverWrite, const char *szPassWord,
                         BOOL bPath, BOOL bDrive, PHB_ITEM pProgress,
                         const char * szComment )
{
   BOOL bFileExist = hb_fsFile( (const char *) szFile );
   BOOL bReturn    = TRUE;

   void * pZip;


   pZip = ZipAchive_New( szFile, ( char * ) szPassWord, bOverWrite, bFileExist );

   if ( ! pZip )
      hb_retl( FALSE );
   else
      bReturn = TRUE ;


   if ( bReturn )
   {

      if ( szComment != NULL )
      {
         ZipAchive_SetGlobalcomment( pZip, szComment) ;
      }

      if ( pProgress )
      {
         ZipArchive_SetActCallBack( pZip, ActCallback, ( void * ) pProgress);
      }

      if( pBlock  != NULL )
      {

         PHB_ITEM FileName = hb_itemPutC( NULL, szFiletoCompress ) ;

         hb_vmEvalBlockV( pBlock, 1, FileName );
         hb_itemRelease( FileName );
      }

      ZipArchive_AddnNewFile(  pZip , ( char * ) szFiletoCompress, bPath, bDrive, iCompLevel);


   }
   bReturn = ZipArchive_Close( pZip );

   if ( pProgress )
   {
      hb_itemRelease( pProgress );
   }

   return ( int ) bReturn;
}

/* TDSpan Zips */
int  hb_CmpTdSpan( char *szFile, PHB_ITEM pArray, int iCompLevel,
                   PHB_ITEM pBlock, BOOL bOverWrite, const char *szPassWord,
                   int iSpanSize, BOOL bPath, BOOL bDrive,
                   PHB_ITEM pProgress ,const char * szComment)
{
   ULONG ulCount;
   char * szFileName;

   BOOL bReturn    = TRUE;
   BOOL bFileExist = hb_fsFile( (const char *) szFile );
   void * pZip;

   if ( iSpanSize  == 0 )
   {
      iSpanSize = 1457664;
   }


   pZip = ZipAchive_NewTdSpan( szFile, ( char * ) szPassWord, bOverWrite, bFileExist, iSpanSize );

   if ( ! pZip )
      hb_retl( FALSE );
   else
      bReturn = TRUE ;


   if ( bReturn )
   {

      if ( szComment != NULL )
      {
         ZipAchive_SetGlobalcomment( pZip, szComment) ;
      }

      if ( HB_IS_BLOCK( pProgress ) )
      {
         ZipArchive_SetActCallBack( pZip, ActCallback, ( void * ) pProgress);
      }

      for ( ulCount = 1;( ulCount<= hb_arrayLen( pArray ) ) ;ulCount++ )
      {
         szFileName     = ( char * )hb_arrayGetCPtr( pArray, ulCount ) ;

         if( pBlock != NULL )
         {
            PHB_ITEM FileName  = hb_itemPutC(  NULL, hb_arrayGetCPtr( pArray, ulCount ) );
            PHB_ITEM FilePos   = hb_itemPutNI( NULL, ulCount );

            hb_vmEvalBlockV( pBlock, 2, FileName, FilePos );

            hb_itemRelease( FileName );
            hb_itemRelease( FilePos );

         }

         ZipArchive_AddnNewFile(  pZip , szFileName, bPath, bDrive, iCompLevel);


      } //for

      bReturn = ZipArchive_Close( pZip );
   }

   if ( pProgress )
   {
      hb_itemRelease( pProgress );
   }

   return ( int ) bReturn;
}

int hb_CmpTdSpanStd( char *szFile, const char * szFiletoCompress, int iCompLevel,
                     PHB_ITEM pBlock, BOOL bOverWrite, const char *szPassWord,
                     int iSpanSize, BOOL bPath,
                     BOOL bDrive, PHB_ITEM pProgress ,const char * szComment)
{
   BOOL bReturn    = TRUE;
   BOOL bFileExist = hb_fsFile( (const char *) szFile );
   void * pZip;

   if ( iSpanSize  == 0 )
   {
      iSpanSize = 1457664;
   }

   pZip = ZipAchive_NewTdSpan( szFile, ( char * ) szPassWord, bOverWrite, bFileExist, iSpanSize );

   if ( ! pZip )
      hb_retl( FALSE );
   else
      bReturn = TRUE ;

   if ( bReturn )
   {
      if ( szComment != NULL )
      {
         ZipAchive_SetGlobalcomment( pZip, szComment) ;
      }

      if ( pProgress )
      {
         ZipArchive_SetActCallBack( pZip, ActCallback, ( void * ) pProgress);
      }

      if( pBlock  != NULL )
      {
         PHB_ITEM FileName  = hb_itemPutC( NULL, szFiletoCompress  )  ;

         hb_vmEvalBlockV(  pBlock, 1, FileName );

         hb_itemRelease( FileName );
      }

      ZipArchive_AddnNewFile(  pZip , ( char * ) szFiletoCompress, bPath, bDrive, iCompLevel);

      bReturn = ZipArchive_Close( pZip );
   }

   if ( pProgress )
   {
      hb_itemRelease( pProgress );
   }


   return ( int ) bReturn;
}


/* PkSpan Functions */

int hb_CmpPkSpan( char *szFile, PHB_ITEM pArray, int iCompLevel,
                  PHB_ITEM pBlock, BOOL bOverWrite, const char *szPassWord,
                  BOOL bPath, BOOL bDrive, PHB_ITEM pProgress,
                  const char * szComment)
{
   ULONG ulCount;

   char *szFileName;

   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFile( (const char *) szFile );

   void * pZip;
   void * sDiskZip = NULL;

   if ( pDiskCallBackBlock  )
   {
       sDiskZip = SpanCallbackc_New( DiskCallBack, ( void * ) pDiskCallBackBlock );
   }


   pZip = ZipAchive_NewPkSpan( szFile, ( char * ) szPassWord, bOverWrite, bFileExist, sDiskZip );

   if ( ! pZip )
      hb_retl( FALSE );
   else
      bReturn = TRUE ;


   if ( ! bReturn )
   {
      return ( int ) bReturn;
   }


   if ( szComment != NULL )
   {
      ZipAchive_SetGlobalcomment( pZip, szComment ) ;
   }

   if ( pProgress )
   {
      ZipArchive_SetActCallBack( pZip, ActCallback, ( void * ) pProgress);
   }

   for( ulCount = 1;( ulCount <= hb_arrayLen( pArray ) ); ulCount++ )
   {
      szFileName = ( char * )hb_arrayGetCPtr( pArray, ulCount );

      if( pBlock  !=  NULL )
      {

         PHB_ITEM FileName  = hb_itemPutC(  NULL, hb_arrayGetCPtr( pArray, ulCount ) );
         PHB_ITEM FilePos   = hb_itemPutNI( NULL, ulCount );

         hb_vmEvalBlockV( pBlock, 2, FileName, FilePos );

         hb_itemRelease( FileName );
         hb_itemRelease( FilePos );
      }

      ZipArchive_AddnNewFile(  pZip , szFileName, bPath, bDrive, iCompLevel);

   }

   bReturn = ZipArchive_Close( pZip );

   if ( pProgress )
   {
      hb_itemRelease( pProgress ) ;
   }


   if( sDiskZip )
   {
      SpanCallbackc_End( &sDiskZip );
   }

   return ( int ) bReturn;
}

int hb_CmpPkSpanStd( char *szFile, const char *szFiletoCompress, int iCompLevel,
                     PHB_ITEM pBlock, BOOL bOverWrite, const char *szPassWord,
                     BOOL bPath, BOOL bDrive, PHB_ITEM pProgress,
                     const char * szComment)
{
   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFile( (const char *) szFile );

   void * pZip;
   void * sDiskZip = NULL;

   if ( pDiskCallBackBlock )
   {
      sDiskZip = SpanCallbackc_New( DiskCallBack,  ( void * ) pDiskCallBackBlock );
   }


   pZip = ZipAchive_NewPkSpan( szFile, ( char * ) szPassWord, bOverWrite, bFileExist, sDiskZip );

   if ( ! pZip )
      hb_retl( FALSE );
   else
      bReturn = TRUE ;

   if ( ! bReturn )
   {
      return ( int ) bReturn;
   }

   if ( szComment != NULL )
   {
      ZipAchive_SetGlobalcomment( pZip, szComment ) ;
   }

   if ( pProgress )
   {
      ZipArchive_SetActCallBack( pZip, ActCallback, ( void * ) pProgress);
   }

   if( pBlock  !=  NULL )
   {
       PHB_ITEM FileName = hb_itemPutC( NULL, szFiletoCompress )  ;
       hb_vmEvalBlockV( pBlock, 1, FileName );
       hb_itemRelease( FileName );
   }

   ZipArchive_AddnNewFile(  pZip, ( char * ) szFiletoCompress, bPath, bDrive, iCompLevel);

   bReturn = ZipArchive_Close( pZip );

   if ( pProgress )
   {
      hb_itemRelease( pProgress ) ;
   }

   if ( pDiskCallBackBlock )
   {
      hb_itemRelease( pDiskCallBackBlock );
   }

   if ( sDiskZip )
      SpanCallbackc_End( &sDiskZip );

   return ( int ) bReturn;
}

BOOL hb_IsPassWord( char *szFile )
{
   void * sDiskZip = NULL ;
   BOOL bReturn ;

   if ( pDiskCallBackBlock )
   {
       sDiskZip = SpanCallbackc_New( DiskCallBack, ( void * ) pDiskCallBackBlock );
   }

   bReturn = ZipArchive_WithPassWord( szFile, sDiskZip ) ;

   if ( pDiskCallBackBlock )
   {
      hb_itemRelease( pDiskCallBackBlock ) ;
   }

   return bReturn;
}

int hb___GetNumberofFilestoUnzip( char *szFile )
{
   int iNumberOfFiles;
   void * sDiskZip = NULL ;

//   if ( HB_IS_BLOCK( pDiskCallBackBlock ) )
//   {
//       sDiskZip = SpanCallbackc_New(DiskCallBack, ( void * ) pDiskCallBackBlock );
//   }

   iNumberOfFiles = ZipArchive_GetCount( szFile , sDiskZip);

   if ( pDiskCallBackBlock )
   {
      hb_itemRelease( pDiskCallBackBlock ) ;
   }

//   if ( sDiskZip )
//      SpanCallbackc_End( &sDiskZip );

   return iNumberOfFiles;
}


PHB_ITEM HB_EXPORT hb___GetFileNamesFromZip( char *szFile, BOOL iMode)
{
   int iNumberOfFiles;
   int iCount;
   int iOMode = GetZipMode( szFile );
   PHB_ITEM pArray;
   char szTempTime[ 80 ] = { 0 } ;

   void * pZip;
   void * sDiskZip = NULL ;
   #ifdef __WIN32__
      BOOL bReadOnly = (GetFileAttributes( szFile ) & FILE_ATTRIBUTE_READONLY );
   #else
      BOOL bReadOnly = 0 ;
   #endif

   pZip = ZipArchive_Open( szFile, ( void * ) sDiskZip, iOMode ,NULL,  ( void * ) pDiskCallBackBlock , bReadOnly  );

   if ( ! pZip )
   {
      pArray = hb_itemArrayNew( 0 );
      return pArray ;
   }

   iNumberOfFiles = ZipArchive_GetFileCount( pZip );
   // TraceLog("my.log"," iNumberOfFiles %i\n" ,iNumberOfFiles);
   pArray = hb_itemNew( NULL );
   hb_arrayNew( pArray, 0 );


   for( iCount = 0 ; iCount < iNumberOfFiles ; iCount++ )
   {
      char *szFileName = ( char * ) hb_xgrab( 256 );
      DWORD dwMethod;
      DWORD dwFileCompressed;
      DWORD dwFileUnCompressed;
      DWORD dwFlag;
      DWORD dwFileTime;
      DWORD dwFileDate;
      DWORD dwAttr;
      DWORD dwCrc;
      BOOL  bEncrypt;
      PHB_ITEM pItem;
      struct tm *SzTime;

      memset(szFileName,'\0',256);
      ZipArchive_GetFileInfo(pZip,&szFileName,&dwMethod,&dwFileCompressed,&dwFileUnCompressed,&dwFlag, &dwFileTime, &dwFileDate,&dwAttr,&dwCrc,&bEncrypt, (WORD) iCount);

      if ( iMode )
      {
         PHB_ITEM TempArray ;
         char szAttr[ 5 ] = { 0 } ;
         char szTime[ 5 ] = { 0 } ;
         char *szMethod;
         char szCRC[ 8 ] = { 0 } ;
         int iRatio;
         int iCount;
         int iiCount = 0;

         TempArray = hb_itemNew( NULL );
         hb_arrayNew( TempArray, 9 );

         pItem = hb_itemPutC( NULL, ( char * )szFileName );
         hb_arraySetForward( TempArray, filePos, pItem );
         hb_itemRelease( pItem ) ;

         hb_xfree( ( char * ) szFileName);

         #if defined( __WIN32__ )
            szAttr[ 0 ] = dwAttr & FILE_ATTRIBUTE_READONLY ? ( char ) 'r' : ( char ) '-';
            szAttr[ 1 ] = dwAttr & FILE_ATTRIBUTE_HIDDEN ? ( char ) 'h' : ( char )   '-';
            szAttr[ 2 ] = dwAttr & FILE_ATTRIBUTE_SYSTEM ? ( char )  's' : ( char ) 'w';
            szAttr[ 3 ] = ( dwAttr & FILE_ATTRIBUTE_DIRECTORY ) ? ( char )  'D' : dwAttr & FILE_ATTRIBUTE_ARCHIVE ? ( char )  'a' : ( char )  '-';
         #endif

         szAttr[ 4 ] = bEncrypt ? ( char )  '*' : ( char )  ' ';

         if ( dwFileUnCompressed>0 )
         {
            pItem = hb_itemPutNL( NULL, dwFileUnCompressed ) ;
            hb_arraySetForward( TempArray, Lenght, pItem );
            hb_itemRelease( pItem ) ;

            pItem = hb_itemPutNL( NULL, dwFileCompressed ) ;
            hb_arraySetForward( TempArray, Size, pItem);
            hb_itemRelease( pItem ) ;
            iRatio = 100-( ( dwFileCompressed*100 ) / dwFileUnCompressed );

            if ( iRatio <0 )
            {
               iRatio = 0;
            }
            pItem = hb_itemPutNL( NULL, iRatio );
            hb_arraySetForward( TempArray, Ratio, pItem );
            hb_itemRelease( pItem ) ;
         }
         else
         {
            pItem = hb_itemPutNL( NULL, dwFileUnCompressed ) ;
            hb_arraySetForward( TempArray, Lenght, pItem);
            hb_itemRelease( pItem ) ;
            pItem = hb_itemPutNL( NULL, dwFileCompressed ) ;
            hb_arraySetForward( TempArray, Size, pItem );
            hb_itemRelease( pItem ) ;
            iRatio = 0;
            pItem = hb_itemPutNL( NULL, iRatio ) ;
            hb_arraySetForward( TempArray, Ratio, pItem);
            hb_itemRelease( pItem ) ;
         }

         #if defined( __WIN32__ )
            if ( dwMethod == 0  || dwAttr & FILE_ATTRIBUTE_DIRECTORY )
            {
            szMethod = "Stored";
            }
         #endif

         if ( dwMethod == 8 )
         {
            UINT iLevel = ( UINT )( ( dwFlag & 0x6 ) / 2 );

            switch( iLevel )
            {
               case 0:
                  szMethod = "DeflatN";
                  break;

               case 1:
                  szMethod = "DeflatX";
                  break;

               case 2:
               case 3:
                  szMethod = "DeflatF";
                  break;

               default:
                  szMethod = "Unknow";
            }
         }

         pItem = hb_itemPutC( NULL, szMethod ) ;
         hb_arraySetForward( TempArray, Method, pItem);
         hb_itemRelease( pItem ) ;
         sprintf( szCRC, "%8.8lx\n", ( ULONG )dwCrc );

         pItem = hb_itemPutCL( NULL, szCRC, 8 );
         hb_arraySetForward( TempArray, Crc32, pItem);
         hb_itemRelease( pItem ) ;
         pItem = hb_itemPutD( NULL, ( LONG ) ( dwFileDate >> 9 ) +1980,( LONG )( ( dwFileDate & ~0xFE00 ) >> 5 ), ( LONG )dwFileDate & ~0xFFE0 ) ;
         hb_arraySetForward( TempArray, Date, pItem);

         hb_itemRelease( pItem ) ;
         SzTime =  localtime( (const time_t*) &dwFileTime );
         hb_____GetTime( SzTime ,(char*) szTempTime);

         for( iCount = 10 ; iCount < 16 ; iCount ++ )
         {
            if( ( iCount>10 ) && ( iCount<16 ) )
            {
               szTime[ iiCount ] = szTempTime[ iCount ];
               iiCount++;
            }
         }

         pItem = hb_itemPutCL( NULL, szTime, 5 ) ;
         hb_arraySetForward( TempArray, Time, pItem);
         hb_itemRelease( pItem ) ;

         pItem = hb_itemPutCL( NULL, szAttr, 5 );
         hb_arraySetForward( TempArray, Attr, pItem);
         hb_itemRelease( pItem ) ;
//         hb_arraySetForward( pArray, iCount+1, TempArray );
         hb_arrayAddForward( pArray, TempArray );
         hb_itemRelease( TempArray );

      }
      else
      {
         pItem = hb_itemPutC( NULL, ( char * ) szFileName );
//         hb_arraySetForward( pArray, iCount+1, pItem);
           hb_arrayAddForward( pArray, pItem );
         hb_itemRelease( pItem ) ;

      }
   }


   if ( pDiskCallBackBlock )
   {
      hb_itemRelease( pDiskCallBackBlock );
   }

   ZipArchive_Close( pZip );

//   if ( sDiskZip )
//      SpanCallbackc_End( &sDiskZip );

   return pArray;
}

int hb_UnzipOne( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, const char *szPassWord, const char *pbyBuffer, const char *szFiletoExtract, PHB_ITEM pProgress)
{
   BOOL bWithPath = lWithPath ? TRUE : FALSE;
   BOOL iReturn = TRUE;
   int iCount;
   int iMode = GetZipMode( szFile );
   char  * szPath = (char*) hb_xgrab( HB_PATH_MAX );
   #ifdef __WIN32__
      BOOL bReadOnly = (GetFileAttributes( szFile ) & FILE_ATTRIBUTE_READONLY );
   #else
      BOOL bReadOnly = 0 ;
   #endif

   void * pZip;
   void * sDiskZip = NULL ;

   if ( pDiskCallBackBlock )
   {
       sDiskZip = SpanCallbackc_New( DiskCallBack, ( void * ) pDiskCallBackBlock );
   }


   pZip = ZipArchive_Open( szFile, ( void * ) sDiskZip, iMode, ( char * ) szPassWord,  NULL, bReadOnly  );

   if (!pZip)
      return 0;


   iCount = ZipArchive_FindFile( pZip,( char * )szFiletoExtract, FALSE );

   if ( iCount == -1 )
   {
      iCount = ZipArchive_FindFile( pZip, ( char * ) szFiletoExtract, TRUE );
   }

   if ( iCount >= 0 )
   {
      char * szFileNameInZip = ZipArchive_GetFileName( pZip, ( WORD )iCount );
      PHB_FNAME pOut;
      pOut = hb_fsFNameSplit( ( char * ) szFileNameInZip );

      if ( pbyBuffer )
      {
         if (hb_stricmp(pbyBuffer,".\\")==0 )
         {
            hb_fsCurDirBuffEx( 0,  szPath, HB_PATH_MAX );
         }
         else
         {
            strcpy(szPath,pbyBuffer);
         }

         hb_fsChDir( (const char *) "\\");

         ZipHandle_ZipSetRoot(pZip,szPath);
      }

      hb_xfree( pOut );

      if( pBlock  !=  NULL )
      {
         PHB_ITEM FileName = hb_itemPutC( NULL, szFiletoExtract ) ;
         hb_vmEvalBlockV( pBlock, 1, FileName );
         hb_itemRelease( FileName );
      }

      ZipArchive_ExtractFile( pZip, ( WORD ) iCount, szPath, bWithPath );
   }

   ZipArchive_Close( pZip );

   if ( sDiskZip )
      SpanCallbackc_End( &sDiskZip );

   if ( pDiskCallBackBlock )
   {
      hb_itemRelease( pDiskCallBackBlock );
   }

   if (szPath)
   {
      hb_fsChDir( (const char *) szPath);
      hb_xfree(szPath);
   }

   return ( int ) iReturn;
}


int HB_EXPORT hb_UnzipSel( char *szFile, PHB_ITEM pBlock, BOOL lWithPath,const char *szPassWord,const char *pbyBuffer, PHB_ITEM pSelArray, PHB_ITEM pProgress )
{
   BOOL bWithPath = lWithPath ? TRUE : FALSE;
   BOOL iReturn = TRUE;
   int iCount;
   int iCause;
   BOOL bChange = FALSE;
   char * lpFiletoExtract;
   int iMode = GetZipMode( szFile );
   void * pZip;
   void * sDiskZip = NULL ;
   char  * szPath = (char*) hb_xgrab( HB_PATH_MAX );
   #ifdef __WIN32__
      BOOL bReadOnly = (GetFileAttributes( szFile ) & FILE_ATTRIBUTE_READONLY );
   #else
      BOOL bReadOnly = 0 ;
   #endif

   if ( pDiskCallBackBlock )
   {
       sDiskZip = SpanCallbackc_New( DiskCallBack, ( void * ) pDiskCallBackBlock );
   }

   pZip = ZipArchive_Open( szFile, ( void * ) sDiskZip, iMode, ( char * ) szPassWord, NULL, bReadOnly  );

   if (!pZip)
      return 0;


   if ( iReturn )
   {
      if ( HB_IS_BLOCK( pProgress ) )
      {
         ZipArchive_SetActCallBack( pZip, ActCallback, ( void * ) pProgress);
      }
      if ( pbyBuffer )
      {
         if (hb_stricmp(pbyBuffer,".\\")==0 )
         {
            hb_fsCurDirBuffEx( 0, szPath, HB_PATH_MAX );
         }
         else
         {
            strcpy(szPath,pbyBuffer);
         }
         hb_fsChDir( (const char *) "\\");

         ZipHandle_ZipSetRoot(pZip,szPath);
      }

      for ( iCause = 1 ; ( iCause <=  ( int ) hb_arrayLen( pSelArray ) ) ; iCause ++ )
      {
         lpFiletoExtract = hb_arrayGetC( pSelArray, iCause );
         iCount = ZipArchive_FindFile( pZip, lpFiletoExtract, FALSE );

         if ( iCount == -1 )
         {
            iCount = ZipArchive_FindFile( pZip, lpFiletoExtract, TRUE );
         }

         if ( iCount >= 0 )
         {
            char *  szFileNameInZip = ZipArchive_GetFileName( pZip, ( WORD )iCount );
            PHB_FNAME pOut;
            pOut = hb_fsFNameSplit( ( char * ) szFileNameInZip );


            hb_xfree( pOut );

            if( pBlock  !=  NULL )
            {

               PHB_ITEM FileName =hb_itemPutC( NULL, ( char * )szFileNameInZip  );
               hb_vmEvalBlockV( pBlock, 1, FileName  );
               hb_itemRelease( FileName );

            }

            ZipArchive_ExtractFile( pZip, ( WORD ) iCount, szPath, bWithPath );


            if ( bChange )
            {
               bChange = FALSE;
               szPath = "";
            }
         }
      }
   }

   if ( pDiskCallBackBlock )
   {
      hb_itemRelease( pDiskCallBackBlock );
   }




   ZipArchive_Close( pZip );

   if ( sDiskZip )
      SpanCallbackc_End( &sDiskZip );

   if (szPath)
   {
      hb_fsChDir( (const char *) szPath);
      hb_xfree( szPath);
   }

   return ( int ) iReturn;
}

int hb_UnzipAll( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, const char *szPassWord, const char *pbyBuffer, PHB_ITEM pProgress)
{
   BOOL bWithPath = lWithPath ? TRUE : FALSE ;
   BOOL iReturn = TRUE;
   int iCount ;
   BOOL bChange = FALSE;
   char  * szPath = (char*) hb_xgrab( HB_PATH_MAX );

   int iMode = GetZipMode( szFile );
   #ifdef __WIN32__
      BOOL bReadOnly = (GetFileAttributes( szFile ) & FILE_ATTRIBUTE_READONLY );
   #else
      BOOL bReadOnly = 0 ;
   #endif
   void * pZip;
   void * sDiskZip =NULL;

   if ( pDiskCallBackBlock )
   {
          sDiskZip = SpanCallbackc_New( DiskCallBack, ( void * ) pDiskCallBackBlock );
   }

   pZip = ZipArchive_Open( szFile, ( void * ) sDiskZip, iMode, ( char * ) szPassWord, NULL, bReadOnly  );

   if ( ! pZip )
      return 0 ;

   if ( iReturn )
   {

      if ( HB_IS_BLOCK( pProgress ) )
      {
         ZipArchive_SetActCallBack( pZip, ActCallback, ( void * ) pProgress);
      }

      if ( pbyBuffer )
      {
         if (hb_stricmp(pbyBuffer,".\\")==0 )
         {
            hb_fsCurDirBuffEx( 0, szPath, HB_PATH_MAX );
         }
         else
         {
            strcpy(szPath,pbyBuffer);
         }

         hb_fsChDir( (const char *) "\\");

         ZipHandle_ZipSetRoot(pZip,szPath);
      }

      for ( iCount = 0 ; iCount < ( int ) ZipArchive_FileCount( pZip ) ; iCount ++ )
      {
         char *  szFileNameInZip = ZipArchive_GetFileName( pZip, ( WORD )iCount );
         PHB_FNAME pOut;

         pOut = hb_fsFNameSplit( ( char * ) szFileNameInZip );


         hb_xfree( pOut );

         if( pBlock  !=  NULL )
         {
            PHB_ITEM FileName  = hb_itemPutC(  NULL, ( char * ) szFileNameInZip );
            PHB_ITEM FilePos   = hb_itemPutNI( NULL, iCount );

            hb_vmEvalBlockV( pBlock, 2, FileName, FilePos );

            hb_itemRelease( FileName );
            hb_itemRelease( FilePos );

         }

         ZipArchive_ExtractFile( pZip, ( WORD ) iCount, szPath, bWithPath );

         if( bChange )
         {
            bChange = FALSE;
            szPath = NULL;
         }
      }
   }

   if ( pDiskCallBackBlock )
   {
      hb_itemRelease( pDiskCallBackBlock );
   }


   ZipArchive_Close( pZip );

   if ( sDiskZip )
      SpanCallbackc_End( &sDiskZip );

   if (szPath)
   {
      hb_fsChDir( (const char *) szPath);
      hb_xfree(szPath);
   }


   return ( int ) iReturn;
}

void hb_____GetTime( struct tm *tz , char *szTempTime)
{
   struct tm t;

   t.tm_sec    = tz->tm_sec;
   t.tm_min    = tz->tm_min;
   t.tm_hour   = tz->tm_hour;
   t.tm_mday   = tz->tm_mday;
   t.tm_mon    = tz->tm_mon;
   t.tm_year   = tz->tm_year;
   t.tm_wday   = 4;
   t.tm_yday   = 0;
   t.tm_isdst  = 0;

   strcpy( szTempTime, asctime( &t ) );
}

void DelhrbBlock(void ** pBlock)
{
   PHB_ITEM p = (PHB_ITEM) *pBlock ;

   if( p )
   {
      hb_itemRelease(p);
   }
}
/* Call back routines */
BOOL ActCallback( DWORD m_uTotalToDo, DWORD m_uTotalSoFar, void * pCallBackData)
{
   BOOL iReturn = 1;

   if( pCallBackData )
   {

      PHB_ITEM Disk  = hb_itemPutNL( NULL, (LONG) m_uTotalSoFar );
      PHB_ITEM Total = hb_itemPutNL( NULL, (LONG) m_uTotalToDo );


     // hb_vmEvalBlockV( &ProgressInfo, 2, &Disk, &Total );
      hb_vmEvalBlockV( ( PHB_ITEM ) pCallBackData, 2, Disk, Total );
      hb_itemRelease( Disk );
      hb_itemRelease( Total );
   }

   return iReturn;
}

BOOL DiskCallBack( DWORD m_uDiskNeeded, void * pCallBackData )
{
   BOOL iReturn = 1;

   if( pCallBackData )
   {

      PHB_ITEM Disk  = hb_itemPutNL( NULL, (LONG) m_uDiskNeeded  );

      hb_vmEvalBlockV( (PHB_ITEM)pCallBackData, 1, Disk  );

      hb_itemRelease( Disk );
   }

   return iReturn;
}

/* Misc Routines */
char * hb___CheckFile( char * szFile )
{
   /*
   unsigned int uiCount, uiLen;
   int iDot_Found = 0;


   uiLen = (unsigned int) strlen( szFile );



   for ( uiCount = 0;uiCount<uiLen;uiCount++ )
   {
      if ( szFile[ uiCount ] == '.' )
      {
         iDot_Found = 1;
      }
   }

   if ( iDot_Found == 0 )
   {
      strcat( szFile, ".zip" );
   }
   */

   PHB_FNAME pFilepath;
   pFilepath = hb_fsFNameSplit( (char*) szFile );

   if ( !pFilepath->szExtension )
   {
      strcat( szFile, ".zip" );
   }

   hb_xfree( pFilepath ) ;
   return szFile ;
}

DWORD  GetCurrentFileSize(char *szFile )
#if defined( HB_OS_WIN_32 ) || defined( __MINGW32__ ) || defined( HB_OS_WIN ) 
{
   DWORD dwFlags = FILE_ATTRIBUTE_ARCHIVE;
   HANDLE hFind;
   WIN32_FIND_DATA  hFilesFind;

   hFind = FindFirstFile( szFile, &hFilesFind );

   if ( hFind != INVALID_HANDLE_VALUE )
   {
      if ( dwFlags & hFilesFind.dwFileAttributes )
      {
         FindClose( hFind );
         if( hFilesFind.nFileSizeHigh>0 )
         {
            return ( ( hFilesFind.nFileSizeHigh*MAXDWORD )+hFilesFind.nFileSizeLow );
         }
         else
         {
            return ( hFilesFind.nFileSizeLow );
         }

      }
   }

   FindClose( hFind );

   return ( DWORD ) -1;

}
#elif defined( __GNUC__ )
{
   USHORT   ushbMask   = 63;
   USHORT   usFileAttr = HB_FA_ARCHIVE;
   struct stat sStat;

   if ( stat( szFile, &sStat ) !=  -1 )
   {
      return sStat.st_size;
   }

   return -1;
}

#endif

int hb___SetCallbackFunc( PHB_ITEM pFunc )
{
   pDiskCallBackBlock = pFunc;

   return ( int ) 1;
}


HB_FUNC( HB_GETZIPCOMMENT )
{
   char * szComment;
   char szFile[ HB_PATH_MAX - 1 ] = { 0 } ;
   int iMode = GetZipMode( szFile );
   #ifdef __WIN32__
      BOOL bReadOnly = (GetFileAttributes( szFile ) & FILE_ATTRIBUTE_READONLY );
   #else
      BOOL bReadOnly = 0 ;
   #endif
   void * pZip;
   void * sDiskZip =NULL;

   strcpy( szFile, hb_parc( 1 ) );

   if ( pDiskCallBackBlock )
   {
          sDiskZip = SpanCallbackc_New( DiskCallBack, ( void * ) pDiskCallBackBlock );
   }

   pZip = ZipArchive_Open( szFile, ( void * ) sDiskZip, iMode, NULL, NULL, bReadOnly  );

   if ( ! pZip )
   {
      hb_retc( "");
      return;
   }

   szComment=ZipAchive_GetGlobalcomment(pZip);

   ZipArchive_Close( pZip );

   hb_retcAdopt( szComment );
}


HB_FUNC( HB_UNZIPALLFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      char szFile[ HB_PATH_MAX - 1 ] = { 0 } ;
      strcpy( szFile, hb_parc( 1 ) );

      bRet = hb_UnzipAll( hb___CheckFile( szFile ),
                             hb_param( 2, HB_IT_BLOCK ),
                             ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                             ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                             ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                             hb_itemParam( 7 ) );


   }

   hb_retl( bRet );
}

int hb_UnzipSelIndex( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, const char *szPassWord, const char *pbyBuffer, PHB_ITEM pSelArray ,PHB_ITEM pProgress)
{
   BOOL bWithPath = lWithPath ? TRUE : FALSE ;
   BOOL iReturn = TRUE;
   int iCount ;
   ULONG ulCount;
   BOOL bChange = FALSE;
   char  * szPath = (char*) hb_xgrab( HB_PATH_MAX );

   int iMode = GetZipMode( szFile );
   #ifdef __WIN32__
      BOOL bReadOnly = (GetFileAttributes( szFile ) & FILE_ATTRIBUTE_READONLY );
   #else
      BOOL bReadOnly = 0 ;
   #endif
   void * pZip;
   void * sDiskZip =NULL;

   if ( pDiskCallBackBlock )
   {
          sDiskZip = SpanCallbackc_New( DiskCallBack, ( void * ) pDiskCallBackBlock );
   }

   pZip = ZipArchive_Open( szFile, ( void * ) sDiskZip, iMode, ( char * ) szPassWord, NULL, bReadOnly  );

   if ( ! pZip )
      return 0 ;

   if ( iReturn )
   {
      if ( HB_IS_BLOCK( pProgress ) )
      {
         ZipArchive_SetActCallBack( pZip, ActCallback, ( void * ) pProgress);
      }

      if ( pbyBuffer )
      {
         if (hb_stricmp(pbyBuffer,".\\")==0 )
         {
            hb_fsCurDirBuffEx( 0, szPath, HB_PATH_MAX );
         }
         else
         {
            strcpy(szPath,pbyBuffer);
         }

         hb_fsChDir( (char *) "\\");

         ZipHandle_ZipSetRoot(pZip,szPath);
      }

      for ( iCount = 1 ; iCount <= ( int ) hb_arrayLen( pSelArray ) ; iCount ++ )
      {
         char *  szFileNameInZip ;
         ulCount = hb_arrayGetNI( pSelArray, iCount ) - 1;

         if( ulCount >=0)
         {
            PHB_FNAME pOut;
            szFileNameInZip = ZipArchive_GetFileName( pZip, ( WORD )ulCount );

            pOut = hb_fsFNameSplit( ( char * ) szFileNameInZip );
            if ( szPath == NULL )
            {
               szPath = ( char* )pOut->szDrive;
               pOut->szDrive = "";
               hb_fsFNameMerge( ( char* )szFileNameInZip, pOut );
               bChange = TRUE;
            }

            ZipHandle_ZipSetRoot( pZip, szPath );
            hb_xfree( pOut );

            if( pBlock  !=  NULL )
            {
               PHB_ITEM FileName  = hb_itemPutC( NULL, ( char * )szFileNameInZip );
               PHB_ITEM FilePos   = hb_itemPutNI( NULL, iCount );
               hb_vmEvalBlockV( pBlock, 2, FileName, FilePos);
               hb_itemRelease( FileName );
               hb_itemRelease( FilePos );

            }

            ZipArchive_ExtractFile( pZip, ( WORD ) ulCount, szPath, bWithPath );

            if( bChange )
            {
               bChange = FALSE;
               szPath = NULL;
            }
      }
     }

   }

   if ( pDiskCallBackBlock )
   {
      hb_itemRelease( pDiskCallBackBlock );
   }


   ZipArchive_Close( pZip );

   if ( sDiskZip )
   {
      SpanCallbackc_End( &sDiskZip );
   }

   if (szPath)
   {
      hb_fsChDir( (const char *) szPath);
      hb_xfree(szPath);
   }


   return ( int ) iReturn;
}


HB_FUNC( HB_UNZIPFILEINDEX )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pDelZip = hb_param( 6, HB_IT_NUMERIC | HB_IT_ARRAY );

      if ( pDelZip )
      {
         char szFile[ HB_PATH_MAX - 1 ] = { 0 } ;
         PHB_ITEM pProgress = hb_param( 7, HB_IT_BLOCK );
         PHB_ITEM  DelZip = hb_itemArrayNew( 0 ), Temp,iProgress;
         PHB_ITEM ZipArray;
         char* szZipFileName;
         int ulLen;


         if( pProgress )
         {
           iProgress= hb_itemNew( pProgress );
//           hb_itemCopy( &iProgress, pProgress );
         }


         strcpy( szFile, hb_parc( 1 ) );
         szZipFileName = hb___CheckFile( szFile );

         ZipArray = hb___GetFileNamesFromZip( szZipFileName, TRUE );
         ulLen = hb_arrayLen( ZipArray ) ; // hb_arrayZipArray->item.asArray.value->ulLen;

         if ( HB_IS_NUMERIC ( pDelZip ) )
         {
            int iIndex = hb_itemGetNI( pDelZip );

            if ( iIndex > 0 && iIndex <= ulLen )
            {
               Temp= hb_itemPutNI( NULL, iIndex ) ;
               hb_arrayAddForward( DelZip,Temp );
               hb_itemRelease( Temp );
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
                  Temp= hb_itemPutNI( NULL, iIndex ) ;
                  hb_arrayAddForward( DelZip, Temp);
                  hb_itemRelease( Temp );
               }
            }

         }

         if( hb_arrayLen( DelZip )  > 0 )
         {
            bRet = hb_UnzipSelIndex( szZipFileName,
                                     hb_param( 2, HB_IT_BLOCK ),
                                     ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                                     ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                                     ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                                     DelZip,
                                     iProgress );
         }

         hb_itemRelease( DelZip );
         hb_xfree( szZipFileName );

      }
   }

   hb_retl( bRet );
}
HB_FUNC( HB_SETZIPCOMMENT )
{

}
