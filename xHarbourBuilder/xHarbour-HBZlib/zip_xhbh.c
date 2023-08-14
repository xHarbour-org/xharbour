/*
 * $Id$
 */

/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Luiz Rafael Culik Guimaraes culikr@brturbo.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/



#include "hbzip.h"
//#include "tchar.h"
#include "time.h"

LONG ZipNew( void )
{
   void * pZip ;
   pZip = ZipHandle_New() ;

   if ( ! pZip )
   {
      return -1 ;
   }

   return ( LONG ) pZip ;
}

int ZipGetLastError( void * oZip)
{
   return ZipHandle_GetLastError(oZip);
}

void ZipSetDisk( void * oZip, void * pAct )
{
//   ZipHandle_SetDiskBlock(oZip,( void * )pDisk);
     ZipArchive_SetDiskCallBack( oZip, DiskCallBack, ( void * ) pAct  );

}

void ZipSetActBlock(void * oZip, void * pAct )
{
    ZipArchive_SetActCallBack( oZip, ActCallback, ( void * ) pAct  );
//      ZipHandle_SetActBlock( oZip, ( void * ) pAct, ActCallback );
}

int ZipSetAdvance( void * oZip,int iWriteBuffer,int iGeneralBuffer, int iSearchBuffer)
{
   return ZipHandle_SetAdvance( oZip, iWriteBuffer, iGeneralBuffer, iSearchBuffer )==0 ?1 :-1;
}

int ZipGetAdvance( void * oZip,int *iWriteBuffer,int *iGeneralBuffer, int *iSearchBuffer)
{

   return ZipHandle_GetAdvance( oZip, iWriteBuffer, iGeneralBuffer, iSearchBuffer ) ==0 ?1 :-1;
}

void ZipSetGlobalComment( void * oZip, const char * szComment )
{
   ZipAchive_SetGlobalcomment( oZip, szComment) ;
}

int  ZipSetFilePath( void * oZip, BOOL bPath )
{
   return ZipHandle_SetWithPath( oZip, bPath );
}


int ZipCreate( void * oZip,  const char * szFile, int iDefault )
{
   return ZipHandle_Create( oZip, szFile, iDefault );
}

int ZipOpen( void * oZip,  const char * szFile, int iDefault )
{
   return ZipHandle_Open( oZip, szFile, iDefault );
}

PHB_ITEM  ZipAddFiles( void * oZip, PHB_ITEM pArray )
{
   ULONG ulCount;
   ULONG ulLen = hb_arrayLen( pArray );
   char *szFileName;
   PHB_ITEM pRetArray = hb_itemArrayNew( ulLen );
   PHB_ITEM temp;
   int iReturn = 0;
   int iRet;

   for ( ulCount = 1; ( ulCount <= ulLen ) ;ulCount++ )
   {
      szFileName = ( char * ) hb_arrayGetCPtr( pArray, ulCount ) ;
      iRet = ZipHandle_AddNewFile(  oZip , szFileName );
      temp = hb_itemPutNL( NULL, iRet );
      hb_itemArrayPut( pRetArray, ulCount, temp );
      hb_itemRelease( temp );

   }
    return pRetArray;
}

int  ZipAddFile( void * oZip, const char *szFileName )
{
   int iReturn ;

   iReturn = ZipHandle_AddNewFile(  oZip , szFileName );
   return iReturn;

}

void ZipClose( void * oZip)
{
     PHB_ITEM p = ( PHB_ITEM ) ZipHandle_GetDiskBlock( oZip );
     PHB_ITEM p1 = ( PHB_ITEM ) ZipHandle_GetActBlock( oZip );
     ZipArchive_Close(oZip);
     if ( p )
     {
        hb_itemRelease( p );
        p = NULL;
     }
     if ( p1 )
     {
        hb_itemRelease( p1 );
        p1 = NULL;
     }

}

int  ZipSetCompressLevel( void * oZip, int iCompressLevel )
{
   return ZipHandle_SetCompressLevel( oZip, iCompressLevel );
}

void ZipSetPassWord( void * oZip, const char * szPassWord)
{
   ZipHandle_SetPassWord( oZip,  szPassWord );
}

void  ZipSetReadOnly( void * oZip, BOOL bReadOnly )
{


   ZipHandle_SetReadOnly( oZip, bReadOnly );
}

int ZipDeleteSelected( void * oZip, PHB_ITEM pArray)
{

   int iCount;
   int iLen = hb_arrayLen( pArray );

   for ( iCount = 1 ; ( iCount <=  ( int ) iLen ) ; iCount ++ )
   {
      char *szFile = ( char * )hb_arrayGetCPtr( pArray, iCount );
      ZipStringArray_Add( oZip, szFile );
   }

   return( ZipHandle_DeleteFile( oZip ) );

}

void  ZipSetExtractPath( void * oZip, const char * szPath )
{
   ZipHandle_SetExtractPath( oZip, szPath );
}

char * ZipGetExtractPath( void * oZip)
{
   return ZipHandle_GetExtractPath( oZip );
}

int ZipExtractFiles( void * oZip )
{
   return ZipHandle_Extract( oZip );
}

PHB_ITEM ZipSetFiles(  void * oZip, PHB_ITEM pFiles,int *Status )
{
   PHB_ITEM pRet;
   if ( ZipHandle_DeleteAll( oZip) == 0 )
   {
      pRet = ZipAddFiles( oZip, pFiles );
      *Status = 0;
//      return 0;
      return pRet;
   }
   pRet = hb_itemArrayNew(0);
   *Status = -1;
   return pRet;
//   return -1;
}


void ZipSetRootPath(void *oZip, const char * szPath)
{
   ZipHandle_ZipSetRoot( oZip,szPath);
}

void ZipSetTempPath(void *oZip, char * szPath)
{
   ZipHandle_ZipSetTempPath( oZip,szPath);
}

char * ZipGetTempPath(void *oZip)
{
   return ZipHandle_ZipGetTempPath( oZip);
}

/* ZipAddFiles( void * oZip, PHB_ITEM pArray )*/

/* Xharbour callable functions */
HB_FUNC( ZIPNEW )
{
   hb_retnl( (LONG) ZipNew() );
}

/*
function Zipcreate( nZipHandle, cFileName, iMode ) -> iErrorCode
Parameters
nZipHandle Handle to the zip object obtained with ZipNew() function
cFileName  Zip file name to Create
iMode     Mode Creation of Zip
   ZIP_ST  Standart Zip
   ZIP_PK  Disk Spanning Zip on removable media
   ZIP_TD  Disk Spanning Zip on HD(as Rar files, first a.rar laters, a.r00,...)
Return
iErrorCode Code that demostrate operation
0 Ok
-1 Invalid Handle
use ZipGetLastError for errr
*/


HB_FUNC(ZIPCREATE)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   const char * szFileName = hb_parc( 2 ) ;
   int iDefault = ISNUM( 3 ) ? hb_parni( 3 ) : 0 ;

   hb_retni( ZipCreate( ( void * ) oZip , szFileName, iDefault ) );
}

/*
function ZipOpen( nZipHandle, cFileName, iMode ) -> iErrorCode
Parameters
nZipHandle Handle to the zip object obtained with ZipNew() function
cFileName  Zip file name to Open
iMode      Zip File Mode
   ZIP_ST  Standart Zip
   ZIP_PK  Disk Spanning Zip on removable media
   ZIP_TD  Disk Spanning Zip on HD(as Rar files, first a.rar laters, a.r00,...)
Return
iErrorCode Code that demostrate operation
0 Ok
-1 Directory Not Find
-2 Disk Spanning Code Block not Set
-3 no handle passed
-4
*/

HB_FUNC(ZIPOPEN)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   const char * szFileName = hb_parc( 2 ) ;
   int iDefault = ISNUM( 3 ) ? hb_parni( 3 ) : 0 ;

   hb_retni( ZipOpen( ( void * ) oZip , szFileName, iDefault ) );
}

/*
function ZipAddFiles( nZipHandle, aFileName ) -> iErrorCode
Parameters
nZipHandle Handle to the zip object obtained with ZipNew() function
aFileName  Array containg File Name to add to zip archive
Return
iErrorCode Code that demostrate operation
0 Ok
-1 Operation now allow on this Zip Type
-2 no handle passed
*/


HB_FUNC(ZIPADDFILES)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY ) ;
   PHB_ITEM pRet ;
//   hb_retni ( ZipAddFiles( ( void * ) oZip, pArray ) );
   pRet = ZipAddFiles( ( void * ) oZip, pArray ) ;
   hb_itemReturn( pRet );
   hb_itemRelease( pRet );
}


/*
function ZipAddFile( nZipHandle, cFileName ) -> iErrorCode
Parameters
nZipHandle Handle to the zip object obtained with ZipNew() function
cFileName  File Name to add to zip archive
Return
iErrorCode Code that demostrate operation
0 Ok
-1 Operation not allow on this Zip Type
-2 Error adding file to zip(get error number by calling ZIPGETLASTERROR() )
-3 no handle passed
*/


HB_FUNC(ZIPADDFILE)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   const char * szFile = hb_parc( 2 ) ;
   hb_retni ( ZipAddFile( ( void * ) oZip, szFile ) );
}

HB_FUNC(ZIPCLOSE)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   ZipClose( (void * ) oZip );
}

/*
function ZipExtractFiles( nZipHandle ) -> iErrorCode
Parameters
nZipHandle Handle to the zip object obtained with ZipNew() function
Return
iErrorCode Code that demostrate operation
0 Ok
-1 unzip error
-2 no handle passed
*/

HB_FUNC(ZIPEXTRACTFILES)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   hb_retni( ZipExtractFiles( ( void * ) oZip) );
}

HB_FUNC( ZIPSETFILES )
{
  LONG oZip = ( LONG ) hb_parnl( 1 );
  PHB_ITEM pFiles = hb_param( 2, HB_IT_ARRAY );
  int iStatus = 0;
  PHB_ITEM pRet;
  pRet= ZipSetFiles ( ( void * ) oZip, pFiles,&iStatus ) ;
  hb_retni(iStatus);
  hb_itemRelease( pRet );
}


/* Xharbour Zip File Properties Set Function  */

HB_FUNC( ZIPSETCOMPRESSLEVEL )
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   int iLevel = ISNUM( 2 ) ? hb_parni( 2 ) : -1 ;
   hb_retni (ZipSetCompressLevel( ( void * ) oZip, iLevel ));
}

HB_FUNC(ZIPSETPASSWORD)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   const char * szPassWord = hb_parc( 2 )  ;
   ZipSetPassWord( ( void * ) oZip, szPassWord );
}

HB_FUNC(ZIPSETREADONLY)
{
  LONG oZip = ( LONG ) hb_parnl( 1 );
  BOOL bReadOnly = hb_parl( 2 );
  ZipSetReadOnly( ( void * ) oZip, bReadOnly );

}

HB_FUNC( ZIPSETEXTRACTPATH )
{
  LONG oZip = ( LONG ) hb_parnl( 1 );
  const char * szPath = hb_parc( 2 )  ;
  ZipSetExtractPath(  ( void * ) oZip, szPath );
}

HB_FUNC ( ZIPSETONDISK )
{
   LONG oZip = ( LONG ) hb_parnl( 1 ) ;
   PHB_ITEM pBlock = hb_param( 2, HB_IT_BLOCK ) ;

   if( pBlock )
   {
      //TraceLog( NULL, "Disk Block: %p\n", pBlock->item.asBlock.value );
      //pBlock->item.asBlock.value->ulCounter++;
      //hb_gcLock( pBlock->item.asBlock.value );
//      ZipSetDisk( ( void * )oZip, (void*) pBlock->item.asBlock.value );
      ZipSetDisk( ( void * )oZip, (void*) hb_itemNew( pBlock ));
   }
}

HB_FUNC( ZIPSETACT)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   PHB_ITEM pBlock = hb_param( 2,HB_IT_BLOCK ) ;

   if( pBlock )
   {
      //TraceLog( NULL, "Block: %p\n", pBlock->item.asBlock.value );
      //pBlock->item.asBlock.value->ulCounter++;
      //hb_gcLock( pBlock->item.asBlock.value );
      //ZipSetActBlock( ( void * ) oZip, ( void * ) pBlock->item.asBlock.value );
      ZipSetActBlock( ( void * ) oZip, ( void * ) hb_itemNew( pBlock ) );
   }
}

/* function ZIPSETBUFFER
return
0 on sucess
-1 invalid handle
*/

HB_FUNC( ZIPSETBUFFER )
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   int  iWriteBuffer   = ISNUM( 2 ) ? hb_parni( 2 ) : 65536 ;
   int  iGeneralBuffer = ISNUM( 3 ) ? hb_parni( 3 ) : 65536 ;
   int  iSearchBuffer  = ISNUM( 4 ) ? hb_parni( 4 ) : 32768 ;
   if (ZipSetAdvance( (void *) oZip, iWriteBuffer, iGeneralBuffer,  iSearchBuffer ))
   {
      hb_retni( 0 ) ;
      return;
   }

      hb_retni( -1 ) ;

}

HB_FUNC( ZIPSETGLOBALCOMMENT )
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   const char * szComment = ISCHAR( 2 ) ? hb_parc ( 2 ) : NULL ;
   ZipSetGlobalComment( (void * ) oZip , szComment );
}

/* function ZIPSETFILEPATH
return 0 ok
return -1 invalid handle
*/

HB_FUNC( ZIPSETFILEPATH )
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   BOOL bPath = ISLOG( 2 ) ? hb_parl( 2 )  : 0 ;
   hb_retni ( ZipSetFilePath( ( void *) oZip, bPath) );
}

/* Xharbour Zip File Properties Get Function  */

/* function ZIPGETCOMPRESSLEVEL
return -2 error handle
other files compress level(-1 is an valid compress level)
*/


HB_FUNC( ZIPGETCOMPRESSLEVEL )
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   hb_retni ( ZipHandle_GetCompressLevel( ( void * ) oZip  )) ;
}

HB_FUNC(ZIPGETPASSWORD)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   hb_retc( ZipHandle_GetPassWord( ( void * ) oZip ) );
}

HB_FUNC(ZIPGETREADONLY)
{
  LONG oZip = ( LONG ) hb_parnl( 1 );
  hb_retl( ZipHandle_GetReadOnly( (void * ) oZip ) );
}

HB_FUNC( ZIPGETEXTRACTPATH )
{
  LONG oZip = ( LONG ) hb_parnl( 1 );

  hb_retc( ZipHandle_GetExtractPath( ( void * ) oZip ) );
}

HB_FUNC ( ZIPGETONDISK )
{
   LONG oZip = ( LONG ) hb_parnl( 1 ) ;
   PHB_ITEM pBlock = hb_itemParam( 2 ) ;
   ZipSetDisk( ( void * )oZip, ( void * ) pBlock );
}

HB_FUNC( ZIPGETACT)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   PHB_ITEM pBlock = hb_itemParam( 2 );
   ZipSetActBlock( ( void * ) oZip, ( void * ) pBlock );
}

/* Function ZIPGETBUFFER
return
0 on success
-1 invalid handle
*/

HB_FUNC( ZIPGETBUFFER )
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   int  iWriteBuffer   ;
   int  iGeneralBuffer ;
   int  iSearchBuffer  ;
   int iReturn = 0 ;
   if (ZipGetAdvance( (void *) oZip, &iWriteBuffer, &iGeneralBuffer,  &iSearchBuffer ))
   {
      hb_storni(iWriteBuffer,2);
      hb_storni(iGeneralBuffer,3);
      hb_storni(iSearchBuffer,4);
   }
   else
   {
      iReturn = -1;
   }

   hb_retni (iReturn );
}

HB_FUNC( ZIPGETGLOBALCOMMENT )
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   hb_retc( ZipAchive_GetGlobalcomment( (void * ) oZip ) );
}


HB_FUNC( ZIPGETFILEPATH )
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   hb_retl ( ZipHandle_GetWithPath( ( void * ) oZip ));

}

HB_FUNC( ZIPGETLASTERROR)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   hb_retni( ZipGetLastError( (void * ) oZip ) );
}


HB_FUNC(ZIPSETROOTPATH)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   ZipHandle_ZipSetRoot( (void * ) oZip, hb_parc( 2 ) );
}

HB_FUNC(ZIPSETTEMPPATH)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   ZipHandle_ZipSetTempPath( (void * ) oZip, hb_parc( 2 ) );
}

HB_FUNC(ZIPGETTEMPPATH)
{
   LONG oZip = ( LONG ) hb_parnl( 1 );
   hb_retc( ZipHandle_ZipGetTempPath( (void * ) oZip ) );
}

HB_FUNC(ZIPHASPASS)
{
   hb_retni( ZipHandle_HasPassWord( hb_parc( 1 ) ) ) ;
}


HB_FUNC(ZIP_TESTPK)
{
   hb_retni( hb_CheckSpanMode( hb_parc(1) ));
}

/* Function ZIPGETVOLUMESIZE
return -1 error handle
otherwise volume size
*/
HB_FUNC( ZIPGETVOLUMESIZE )
{
   LONG oZip = ( LONG ) hb_parnl( 1) ;
   hb_retni( ZipHandle_GetVolumeSize( ( void * ) oZip));
}

/* Function ZIPSETVOLUMESIZE
return -1 error handle

*/
HB_FUNC( ZIPSETVOLUMESIZE )
{
   LONG oZip = ( LONG ) hb_parnl( 1) ;
   hb_retni( ZipHandle_SetVolumeSize( ( void * ) oZip , hb_parni( 2 ) ) );
}
