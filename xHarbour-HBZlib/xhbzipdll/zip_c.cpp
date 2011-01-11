/*
 * $Id$
 */

/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Luiz Rafael Culik Guimaraes culikr@brturbo.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/
#ifdef __WIN32__
#define ZIP_HAS_DLL 1
#define ZIP_BUILD_DLL
#endif

#include "zip_c.h"

#include "zip_c1.h"
#include "ZipArchive.h"
#ifdef __WIN32__
#include "windows.h"
#else
   #undef ULONG                           /* 4 or 8 bytes unsigned */
   typedef unsigned long ULONG;
#endif
#include "stdafx.h"

/* internal functions*/

class SpanCallbackc : public CZipSpanCallback
{

   bool Callback( int iProgress )
   {
      pDiskCallBack p = ( pDiskCallBack  ) pCargoFunc;
      p( m_uDiskNeeded, pCargoData );

      return TRUE;
   }
   public:
   void * pCargoData;
   void * pCargoFunc;

};

class SpanActionCallbackc : public CZipActionCallback
{
   bool Callback( int iProgress )
   {
      bool iReturn    = true;
      pActCallBack p = ( pActCallBack  ) pCargoFunc;
      p( m_uTotalToDo, m_uTotalSoFar, pCargoData ) ;
      return iReturn;
   }
   public:
   void * pCargoData;
   void * pCargoFunc;

};

extern "C" ZIP_API int hb_CheckSpanMode( const char * szFile )
{
   int iReturn = 0;

   HCZipArchive pZip;
   SpanCallbackc span;
   SpanActionCallbackc spanac;

   #ifdef WIN32   
      DWORD  dwAttr = GetFileAttributes( szFile )  ;
      BOOL bReadOnly = ( dwAttr & FILE_ATTRIBUTE_READONLY );      
   #else   
      BOOL bReadOnly = 0 ;   
   #endif

   pZip.SetSpanCallback( &span );


   try
   {
      pZip.Open( szFile, bReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
   }

   catch( CZipException &e )
   {
      if (e.m_iCause   == 2 )
         iReturn= 101;
      if ( e.m_iCause == CZipException::cdirNotFound )
      {
         pZip.Close( true );
         iReturn = 114;
      }
      else if ( e.m_iCause == CZipException::noCallback )
      {
         pZip.Close( true );
         iReturn = 103;
      }
      else if ( e.m_iCause == CZipException::cdirNotFound )
      {
         pZip.Close( true );
         iReturn = 116;
      }
      else if ( e.m_iCause == CZipException::badZipFile )
      {
         pZip.Close( true );
         iReturn = 101;
      }
      
      else if ( e.m_iCause == CZipException::badCrc )
      {
         pZip.Close( true );
         iReturn = 103;
      }

      else if ( e.m_iCause == 101 )
      {
         pZip.Close( true );
         iReturn = 101;
      }
      else
      {
         pZip.Close( true );
         iReturn = e.m_iCause;

      }


   }

   if ( ! iReturn )
   {
      iReturn = pZip.GetSpanMode( );
      pZip.Close( );
   }

   return iReturn;
}


/* C function to Create standart Zip File */
extern "C" ZIP_API void * ZipAchive_NewTdSpan(char *szFile, char *szPassWord, BOOL bOverWrite,BOOL bFileExist,int iSpanSize)
{
   HCZipArchive * pZip = new HCZipArchive;
   BOOL bReturn=TRUE;

   if ( iSpanSize  == 0 )
   {
      iSpanSize = 1457664;
   }

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         pZip->Open( szFile, CZipArchive::zipCreateSpan, iSpanSize );
      }
      else
      {
         bReturn = FALSE;
      }
   }

   catch ( CZipException &e )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   if ( bReturn )
   {

      if ( szPassWord != NULL )
      {
         pZip->SetPassword( szPassWord );
      }

      return ( void * ) pZip ;
   }

    return ( void * ) NULL;
}

extern "C" ZIP_API void * ZipAchive_NewPkSpan(char *szFile, char *szPassWord, BOOL bOverWrite,BOOL bFileExist, void * szDisk)
{
   HCZipArchive * pZip = new HCZipArchive;
   BOOL bReturn=TRUE;
   SpanCallbackc * span = ( SpanCallbackc * ) szDisk;
   pZip->SetSpanCallback( span );

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         pZip->Open( szFile, CZipArchive::zipCreateSpan, 0 );
      }
      else
      {
         bReturn = FALSE;
      }
   }

   catch ( CZipException &e )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   if ( bReturn )
   {

      if ( szPassWord != NULL )
      {
         pZip->SetPassword( szPassWord );
      }

      return ( void * ) pZip ;
   }

    return ( void * ) NULL;
}

extern "C" ZIP_API void * ZipAchive_New(char *szFile, char *szPassWord, BOOL bOverWrite,BOOL bFileExist)
{
   HCZipArchive * pZip = new HCZipArchive;
   BOOL bReturn=TRUE;
   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         pZip->Open( szFile, CZipArchive::zipCreate, 0 );
      }
      else
      {
         pZip->Open( szFile, CZipArchive::zipOpen, 0 );
      }
   }

   catch ( CZipException &e )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   if ( bReturn )
   {

      if ( szPassWord != NULL )
      {
         pZip->SetPassword( szPassWord );
      }

      return ( void * ) pZip ;
   } 

    return ( void * ) NULL;
}

 /* Misc Funcs */
extern "C" ZIP_API void ZipAchive_SetGlobalcomment( void * pZip ,const char *szComment)
{

   (( HCZipArchive *) pZip)->SetGlobalComment( szComment ) ;
}

extern "C" ZIP_API char * ZipAchive_GetGlobalcomment( void * pZip )
{
   CZipString s = ( LPCTSTR )(( HCZipArchive *) pZip)->GetGlobalComment( ) ;
//   LPTSTR r= s.GetBuffer(255);
//   printf("%s\r\n",r);
   if ( (( HCZipArchive *) pZip)->szComment  == NULL  && s )
   {
      (( HCZipArchive *) pZip)->szComment = ( char * ) malloc ( s.GetLength() );
      strcpy( (( HCZipArchive *) pZip)->szComment , (LPCTSTR)s );
   }
   else
   {
      return (char * ) "";
   }

   return ( char *) (( HCZipArchive *) pZip)->szComment ; 
}


extern "C" ZIP_API void ZipArchive_AddnNewFile(  void * pZip , char * szFile, BOOL bPath, BOOL bDrive, int iCompLevel)
{

   int Res;
   CZipAddNewFileInfo zanfi ( szFile, false);
   zanfi.m_iComprLevel = iCompLevel;
   zanfi.m_iSmartLevel = CZipArchive::zipsmSafeSmart;
   zanfi.m_nBufSize = 65536 ;
   Res = (( HCZipArchive *) pZip)->FindFile(szFile, false );
   if (Res == -1 )
   {
      Res = (( HCZipArchive *) pZip)->FindFile(szFile, true );
   }

   zanfi.m_iReplaceIndex = Res;

   try
   {

      if ( bDrive )
      {
         (( HCZipArchive *) pZip)->m_bRemoveDriveLetter = false;
         zanfi.m_bFullPath = true;
         (( HCZipArchive *) pZip)->AddNewFile( zanfi );

//         (( HCZipArchive *) pZip)->AddNewFile( szFile, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
      }
      if ( bPath )
      {
         zanfi.m_bFullPath = true;
         (( HCZipArchive *) pZip)->AddNewFile( zanfi );

  //       (( HCZipArchive *) pZip)->AddNewFile( szFile, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
      }
      else if ( !bPath && !bDrive )
      {
         zanfi.m_bFullPath = false;
         (( HCZipArchive *) pZip)->AddNewFile( zanfi );

//         (( HCZipArchive *) pZip)->AddNewFile( szFile, iCompLevel, false, CZipArchive::zipsmSafeSmart, 65536 );
      }
  
   }

   catch( CZipException &e )
   {
      (( HCZipArchive *) pZip)->iLastError = e.m_iCause ;
      (( HCZipArchive *) pZip)->CloseNewFile(1);
   }
}

extern "C" ZIP_API BOOL ZipArchive_Close( void * pZip )
{

   BOOL bReturn = TRUE;

   try
   {
      (( HCZipArchive *) pZip)->Close();
   }


   catch ( CZipException &e )
   {
        printf ("Error while processing archive %s\n",  (LPCTSTR)e.GetErrorDescription());
		if (e.m_szFileName.IsEmpty())
			printf("\n");
		else
			printf("Filename in error object: %s\n\n", (LPCTSTR)e.m_szFileName);
        (( HCZipArchive *) pZip)->Close(true);

    

      bReturn = TRUE;
   }

   catch( ... ){}


   if ( bReturn )
   {
      delete (( HCZipArchive *) pZip) ; 
      pZip = NULL;
   }


   return bReturn;

}

extern "C" ZIP_API void ZipArchive_SetDiskCallBack(void * pZip, pDiskCallBack p, void * pHarbBlock)
{                                                                               
   SpanCallbackc *span = new  SpanCallbackc;
   if ( p )
   {
      span->pCargoFunc = (void * )p;
      span->pCargoData = pHarbBlock;
      (( HCZipArchive *) pZip)->pChangeDiskBlock = pHarbBlock ;
      (( HCZipArchive *) pZip)->SetSpanCallback( span );
   }
}
extern "C" ZIP_API void ZipArchive_SetActCallBack(void * pZip, pActCallBack p, void * pHarbBlock)
{
   SpanActionCallbackc *span = new SpanActionCallbackc();
   if ( p )
   {
      span->pCargoFunc = (void * )p;
      span->pCargoData = pHarbBlock ;
      (( HCZipArchive *) pZip)->pProgressBlock = pHarbBlock ;
      (( HCZipArchive *) pZip)->SetCallback( span );
   }
}

extern "C" ZIP_API void *    SpanCallbackc_New( pDiskCallBack  p, void * pHarbBlock)
{

  SpanCallbackc *s = new SpanCallbackc();
  s->pCargoFunc = ( void * ) p ;
  s->pCargoData = pHarbBlock ;
  return (void * ) s ;

}

extern "C" ZIP_API void   SpanCallbackc_End( void * oSpan )
{

  SpanCallbackc *s = (SpanCallbackc *) oSpan ;

  if ( s )
     delete s; 

}



extern "C" ZIP_API int ZipArchive_WithPassWord( char * szFile, void * szDisk )

{
   BOOL bReturn=TRUE;
   SpanCallbackc * span = ( SpanCallbackc * ) szDisk;
   CZipFileHeader fh;
   HCZipArchive pZip;

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            pZip.Open( szFile, CZipArchive::zipOpen, 0 );
            break;

         case -1:
            pZip.SetSpanCallback( span );
            pZip.Open( szFile, CZipArchive::zipOpen, 0 );
            break;

         case -2:
            pZip.Open( szFile, CZipArchive::zipOpen, 1 );
            break;

         default:
            bReturn = FALSE;
      }
   }
   catch ( CZipException& e )   {}

   if ( bReturn )
   {
      pZip.GetFileInfo( fh, ( WORD )0 );

      bReturn = fh.IsEncrypted();

      pZip.Close( );
   }

   return bReturn;
}

extern "C" ZIP_API int ZipArchive_GetCount( char *szFile ,void * pDisk )
{
   int iNumberOfFiles;

   HCZipArchive pZip;
   SpanCallbackc * span = ( SpanCallbackc * ) pDisk;
   pZip.SetSpanCallback( span );

   pZip.Open( szFile, CZipArchive::zipOpen, 0 );
   iNumberOfFiles = pZip.GetCount( );
   pZip.Close( );

   return iNumberOfFiles;
}


extern "C" ZIP_API void * ZipArchive_Open( char * szFile, void * pDisk, int uiMode,
                                   char * szPassWord, void * pHarbBlock,
                                   BOOL bReadOnly )
{
   

   HCZipArchive *pZip = new HCZipArchive;
   BOOL bReturn = TRUE;

   try
   {
      switch( uiMode )
      {
         case 0:
            pZip->Open( szFile, bReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            {
               SpanCallbackc  span ;

               if ( pHarbBlock )
               {
                  span.pCargoData = pHarbBlock ;
                  pZip->SetSpanCallback( &span );
               }

               pZip->Open( szFile, bReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            }
            break;

         case -2:
            pZip->Open( szFile, bReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            bReturn = false;
      }
   }

   catch ( ... )   { }

   if ( bReturn )
   {
      if ( szPassWord != NULL )
      {
         pZip->SetPassword( szPassWord );
      }

      return ( void * ) pZip  ;
   }      

   return NULL;
}
   
extern "C" ZIP_API int ZipArchive_GetFileCount( void * pZip)
{
   return  (( HCZipArchive *) pZip)->GetCount();
}

extern "C" ZIP_API void ZipArchive_GetFileInfo(void * pZip,char **szFileName,
                                       DWORD * dwMethod,DWORD * dwFileCompressed,
                                       DWORD * dwFileUnCompressed, DWORD * dwFlag,
                                       DWORD * dwFileTime, DWORD * dwFileData,
                                       DWORD * dwAttr,DWORD * dwCrc,BOOL *bEncrypt,
                                       WORD uiCount)
{
   CZipFileHeader fh;
   const char * szFileNameInZip;
   CZipString szTempString;

   (( HCZipArchive *) pZip)->GetFileInfo( fh, ( WORD )uiCount );
   szTempString  = ( LPCTSTR )fh.GetFileName( );
   szFileNameInZip = ( const char * ) szTempString;

   memcpy(*szFileName , (char *)szFileNameInZip,strlen((char *)szFileNameInZip));
   *dwMethod = fh.m_uMethod;
   *dwFileCompressed = fh.m_uComprSize ;
   *dwFileUnCompressed = fh.m_uUncomprSize;
   *dwFlag = fh.m_uFlag; 
   *dwFileTime = fh.GetTime( ) ;
   *dwFileData = fh.m_uModDate ;
   *dwAttr = fh.GetSystemAttr() ;
   *dwCrc = fh.m_uCrc32;
   *bEncrypt = fh.IsEncrypted();
}



extern "C" ZIP_API int ZipArchive_FindFile( void * pZip, char * szFile, BOOL bCase)
{
   int Res;

   Res = (( HCZipArchive *) pZip)->FindFile(szFile, bCase );

   return Res;
}

extern "C" ZIP_API void ZipArchive_ExtractFile( void *pZip, WORD wPos, char *szFile, BOOL bPath )
{
   try
   {
   (( HCZipArchive *) pZip)->ExtractFile( wPos, szFile, bPath, NULL, 65536 );
   }
   catch( CZipException &e )
   {
      (( HCZipArchive *) pZip)->CloseFile( NULL, true);
   }
}


extern "C" ZIP_API char * ZipArchive_GetFileName( void *pZip, WORD wPos)
{
   CZipFileHeader fh;
   CZipString szTempString;
   const char *  szFileNameInZip;

   (( HCZipArchive *) pZip)->GetFileInfo( fh, wPos );
   szTempString  = ( LPCTSTR )fh.GetFileName( );
   szFileNameInZip = (const char * )  szTempString ;

   return (char * ) szFileNameInZip;
}

extern "C" ZIP_API int ZipArchive_FileCount( void * pZip)
{
   int iNumberOfFiles;

   iNumberOfFiles =(( HCZipArchive *) pZip)->GetCount( );

   return iNumberOfFiles;
}

extern "C" ZIP_API  void * ZipHandle_New( void )
{
   HCZipArchive * pZip = new HCZipArchive;

   if ( ! pZip )
   {
     return NULL ;
   }

   return ( void * ) pZip ;
}


extern "C" ZIP_API  int ZipHandle_SetOverWrite( void * pZip, BOOL bOverWrite )
{
   if (! pZip)
      return -1;

   (( HCZipArchive *) pZip)->hInfo.bOverWrite = bOverWrite ;

   return 0;
}

extern "C" ZIP_API  BOOL ZipHandle_GetOverWrite( void * pZip)
{
   return (( HCZipArchive *) pZip)->hInfo.bOverWrite ;
}

extern "C" ZIP_API  int ZipHandle_SetWithPath( void * pZip, BOOL bWithPath )
{
   if (! pZip)
      return -1;

   (( HCZipArchive *) pZip)->hInfo.bWithPath = bWithPath ;
   return 0;
}

extern "C" ZIP_API  BOOL ZipHandle_GetWithPath( void * pZip )
{
   return (( HCZipArchive *) pZip)->hInfo.bWithPath ;
}


extern "C" ZIP_API  int ZipHandle_SetCompressLevel( void * pZip, int iCompressLevel)
{
   if (! pZip)
      return -1;

   (( HCZipArchive *) pZip)->hInfo.iCompressLevel = iCompressLevel ;
   return 0;
}

extern "C" ZIP_API  int ZipHandle_GetCompressLevel( void * pZip )
{
   if (! pZip)
      return -2;
   
   return (( HCZipArchive *) pZip)->hInfo.iCompressLevel ;

}

extern "C" ZIP_API  int ZipHandle_SetWithDrive( void * pZip, BOOL bWithDrive )
{
   if (! pZip)
      return -1;

   (( HCZipArchive *) pZip)->hInfo.bWithDrive = bWithDrive ;
   return 0;
}

extern "C" ZIP_API  BOOL ZipHandle_GetWithDrive( void * pZip )
{
   return (( HCZipArchive *) pZip)->hInfo.bWithDrive ;
}


extern "C" ZIP_API  int ZipHandle_SetVolumeSize( void * pZip, int iVolumeSize )
{
   if (! pZip)
      return -1;

   (( HCZipArchive *) pZip)->hInfo.iVolumeSize = iVolumeSize ;
   return 0 ;
}

extern "C" ZIP_API  int ZipHandle_GetVolumeSize( void * pZip )
{
   if (! pZip)
      return -1;

   return (( HCZipArchive *) pZip)->hInfo.iVolumeSize ;
}

extern "C" ZIP_API  void ZipHandle_SetDiskBlock( void *pZip, void * pDisk )
{
   SpanCallbackc Span;

   Span.pCargoData = pDisk;
   (( HCZipArchive *) pZip)->pChangeDiskBlock = pDisk ;
   (( HCZipArchive *) pZip)->SetSpanCallback( &Span );
}

extern "C" ZIP_API  void ZipHandle_SetActBlock( void *pZip, void * pDisk, pActCallBack p)
{
   SpanActionCallbackc Span;
   Span.pCargoFunc = (void * ) p;
   Span.pCargoData = pDisk;
   (( HCZipArchive *) pZip)->pProgressBlock = pDisk ;
   (( HCZipArchive *) pZip)->SetCallback( &Span );

}

extern "C" ZIP_API  int ZipHandle_GetWriteBuffer(void * pZip )
{
   int iWriteBuffer ;


   (( HCZipArchive *) pZip)->GetAdvanced(&iWriteBuffer,NULL,NULL);

   return iWriteBuffer ;
}
extern "C" ZIP_API  int ZipHandle_GetGeneralBuffer(void * pZip )
{
   int iGeneralBuffer ;

   (( HCZipArchive *) pZip)->GetAdvanced(NULL,&iGeneralBuffer,NULL);

   return iGeneralBuffer ;
}

extern "C" ZIP_API  int ZipHandle_GetSearchBuffer(void * pZip )
{
   int iSearchBuffer ;

   (( HCZipArchive *) pZip)->GetAdvanced(NULL,NULL,&iSearchBuffer);

   return iSearchBuffer ;
}

extern "C" ZIP_API  void * ZipHandle_GetDiskBlock( void * pZip )
{
  return  (( HCZipArchive *) pZip)->pChangeDiskBlock ;
}

extern "C" ZIP_API  void * ZipHandle_GetActBlock( void * pZip )
{
  return (( HCZipArchive *) pZip)->pProgressBlock ;
}

extern "C" ZIP_API  int ZipHandle_Create( void *pZip, const char *szFile,int  iDefault )
{
   int iReturn = 0;

   if (! pZip)
      return -1;

   (( HCZipArchive *) pZip)->iAction = ZIP_ACTION_CREATE ;
   (( HCZipArchive *) pZip)->iOperAllow = ZIP_ALL ;
   try
   {
      switch ( iDefault )
      {
      case 1:
         (( HCZipArchive *) pZip)->Open(szFile,CZipArchive::zipCreateSpan,0 );
         break;
      case 2:
         (( HCZipArchive *) pZip)->Open(szFile,CZipArchive::zipCreateSpan,(( HCZipArchive *) pZip)->hInfo.iVolumeSize );
         break;
      default:
         (( HCZipArchive *) pZip)->Open(szFile,CZipArchive::zipCreate,0 );
         break;
      }
    }
   catch ( CZipException &e )
   {
       (( HCZipArchive *) pZip)->iLastError = e.m_iCause;
       if ( e.m_iCause == CZipException::cdirNotFound )
       {
          (( HCZipArchive *) pZip)->Close( true );
          iReturn = -1;

       } else if ( e.m_iCause == CZipException::noCallback ) 
       {
          (( HCZipArchive *) pZip)->Close( true );
          iReturn = -1;
       }
       else
       {
          (( HCZipArchive *) pZip)->Close( true );
          iReturn = -1;

       }

   }
   
   return iReturn;
}

extern "C" ZIP_API  int ZipHandle_Open( void *pZip, const char *szFile,int  iDefault )
{

   int iReturn = 0;
   if (! pZip)
      return -1;

   (( HCZipArchive *) pZip)->iAction = ZIP_ACTION_OPEN ;
   (( HCZipArchive *) pZip)->iOperAllow = iDefault ;
   try
   {
     switch ( iDefault )
     {
     case 1:
        (( HCZipArchive *) pZip)->Open(szFile, (( HCZipArchive *) pZip)->hInfo.bReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
        break;
     case 2:
        (( HCZipArchive *) pZip)->Open(szFile, (( HCZipArchive *) pZip)->hInfo.bReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, (( HCZipArchive *) pZip)->hInfo.iVolumeSize );
        break;
     default:
        (( HCZipArchive *) pZip)->Open(szFile, (( HCZipArchive *) pZip)->hInfo.bReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
        break;
     }
   }
   catch ( CZipException &e )
   {
      (( HCZipArchive *) pZip)->iLastError = e.m_iCause;
      if ( e.m_iCause == CZipException::cdirNotFound )
      {
         (( HCZipArchive *) pZip)->Close( true );
         iReturn = -1;
      } else if ( e.m_iCause == CZipException::noCallback )
      {
         (( HCZipArchive *) pZip)->Close( true );
         iReturn = -1;
      }

 
   }
 
   return iReturn;
}


extern "C" ZIP_API int ZipHandle_AddNewFile(  void * pZip , const char * szFile)
{

   int Res;
   int iRet = 0;
   CZipAddNewFileInfo zanfi ( szFile, false);

   if (! pZip)
      return -3;


   if ( (( HCZipArchive *) pZip)->iAction == ZIP_ACTION_OPEN )
   {
      if ( (( HCZipArchive *) pZip)->iOperAllow  ==  ZIP_PK )
      {
         (( HCZipArchive *) pZip)->iLastError = ERROR_FILEADD_NOT_ALLOW ;
         return -1 ;
      }
   }

   zanfi.m_iComprLevel = (( HCZipArchive *) pZip)->hInfo.iCompressLevel;
   zanfi.m_iSmartLevel = CZipArchive::zipsmSafeSmart;
   zanfi.m_nBufSize = 65536 ;

   Res = (( HCZipArchive *) pZip)->FindFile(szFile, false );
   if (Res == -1 )
   {
      Res = (( HCZipArchive *) pZip)->FindFile(szFile, true );
   }
   zanfi.m_iReplaceIndex = Res;

   try
   {
      if ( (( HCZipArchive *) pZip)->hInfo.bWithDrive)
      {
         (( HCZipArchive *) pZip)->m_bRemoveDriveLetter = false;
//         (( HCZipArchive *) pZip)->AddNewFile( szFile, (( HCZipArchive *) pZip)->hInfo.iCompressLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
         zanfi.m_bFullPath = true;
         (( HCZipArchive *) pZip)->AddNewFile( zanfi ) ;

      }
      if ( (( HCZipArchive *) pZip)->hInfo.bWithPath)
      {
//         (( HCZipArchive *) pZip)->AddNewFile( szFile, (( HCZipArchive *) pZip)->hInfo.iCompressLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
         zanfi.m_bFullPath = true;
         (( HCZipArchive *) pZip)->AddNewFile( zanfi ) ;

      }
      else if ( ! (( HCZipArchive *) pZip)->hInfo.bWithPath && ! (( HCZipArchive *) pZip)->hInfo.bWithDrive )
      {
//         (( HCZipArchive *) pZip)->AddNewFile( szFile, (( HCZipArchive *) pZip)->hInfo.iCompressLevel, false, CZipArchive::zipsmSafeSmart, 65536 );
         zanfi.m_bFullPath = false;
         (( HCZipArchive *) pZip)->AddNewFile( zanfi ) ;

      }
  
   }
   catch( CZipException &e )
   {
      (( HCZipArchive *) pZip)->iLastError = e.m_iCause ;
      (( HCZipArchive *) pZip)->CloseNewFile(1);
      iRet = -2;
   }

   return iRet;
}


extern "C" ZIP_API  int ZipHandle_SetAdvance( void *pZip, int iWriteBuffer, int iGeneralBuffer, int iSearchBuffer )
{
   if (! pZip)
      return -1;

     (( HCZipArchive *) pZip)->SetAdvanced( iWriteBuffer,  iGeneralBuffer, iSearchBuffer );
     return 0 ;
}

extern "C" ZIP_API  int ZipHandle_GetAdvance( void *pZip, int *iWriteBuffer, int *iGeneralBuffer, int *iSearchBuffer )
{
   if (! pZip)
      return -1;

     (( HCZipArchive *) pZip)->GetAdvanced( iWriteBuffer, iGeneralBuffer, iSearchBuffer );
   return 0;
}

extern "C" ZIP_API  void ZipHandle_SetPassWord( void *pZip, const char * szPassWord)
{
   if ( szPassWord )
      (( HCZipArchive *) pZip)->SetPassword( szPassWord );
   else
      (( HCZipArchive *) pZip)->SetPassword( NULL );
}

extern "C" ZIP_API  char * ZipHandle_GetPassWord( void *pZip )
{
   CZipString s = ( LPCTSTR ) (( HCZipArchive *) pZip)->GetPassword();
   const char * r = ( const char * ) s;
   return (char * )r;
}


extern "C" ZIP_API  void ZipHandle_SetReadOnly( void * pZip, BOOL bReadOnly )
{
      (( HCZipArchive *) pZip)->hInfo.bReadOnly =  bReadOnly ;
}


extern "C" ZIP_API  BOOL ZipHandle_GetReadOnly( void * pZip)
{
   return (( HCZipArchive *) pZip)->hInfo.bReadOnly ;
}


extern "C" ZIP_API  void ZipStringArray_Add( void * pZip, char * szFile )
{
   (( HCZipArchive *) pZip)->cArray.Add( szFile ) ;
}

extern "C" ZIP_API  int ZipHandle_DeleteFile( void * pZip)
{

   if ( (( HCZipArchive *) pZip)->iAction == ZIP_ACTION_OPEN )
   {
      if ( (( HCZipArchive *) pZip)->iOperAllow  ==  ZIP_PK )
      {
         (( HCZipArchive *) pZip)->iLastError = ERROR_FILEDELETE_NOT_ALLOW ;
         return -1 ;

      }
   }


   (( HCZipArchive *) pZip)->DeleteFiles( (( HCZipArchive *) pZip)->cArray) ;

   return 0;
}

extern "C" ZIP_API  char * ZipHandle_GetExtractPath( void *pZip )
{
   return  ( char *) (( HCZipArchive *) pZip)->szExtractPath == NULL ? (char*)   "" : ( char *) (( HCZipArchive *) pZip)->szExtractPath ;
}

extern "C" ZIP_API  void ZipHandle_SetExtractPath( void *pZip, const char * szPath )
{
   if ( (( HCZipArchive *) pZip)->szExtractPath  == NULL )
   {
      (( HCZipArchive *) pZip)->szExtractPath = ( char * ) malloc ( 256 );
      strcpy( (( HCZipArchive *) pZip)->szExtractPath , szPath );
   }
   else
   {
      strcpy( (( HCZipArchive *) pZip)->szExtractPath , szPath );
   }
}



extern "C" ZIP_API  int ZipHandle_Extract( void * pZip )
{
   ULONG  ulCount =0;
   ULONG wFiles ;
   if (! pZip)
      return -1;

   (( HCZipArchive *) pZip)->iLastError = 0;

   wFiles = (ULONG)(( HCZipArchive *) pZip)->GetCount( );

   for ( ulCount = 0; ulCount <= (ULONG)wFiles; ulCount ++)
   {
      try
      {
           if (!(( HCZipArchive *) pZip)->ExtractFile( (WORD) ulCount, ( LPCTSTR )(( HCZipArchive *) pZip)->szExtractPath, (( HCZipArchive *) pZip)->hInfo.bWithPath, NULL, 65536 ))
              wFiles++;
      }
      catch( CZipException &e )
      {
         (( HCZipArchive *) pZip)->CloseFile( NULL, true);
      }

   }

   return 0;
}


extern "C" ZIP_API  int ZipHandle_DeleteAll( void * pZip )
{
   CZipWordArray aFiles;
   WORD wPos;

   if ( (( HCZipArchive *) pZip)->iAction == ZIP_ACTION_OPEN )
   {
      if ( (( HCZipArchive *) pZip)->iOperAllow  ==  ZIP_PK )
      {
         (( HCZipArchive *) pZip)->iLastError = ERROR_FILEDELETE_NOT_ALLOW ;
         return -1 ;
      }
   }

   for (wPos = 0 ; wPos < (( HCZipArchive *) pZip)->GetCount(); wPos++)
   {
      aFiles.Add( wPos );
   }

   (( HCZipArchive *) pZip)->DeleteFiles( aFiles ) ;

   return 0;
}


extern "C" ZIP_API  BOOL ZipHandle_HasPassWord( const char * szFile)
{
   BOOL bReturn = TRUE;
   CZipFileHeader *fh =  new CZipFileHeader;

   CZipArchive *szZip  = new CZipArchive;
   SpanCallbackc span;

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip->Open( szFile,  CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip->SetSpanCallback( &span );
            szZip->Open( szFile,  CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip->Open( szFile,  CZipArchive::zipOpen, 1 );
            break;

         default:
            bReturn = FALSE;
      }
   }
   catch ( CZipException& e )   {}

   if ( bReturn )
   {
      szZip->GetFileInfo( (CZipFileHeader&)fh, ( WORD )0 );

      bReturn = fh->IsEncrypted();

      szZip->Close( );
   }

   return bReturn;
}

extern "C" ZIP_API  int ZipHandle_GetLastError(void * pZip)
{
   return (( HCZipArchive *) pZip)->iLastError;
}

extern "C" ZIP_API  void ZipHandle_ZipSetRoot(void *pZip, const char * szPath)
{
   (( HCZipArchive *) pZip)->SetRootPath(szPath);
}

extern "C" ZIP_API  void ZipHandle_ZipSetTempPath(void *pZip,const char * szPath)
{
   (( HCZipArchive *) pZip)->SetTempPath(szPath);
}

extern "C" ZIP_API  char* ZipHandle_ZipGetTempPath(void *pZip)
{
   CZipString s = ( LPCTSTR )(( HCZipArchive *) pZip)->GetTempPath( ) ;
   const char * r = ( const char * ) s;
   return (char * )r; 

}
