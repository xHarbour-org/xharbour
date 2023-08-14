#ifndef __ZIP_C_H__
#define __ZIP_C_H__
#include "stdafx.h"
#ifdef __WIN32__
#include "windows.h"
#include "ZipExport.h"
#else
#include "ZipExport.h"
#endif
#ifdef __cplusplus
extern "C"  {
#endif
/* typedef for xharbour callbacks */

   typedef int( * pDiskCallBack )( DWORD , void *) ;
   typedef int( * pActCallBack )( DWORD, DWORD, void *) ;

/* Prototype for Zip Create functions */

   ZIP_API void *    ZipAchive_New( char *szFile, char *szPassWord, BOOL bOverWrite,
                         BOOL bFileExist);
   ZIP_API void *   ZipAchive_NewPkSpan( char *szFile, char *szPassWord, BOOL bOverWrite,
                               BOOL bFileExist, void * pDisk );
   ZIP_API void *   ZipAchive_NewTdSpan( char *szFile, char *szPassWord, BOOL bOverWrite,
                               BOOL bFileExist, int iSpanSize );

/* Prototype for misc functions */

   ZIP_API void   ZipAchive_SetGlobalcomment( void * oZip , const char *szComment);
   ZIP_API BOOL   ZipArchive_Close( void * oZip ) ;
   ZIP_API void   ZipArchive_AddnNewFile( void * oZip , char * szFile, BOOL bPath,
                                BOOL bDrive, int iCompLevel);
   ZIP_API void   ZipArchive_SetDiskCallBack(void * pZip, pDiskCallBack p, void * pHarbBlock);
   ZIP_API void   ZipArchive_SetActCallBack(void * oZip, pActCallBack p, void * pHarbBlock);
   ZIP_API void*  SpanCallbackc_New( pDiskCallBack  p, void * pHarbBlock);
   ZIP_API void   SpanCallbackc_End( void * oSpan );

   ZIP_API int    ZipArchive_WithPassWord( char * szFile, void * szDisk );
   ZIP_API int    ZipArchive_GetCount( char *szFile ,void * pDisk ) ;
   ZIP_API void*  ZipArchive_Open ( char * szFile, void * pDisk, int uiMode, 
                  
	       char *szPassWord, void * pHarbBlock, BOOL bReadOnly );

   ZIP_API int   ZipArchive_GetFileCount( void * oZip);
   ZIP_API int   hb_CheckSpanMode( const char * szFile );
   ZIP_API void  ZipArchive_GetFileInfo(void * oZip,char **szFileName,
                               DWORD * dwMethod,DWORD * dwFileCompressed,
                               DWORD * dwFileUnCompressed, DWORD * dwFlag,
                               DWORD * dwFileTime, DWORD * dwFileData,
                               DWORD * dwAttr, DWORD * dwCrc, BOOL *bEncrypt,
                               WORD uiCount);
   ZIP_API int  ZipArchive_FindFile( void * oZip, char * szFile, BOOL bCase );                 
   ZIP_API void ZipArchive_ExtractFile( void * oZip, WORD wPos, char * szFile, 
                                BOOL bPath );
   ZIP_API char * ZipArchive_GetFileName( void *oZip, WORD wPos) ;
   ZIP_API int  ZipArchive_FileCount( void * oZip );

   /* Zip Handle Functions */
   ZIP_API void ZipHandle_SetDiskBlock( void *oZip, void * pDisk );

   ZIP_API void * ZipHandle_New( void );
   ZIP_API int ZipHandle_Open( void *pZip, const char *szFile,int  iDefault );
   ZIP_API int ZipHandle_SetOverWrite( void * oZip, BOOL bOverWrite ) ;

   ZIP_API BOOL ZipHandle_GetOverWrite( void * oZip) ;

   ZIP_API int  ZipHandle_SetWithPath( void * oZip, BOOL bWithPath ) ;

   ZIP_API BOOL ZipHandle_GetWithPath( void * oZip ) ;

   ZIP_API int  ZipHandle_SetWithDrive( void * oZip, BOOL bWithDrive ) ;

   ZIP_API BOOL ZipHandle_GetWithDrive( void * oZip ) ;

   ZIP_API int  ZipHandle_SetVolumeSize( void * oZip, int iVolumeSize ) ;

   ZIP_API int  ZipHandle_GetVolumeSize( void * oZip ) ;

   ZIP_API void ZipHandle_SetActBlock( void *oZip, void * pDisk ,pActCallBack p) ;

   ZIP_API int  ZipHandle_GetWriteBuffer(void * oZip ) ;
   ZIP_API int  ZipHandle_GetGeneralBuffer(void * oZip ) ;

   ZIP_API int  ZipHandle_GetSearchBuffer(void * oZip ) ;

   ZIP_API void * ZipHandle_GetDiskBlock( void * oZip ) ;

   ZIP_API void * ZipHandle_GetActBlock( void * oZip ) ;

   ZIP_API int   ZipHandle_Create( void *oZip, const char *szFile,int  iDefault );
   ZIP_API int   ZipHandle_AddNewFile(  void * oZip , const char * szFile);
   ZIP_API int   ZipHandle_SetAdvance( void *oZip, int iWriteBuffer, int iGeneralBuffer, int iSearchBuffer );
   ZIP_API int   ZipHandle_GetAdvance( void *oZip, int *iWriteBuffer, int *iGeneralBuffer, int *iSearchBuffer );
   ZIP_API void  ZipHandle_SetPassWord( void *pZip, const char * szPassWord);
   ZIP_API void  ZipHandle_SetReadOnly( void * oZip, BOOL bReadOnly );
   ZIP_API BOOL  ZipHandle_GetReadOnly( void * oZip);
   ZIP_API void  ZipStringArray_Add( void * pZip, char * szFile );
   ZIP_API int   ZipHandle_DeleteFile( void * pZip );
   ZIP_API char* ZipHandle_GetExtractPath( void *pZip );
   ZIP_API void  ZipHandle_SetExtractPath( void *pZip, const char * szPath );   
   ZIP_API int   ZipHandle_DeleteAll( void * pZip );
   ZIP_API char* ZipHandle_GetPassWord( void *pZip );
   ZIP_API char* ZipAchive_GetGlobalcomment( void * pZip );
   ZIP_API void  ZipHandle_SetReadOnly( void * pZip, BOOL bReadOnly );
   ZIP_API void  ZipStringArray_Add( void * pZip, char * szFile );
   ZIP_API int   ZipHandle_DeleteFile( void * pZip);

   ZIP_API char* ZipHandle_GetExtractPath( void *pZip );
   ZIP_API int   ZipHandle_Extract( void * pZip );
   ZIP_API int   ZipHandle_DeleteAll( void * pZip );
   ZIP_API char* ZipHandle_GetPassWord( void *pZip );
   ZIP_API BOOL  ZipHandle_GetReadOnly( void * pZip);
   ZIP_API char* ZipAchive_GetGlobalcomment( void * pZip );
   ZIP_API BOOL  ZipHandle_HasPassWord( const char * szFile);

   ZIP_API int ZipHandle_SetCompressLevel( void * pZip, int iCompressLevel);
   ZIP_API int ZipHandle_GetCompressLevel( void * pZip );
   ZIP_API int ZipHandle_GetLastError(void * pZip);
   ZIP_API void ZipHandle_ZipSetRoot(void *pZip,const char * szPath);
   ZIP_API void ZipHandle_ZipSetTempPath(void *pZip,const char * szPath);
   ZIP_API char * ZipHandle_ZipGetTempPath(void *pZip);
#ifdef __cplusplus
}
#endif
#endif
