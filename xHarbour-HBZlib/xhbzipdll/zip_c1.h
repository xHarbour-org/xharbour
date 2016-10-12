#ifndef __ZIP_C1_H__
#define __ZIP_C1_H__

#include "ZipArchive.h"
#ifdef __WIN32__
#include "windows.h"
#endif
#include "stdafx.h"
#ifdef __cplusplus
extern "C"  {
#endif

   typedef int( * pDiskCallBack )( DWORD , void *) ;
   typedef int( * pActCallBack )( DWORD, DWORD, void *) ;

#ifdef __cplusplus
}
#endif

typedef struct _my_stuct
{
   BOOL bOverWrite;
   int  iVolumeSize;
   BOOL bWithPath;
   BOOL bWithDrive;
   int  iCompressLevel;
   BOOL bReadOnly;
  

} HB_INFO,*PHB_INFO;     

   #define ZIP_ST 0
   #define ZIP_PK 1
   #define ZIP_TD 2
   #define ZIP_ALL 3
   #define ZIP_ACTION_CREATE 1
   #define ZIP_ACTION_OPEN   2

   // Error codes
   #define ERROR_FILEADD_NOT_ALLOW 600
   #define ERROR_FILEDELETE_NOT_ALLOW 601

class HCZipArchive : public CZipArchive
{
    public:
    HB_INFO hInfo;
    HCZipArchive() ;
    ~HCZipArchive() ;  
    int iOperAllow;
    int iAction ;
    void * pChangeDiskBlock;
    void * pProgressBlock ;
    char *szExtractPath;
    char *szComment;
    CZipStringArray cArray;
    int  iLastError;
    int iExtractPos;
};

HCZipArchive::HCZipArchive ()
{
   hInfo.bOverWrite     =  0 ;
   hInfo.iVolumeSize    =  0 ;
   hInfo.bWithPath      =  0 ;
   hInfo.bWithDrive     =  0 ;
   hInfo.iCompressLevel = -1 ;
   hInfo.bReadOnly      =  0 ;
   pChangeDiskBlock     = NULL ;
   pProgressBlock       = NULL ;
   iOperAllow           = ZIP_ALL ;
   iAction              = 0 ;
   szExtractPath        = NULL ;
   szComment            = NULL ;
   iLastError           = 0 ;
   iExtractPos          = 0;

}
HCZipArchive::~HCZipArchive ()
{
   if ( szExtractPath )
   {
      free( szExtractPath );
   }
   if (szComment )
   {
      free( szComment ) ;
   }
      
   
   
}

#endif

