/*
 * $Id: zipnew.cpp,v 1.1 2002/07/03 02:59:07 lculik Exp $
 */

/*
 * Harbour Project source code:
 * Zlib low level interface for Harbour
 *
 * Copyright 2000-2001 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "hbzip2.h"
#include "hbapifs.h"
char  szTempTime[80];
PHB_ITEM pArray=NULL;

PHB_ITEM pChangeDiskBlock;
extern PHB_ITEM pProgressInfo;
extern int iTotal;
//PHB_ITEM pProgressInfo=NULL;
//int iTotal=0;

int hb_CheckSpamMode(char * szFile);
/* hb_itemRelease(pChangeDiskBlock); */
#ifdef __cplusplus
extern "C" {
bool hb_SetCallBack(DWORD iNumber, int , void* pData);
/*bool hb_SetProgress(DWORD , int iSoFar, void* pData);*/
extern bool     hb_SetProgressofTdSpan(DWORD , int iSoFar, void* pData);
bool hb_SetProgressofUnc(DWORD , int iSoFar, void* pData);
HB_ZIP_INTERNAL pZipI;
#endif
class SpanCallback : public CZipSpanCallback
{
 bool Callback(int iProgress)
 {
      PHB_ITEM pDisk=hb_itemPutNL(NULL,m_uDiskNeeded);
      bool iReturn=true;
      hb_vmEvalBlockV( pChangeDiskBlock, 1,pDisk );
      hb_itemRelease(pDisk);
      return iReturn;
 }
};

class SpanActionCallback : public CZipActionCallback
{
 bool Callback(int iProgress)
 {
      int iReturn=1;
      PHB_ITEM pDisk;
      PHB_ITEM pTotal =hb_itemPutNL(NULL,m_uTotalToDo);
      pDisk=  hb_itemPutNL(NULL,m_uTotalSoFar);
      hb_vmEvalBlockV( pProgressInfo, 2,pDisk,pTotal);
      hb_itemRelease(pDisk);
      hb_itemRelease(pTotal);
      return iReturn;    
 }
};

int  hb_CmpPkSpan(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];
    CZipArchive szZip;    

    uLong uiPos;
    DWORD dwSize;
    BOOL bReturn=true;


    BOOL bFileExist=hb_fsFile((BYTE*)szFile);
    CZipString szArchive = szFile;
    SpanCallback span;
    SpanActionCallback spanac;
    szZip.SetSpanCallback(&span);
//    szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    try {
        if (bFileExist && bOverWrite){
              szZip.Open(szArchive/*szFile*/,CZipArchive::zipCreateSpan/* CZipArchive::zipCreate*/,0);
        }
        else{
            if (!bFileExist) {
                szZip.Open(szArchive/*szFile*/,CZipArchive::zipCreateSpan/* CZipArchive::zipCreate*/,0);
             }           
             else {

             return false;
            }
        }

      }
    catch (CZipException  e)
	{

    bReturn=false;
	}
     catch(...){}
    if (pZipI.szComment != NULL) {
        szZip.SetGlobalComment(pZipI.szComment);
        hb_xfree(pZipI.szComment);
}
     if (HB_IS_BLOCK(pProgress)){
        pProgressInfo=pProgress;
        szZip.SetCallback(&spanac);
        }

        for (uiCount=1;(uiCount<= hb_arrayLen(pArray)) ;uiCount++)
        {
                const char *szDummy = (char *)hb_arrayGetCPtr(pArray,uiCount) ;
                BOOL bAdded=false;
                dwSize=GetCurrentFileSize(szDummy);
                uiPos=uiCount;
                
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,hb_arrayGetCPtr(pArray,uiCount));
                   PHB_ITEM pFilePos=hb_itemPutNI(NULL,uiCount);
                   hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
                   hb_itemRelease(pFileName);
                   hb_itemRelease(pFilePos);
                }
                
                try {
                    if (szPassWord != NULL){
                        szZip.SetPassword(szPassWord);
                     }
                     if (uiPos== hb_arrayLen(pArray))
                                iTotal+=dwSize;


                     #if defined(__WIN32__) || defined(__MINGW32__)
                     if (bDrive && !bAdded ) {
                        szZip.AddNewFileDrv(szDummy, iCompLevel, true,CZipArchive::zipsmSafeSmart,65536);
                        bAdded = true;
                        }
                     #endif
                     if (bPath && !bAdded ) {
                        szZip.AddNewFile(szDummy, iCompLevel, true,CZipArchive::zipsmSafeSmart,65536);
                           bAdded = true;
                           }
                     else if (!bDrive && !bPath && !bAdded ){
                        szZip.AddNewFile(szDummy, iCompLevel, false,CZipArchive::zipsmSafeSmart,65536);
                        }
                      

                      if (uiPos== hb_arrayLen(pArray))
                                            iTotal-=dwSize;
                      else
                        iTotal+=dwSize;
                

                     }
    
                catch(...){}
              
      }
    try {
    szZip.Close();
    }
    catch (CZipException* e)
	{

    bReturn=false;
	}
     catch(...){}
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
     if (pProgressInfo)
        hb_itemRelease(pProgressInfo    );

    return  bReturn;  /* to avoid warning */
}


PHB_ITEM hb___GetFilesNamesFromZip(char *szFile,BOOL iMode)
{
        char szFileNameinZip[_POSIX_PATH_MAX];
        int iNumbersOfFiles;
        int iReturn=true;
        CZipArchive szZip;
        int uiCount;
        SpanCallback span;


int iOMode=hb_CheckSpamMode(szFile);
if (pZipI.iWrite>0) {
    szZip.SetAdvanced(pZipI.iWrite,pZipI.iExtract,pZipI.iRead);

}
    
     try {
        if(iOMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iOMode ==-1) {

                szZip.SetSpanCallback(&span);
//                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iOMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }
    catch (...)    {	}

if (iReturn) {
        iNumbersOfFiles=szZip.GetCount();
        pArray=hb_itemArrayNew( iNumbersOfFiles );
        time_t theTime;
        tm *SzTime;

 for(uiCount=0;uiCount<iNumbersOfFiles;uiCount++)
 {
		CZipFileHeader fh;
      PHB_ITEM pItem;
        szZip.GetFileInfo(fh, (WORD)uiCount);
        
                if (iMode)
                {
                    
                    const char *  szFileNameInZip;
                    CZipString szTempString;
                    PHB_ITEM pTempArray=hb_itemArrayNew(9);
                    char szAttr[5];
                    char szTime[5];
                    char *szMethod;
                    char szCRC[8];
                    int iRatio;
                    int iMeth=fh.m_uMethod;

                  int iCount;
                  int iiCount=0;
                  DWORD uAttr = fh.GetSystemAttr();
                    szTempString =(LPCTSTR)fh.GetFileName();                  
                    szFileNameInZip=(const char *)szTempString;
                    pItem=hb_itemPutC(NULL,(char *)szFileNameInZip);
                    hb_arraySet(pTempArray,filePos,pItem);
                    hb_itemRelease(pItem);
      #if defined(__WIN32__)
        szAttr[0] = uAttr & FILE_ATTRIBUTE_READONLY ? _T('r') : _T('-');
        szAttr[1] = uAttr & FILE_ATTRIBUTE_HIDDEN ? _T('h') : _T('-');
        szAttr[2] = uAttr & FILE_ATTRIBUTE_SYSTEM ? _T('s') : _T('w');
        szAttr[3] = (uAttr & FILE_ATTRIBUTE_DIRECTORY) ? _T('D') : uAttr & FILE_ATTRIBUTE_ARCHIVE ? _T('a'): _T('-');
        #endif
        szAttr[4] = fh.IsEncrypted() ? _T('*') : _T(' ');

                      if (fh.m_uUncomprSize>0) {

                        pItem=hb_itemPutNL(NULL,fh.m_uUncomprSize);
                        hb_arraySet(pTempArray,Lenght,pItem);
                        hb_itemRelease(pItem);
                        pItem=hb_itemPutNL(NULL,fh.m_uComprSize);
                        hb_arraySet(pTempArray,Size,pItem);
                        hb_itemRelease(pItem);
                        iRatio=100-((fh.m_uComprSize*100)/fh.m_uUncomprSize);
                        if (iRatio <0){
                            iRatio=0;
                            }
                        pItem=hb_itemPutNL(NULL,iRatio);
                        hb_arraySet(pTempArray,Ratio,pItem);
                        hb_itemRelease(pItem);
                        }
                        else {
                        pItem=hb_itemPutNL(NULL,fh.m_uUncomprSize);
                        hb_arraySet(pTempArray,Lenght,pItem);
                        hb_itemRelease(pItem);
                        pItem=hb_itemPutNL(NULL,fh.m_uComprSize);
                        hb_arraySet(pTempArray,Size,pItem);
                        hb_itemRelease(pItem);
                        iRatio=0;
                        pItem=hb_itemPutNL(NULL,iRatio);
                        hb_arraySet(pTempArray,Ratio,pItem);
                        hb_itemRelease(pItem);
                        }
#if defined(__WIN32__)
        if (iMeth==0  || uAttr & FILE_ATTRIBUTE_DIRECTORY) {
                  szMethod="Stored";
            }
#endif
        if (iMeth==Z_DEFLATED)       {
            uInt iLevel=(uInt)((fh.m_uFlag & 0x6)/2);
            if (iLevel==0)                           {
                    szMethod="DeflatN";
                    }
            else if (iLevel==1) {
                    szMethod="DeflatX";
                    }
            else if ((iLevel==2) || (iLevel==3)) {
                                    szMethod="DeflatF";
                    }
           else {
                  szMethod="Unknow";
                  }
            }
                    pItem=hb_itemPutC(NULL,szMethod);
                    hb_arraySet(pTempArray,Method,pItem);
                    hb_itemRelease(pItem);
            
                        sprintf(szCRC,"%8.8lx\n",(uLong)fh.m_uCrc32);

                        pItem=hb_itemPutCL(NULL,szCRC,8);
                        hb_arraySet(pTempArray,Crc32,pItem);
                        hb_itemRelease(pItem);

                        pItem=hb_itemPutD(NULL,(long) (fh.m_uModDate >> 9) +1980 ,     (long)  ((fh.m_uModDate & ~0xFE00) >> 5) ,(long)fh.m_uModDate & ~0xFFE0);
                       /* (long)file_info.tmu_date.tm_year  ,(long)file_info.tmu_date.tm_mon + 1,(long)file_info.tmu_date.tm_mday);*/
                        hb_arraySet(pTempArray,Date,pItem);
                        hb_itemRelease(pItem);
                        theTime=fh.GetTime();
                        SzTime= localtime(&theTime);
                        hb_____GetTime(SzTime);


                      for(iCount=10;iCount<16;iCount++) {
                         if( (iCount>10) && (iCount<16)) {
                                szTime[iiCount]=szTempTime[iCount];
                                iiCount++;
                            }
                        }
                    pItem=hb_itemPutCL(NULL,szTime,5);
                    hb_arraySet(pTempArray,Time,pItem);
                    hb_itemRelease(pItem);
                    pItem=hb_itemPutCL(NULL,szAttr,5);
                    hb_arraySet(pTempArray,Attr,pItem);
                    hb_itemRelease(pItem);
               hb_arraySet(pArray,uiCount+1,pTempArray);
                    hb_itemRelease(pTempArray);
                    
                }   
                else  {
                    const char *  szFileNameInZip;
                    CZipString szTempString=(LPCTSTR)fh.GetFileName();
                    szFileNameInZip=(const char *)szTempString;
                pItem=hb_itemPutC(NULL,(char *) szFileNameInZip);
                hb_arraySet(pArray,uiCount+1,pItem);
                hb_itemRelease(pItem);
                }
}
}
            szZip.Close();
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
//  hb_itemReturn(  pArray);
return pArray;
}

 char *hb___CheckFile( char * szFile)
    {
        int uiCount,uiLen;
        int uiDot_Found=0;
        uiLen=strlen(szFile);

        for (uiCount=0;uiCount<uiLen;uiCount++)
            if (szFile[uiCount]=='.')
                uiDot_Found=1;

        if (uiDot_Found==0)
            strcat(szFile,".zip");

        return szFile;

    }


 void  hb_____GetTime(struct tm *tz)
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
  strcpy(szTempTime, asctime(&t));

}
BOOL hb_IsPassWord(char *szFile)
{

CZipArchive szZip;

int iMode=hb_CheckSpamMode(szFile);

bool bReturn=true;
                SpanCallback span;
		CZipFileHeader fh;
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
                     }
        else {
            if (iMode ==-1) {

                szZip.SetSpanCallback(&span);

//                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                            }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                              }
                else {
                    bReturn =false;
                     }
                  }
             }
    }
    catch (CZipException& e)    { }


szZip.GetFileInfo(fh, (WORD)0);
if (fh.IsEncrypted()){
    bReturn=true;
    }
szZip.Close();
return bReturn;
}


int hb___GetNumbersofFilestoUnzip(char *szFile)
{
        int iNumbersOfFiles;
        CZipArchive szZip;
        SpanCallback span;
        szZip.SetSpanCallback(&span);

        szZip.Open(szFile,CZipArchive::zipOpen,0);
        iNumbersOfFiles=szZip.GetCount();
        szZip.Close();
        if (pChangeDiskBlock){
          hb_itemRelease(pChangeDiskBlock);
       }

        return iNumbersOfFiles;
}
            
int  hb_CmpPkSpanStd(char *szFile,char *szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];
    CZipArchive szZip;    

    DWORD dwSize;
    BOOL bReturn=true;
    BOOL bFileExist=hb_fsFile((BYTE*)szFile);
    BOOL bAdded;
    SpanCallback span;
    SpanActionCallback spanac;
    szZip.SetSpanCallback(&span);

//    szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    try {
        if (bFileExist && bOverWrite){
              szZip.Open(szFile,CZipArchive::zipCreateSpan,0);
        }
        else{
            if (!bFileExist) {
                szZip.Open(szFile,CZipArchive::zipCreateSpan,0);
             }           
             else {

             return false;
            }
        }

      }

    catch (CZipException* e)
	{

    bReturn=false;
	}
     catch(...){}
    if (pZipI.szComment != NULL){
        szZip.SetGlobalComment(pZipI.szComment);
        hb_xfree(pZipI.szComment);
        }

     if (HB_IS_BLOCK(pProgress)){
        pProgressInfo=pProgress;
        szZip.SetCallback(&spanac);
        }

                try {
                     dwSize=GetCurrentFileSize(szFiletoCompress);
                if(pBlock !=NULL){
                   PHB_ITEM pFileName=hb_itemPutC(NULL,szFiletoCompress );
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);

                }

                    if (szPassWord != NULL){
                        szZip.SetPassword(szPassWord);
                     }
                     #if defined(__WIN32__) || defined(__MINGW32__)
                     if (bDrive && !bAdded ) {
                        szZip.AddNewFileDrv(szFiletoCompress, iCompLevel, true,CZipArchive::zipsmSafeSmart,65536);
                        bAdded =true;
                        }
                     #endif
                     if (bPath && !bAdded ) {
                        szZip.AddNewFile(szFiletoCompress, iCompLevel, true,CZipArchive::zipsmSafeSmart,65536);
                           bAdded =true;
                           }
                     if (!bDrive && !bPath && !bAdded ){
                        szZip.AddNewFile(szFiletoCompress, iCompLevel, false,CZipArchive::zipsmSafeSmart,65536);
                        bAdded =true;
                        }

                     iTotal+=dwSize;
                     }
    
                catch(...){}
              
      
    try {
    szZip.Close();
    }
    catch (CZipException* e)
	{

    bReturn=false;
	}
     catch(...){}
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
     if (pProgressInfo)
        hb_itemRelease(pProgressInfo    );
    
    return     bReturn;  /* to avoid warning */
}


int hb___SetCallbackFunc(PHB_ITEM pFunc)
{
pChangeDiskBlock=pFunc;
pZipI.pItem=pFunc;
return true;
}
bool hb_SetCallBack(DWORD iNumber, int , void* pData)
{
    PHB_ITEM pDisk=hb_itemPutNL(NULL,iNumber);
    bool iReturn=true;
    HB_SYMBOL_UNUSED( pData );
    hb_vmEvalBlockV( pChangeDiskBlock, 1,pDisk );
    hb_itemRelease(pDisk);
    return iReturn;    

}
/*bool hb_SetProgress(DWORD , int iSoFar, void* pData){
    CProgressInfo* p = static_cast<CProgressInfo*>(pData);
	iSoFar += p->m_iTotalSoFar;
	//iTotal = p->m_iTotal;

	p->m_pProgress->SetPos(iSoFar);
	p->m_pProgress->RedrawWindow();
	return true;

return TRUE;
}           */
int hb_UnzipAll(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pDiskBlock,PHB_ITEM pProgress){
bool iReturn=true;
int uiCount=0;

int iMode;
CZipArchive szZip;
BOOL bChange=FALSE;
    SpanCallback span;
    SpanActionCallback spanac;
iTotal=0;
if (pDiskBlock){
    pChangeDiskBlock=pDiskBlock;
}
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
iMode=hb_CheckSpamMode(szFile);
     if (HB_IS_BLOCK(pProgress)){
        pProgressInfo=pProgress;
        szZip.SetCallback(&spanac);
        }

     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
                     }
        else {
            if (iMode ==-1) {

    szZip.SetSpanCallback(&span);

//                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                            }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                              }
                else {
                    iReturn =false;
                     }
                  }
             }
    }
    catch (CZipException& e)    {

	}

 if (iReturn) {

    for (uiCount=0;uiCount<(int)szZip.GetCount();uiCount++){
		CZipFileHeader fh;
        const char *  szFileNameInZip;
        CZipString szTempString;        

        szZip.GetFileInfo(fh, (WORD)uiCount);
            PHB_FNAME pOut;
              szTempString =(LPCTSTR)fh.GetFileName();                  
              szFileNameInZip=(const char *)szTempString;
              pOut=hb_fsFNameSplit( ( char * ) szFileNameInZip );
              if (szPath==NULL){
                  szPath=(char*)pOut->szDrive;
                  pOut->szDrive="";
                  hb_fsFNameMerge( (char*)szFileNameInZip, pOut );
                  bChange=TRUE;
                  }
               hb_xfree( pOut);
               iTotal=fh.m_uUncomprSize        ;
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   PHB_ITEM pFilePos=hb_itemPutNI(NULL,uiCount);
                   hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
                   hb_itemRelease(pFileName);
                   hb_itemRelease(pFilePos);
                }

        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {
         
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);

            }
            else
            {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);
            }
            
        }
    catch (CZipException& e)
	{

	}
        if(bChange) {
        bChange=FALSE;
        szPath=NULL;
        }

    }

    }
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
     if (pProgressInfo)
        hb_itemRelease(pProgressInfo    );

return iReturn;
}
int   hb_UnzipOne(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,char *szFiletoExtract,PHB_ITEM pProgress)
{
bool iReturn=true;
int uiCount;

int iMode;
iTotal=0;
   CZipArchive szZip;
    SpanCallback span;
    SpanActionCallback spanac;
    szZip.SetSpanCallback(&span);

//    szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
iMode=hb_CheckSpamMode(szFile) ;
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {

    szZip.SetSpanCallback(&span);


                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }

    catch (CZipException& e)    {

	}
/*        if (iCause != 0){
            szZip.Close();
    }*/
     if (HB_IS_BLOCK(pProgress)){
        pProgressInfo=pProgress;
        szZip.SetCallback(&spanac);
        }

        uiCount = szZip.FindFile((LPCTSTR)szFiletoExtract,false);
        if (uiCount ==-1){
        uiCount = szZip.FindFile((LPCTSTR)szFiletoExtract,true);
        }
        if (uiCount >=0){
		CZipFileHeader fh;
        const char *  szFileNameInZip;
        PHB_FNAME pOut;
        CZipString szTempString;        
        szZip.GetFileInfo(fh, (WORD)uiCount);
        szTempString =(LPCTSTR)fh.GetFileName();

        iTotal=fh.m_uUncomprSize;
        szFileNameInZip=(const char *)szTempString;
               pOut=hb_fsFNameSplit( ( char * ) szFileNameInZip );
              if (szPath==NULL){
                  szPath=(char*)pOut->szDrive;
                  pOut->szDrive="";
                  hb_fsFNameMerge( (char*)szFileNameInZip, pOut );
                  }
                hb_xfree( pOut);
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }
   
        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {

            szZip.SetPassword(szPassWord);

            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);
            }
            else {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);

            }
        }
    catch (CZipException& e)
	{

	}

    }
    szZip.Close();
    if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
     if (pProgressInfo)
        hb_itemRelease(pProgressInfo    );

return iReturn;
}

int   hb_DeleteOne(char *szFile,char *szFiletoDelete)
{
bool iReturn=true;
int uiCount;

   CZipArchive szZip;
int iMode=hb_CheckSpamMode(szFile);
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1 ||iMode == -2) {
                    iReturn =false;
             }
        }
     }

    catch (CZipException e)    {

	}
        uiCount = szZip.FindFile((LPCTSTR)szFiletoDelete,false);
        if (uiCount ==-1){
        uiCount = szZip.FindFile((LPCTSTR)szFiletoDelete,true);
        }
        if (uiCount >=0){
		CZipFileHeader fh;
        szZip.GetFileInfo(fh, (WORD)uiCount);
        try{
           szZip.DeleteFile((WORD)uiCount);
         }
			catch (...)
			{
            iReturn = false;        
			}
        }
            szZip.Close();
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

return iReturn;
}

int   hb_DeleteSel(char *szFile,PHB_ITEM pArray,BOOL bCase)
{
    bool iReturn=true;
    int uiCount;

    CZipArchive szZip;
    CZipStringArray  aFiles;
    int iMode=hb_CheckSpamMode(szFile);
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1 ||iMode == -2) {
                    iReturn =false;
             }
             }
    }

    catch (CZipException e)    {

	}
    if (iReturn){
    for (uiCount=1;(uiCount<= (int)hb_arrayLen(pArray)) ;uiCount++) {
        const char *szDummy = (char *)hb_arrayGetCPtr(pArray,uiCount) ;
        aFiles.Add(szDummy);
    }
        szZip.DeleteFiles(aFiles);
}
    szZip.Close();
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

    return iReturn;
}


int hb_UnzipSel(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pSelArray,PHB_ITEM pProgress)
{
bool iReturn=true;
int uiCount;
int iCause;
CZipArchive szZip;
int iMode=hb_CheckSpamMode(szFile);
iTotal=0;
BOOL bChange=FALSE;
LPCTSTR lpFiletoExtract;
    SpanCallback span;
    SpanActionCallback spanac;

    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
     if (HB_IS_BLOCK(pProgress)){
        pProgressInfo=pProgress;
        szZip.SetCallback(&spanac);
        }

     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {

    szZip.SetSpanCallback(&span);

//                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }
    catch (CZipException* e)	{

	}
    if (iReturn)  {
        for (iCause=1;(iCause<= (int)hb_arrayLen(pSelArray)) ;iCause++){
        lpFiletoExtract=hb_arrayGetC(pSelArray,iCause);
        uiCount = szZip.FindFile((LPCTSTR)lpFiletoExtract,false);
        if (uiCount ==-1){
        uiCount = szZip.FindFile((LPCTSTR)lpFiletoExtract,true);
        }
        if (uiCount >=0){
		CZipFileHeader fh;
        const char *  szFileNameInZip;
        CZipString szTempString;
        PHB_FNAME pOut;
        szZip.GetFileInfo(fh, (WORD)uiCount);
        szTempString =(LPCTSTR)fh.GetFileName();                  
        szFileNameInZip=(const char *)szTempString;
               pOut=hb_fsFNameSplit( ( char * ) szFileNameInZip );
              if (szPath==NULL){
                  szPath=(char*)pOut->szDrive;
                  pOut->szDrive="";
                  hb_fsFNameMerge( (char*)szFileNameInZip, pOut );
                  bChange=TRUE;
                  }
           hb_xfree( pOut);
         iTotal=fh.m_uUncomprSize   ;
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }

        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {

            szZip.SetPassword(szPassWord);

            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);
            }
            else {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);
            }
        }
    catch (CZipException* e)
	{

	}
      if (bChange) {
      bChange=FALSE;
      szPath="";
      }
        }
}
    }
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
     if (pProgressInfo)
        hb_itemRelease(pProgressInfo    );

    szZip.Close();
return iReturn;
}

int  hb_TestForPKS(char *szFile)
{
    return hb_CheckSpamMode(szFile);
}
void hb_SetZipBuff(int a,int b,int c)
{
if (a && b && c){
   pZipI.iWrite = a>= 65535  ?  a : 65535;
   pZipI.iExtract = b>=16384 ? b : 16384;
   pZipI.iRead= c >=32768 ? c : 32768;
}
}
void hb_SetZipComment(char *szComment)
{
int iLen=strlen((const char *)szComment)+1;
pZipI.szComment=(char*)hb_xgrab(iLen);
strcpy(pZipI.szComment,szComment);
/*pZipI.szComment=hb_itemGetC(pComment);*/
}
const char * hb_GetZipComment(char *szFile)
{
const char *szReturn;
char *szTempR;
bool iReturn=true;
CZipString szTemp;

CZipArchive szZip;
int iMode=hb_CheckSpamMode(szFile);
    SpanCallback span;
    SpanActionCallback spanac;

     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {

    szZip.SetSpanCallback(&span);

//                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }
    catch (CZipException* e)	{

	}
if (iReturn) {
    szTemp=szZip.GetGlobalComment();
    szReturn=(const char *) szTemp;
}
szTempR=(char*)hb_xgrab(strlen((const char*)szReturn)+1);
   strcpy(szTempR,(char*)szReturn);
    if (pChangeDiskBlock)
      hb_itemRelease(pChangeDiskBlock);
    

szZip.Close();
return  szTempR;

}

int   hb_UnzipOneIndex(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,int uiCount,PHB_ITEM pProgress)
{
bool iReturn=true;

int iMode=hb_CheckSpamMode(szFile);
iTotal=0;
uiCount--;
    CZipArchive szZip;
    SpanCallback span;
    SpanActionCallback spanac;
    szZip.SetSpanCallback(&span);

//    szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);

    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
     /*
    try {
        if (hb_CheckSpamMode(szFile) !=-2) {
           szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else{            
               szZip.Open(szFile,CZipArchive::zipOpen,1);
            }
      }
*/
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {

    szZip.SetSpanCallback(&span);

//                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }

    catch (CZipException& e)    {

	}
     if (HB_IS_BLOCK(pProgress)){
        pProgressInfo=pProgress;
        szZip.SetCallback(&spanac);
        }

        if (uiCount >=0){
        CZipFileHeader fh;
        const char *  szFileNameInZip;
        CZipString szTempString;        
        szZip.GetFileInfo(fh, (WORD)uiCount);
        szTempString =(LPCTSTR)fh.GetFileName();
        iTotal=fh.m_uUncomprSize;
        szFileNameInZip=(const char *)szTempString;
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }
   
        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {

            szZip.SetPassword(szPassWord);

            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);
            }
            else {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);

            }
        }
    catch (CZipException& e)
	{

	}

    }
    szZip.Close();
    if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
     if (pProgressInfo)
        hb_itemRelease(pProgressInfo    );

return iReturn;
}
int hb_UnzipSelIndex(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pSelArray,PHB_ITEM pProgress)
{
bool iReturn=true;
int uiCount;
int iCause;
CZipArchive szZip;
int iMode=hb_CheckSpamMode(szFile);
    SpanCallback span;
    SpanActionCallback spanac;
iTotal=0;

    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
     if (HB_IS_BLOCK(pProgress)){
        pProgressInfo=pProgress;
        szZip.SetCallback(&spanac);
        }

     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {

    szZip.SetSpanCallback(&span);

//                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }
    catch (CZipException* e)	{

	}
    if (iReturn)  {
        for (iCause=0;(iCause<= (int)hb_arrayLen(pSelArray)) ;iCause++){
        uiCount= hb_arrayGetNI(pSelArray,iCause)-1;
        if (uiCount >=0){
		CZipFileHeader fh;
        const char *  szFileNameInZip;
        CZipString szTempString;        
        szZip.GetFileInfo(fh, (WORD)uiCount);
        szTempString =(LPCTSTR)fh.GetFileName();                  
        szFileNameInZip=(const char *)szTempString;
         iTotal=fh.m_uUncomprSize   ;
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }

        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {

            szZip.SetPassword(szPassWord);

            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);
            }
            else {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,65536);
            }
        }
    catch (CZipException* e)
	{

	}

        }
}
    }
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
     if (pProgressInfo)
        hb_itemRelease(pProgressInfo    );

    szZip.Close();
return iReturn;
}
BOOL hb_TransferFilesFromzip(char *szSource,char *szDest,PHB_ITEM pArray)
{
   CZipArchive szZSource;
   CZipArchive szZDest;
   CZipStringArray  aFiles;
   int uiCount;
   BOOL bReturn=true;
   BOOL bReturn1=true;
   
   int iMode=hb_CheckSpamMode(szSource);
   int iMode1=hb_CheckSpamMode(szDest);
   try {
      if(iMode==0) 
         szZSource.Open(szSource,CZipArchive::zipOpen,0);
      
      else 
         if (iMode ==-1 ||iMode == -2) 
            bReturn =false;
      
   }
   catch (CZipException &e)
       {	}
   try {
      if(iMode1==0) 
         szZDest.Open(szDest,CZipArchive::zipOpen,0);
      
      else 
         if (iMode ==-1 ||iMode == -2) 
            bReturn1 =false;
         
}
  catch (CZipException &e)    { }
   if (bReturn && bReturn1) {
    for (uiCount=1;(uiCount<= (int)hb_arrayLen(pArray)) ;uiCount++) {
        const char *szDummy = (char *)hb_arrayGetCPtr(pArray,uiCount) ;
        aFiles.Add(szDummy);
    }
   if (szZDest.GetFromArchive(szZSource,aFiles,false))
      bReturn =true;
   
   szZDest.Close();
   szZSource.Close();
   return true;
   }
   else
      return false;      
}


int   hb_DeleteOneIndex(char *szFile,int uiCount)
{
   bool iReturn=true;

   CZipArchive szZip;
   int iMode=hb_CheckSpamMode(szFile);
   (int)uiCount--;

   try {
      if(iMode==0) {
         szZip.Open(szFile,CZipArchive::zipOpen,0);
      }
      else {
         if (iMode ==-1 ||iMode == -2) {
            iReturn =false;
         }
      }
   }

   catch (CZipException &e)    {

	}
   if (uiCount >=0){
      CZipFileHeader fh;
      szZip.GetFileInfo(fh, (WORD)uiCount);
      try
      {
      szZip.DeleteFile((WORD)uiCount);
      }
      catch(...) {
         iReturn = false;
      }
   }
   szZip.Close();
   if (pChangeDiskBlock){
      hb_itemRelease(pChangeDiskBlock);
   }

   return iReturn;
}


DWORD GetCurrentFileSize(   LPCTSTR szFile)
#if defined(HB_OS_WIN_32) || defined(__MINGW32__)
{

   DWORD dwFlags=FILE_ATTRIBUTE_ARCHIVE;
   HANDLE hFind;
   WIN32_FIND_DATA  hFilesFind;

            hFind = FindFirstFile(szFile,&hFilesFind);
                  if (hFind != INVALID_HANDLE_VALUE){
                      if (dwFlags & hFilesFind.dwFileAttributes) {
                         if(hFilesFind.nFileSizeHigh>0)
                              return ((hFilesFind.nFileSizeHigh*MAXDWORD)+hFilesFind.nFileSizeLow);    
                         else
                              return (hFilesFind.nFileSizeLow);
                       }

         }

   FindClose(hFind);
   return -1;
   }
#elif defined(__GNUC__)


{
   USHORT   ushbMask = 63;
   USHORT   usFileAttr = HB_FA_ARCHIVE;
   struct stat sStat;
   if (stat(szFile,&sStat )!=-1){
           return sStat.st_size;
    }
    return -1;
}

#endif

#ifdef __cplusplus
}
#endif

int hb_CheckSpamMode(char * szFile)
{
   CZipArchive szZip;
   int iReturn ;
    SpanCallback span;
    SpanActionCallback spanac;
    szZip.SetSpanCallback(&span);

//   szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
   try{  szZip.Open(szFile,CZipArchive::zipOpen,0);}
   catch(CZipException &e) {
      if (e.m_iCause == CZipException::cdirNotFound) {
         szZip.Close(true);
         iReturn=114;
         return iReturn;
      }
     if (e.m_iCause == CZipException::noCallback) {
         szZip.Close(true);
         iReturn=103;
         return iReturn;  
     }
   }
    iReturn =szZip.GetSpanMode();
    szZip.Close();
    return iReturn;
}
bool hb_SetProgressofUnc(DWORD, int iSoFar, void* pData){
      
      int iReturn=1;
      PHB_ITEM pDisk;
      PHB_ITEM pTotal =hb_itemPutNL(NULL,iTotal);
      HB_SYMBOL_UNUSED( pData );
      pDisk=  hb_itemPutNL(NULL,iSoFar);
                 hb_vmEvalBlockV( pProgressInfo, 2,pDisk,pTotal);
                   hb_itemRelease(pDisk);
                   hb_itemRelease(pTotal);

      return iReturn;    

}
