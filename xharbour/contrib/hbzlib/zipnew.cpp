/*
 * $Id: zipnew.cpp,v 1.10 2004/02/14 22:49:58 lculik Exp $
 */

/*
 * Harbour Project source code:
 * Zlib low level interface for Harbour
 *
 * Copyright 2000-2003 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or ( at your option )
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
 * Boston, MA 02111-1307 USA ( or visit the web site http://www.gnu.org/ ).
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

char szTempTime[ 80 ];
PHB_ITEM pArray = NULL;

PHB_ITEM pChangeDiskBlock;
extern PHB_ITEM pProgressInfo;


int hb_CheckSpanMode( char * szFile );

#ifdef __cplusplus
extern "C" {

bool hb_SetCallBack( DWORD iNumber, int, void* pData );

extern bool hb_SetProgressofTdSpan( DWORD, int iSoFar, void* pData );

HB_ZIP_INTERNAL pZipI;

#endif

class SpanCallback : public CZipSpanCallback
{
   bool Callback( int iProgress )
   {
      PHB_ITEM pDisk = hb_itemPutNL( NULL, m_uDiskNeeded );

      hb_vmEvalBlockV( pChangeDiskBlock, 1, pDisk );
      hb_itemRelease( pDisk );

      return true;
   }
};

class SpanActionCallback : public CZipActionCallback
{
   bool Callback( int iProgress )
   {
      PHB_ITEM pDisk  = hb_itemPutNL( NULL, m_uTotalSoFar );
      PHB_ITEM pTotal = hb_itemPutNL( NULL, m_uTotalToDo );

      hb_vmEvalBlockV( pProgressInfo, 2, pDisk, pTotal );
      hb_itemRelease( pDisk );
      hb_itemRelease( pTotal );

      return true;
   }
};

int hb_CmpPkSpan( char *szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char *szPassWord, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   ULONG ulCount;

   const char *szDummy;

   BOOL bReturn = TRUE;
   BOOL bAdded;
   BOOL bFileExist = hb_fsFile( ( BYTE* )szFile );
   CZipString szArchive = szFile;

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   szZip.SetSpanCallback( &span );
   bDrive = false;

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         szZip.Open( szArchive, CZipArchive::zipCreateSpan, 0 );
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

   if ( ! bReturn )
   {
      return ( int ) bReturn;
   }

   if ( szPassWord != NULL )
   {
      szZip.SetPassword( szPassWord );
   }

   if ( pZipI.szComment != NULL )
   {
      szZip.SetGlobalComment( pZipI.szComment );
      hb_xfree( pZipI.szComment );
   }

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   for( ulCount = 1;( ulCount <= hb_arrayLen( pArray ) ); ulCount++ )
   {
      szDummy = ( char * )hb_arrayGetCPtr( pArray, ulCount );
      bAdded = FALSE;    

      if( pBlock  !=  NULL )
      {
         PHB_ITEM pFileName = hb_itemPutC( NULL, hb_arrayGetCPtr( pArray, ulCount ) );
         PHB_ITEM pFilePos = hb_itemPutNI( NULL, ulCount );
         hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
         hb_itemRelease( pFileName );
         hb_itemRelease( pFilePos );
      }
      try
      {

         if ( bPath && !bAdded )
         {
            szZip.AddNewFile( szDummy, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
            bAdded = true;
         }
         else if ( !bDrive && !bPath && !bAdded )
         {
            szZip.AddNewFile( szDummy, iCompLevel, false, CZipArchive::zipsmSafeSmart, 65536 );
         }

      }
      catch( ... ){}
   }

   try
   {
      szZip.Close( );
   }

   catch ( CZipException&  e )
   {
      bReturn = FALSE;
   }
   catch( ... ){}

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   return ( int ) bReturn;
}


PHB_ITEM hb___GetFileNamesFromZip( char *szFile, BOOL iMode )
{
   int iNumberOfFiles;
   ULONG ulCount;
   int iOMode = hb_CheckSpanMode( szFile );
   bool iReturn = true;

   CZipArchive szZip;
   SpanCallback span;

   if ( pZipI.iWrite > 0 )
   {
      szZip.SetAdvanced( pZipI.iWrite, pZipI.iExtract, pZipI.iRead );
   }

   try
   {
      switch( iOMode )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( ... )   { }

   if ( iReturn )
   {
      iNumberOfFiles = szZip.GetCount();
      pArray = hb_itemArrayNew( iNumberOfFiles );
      time_t theTime;
      tm *SzTime;

      for( ulCount = 0 ; ulCount < ( ULONG )iNumberOfFiles ; ulCount++ )
      {
         CZipFileHeader fh;
         PHB_ITEM pItem;
         szZip.GetFileInfo( fh, ( WORD )ulCount );

         if ( iMode )
         {
            const char * szFileNameInZip;
            CZipString szTempString;
            PHB_ITEM pTempArray = hb_itemArrayNew( 9 );
            char szAttr[ 5 ];
            char szTime[ 5 ];
            char *szMethod;
            char szCRC[ 8 ];
            int iRatio;
            int iMeth = fh.m_uMethod;
            int iCount;
            int iiCount = 0;
            DWORD uAttr = fh.GetSystemAttr( );

            szTempString  = ( LPCTSTR )fh.GetFileName( );
            szFileNameInZip = ( const char * )szTempString;
            pItem = hb_itemPutC( NULL, ( char * )szFileNameInZip );
            hb_arraySet( pTempArray, filePos, pItem );
            hb_itemRelease( pItem );

            #if defined( __WIN32__ )
               szAttr[ 0 ] = uAttr & FILE_ATTRIBUTE_READONLY ? ( char ) _T( 'r' ) : ( char ) _T( '-' );
               szAttr[ 1 ] = uAttr & FILE_ATTRIBUTE_HIDDEN ? ( char ) _T( 'h' ) : ( char ) _T( '-' );
               szAttr[ 2 ] = uAttr & FILE_ATTRIBUTE_SYSTEM ? ( char ) _T( 's' ) : ( char ) _T( 'w' );
               szAttr[ 3 ] = ( uAttr & FILE_ATTRIBUTE_DIRECTORY ) ? ( char ) _T( 'D' ) : uAttr & FILE_ATTRIBUTE_ARCHIVE ? ( char ) _T( 'a' ): ( char ) _T( '-' );
            #endif

            szAttr[ 4 ] = fh.IsEncrypted( ) ? ( char ) _T( '*' ) : ( char ) _T( ' ' );

            if ( fh.m_uUncomprSize>0 )
            {
               pItem = hb_itemPutNL( NULL, fh.m_uUncomprSize );
               hb_arraySet( pTempArray, Lenght, pItem );
               hb_itemRelease( pItem );
               pItem = hb_itemPutNL( NULL, fh.m_uComprSize );
               hb_arraySet( pTempArray, Size, pItem );
               hb_itemRelease( pItem );
               iRatio = 100-( ( fh.m_uComprSize*100 ) / fh.m_uUncomprSize );

               if ( iRatio <0 )
               {
                  iRatio = 0;
               }

               pItem = hb_itemPutNL( NULL, iRatio );
               hb_arraySet( pTempArray, Ratio, pItem );
               hb_itemRelease( pItem );
            }
            else
            {
               pItem = hb_itemPutNL( NULL, fh.m_uUncomprSize );
               hb_arraySet( pTempArray, Lenght, pItem );
               hb_itemRelease( pItem );
               pItem = hb_itemPutNL( NULL, fh.m_uComprSize );
               hb_arraySet( pTempArray, Size, pItem );
               hb_itemRelease( pItem );
               iRatio = 0;
               pItem = hb_itemPutNL( NULL, iRatio );
               hb_arraySet( pTempArray, Ratio, pItem );
               hb_itemRelease( pItem );
            }

            #if defined( __WIN32__ )
               if ( iMeth == 0  || uAttr & FILE_ATTRIBUTE_DIRECTORY )
               {
                  szMethod = "Stored";
               }
            #endif

            if ( iMeth == Z_DEFLATED )
            {
               UINT iLevel = ( UINT )( ( fh.m_uFlag & 0x6 ) / 2 );

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

            pItem = hb_itemPutC( NULL, szMethod );
            hb_arraySet( pTempArray, Method, pItem );
            hb_itemRelease( pItem );

            sprintf( szCRC, "%8.8lx\n", ( ULONG )fh.m_uCrc32 );

            pItem = hb_itemPutCL( NULL, szCRC, 8 );
            hb_arraySet( pTempArray, Crc32, pItem );
            hb_itemRelease( pItem );

            pItem = hb_itemPutD( NULL, ( LONG ) ( fh.m_uModDate >> 9 ) +1980,( LONG )( ( fh.m_uModDate & ~0xFE00 ) >> 5 ), ( LONG )fh.m_uModDate & ~0xFFE0 );

            hb_arraySet( pTempArray, Date, pItem );
            hb_itemRelease( pItem );
            theTime = fh.GetTime( );
            SzTime =  localtime( &theTime );
            hb_____GetTime( SzTime );

            for( iCount = 10 ; iCount < 16 ; iCount ++ )
            {
               if( ( iCount>10 ) && ( iCount<16 ) )
               {
                  szTime[ iiCount ] = szTempTime[ iCount ];
                  iiCount++;
               }
            }

            pItem = hb_itemPutCL( NULL, szTime, 5 );
            hb_arraySet( pTempArray, Time, pItem );
            hb_itemRelease( pItem );
            pItem = hb_itemPutCL( NULL, szAttr, 5 );
            hb_arraySet( pTempArray, Attr, pItem );
            hb_itemRelease( pItem );
            hb_arraySet( pArray, ulCount+1, pTempArray );
            hb_itemRelease( pTempArray );

         }
         else
         {
            const char *  szFileNameInZip;
            CZipString szTempString = ( LPCTSTR )fh.GetFileName( );
            szFileNameInZip = ( const char * )szTempString;
            pItem = hb_itemPutC( NULL, ( char * ) szFileNameInZip );
            hb_arraySet( pArray, ulCount+1, pItem );
            hb_itemRelease( pItem );
         }
      }
   }

   szZip.Close( );

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   return pArray;
}

char *hb___CheckFile( char * szFile )
{
   ULONG ulCount, ulLen;
   int ulDot_Found = 0;

   ulLen = strlen( szFile );

   /* TODO: This needs to be fixed!  */

   for ( ulCount = 0;ulCount<ulLen;ulCount++ )
   {
      if ( szFile[ ulCount ] == '.' )
      {
         ulDot_Found = 1;
      }
   }

   if ( ulDot_Found == 0 )
   {
      strcat( szFile, ".zip" );
   }

   return szFile;
}

void hb_____GetTime( struct tm *tz )
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

BOOL hb_IsPassWord( char *szFile )
{
   BOOL bReturn = TRUE;
   CZipFileHeader fh;

   CZipArchive szZip;
   SpanCallback span;

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            bReturn = FALSE;
      }
   }
   catch ( CZipException& e )   {}

   if ( bReturn )
   {
      szZip.GetFileInfo( fh, ( WORD )0 );

      bReturn = fh.IsEncrypted();

      szZip.Close( );
   }

   return bReturn;
}

int hb___GetNumberofFilestoUnzip( char *szFile )
{
   int iNumberOfFiles;

   CZipArchive szZip;
   SpanCallback span;

   szZip.SetSpanCallback( &span );

   szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
   iNumberOfFiles = szZip.GetCount( );
   szZip.Close( );

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   return iNumberOfFiles;
}

int hb_CmpPkSpanStd( char *szFile, char *szFiletoCompress, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char *szPassWord, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{

   BOOL bReturn = TRUE;
   BOOL bAdded = FALSE;
   BOOL bFileExist = hb_fsFile( ( BYTE* )szFile );

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   szZip.SetSpanCallback( &span );
   bDrive= false;

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         szZip.Open( szFile, CZipArchive::zipCreateSpan, 0 );
      }
      else
      {
         return ( int ) FALSE;
      }
   }

   catch ( CZipException&  e )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   if ( ! bReturn )
   {
      return ( int ) bReturn;
   }

   if ( szPassWord != NULL )
   {
      szZip.SetPassword( szPassWord );
   }

   if ( pZipI.szComment !=  NULL )
   {
      szZip.SetGlobalComment( pZipI.szComment );
      hb_xfree( pZipI.szComment );
   }

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   try
   {

      if( pBlock  !=  NULL )
      {
          PHB_ITEM pFileName = hb_itemPutC( NULL, szFiletoCompress );
          hb_vmEvalBlockV( pBlock, 1, pFileName );
          hb_itemRelease( pFileName );
      }

//      if ( szPassWord !=  NULL )
//      {
//          szZip.SetPassword( szPassWord );
//      }

/*      #if defined( __WIN32__ ) || defined( __MINGW32__ )
         if ( bDrive && !bAdded )
         {
            szZip.AddNewFileDrv( szFiletoCompress, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
            bAdded  = true;
         }
      #endif*/

      if ( bPath && !bAdded )
      {
         szZip.AddNewFile( szFiletoCompress, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
         bAdded  = true;
      }

      if ( !bDrive && !bPath && !bAdded )
      {
         szZip.AddNewFile( szFiletoCompress, iCompLevel, false, CZipArchive::zipsmSafeSmart, 65536 );
         bAdded  = true;
      }

      
   }

   catch( ... ) {}

   try
   {
      szZip.Close( );
   }

   catch ( CZipException&  e )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   return ( int ) bReturn;
}


int hb___SetCallbackFunc( PHB_ITEM pFunc )
{
   pChangeDiskBlock = pFunc;
   pZipI.pItem = pFunc;

   return ( int ) true;
}

bool hb_SetCallBack( DWORD iNumber, int, void* pData )
{
   PHB_ITEM pDisk = hb_itemPutNL( NULL, iNumber );
   bool iReturn = true;
   HB_SYMBOL_UNUSED( pData );

   hb_vmEvalBlockV( pChangeDiskBlock, 1, pDisk );
   hb_itemRelease( pDisk );

   return iReturn;
}

int hb_UnzipAll( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, char *szPassWord, char *pbyBuffer,  PHB_ITEM pProgress )
{
   bool bWithPath = lWithPath?true:false;
   bool iReturn = true;
   ULONG ulCount = 0;
   int iMode;
   BOOL bChange = FALSE;
   char  * szPath = (char*) hb_xgrab( _POSIX_PATH_MAX + 1 );

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;



   iMode = hb_CheckSpanMode( szFile );

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   try
   {
      switch( iMode )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( CZipException& e ){}

   if ( iReturn )
   {
      if ( szPassWord !=  NULL )
      {
         szZip.SetPassword( szPassWord );
      }

      if ( pbyBuffer )
      { 
         if (hb_stricmp(pbyBuffer,".\\")==0 )
         {
            hb_fsCurDirBuffEx( 0, (BYTE*) szPath, _POSIX_PATH_MAX + 1 );
         }
         else
         {
            strcpy(szPath,pbyBuffer);
         }
         hb_fsChDir((BYTE*)"\\");

         szZip.SetRootPath(szPath);
      }
      for ( ulCount = 0 ; ulCount < ( ULONG ) szZip.GetCount( ) ; ulCount ++ )
      {
         CZipFileHeader fh;
         const char *  szFileNameInZip;
         CZipString szTempString;

         szZip.GetFileInfo( fh, ( WORD )ulCount );
         PHB_FNAME pOut;
         szTempString    = ( LPCTSTR )fh.GetFileName( );
         szFileNameInZip = ( const char * )szTempString;
         pOut = hb_fsFNameSplit( ( char * ) szFileNameInZip );

/*         if ( pbyBuffer == NULL )
         {
            pbyBuffer= ( char* )pOut->szDrive;
            pOut->szDrive = "";
            hb_fsFNameMerge( ( char* )szFileNameInZip, pOut );
            bChange = TRUE;
            strcpy(szPath,pbyBuffer);
         }
         else if (hb_stricmp(pbyBuffer,".\\")==0 )        
         {
          hb_fsFNameMerge( ( char* )szPath, pOut );
            hb_fsCurDirBuff( 0, szPath, _POSIX_PATH_MAX + 1 );       
         }
         */

         hb_xfree( pOut );



         if( pBlock  !=  NULL )
         {
            PHB_ITEM pFileName = hb_itemPutC( NULL, ( char * )szFileNameInZip );
            PHB_ITEM pFilePos = hb_itemPutNI( NULL, ulCount );
            hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
            hb_itemRelease( pFileName );
            hb_itemRelease( pFilePos );
         }

         try
         {
            /* TODO:  They're both the same.... */
            if ( !HB_IS_BLOCK( pProgress ) )
            {
//               szZip.SetPassword( szPassWord );
               szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
            }
            else
            {
//               szZip.SetPassword( szPassWord );
               szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
            }
         }

         catch ( CZipException& e ) {}

         if( bChange )
         {
            bChange = FALSE;
            szPath = NULL;
         }
      }
   }

   if (szPath)
   {
      hb_fsChDir((BYTE*)szPath);
      hb_xfree(szPath);
   }
   return ( int ) iReturn;
}

int hb_UnzipOne( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, char *szPassWord, char *pbyBuffer, char *szFiletoExtract, PHB_ITEM pProgress )
{
   bool bWithPath = lWithPath?true:false;
   bool iReturn = true;
   ULONG ulCount;
   int iMode;
   char  * szPath = (char*) hb_xgrab( _POSIX_PATH_MAX + 1 );   

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   szZip.SetSpanCallback( &span );

   iMode = hb_CheckSpanMode( szFile );

   try
   {
      switch( iMode )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn  = FALSE;
      }
   }

   catch ( CZipException& e )    {}

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   if ( szPassWord !=  NULL )
   {
      szZip.SetPassword( szPassWord );
   }

   ulCount = szZip.FindFile( ( LPCTSTR )szFiletoExtract, false );

   if ( ulCount == ( ULONG )-1 )
   {
      ulCount = szZip.FindFile( ( LPCTSTR )szFiletoExtract, true );
   }

   if ( pbyBuffer )
   { 
      if (hb_stricmp(pbyBuffer,".\\")==0 )
      {
         hb_fsCurDirBuffEx( 0, (BYTE*) szPath, _POSIX_PATH_MAX + 1 );
      }
      else
      {
         strcpy(szPath,pbyBuffer);
      }
      hb_fsChDir((BYTE*)"\\");
      szZip.SetRootPath(szPath);
   }

   if ( ulCount >= 0 )
   {
      CZipFileHeader fh;
      const char *  szFileNameInZip;
      PHB_FNAME pOut;
      CZipString szTempString;

      szZip.GetFileInfo( fh, ( WORD )ulCount );
      szTempString  = ( LPCTSTR )fh.GetFileName( );


      szFileNameInZip = ( const char * ) szTempString;
      pOut = hb_fsFNameSplit( ( char * ) szFileNameInZip );
      hb_xfree( pOut );

      if( pBlock  !=  NULL )
      {
         PHB_ITEM pFileName = hb_itemPutC( NULL, ( char * )szFileNameInZip );
         hb_vmEvalBlockV( pBlock, 1, pFileName );
         hb_itemRelease( pFileName );
      }

      try
      {
         /* TODO:  They're both the same.... */
         if ( !HB_IS_BLOCK( pProgress ) )
         {
//            szZip.SetPassword( szPassWord );
            szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
         }
         else
         {
//            szZip.SetPassword( szPassWord );
            szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
         }
      }
      catch ( CZipException& e ) { }
   }

   szZip.Close( );

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   if (szPath)
   {
      hb_fsChDir((BYTE*)szPath);
      hb_xfree(szPath);
   }


   return ( int ) iReturn;

}

int hb_DeleteOne( char *szFile, char *szFiletoDelete )
{
   bool iReturn = true;
   ULONG ulCount;

   CZipArchive szZip;

   try
   {
      switch(hb_CheckSpanMode( szFile ))
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
//       default:
            iReturn = false;
      }
   }

   catch ( CZipException &e )    {}

   ulCount = szZip.FindFile( ( LPCTSTR )szFiletoDelete, false );

   if ( ulCount == ( ULONG ) -1 )
   {
      ulCount = szZip.FindFile( ( LPCTSTR )szFiletoDelete, true );
   }

   if ( ulCount >= 0 )
   {
      CZipFileHeader fh;
      szZip.GetFileInfo( fh, ( WORD )ulCount );

      try
      {
         szZip.DeleteFile( ( WORD )ulCount );
      }

      catch ( ... )
      {
         iReturn = false;
      }
   }

   szZip.Close( );

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   return ( int ) iReturn;
}

int hb_DeleteSel( char *szFile, PHB_ITEM pArray, BOOL bCase )
{
   bool iReturn = true;
   ULONG ulCount;
   CZipArchive szZip;
   CZipStringArray  aFiles;

   try
   {
      switch(hb_CheckSpanMode( szFile ))
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
//       default:
            iReturn = false;
      }
   }

   catch ( CZipException &e )    {}

   if ( iReturn )
   {
      for ( ulCount = 1 ; ( ulCount <=  ( ULONG ) hb_arrayLen( pArray ) ) ; ulCount ++ )
      {
         const char *szDummy = ( char * )hb_arrayGetCPtr( pArray, ulCount );
         aFiles.Add( szDummy );
      }

      szZip.DeleteFiles( aFiles );
   }

   szZip.Close( );

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   return ( int ) iReturn;
}


int hb_UnzipSel( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, char *szPassWord, char *pbyBuffer, PHB_ITEM pSelArray, PHB_ITEM pProgress )
{
   bool bWithPath = lWithPath?true:false;
   bool iReturn = true;
   ULONG ulCount;
   int iCause;
   int iMode = hb_CheckSpanMode( szFile );
   char  * szPath = (char*) hb_xgrab( _POSIX_PATH_MAX + 1 );

   BOOL bChange = FALSE;
   LPCTSTR lpFiletoExtract;

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   try
   {
      switch( iMode )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( CZipException&  e )    {}

   if ( iReturn )
   {
      if ( szPassWord !=  NULL )
      {
         szZip.SetPassword( szPassWord );
      }
      if ( pbyBuffer )
      { 
         if (hb_stricmp(pbyBuffer,".\\")==0 )
         {
            hb_fsCurDirBuffEx( 0, (BYTE*) szPath, _POSIX_PATH_MAX + 1 );
         }
         else
         {
            strcpy(szPath,pbyBuffer);
         }
         hb_fsChDir((BYTE*)"\\");

         szZip.SetRootPath(szPath);
      }


      for ( iCause = 1 ; ( iCause <=  ( int ) hb_arrayLen( pSelArray ) ) ; iCause ++ )
      {
         lpFiletoExtract = hb_arrayGetC( pSelArray, iCause );
         ulCount = szZip.FindFile( ( LPCTSTR )lpFiletoExtract, false );

         if ( ulCount == (ULONG ) -1 )
         {
            ulCount = szZip.FindFile( ( LPCTSTR )lpFiletoExtract, true );
         }

         if ( ulCount >= 0 )
         {
            CZipFileHeader fh;
            const char *  szFileNameInZip;
            CZipString szTempString;
            PHB_FNAME pOut;
            szZip.GetFileInfo( fh, ( WORD )ulCount );
            szTempString  = ( LPCTSTR )fh.GetFileName( );
            szFileNameInZip = ( const char * )szTempString;
            pOut = hb_fsFNameSplit( ( char * ) szFileNameInZip );

            if ( szPath == NULL )
            {
               szPath = ( char* )pOut->szDrive;
               pOut->szDrive = "";
               hb_fsFNameMerge( ( char* )szFileNameInZip, pOut );
               bChange = TRUE;
            }
            szZip.SetRootPath(szPath);
            hb_xfree( pOut );


            if( pBlock  !=  NULL )
            {
               PHB_ITEM pFileName = hb_itemPutC( NULL, ( char * )szFileNameInZip );
               hb_vmEvalBlockV( pBlock, 1, pFileName );
               hb_itemRelease( pFileName );
            }

            try
            {
               /* TODO:  They're both the same.... */
               if ( !HB_IS_BLOCK( pProgress ) )
               {
//                  szZip.SetPassword( szPassWord );
                  szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
               }
               else
               {
//                  szZip.SetPassword( szPassWord );
                  szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
               }
            }

            catch ( CZipException&  e )   {}

            if ( bChange )
            {
               bChange = FALSE;
               szPath = "";
            }
         }
      }
   }

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   szZip.Close();

   if (szPath)
   {
      hb_fsChDir((BYTE*)szPath);
      hb_xfree(szPath);
   }


   return ( int ) iReturn;
}

int hb_TestForPKS( char *szFile )
{
   return hb_CheckSpanMode( szFile );
}

void hb_SetZipBuff( int a, int b, int c )
{
   pZipI.iWrite   = a > 65535 ? a : 65535;
   pZipI.iExtract = b > 16384 ? b : 16384;
   pZipI.iRead    = c > 32768 ? c : 32768;
}

void hb_SetZipComment( char *szComment )
{
   int iLen = strlen( ( const char * ) szComment ) + 1;
   pZipI.szComment = ( char* ) hb_xgrab( iLen );
   strcpy( pZipI.szComment, szComment );
}

void hb_SetZipReadOnly(int iRead )
{

   pZipI.iReadOnly = iRead ;

}


const char * hb_GetZipComment( char *szFile )
{
   const char *szReturn;
   char *szTempR;
   bool iReturn = true;
   CZipString szTemp;

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( CZipException&  e )  {}

   if ( iReturn )
   {
      szTemp = szZip.GetGlobalComment( );
      szReturn = ( const char * ) szTemp;
   }

   szTempR = ( char* ) hb_xgrab( strlen( ( const char* ) szReturn ) + 1 );
   strcpy( szTempR, ( char* ) szReturn );

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   szZip.Close( );

   return szTempR;

}

int hb_UnzipOneIndex( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, char *szPassWord, char *szPath, ULONG ulCount, PHB_ITEM pProgress )
{
   bool bWithPath = lWithPath?true:FALSE;
   bool iReturn = true;

   int iMode = hb_CheckSpanMode( szFile );

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   
   ulCount--;

   szZip.SetSpanCallback( &span );

   try
   {
      switch( iMode )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( CZipException& e )    {}

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   if ( ulCount >= 0 )
   {
      CZipFileHeader fh;
      const char *  szFileNameInZip;
      CZipString szTempString;

      if ( szPassWord !=  NULL )
      {
         szZip.SetPassword( szPassWord );
      }

      szZip.GetFileInfo( fh, ( WORD )ulCount );
      szTempString  = ( LPCTSTR )fh.GetFileName( );

      szFileNameInZip = ( const char * )szTempString;

      if( pBlock != NULL )
      {
         PHB_ITEM pFileName = hb_itemPutC( NULL, ( char * )szFileNameInZip );
         hb_vmEvalBlockV( pBlock, 1, pFileName );
         hb_itemRelease( pFileName );
      }

      try
      {
         /* TODO:  They're both the same.... */
         if ( !HB_IS_BLOCK( pProgress ) )
         {
//            szZip.SetPassword( szPassWord );
            szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
         }
         else
         {
//            szZip.SetPassword( szPassWord );
            szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
         }
      }

      catch ( CZipException& e )   {}

   }
   szZip.Close( );
   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   return ( int ) iReturn;
}

int hb_UnzipSelIndex( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, char *szPassWord, char *szPath, PHB_ITEM pSelArray, PHB_ITEM pProgress )
{
   bool bWithPath = lWithPath?true:false;
   bool iReturn = true;
   ULONG ulCount;
   int iCause;
   int iMode = hb_CheckSpanMode( szFile );

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   try
   {
      switch( iMode )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( CZipException &e )  {}

   if ( iReturn )
   {
      if ( szPassWord !=  NULL )
      {
         szZip.SetPassword( szPassWord );
      }

      for ( iCause = 0;( iCause<=  ( int )hb_arrayLen( pSelArray ) ) ; iCause++ )
      {
         ulCount =  hb_arrayGetNI( pSelArray, iCause ) - 1;

         if ( ulCount >= 0 )
         {
            CZipFileHeader fh;
            const char *  szFileNameInZip;
            CZipString szTempString;
            szZip.GetFileInfo( fh, ( WORD )ulCount );
            szTempString  = ( LPCTSTR )fh.GetFileName( );
            szFileNameInZip = ( const char * )szTempString;

            if( pBlock  !=  NULL )
            {
               PHB_ITEM pFileName = hb_itemPutC( NULL, ( char * )szFileNameInZip );
               hb_vmEvalBlockV( pBlock, 1, pFileName );
               hb_itemRelease( pFileName );
            }

            try
            {
               /* TODO:  They're both the same.... */
               if ( !HB_IS_BLOCK( pProgress ) )
               {
//                  szZip.SetPassword( szPassWord );
                  szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
               }
               else
               {
//                  szZip.SetPassword( szPassWord );
                  szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
               }
            }

            catch ( CZipException&  e )   {}
         }
      }
   }

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   szZip.Close( );

   return (int) iReturn;
}

BOOL hb_TransferFilesFromzip( char *szSource, char *szDest, PHB_ITEM pArray )
{
   CZipArchive szZSource;
   CZipArchive szZDest;
   CZipStringArray aFiles;
   const char *szDummy;
   ULONG ulCount;
   BOOL bReturn  = TRUE;
   BOOL bReturn1 = TRUE;

   try
   {
      switch(hb_CheckSpanMode( szSource ))
      {
         case 0:
            szZSource.Open( szSource, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
//       default:
            bReturn = FALSE;
      }
   }

   catch ( CZipException &e ) { }

   try
   {
      switch(hb_CheckSpanMode( szDest ))
      {
         case 0:
            szZDest.Open( szDest, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
//       default:
            bReturn1 = FALSE;
      }
   }

   catch ( CZipException &e ) { }

   if ( bReturn && bReturn1 )
   {
      for ( ulCount = 1 ; ( ulCount <=  ( ULONG ) hb_arrayLen( pArray ) ) ; ulCount++ )
      {
         szDummy = ( char * )hb_arrayGetCPtr( pArray, ulCount );
         aFiles.Add( szDummy );
      }

      if ( szZDest.GetFromArchive( szZSource, aFiles, false ) )
      {
         bReturn  = true;
      }

      szZDest.Close( );
      szZSource.Close( );

      return TRUE;

   }

   return FALSE;
}


int hb_DeleteOneIndex( char *szFile, ULONG ulCount )
{
   bool iReturn = true;

   CZipArchive szZip;

   ulCount--;

   try
   {
      switch(hb_CheckSpanMode( szFile ))
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
//       default:
            iReturn = true;
      }
   }

   catch ( CZipException &e )   {}

   if ( ulCount >= 0 )
   {
      CZipFileHeader fh;
      szZip.GetFileInfo( fh, ( WORD )ulCount );

      try
      {
         szZip.DeleteFile( ( WORD )ulCount );
      }

      catch( ... )
      {
         iReturn = false;
      }
   }

   szZip.Close( );

   if ( pChangeDiskBlock )
   {
      hb_itemRelease( pChangeDiskBlock );
   }

   return ( int ) iReturn;
}


DWORD GetCurrentFileSize( LPCTSTR szFile )
#if defined( HB_OS_WIN_32 ) || defined( __MINGW32__ )
{
   DWORD dwFlags = FILE_ATTRIBUTE_ARCHIVE;
   HANDLE hFind;
   WIN32_FIND_DATA  hFilesFind;

   hFind = FindFirstFile( szFile, &hFilesFind );

   if ( hFind != INVALID_HANDLE_VALUE )
   {
      if ( dwFlags & hFilesFind.dwFileAttributes )
      {
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

#ifdef __cplusplus
}
#endif

int hb_CheckSpanMode( char * szFile )
{
   int iReturn = 0;

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   szZip.SetSpanCallback( &span );

   try
   {
      szZip.Open( szFile, CZipArchive::zipOpen, 0 );
   }

   catch( CZipException &e )
   {
      if ( e.m_iCause == CZipException::cdirNotFound )
      {
         szZip.Close( true );
         iReturn = 114;
      } else if ( e.m_iCause == CZipException::noCallback )
      {
         szZip.Close( true );
         iReturn = 103;
      }
   }

   if ( ! iReturn )
   {
      iReturn = szZip.GetSpanMode( );
      szZip.Close( );
   }

   return iReturn;
}

