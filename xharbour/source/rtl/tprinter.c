/*
 * Harbour Project source code:
 * Tprinter.cpp
 * Harbour THarbourPrinter C++ Class for Harbour print support
 * Copyright 2002 Luiz Rafael Culik<culikr@uol.com.br>
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

#include "hbsetup.h"

#if defined(HB_OS_WIN_32) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))

#include <windows.h>

#define HB_OS_WIN_32_USED
#include "hbapi.h"

static LPTSTR MyDeviceName;
static LPTSTR MyJobName = "RawPrint";

static BOOL IsOpenDevice = FALSE;
static BOOL IsOpenPage = FALSE;

BOOL THarbourPrinter_OpenDevice( void );
BOOL THarbourPrinter_CloseDevice( void );
BOOL THarbourPrinter_WriteString(LPCSTR Text);
BOOL THarbourPrinter_NewPage( void );
void THarbourPrinter_SetDevice(LPTSTR szDevice);
void THarbourPrinter_SetJobName(LPTSTR szJobName);
BOOL hb_GetDefaultPrinter(LPTSTR pPrinterName, LPDWORD pdwBufferSize);
BOOL hb_GetPrinterNameByPort(LPTSTR pPrinterName, LPDWORD pdwBufferSize,LPTSTR pPortName);
HANDLE THarbourPrinter_hPrinter( void );

static HANDLE hPrinter;

HANDLE THarbourPrinter_hPrinter( void )
{
    return hPrinter;
}

void THarbourPrinter_SetDevice( LPTSTR szDevice )
{
   MyDeviceName = szDevice;
   IsOpenDevice = FALSE;
}

void THarbourPrinter_SetJobName( LPTSTR szJobName )
{
   MyJobName = szJobName;
}

BOOL THarbourPrinter_OpenDevice(  void )
{
   DOC_INFO_1 doc_info = { 0 };
   BOOL Result = TRUE;

   if( ! OpenPrinter( MyDeviceName , &hPrinter , NULL ) )
   {
      Result = FALSE;
   }
   else
   {
      doc_info.pDocName = MyJobName;
      doc_info.pOutputFile = NULL;
      doc_info.pDatatype = "RAW";

      if ( !StartDocPrinter( hPrinter ,1 , ( LPBYTE ) &doc_info ) )
      {
         ClosePrinter( hPrinter );
         Result = FALSE;
      }
      else
      {
         IsOpenDevice = TRUE;
         THarbourPrinter_NewPage();
      }
   }

   return Result;
}

BOOL THarbourPrinter_CloseDevice()
{
    BOOL Result ;

    if ( IsOpenDevice )
    {
        if ( IsOpenPage )
        {
            if ( EndPagePrinter( hPrinter ) )
            {
               THarbourPrinter_WriteString( " " );
            }
        }
    }

    if ( ! EndDocPrinter( hPrinter ) )
    {
        Result = FALSE;
    }
    else
    {
        if ( ! ClosePrinter( hPrinter ) )
        {
            Result = FALSE;
        }
        else
        {
            Result = TRUE;
            IsOpenDevice = FALSE;
        }
    }

    return Result;
}

BOOL THarbourPrinter_WriteString( LPCSTR Text )
{
    DWORD WrittenChars;
    DWORD dwNumByte;
    BOOL  Result = FALSE;

    if( IsOpenDevice )
    {
        dwNumByte = lstrlen( ( LPCTSTR ) Text );
        Result = TRUE;

        if( ! WritePrinter( hPrinter, ( void* )  Text , dwNumByte , &WrittenChars ) )
        {
            Result = FALSE;
        }
    }

    return Result;
}

BOOL THarbourPrinter_NewPage()
{
   BOOL Result;

   if( IsOpenDevice )
   {
      if ( IsOpenPage )
      {
         if ( EndPagePrinter( hPrinter ) )
         {
            THarbourPrinter_WriteString( "\f\n" ) ;
         }
      }
   }

   if ( !StartPagePrinter( hPrinter ) )
   {
      Result = FALSE;
   }
   else
   {
      IsOpenPage = TRUE;
      Result=TRUE;
   }

   return Result;
}

#define MAXBUFFERSIZE 255

BOOL hb_GetDefaultPrinter( LPTSTR pPrinterName, LPDWORD pdwBufferSize )
{
   BOOL Result= FALSE ;
   TCHAR cBuffer[ MAXBUFFERSIZE ];
   DWORD nSize ;
   *pPrinterName = '\0' ;
   nSize = GetProfileString( "windows", "device", "", cBuffer, MAXBUFFERSIZE ) ;
   if (nSize < *pdwBufferSize) {
     strtok( cBuffer, "," ) ;
     lstrcpy( pPrinterName, cBuffer ) ;
     /* Set buffer size parameter to min required buffer size... */
     *pdwBufferSize = ( DWORD ) lstrlen( cBuffer )+1;
     Result = TRUE ;
   }
   else {
     /* If given buffer too small, set required size and fail... */
     *pdwBufferSize = nSize+1 ;
   }
   return Result ;
}


HB_FUNC(GETDEFAULTPRINTER)
{
      char szDefaultPrinter[MAXBUFFERSIZE];
      DWORD pdwBufferSize = MAXBUFFERSIZE;
      if( hb_GetDefaultPrinter( ( LPTSTR ) &szDefaultPrinter , &pdwBufferSize ) )
         hb_retclen(szDefaultPrinter , pdwBufferSize-1);
      else
         hb_retc("");
}

BOOL hb_GetPrinterNameByPort( LPTSTR pPrinterName, LPDWORD pdwBufferSize,LPTSTR pPortName )
{
  BOOL Result = FALSE, bFound = FALSE ;
  unsigned long needed, returned, a;
  PRINTER_INFO_5 *pPrinterEnum,*buffer;
  HB_TRACE(HB_TR_DEBUG, "hb_GetPrinterNameByPort()");
  EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS ,NULL,5,( LPBYTE ) buffer, 0, &needed,&returned ) ;
  if (needed>0) {
    pPrinterEnum = buffer = ( PRINTER_INFO_5 * ) hb_xgrab( needed ) ;
    if (EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS ,NULL,5,( LPBYTE ) buffer, needed, &needed,&returned ) ) {
      for( a = 0 ; a < returned && !bFound ; a++, pPrinterEnum++ )  {
        if ( lstrcmp( pPrinterEnum->pPortName , pPortName ) == 0 ) {
          bFound = TRUE ;
          if (*pdwBufferSize >= strlen(pPrinterEnum->pPrinterName)+1) {
            lstrcpy( pPrinterName , pPrinterEnum->pPrinterName ) ;
            Result = TRUE;
          }
          // Store name length + \0 char for return
          *pdwBufferSize = ( DWORD ) lstrlen( pPrinterEnum->pPrinterName )  + 1;
        }
      }
    }
    hb_xfree(buffer) ;
  }
  return Result;
}

HB_FUNC(PRINTERPORTTONAME) {
  char szDefaultPrinter[MAXBUFFERSIZE];
  DWORD pdwBufferSize = MAXBUFFERSIZE;
  if( ISCHAR(1) && hb_parclen(1)>0 && hb_GetPrinterNameByPort( ( LPTSTR ) &szDefaultPrinter , &pdwBufferSize , hb_parc(1)) )
    hb_retc(szDefaultPrinter);
  else
    hb_retc("");
}
#define BIG_PRINT_BUFFER (1024*32)

LONG hb_PrintFileRaw(UCHAR *cPrinterName,UCHAR *cFileName, UCHAR *cDocName) {
  UCHAR  printBuffer[BIG_PRINT_BUFFER] ;
  HANDLE  hPrinter, hFile ;
  DOC_INFO_1 DocInfo ;
  DWORD nRead, nWritten, Result;
  if ( OpenPrinter(cPrinterName, &hPrinter, NULL) != 0 ) {
    DocInfo.pDocName = cDocName ;
    DocInfo.pOutputFile = NULL ;
    DocInfo.pDatatype = "RAW" ;
    if ( StartDocPrinter(hPrinter,1,(char *) &DocInfo) != 0 ) {
      if ( StartPagePrinter(hPrinter) != 0 ) {
        hFile = CreateFile(cFileName,GENERIC_READ,0,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL)   ;
        if (hFile != INVALID_HANDLE_VALUE ) {
          while (ReadFile(hFile, printBuffer, BIG_PRINT_BUFFER, &nRead, NULL) && (nRead > 0)) {
            if (printBuffer[nRead-1] == 26 )
              nRead-- ; // Skip the EOF() character
            WritePrinter(hPrinter, printBuffer, nRead, &nWritten) ;
          }
          Result = 1 ;
          CloseHandle(hFile) ;
        }
        else
          Result= -6 ;
        EndPagePrinter(hPrinter) ;
      }
      else
        Result = -4 ;
      EndDocPrinter(hPrinter);
    }
    else
      Result= -3 ;
    ClosePrinter(hPrinter) ;
  }
  else
    Result= -2 ;
  return Result ;
}

HB_FUNC( PRINTFILERAW )  {
  UCHAR *cPrinterName, *cFileName, *cDocName ;
  DWORD Result = -1 ;
  if (ISCHAR(1) && ISCHAR(2)) {
    cPrinterName= hb_parc(1) ;
    cFileName= hb_parc(2) ;
    cDocName = ISCHAR(3) ? hb_parc(3) : cFileName ;
    Result = hb_PrintFileRaw(cPrinterName, cFileName, cDocName) ;
  }
  hb_retnl(Result) ;
}
static BOOL isWinNt(void) {
  OSVERSIONINFO osvi ;
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx (&osvi);
  return(osvi.dwPlatformId == VER_PLATFORM_WIN32_NT); // && osvi.dwMajorVersion >= 4);
}

HB_FUNC(GETPRINTERS) {
  HANDLE hPrinter ;
  DWORD Flags = PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS;
  BOOL bPrinterNamesOnly= TRUE ;
  PRINTER_INFO_4 *buffer4, *pPrinterEnum4;
  PRINTER_INFO_5 *buffer, *pPrinterEnum;
  PRINTER_INFO_2 *pPrinterInfo2 ;
  unsigned long needed = 0 , returned=0, a;
  PHB_ITEM pSubItems, pFile, pPort ;
  PHB_ITEM pArrayPrinter = hb_itemArrayNew( 0 );
  buffer = NULL ;
  HB_TRACE(HB_TR_DEBUG, "GETPRINTERS()");
  if (ISLOG(1))
    bPrinterNamesOnly = !hb_parl(1) ;
  if (isWinNt()) {
    EnumPrinters(Flags,NULL,4,(LPBYTE) pPrinterEnum4,0,&needed,&returned) ;
    if (needed > 0) {
      pPrinterEnum4 = buffer4 = ( PRINTER_INFO_4 * ) hb_xgrab( needed ) ;
      if (EnumPrinters(Flags,NULL,4,(LPBYTE)  pPrinterEnum4,needed,&needed,&returned)) {
        if (bPrinterNamesOnly ) {
          for ( a = 0 ; a < returned ; a++, pPrinterEnum4++) {
            pFile = hb_itemPutC( NULL, pPrinterEnum4->pPrinterName );
            hb_arrayAdd( pArrayPrinter , pFile );
            hb_itemRelease( pFile ) ;
          }
        }
        else {
          for ( a = 0 ; a < returned ; a++, pPrinterEnum4++) {
            pSubItems = hb_itemArrayNew( 2 );
            pFile = hb_itemPutC( NULL, pPrinterEnum4->pPrinterName );
            if (OpenPrinter(pPrinterEnum4->pPrinterName, &hPrinter, NULL)) {
              GetPrinter(hPrinter, 2, NULL, 0, &needed);
              pPrinterInfo2 = ( PRINTER_INFO_2 * ) hb_xgrab( needed ) ;
              if (GetPrinter(hPrinter,2,(LPBYTE) pPrinterInfo2, needed,&needed))
                pPort = hb_itemPutC( NULL,pPrinterInfo2->pPortName );
              else
                pPort = hb_itemPutC( NULL,"Error" );
              hb_xfree(pPrinterInfo2) ;
              CloseHandle(hPrinter) ;
            }
            else
              pPort = hb_itemPutC( NULL,"Error" );

            hb_arraySet( pSubItems , 1 , pFile ) ;
            hb_arraySet( pSubItems , 2 , pPort ) ;
            hb_arrayAdd( pArrayPrinter , pSubItems );
            hb_itemRelease( pFile ) ;
            hb_itemRelease( pPort ) ;
            hb_itemRelease( pSubItems );
          }
        }
      }
      hb_xfree(buffer4) ;
    }
  }
  else {
    EnumPrinters( Flags,NULL,5,(LPBYTE) buffer,0,&needed,&returned );
    if( needed > 0 ) {
      pPrinterEnum = buffer = ( PRINTER_INFO_5 * ) hb_xgrab( needed ) ;
      if ( EnumPrinters(Flags, NULL , 5 , (LPBYTE) buffer , needed , &needed , &returned ) ) {
        for ( a = 0 ; a < returned ; a++, pPrinterEnum++) {
          if (bPrinterNamesOnly ) {
            pFile = hb_itemPutC( NULL, pPrinterEnum->pPrinterName );
            hb_arrayAdd( pArrayPrinter , pFile );
            hb_itemRelease( pFile ) ;
          }
          else {
            pSubItems = hb_itemArrayNew( 2 );
            pFile = hb_itemPutC( NULL, pPrinterEnum->pPrinterName );
            pPort = hb_itemPutC( NULL,pPrinterEnum->pPortName );
            hb_arraySet( pSubItems , 1 , pFile ) ;
            hb_arraySet( pSubItems , 2 , pPort ) ;
            hb_arrayAdd( pArrayPrinter , pSubItems );
            hb_itemRelease( pFile ) ;
            hb_itemRelease( pPort ) ;
            hb_itemRelease( pSubItems );
          }
        }
      }
      hb_xfree(buffer) ;
    }
  }
  hb_itemRelease( hb_itemReturn( pArrayPrinter ) );
}


#endif


