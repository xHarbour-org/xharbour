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

#define HB_OS_WIN_32_USED

#include "windows.h"

#include "hbapi.h"

#if defined(HB_OS_WIN_32) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))

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
BOOL THarbourPrinter_DPGetDefaultPrinter(LPTSTR pPrinterName, LPDWORD pdwBufferSize);
BOOL THarbourPrinter_ReOpenDevice( void );
BOOL THarbourPrinter_GetPrinterNameByPort(LPTSTR pPrinterName, LPDWORD pdwBufferSize,LPTSTR pPortName);
HANDLE THarbourPrinter_hPrinter( void );

static HANDLE hPrinter;

HANDLE THarbourPrinter_hPrinter( void )
{
    return hPrinter;
}

void THarbourPrinter_SetDevice( LPTSTR szDevice )
{
   MyDeviceName = szDevice;
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

#define MAXBUFFERSIZE 250

BOOL THarbourPrinter_DPGetDefaultPrinter( LPTSTR pPrinterName, LPDWORD pdwBufferSize )
{
   BOOL bFlag;
   OSVERSIONINFO osv;
   TCHAR cBuffer[ MAXBUFFERSIZE ];
   PRINTER_INFO_2 *ppi2 = NULL;
   DWORD dwNeeded = 0;
   DWORD dwReturned = 0;

   /* What version of Windows are you running? */
   osv.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &osv );

   /* If Windows 95 or 98, use EnumPrinters... */
   if ( osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
   {
      /* The first EnumPrinters() tells you how big our buffer should
        be in order to hold ALL of PRINTER_INFO_2. Note that this will
        usually return FALSE. This only means that the buffer (the 4th
        parameter) was not filled in. You don't want it filled in here... */

      EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 2, NULL, 0, &dwNeeded, &dwReturned ) ;

      if ( dwNeeded == 0 )
      {
         return FALSE;
      }

      /* Allocate enough space for PRINTER_INFO_2... */
      ppi2 = ( PRINTER_INFO_2 * ) GlobalAlloc( GPTR, dwNeeded ) ;

      if ( !ppi2 )
      {
         return FALSE;
      }

      /* The second EnumPrinters( )  will fill in all the current information... */
      bFlag = EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 2, ( LPBYTE ) ppi2, dwNeeded, &dwNeeded, &dwReturned ) ;

      if ( !bFlag )
      {
         GlobalFree( ppi2 ) ;

         return FALSE;
      }

      /* If given buffer too small, set required size and fail... */
      if ( ( DWORD ) lstrlen( ppi2->pPrinterName )  >= *pdwBufferSize )
      {
         *pdwBufferSize = ( DWORD ) lstrlen( ppi2->pPrinterName )  + 1;
         GlobalFree( ppi2 ) ;

        return FALSE;
      }

      /* Copy printer name into passed-in buffer... */
      lstrcpy( pPrinterName, ppi2->pPrinterName ) ;

      /* Set buffer size parameter to min required buffer size... */
      *pdwBufferSize = ( DWORD ) lstrlen( ppi2->pPrinterName )  + 1;
   }
   else if ( osv.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      /* If Windows NT, use the GetDefaultPrinter API for Windows 2000,
         or GetProfileString for version 4.0 and earlier... */
#if( WINVER >= 0x0500 )
      if ( osv.dwMajorVersion >= 5 )  /* Windows 2000 or later */
      {
         bFlag = GetDefaultPrinter( pPrinterName, pdwBufferSize ) ;

         if ( !bFlag )
         {
            return FALSE;
         }
      }
      else /* NT4.0 or earlier */
#endif
      {
         /* Retrieve the default string from Win.ini (the registry ) .
           String will be in form "printername,drivername,portname". */
         if ( GetProfileString( "windows", "device", ",,,", cBuffer, MAXBUFFERSIZE )  <= 0 )
         {
            return FALSE;
         }

         /* Printer name precedes first "," character... */
         strtok( cBuffer, "," ) ;

         /* If given buffer too small, set required size and fail... */
         if ( ( DWORD ) lstrlen( cBuffer )  >= *pdwBufferSize )
         {
            *pdwBufferSize = ( DWORD ) lstrlen( cBuffer )  + 1;

            return FALSE;
         }

         /* Copy printer name into passed-in buffer... */
         lstrcpy( pPrinterName, cBuffer ) ;

         /* Set buffer size parameter to min required buffer size... */
         *pdwBufferSize = ( DWORD ) lstrlen( cBuffer )  + 1;
      }
   }

   /* Cleanup... */
   if ( ppi2 )
   {
      GlobalFree( ppi2 ) ;
   }

   return TRUE;
}


#define MAX_PRINTERS 20
BOOL THarbourPrinter_GetPrinterNameByPort( LPTSTR pPrinterName, LPDWORD pdwBufferSize,LPTSTR pPortName )
{
   unsigned long needed, returned, a;
   PRINTER_INFO_5 buffer[ MAX_PRINTERS ];

   EnumPrinters( PRINTER_ENUM_LOCAL,NULL,5,( LPBYTE ) buffer, MAX_PRINTERS*sizeof( PRINTER_INFO_5 ), &needed,&returned ) ;

   for( a = 0 ; a < returned ; a++ )
   {
      if ( lstrcmp( buffer[a].pPortName , pPortName ) == 0 )
      {
          lstrcpy( pPrinterName , buffer[a].pPrinterName ) ;
          *pdwBufferSize = ( DWORD ) lstrlen( buffer[a].pPrinterName )  + 1;

          return TRUE;
      }
   }

   return FALSE;
}
#endif
