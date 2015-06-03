/*
 * $Id$
 */

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

#if defined( HB_OS_WIN ) && ( ! defined( __RSXNT__ ) ) && ( ! defined( __CYGWIN__ ) )

#include <windows.h>

#if defined( __LCC__ )
   #include <winspool.h>
#endif


#include "hbapi.h"
#include "hbapiitm.h"

HB_EXTERN_BEGIN
extern BOOL hb_GetDefaultPrinter( LPTSTR pPrinterName, LPDWORD pdwBufferSize );
extern BOOL hb_SetDefaultPrinter( LPTSTR pPrinterName );
extern BOOL hb_GetPrinterNameByPort( LPTSTR pPrinterName, LPDWORD pdwBufferSize, const char * pPortName, BOOL bSubStr );
extern BOOL hb_isLegacyDevice( LPTSTR pPrinterName );
extern BOOL hb_PrinterExists( LPTSTR pPrinterName );
extern LONG hb_PrintFileRaw( const char * cPrinterName, const char * cFileName, const char * cDocName );
HB_EXTERN_END

#define MAXBUFFERSIZE 255

#undef HWND_BROADCAST
#if defined( __XCC__ )
   #define HWND_BROADCAST  ((HWND)0xffff)
#else
   #define HWND_BROADCAST  ((HWND)(HB_LONG)0xffff)
#endif

BOOL hb_isLegacyDevice( LPTSTR pPrinterName )
{
   BOOL     bLegacyDev  = FALSE;
   int      n           = 0;
   LPTSTR   pszPrnDev[] = { "lpt1", "lpt2", "lpt3", "lpt4", "lpt5", "lpt6", "com1", "com2", "com3", "com4", NULL };

   while( pszPrnDev[ n ] && ! bLegacyDev )
   {
      bLegacyDev = ( hb_strnicmp( pPrinterName, pszPrnDev[ n ], strlen( pszPrnDev[ n ] ) ) == 0 );
      n++;
   }
   return bLegacyDev;
}

BOOL hb_PrinterExists( LPTSTR pPrinterName )
{
   BOOL              Result   = FALSE;
   DWORD             Flags    = PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS;
   PRINTER_INFO_4 *  buffer4, * pPrinterEnum4;
   HANDLE            hPrinter;
   ULONG             needed   = 0, returned = 0, a;

   HB_TRACE( HB_TR_DEBUG, ( "hb_PrinterExists(%s)", pPrinterName ) );

   if( ! strchr( pPrinterName, HB_OS_PATH_LIST_SEP_CHR )
       && ! hb_isLegacyDevice( pPrinterName ) ) // Don't bother with test if '\' in string
   {
      if( hb_iswinnt() ) // Use EnumPrinter() here because much faster than OpenPrinter()
      {
         EnumPrinters( Flags, NULL, 4, ( LPBYTE ) NULL, 0, &needed, &returned );
         if( needed > 0 )
         {
            pPrinterEnum4 = buffer4 = ( PRINTER_INFO_4 * ) hb_xgrab( needed );
            if( pPrinterEnum4 )
            {
               if( EnumPrinters( Flags, NULL, 4, ( LPBYTE ) pPrinterEnum4, needed, &needed, &returned ) )
               {
                  for( a = 0; ! Result && a < returned; a++, pPrinterEnum4++ )
                     Result = ( strcmp( ( const char * ) pPrinterName, ( const char * ) pPrinterEnum4->pPrinterName ) == 0 );
               }
               hb_xfree( buffer4 );
            }
         }
      }
      else if( OpenPrinter( ( char * ) pPrinterName, &hPrinter, NULL ) )
      {
         ClosePrinter( hPrinter );
         Result = TRUE;
      }
   }
   return Result;
}

HB_FUNC( PRINTEREXISTS )
{
   BOOL Result = FALSE;

   if( ISCHAR( 1 ) )
      Result = hb_PrinterExists( ( LPSTR ) hb_parcx( 1 ) );

   hb_retl( Result );
}

BOOL hb_GetDefaultPrinter( LPTSTR pPrinterName, LPDWORD pdwBufferSize )
{
   BOOL           Result = FALSE;
   OSVERSIONINFO  osvi;

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &osvi );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion >= 5 )  /* Windows 2000 or later */
   {
      typedef BOOL ( WINAPI * DEFPRINTER )( LPTSTR, LPDWORD );                   // stops warnings
      DEFPRINTER  fnGetDefaultPrinter;
      HMODULE     hWinSpool = LoadLibrary( "winspool.drv" );

      if( hWinSpool )
      {
         fnGetDefaultPrinter = ( DEFPRINTER ) GetProcAddress( hWinSpool, "GetDefaultPrinterA" );

         if( fnGetDefaultPrinter )
            Result = ( *fnGetDefaultPrinter )( pPrinterName, pdwBufferSize );

         FreeLibrary( hWinSpool );
      }
   }

   if( ! Result ) /* Win9X and Windows NT 4.0 or earlier & 2000+ if necessary for some reason i.e. dll could not load!!!! */
   {
      DWORD dwSize = GetProfileString( "windows", "device", "", pPrinterName, *pdwBufferSize );
      if( dwSize && dwSize < *pdwBufferSize )
      {
         dwSize = 0;
         while( pPrinterName[ dwSize ] != '\0' && pPrinterName[ dwSize ] != ',' )
         {
            dwSize++;
         }
         pPrinterName[ dwSize ]  = '\0';
         *pdwBufferSize          = dwSize + 1;
         Result                  = TRUE;
      }
      else
         *pdwBufferSize = dwSize + 1;
   }

   if( ! Result && osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
   {
/*
 *    This option should never be required but is included because of this article
 *
 *        http://support.microsoft.com/kb/246772/en-us
 *
 *    This option will not enumerate any network printers.
 *
 *    From the SDK technical reference for EnumPrinters();
 *
 *    If Level is 2 or 5, Name is a pointer to a null-terminated string that specifies
 *    the name of a server whose printers are to be enumerated.
 *    If this string is NULL, then the function enumerates the printers installed on the local machine.
 */

      DWORD             dwNeeded, dwReturned;
      PRINTER_INFO_2 *  ppi2;
      if( EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 2, NULL, 0, &dwNeeded, &dwReturned ) )
      {
         if( dwNeeded > 0 )
         {
            ppi2 = ( PRINTER_INFO_2 * ) hb_xgrab( dwNeeded );
            if( ppi2 )
            {
               if( EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 2, ( LPBYTE ) ppi2, dwNeeded, &dwNeeded, &dwReturned ) && dwReturned > 0 )
               {
                  DWORD dwSize = ( DWORD ) lstrlen( ppi2->pPrinterName );
                  if( dwSize && dwSize < *pdwBufferSize )
                  {
                     lstrcpy( pPrinterName, ppi2->pPrinterName );
                     *pdwBufferSize = dwSize + 1;
                     Result         = TRUE;
                  }
               }
               hb_xfree( ppi2 );
            }
         }
      }
   }
   return Result;
}

HB_FUNC( GETDEFAULTPRINTER )
{
   char  szDefaultPrinter[ MAXBUFFERSIZE ];
   DWORD pdwBufferSize = MAXBUFFERSIZE;

   if( hb_GetDefaultPrinter( ( LPTSTR ) &szDefaultPrinter, &pdwBufferSize ) )
      hb_retclen( szDefaultPrinter, pdwBufferSize - 1 );
   else
      hb_retc( "" );
}

/* You are explicitly linking to SetDefaultPrinter because implicitly
 * linking on Windows 95/98 or NT4 results in a runtime error.
 * This block specifies which text version you explicitly link to.
 */
#ifdef UNICODE
  #define SETDEFAULTPRINTER   "SetDefaultPrinterW"
#else
  #define SETDEFAULTPRINTER   "SetDefaultPrinterA"
#endif

/*-----------------------------------------------------------------*/
/* hb_SetDefaultPrinter                                            */
/*                                                                 */
/* Parameters:                                                     */
/*   pPrinterName: Valid name of existing printer to make default. */
/*                                                                 */
/* Returns: TRUE for success, FALSE for failure.                   */
/*-----------------------------------------------------------------*/
BOOL hb_SetDefaultPrinter( LPTSTR pPrinterName )
{
   BOOL              bFlag;
   OSVERSIONINFO     osv;
   DWORD             dwNeeded = 0;
   HANDLE            hPrinter = NULL;
   PRINTER_INFO_2 *  ppi2     = NULL;
   LPTSTR            pBuffer  = NULL;

   /* What version of Windows are you running? */
   osv.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &osv );

   if( ! pPrinterName )
      return FALSE;

   /* If Windows 95 or 98, use SetPrinter. */
   if( osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
   {
      /* Open this printer so you can get information about it. */
      bFlag = OpenPrinter( pPrinterName, &hPrinter, NULL );
      if( ! bFlag || ! hPrinter )
         return FALSE;

      /* The first GetPrinter() tells you how big our buffer must
         be to hold ALL of PRINTER_INFO_2. Note that this will
         typically return FALSE. This only means that the buffer (the 3rd
         parameter) was not filled in. You do not want it filled in here. */
      SetLastError( 0 );
      bFlag = GetPrinter( hPrinter, 2, 0, 0, &dwNeeded );
      if( ! bFlag )
      {
         if( ( GetLastError() != ERROR_INSUFFICIENT_BUFFER ) || ( dwNeeded == 0 ) )
         {
            ClosePrinter( hPrinter );
            return FALSE;
         }
      }

      /* Allocate enough space for PRINTER_INFO_2. */
      ppi2 = ( PRINTER_INFO_2 * ) GlobalAlloc( GPTR, dwNeeded );
      if( ! ppi2 )
      {
         ClosePrinter( hPrinter );
         return FALSE;
      }

      /* The second GetPrinter() will fill in all the current information
         so that all you have to do is modify what you are interested in. */
      bFlag = GetPrinter( hPrinter, 2, ( LPBYTE ) ppi2, dwNeeded, &dwNeeded );
      if( ! bFlag )
      {
         ClosePrinter( hPrinter );
         GlobalFree( ppi2 );
         return FALSE;
      }

      /* Set default printer attribute for this printer. */
      ppi2->Attributes |= PRINTER_ATTRIBUTE_DEFAULT;
      bFlag             = SetPrinter( hPrinter, 2, ( LPBYTE ) ppi2, 0 );
      if( ! bFlag )
      {
         ClosePrinter( hPrinter );
         GlobalFree( ppi2 );
         return FALSE;
      }

      /* Tell all open programs that this change occurred.
         Allow each program 1 second to handle this message. */
      SendMessageTimeout( HWND_BROADCAST, WM_SETTINGCHANGE, 0L, ( LPARAM ) ( LPCTSTR ) "windows", SMTO_NORMAL, 1000, NULL );
   }
   /* If Windows NT, use the SetDefaultPrinter API for Windows 2000,
      or WriteProfileString for version 4.0 and earlier. */
   else if( osv.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      if( osv.dwMajorVersion >= 5 ) /* Windows 2000 or later (use explicit call) */
      {
         HMODULE     hWinSpool;
         typedef BOOL ( WINAPI * DEFPRINTER )( LPTSTR ); /* stops warnings */
         DEFPRINTER  fnSetDefaultPrinter;

         hWinSpool            = LoadLibrary( "winspool.drv" );
         if( ! hWinSpool )
            return FALSE;
         fnSetDefaultPrinter  = ( DEFPRINTER ) GetProcAddress( hWinSpool, SETDEFAULTPRINTER );
         if( ! fnSetDefaultPrinter )
         {
            FreeLibrary( hWinSpool );
            return FALSE;
         }

         bFlag = ( *fnSetDefaultPrinter )( pPrinterName );
         FreeLibrary( hWinSpool );
         if( ! bFlag )
            return FALSE;
      }
      else /* NT4.0 or earlier */
      {
         /* Open this printer so you can get information about it. */
         bFlag = OpenPrinter( pPrinterName, &hPrinter, NULL );
         if( ! bFlag || ! hPrinter )
            return FALSE;

         /* The first GetPrinter() tells you how big our buffer must
            be to hold ALL of PRINTER_INFO_2. Note that this will
            typically return FALSE. This only means that the buffer (the 3rd
            parameter) was not filled in. You do not want it filled in here. */
         SetLastError( 0 );
         bFlag = GetPrinter( hPrinter, 2, 0, 0, &dwNeeded );
         if( ! bFlag )
         {
            if( ( GetLastError() != ERROR_INSUFFICIENT_BUFFER ) || ( dwNeeded == 0 ) )
            {
               ClosePrinter( hPrinter );
               return FALSE;
            }
         }

         /* Allocate enough space for PRINTER_INFO_2. */
         ppi2 = ( PRINTER_INFO_2 * ) GlobalAlloc( GPTR, dwNeeded );
         if( ! ppi2 )
         {
            ClosePrinter( hPrinter );
            return FALSE;
         }

         /* The second GetPrinter() fills in all the current
            information. */
         bFlag = GetPrinter( hPrinter, 2, ( LPBYTE ) ppi2, dwNeeded, &dwNeeded );
         if( ( ! bFlag ) || ( ! ppi2->pDriverName ) || ( ! ppi2->pPortName ) )
         {
            ClosePrinter( hPrinter );
            GlobalFree( ppi2 );
            return FALSE;
         }

         /* Allocate buffer big enough for concatenated string.
            String will be in form "printername,drivername,portname". */
         pBuffer = ( LPTSTR ) GlobalAlloc( GPTR,
                                           lstrlen( pPrinterName ) +
                                           lstrlen( ppi2->pDriverName ) +
                                           lstrlen( ppi2->pPortName ) + 3 );
         if( ! pBuffer )
         {
            ClosePrinter( hPrinter );
            GlobalFree( ppi2 );
            return FALSE;
         }

         /* Build string in form "printername,drivername,portname". */
         lstrcpy( pBuffer, pPrinterName );  lstrcat( pBuffer, "," );
         lstrcat( pBuffer, ppi2->pDriverName );  lstrcat( pBuffer, "," );
         lstrcat( pBuffer, ppi2->pPortName );

         /* Set the default printer in Win.ini and registry. */
         bFlag = WriteProfileString( "windows", "device", pBuffer );
         if( ! bFlag )
         {
            ClosePrinter( hPrinter );
            GlobalFree( ppi2 );
            GlobalFree( pBuffer );
            return FALSE;
         }
      }

      /* Tell all open programs that this change occurred.
         Allow each app 1 second to handle this message. */
      SendMessageTimeout( HWND_BROADCAST, WM_SETTINGCHANGE, 0L, 0L, SMTO_NORMAL, 1000, NULL );

   }

   /* Clean up. */
   if( hPrinter )
      ClosePrinter( hPrinter );

   if( ppi2 )
      GlobalFree( ppi2 );

   if( pBuffer )
      GlobalFree( pBuffer );

   return TRUE;
}
#undef SETDEFAULTPRINTER

HB_FUNC( SETDEFAULTPRINTER )
{
   hb_retl( hb_SetDefaultPrinter( ( LPSTR ) hb_parc( 1 ) ) );
}

BOOL hb_GetPrinterNameByPort( LPTSTR pPrinterName, LPDWORD pdwBufferSize, const char * pPortName, BOOL bSubStr )
{
   BOOL              Result = FALSE, bFound = FALSE;
   ULONG             needed, returned, a;
   PRINTER_INFO_5 *  pPrinterEnum, * buffer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_GetPrinterNameByPort(%s,%s)", pPrinterName, pPortName ) );

   EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, NULL, 5, ( LPBYTE ) NULL, 0, &needed, &returned );
   if( needed > 0 )
   {
      pPrinterEnum = buffer = ( PRINTER_INFO_5 * ) hb_xgrab( needed );

      if( EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, NULL, 5, ( LPBYTE ) buffer, needed, &needed, &returned ) )
      {
         for( a = 0; a < returned && ! bFound; a++, pPrinterEnum++ )
         {

            if( bSubStr )
               bFound = ( hb_strnicmp( pPrinterEnum->pPortName, pPortName, strlen( pPortName ) ) == 0 );
            else
               bFound = ( hb_stricmp( pPrinterEnum->pPortName, pPortName ) == 0 );

            if( bFound )
            {
               if( *pdwBufferSize >= strlen( pPrinterEnum->pPrinterName ) + 1 )
               {
                  hb_xstrcpy( pPrinterName, pPrinterEnum->pPrinterName, 0 );
                  Result = TRUE;
               }
               /* Store name length + \0 char for return */
               *pdwBufferSize = ( DWORD ) strlen( pPrinterEnum->pPrinterName ) + 1;
            }
         }
      }
      hb_xfree( buffer );
   }
   return Result;
}

HB_FUNC( PRINTERPORTTONAME )
{
   char  szDefaultPrinter[ MAXBUFFERSIZE ];
   DWORD pdwBufferSize = MAXBUFFERSIZE;

   if( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 && hb_GetPrinterNameByPort( ( LPTSTR ) &szDefaultPrinter, &pdwBufferSize, hb_parcx( 1 ), ISLOG( 2 ) ? hb_parl( 2 ) : FALSE ) )
      hb_retc( szDefaultPrinter );
   else
      hb_retc( "" );
}

#define BIG_PRINT_BUFFER ( 1024 * 32 )

LONG hb_PrintFileRaw( const char * cPrinterName, const char * cFileName, const char * cDocName )
{
   UCHAR       printBuffer[ BIG_PRINT_BUFFER ];
   HANDLE      hPrinter, hFile;
   DOC_INFO_1  DocInfo;
   DWORD       nRead, nWritten, Result;

   if( OpenPrinter( ( LPSTR ) cPrinterName, &hPrinter, NULL ) != 0 )
   {
      DocInfo.pDocName     = ( LPSTR ) cDocName;
      DocInfo.pOutputFile  = NULL;
      DocInfo.pDatatype    = "RAW";
      if( StartDocPrinter( hPrinter, 1, ( UCHAR * ) &DocInfo ) != 0 )
      {
         if( StartPagePrinter( hPrinter ) != 0 )
         {
            hFile = CreateFile( cFileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );
            if( hFile != INVALID_HANDLE_VALUE )
            {
               while( ReadFile( hFile, printBuffer, BIG_PRINT_BUFFER, &nRead, NULL ) && ( nRead > 0 ) )
               {
                  if( printBuffer[ nRead - 1 ] == 26 )
                     nRead--;  /* Skip the EOF() character */

                  WritePrinter( hPrinter, printBuffer, nRead, &nWritten );
               }
               Result = 1;
               CloseHandle( hFile );
            }
            else
               Result = ( DWORD ) -6;

            EndPagePrinter( hPrinter );
         }
         else
            Result = ( DWORD ) -4;

         EndDocPrinter( hPrinter );
      }
      else
         Result = ( DWORD ) -3;

      ClosePrinter( hPrinter );
   }
   else
      Result = ( DWORD ) -2;

   return Result;
}

HB_FUNC( PRINTFILERAW )
{
   const char *   cPrinterName, * cFileName, * cDocName;
   DWORD          Result = ( DWORD ) -1;

   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
   {
      cPrinterName   = hb_parcx( 1 );
      cFileName      = hb_parcx( 2 );
      cDocName       = ( ISCHAR( 3 ) ? hb_parcx( 3 ) : cFileName );
      Result         = hb_PrintFileRaw( cPrinterName, cFileName, cDocName );
   }
   hb_retnl( Result );
}

HB_FUNC( GETPRINTERS )
{
   HANDLE            hPrinter;
   DWORD             Flags             = PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS;
   BOOL              bPrinterNamesOnly = TRUE;
   BOOL              bLocalPrintersOnly;
   PRINTER_INFO_4 *  buffer4, * pPrinterEnum4;
   PRINTER_INFO_5 *  buffer, * pPrinterEnum;
   PRINTER_INFO_2 *  pPrinterInfo2;
   ULONG             needed = 0, returned = 0, a;
   PHB_ITEM          SubItems, File, Port, Net, Driver, Share, ArrayPrinter = hb_itemNew( NULL );

   hb_arrayNew( ArrayPrinter, 0 );

   buffer = NULL;
   HB_TRACE( HB_TR_DEBUG, ( "GETPRINTERS()" ) );

   if( ISLOG( 1 ) )
      bPrinterNamesOnly = ! hb_parl( 1 );

   bLocalPrintersOnly = ISLOG( 2 ) ? hb_parl( 2 ) : FALSE;

   if( hb_iswinnt() )
   {
      EnumPrinters( Flags, NULL, 4, ( LPBYTE ) NULL, 0, &needed, &returned );

      if( needed > 0 )
      {
         pPrinterEnum4 = buffer4 = ( PRINTER_INFO_4 * ) hb_xgrab( needed );
         if( pPrinterEnum4 )
         {
            if( EnumPrinters( Flags, NULL, 4, ( LPBYTE ) pPrinterEnum4, needed, &needed, &returned ) )
            {
               if( bPrinterNamesOnly )
               {
                  for( a = 0; a < returned; a++, pPrinterEnum4++ )
                  {
                     if( ! bLocalPrintersOnly || pPrinterEnum4->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                     {
                        File = hb_itemNew( NULL );
                        hb_itemPutC( File, pPrinterEnum4->pPrinterName );
                        hb_arrayAddForward( ArrayPrinter, File );
                        hb_itemRelease( File );
                     }
                  }
               }
               else
               {
                  for( a = 0; a < returned; a++, pPrinterEnum4++ )
                  {
                     if( ! bLocalPrintersOnly || pPrinterEnum4->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                     {
                        if( OpenPrinter( pPrinterEnum4->pPrinterName, &hPrinter, NULL ) )
                        {
                           GetPrinter( hPrinter, 2, NULL, 0, &needed );
                           if( needed > 0 )
                           {
                              pPrinterInfo2 = ( PRINTER_INFO_2 * ) hb_xgrab( needed );
                              if( pPrinterInfo2 )
                              {
                                 SubItems = hb_itemNew( NULL );
                                 hb_arrayNew( SubItems, 0 );
                                 File     = hb_itemNew( NULL );
                                 Port     = hb_itemNew( NULL );
                                 Driver   = hb_itemNew( NULL );
                                 Net      = hb_itemNew( NULL );
                                 Share    = hb_itemNew( NULL );
                                 hb_itemPutC( File, pPrinterEnum4->pPrinterName );

                                 if( GetPrinter( hPrinter, 2, ( LPBYTE ) pPrinterInfo2, needed, &needed ) )
                                 {
                                    hb_itemPutC( Port, pPrinterInfo2->pPortName );
                                    hb_itemPutC( Driver, pPrinterInfo2->pDriverName );
                                    hb_itemPutC( Share, pPrinterInfo2->pShareName );
                                 }
                                 else
                                 {
                                    hb_itemPutC( Port, "Error" );
                                    hb_itemPutC( Driver, "Error" );
                                    hb_itemPutC( Share, "Error" );
                                 }

                                 if( pPrinterEnum4->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                                    hb_itemPutC( Net, "LOCAL" );
                                 else
                                 {
                                    if( pPrinterEnum4->Attributes & PRINTER_ATTRIBUTE_NETWORK )
                                       hb_itemPutC( Net, "NETWORK" );
                                    else
                                       hb_itemPutC( Net, "ERROR" );
                                 }

                                 hb_arrayAddForward( SubItems, File );
                                 hb_arrayAddForward( SubItems, Port );
                                 hb_arrayAddForward( SubItems, Net );
                                 hb_arrayAddForward( SubItems, Driver );
                                 hb_arrayAddForward( SubItems, Share );

                                 hb_arrayAddForward( ArrayPrinter, SubItems );
                                 hb_xfree( pPrinterInfo2 );
                                 hb_itemRelease( File );
                                 hb_itemRelease( Port );
                                 hb_itemRelease( Net );
                                 hb_itemRelease( Driver );
                                 hb_itemRelease( Share );
                                 hb_itemRelease( SubItems );
                              }
                           }
                           ClosePrinter( hPrinter );
                        }
                     }
                  }
               }
            }
            hb_xfree( buffer4 );
         }
      }
   }
   else
   {
      EnumPrinters( Flags, NULL, 5, ( LPBYTE ) buffer, 0, &needed, &returned );

      if( needed > 0 )
      {
         pPrinterEnum = buffer = ( PRINTER_INFO_5 * ) hb_xgrab( needed );
         if( pPrinterEnum )
         {
            if( EnumPrinters( Flags, NULL, 5, ( LPBYTE ) buffer, needed, &needed, &returned ) )
            {
               for( a = 0; a < returned; a++, pPrinterEnum++ )
               {
                  if( ! bLocalPrintersOnly || pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                  {
                     if( bPrinterNamesOnly )
                     {
                        File = hb_itemNew( NULL );
                        hb_itemPutC( File, pPrinterEnum->pPrinterName );
                        hb_arrayAddForward( ArrayPrinter, File );
                        hb_itemRelease( File );
                     }
                     else
                     {
                        /* Tony (ABC)   11/1/2005        1:40PM. */
                        for( a = 0; a < returned; a++, pPrinterEnum++ )
                        {
                           if( ! bLocalPrintersOnly || pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                           {
                              if( OpenPrinter( pPrinterEnum->pPrinterName, &hPrinter, NULL ) )
                              {
                                 GetPrinter( hPrinter, 2, NULL, 0, &needed );
                                 if( needed > 0 )
                                 {
                                    pPrinterInfo2 = ( PRINTER_INFO_2 * ) hb_xgrab( needed );
                                    if( pPrinterInfo2 )
                                    {
                                       SubItems = hb_itemNew( NULL );
                                       hb_arrayNew( SubItems, 0 );
                                       File     = hb_itemNew( NULL );
                                       Port     = hb_itemNew( NULL );
                                       Driver   = hb_itemNew( NULL );
                                       Net      = hb_itemNew( NULL );
                                       Share    = hb_itemNew( NULL );

                                       hb_itemPutC( File, pPrinterEnum->pPrinterName );

                                       if( GetPrinter( hPrinter, 2, ( LPBYTE ) pPrinterInfo2, needed, &needed ) )
                                       {
                                          hb_itemPutC( Port, pPrinterInfo2->pPortName );
                                          hb_itemPutC( Driver, pPrinterInfo2->pDriverName );
                                          hb_itemPutC( Share, pPrinterInfo2->pShareName );
                                       }
                                       else
                                       {
                                          hb_itemPutC( Port, "Error" );
                                          hb_itemPutC( Driver, "Error" );
                                          hb_itemPutC( Share, "Error" );
                                       }

                                       if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                                          hb_itemPutC( Net, "LOCAL" );
                                       else
                                       {
                                          if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_NETWORK )
                                             hb_itemPutC( Net, "NETWORK" );
                                          else
                                             hb_itemPutC( Net, "ERROR" );
                                       }

                                       hb_arrayAddForward( SubItems, File );
                                       hb_arrayAddForward( SubItems, Port );
                                       hb_arrayAddForward( SubItems, Net );
                                       hb_arrayAddForward( SubItems, Driver );
                                       hb_arrayAddForward( SubItems, Share );
                                       hb_arrayAddForward( ArrayPrinter, SubItems );
                                       hb_itemRelease( File );
                                       hb_itemRelease( Port );
                                       hb_itemRelease( Net );
                                       hb_itemRelease( Driver );
                                       hb_itemRelease( Share );
                                       hb_itemRelease( SubItems );

                                       hb_xfree( pPrinterInfo2 );
                                    }
                                 }
                                 ClosePrinter( hPrinter );
                              }
                           }
                        }
/*
*                         // Tony (ABC)   11/1/2005        1:40PM. Old Code... Justo in case.
*                         hb_arrayNew( &SubItems, 0 );
*                         hb_itemPutC( &File, pPrinterEnum->pPrinterName );
*                         hb_itemPutC( &Port, pPrinterEnum->pPortName );
*
*                         if ( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL)
*                         {
*                            hb_itemPutC( &Net,"LOCAL" );
*                         }
*                         else
*                         {
*                            if ( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_NETWORK)
*                            {
*                               hb_itemPutC( &Net,"NETWORK" );
*                            }
*                            else
*                            {
*                               hb_itemPutC( &Net, "ERROR" );
*                            }
*                         }
*
*                         hb_arrayAddForward( &SubItems, &File ) ;
*                         hb_arrayAddForward( &SubItems, &Port ) ;
*                         hb_arrayAddForward( &SubItems, &Net ) ;
*                         hb_arrayAddForward( &ArrayPrinter , &SubItems );
*/
                     }
                  }
               }
            }
            hb_xfree( buffer );
         }
      }
   }

   hb_itemReturnRelease( ArrayPrinter );
}

#endif
