//---------------------------------------------------------------------//
//---------------------------------------------------------------------//
//---------------------------------------------------------------------//
// 
//                         Internet Functions
//
//                 Requires WinINet.dll and WinInet.h
//
//
//                 Pritpal Bedi <vouch32@vouchcac.com>
//
//---------------------------------------------------------------------//
//---------------------------------------------------------------------//
//---------------------------------------------------------------------//

#define _WIN32_WINNT   0x0400

#include   <shlobj.h>
#include   <windows.h>
#include   <shellApi.h>
#include   <wininet.h>
#include   "hbapi.h"
#include   "hbvm.h"
#include   "hbstack.h"
#include   "hbapiitm.h"
#include   "winreg.h"

//---------------------------------------------------------------------//
/*
   DWORD InternetDial(
       IN HWND     hwndParent,
       IN LPTSTR   lpszConnectoid,
       IN DWORD    dwFlags,
       OUT LPDWORD lpdwConnection,
       IN DWORD    dwReserved
   );
*/
//
//     InternetDial()
//
HB_FUNC ( INTERNETDIAL )
{
   HWND    hWnd   = ISNIL( 1 ) ? NULL : ( HWND ) hb_parnl( 1 ) ;
   LPTSTR  lpszId = ISNIL( 2 ) ? NULL : hb_parc( 2 ) ;
   DWORD   nFlags = INTERNET_AUTODIAL_FORCE_ONLINE ;
   DWORD   nRet   = 0;
   
   hb_retnl( InternetDialA( hWnd, lpszId, nFlags, &nRet, 0 ) );
   
}

//---------------------------------------------------------------------//
/*
   BOOL InternetGetConnectedState(
       OUT LPDWORD lpdwFlags,
       IN DWORD    dwReserved
   );
*/
//
//     lIsOn := InternetGetConnectedState()
//
HB_FUNC ( INTERNETGETCONNECTEDSTATE )
{
   hb_retl( InternetGetConnectedState( NULL, 0 ) ) ;
}

//---------------------------------------------------------------------//
/*
   HINTERNET InternetOpen(
       IN LPCTSTR lpszAgent,
       IN DWORD   dwAccessType,
       IN LPCTSTR lpszProxyName,
       IN LPCTSTR lpszProxyBypass,
       IN DWORD   dwFlags
   );
*/
//
//   hInternet := InternetOpen()
//   if hInternet <> 0
//       hFtp := InternetConnect( hInternet, 'vouchcac.com', ;
//                   INTERNET_DEFAULT_FTP_PORT, cUserName, cPassword, ;
//                        INTERNET_SERVICE_FTP )
//       if hFtp <> 0
//          if FtpOpenFile( hFtp, 'Temp/Testing.txt', GENERIC_WRITE )
//             cBuffer  := 'This is testing string' + chr( 13 ) + chr( 10 )
//             lSuccess := InternetWrite( hFtp, cBuffer, len( cBuffer ), @nWritten )
//             if lSuccess
//                ? nWritten 
//             endif
//          endif
//          InternetCloseHandle( hFtp )
//       endif
//       InternetCloseHandle( hInternet )
//   endif
//
//
//
HB_FUNC ( INTERNETOPEN )
{
   LPCTSTR lpszAgent       = ISNIL( 1 ) ? NULL : hb_parc( 1 ) ;
   DWORD   dwAccessType    = ISNIL( 2 ) ? INTERNET_OPEN_TYPE_DIRECT : hb_parnl( 2 ) ;
   LPCTSTR lpszProxyName   = ISNIL( 3 ) ? NULL : hb_parc( 3 ) ; 
   LPCTSTR lpszProxyBypass = ISNIL( 4 ) ? NULL : hb_parc( 4 ) ;
   DWORD   dwFlags         = ISNIL( 5 ) ? NULL : hb_parnl( 5 ) ;	
	
   hb_retnl( ( ULONG ) InternetOpenA( lpszAgent, dwAccessType, lpszProxyName, lpszProxyBypass, dwFlags ) ) ;
}

//---------------------------------------------------------------------//
/*
   HINTERNET InternetConnect(
     IN HINTERNET hInternet,
     IN LPCTSTR   lpszServerName,
     IN INTERNET_PORT nServerPort,
     IN LPCTSTR   lpszUserName,
     IN LPCTSTR   lpszPassword,
     IN DWORD     dwService,
     IN DWORD     dwFlags,
     IN DWORD_PTR dwContext
   );
*/
//
//       hFtp := InternetConnect( hInternet, 'chcac.com', ;
//                   INTERNET_DEFAULT_FTP_PORT, cUserName, cPassword, ;
//                        INTERNET_SERVICE_FTP )
//
HB_FUNC ( INTERNETCONNECT )
{
   HINTERNET     hInternet      = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCTSTR       lpszServerName = hb_parc( 2 )  ;
   INTERNET_PORT nServerPort    = ISNIL( 3 ) ? INTERNET_DEFAULT_HTTP_PORT : hb_parni( 3 ) ;
   LPCTSTR       lpszUserName   = ISNIL( 4 ) ? NULL : hb_parc( 4 ) ;
   LPCTSTR       lpszPassword   = ISNIL( 5 ) ? NULL : hb_parc( 5 ) ;
   DWORD         dwService      = ISNIL( 6 ) ? INTERNET_SERVICE_HTTP : hb_parnl( 6 ) ;
   DWORD         dwFlags        = ISNIL( 7 ) ? NULL : hb_parnl( 7 ) ;
   DWORD_PTR     dwContext		  = ISNIL( 8 ) ? NULL : hb_parnl( 8 ) ;
	
   hb_retnl( ( ULONG ) InternetConnectA( hInternet,    lpszServerName, 
	                           nServerPort, lpszUserName, lpszPassword,   
	                           dwService, dwFlags,      dwContext ) ) ;
}

//---------------------------------------------------------------------//
/*
   HINTERNET FtpOpenFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszFileName,
       IN DWORD     dwAccess,
       IN DWORD     dwFlags,
       IN DWORD_PTR dwContext
   );
*/
//
//    if FtpOpenFile( hInternet, 'Temp/Config.sys', GENERIC_WRITE )
//       // take next step
//    endif
//
HB_FUNC ( FTPOPENFILE )
{
   HINTERNET hFtp         = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCTSTR   lpszFileName = hb_parc( 2 ) ;
   DWORD     dwAccess     = ISNIL( 3 ) ? GENERIC_READ : hb_parni( 3  ) ;
   DWORD     dwFlags      = ISNIL( 4 ) ? FTP_TRANSFER_TYPE_BINARY : hb_parni( 4 ) ;
   DWORD_PTR dwContext    = ISNIL( 5 ) ? NULL : hb_parnl( 5 ) ;
	
   hb_retl( FtpOpenFileA( hFtp, lpszFileName, dwAccess, dwFlags, dwContext ) ) ;
}

//---------------------------------------------------------------------//
/*
   BOOL InternetWriteFile(
       IN HINTERNET hFile,
       IN LPCVOID   lpBuffer,
       IN DWORD     dwNumberOfBytesToWrite,
       OUT LPDWORD  lpdwNumberOfBytesWritten
   );
*/
//
//    if InternetWriteFile( hFile, @cBuffer, len( cBuffer ), @nWritten )
//       // Take next step
//    endif
//
HB_FUNC ( INTERNETWRITEFILE )
{
   HINTERNET hFile                    = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCVOID   lpBuffer                 = hb_parc( 2 ) ;
   DWORD     dwNumberOfBytesToWrite   = ( DWORD ) hb_parnl( 3 ) ;
   LPDWORD   lpdwNumberOfBytesWritten = ( DWORD ) 0 ;
	
   hb_retl( InternetWriteFile( hFile, lpBuffer, dwNumberOfBytesToWrite, 
   	                                          lpdwNumberOfBytesWritten ) ) ;
	                                          
   if ISBYREF( 4 )
      hb_stornl( ( ULONG ) lpdwNumberOfBytesWritten, 4 ) ;
}

//---------------------------------------------------------------------//
/*
   BOOL InternetReadFile(
       IN HINTERNET hFile,
       IN LPVOID    lpBuffer,
       IN DWORD     dwNumberOfBytesToRead,
       OUT LPDWORD  lpdwNumberOfBytesRead
   );
*/
//
//     if InternetReadFile( hFile, @cBuffer, len( cBuffer ), @nRead )
//        // Write to local handle
//     endif
//
HB_FUNC ( INTERNETREADFILE )
{
   HINTERNET hFile                    = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCVOID   lpBuffer                 = hb_parc( 2 ) ;
   DWORD     dwNumberOfBytesToRead    = ( DWORD ) hb_parnl( 3 ) ;
   LPDWORD   lpdwNumberOfBytesRead    = ( DWORD ) 0  ;
   BOOL      bRet ;
	
   bRet = InternetReadFile( hFile, &lpBuffer, 
          	               dwNumberOfBytesToRead, lpdwNumberOfBytesRead ) ;

   hb_retl( bRet ); 
	
   if ( bRet )
   {	                                          
      if ISBYREF( 4 )
      {
         hb_stornl( ( ULONG ) lpdwNumberOfBytesRead, 4 ) ;
      }
         hb_storclen( ( char * ) lpBuffer, ( ULONG ) lpdwNumberOfBytesRead, 2 ) ;
   }	
}

//---------------------------------------------------------------------//
/*
   BOOL FtpCommand(
       IN HINTERNET  hConnect,
       IN BOOL       fExpectResponse,
       IN DWORD      dwFlags,
       IN LPCTSTR    lpszCommand,
       IN DWORD_PTR  dwContext,
       OUT HINTERNET *phFtpCommand
   );
*/
//
//       
//
HB_FUNC ( FTPCOMMAND )
{
   HINTERNET hInternet       = ( HINTERNET ) hb_parnl( 1 ) ;
   BOOL      fExpectResponse = ISNIL( 2 ) ? 0 : hb_parl( 2 ) ;
   DWORD     dwFlags         = ISNIL( 3 ) ? FTP_TRANSFER_TYPE_BINARY : hb_parnl( 3 ) ;
   LPCTSTR   lpszCommand     = hb_parc( 4 ) ;
   DWORD_PTR dwContext       = ISNIL( 5 ) ? NULL : hb_parnl( 5 ) ;
   HINTERNET *phFtpCommand ;
	
   BOOL      bRet ;
	
   bRet = FtpCommand( hInternet, fExpectResponse, dwFlags, lpszCommand,
	                   dwContext, phFtpCommand ) ;
	                   
   hb_retl( bRet ) ;

   if ( bRet )
   {
      if ( ISBYREF( 6 ) )
         hb_stornl( ( ULONG ) phFtpCommand, 6 ) ;
   }
}

//---------------------------------------------------------------------//
/*
typedef struct _WIN32_FIND_DATAA {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    CHAR   cFileName[ MAX_PATH ];
    CHAR   cAlternateFileName[ 14 ];
#ifdef _MAC
    DWORD dwFileType;
    DWORD dwCreatorType;
    WORD  wFinderFlags;
#endif
} WIN32_FIND_DATAA, *PWIN32_FIND_DATAA, *LPWIN32_FIND_DATAA;
*/

/*
   HINTERNET FtpFindFirstFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszSearchFile,
       OUT LPWIN32_FIND_DATA lpFindFileData,
       IN DWORD     dwFlags,
       IN DWORD_PTR dwContext
   );
*/
//
//   aDirInfo := array( 3 )
//   hFind := FtpFindFirstFile( hInternet, '*.*', @aDirInfo )
//
//   ? aDirInfo[ 1 ]  // File Name
//   ? aDirInfo[ 2 ]  // File attribute in numeric, 16 for directory, 128 for file
//   ? aDirInfo[ 3 ]  // File size in bytes
//
HB_FUNC ( FTPFINDFIRSTFILE )
{
   HINTERNET hInternet              = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCTSTR   lpszSearchFile         = ISNIL( 2 ) ? TEXT ("*.*") : hb_parc( 2 ) ;
   WIN32_FIND_DATA lpFindFileData ;
   DWORD     dwFlags                = ISNIL( 4 ) ? INTERNET_FLAG_NEED_FILE : hb_parnl( 4 ) ;
   DWORD_PTR dwContext              = ISNIL( 5 ) ? NULL : hb_parnl( 5 ) ;
   HINTERNET hResult ; 	

   hResult = FtpFindFirstFile( hInternet, lpszSearchFile, 
			                            &lpFindFileData, dwFlags, dwContext ) ;
        
   if ( hResult )
      if ( ISBYREF( 3 ) )   	   
      {
         hb_storc ( lpFindFileData.cFileName        , 3, 1 ) ;
	 hb_stornl( lpFindFileData.dwFileAttributes , 3, 2 ) ;
	 hb_stornl( lpFindFileData.nFileSizeLow     , 3, 3 ) ;
      }
		
   hb_retnl( ( ULONG ) hResult ) ;
}

//---------------------------------------------------------------------//
/*
   BOOL InternetFindNextFile(
       IN HINTERNET hFind,
       OUT LPVOID   lpvFindData
   );
*/
//
//   aDirInfo := array( 3 )
//
//   hFind := FtpFindFirstFile( hInternet, '*.*', @aDirInfo )
//   if hFind <> 0
//      do while InternetFindNextFile( hFind, @aDirInfo )
//         ? aDirInfo[ 1 ]  // File Name
//         ? aDirInfo[ 2 ]  // File attribute in numeric, 16 for directory, 128 for file
//         ? aDirInfo[ 3 ]  // File size in bytes
//      enddo
//   endif
//
//
HB_FUNC ( INTERNETFINDNEXTFILE )
{
   HINTERNET       hFind       = ( HINTERNET ) hb_parnl( 1 ) ;
   WIN32_FIND_DATA lpFindFileData ;
	
   if ( InternetFindNextFile( hFind, &lpFindFileData ) ) 
      {   
         hb_retl( TRUE );
         if ( ISBYREF( 2 ) )
         {
            hb_storc ( lpFindFileData.cFileName        , 2, 1 ) ;
	    hb_stornl( lpFindFileData.dwFileAttributes , 2, 2 ) ;
	    hb_stornl( lpFindFileData.nFileSizeLow     , 2, 3 ) ;
         }
      }
   else
      hb_retl(FALSE) ;
	
}

//---------------------------------------------------------------------//
/*
   BOOL FtpGetFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszRemoteFile,
       IN LPCTSTR   lpszNewFile,
       IN BOOL      fFailIfExists,
       IN DWORD     dwFlagsAndAttributes,
       IN DWORD     dwFlags,
       IN DWORD_PTR dwContext
   );
*/
//
//   if FtpGetFile( hInternet, cRemoteFile, cLocalFile, lFailIfExist )
//      ? 'Success'
//   endif
//
HB_FUNC ( FTPGETFILE )
{
   HINTERNET hInternet            = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCTSTR   lpszRemoteFile       = hb_parc( 2 ) ;
   LPCTSTR   lpszLocalFile        = hb_parc( 3 ) ;
   BOOL      fFailIfExist         = ISNIL( 4 ) ? FALSE : hb_parl( 4 ) ;
   DWORD     dwFlagsAndAttributes = ISNIL( 5 ) ? FILE_ATTRIBUTE_NORMAL : hb_parnl( 5 ) ;
   DWORD     dwFlags              = ISNIL( 6 ) ? FTP_TRANSFER_TYPE_BINARY | INTERNET_FLAG_RELOAD : hb_parnl( 6 ) ;
   DWORD_PTR dwContext            = ISNIL( 7 ) ? 0 : hb_parnl( 7 ) ;
	
   hb_retl( FtpGetFile( hInternet, lpszRemoteFile, lpszLocalFile, 
	                     fFailIfExist, dwFlagsAndAttributes,
	                     dwFlags, dwContext ) ) ;
}

//---------------------------------------------------------------------//
/*
   BOOL FtpPutFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszLocalFile,
       IN LPCTSTR   lpszNewRemoteFile,
       IN DWORD     dwFlags,
       IN DWORD_PTR dwContext
   );
*/
//
//   if FtpPutFile( hInternet, cLocalFile, cRemoteFile )
//      ?
//   endif
//
HB_FUNC ( FTPPUTFILE )
{
   HINTERNET hInternet            = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCTSTR   lpszLocalFile        = hb_parc( 2 ) ;
   LPCTSTR   lpszRemoteFile       = hb_parc( 3 ) ;
   DWORD     dwFlags              = ISNIL( 4 ) ? FTP_TRANSFER_TYPE_BINARY | INTERNET_FLAG_RELOAD : hb_parnl( 4 ) ;
   DWORD_PTR dwContext            = ISNIL( 5 ) ? 0 : hb_parnl( 5 ) ;
	
   hb_retl( FtpPutFile( hInternet, lpszLocalFile, lpszRemoteFile, dwFlags, dwContext ) ) ;
}

//---------------------------------------------------------------------//
/*
   BOOL FtpCreateDirectory(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszDirectory
   );
*/
//
//   if FtpCreateDirectory( hInternet, 'Temp' )
//      ? 'Success'
//   endif
//
HB_FUNC ( FTPCREATEDIRECTORY )
{
   HINTERNET hInternet     = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCTSTR   lpszDirectory = hb_parc( 2 ) ;
	
   hb_retl( FtpCreateDirectoryA( hInternet, lpszDirectory ) ) ;
	
}

//---------------------------------------------------------------------//
/*
   BOOL FtpRemoveDirectory(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszDirectory
   );
*/
//
//   if FtpRemoveDirectory( hInternet, cDirectory )
//      ? 'Success'
//   endif
//
HB_FUNC ( FTPREMOVEDIRECTORY )
{
   HINTERNET hInternet     = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCTSTR   lpszDirectory = hb_parc( 2 ) ;
	
   hb_retl( FtpRemoveDirectoryA( hInternet, lpszDirectory ) ) ;

}

//---------------------------------------------------------------------//
/*
   BOOL FtpDeleteFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszFileName
   );
*/
//
//   if FtpDeleteFile( hInternet, 'Temp\Config.sys' )
//      ? 'Sucess'
//   endif
//
HB_FUNC ( FTPDELETEFILE )
{
   HINTERNET hInternet    = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCTSTR   lpszFileName = hb_parc( 2 ) ;
	
   hb_retl( FtpDeleteFile( hInternet, lpszFileName ) ) ;
	
}

//---------------------------------------------------------------------//
/*
   BOOL FtpRenameFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszExisting,
       IN LPCTSTR   lpszNew
   );
*/
//
//   if FtpRenameFile( hInternet, cExisting, cNew )
//      ? 'Success'
//   endif
//
HB_FUNC ( FTPRENAMEFILE )
{
   HINTERNET hInternet    = ( HINTERNET ) hb_parnl( 1 ) ;
   LPCTSTR   lpszExisting = hb_parc( 2 ) ;
   LPCTSTR   lpszNew      = hb_parc( 3 ) ;

   hb_retl( FtpRenameFileA( hInternet, lpszExisting, lpszNew ) ) ;

}

//---------------------------------------------------------------------//
/*
   BOOL FtpGetCurrentDirectory(
       IN HINTERNET   hConnect,
       OUT LPTSTR     lpszCurrentDirectory,
       IN OUT LPDWORD lpdwCurrentDirectory
   );
*/
//
//   if FtpGetCurrentDirectory( hInternet, @cDirectory )
//      ? cDirectory
//   endif
//
HB_FUNC ( FTPGETCURRENTDIRECTORY )
{
   HINTERNET hInternet           = ( HINTERNET ) hb_parnl( 1 ) ;
   LPTSTR   lpszCurrentDirectory = hb_xgrab( MAX_PATH ) ; 
   DWORD    dwCurrentDirectory   = MAX_PATH     ;
   BOOL     bRet ;
	
   bRet = FtpGetCurrentDirectory( hInternet, lpszCurrentDirectory, &dwCurrentDirectory ) ;
   hb_retl( bRet ) ;
	
   if ( bRet )
   {
      if ( ISBYREF( 2 ) )
         hb_storclen( ( char * ) lpszCurrentDirectory, ( ULONG ) dwCurrentDirectory, 2 ) ;
   }
	
   hb_xfree( lpszCurrentDirectory ) ;
}

//---------------------------------------------------------------------//
/*
   BOOL FtpSetCurrentDirectory(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszDirectory
   );
*/
//
//    if FtpSetCurrentDirectory( hInternet, cDirectory )
//       ? 'Success'
//    endif
//
HB_FUNC ( FTPSETCURRENTDIRECTORY )
{
   HINTERNET hInternet     = ( HINTERNET ) hb_parnl( 1 ) ;
   LPTSTR    lpszDirectory = hb_parc( 2 ) ;

   hb_retl( FtpSetCurrentDirectoryA( hInternet, lpszDirectory ) ) ;
}

//---------------------------------------------------------------------//
/*
   BOOL InternetCloseHandle(
       IN HINTERNET hInternet
   );  
*/
//
//    if InternetCloseHandle( hInternet )
//       ? 'Success'
//    endif
//
HB_FUNC ( INTERNETCLOSEHANDLE )
{
   HINTERNET hInternet = ( HINTERNET ) hb_parnl( 1 ) ;
	
   hb_retl( InternetCloseHandle( hInternet ) ) ;
}

//---------------------------------------------------------------------//
/*
   DWORD InternetAttemptConnect(
       IN DWORD dwReserved
   );
*/
//
//    InternetAttempConnect()
//
HB_FUNC ( INTERNETATTEMPTCONNECT )
{
   DWORD dwReserved = 0 ;
	
   hb_retnl( ( ULONG ) InternetAttemptConnect( dwReserved ) ) ;
}

//---------------------------------------------------------------------//

