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

HB_FUNC ( INTERNETDIAL )
{
   HWND    hWnd   = NULL;    // ( HWND ) GetDeskTopWindow() ;
   LPTSTR  lpszId = ISNIL( 1 ) ? NULL : hb_parc( 1 ) ;
   DWORD   nFlags = INTERNET_AUTODIAL_FORCE_ONLINE ;
   LPDWORD nRet   = 0;
   
   hb_retnl( InternetDialA( hWnd, lpszId, nFlags, nRet, 0 ) );
   
}

//---------------------------------------------------------------------//

HB_FUNC ( INTERNETGETCONNECTEDSTATE )
{
	hb_retl( InternetGetConnectedState( NULL, 0 ) ) ;
}

//---------------------------------------------------------------------//
//
//   hInternet := InternetOpen()
//   if hInternet <> 0
//       hFtp := InternetConnect( hInternet, 'chcac.com', ;
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
//
//   InternetOpenFile( hInternet, 'Temp/Config.sys', GENERIC_WRITE )
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
//
//    InternetWriteFile( hFile, @cBuffer, len( cBuffer ), @nWritten )
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
//
//    InternetReadFile( hFile, @cBuffer, len( cBuffer ), @nRead )
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
//
//   Fills the structure which have to be resolved
//
HB_FUNC ( FTPFINDFIRSTFILE )
{
	HINTERNET hInternet              = ( HINTERNET ) hb_parnl( 1 ) ;
	LPCTSTR   lpszSearchFile         = ISNIL( 2 ) ? TEXT ("*.*") : hb_parc( 2 ) ;
	LPWIN32_FIND_DATA lpFindFileData ;
	DWORD     dwFlags                = ISNIL( 4 ) ? INTERNET_FLAG_NEED_FILE : hb_parnl( 4 ) ;
	DWORD_PTR dwContext              = ISNIL( 5 ) ? NULL : hb_parnl( 5 ) ;
	
	hb_retnl( ( ULONG ) FtpFindFirstFile( hInternet, lpszSearchFile, 
			                       lpFindFileData, dwFlags, dwContext ) ) ;
	
}

//---------------------------------------------------------------------//
//
//   FtpGetFile( hInternet, cRemoteFile, cLocalFile, lFailIfExist )
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
//
//   FtpPutFile( hInternet, cLocalFile, cRemoteFile )
//
HB_FUNC ( FTPPUTFILE )
{
	HINTERNET hInternet            = ( HINTERNET ) hb_parnl( 1 ) ;
	LPCTSTR   lpszLocalFile        = hb_parc( 2 ) ;
	LPCTSTR   lpszRemoteFile       = hb_parc( 3 ) ;
	DWORD     dwFlags              = ISNIL( 4 ) ? FTP_TRANSFER_TYPE_BINARY | INTERNET_FLAG_RELOAD : hb_parnl( 4 ) ;
	DWORD_PTR dwContext            = ISNIL( 5 ) ? 0 : hb_parnl( 5 ) ;
	
	hb_retl( FtpPutFile( hInternet, lpszLocalFile, lpszRemoteFile, 
							                     dwFlags, dwContext ) ) ;
}

//---------------------------------------------------------------------//
//
//   if FtpCreateDirectory( hInternet, 'Temp' )
//      ? 'Sucess'
//   endif
//
HB_FUNC ( FTPCREATEDIRECTORY )
{
	HINTERNET hInternet     = ( HINTERNET ) hb_parnl( 1 ) ;
	LPCTSTR   lpszDirectory = hb_parc( 2 ) ;
	
	hb_retl( FtpCreateDirectoryA( hInternet, lpszDirectory ) ) ;
	
}

//---------------------------------------------------------------------//
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
//
//   FtpRenameFile( hInternet, cExisting, cNew )
//
HB_FUNC ( FTPRENAMEFILE )
{
	HINTERNET hInternet    = ( HINTERNET ) hb_parnl( 1 ) ;
	LPCTSTR   lpszExisting = hb_parc( 2 ) ;
	LPCTSTR   lpszNew      = hb_parc( 3 ) ;

	hb_retl( FtpRenameFileA( hInternet, lpszExisting, lpszNew ) ) ;

}

//---------------------------------------------------------------------//
//
//   if FtpGetCurrentDirectory( hInternet, @cDirectory )
//      ? cDirectory
//   endif
//
HB_FUNC ( FTPGETCURRENTDIRECTORY )
{
	HINTERNET hInternet            = ( HINTERNET ) hb_parnl( 1 ) ;
	LPTSTR    lpszCurrentDirectory = hb_parc( 2 ) ;
	DWORD     dwtemp               = MAX_PATH     ;
	LPDWORD   lpdwCurrentDirectory = &dwtemp      ;
	
	BOOL      bRet ;
	
	bRet = FtpGetCurrentDirectoryA( hInternet, lpszCurrentDirectory, lpdwCurrentDirectory ) ;
	
	hb_retl( bRet ) ;
	
	if ( bRet )
		hb_storclen( ( char * ) lpszCurrentDirectory, ( ULONG ) lpdwCurrentDirectory, 2 ) ;

}

//---------------------------------------------------------------------//
//
//   FtpSetCurrentDirectory( hInternet, cDirectory )
//
HB_FUNC ( FTPSETCURRENTDIRECTORY )
{
	HINTERNET hInternet     = ( HINTERNET ) hb_parnl( 1 ) ;
	LPTSTR    lpszDirectory = hb_parc( 2 ) ;
	

	hb_retl( FtpSetCurrentDirectoryA( hInternet, lpszDirectory ) ) ;
}

//---------------------------------------------------------------------//

HB_FUNC ( INTERNETCLOSEHANDLE )
{
	HINTERNET hInternet = ( HINTERNET ) hb_parnl( 1 ) ;
	
	hb_retl( InternetCloseHandle( hInternet ) ) ;
}

//---------------------------------------------------------------------//

HB_FUNC ( INTERNETATTEMPTCONNECT )
{
	DWORD dwReserved = 0 ;
	
	hb_retnl( ( ULONG ) InternetAttemptConnect( dwReserved ) ) ;
}

//---------------------------------------------------------------------//

