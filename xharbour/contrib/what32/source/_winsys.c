
// WHAT32

// System Services

/*
 * Some parts Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * with author's permission granted on 27 MAy 2002
    Last change:  WN   29 May 2002   10:48 pm
 */



#define _WIN32_WINNT   0x0400

#include <shlobj.h>
#include <windows.h>
#include <commctrl.h>
#include <htmlhelp.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"



//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetFreeSpace(UINT);


HB_FUNC( GETFREESPACE )
{
   hb_retnl( (LONG) GetFreeSpace( (UINT) hb_parni( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC ( OUTPUTDEBUGSTRING )
{
   OutputDebugString( (LPCSTR) hb_parc( 1 ) ) ;
}

//-----------------------------------------------------------------------------
//DWORD GetTimeZoneInformation(LPTIME_ZONE_INFORMATION lpTimeZoneInformation)

// SYNTAX:
// cTZI:=tzi:value
// GetTimeZoneInformation(@cTZI)
// tzi:Buffer(cTZI)

HB_FUNC( GETTIMEZONEINFORMATION )
{
 TIME_ZONE_INFORMATION tzi;

 hb_retnl( GetTimeZoneInformation( &tzi ) ) ;

 if ( ISBYREF(1) )
    hb_storclen( (char*) &tzi, sizeof(tzi), 1);

}

//-----------------------------------------------------------------------------
//BOOL SetTimeZoneInformation(TIME_ZONE_INFORMATION *TimeZoneInformation)

// SYNTAX: SetTimeZoneInformation(tzi:value)

HB_FUNC( SETTIMEZONEINFORMATION )
{
 TIME_ZONE_INFORMATION *tzi = ( TIME_ZONE_INFORMATION *) hb_param( 1, HB_IT_STRING )->item.asString.value ;

 hb_retl( SetTimeZoneInformation( tzi ) ) ;

}

//-----------------------------------------------------------------------------

// Win98 ++

/*
HB_FUNC ( ISDEBUGGERPRESENT )
{
   hb_retl( IsDebuggerPresent() ) ;
}
*/

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI DebugBreak( VOID );


HB_FUNC( DEBUGBREAK )
{
   DebugBreak(  ) ;
}


//-----------------------------------------------------------------------------
// WINADVAPI BOOL WINAPI EncryptFileA( IN LPCSTR lpFileName );

// NT ?
/*
HB_FUNC( ENCRYPTFILE )
{
   hb_retl( EncryptFileA( (LPCSTR) hb_parc( 1 ) ) ) ;
}
*/

//-----------------------------------------------------------------------------
// WINADVAPI BOOL WINAPI DecryptFileA( IN LPCSTR lpFileName, IN DWORD dwReserved );

// NT ?
/*
HB_FUNC( DECRYPTFILE )
{
   hb_retl( DecryptFileA( (LPCSTR) hb_parc( 1 ), 0 ) ) ; //(DWORD) hb_parnl( 2 ) ) ) ;
}
*/

//-----------------------------------------------------------------------------
// WINADVAPI BOOL WINAPI FileEncryptionStatusA( LPCSTR lpFileName, LPDWORD lpStatus );

/*

// need function info !

HB_FUNC( FILEENCRYPTIONSTATUSA )
{
   LPDWORD lpStatus   ;

   // Your code goes here

   hb_retl( FileEncryptionStatusA( (LPCSTR) hb_parc( 1 ), lpStatus ) ) ;
}

*/


//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI IsProcessorFeaturePresent( IN DWORD ProcessorFeature );


HB_FUNC( ISPROCESSORFEATUREPRESENT )
{
   hb_retl( IsProcessorFeaturePresent( (DWORD) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI int WINAPI MulDiv( IN int nNumber, IN int nNumerator, IN int nDenominator );


HB_FUNC( MULDIV )
{
   hb_retni( MulDiv( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SystemParametersInfoA( IN UINT uiAction, IN UINT uiParam, IN OUT PVOID pvParam, IN UINT fWinIni);

/*

HB_FUNC( SYSTEMPARAMETERSINFO )
{
   PVOID pvParam  ;

   // Your code goes here

   hb_retl( SystemParametersInfo( (UINT) hb_parni( 1 ),
                                  (UINT) hb_parni( 2 ),
                                  pvParam             ,
                                  (UINT) hb_parni( 4 )
                                ) ) ;
}

*/



//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI FreeResource( IN HGLOBAL hResData );

HB_FUNC( FREERESOURCE )
{
   hb_retl( FreeResource( (HGLOBAL) hb_parnl( 6 )) ) ;
}




//-----------------------------------------------------------------------------
// WINUSERAPI VOID WINAPI SetDebugErrorLevel( IN DWORD dwLevel );


HB_FUNC( SETDEBUGERRORLEVEL )
{
   SetDebugErrorLevel( (DWORD) hb_parnl( 1 ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI VOID WINAPI SetLastErrorEx( IN DWORD dwErrCode, IN DWORD dwType );


HB_FUNC( SETLASTERROREX )
{
   SetLastErrorEx( (DWORD) hb_parnl( 1 ), (DWORD) hb_parnl( 2 ) ) ;
}


//-----------------------------------------------------------------------------
/*
HANDLE GetStdHandle(DWORD nStdHandle )  // input, output, or error device
*/

HB_FUNC ( GETSTDHANDLE )
{
   hb_retnl( (LONG) GetStdHandle( (DWORD) hb_parnl(1) ) ) ;
}


//-----------------------------------------------------------------------------
/*
BOOL SetStdHandle(
                  DWORD nStdHandle,  // input, output, or error device
                  HANDLE hHandle     // handle to be a standard handle
);
*/ 

HB_FUNC ( SETSTDHANDLE )
{
   hb_retl( SetStdHandle( (DWORD) hb_parnl(1), (HANDLE) hb_parnl(2) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetSystemMetrics( IN int nIndex);


HB_FUNC( GETSYSTEMMETRICS )
{
   hb_retni( GetSystemMetrics( hb_parni( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI UINT_PTR WINAPI SetTimer( IN HWND hWnd, IN UINT_PTR nIDEvent, IN UINT uElapse, IN TIMERPROC lpTimerFunc);

HB_FUNC( SETTIMER )
{

   hb_retni( SetTimer( (HWND) hb_parnl( 1 ),
                       (UINT) hb_parni( 2 ),
                       (UINT) hb_parni( 3 ),
                       ISNIL(4) ? NULL : (TIMERPROC) hb_parnl(4)         
                      ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI KillTimer( IN HWND hWnd, IN UINT_PTR uIDEvent);

HB_FUNC( KILLTIMER )
{
   
   hb_retl( KillTimer( (HWND) hb_parnl( 1 ), (UINT) hb_parni(2) ) ) ;

}


//-----------------------------------------------------------------------------

HB_FUNC ( GETSYSCOLOR )
{
  hb_retnl( GetSysColor( hb_parni(1) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ExitWindowsEx( IN UINT uFlags, IN DWORD dwReserved);


HB_FUNC( EXITWINDOWSEX )
{
   hb_retl( ExitWindowsEx( (UINT) hb_parni( 1 ), (DWORD) hb_parnl( 2 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI HBRUSH WINAPI GetSysColorBrush( IN int nIndex);


HB_FUNC( GETSYSCOLORBRUSH )
{
   hb_retnl( (LONG) GetSysColorBrush( hb_parni( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetSysColors( IN int cElements, IN CONST INT * lpaElements, IN CONST COLORREF * lpaRgbValues);

/*

HB_FUNC( SETSYSCOLORS )
{
   COLORREF lpaRgbValues ;

   // Your code goes here

   hb_retl( SetSysColors( hb_parni( 1 ), hb_parni( 2 ), &lpaRgbValues ) ) ;
}

*/



//----------------------------------------------------------------------------

HB_FUNC ( AND )
{
  hb_retnl( hb_parnl(1) & hb_parnl(2) ) ;
}

//----------------------------------------------------------------------------

HB_FUNC ( OR )
{
  hb_retnl( hb_parnl(1) | hb_parnl(2) ) ;
}

//----------------------------------------------------------------------------

HB_FUNC( NOT )
{
   hb_retnl( ~( hb_parnl(1) ) ) ;
}


//-----------------------------------------------------------------------------

HB_FUNC ( _GETINSTANCE )
{
   hb_retnl( (LONG) GetModuleHandle( NULL ) );
}

//-----------------------------------------------------------------------------

HB_FUNC ( LOWORD )
{
   hb_retni( (int) ( hb_parnl( 1 ) & 0xFFFF ) );
}

//-----------------------------------------------------------------------------

HB_FUNC ( HIWORD )
{
   hb_retni( (int) ( ( hb_parnl( 1 ) >> 16 ) & 0xFFFF ) );
}

//-----------------------------------------------------------------------------

HB_FUNC (MAKELONG )
{

   hb_retnl( ((LONG) (((WORD) (hb_parni(1))) | ((DWORD) ((WORD) (hb_parni(2)))) << 16)) ) ;
}


//-----------------------------------------------------------------------------

HB_FUNC (GETLASTERROR )
{
  hb_retnl( (LONG) GetLastError() ) ;
}

//-----------------------------------------------------------------------------

// T.B.D.
// returns error message text

/*
HB_FUNC( FORMATMESSAGE)
{

   hb_retnl( FormatMessage( (DWORD) hb_parnl( 1 )           ,   // source and processing options
                            ISNIL( 2) ? NULL : hb_parc( 2 ) ,   // pointer to  message source
                            (DWORD) hb_parnl( 3 )           ,  // requested message identifier
                            (DWORD) hb_parnl( 4 )           , // language identifier for requested message
  LPTSTR lpBuffer,    // pointer to message buffer
  DWORD nSize,        // maximum size of message buffer
  va_list *Arguments  // pointer to array of message inserts
);
 
}
*/

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI SetLastError( IN DWORD dwErrCode );


HB_FUNC( SETLASTERROR )
{
   SetLastError( (DWORD) hb_parnl( 1 ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI SetErrorMode( IN UINT uMode );


HB_FUNC( SETERRORMODE )
{
   hb_retni( SetErrorMode( (UINT) hb_parni( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------


HB_FUNC( OEMTOANSI )
{
   char *buffer = hb_parc(1);
   OemToChar( buffer, buffer );
   hb_retc( buffer );
}

//-----------------------------------------------------------------------------


HB_FUNC( ANSITOOEM )
{
   char *buffer = hb_parc(1);
   CharToOem( buffer, buffer );
   hb_retc( buffer );
}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetVersion( VOID );


HB_FUNC( GETVERSION )
{
   hb_retnl( (LONG) GetVersion(  ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI HRSRC WINAPI FindResourceA( IN HMODULE hModule, IN LPCSTR lpName, IN LPCSTR lpType );


HB_FUNC( FINDRESOURCE )
{
   hb_retnl( (LONG) FindResourceA( (HMODULE) hb_parnl( 1 ),
                                   (LPCSTR) hb_parc( 2 )  ,
                                   (LPCSTR) hb_parc( 3 )  
                                   ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI HRSRC WINAPI FindResourceExA( IN HMODULE hModule, IN LPCSTR lpType, IN LPCSTR lpName, IN WORD wLanguage );

HB_FUNC( FINDRESOURCEEX )
{
   hb_retnl( (LONG) FindResourceExA( (HMODULE) hb_parnl( 1 ),
                                     (LPCSTR) hb_parc( 2 )  ,
                                     (LPCSTR) hb_parc( 3 )  ,
                                     (WORD) hb_parni( 4 )              
                                     ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI HGLOBAL WINAPI LoadResource( IN HMODULE hModule, IN HRSRC hResInfo );


HB_FUNC( LOADRESOURCE )
{
   hb_retnl( (LONG) LoadResource( (HMODULE) hb_parnl( 1 ),
                                  (HRSRC) hb_parnl( 2 )  
                                 ) ) ;
}


//-----------------------------------------------------------------------------
//int LoadString(HINSTANCE hInstance,  // handle to module containing string resource
//               UINT uID,             // resource identifier
//               LPTSTR lpBuffer,      // pointer to buffer for resource
//               int nBufferMax        // size of buffer
//              );

// modified
 
HB_FUNC( LOADSTRING )
{
   USHORT iLen = ISNIL(3) ? MAX_PATH : hb_parclen( 3 );
   LPTSTR cText = (char*) hb_xgrab( iLen+1 );

   iLen = LoadString( ( ISNIL(1) ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl(1) ),
                      (UINT) hb_parni(2) , 
                      (LPTSTR) cText ,
                      iLen ) ;
                     
   hb_retclen( cText, iLen );
   hb_xfree( cText );
}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI SizeofResource( IN HMODULE hModule, IN HRSRC hResInfo );


HB_FUNC( SIZEOFRESOURCE )
{
   hb_retnl( (LONG) SizeofResource( (HMODULE) hb_parnl( 1 ),
                                    (HRSRC) hb_parnl( 2 )  
                                    ) ) ;
}



//-----------------------------------------------------------------------------
// WINBASEAPI LPVOID WINAPI LockResource( IN HGLOBAL hResData );


HB_FUNC( LOCKRESOURCE )
{
   hb_retnl( (LONG) LockResource( (HGLOBAL) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI LoadModule( IN LPCSTR lpModuleName, IN LPVOID lpParameterBlock );

/*

HB_FUNC( LOADMODULE )
{
   LPVOID lpParameterBlock ;

   // Your code goes here

   hb_retnl( (LONG) LoadModule( (LPCSTR) hb_parc( 1 ), lpParameterBlock ) ) ;
}

*/


//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI Beep( IN DWORD dwFreq, IN DWORD dwDuration );


HB_FUNC( TONE )
{
   hb_retl( Beep( (DWORD) hb_parnl( 1 ), (DWORD) hb_parnl( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetModuleFileNameA( IN HMODULE hModule, OUT LPSTR lpFilename, IN DWORD nSize );


HB_FUNC( GETMODULEFILENAME )
{
   char szBuffer[ MAX_PATH + 1 ] = {0} ;
   GetModuleFileNameA( ISNIL(1) ? GetModuleHandle(NULL) : (HMODULE) hb_parnl( 1 ),
                       szBuffer  ,
                       MAX_PATH
                     ) ;
   hb_retc(szBuffer);
}

//-----------------------------------------------------------------------------
// WINBASEAPI HMODULE WINAPI GetModuleHandleA( IN LPCSTR lpModuleName );


HB_FUNC( GETMODULEHANDLE )
{
   
   hb_retnl( (LONG) GetModuleHandleA( (ISNIL(1) ? NULL : (LPCSTR) hb_parc( 1 ) ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI LPSTR WINAPI GetCommandLineA( VOID );


HB_FUNC( GETCOMMANDLINE )
{
// (LPSTR) GetCommandLineA(  ) ) ;
}



//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI GetSystemTime( OUT LPSYSTEMTIME lpSystemTime );

/*

HB_FUNC( GETSYSTEMTIME )
{
   LPSYSTEMTIME lpSystemTime ;

   // Your code goes here

   GetSystemTime( lpSystemTime ) ;
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI SetSystemTime( IN CONST SYSTEMTIME *lpSystemTime );

/*

HB_FUNC( SETSYSTEMTIME )
{
   SYSTEMTIME CONST lpSystemTime ;

   // Your code goes here

   hb_retl( SetSystemTime( &lpSystemTime ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI GetLocalTime( OUT LPSYSTEMTIME lpSystemTime );

/*

HB_FUNC( GETLOCALTIME )
{
   LPSYSTEMTIME lpSystemTime ;

   // Your code goes here

   GetLocalTime( lpSystemTime ) ;
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI SetLocalTime( IN CONST SYSTEMTIME *lpSystemTime );

/*

HB_FUNC( SETLOCALTIME )
{
   SYSTEMTIME CONST lpSystemTime ;

   // Your code goes here

   hb_retl( SetLocalTime( &lpSystemTime ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI GetSystemInfo( OUT LPSYSTEM_INFO lpSystemInfo );

/*

HB_FUNC( GETSYSTEMINFO )
{
   LPSYSTEM_INFO lpSystemInfo ;

   // Your code goes here

   GetSystemInfo( lpSystemInfo ) ;
}

*/


//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetTickCount( VOID );


HB_FUNC( GETTICKCOUNT )
{
   hb_retnl( (LONG) GetTickCount(  ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetLogicalDriveStringsA( IN DWORD nBufferLength, OUT LPSTR lpBuffer );


HB_FUNC( GETLOGICALDRIVESTRINGS )
{
   hb_retnl( (LONG) GetLogicalDriveStrings( (DWORD) hb_parnl( 1 ),
                                             (LPSTR) hb_parc( 2 ) 
                                             ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GetComputerNameA ( OUT LPSTR lpBuffer, IN OUT LPDWORD nSize );



HB_FUNC( GETCOMPUTERNAME )
{
   char cText[MAX_COMPUTERNAME_LENGTH+1]  ;
   DWORD nSize = MAX_COMPUTERNAME_LENGTH+1;

   hb_retl( GetComputerNameA( (LPSTR) &cText, &nSize ) ) ;

   hb_storc( cText, 1 ) ;
   hb_stornl( nSize, 2 ) ;

}



//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI SetComputerNameA ( IN LPCSTR lpComputerName );


HB_FUNC( SETCOMPUTERNAME )
{
   hb_retl( SetComputerNameA( (LPCSTR) hb_parc( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GetComputerNameExA ( IN COMPUTER_NAME_FORMAT NameType, OUT LPSTR lpBuffer, IN OUT LPDWORD nSize );

/*

HB_FUNC( GETCOMPUTERNAMEEX )
{
   COMPUTER_NAME_FORMAT NameType ;
   LPDWORD              nSize    ;

   // Your code goes here

   hb_retl( GetComputerNameExA( NameType, (LPSTR) hb_parc( 2 ), nSize ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI SetComputerNameExA ( IN COMPUTER_NAME_FORMAT NameType, IN LPCSTR lpBuffer );

/*

HB_FUNC( SETCOMPUTERNAMEEX )
{
   COMPUTER_NAME_FORMAT NameType ;

   // Your code goes here

   hb_retl( SetComputerNameExA( NameType, (LPCSTR) hb_parc( 2 ) ) ) ;
}

*/

 //-----------------------------------------------------------------------------
// WINADVAPI BOOL WINAPI GetUserNameA ( OUT LPSTR lpBuffer, IN OUT LPDWORD nSize );



HB_FUNC( GETUSERNAME )
{
   LPDWORD nSize    ;
   char *szUser = hb_parc( 1 );

   hb_retl( GetUserNameA( szUser, nSize ) ) ;
   hb_storc( szUser , 1 ) ;
   hb_stornl( ( LONG ) nSize , 2 ) ;
}




//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GetVersionExA( IN OUT LPOSVERSIONINFOA lpVersionInformation );

/*

HB_FUNC( GETVERSIONEX )
{
   LPOSVERSIONINFOA lpVersionInformation ;

   // Your code goes here

   hb_retl( GetVersionExA( lpVersionInformation ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI VerifyVersionInfoA( IN LPOSVERSIONINFOEXA lpVersionInformation, IN DWORD dwTypeMask, IN DWORDLONG dwlConditionMask );

/*

HB_FUNC( VERIFYVERSIONINFO )
{
   LPOSVERSIONINFOEXA lpVersionInformation ;

   // Your code goes here

   hb_retl( VerifyVersionInfoA( lpVersionInformation     ,
                                (DWORD) hb_parnl( 2 )    ,
                                (DWORDLONG) hb_parnl( 3 )
                                ) ) ;
}

*/


//--------------------------------------------

// The next 3 functions should go to _WinMDI.c

//-----------------------------------------------------------------------------
// WINUSERAPI UINT WINAPI ArrangeIconicWindows( IN HWND hWnd);


HB_FUNC( ARRANGEICONICWINDOWS )
{
   hb_retni( ArrangeIconicWindows( (HWND) hb_parnl( 1 ) ) ) ;
}


   //-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI TileWindows( IN HWND hwndParent, IN UINT wHow, IN CONST RECT * lpRect, IN UINT cKids, IN const HWND FAR * lpKids);

// move to MDI

/*

HB_FUNC( TILEWINDOWS )
{
   RECT lpRect     ;

   // Your code goes here

   hb_retni( TileWindows( (HWND) hb_parnl( 1 ),
                          (UINT) hb_parni( 2 ),
                          &lpRect             ,
                          (UINT) hb_parni( 4 ),
                          (HWND) hb_parnl( 5 )
                        ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI CascadeWindows( IN HWND hwndParent, IN UINT wHow, IN CONST RECT * lpRect, IN UINT cKids, IN const HWND FAR * lpKids);

// move to MDI

/*

HB_FUNC( CASCADEWINDOWS )
{
   RECT lpRect     ;

   // Your code goes here

   hb_retni( CascadeWindows( (HWND) hb_parnl( 1 ),
                             (UINT) hb_parni( 2 ),
                             &lpRect             ,
                             (UINT) hb_parni( 4 ),
                             (HWND) hb_parnl( 5 )
                           ) ) ;
}

*/





//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI WinHelpA( IN HWND hWndMain, IN LPCSTR lpszHelp, IN UINT uCommand, IN ULONG_PTR dwData );

// need to verify 4th parameter !

HB_FUNC( WINHELP )
{
      hb_retl( WinHelp( (HWND) hb_parnl( 1 ) ,
                     (LPCSTR) hb_parc( 2 ),
                     (UINT) hb_parni( 3 ) ,
                     (ULONG) hb_parnl( 4 )              
                   ) ) ;
}

//-----------------------------------------------------------------------------

// HWND HtmlHelp(HWND hwndCaller, LPCSTR pszFile, UINT uCommand, DWORD dwData); 

//
//  HtmlHelp( hWndCaller,        ;  // Handle of caller window, can be GetDeskTopWindow()
//            cFullPathAndTopic  )  // C:\Creative.acp\Help\VVouch.htm::default.htm
//                                  // If topic is not given, default topic will appear 
//
//  HtmlHelp( GetDeskTopWindow(), 'c:\help\vvouch.chm::de_windo.htm' )
//
//  To create a .chm file, you need to work with Microsoft's
//  free HtmlHelp Workshop doanloadable from MSDN
//


/*
HB_FUNC(HTMLHELP)
{

 hb_retnl( (LONG) HtmlHelp( (HWND)   hb_parnl( 1 )  ,
                            (LPCSTR) hb_parc( 2 ) ,
                            (UINT)   ISNIL(3) ? HH_DISPLAY_TOPIC : hb_parni( 3 )  ,
                            (DWORD)  ISNIL(4) ? NULL : hb_parnl( 4 )
                          )
         ) ;     
}

*/

//-----------------------------------------------------------------------------
 /*
 HANDLE CreateFile(
  LPCTSTR lpFileName,          // pointer to name of the file
  DWORD dwDesiredAccess,       // access (read-write) mode
  DWORD dwShareMode,           // share mode
  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
                               // pointer to security attributes
  DWORD dwCreationDisposition,  // how to create
  DWORD dwFlagsAndAttributes,  // file attributes
  HANDLE hTemplateFile         // handle to file with attributes to 
                               // copy
);
*/


HB_FUNC( CREATFILE )
{

 SECURITY_ATTRIBUTES *sa = ( SECURITY_ATTRIBUTES *) hb_param( 4, HB_IT_STRING )->item.asString.value ;

 hb_retnl( (LONG) CreateFile( (LPCTSTR) hb_parc(1),
                       (DWORD)   hb_parnl(2),
                       (DWORD)   hb_parnl(3),
                       (SECURITY_ATTRIBUTES *) sa ,
                       (DWORD) hb_parnl(5),
                       (DWORD) hb_parnl(6),
                       (HANDLE) hb_parnl(7) ) ) ;
}


//-----------------------------------------------------------------------------

/*
BOOL CloseHandle(
  HANDLE hObject   // handle to object to close
);
*/

HB_FUNC( CLOSEHANDLE )
{
  hb_retl( CloseHandle( (HANDLE) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------

/*
 BOOL ReadFile(
  HANDLE hFile,                // handle of file to read
  LPVOID lpBuffer,             // pointer to buffer that receives data
  DWORD nNumberOfBytesToRead,  // number of bytes to read
  LPDWORD lpNumberOfBytesRead, // pointer to number of bytes read
  LPOVERLAPPED lpOverlapped    // pointer to structure for data
);
 
*/

//-----------------------------------------------------------------------------

/*

BOOL WriteFile(
  HANDLE hFile,                    // handle to file to write to
  LPCVOID lpBuffer,                // pointer to data to write to file
  DWORD nNumberOfBytesToWrite,     // number of bytes to write
  LPDWORD lpNumberOfBytesWritten,  // pointer to number of bytes written
  LPOVERLAPPED lpOverlapped        // pointer to structure for overlapped I/O
);
 

*/



 


