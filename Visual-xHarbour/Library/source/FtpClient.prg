#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

#ifdef VXH_PROFESSIONAL

#include "vxh.ch"
#include "debug.ch"
#include "wininet.ch"

#define INTERNET_INVALID_STATUS_CALLBACK -1

static __oFtp
static aEvents

CLASS FtpClient INHERIT Component
   DATA EnumOperation EXPORTED INIT { { "Synchronous", "Asynchronous" }, { 0, INTERNET_FLAG_ASYNC } }
   DATA Operation     PUBLISHED INIT 0
 
   DATA EnumService   EXPORTED INIT { { "FTP", "HTTP" }, { INTERNET_SERVICE_FTP, INTERNET_SERVICE_HTTP } }
   DATA Service       PUBLISHED INIT INTERNET_SERVICE_FTP

   DATA EnumOpenType  EXPORTED INIT { { "Direct", "Pre-Config", "Proxy" }, { INTERNET_OPEN_TYPE_DIRECT, INTERNET_OPEN_TYPE_PRECONFIG, INTERNET_OPEN_TYPE_PROXY } }
   DATA OpenType      PUBLISHED INIT INTERNET_OPEN_TYPE_DIRECT


   DATA Context, InternetStatus, StatusInformation
   DATA hHandle   EXPORTED INIT 0
   DATA hFtp      EXPORTED
   DATA Server    PUBLISHED
   DATA UserName  PUBLISHED
   DATA Password  PUBLISHED
   DATA Port      PUBLISHED INIT INTERNET_DEFAULT_FTP_PORT
   DATA Passive   PUBLISHED INIT .F.
   
   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD Connect()
   METHOD DisConnect()
   METHOD GetDirectory()
   METHOD GetCurrentDirectory()
   METHOD PutFile( cLocalFile, cRemoteFile )    INLINE FtpPutFile( ::hFtp, cLocalFile, cRemoteFile )
   METHOD GetFile( cRemoteFile, cLocalFile )    INLINE FtpGetFile( ::hFtp, cRemoteFile, cLocalFile )
   METHOD DeleteFile( cRemoteFile )             INLINE FtpDeleteFile( ::hFtp, cRemoteFile )
   METHOD RenameFile( cExistingFile, cNewFile ) INLINE FtpRenameFile( ::hFtp, cExistingFile, cNewFile )
   METHOD CreateDirectory( cDirectory )         INLINE FtpCreateDirectory( ::hFtp, cDirectory )
   METHOD RemoveDirectory( cDirectory )         INLINE FtpRemoveDirectory( ::hFtp, cDirectory )
   METHOD SetCurrentDirectory( cDirectory )     INLINE FtpSetCurrentDirectory( ::hFtp, cDirectory )
   //METHOD Read()
   METHOD GetLastResponseInfo()
ENDCLASS

METHOD Init( oOwner ) CLASS FtpClient
   LOCAL n
   ::__xCtrlName   := "FtpClient"
   ::ComponentType := "FtpClient"
   ::Super:Init( oOwner )

   DEFAULT aEvents TO {;
                        { INTERNET_STATUS_RESOLVING_NAME,       "ResolvingName" },;
                        { INTERNET_STATUS_NAME_RESOLVED,        "NameResolved" },;
                        { INTERNET_STATUS_CONNECTING_TO_SERVER, "ConnectingToServer" },;
                        { INTERNET_STATUS_CONNECTED_TO_SERVER,  "Connected" },;
                        { INTERNET_STATUS_SENDING_REQUEST,      "SendingRequest" },;
                        { INTERNET_STATUS_REQUEST_SENT,         "RequestSent" },;
                        { INTERNET_STATUS_RECEIVING_RESPONSE,   "ReceivingResponse" },;
                        { INTERNET_STATUS_RESPONSE_RECEIVED,    "ResponseReceived" },;
                        { INTERNET_STATUS_REDIRECT,             "Redirect" },;
                        { INTERNET_STATUS_CLOSING_CONNECTION,   "ClosingConnection" },;
                        { INTERNET_STATUS_CONNECTION_CLOSED,    "ConnectionClosed" },;
                        { INTERNET_STATUS_HANDLE_CREATED,       "HandleCreated" },;
                        { INTERNET_STATUS_HANDLE_CLOSING,       "HandleClosing" },;
                        { INTERNET_STATUS_REQUEST_COMPLETE,     "RequestComplete" } }

   ::Events := { {"Status", {} } }
   FOR n := 1 TO LEN( aEvents )
       AADD( ::Events[1][2], { aEvents[n][2], "", "" } )
   NEXT
   
RETURN self

//-------------------------------------------------------------------------------------------------------
METHOD Create() CLASS FtpClient
   IF ::__ClassInst == NIL
      ::hHandle := InternetOpen( ::Application:Name, ::OpenType, NIL, NIL, ::Operation )
      __oFtp := Self
      InternetSetStatusCallback( ::hHandle, ( @FtpStatusCallback() ) )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
FUNCTION FtpStatusCallback( hInternet, dwContext, dwInternetStatus, lpvStatusInformation, dwStatusInformationLength )
   LOCAL n
   __oFtp:Context           := dwContext
   __oFtp:InternetStatus    := dwInternetStatus
   __oFtp:StatusInformation := lpvStatusInformation
   
   IF ( n := ASCAN( aEvents, {|a| a[1]==dwInternetStatus} ) ) > 0
      ExecuteEvent( aEvents[n][2], __oFtp )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD Connect() CLASS FtpClient
   IF ::hHandle == 0
      ::Create()
   ENDIF
   IF ::hHandle <> 0 .AND. ( ::hFtp := InternetConnect( ::hHandle, ::Server, IIF( ::Port == 0, INTERNET_DEFAULT_FTP_PORT, ::Port ), ::UserName, ::Password, ::Service, IIF( ::Passive, INTERNET_FLAG_PASSIVE, )  ) ) <> NIL
      RETURN .T.
   ENDIF
RETURN .F.

//METHOD Read( cFile ) CLASS FtpClient
//   LOCAL hFile := FtpOpenFile( ::hHandle, cFile,  InternetReadFile( ::hFtp, cBuffer, cFile )

//-------------------------------------------------------------------------------------------------------

METHOD DisConnect() CLASS FtpClient
   LOCAL lRet := .f.
   IF ::hFtp <> 0 .AND. InternetCloseHandle( ::hFtp )
      lRet := InternetCloseHandle( ::hHandle )
      ::hHandle := 0
   ENDIF
RETURN lRet

//-------------------------------------------------------------------------------------------------------

METHOD GetCurrentDirectory() CLASS FtpClient
   LOCAL cDirectory := space( 260 )
   FtpGetCurrentDirectory( ::hFtp, @cDirectory )
RETURN cDirectory

//-------------------------------------------------------------------------------------------------------

METHOD GetLastResponseInfo() CLASS FtpClient
   LOCAL nError, cRet := ""
   InternetGetLastResponseInfo( @nError, @cRet )
RETURN cRet

//-------------------------------------------------------------------------------------------------------

METHOD GetDirectory( cFileSpec ) CLASS FtpClient
   LOCAL hFind, n, aFile := {}, aDir := {}
   LOCAL pFindData := (struct WIN32_FIND_DATA)
   LOCAL pSysTime  := (struct SYSTEMTIME)
   LOCAL cBuffer
   DEFAULT cFileSpec TO "*.*"

   IF ( hFind := _FtpFindFirstFile( ::hFtp, cFileSpec, @cBuffer, INTERNET_FLAG_NEED_FILE ) ) != 0
      pFindData:Buffer( cBuffer )

      aFile:= array( 5 )
      aFile[1] := pFindData:cFileName:AsString()
      aFile[2] := pFindData:dwFileAttributes
      aFile[3] := pFindData:nFileSizeLow

      IF _FileTimeToSystemTime( pFindData:ftLastWriteTime:Value, @cBuffer )
         pSysTime:Buffer( cBuffer )

         aFile[4] := STOD( STRZERO( pSysTime:wYear, 4 ) + STRZERO( pSysTime:wMOnth, 2 ) + STRZERO( pSysTime:wDay, 2 ) )
         aFile[5] := STRZERO( pSysTime:wHour, 2 ) + ":" + STRZERO( pSysTime:wMinute, 2 ) + ":" + STRZERO( pSysTime:wSecond, 2 )
      ENDIF

      AADD( aDir, aFile )

      DO WHILE _InternetFindNextFile( hFind, @cBuffer )
         pFindData:Buffer( cBuffer )

         aFile:= array( 5 )
         aFile[1] := pFindData:cFileName:AsString()
         aFile[2] := pFindData:dwFileAttributes
         aFile[3] := pFindData:nFileSizeLow

         IF _FileTimeToSystemTime( pFindData:ftLastWriteTime:Value, @cBuffer )
            pSysTime:Buffer( cBuffer )

            aFile[4] := STOD( STRZERO( pSysTime:wYear, 4 ) + STRZERO( pSysTime:wMOnth, 2 ) + STRZERO( pSysTime:wDay, 2 ) )
            aFile[5] := STRZERO( pSysTime:wHour, 2 ) + ":" + STRZERO( pSysTime:wMinute, 2 ) + ":" + STRZERO( pSysTime:wSecond, 2 )
         ENDIF

         AADD( aDir, aFile )

      ENDDO

      InternetCloseHandle( hFind )
   ENDIF

RETURN aDir

#endif