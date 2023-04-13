/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    CGI Session Manager Class
 *
 * Copyright 2003-2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
 * www - http://www.xharbour.org
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
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "cgidefs.ch"
#include "common.ch"
#include "hbclass.ch"
#include "fileio.ch"
#include "directry.ch"

// -------------------------------- //
// Defines

#define SID_LENGTH      25
#define BASE_KEY_STRING "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
#define CRC_KEY_STRING  "Ak3yStR1Ng"  // Max Length must be 10 chars

// -------------------------------- //

GLOBAL EXTERNAL oCGI

CLASS TSession

   METHOD New()

   METHOD Start()
   METHOD CacheExpire( nMinutes ) INLINE SetNewValueReturnOld( ::nCache_ExpireMin, nMinutes )
   METHOD CacheType()
   METHOD GetCookieParams()       INLINE { ::nCookie_LifeTime, ::cCookie_Path, ::cCookie_Domain, ::lCookie_Secure  }
   METHOD SetCookieParams()
   METHOD ID( cID )               INLINE SetNewValueReturnOld( ::cSID, cID )
   METHOD Name( cName )           INLINE SetNewValueReturnOld( ::cName, cName )
   METHOD RegenerateID()
   METHOD SavePath( cPath )       INLINE SetNewValueReturnOld( ::cSavePath, cPath )
   METHOD IsStarted()             INLINE ( ::nActiveSessions > 0 )
   METHOD UseOnlyCookies()        INLINE ::lUse_Only_Cookies

   METHOD SaveCookie()
   METHOD GetVar( cVar )          INLINE HGetValue( ::hSession, cVar )
   METHOD SetVar( cVar, xValue )  INLINE ::hSession[ cVar ] := xValue

   METHOD SetSessionHandler()
   METHOD Open( cPath, cName )    INLINE Eval( ::bOpen, cPath, cName  )
   METHOD Close()
   METHOD Read( cID )             INLINE Eval( ::bRead, cID  )
   METHOD Write( cID, cData )     INLINE Eval( ::bWrite, cID, cData )
   METHOD Destroy( cID )
   METHOD GC( nSID_LifeTime )     INLINE Eval( ::bGC, nSID_LifeTime )

   //METHOD SessionContainer( hHash )  INLINE SetNewValueReturnOld( ::hSession, hHash )
   METHOD Encode()
   METHOD Decode()

 HIDDEN:

   DATA cSID
   DATA cSavePath               INIT Configure( "DIR_FS_TEMP" )
   DATA cName
   DATA nGc_Percent             INIT 33        // Every 1/3 of checks i'll lunch Session GC
   DATA nGc_SID_LifeTime        INIT 1440      // seconds - Number of seconds after gc can delete a session
   DATA nCookie_LifeTime        INIT 3600      // Number of seconds to keep cookie, 0 = until browser is closed
   DATA cCookie_Path            INIT "/"
   DATA cCookie_Domain
   DATA lCookie_Secure          INIT FALSE
   DATA lUse_Cookies            INIT TRUE      // TRUE = Use cookies to store session id on client side
   DATA lUse_Only_Cookies       INIT FALSE
   DATA lReferer_Check          INIT FALSE     // TRUE = check if referer is same of this host name
   DATA cCache_Type             INIT "nocache" // Possible values are: none, nocache, private, private_no_expire, public
   DATA nCache_ExpireMin        INIT 180       // in minutes, not used if cCache_Type == none or nocache
   //DATA lUse_Trans_SID          INIT FALSE     // FALSE = no SID appended to URL

   // Session Storage code blocks
   DATA bOpen
   DATA bClose
   DATA bRead
   DATA bWrite
   DATA bDestroy
   DATA bGC

   DATA hSession                // INIT oCGI:h_Session
   DATA nActiveSessions         INIT 0

   DATA cCRCKeyString           INIT CRC_KEY_STRING

   METHOD GenerateSID()
   METHOD CheckSID()
   METHOD SessionOpen()
   METHOD SessionClose()
   METHOD SessionRead()
   METHOD SessionWrite()
   METHOD SessionDestroy()
   METHOD SessionGC()

   METHOD SendCacheHeader()

ENDCLASS

// ------------------------------ ***************************** -----------------------------------

METHOD New( hSession, cSessionName ) CLASS TSession

  DEFAULT hSession     TO oCGI:h_Session
  DEFAULT cSessionName TO "SESSION"

  ::hSession  := hSession
  ::cName     := cSessionName + "ID"

  // Code blocks for session file / database handling - default using Session*() methods
  // Use ::SetSessionHandler() to change them
  ::bOpen     := {|cPath, cName|  ::SessionOpen( cPath, cName ) }
  ::bClose    := {||              ::SessionClose() }
  ::bRead     := {|cID|           ::SessionRead( cID ) }
  ::bWrite    := {|cID, cData|    ::SessionWrite( cID, cData ) }
  ::bDestroy  := {|cID|           ::SessionDestroy( cID ) }
  ::bGC       := {|nSID_LifeTime| ::SessionGC( nSID_LifeTime ) }

RETURN Self

METHOD Start() CLASS TSession
  LOCAL lSendCookie := TRUE
  LOCAL lDefine_SID := TRUE
  LOCAL cSID, xVal, nRand, hUrl

  //TraceLog( "Active Sessions : " + cStr( ::nActiveSessions ) )

  // If I have already sessions stops here
  IF ::nActiveSessions <> 0
     RETURN FALSE
  ENDIF

  // Start checking ID from global vars
  IF ( ::cName IN oCGI:h_Request:Keys )
     // Take SID name from GLOBAL session vars
     ::cSID := oCGI:h_Request[ ::cName ]
     // I don't need to create a cookie o a SID
     lSendCookie := FALSE
     lDefine_SID := FALSE
  ENDIF

  // if I have already a SID I have to check it
  IF !Empty( ::cSID ) .AND. !::CheckSID()
     // if SID is NOT valid, someone altered it
     ::cSID      := NIL   // invalidate current SID, i'll generate a new one
     lSendCookie := TRUE  // force to send a cookie
     lDefine_SID := TRUE  // a create a new SID
   ENDIF

  IF !Empty( ::cSID ) .AND. ::lReferer_Check
     // check if referer is from current host name

     hUrl := SplitUrl( oCGI:h_Server[ 'HTTP_REFERER' ] )
     IF Upper( RTrim( hUrl:HOST ) ) == Upper( oCGI:h_Server[ 'SERVER_NAME' ] )
        ::cSID      := NIL   // invalidate current SID, i'll generate a new one
        lSendCookie := TRUE  // force to send a cookie
        lDefine_SID := TRUE  // a create a new SID
     ENDIF
  ENDIF

  //If I have not an existing SID ...
  IF Empty( ::cSID )
     // Create a new one
     ::cSID := ::GenerateSID()
  ENDIF

  // if I have set to not use cookies
  IF !::lUse_Cookies .AND. lSendCookie
     lDefine_SID := TRUE
     lSendCookie := FALSE
  ENDIF

  // if I have to send a cookie
  IF lSendCookie
     oCGI:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, DateToGMT(,,,::nCookie_LifeTime), ::lCookie_Secure )
  ENDIF

  // if I have to define the SID
  IF lDefine_SID
     cSID := ::cName + '=' + ::cSID

     // save in REQUEST global hash
     oCGI:h_Request[ ::cName ] := ::cSID
  ENDIF

  // add active session
  ::nActiveSessions++

  // Send caching headers

  // Start session
  IF !::Open( ::cSavePath, ::cName )
     Die( 'ERROR: Failed to open session file' )
  ENDIF

  // Read session data
  IF !( ( xVal := ::Read( ::cSID  ) ) == NIL )
     //TraceLog( "Read session data - xVal", xVal )
     // Decode session data
     ::Decode( xVal )
  ENDIF

  // Send HTTP cache headers
  ::SendCacheHeader()

  // Is it time to clean ?
  IF ::nGc_Percent > 0

     nRand := HB_RandomInt( 1, 100 )

     //TraceLog( "::nGc_Percent - nRand = " + cStr( nRand ) )
     IF nRand <= ::nGc_Percent
        ::GC( ::nGc_SID_LifeTime )
     ENDIF
  ENDIF

RETURN TRUE

METHOD Destroy() CLASS TSession

    IF ::nActiveSessions == 0
       RETURN FALSE
    ENDIF

    // Destroy session
    IF !Eval( ::bDestroy, ::cSID )
       RETURN FALSE
    ENDIF

RETURN TRUE

METHOD Close() CLASS TSession
  LOCAL cVal

    //TraceLog( "Session Close() - oCGI:h_Session", DumpValue( oCGI:h_Session ) )

    IF ::nActiveSessions == 0
      RETURN FALSE
    ENDIF

    // Encode session
    cVal := ::Encode()

    // Save session vars to file / database
    IF !::Write( ::cSID, cVal )
       Die( 'Cannot save session SID' )
    ENDIF

    // Close session
    IF !Eval( ::bClose )
      Die('Cannot close session')
    ENDIF

    ::nActiveSessions--

RETURN TRUE

METHOD CacheType( cNewType ) CLASS TSession
  LOCAL cOldType := ::cCache_Type

  IF cNewType <> NIL
     IF cNewType $ "none/nocache/private/private_no_expire/public"
        ::cCache_Type := cNewType
     ELSE
        Die( "ERROR: TSession:CacheType() - New Type is incorrect" )
     ENDIF
  ENDIF
RETURN cOldType

METHOD SetCookieParams( nLifeTime, cPath, cDomain, lSecure  ) CLASS TSession
  IF nLifeTime <> NIL THEN ::nCookie_LifeTime := nLifeTime
  IF cPath     <> NIL THEN ::cCookie_Path     := cPath
  IF cDomain   <> NIL THEN ::cCookie_Domain   := cDomain
  IF lSecure   <> NIL THEN ::lCookie_Secure   := lSecure
RETURN NIL

METHOD RegenerateID() CLASS TSession
  LOCAL lSuccess := TRUE

  ::cSID := ::GenerateSID()

  IF ::lUse_Cookies
     oCGI:SetCookie( ::cName, ::cSID, ::cCookie_Domain, ::cCookie_Path, DateToGMT(,,,::nCookie_LifeTime), ::lCookie_Secure )
  ENDIF
RETURN ::cSID

METHOD SaveCookie() CLASS TSession
  LOCAL cExpires := DateToGMT( Date(), Time(),, ::nCookie_LifeTime )
  LOCAL hSession := ::hSession
  LOCAL cKey

  FOR EACH cKey IN hSession:Keys
      oCGI:SetCookie( ::cName + "_" + cKey, hSession[ cKey ], ::cCookie_Domain, ::cCookie_Path, cExpires, ::lCookie_Secure )
  NEXT
RETURN NIL

/*
 * SID = 25 random chars + 5 CRC chars
 */

METHOD GenerateSID( cCRCKey ) CLASS TSession
   LOCAL cSID, nSIDCRC, cSIDCRC, n, cTemp
   LOCAL nLenSID     := SID_LENGTH
   LOCAL cBaseKeys   := BASE_KEY_STRING
   LOCAL nLenKeys    := Len( cBaseKeys )
   LOCAL cRet
   LOCAL nRand, nKey := 0
   //LOCAL a := 0

   DEFAULT cCRCKey  TO ::cCRCKeyString

   cCRCKey := Left( cCRCKey, 10 )      // Max Lenght must to be of 10 chars

   /* Let's generate the sequence */
   cSID := Space( nLenSID )
   FOR n := 1 TO nLenSID
      nRand     := HB_RandomInt( 1, nLenKeys )
      cSID[ n ] := cBaseKeys[ nRand ]
      nKey      += nRand
   NEXT

   nSIDCRC := nKey * 51 // Max Value is 99603 a 5 chars number
   cTemp   := StrZero( nSIDCRC, 5 )
   cSIDCRC := ""
   FOR n := 1 TO Len( cTemp )
       cSIDCRC += cCRCKey[ Val( cTemp[ n ] ) + 1 ]
   NEXT

   cRet := cSID + cSIDCRC

   //TraceLog( "Generate SID: cRet, cSID, nSIDCRC, cTemp, cSIDCRC, nKey, a", cRet, cSID, nSIDCRC, cTemp, cSIDCRC, nKey, a )

RETURN cRet

METHOD CheckSID( cSID, cCRCKey ) CLASS TSession
   LOCAL nSIDCRC, cSIDCRC, n, cTemp
   LOCAL nLenSID     := SID_LENGTH
   LOCAL cBaseKeys   := BASE_KEY_STRING
   LOCAL nLenKeys    := Len( cBaseKeys )
   LOCAL nRand, nKey := 0
   //LOCAL a := 0

   DEFAULT cSID    TO ::cSID
   DEFAULT cCRCKey TO ::cCRCKeyString

   cCRCKey := Left( cCRCKey, 10 )      // Max Lenght must to be of 10 chars

   /* Calculate the key */
   FOR n := 1 TO nLenSID
      nRand := At( cSID[ n ], cBaseKeys )
      nKey  += nRand
   NEXT

   // Recalculate the CRC
   nSIDCRC := nKey * 51 // Max Value is 99603. a 5 chars number
   cTemp   := StrZero( nSIDCRC, 5 )
   cSIDCRC := ""
   FOR n := 1 TO Len( cTemp )
       cSIDCRC += cCRCKey[ Val( cTemp[ n ] ) + 1 ]
   NEXT

   //TraceLog( "Check SID: cRet, cSID, nSIDCRC, cTemp, cSIDCRC, nKey, a", cRet, cSID, nSIDCRC, cTemp, cSIDCRC, nKey, a )

RETURN ( Right( cSID, 5 ) == cSIDCRC )

// -------------------------------*************************-----------------------------------------

// Change Session Handler
METHOD SetSessionHandler( bOpen, bClose, bRead, bWrite, bDestroy, bGC ) CLASS TSession
   IF bOpen    <> NIL THEN ::bOpen    := bOpen
   IF bClose   <> NIL THEN ::bClose   := bClose
   IF bRead    <> NIL THEN ::bRead    := bRead
   IF bWrite   <> NIL THEN ::bWrite   := bWrite
   IF bDestroy <> NIL THEN ::bDestroy := bDestroy
   IF bGC      <> NIL THEN ::bGC      := bGC
RETURN NIL

// Internal file session management

METHOD SessionOpen( cPath, cName ) CLASS TSession
  //TraceLog( "SessionOpen() - cName", cName )
  IF cPath <> NIL THEN ::cSavePath := cPath
  IF cName <> NIL THEN ::cName     := cName

RETURN TRUE

METHOD SessionClose() CLASS TSession
  //TraceLog( "SessionClose()" )
  // Nothing to do
RETURN TRUE

METHOD SessionRead( cID ) CLASS TSession
  LOCAL nH
  LOCAL cFile
  LOCAL nFileSize
  LOCAL cBuffer

  DEFAULT cID TO ::cSID

  cFile := ::cSavePath + "/" + ::cName + "_" + cID

  //TraceLog( "SessionRead: cFile", cFile )
  IF File( cFile )
     IF ( nH := FOpen( cFile, FO_READ ) ) <> -1
        nFileSize := FSeek( nH, 0, FS_END )
        FSeek( nH, 0, FS_SET )
        cBuffer := Space( nFileSize )
        IF ( FRead( nH, @cBuffer,  nFileSize ) ) <> nFileSize
           Die( "ERROR: On reading session file : " + cFile + ", File error : " + cStr( FError() ) )
        ELSE
           FClose( nH )
        ENDIF
     ELSE
        Die( "ERROR: On opening session file : " + cFile + ", file not exist." )
     ENDIF
  ENDIF
  //TraceLog( "SessionRead() - cID, cFile, nFileSize, cBuffer", cID, cFile, nFileSize, cBuffer )
RETURN cBuffer

METHOD SessionWrite( cID, cData ) CLASS TSession
  LOCAL nH
  LOCAL cFile
  LOCAL nFileSize := Len( cData )
  LOCAL lOk := FALSE
  //TraceLog( "SessionWrite() - cID, cData", cID, cData )
  DEFAULT cID TO ::cSID
  cFile := ::cSavePath + "/" + ::cName + "_" + cID
  //TraceLog( "SessionWrite() - cFile", cFile )
  IF ( nH := FCreate( cFile, FC_NORMAL ) ) <> -1
     IF ( FWrite( nH, @cData,  nFileSize ) ) <> nFileSize
        Die( "ERROR: On writing session file : " + cFile + ", File error : " + cStr( FError() ) )
     ELSE
        lOk := TRUE
        FClose( nH )
     ENDIF
  ELSE
     Die( "ERROR: On WRITING session file. I can not create session file : " + cFile + ", File error : " + cStr( FError() ) )
  ENDIF
RETURN lOk

METHOD SessionDestroy( cID ) CLASS TSession
  LOCAL cFile
  LOCAL lOk := FALSE
  //TraceLog( "SessionDestroy() - cID", cID )
  DEFAULT cID TO ::cSID

  oCGI:h_Session := Hash()
  oCGI:DeleteCookie( ::cName )

  cFile := ::cSavePath + "/" + ::cName + "_" + cID
  IF !( lOk := ( FErase( cFile ) == 0 ) )
     Die( "ERROR: On deleting session file : " + cFile + ", File error : " + cStr( FError() ) )
  ELSE
     ::RegenerateID()
  ENDIF
RETURN lOk

METHOD SessionGC( nSID_LifeTime ) CLASS TSession
  LOCAL nSecs
  LOCAL aDir, aFile

  DEFAULT nSID_LifeTime TO ::nGc_SID_LifeTime
  aDir := Directory( ::cSavePath + "/" + ::cName + "_*.*" )

  FOR EACH aFile IN aDir
      nSecs := TimeDiffAsSeconds( aFile[ F_DATE ], Date(), aFile[ F_TIME ], Time() )
      IF nSecs > nSID_LifeTime
         FErase( ::cSavePath + "/" + aFile[ F_NAME ] )
      ENDIF
  NEXT

RETURN TRUE

METHOD Encode() CLASS TSession
  LOCAL aSerial := {}
  LOCAL cKey, xVal

  FOR EACH cKey IN ::hSession:Keys
      xVal := ::hSession[ cKey ]
      IF xVal <> NIL THEN aAdd( aSerial, { cKey, xVal } )
  NEXT

RETURN HB_Serialize( aSerial )

METHOD Decode( cData ) CLASS TSession
  LOCAL lOk := TRUE
  LOCAL cSerial := HB_DeserialBegin( cData )
  // LOCAL xVal, cKey, aElem
  LOCAL xVal, aElem

  DO WHILE ( xVal := HB_DeserialNext( cSerial ) ) <> NIL

     SWITCH ValType( xVal )
        CASE 'A'  // Vars are stored in array { VarName, Value }
           FOR EACH aElem IN xVal
               ::hSession[ aElem[1] ] := aElem[2]
           NEXT
           EXIT

        DEFAULT
           Die( "ERROR: On deserializing session data" )
           lOk := FALSE
           EXIT
     END
  ENDDO

RETURN lOk

METHOD SendCacheHeader() CLASS TSession
  DO CASE
     CASE ::cCache_Type == 'nocache'
          //oCGI:Header( 'Expires: Thu, 19 Nov 1981 08:52:00 GMT' )
          oCGI:Header('Expires: ' + DateToGMT( ,,-1, ) )
          oCGI:Header( 'Cache-Control: no-cache' )
          //oCGI:Header("Cache-Control: no-store, no-cache, must-revalidate")  // HTTP/1.1
          //oCGI:Header("Cache-Control: post-check=0, pre-check=0", FALSE )
          oCGI:Header( 'Pragma: no-cache' )

     CASE ::cCache_Type == 'private'
          oCGI:Header( 'Expires: Thu, 19 Nov 1981 08:52:00 GMT' )
          oCGI:Header( 'Cache-Control: private, max-age=' + LTrim( Str( ::nCache_ExpireMin * 60 ) ) )
          oCGI:Header('Last-Modified: ' + DateToGMT( FileDate( hb_argv(0) ) ) )

     CASE ::cCache_Type == 'public'
          oCGI:Header('Expires: ' + DateToGMT( ,,, ::nCache_ExpireMin * 60 ) )
          oCGI:Header( 'Cache-Control: public, max-age=' + LTrim( Str( ::nCache_ExpireMin * 60 ) ) )
          oCGI:Header('Last-Modified: ' + DateToGMT( FileDate( hb_argv(0) ) ) )

     OTHERWISE
          Die( "ERROR: Caching method " + ::cCache_Type + " not implemented." )

  ENDCASE
  //__OutDebug( "Header cache '" + ::cCache_Type + "' inviato" )
RETURN NIL
