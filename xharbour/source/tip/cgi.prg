/*
 * $Id$
 */
/*
 * xHarbour Project source code:
 * TipCgi Class oriented cgi protocol
 *
 * Copyright 2006 Lorenzo Fiorini <lorenzo_fiorini@teamwork.it>
 *
 * code from:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
 * www - http://www.harbour-project.org
 *
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
#include 'hbclass.ch'
#include 'tip.ch'
#include 'common.ch'
#include 'fileio.ch'
#define CGI_IN  0
#define CGI_OUT 1
#define _CRLF chr(13)+chr(10)
#define _BR '<br />'
#define SID_LENGTH      25
#define BASE_KEY_STRING "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
#define CRC_KEY_STRING  "Ak3yStR1Ng"  // Max Length must be 10 chars

CLASS TIpCgi

   DATA HTTP_RAW_POST_DATA
   DATA cCgiHeader
   DATA cHtmlPage
   DATA hGets        INIT { => }
   DATA hPosts       INIT { => }
   DATA hCookies     INIT { => }
   DATA hSession     INIT { => }
   DATA bSavedErrHandler
   DATA cSessionSavePath
   DATA cSID
   METHOD New()
   METHOD Header( hOptions )
   METHOD Redirect( cUrl )
   METHOD PRINT( cString )
   METHOD Flush()
   METHOD ErrHandler()
   METHOD StartHtml( hOptions )
   METHOD EndHtml()
   METHOD StartFrameSet( hOptions )
   METHOD EndFrameSet()
   METHOD SaveHtmlPage( cFile )
   METHOD StartSession()
   METHOD DestroySession()
   METHOD CreateSID( cCRCKey ) INLINE ::cSID := GenerateSID( cCrcKey )
   METHOD CheckCrcSID( cSID, cCRCKey ) INLINE CheckSID( cSID, cCRCKey )
   METHOD SessionEncode()
   METHOD SessionDecode( cData )

ENDCLASS

METHOD New() CLASS TIpCgi

   LOCAL aTemp
   LOCAL aVar
   LOCAL lPost
   LOCAL nCount
   LOCAL nLen
   LOCAL nRead
   LOCAL cTemp

   ::bSavedErrHandler := ErrorBlock( { |e| ::ErrHandler( e ) } )
   ::cCgiHeader := ''
   ::cHtmlPage := ''
   lPost := ( 'POST' $ Upper( GetEnv( 'REQUEST_METHOD' ) ) )
   IF lPost
      nLen := Val( GetEnv( 'CONTENT_LENGTH' ) )
      cTemp := Space( nLen )
      IF ( ( nRead := FRead( CGI_IN, @cTemp, nLen, 0 ) ) != nLen )
         ::ErrHandler( 'post error read ' + Str( nRead ) + ' instead of ' + Str( nLen ) )
      ELSE
         ::HTTP_RAW_POST_DATA := cTemp
         aTemp := hb_ATokens( cTemp, '&' )
         nLen := Len( aTemp )
         IF nLen > 0
            FOR nCount := 1 TO nLen
               aVar := hb_ATokens( aTemp[ nCount ], '=' )
               IF Len( aVar ) == 2
                  ::hPosts[ alltrim( TipEncoderUrl_Decode( aVar[ 1 ] ) ) ] := TipEncoderUrl_Decode( aVar[ 2 ] )
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ELSE
      cTemp := GetEnv( 'QUERY_STRING' )
      IF !Empty( cTemp )
         aTemp := hb_ATokens( cTemp, '&' )
         nLen := Len( aTemp )
         IF nLen > 0
            FOR nCount := 1 TO nLen
               aVar := hb_ATokens( aTemp[ nCount ], '=' )
               IF Len( aVar ) == 2
                  ::hGets[ alltrim( TipEncoderUrl_Decode( aVar[ 1 ] ) ) ] := TipEncoderUrl_Decode( aVar[ 2 ] )
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF
   cTemp := GetEnv( 'HTTP_COOKIE' )
   IF !Empty( cTemp )
      aTemp := hb_ATokens( cTemp, ';' )
      nLen := Len( aTemp )
      IF nLen > 0
         FOR nCount := 1 TO nLen
            aVar := hb_ATokens( aTemp[ nCount ], '=' )
            IF Len( aVar ) == 2
               ::hCookies[ alltrim( TipEncoderUrl_Decode( aVar[ 1 ] ) ) ] := TipEncoderUrl_Decode( aVar[ 2 ] )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN Self

METHOD Header( cValue ) CLASS TIpCgi

   IF Empty( cValue )
      ::cCgiHeader += 'Content-Type: text/html' + _CRLF
   ELSE
      ::cCgiHeader += cValue + _CRLF
   ENDIF

   RETURN Self

METHOD Redirect( cUrl ) CLASS TIpCgi

   ::cCgiHeader += 'Location: ' + cUrl + _CRLF

   RETURN Self

METHOD PRINT( cString ) CLASS TIpCgi

   ::cHtmlPage += cString + _CRLF

   RETURN Self

METHOD Flush() CLASS TIpCgi

   LOCAL nLen
   LOCAL cStream
   LOCAL lRet
   LOCAL nH
   LOCAL cFile
   LOCAL nFileSize
   LOCAL cSID := ::cSID
   LOCAL cSession

   hEval( ::hCookies, { |k, v| ::cCgiHeader += 'Set-Cookie: ' + k + '=' + v + ';' + _CRLF } )
   cStream := ::cCgiHeader + _CRLF + ::cHtmlPage + _CRLF
   nLen := Len( cStream )
   lRet := ( FWrite( CGI_OUT, cStream, nLen ) == nLen )
   ::cCgiHeader := ''
   ::cHtmlPage := ''
   IF !Empty( cSID )
      cFile := ::cSessionSavePath + "SESSIONID_" + cSID
      cSession := ::SessionEncode()
      nFileSize := Len( cSession )
      IF ( nH := FCreate( cFile, FC_NORMAL ) ) != - 1
         IF ( FWrite( nH, @cSession,  nFileSize ) ) != nFileSize
            ::Print( "ERROR: On writing session file : " + cFile + ", File error : " + cStr( FError() ) )
         ENDIF
         FClose( nH )
      ELSE
         ::Print( "ERROR: On writing session file : " + cFile + ", File error : " + cStr( FError() ) )
      ENDIF
   ENDIF

   RETURN lRet

METHOD DestroySession( cID ) CLASS TIpCgi

   LOCAL cFile
   LOCAL cSID := ::cSID
   LOCAL lRet

   IF !Empty( cID )
      cSID := cID
   ENDIF

   IF !Empty( cSID )
      ::hSession := { => }
      cFile := ::cSessionSavePath + "SESSIONID_" + cSID
      IF !( lRet := ( FErase( cFile ) == 0 ) )
         ::Print( "ERROR: On deleting session file : " + cFile + ", File error : " + cStr( FError() ) )
      ELSE
         ::hCookies[ 'SESSIONID' ] := cSID + "; expires= " + DateToGMT( Date() - 1 )
         ::CreateSID()
         cSID := ::cSID
         ::hCookies[ 'SESSIONID' ] := cSID
      ENDIF
   ENDIF

   RETURN lRet

METHOD ErrHandler( xError ) CLASS TIpCgi

   LOCAL nCalls

   ::Print( '<table border="1">' )
   ::Print( '<tr><td>SCRIPT NAME:</td><td>' + GetEnv( 'SCRIPT_NAME' ) + '</td></tr>' )
   SWITCH ValType( xError )
   CASE "O"
      ::Print( '<tr><td>CRITICAL ERROR:</td><td>' + xError:Description + '</td></tr>' )
      ::Print( '<tr><td>OPERATION:</td><td>' + xError:Operation + '</td></tr>' )
      ::Print( '<tr><td>OS ERROR:</td><td>' + AllTrim( Str( xError:OsCode ) ) + ' IN ' + xError:SubSystem + '/' + AllTrim( Str( xError:SubCode ) ) + '</td></tr>' )
      ::Print( '<tr><td>FILENAME:</td><td>' + Right( xError:FileName, 40 ) + '</td></tr>' )
      EXIT
   CASE "C"
      ::Print( '<tr><td>ERROR MESSAGE:</td><td>' + xError + '</td></tr>' )
      EXIT
   END SWITCH
   FOR nCalls := 2 TO 6
      IF !Empty( ProcName( nCalls ) )
         ::Print( '<tr><td>PROC/LINE:</td><td>' + ProcName( nCalls ) + "/" + AllTrim( Str( ProcLine( nCalls ) ) ) + '</td></tr>' )
      ENDIF
   NEXT
   ::Print( '</table>' )
   ::Flush()

   RETURN nil

METHOD StartHtml( hOptions ) CLASS TIpCgi

   ::cHtmlPage += '<?xml version="1.0"?>' + _CRLF + ;
      '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"' + _CRLF + ;
      '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' + _CRLF + ;
      '<html xmlns="http://www.w3.org/1999/xhtml">' + ;
      '<head>' + ;
      HtmlTag( hOptions, 'title' ) + ;
      HtmlScript( hOptions ) + ;
      HtmlStyle( hOptions ) + ;
      '</head>' + ;
      '<body ' + ;
      HtmlAllOption( hOptions ) + ;
      '>'

   RETURN Self

METHOD EndHtml() CLASS TIpCgi

   ::cHtmlPage += '</body></html>'

   RETURN Self

METHOD StartFrameSet( hOptions ) CLASS TIpCgi

   ::cHtmlPage += '<?xml version="1.0"?>' + _CRLF + ;
      '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"' + _CRLF + ;
      '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' + _CRLF + ;
      '<html xmlns="http://www.w3.org/1999/xhtml">' + ;
      '<head>' + ;
      HtmlTag( hOptions, 'title' ) + ;
      HtmlScript( hOptions ) + ;
      HtmlStyle( hOptions ) + ;
      '</head>' + ;
      '<frameset ' + ;
      HtmlValue( hOptions, 'frame' ) + ;
      '>'

   RETURN Self

METHOD EndFrameSet( hOptions ) CLASS TIpCgi

   ::cHtmlPage += '</frameset><noframes>' + ;
      HtmlValue( hOptions, 'frame' ) + ;
      '</noframes></html>'

   RETURN Self

METHOD SaveHtmlPage( cFile ) CLASS TIpCgi

   LOCAL nFile
   LOCAL lSuccess
   LOCAL nLen
   LOCAL cStream

   cStream := ::cHtmlPage + _CRLF
   nLen := Len( cStream )
   nFile := FCreate( cFile )
   IF nFile != 0
      lSuccess := ( FWrite( nFile, cStream, nLen ) == nLen )
      FClose( nFile )
   ELSE
      lSuccess := .F.
   ENDIF

   RETURN lSuccess

METHOD StartSession() CLASS TIpCgi

   LOCAL cSID
   LOCAL nH
   LOCAL cFile
   LOCAL nFileSize
   LOCAL cBuffer

   IF ( nH := hGetPos( ::hGets, 'SESSIONID' ) ) != 0
      cSID := hGetValueAt( ::hGets, nH )
   ELSEIF ( nH := hGetPos( ::hPosts, 'SESSIONID' ) ) != 0
      cSID := hGetValueAt( ::hPosts, nH )
   ELSEIF ( nH := hGetPos( ::hCookies, 'SESSIONID' ) ) != 0
      cSID := hGetValueAt( ::hCookies, nH )
   ENDIF
   IF Empty( ::cSessionSavePath )
      ::cSessionSavePath := "/tmp/"
   ENDIF
   IF !Empty( cSID )
      ::cSID := cSID
      cFile := ::cSessionSavePath + "SESSIONID_" + cSID
      IF File( cFile )
         IF ( nH := FOpen( cFile, FO_READ ) ) != - 1
            nFileSize := FSeek( nH, 0, FS_END )
            FSeek( nH, 0, FS_SET )
            cBuffer := Space( nFileSize )
            IF ( FRead( nH, @cBuffer,  nFileSize ) ) != nFileSize
               ::ErrHandler( "ERROR: On reading session file : " + cFile + ", File error : " + cStr( FError() ) )
            ELSE
               ::SessionDecode( cBuffer )
            ENDIF
            FClose( nH )
         ENDIF
      ELSE
         ::ErrHandler( "ERROR: On opening session file : " + cFile + ", file not exist." )
      ENDIF
   ELSE
      ::CreateSID()
      ::hSession := { => }
   ENDIF
   ::hCookies[ 'SESSIONID' ] := ::cSID

   RETURN nil

METHOD SessionEncode() CLASS TIpCgi

   LOCAL aSerial := {}
   LOCAL cKey, xVal

   FOR EACH cKey in ::hSession:Keys
      xVal := ::hSession[ cKey ]
      IF xVal != nil
         AAdd( aSerial, { cKey, xVal } )
      ENDIF
   NEXT

   RETURN hb_Serialize( aSerial )

METHOD SessionDecode( cData ) CLASS TIpCgi

   LOCAL lRet := .T.
   LOCAL cSerial := HB_DeserialBegin( cData )
   LOCAL xVal, aElem

   DO WHILE ( xVal := HB_DeserialNext( @cSerial ) ) != nil
      SWITCH ValType( xVal )
      CASE 'A'  // Vars are stored in array { VarName, Value }
         FOR EACH aElem in xVal
            ::hSession[ aElem[1] ] := aElem[2]
         NEXT
         EXIT
         DEFAULT
         lRet := .F.
         EXIT
      END SWITCH
   ENDDO

   RETURN lRet

STATIC FUNCTION HtmlTag( xVal, cKey )

   LOCAL cVal := ''

   IF !Empty( xVal )
      IF Empty( cKey )
         cVal := xVal
      ELSEIF hHasKey( xVal, cKey )
         cVal := hGet( xVal, cKey )
         cVal := '<' + cKey + '>' + cVal + '</' + cKey + '>'
         hDel( xVal, cKey )
      ENDIF
   ENDIF

   RETURN cVal

STATIC FUNCTION HtmlAllTag( hTags, cSep )

   LOCAL cVal := ''

   DEFAULT cSep TO ' '
   hEval( hTags, { |k| cVal += HtmlTag( hTags, k ) + cSep } )

   RETURN cVal

STATIC FUNCTION HtmlOption( xVal, cKey, cPre, cPost, lScan )

   LOCAL cVal := ''

   IF !Empty( xVal )
      IF Empty( cKey )
         cVal := xVal
      ELSEIF hHasKey( xVal, cKey )
         cVal := hGet( xVal, cKey )
         IF Empty( lScan )
            hDel( xVal, cKey )
         ENDIF
         IF !Empty( cPre ) .AND. !Empty( cPost )
            cVal := cPre + cKey + cPost + cVal
         ELSE
            cVal := cKey + '="' + cVal + '"'
         ENDIF
      ENDIF
   ENDIF

   RETURN cVal

STATIC FUNCTION HtmlAllOption( hOptions, cSep )

   LOCAL cVal := ''

   DEFAULT cSep TO ' '
   IF !Empty( hOptions )
      hEval( hOptions, { |k| cVal += HtmlOption( hOptions, k,,, .T. ) + cSep } )
   ENDIF

   RETURN cVal

STATIC FUNCTION HtmlValue( xVal, cKey, cDefault )

   LOCAL cVal := ''

   DEFAULT cDefault TO ''
   IF Empty( xVal )
      cVal := cDefault
   ELSEIF Empty( cKey )
      cVal := xVal
   ELSEIF hHasKey( xVal, cKey )
      cVal := hGet( xVal, cKey )
      hDel( xVal, cKey )
   ENDIF

   RETURN cVal

STATIC FUNCTION HtmlAllValue( hValues, cSep )

   LOCAL cVal := ''

   DEFAULT cSep TO ' '
   IF !Empty( hValues )
      hEval( hValues, { |k| cVal += HtmlValue( hValues, k ) + cSep } )
   ENDIF

   RETURN cVal

STATIC FUNCTION HtmlScript( xVal, cKey )

   LOCAL cVal := ''
   LOCAL nPos
   LOCAL cTmp

   DEFAULT cKey TO 'script'
   IF !Empty( xVal )
      IF ( nPos := hGetPos( xVal, cKey ) ) != 0
         cVal := hGetValueAt( xVal, nPos )
         SWITCH ValType( cVal )
         CASE "C"
            cVal := '<script language="JavaScript" type="text/javascript">' + _CRLF + ;
               '<!--' + _CRLF + ;
               cVal + _CRLF + ;
               '-->' + _CRLF + ;
               '</script>'
            EXIT
         CASE "H"
            IF ( nPos := hGetPos( cVal, 'src' ) ) != 0
               cVal := hGetValueAt( cVal, nPos )
               SWITCH ValType( cVal )
               CASE "C"
                  cVal := '<script language="JavaScript" src="' + cVal + '" type="text/javascript">' + _CRLF
                  EXIT
               CASE "A"
                  cTmp := ''
                  AScan( cVal, { |cFile| cTmp += '<script language="JavaScript" src="' + cFile + '" type="text/javascript">' + _CRLF } )
                  cVal := cTmp
                  EXIT
               END SWITCH
            ENDIF
            EXIT
         END SWITCH
         hDel( xVal, cKey )
      ENDIF
   ENDIF

   RETURN cVal

STATIC FUNCTION HtmlStyle( xVal, cKey )

   LOCAL cVal := ''
   LOCAL nPos
   LOCAL cTmp

   DEFAULT cKey TO 'style'
   IF !Empty( xVal )
      IF ( nPos := hGetPos( xVal, cKey ) ) != 0
         cVal := hGetValueAt( xVal, nPos )
         SWITCH ValType( cVal )
         CASE "C"
            cVal := '<style type="text/css">' + _CRLF + ;
               cVal + _CRLF + ;
               '</style>'
            EXIT
         CASE "H"
            IF ( nPos := hGetPos( cVal, 'src' ) ) != 0
               cVal := hGetValueAt( cVal, nPos )
               SWITCH ValType( cVal )
               CASE "C"
                  cVal := '<link rel="StyleSheet" href="' + cVal + '" type="text/css" />'
                  EXIT
               CASE "A"
                  cTmp := ''
                  AScan( cVal, { |cFile| cTmp += '<link rel="StyleSheet" href="' + cFile + '" type="text/css" />' + _CRLF } )
                  cVal := cTmp
                  EXIT
               END SWITCH
            ENDIF
            EXIT
         END SWITCH
         hDel( xVal, cKey )
      ENDIF
   ENDIF

   RETURN cVal

STATIC FUNCTION GenerateSID( cCRCKey )

   LOCAL cSID, nSIDCRC, cSIDCRC, n, cTemp
   LOCAL nLenSID     := SID_LENGTH
   LOCAL cBaseKeys   := BASE_KEY_STRING
   LOCAL nLenKeys    := Len( cBaseKeys )
   LOCAL cRet
   LOCAL nRand, nKey := 0

   DEFAULT cCRCKey  TO CRC_KEY_STRING
   cCRCKey := Left( cCRCKey, 10 )      // Max Lenght must to be of 10 chars
   /* Let's generate the sequence */
   cSID := Space( nLenSID )
   for n := 1 TO nLenSID
      nRand     := HB_RandomInt( 1, nLenKeys )
      cSID[ n ] := cBaseKeys[ nRand ]
      nKey      += nRand
   next
   nSIDCRC := nKey * 51 // Max Value is 99603 a 5 chars number
   cTemp   := StrZero( nSIDCRC, 5 )
   cSIDCRC := ""
   for n := 1 to Len( cTemp )
      cSIDCRC += cCRCKey[ Val( cTemp[ n ] ) + 1 ]
   next
   cRet := cSID + cSIDCRC
   RETURN cRet

STATIC FUNCTION CheckSID( cSID, cCRCKey )

   local nSIDCRC, cSIDCRC, n, cTemp
   local nLenSID     := SID_LENGTH
   local cBaseKeys   := BASE_KEY_STRING
   local nRand, nKey := 0
   DEFAULT cCRCKey  TO CRC_KEY_STRING
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

   RETURN ( Right( cSID, 5 ) == cSIDCRC )

STATIC FUNCTION DateToGMT( dDate, cTime )

   LOCAL cStr
   LOCAL cOldDateFormat := Set( _SET_DATEFORMAT, "dd-mm-yy" )
   LOCAL nDay, nMonth, nYear, nDoW
   LOCAL aDays   := { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" }
   LOCAL aMonths := { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }

   DEFAULT dDate      TO Date()
   DEFAULT cTime      TO Time()
   nDay   := Day( dDate )
   nMonth := Month( dDate )
   nYear  := Year( dDate )
   nDoW   := DOW( dDate )
   cStr   := aDays[ nDow ] + ", " + StrZero( nDay, 2 ) + "-" + aMonths[ nMonth ] + "-" + ;
             Right( StrZero( nYear, 4 ), 2 ) + " " + cTime + " GMT"
   SET( _SET_DATEFORMAT, cOldDateFormat )

   RETURN cStr
