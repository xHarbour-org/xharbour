/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Main HTML DocType CLASS for HTML LIB
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

#include "common.ch"
#include "hbclass.ch"
#include "cgidefs.ch"

#xtranslate HTMLIndent( <x> )   => ( Space( ::nIndent ) + <x> )
#xtranslate HTMLIndentLN( <x> ) => ( Space( ::nIndent ) + <x> + CRLF() )

/****
*
*     Class THtmlBase()
*
*/

CLASS THtmlBase

   DATA cFile
   DATA lCGI               INIT TRUE
   DATA nH
   DATA cTitle
   DATA aElements          INIT {}
   //DATA cDocType           INIT '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"> ' // '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">'
   //DATA cDocType           INIT '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">'
   DATA cDocType           INIT '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">'

   DATA cHostHame_HTTP
   DATA cHostHame_HTTPS
   DATA cScriptName
   DATA cRequestType       INIT 'NONSSL'
   DATA lEnableSSL         INIT FALSE

   // Variables like PHP globals
   DATA h_Server           INIT Hash()
   DATA h_Get              INIT Hash()
   DATA h_Post             INIT Hash()
   DATA h_Cookie           INIT Hash()
   DATA h_Files            INIT Hash()
   DATA h_Env              INIT Hash()
   DATA h_Request          INIT Hash()
   DATA h_Session          INIT Hash()

   DATA Session            // INIT TSession():New( QSelf():h_Session )

   METHOD New()            CONSTRUCTOR
   METHOD Add( oElement )  INLINE aAdd( ::aElements, oElement )

   METHOD GetField()
   METHOD SetField()

   METHOD Create()
   METHOD Write()
   MESSAGE Echo            METHOD Write
   METHOD WriteLN()
   METHOD Close()
   METHOD Flush()          INLINE ::Write()
   METHOD Indent( n )      INLINE DEFAULT( n, 1 ), ::nIndent += n
   METHOD UnIndent( n )    INLINE DEFAULT( n, 1 ), ::nIndent -= n
   METHOD GetIndent()      INLINE Space( ::nIndent )

   METHOD OutPut( cHtml )    INLINE ( IIF( ::lOutPut, ::Write( cHtml ), ), cHtml )
   METHOD OutPutOn()         INLINE ::lOutPut := TRUE
   METHOD OutPutOff()        INLINE ::lOutPut := FALSE
   METHOD SetOutPut( lOn )   INLINE LOCAL lOld, lOld := ::lOutPut, IIF( ValType( lOn ) == "L", ::lOutPut := lOn, ), lOld

   METHOD DeferredBegin()  INLINE ::cOutPutData := "", ::lDeferred := TRUE
   METHOD DeferredEnd()    INLINE LOCAL cBuffer, cBuffer := ::cOutPutData, ::cOutPutData := NIL, ::lDeferred := FALSE, cBuffer
   METHOD DeferredReset()  INLINE ::cOutPutData := NIL, ::lDeferred := FALSE
   METHOD DeferredFlush()  INLINE LOCAL cBuffer, cBuffer := ::cOutPutData, ::cOutPutData := NIL, ::lDeferred := FALSE,  ::Write( cBuffer )
   METHOD IsDeferred()     INLINE ::lDeferred
   METHOD GetData()        INLINE ::cOutPutData

   METHOD IsHeaders()      INLINE !Empty( ::aHeaders )
   METHOD HeaderSent()     INLINE ::lHeaderSent
   METHOD Header()
   METHOD Redirect( cLink ) INLINE ::Header( "Location: " + cLink )

   METHOD SetCookie()
   METHOD DeleteCookie()
   METHOD DeleteAllCookies()
   METHOD GetCookie()
   METHOD IsCookie( cCookieName )  INLINE ::GetCookie( cCookieName ) <> NIL
   METHOD IsCookies()      INLINE !Empty( ::aaCookieToSet )
   METHOD SetCookieDefaults()

   METHOD SendRawData()
   METHOD SendSingleRawData()

   METHOD PageHandle()     INLINE ::nH
   METHOD HRef_Link_String()

   HIDDEN:
   DATA nIndent            INIT 0

   DATA lDeferred          INIT FALSE
   DATA cOutPutData

   DATA lOutPutStarted     INIT FALSE
   DATA lOutPut            INIT FALSE

   DATA aHeaders           INIT {}
   DATA lHeaderSent        INIT FALSE

   // Data for cookies
   DATA cCookieDomain
   DATA cCookiePath        INIT "/"
   DATA cCookieExpire
   DATA nCookieExpireDays  INIT 0
   DATA nCookieExpireSecs  INIT 7200       // 1 hour  - TODO set environment constant
   DATA lCookiesSent       INIT FALSE

   DATA nParseTimeStart    INIT 0
   DATA nParseTimeStop     INIT 0

   METHOD LoadVars()
   METHOD GetVars()
   METHOD GetVarsMulti()
   METHOD AddVars( hDest, hVars )     INLINE hDest += hVars // Sum di hashes - work by reference
   METHOD DisplayVars()
   METHOD DisplayHash()

ENDCLASS

METHOD New( cSessionName, lCGI, cTitle, cFile ) CLASS THtmlBase

   ::lCGI   := DEFAULT( lCGI, FALSE )
   ::cTitle := DEFAULT( cTitle, "My HTML Document" )
   IF !::lCGI
      ::cFile  := DEFAULT( cFile, "index.htm" )
   ENDIF

   //::cCookieDomain := "localhost"
   ::cCookiePath   := "/"

   // Headers - Put a standard header
   ::Header( 'Content-Type: text/html' )

   ::Session := TSession():New( ::h_Session, cSessionName )

   ::LoadVars()

   ::cHostHame_HTTP  := "http://" + ::h_Server:SERVER_NAME + IIF( ::h_Server:SERVER_PORT == "80", "", ":" + ::h_Server:SERVER_PORT )
   ::cHostHame_HTTPS := "https://" + ::h_Server:SERVER_NAME + IIF( ::h_Server:SERVER_PORT == "443", "", ":" + ::h_Server:SERVER_PORT )
   ::cScriptName     := ::h_Server:SCRIPT_NAME

   ::Create()

RETURN Self

METHOD Create() CLASS THtmlBase
  // TODO: Start creation only on sending data
  IF ::lCGI
     ::nH := STD_OUT
  ELSE
     ::nH := FCreate( ::cFile )
  ENDIF
RETURN ::nH

// This really send data - MUST BE USED INTERNALLY ONLY
METHOD SendRawData( ... ) CLASS THtmlBase
   LOCAL nCount, cVal

   cVal := Space( ::nIndent )
   FOR nCount := 1 TO PCount()
       cVal += Cstr( HB_Pvalue( nCount ) )
   NEXT

   IF ::cOutPutData != NIL
      ::cOutPutData += cVal
   ELSE
      FWrite( ::nH, cVal )
   ENDIF

RETURN NIL

// As above but for only one data and faster
METHOD SendSingleRawData( xVal ) CLASS THtmlBase

   //TraceLog( "SendSingleRawData", xVal )
   IF ::cOutPutData != NIL
      ::cOutPutData += cStr( xVal )
   ELSE
      FWrite( ::nH, xVal )
   ENDIF

RETURN NIL

METHOD Write( ... ) CLASS THtmlBase
   LOCAL cVal
   LOCAL aParams := HB_aParams()

   // Check if output already sent. Check if headers were sent
   IF !::IsDeferred() .AND. !::lHeaderSent

      cVal := ""

      // Check if there are headers to send
      IF ::IsHeaders()

         // This part have to be actived this code is used from a webserver
         /*
         // First must to be a HTTP/ response header
         IF !( Left( ::aHeaders[ 1 ], 5 ) == "HTTP/" )
            // Put a 200 OK response code
            ::Header( "HTTP/1.0 200 OK "  )
         ENDIF
         */

         aEval( ::aHeaders, {|e| cVal += e } )
         cVal += 'Content-Type: text/html' + CRLF()
         cVal += CRLF() + CRLF()
      ENDIF
      // Ok, Headers added, now add doctype and send

      cVal += ::cDocType + CRLF()

      ::SendSingleRawData( cVal )

      ::lHeaderSent := TRUE

   ENDIF

   // Send other data
   HB_ExecFromArray( Self, "SendRawData", aParams )

RETURN NIL

// Add to write a CRLF
METHOD WriteLn( ... ) CLASS THtmlBase
   LOCAL aParams := HB_aParams()

   aAdd( aParams, CRLF()  )
   HB_ExecFromArray( Self, "Write", aParams )

RETURN NIL

METHOD Close() CLASS THtmlBase
  LOCAL lOk := TRUE

  // Close session if it is active
  IF ::Session <> NIL THEN ::Session:Close()

  // Flush pending data
  ::Flush()

  IF !::lCGI
     lOk := FClose( ::nH )
  ENDIF

RETURN lOk

METHOD GetField( cVar, cType ) CLASS THtmlBase
  LOCAL xVal
  LOCAL nPos := HGetPos( ::h_Request, cVar )

  IF nPos > 0 //cVar IN ::h_Request:Keys
     xVal := HGetValueAt( ::h_Request, nPos ) //::h_Request[ cVar ]
     IF Empty( xVal )
        xVal := NIL
     ENDIF
     IF cType <> NIL .AND. cType $ "NLD"
        xVal := CStrToVal( xVal, cType )
     ENDIF
  ENDIF

RETURN xVal

METHOD SetField( cVar, cVal ) CLASS THtmlBase
  LOCAL xVal := HGetValue( ::h_Request, cVar )
  ::h_Request[ cVar ] := cVal
RETURN xVal

METHOD LoadVars() CLASS THtmlBase
   LOCAL cFields
   LOCAL hVars

   ::h_Server[ 'SERVER_SOFTWARE'   ] := GetEnv( "SERVER_SOFTWARE" )
   ::h_Server[ 'SERVER_NAME'       ] := GetEnv( "SERVER_NAME" )
   ::h_Server[ 'GATEWAY_INTERFACE' ] := GetEnv( "GATEWAY_INTERFACE" )
   ::h_Server[ 'SERVER_PROTOCOL'   ] := GetEnv( "SERVER_PROTOCOL" )
   ::h_Server[ 'SERVER_PORT'       ] := GetEnv( "SERVER_PORT" )
   ::h_Server[ 'REQUEST_METHOD'    ] := GetEnv( "REQUEST_METHOD" )
   ::h_Server[ 'HTTP_ACCEPT'       ] := GetEnv( "HTTP_ACCEPT" )
   ::h_Server[ 'HTTP_USER_AGENT'   ] := GetEnv( "HTTP_USER_AGENT" )
   ::h_Server[ 'HTTP_REFERER'      ] := GetEnv( "HTTP_REFERER" )
   ::h_Server[ 'PATH_INFO'         ] := GetEnv( "PATH_INFO" )
   ::h_Server[ 'PATH_TRANSLATED'   ] := GetEnv( "PATH_TRANSLATED" )
   ::h_Server[ 'SCRIPT_NAME'       ] := GetEnv( "SCRIPT_NAME" )
   ::h_Server[ 'QUERY_STRING'      ] := GetEnv( "QUERY_STRING" )
   ::h_Server[ 'REMOTE_HOST'       ] := GetEnv( "REMOTE_HOST" )
   ::h_Server[ 'REMOTE_ADDR'       ] := GetEnv( "REMOTE_ADDR" )
   ::h_Server[ 'IPADDRESS'         ] := GetEnv( "REMOTE_ADDR" )
   ::h_Server[ 'REMOTE_USER'       ] := GetEnv( "REMOTE_USER" )
   ::h_Server[ 'AUTH_TYPE'         ] := GetEnv( "AUTH_TYPE" )
   ::h_Server[ 'AUTH_USER'         ] := GetEnv( "AUTH_USER" )
   ::h_Server[ 'AUTH_PASS'         ] := GetEnv( "AUTH_PASS" )
   ::h_Server[ 'CONTENT_TYPE'      ] := GetEnv( "CONTENT_TYPE" )
   ::h_Server[ 'CONTENT_LENGTH'    ] := GetEnv( "CONTENT_LENGTH" )
   ::h_Server[ 'ANNOTATION_SERVER' ] := GetEnv( "ANNOTATION_SERVER" )
   ::h_Server[ 'HTTP_COOKIE'       ] := GetEnv( "HTTP_COOKIE" )
   ::h_Server[ 'HTTP_POST_FILES'   ] := GetEnv( "HTTP_POST_FILES" )

   // Before GET fields

   // check if I pass parameters from command line
   IF !Empty( HB_ARGV( 1 ) )
      cFields := HB_ARGV( 1 )
      hVars := ::GetVars( cFields )
      ::AddVars( @::h_Get, hVars )
      ::AddVars( @::h_Request, hVars )
   ENDIF

   // then load values from query string
   cFields := ::h_Server[ 'QUERY_STRING' ]
   IF !Empty( cFields )
      hVars := ::GetVars( cFields )
      ::AddVars( @::h_Get, hVars )
      ::AddVars( @::h_Request, hVars )
   ENDIF

   IF "POST" $ Upper( ::h_Server[ 'REQUEST_METHOD' ] )
      IF Left( ::h_Server[ 'CONTENT_TYPE' ], Len( "multipart/form-data" ) ) == "multipart/form-data"
         hVars := ::GetVarsMulti()
         ::AddVars( @::h_Post, hVars )
         ::AddVars( @::h_Request, hVars )
      ELSE
         cFields := RTrim( FReadStr( 0, Val( ::h_Server[ 'CONTENT_LENGTH' ] ) ) )
         IF !Empty( cFields )
            hVars := ::GetVars( cFields )
            ::AddVars( @::h_Post, hVars )
            ::AddVars( @::h_Request, hVars )
         ENDIF
      ENDIF
      //__OutDebug( "cFields", cFields )

   ENDIF

   cFields := ::h_Server[ 'HTTP_COOKIE' ]
   IF !Empty( cFields )
      hVars := ::GetVars( cFields, ";" )
      ::AddVars( @::h_Cookie, hVars )
      ::AddVars( @::h_Request, hVars )
   ENDIF

RETURN Self

METHOD GetVars( cFields, cSeparator ) CLASS THtmlBase
   LOCAL hHashVars := Hash()
   LOCAL aField, cField, aFields
   LOCAL cName, xValue
   DEFAULT cSeparator TO "&"

   aFields := SplitString( cSeparator, cFields, cSeparator )

   FOR EACH cField in aFields
      aField := SplitString( cField, "=" )
      IF Len( aField ) != 2
         LOOP
      ENDIF

      cName  := LTrim( aField[1] )
      xValue := UrlDecode( aField[2] )

      // is it an array entry?
      IF Substr( cName, Len( cName ) - 1 ) == "[]"
         cName := Substr( cName, 1, Len( cName ) - 2 )

         hHashVars[ cName ] := { xValue }

      ELSE

         hHashVars[ cName ] := xValue

      ENDIF
      //Tracelog( "hHashVars, cName, xValue", DumpValue( hHashVars ), cName, xValue )
   NEXT

RETURN hHashVars

#define FD_CONTENT_DISPOSITION "Content-Disposition: "
#define FD_CONTENT_TYPE        "Content-Type: "

METHOD GetVarsMulti() CLASS THtmlBase
   LOCAL hHashVars := Hash()
   LOCAL cBoundary, nLenB

   LOCAL cType   := ::h_Server[ 'CONTENT_TYPE' ]
   LOCAL nLenght := Val( ::h_Server[ 'CONTENT_LENGTH' ] )
   LOCAL n
   LOCAL cSepBefore := "--", cSepAfter := "--"
   LOCAL cBuff, cVar, cFileName, lFile
   LOCAL aRighe := {}
   LOCAL nBuff, nRead, nReadTot := 0
   LOCAL cEOL   := HB_OSNewLine()

   LOCAL lAscii, lNeedRead, lEOF, cNextLine, cLine, cCurrentLine, nPos, cValue
   LOCAL cLocBuff, lDoubleBuffered, hFileTemp, cFileTemp, nUplError := 0, nFileSize
   LOCAL nFileSizeMax

   IF Left( cType, Len( "multipart/form-data" ) ) == "multipart/form-data"

      // see RFC 2388
      nPos      := AT( "boundary=", cType )
      // Complete boundary contains two initial -
      cBoundary := "--" + Substr( cType, nPos + Len( "boundary=" ) )
      nLenB     := Len( cBoundary )
      n         := 1
      nBuff     := Max( 128, nLenB ) // Buffer length must be min boundary length

      /*

      ATTENTION !!!!
      don't delete this part. It is ok, I have commented out to fix unpack method

      nLen := 0
      nLenTot := 0
      DO WHILE nLenTot < nLenght
         cBuff := Space( nBuff )

         lRead := ReadFile( , @cBuff, nBuff, @nLen )

         IF !lRead .AND. nLen <> 0
            HB_IDLESLEEP( 1 )
            LOOP
         ELSEIF !lRead .AND. nLen == 0
            EXIT
         ENDIF
         nLenTot += nLen

         FWrite( hFile, cBuff, nLen )
      ENDDO

      */

      lFile     := FALSE
      cBuff     := ""
      lAscii    := TRUE
      lNeedRead := TRUE
      lEOF      := FALSE
      cNextLine := "BD"  // First line I'm expecting is a boundary
      cCurrentLine := ""

      DO WHILE TRUE //nReadTot <= nLenght

         // Load buffer
         IF !lEOF .AND. lNeedRead

            cLocBuff := ReadStdInput( nBuff, @nRead )
            nReadTot += nRead
            cBuff += cLocBuff

            // If I have read nothing, file is end
            IF nRead == 0
               lEOF := TRUE
            ENDIF

            lNeedRead := FALSE
         ENDIF

         // Check if I am in ASCII or Binary mode
         IF lAscii

            // If I am in ASCII mode, I will load the buffer until I get an EOL
            IF !lEOF .AND. ( nPos := AT( cEOL, cBuff ) ) == 0
               lNeedRead := TRUE
               LOOP
            ENDIF

            // Read the line
            IF nPos > 0 // Attention, nPos is used also after, don't reuse
               cLine := SubStr( cBuff, 1, nPos - 1 )
            ELSE
               cLine := cBuff
            ENDIF

            // Check if I get end of file boundary
            IF Left( cBuff, Len( cBoundary + "--" ) ) == cBoundary + "--"
               // Yes, I can exit
               EXIT

            // Otherwise I have to check if I get a separator boundary
            ELSEIF Left( cBuff, Len( cBoundary ) ) == cBoundary

               // If yes, I have to reset vars used for next data

               cNextLine := "CD" // Next line is a Content-Disposition

               // Delete boundary
               cBuff := Stuff( cBuff, 1, Len( cBoundary ) + IIF( nPos > 0, Len( cEOL ), 0 ), "" )
               LOOP
            ELSE

               // I have to delete line from buffer
               cBuff := Stuff( cBuff, 1, Len( cLine ) + IIF( nPos > 0, Len( cEOL ), 0 ), "" )

            ENDIF

            // Read line type
            DO CASE
               CASE cNextLine == "CD"   // Content-Disposition

                    IF Left( cLine, Len( FD_CONTENT_DISPOSITION ) ) == FD_CONTENT_DISPOSITION

                       // Split line
                       cLine := SubStr( cLine, Len( FD_CONTENT_DISPOSITION ) + 1 )
                       cCurrentLine := "CD"
                       cType        := Left( cLine, nPos := ( AT( ";", cLine ) - 1 ) )  // es: form-data

                       // Remove type
                       cLine := SubStr( cLine, nPos + 2 ) // Also with next space
                    ELSE
                       // TODO __OutDebug( "ERROR !!!!!" )
                    ENDIF

               CASE cNextLine == "CT"   // Content-Type
                    IF Left( cLine, Len( FD_CONTENT_TYPE ) ) == FD_CONTENT_TYPE

                       // Split line
                       cLine := SubStr( cLine, Len( FD_CONTENT_TYPE ) + 1 )
                       cCurrentLine := "CT"
                       cType        := cLine  // es: application/octect-stream

                       // Remove type
                       cLine := ""

                    ELSE

                       cCurrentLine := "CT"
                       cType := "text/plain"
                       cLine := ""

                    ENDIF

               CASE cNextLine == "TP"   // Ascii-text = var value
                    cValue := cLine
                    // Save the var
                    // is it an array entry?
                    IF Substr( cVar, Len( cVar ) - 1 ) == "[]"
                       cVar := Substr( cVar, 1, Len( cVar ) - 2 )
                       hHashVars[ cVar ] := { cValue }
                    ELSE
                       hHashVars[ cVar ] := cValue
                    ENDIF
                    // finished, loop
                    cCurrentLine := "BD"
                    LOOP

               CASE cNextLine == "AO"   // Binary - file

                    // Make a temporary file
                    cFileTemp := TempFile( Configure( "DIR_FS_UPLOAD" ), Configure( "UPLOAD_FILE_EXT" ) )
                    IF ( hFileTemp := FCreate( cFileTemp, 0 ) ) <> -1

                       // Switch to binary mode
                       lAscii := FALSE
                       lDoubleBuffered := FALSE

                    ELSE
                       // TODO: Gestire l'errore
                    ENDIF

                    // Finished, loop
                    LOOP
            ENDCASE

            // Check the line
            DO CASE
               CASE cCurrentLine == "CD"
                    cVar := cFileName := NIL
                    DO CASE
                       CASE cType == "form-data"

                            // Check if I have a var
                            IF ( nPos := AT( 'name="', cLine ) ) > 0

                               cLine := SubStr( cLine, nPos + Len( 'name="' ) )

                               // Read var name
                               cVar := Left( cLine, nPos := ( AT( '"', cLine ) - 1 ) )

                               // Delete this part that end with '"'
                               cLine := Substr( cLine, nPos + 1 )
                            ENDIF

                            // Check if I have a file name
                            IF ( nPos := AT( 'filename="', cLine ) ) > 0

                               cLine := SubStr( cLine, nPos + Len( 'filename="' ) )

                               // Read file name
                               cFileName := Left( cLine, nPos := ( AT( '"', cLine ) - 1 ) )

                               // Delete this part that end with '"'
                               cLine := Substr( cLine, nPos + 1 )
                            ENDIF

                            // If I'm arrived here I don't need nothing else and I can exit
                            cNextLine := "CT"  // Next line is Content-Type
                    ENDCASE

               CASE cCurrentLine == "CT"
                    DO CASE
                       CASE cType == "text/plain" .OR. Empty( cType )
                            cNextLine := "TP"  // Next Line is Ascii data
                       OTHERWISE
                            cNextLine := "AO"  // Next line is Binary data
                    ENDCASE
            ENDCASE

         ELSE  // BINARY MODE
            // Otherwise

            // TODO - Sorry I have to translate in english
            // se sono in modalit… binaria devo caricare un paio di volte il buffer
            // cosi posso essere sicuro di trovare il boundary, ma se non lo trovo alla
            // seconda lettura, scarico la prima parte sul file temporaneo e riprovo
            // finchŠ non lo trovo.
            // in questo modo posso ricevere file di qualsiasi dimensione senza caricare
            // tutto in memoria

            // Se sono in modalit… binaria controllo se ho il boundary nel buffer
            // altrimento ricarico (ma solo una seconda volta) il buffer per evitare di
            // avere un boundary spezzato

            IF ( nPos := AT( cBoundary, cBuff ) ) == 0 .AND. !lDoubleBuffered
               lDoubleBuffered := TRUE
               lNeedRead := TRUE
               LOOP
            ENDIF

            // Reset doble buffer checking because in meanwhile I have to write file to disk
            lDoubleBuffered := FALSE

            // Check if I have separator boundary
            IF ( nPos := AT( cBoundary, cBuff ) ) > 0

               // Read line from buffer
               cLine := SubStr( cBuff, 1, nPos - 1 - Len( cEOL ) )
               // and empty line buffer
               cBuff := SubStr( cBuff, nPos )

               // Write buffer in temporary file
               FWrite( hFileTemp, cLine, Len( cLine ) )

               // Save data about file

                    cValue       := cFileName
                    nUplError    := 0 // File is OK
                    nFileSize    := FSeek( hFileTemp, 0, 2 )
                    nFileSizeMax := hGetValue( hHashVars, [ 'MAX_FILE_SIZE' ] )

                    IF nFileSize == 0
                       nUplError := 4  // No file uploaded
                    ELSEIF nFileSizeMax <> NIL .AND. nFileSize > nFileSizeMax
                       nUplError := 2  // No file uploaded
                    ENDIF

                    ::h_Files[ cVar ] := { ;
                                           "name" => cFileName ,;
                                           "type" => cType     ,;
                                           "size" => FSeek( hFileTemp, 0, 2 ) ,;
                                           "tmp_name" => cFileTemp, ;
                                           "error" => nUplError ;
                                         }

                    IF Substr( cVar, Len( cVar ) - 1 ) == "[]"
                       cVar := Substr( cVar, 1, Len( cVar ) - 2 )
                       hHashVars[ cVar ] := { ::h_Files[ cVar ] }
                    ELSE
                       hHashVars[ cVar ] := ::h_Files[ cVar ]
                    ENDIF

               // Close the file
               FClose( hFileTemp )

               cNextLine := "BD" // Next line is a Boundary
               lAscii    := TRUE // Return to ASCII mode

            ELSE

               // Read line from buffer
               cLine := cBuff
               // and empty it
               cBuff := ""

               // Write line to temp file
               FWrite( hFileTemp, cLine, Len( cLine ) )

            ENDIF

         ENDIF
      ENDDO

   ELSE
      Die( "Error: " + cStr( FError() ) )
   ENDIF

RETURN hHashVars


// ------------------------------ ***************************** -----------------------------------

METHOD HRef_Link_String( cPage, cParameters, cConnection, lAddSID, lSearchEngineSafe ) CLASS THtmlBase
  LOCAL cLink := ""
  LOCAL cSeparator
  LOCAL cSID

  ::cHostHame_HTTP := ""   // TODO - remove when in production environment

  DEFAULT cConnection       TO "NONSSL"
  DEFAULT lAddSID           TO TRUE
  DEFAULT lSearchEngineSafe TO TRUE

  IF !Empty( cPage )
     IF Lower( Left( cPage, At( ":", cPage ) - 1 ) ) IN { "http", "ftp", "mailto", "news" }
        // External link
        lAddSID := FALSE
     ENDIF
  ENDIF
  DO CASE
     CASE cConnection == "NONSSL"
          cLink := IIF( cPage == NIL, ::cHostHame_HTTP + ::cScriptName, AllTrim( cPage ) )
     CASE cConnection == "SSL"
          IF ::lEnableSSL
             cLink := IIF( cPage == NIL, ::cHostHame_HTTPS + ::cScriptName, AllTrim( cPage ) )
          ELSE
             cLink := IIF( cPage == NIL, ::cHostHame_HTTP + ::cScriptName, AllTrim( cPage ) )
          ENDIF
     OTHERWISE
          Die( "ERROR. Invalid Connection Parameter" )
  ENDCASE

  IF cParameters <> NIL
     cLink += '?' + OutputString( cParameters )
     cSeparator := '&'
  ELSE
     cSeparator := '?'
  ENDIF

  //HB_OutDebug( "HRef_Link_String() cLink prima " + cLink )
  DO WHILE ( ( SubStr(cLink, -1) == '&' ) .OR. ( SubStr( cLink, -1 ) == '?' ) )
     cLink := substr( cLink, 0, -1)
  ENDDO
  //HB_OutDebug( "HRef_Link_String() cLink dopo " + cLink )

  // Add the session ID when moving from different HTTP and HTTPS servers, or when SID is defined
  IF lAddSID .AND. ::Session:IsStarted().AND. !::Session:UseOnlyCookies()
     IF ::Session:ID() <> NIL
        cSID := ::Session:Name() + '=' + ::Session:ID()
     ELSEIF ( ::cRequestType == 'NONSSL' .AND. cConnection == "SSL" .AND. ::lEnableSSL ) .OR.;
            ( ::cRequestType == 'SSL' .AND. cConnection == 'NONSSL' )
        IF !( Configure( "HTTP_COOKIE_DOMAIN" ) == Configure( "HTTPS_COOKIE_DOMAIN" ) )
           cSID := ::Session:Name() + '=' + ::Session:ID()
        ENDIF
     ENDIF
  ENDIF

  IF ( Configure( "SEARCH_ENGINE_FRIENDLY_URLS" ) .AND. lSearchEngineSafe )
     DO WHILE !Empty( StrStr( cLink, '&&') )
        cLink := StrTran( cLink, '&&', '&' )
     ENDDO
     cLink := StrTran( cLink, '?', '/' )
     cLink := StrTran( cLink, '&', '/' )
     cLink := StrTran( cLink, '=', '/' )

     cSeparator := '?'
  ENDIF

  IF cSID <> NIL
    cLink += cSeparator + cSID
  ENDIF

RETURN cLink

// ------------------------------ ***************************** -----------------------------------

METHOD Header( cHeader, lReplace, cResponseCode ) CLASS THtmlBase
  LOCAL nPos, n
  LOCAL cKey

  DEFAULT lReplace TO TRUE

  IF Left( cHeader, 5 ) == "HTTP/"

     // Special case - This header must be the first
     cKey := "HTTP/"
     //cHeader += CRLF()

     IF Len( ::aHeaders ) > 0
        IF Left( ::aHeaders[ 1 ], 5 ) == "HTTP/"
           ::aHeaders[ 1 ] := cHeader
        ELSE
           aSize( ::aHeaders, Len( ::aHeaders ) + 1 )
           aIns( ::aHeaders, 1 )
           ::aHeaders[ 1 ] := cHeader
        ENDIF
     ELSE
        aAdd( ::aHeaders, cHeader )
     ENDIF

  ELSEIF ( nPos := At( ":", cHeader ) ) > 0

     cHeader += CRLF()

     cKey := Left( cHeader, nPos - 1 )

     IF lReplace
        n := aScan( ::aHeaders, {|e| GetHeaderKey( e ) == cKey } )
        IF n > 0
           ::aHeaders[ n ] := cHeader
        ELSE
           aAdd( ::aHeaders, cHeader )
        ENDIF
     ELSE
        aAdd( ::aHeaders, cHeader )
     ENDIF

     IF cKey == "Location"
        cResponseCode := "302 Found "
     ENDIF

     IF cResponseCode <> NIL
        // Recursive call to change the HTTP/ field
        // ::Header( "HTTP/1.0 " + cResponseCode )
     ENDIF

  ELSE
     ::SendSingleRawData( "ERRORE: Header errato" + CRLF() + cStr( cHeader ) )
  ENDIF
RETURN NIL

STATIC FUNCTION GetHeaderKey( cHeader )
  LOCAL nPos := At( ":", cHeader )
RETURN IIF( nPos > 0, Left( cHeader, nPos - 1 ), "" )

// ------------------------------ ***************************** -----------------------------------

METHOD SetCookieDefaults( cDomain, cPath, nExpireDays, nExpireSecs ) CLASS THtmlBase
   IF cDomain     <> NIL THEN ::cCookieDomain := cDomain
   IF cPath       <> NIL THEN ::cCookiePath   := cPath
   IF nExpireDays <> NIL THEN ::nCookieExpireDays := nExpireDays
   IF nExpireSecs <> NIL THEN ::nCookieExpireSecs := nExpireSecs
RETURN NIL

METHOD SetCookie( cCookieName, xValue, cDomain, cPath, cExpires, lSecure ) CLASS THtmlBase
  LOCAL cVal

  DEFAULT cDomain      TO ::cCookieDomain
  DEFAULT cPath        TO ::cCookiePath
  DEFAULT cExpires     TO DateToGMT( Date(), Time(), ::nCookieExpireDays, ::nCookieExpireSecs )

  cVal := "Set-Cookie:" + cCookieName + "=" + UrlEncode( cStr( xValue ) )

  IF cDomain <> NIL
     cVal += "; domain=" + cDomain
  ENDIF
  IF cPath <> NIL
     cVal += "; path=" + cPath
  ENDIF
  IF cExpires <> NIL
     cVal += "; expires=" + cExpires
  ENDIF
  IF ValType( lSecure ) == "L" .AND. lSecure
      cVal += "; secure"
  ENDIF

  //TraceLog( "SendCookies", cVal )

  // Send the header
  ::Header( cVal, FALSE )

  //::SendSingleRawData( cVal )

RETURN NIL

METHOD DeleteCookie( cCookieName, cDomain, cPath, lSecure ) CLASS THtmlBase
  LOCAL cValue := ""
  LOCAL cExpires := DateToGMT( DATE() - 1 ) // Setting date in the past delete cookie

  ::SetCookie( cCookieName, "", cDomain, cPath, cExpires, lSecure )

RETURN NIL

METHOD DeleteAllCookies( cDomain, cPath, lSecure ) CLASS THtmlBase
  LOCAL cCookieName

  FOR EACH cCookieName IN ::h_Cookie:Keys
      //::DeleteCookie( Substr( cCookieName, 2 ), cDomain, cPath, lSecure )
      ::DeleteCookie( cCookieName, cDomain, cPath, lSecure )
  NEXT

RETURN NIL

METHOD GetCookie( cCookieName ) CLASS THtmlBase
   LOCAL cHeader
   LOCAL xRet
   LOCAL n, nLen := Len( ::aHeaders )

   FOR n := 1 TO nLen
       IF GetHeaderKey( ::aHeaders[ n ] ) == "Set-Cookie"
          cHeader := ::aHeaders[ n ]
          IF GetCookieKey( cHeader ) == cCookieName
             xRet := ::aHeaders[ n ]
             EXIT
          ENDIF
       ENDIF
   NEXT

RETURN xRet

STATIC FUNCTION GetCookieKey( cHeader )
  LOCAL nPos    := At( "Set-Cookie:", cHeader )
  LOCAL cCookie := IIF( nPos > 0, Substr( cHeader, nPos + 1 ), "" )
  LOCAL cKey
  IF cCookie <> ""
     IF ( nPos := At( "=", cCookie ) ) > 0
        cKey := Left( cCookie, nPos - 1 )
     ENDIF
  ENDIF
RETURN cKey


