/*
 * $Id: sitesvr.prg,v 1.1 2003/01/10 22:17:58 jonnymind Exp $
 */

***********************************************************
* Web Server in xHarbour programming language!
* Copyright, Giancarlo Niccolai 2002
*
* LICENSE
* This software is released under GPL license 2.0 or any
* later version, at your option. This is free software,
* provided as is, without any warranty, even without the
* implied warranty of suitability for any particular needs.
* Commercial use of this software is forbidden.
*
*

GLOBAL g_nUserCount
GLOBAL g_nTotalCount
GLOBAL MutexDB
EXTERN BuildDBF

PROCEDURE Main( cPort)
   LOCAL GetList
   LOCAL socket
   LOCAL Key
   LOCAL cCommand
   LOCAL hView, hAccept

   BuildDBF()

   IF cPort == NIL
      cPort := "8085"
   ENDIF

   GetList := {}
   g_nUserCount  := 0
   g_nTotalCount := 0

   MutexDB := CreateMutex()

   CLEAR SCREEN

   ********************************************
   * SITE.DBF our DBF database
   * SITE01.NTX : index on ID field
   * SITE02.NTX : index on "str( parent ) + '-' + NAME
   * This second index allows to pinpoint a page in an area
   *

   SELECT 1
   USE "site.dbf" INDEX "site01.ntx", "site02.ntx"

   @ 1, 15 SAY "Welcome to Giancarlo Niccolai's xHarbour WEB SERVER"

   InetInit()

   Socket := InetServer( val( cPort) )

   @ 3, 10 SAY "Waiting for connections on port " + cport

   * Initializing List of Threads

   hView   := StartThread( @ViewUpdate(), Socket )
   hAccept := StartThread( @AcceptIncoming(), Socket )

   DO WHILE .T.
      cCommand := Space( 50 )

      @ 5, 5 SAY "Enter Command      : " GET cCommand
      READ

      cCommand := Trim( cCommand )

      IF Upper( cCommand ) == "QUIT"
         StopThread( hView )
         StopThread( hAccept )
         EXIT
      ENDIF
   ENDDO

   InetDestroy( Socket )
   InetCleanup()
   DestroyMutex( MutexDB )

   CLOSE ALL

RETURN

******************************************
* Managing visual updates
*

PROCEDURE ViewUpdate( Socket )

   LOCAL nRow, nCol
   LOCAL nProgress

   TraceLog( Socket )

   nProgress := 0

   DO WHILE .T.
      * Saving cursor status before screen update

      HBConsoleLock()

      nRow := Row()
      nCol := Col()

      Looping( @nProgress, 6, 5 )

      @ 6, 9 SAY "Looping "
      @ 7, 5 SAY "Main socket status : " + InetErrorDesc( Socket ) + ;
                 "(" + Trim( Str( InetErrorCode( Socket ) ) ) + ")"
      @ 8, 5 SAY "Connected Users    : " + Str( g_nUserCount )
      @ 9, 5 SAY "Total users        : " + Str( g_nTotalCount )

      @ nRow, nCol
      HBConsoleUnlock()

      ThreadSleep( 100 )
   ENDDO

RETURN

************************************************************
* Server Socket manager
*

PROCEDURE AcceptIncoming( Socket )

   LOCAL Com

   TraceLog( Socket )

   DO WHILE .T.
      * Saving cursor status before screen update
      Com := InetAccept( Socket )

      IF InetErrorCode( Com ) == 0
         g_nUserCount++
         g_nTotalCount++

         *StartThread( @ServeClient(), com )
         ServeClient( Com )
         HB_GcAll( .T. )
      ELSE
         InetDestroy( Com )
         EXIT
      ENDIF
   ENDDO

RETURN



************************************************************
* Service incoming connection
*

PROCEDURE ServeClient( Socket )
   LOCAL cRequest
   LOCAL cLine
   LOCAL aFields
   LOCAL cPostData := ""

   LOCAL nLength   := 0
   LOCAL nContLen  := 0

   aFields := {}

   *** First of all, we must take the request of the user
   cRequest := InetRecvLine( Socket, @nLength )
   IF nLength < 0
      InetDestroy( Socket )
      RETURN
   ENDIF

   *** Then we must get all the HTTP fields sent by the navigator
   WHILE .T.
      cLine := InetRecvLine( Socket, @nLength )
      *** end of record request ? nLength == 2
      IF nLength == 0
         EXIT
      ENDIF

      *** An invalid line request has length between -1 and 1
      IF nLength > 1
         AAdd( @aFields, cLine )
         *** We must record Content-Length for later
         IF At( "CONTENT-LENGTH:", Upper( cLine ) ) == 1
            cLine    := Substr( cLine, At( ":", cLine)+1 )
            nContLen := Val( cLine )
         ENDIF
      ELSE
         *** invalid
         InetDestroy( Socket )
         RETURN
      ENDIF
   ENDDO

   *** Now, if the request has a content-lenght, we must read it
   IF nContLen > 0
      *** cPostData is autoAllocated
      cPostData := Space( nContLen )
      IF InetRecvAll( Socket, @cPostData, nContLen ) <= 0
         InetDestroy( Socket )
         RETURN
      ENDIF
   ENDIF

   *** Now we process the request:
   ProcessRequest( Socket, @cRequest, @aFields, cPostData )

   *** The segmentation fault should be here.
   InetClose( Socket )
   g_nUserCount--

RETURN


************************************************************
* Processing the request
*

PROCEDURE ProcessRequest( Socket, cRequest, aFields, cPostData )

   LOCAL cReply, cField, nR
   LOCAL nRow, nCol

   *** For now, just display some data from the request
   HBConsoleLock()

   nRow := Row()
   nCol := Col()
   @ 15, 1 SAY "REQ: " + Substr( cRequest, 0 , 75 ) + Space( 60 )
   @ 16, 1 SAY "CONTENT-LENGTH: " + RTrim( Str( Len( cPostData ) ) ) + Space( 60 )

   nR := 12

/*
   FOR EACH cField in aFields
      IF nR == 23
         @nR,1 SAY ".... and more"
         EXIT
      ENDIF
      @nR, 1 SAY cField
      nR++
   NEXT
*/

   @nRow, nCol
   HBConsoleUnlock()

   IF At( "/admin", cRequest ) > 0
      cReply := CreateReply( 200, "OK", ProcessAdminRequest( cRequest, cPostData ) )
   ELSE
      cReply := ProcessFileRequest( cRequest )
   ENDIF

   InetSendAll( Socket, @cReply )
   ThreadSleep( 100 )
RETURN

******************************************
* Standard request processing
*
PROCEDURE ParseRequest( cRequest, cReq, cFile, cSign )
   /** Request field is <REQ> <FILE> <HTTPsing> **/
   cReq     := Substr( cRequest, 0, At( " ", cRequest ) -1 )
   cRequest := Substr( cRequest, At( " ", cRequest ) +1 )
   cFile    := Substr( cRequest, 0, At( " ", cRequest ) -1 )
   cSign    := Substr( cRequest, At( " ", cRequest ) +1 )
RETURN

******************************************
* Processing a standard file request
*

FUNCTION ProcessFileRequest( cRequest )
   LOCAL cReq, cFile, cSign, cFileOrig
   LOCAL nLocId, cLocName, cReply

   ParseRequest( cRequest, @cReq, @cFile, @cSign )

   /*** NEED TO LOCK THE DATABASE HERE ***/
   MutexLock( MutexDB )

   ** Using parent/name based index
   SET ORDER TO 2

   /* PARENT of the root directory */
   nLocId    := 0
   cFileOrig := cFile

   DO WHILE Len( cFile ) > 0

      IF At( "/", cFile ) > 0
         cLocName := Substr( cFile, 1, At( "/", cFile ) -1 )
      ELSE
         cLocName := cFile
      ENDIF

      IF cLocName == ""
         if nLocId == 0
            *** Index page:
            cLocName := "ROOT"
         ELSE
            *** remove "///"
            cFile    := Substr( cFile, 2 )
            LOOP
         ENDIF
      ENDIF

      *** do the seek: current location + the name of the child
      SEEK Str( nLocId ) + "-" + cLocName

      IF .NOT. Found()
         cReply := PageNotFound( cFileOrig )
         nLocId := -1
         EXIT
      ENDIF

      nLocId := field->ID

      IF At( "/", cFile ) > 0
         cFile := Substr( cFile, At( "/", cFile ) +1 )
      ELSE
         cFile := ""
      ENDIF
   ENDDO

   ** Found ?
   IF nLocId > 0
      cReply := CreateReply( 200, "OK", field->CONTENT, field->MIMETYPE )
   ENDIF

   MutexUnlock( MutexDB )

RETURN cReply

************************************************************
* Process An administrative request
*
FUNCTION ProcessAdminRequest( cRequest, cPostData )
   LOCAL cReq, cFile, cSign
   LOCAL cReply := ""
   LOCAL nID, nPos, cFields, aFields

   ParseRequest( cRequest, @cReq, @cFile, @cSign )

   nPos := At( "?ID=", cFile )
   IF nPos > 0
      nID   := Val( Substr( cFile, nPos + 4 ) )
      cFile := Substr( cFile, 1, nPos -1 )
   ELSE
      nID   := -1
   ENDIF


   DO CASE
      CASE cFile == "/admin"
           cReply := AdminFrontPage()

      CASE cFile == "/admin/new"
           cReply := AdminEditPageMask( -nID )

      CASE cFile == "/admin/edit"
           cReply := AdminEditPageMask( nID )

      CASE cFile == "/admin/del"
           cReply := AdminDelPage( nID )

      CASE cFile == "/admin/mod"
           cReply := AdminModPage( cPostData )

   ENDCASE

RETURN cReply

****************************************************************
* The front page
*

FUNCTION AdminFrontPage()
   LOCAL cReply

   cReply := "<HTML><HEAD><TITLE>xHarbour Server Administration</TITLE></HEAD>" +;
             "<BODY><H1>xHarbour Server Administration</H1><H2>Directory structure:</H2>"

   /* Now we can traverse the whole database in search of our pages */
   MutexLock( MutexDB )
   SET ORDER TO 1
   SEEK 1
   AddPageToList( @cReply, 0 )
   MutexUnlock( MutexDB )

   DrawDBTree( 1, 1, @cReply )

   cReply += "</BODY></HTML> "
RETURN cReply

PROCEDURE DrawDBTree( nItemID, nLevel, cReply )
   LOCAL i, nID, nRecno, cID

   MutexLock( MutexDB )
   cID := AllTrim( Str( nItemID ) )
   /* Now draw the tree for all the elements that have myself as parent */
   SET ORDER TO 2
   SEEK Str( nItemID ) + "-"

   DO WHILE FIELD->PARENT == nItemID
      nID    := FIELD->ID
      nRecno := Recno()
      AddPageToList( @cReply, nLevel )
      MutexUnlock( MutexDB )

      DrawDBTree( nID, nLevel + 1, @cReply )

      MutexLock( MutexDB )
      SET ORDER TO 2
      GOTO nRecno
      SKIP
   ENDDO

   MutexUnlock( MutexDB )

   /** The ADD request */
   FOR i := 1 TO nLevel
       cReply += "&nbsp;&nbsp;&nbsp;"
   NEXT
   cReply += "<a href='/admin/new?ID=" + cID +"'>--&gt;Add a page here</a><BR>"

RETURN

PROCEDURE AddPageToList( cReply, nLevel )
   LOCAL cID, i

   cID := AllTrim( Str( FIELD->ID ) )

   FOR i := 1 TO nLevel
       cReply += "&nbsp;&nbsp;&nbsp;"
   NEXT

   cReply += AllTrim(FIELD->NAME) + " &nbsp;<a href='/admin/edit?ID=" + cID +;
             "'>&lt;mod&gt;</a>&nbsp;<a href='/admin/del?ID=" + cID +;
             "'>&lt;del&gt;</a><BR>"
RETURN

************************************************************
* Managing the pages
*

FUNCTION AdminEditPageMask( nID )
   LOCAL cReply, cName, cID, nParent

   MutexLock( MutexDB )
   SET ORDER TO 1

   /** IF ID < 0 , we must add a page */
   IF nID < 0
      GOTO BOTTOM
      nParent := -nID
      nID     := FIELD->ID + 1
      APPEND BLANK
      REPLACE ID WITH nID, PARENT WITH nParent
   ELSE
      SEEK nID
   ENDIF

   cReply := "<HTML><HEAD><TITLE>Modify record</TITLE></HEAD><BODY>"

   IF .NOT. Found()
      cReply += "<P>ERROR: record not found"
   ELSE
      cName  := AllTrim( FIELD->NAME )
      cID    := AllTrim( Str( nID ) )
      cReply := "<H1>Changing page " +cName + "(" + cID + ")</H1>" +;
                "<FORM ACTION='/admin/mod' METHOD='POST'>" + ;
                "<INPUT TYPE='HIDDEN' NAME='ID' VALUE='" + cID +"'>" +;
                "<P>Page Name: <INPUT NAME='NAME' SIZE='30' MAXLENGTH='30' TYPE='TEXT' VALUE='"+ cName + "'><BR>" +;
                "Page Type: <INPUT NAME='MIMETYPE' SIZE='30' MAXLENGTH='30' TYPE='TEXT' VALUE='"+ AllTrim( FIELD->MIMETYPE ) + "'><BR>"
      IF FIELD->MIMETYPE = "text/plain" .OR. FIELD->MIMETYPE = "text/html" .OR.;
         FIELD->MIMETYPE = "text/xml"   .OR. FIELD->MIMETYPE = ""
         cReply += "<P>Change the content:<BR><TEXTAREA ROWS='10' COLS='70' NAME='CONTENT'>" +;
                   FIELD->CONTENT +;
                   "</TEXTAREA>"
      ENDIF

      cReply += "<BR><BR><INPUT TYPE='SUBMIT' VALUE='Modify'></FORM>"
   ENDIF

   MutexUnlock( MutexDB )

   cReply += "<BR><HR><BR><a href='/admin'>Return to administration page</a>" +;
             "</BODY></HTML> "

RETURN cReply

***********************************************************
* Delete a page
*

FUNCTION AdminDelPage( nID )
   LOCAL cReply

   MutexLock( MutexDB )
   SET ORDER TO 1
   SEEK nID

   cReply := "<HTML><HEAD><TITLE>Delete page</TITLE></HEAD><BODY>"
   IF .NOT. Found()
      cReply += "<P>ERROR: record not found"
   ELSE
      DELETE
      PACK
      cReply += "<P>Record deleted"
   ENDIF

   MutexUnlock( MutexDB )

   cReply += "<BR><HR><BR><a href='/admin'>Return to administration page</a>" +;
             "</BODY></HTML> "

RETURN cReply

************************************************************
* Change a page content
*
FUNCTION AdminModPage( cPostData )
   LOCAL cReply, cName, nID, cContent, cMimeType
   LOCAL aFields := ParsePostData( cPostData )


   MutexLock( MutexDB )
   SET ORDER TO 1
   nID := Val( GetPostField( aFields, "ID" ) )
   SEEK nID

   cReply := "<HTML><HEAD><TITLE>Modify page</TITLE></HEAD><BODY>"
   IF .NOT. Found()
      cReply += "<P>ERROR: record not found"
   ELSE
      cName     := GetPostField( aFields, "NAME" )
      cContent  := GetPostField( aFields, "CONTENT" )
      cMimeType := GetPostField( aFields, "MIMETYPE" )

      IF cName != NIL
         REPLACE NAME WITH cName
      ENDIF

      IF cContent != NIL
         REPLACE CONTENT WITH cContent
      ENDIF

      IF cMimeType != NIL
         REPLACE MIMETYPE WITH cMimeType
      ENDIF

      cReply += "<P>Page has been Changed"
   ENDIF

   MutexUnlock( MutexDB )

   cReply += "<BR><HR><BR><a href='/admin'>Return to administration page</a>" +;
             "</BODY></HTML> "

RETURN cReply


************************************************************
* Parsing POST http request data
*
FUNCTION ParsePostData( cPostData )
   LOCAL cField, cValue, cElem
   LOCAL aFields := {}

   DO WHILE Len( cPostData ) > 0
      IF At( "&", cPostData ) > 1
         cElem     := Substr( cPostData, 1, At( "&", cPostData ) - 1 )
         cPostData := SubStr( cPostData, At( "&", cPostData) + 1 )
      ELSE
         cElem     := cPostData
         cPostData := ""
      ENDIF
      cField := Substr( cElem, 1, At( "=", cElem ) - 1  )
      cValue := Substr( cElem, At( "=", cElem ) + 1  )

      cValue := URLDecode( cValue )

      AAdd( aFields, { cField, cValue } )
   ENDDO

RETURN aFields

FUNCTION URLDecode( cStr )
   LOCAL cRet := "", i, nVal, cCar

   FOR i := 1 TO Len( cStr )
      DO CASE

         CASE cStr[i] == "+"
              cRet += " "

         CASE cStr[i] == "%"
              cCar := UPPER( cStr[i+1] )
              IF cCar >= "0" .AND. cCar <= "9"
                 nVal := ( Asc( cCar ) - Asc( "0" ) ) * 16
              ELSE
                 nVal := ( Asc( cCar ) - Asc( "A" ) + 10 ) * 16
              ENDIF

              cCar := UPPER( cStr[i+2] )
              IF cCar >= "0" .AND. cCar <= "9"
                 nVal += ( Asc( cCar ) - Asc( "0" ) )
              ELSE
                 nVal += Asc( cCar ) - Asc( "A" ) + 10
              ENDIF
              cRet += Chr( nVal )
              i += 2

         OTHERWISE
              cRet += cStr[i]

      ENDCASE
   NEXT
RETURN cRet

FUNCTION GetPostField( aFields, cField )
   LOCAL cRet := NIL, nPos := 1

   DO WHILE cRet == NIL .AND. nPos <= Len( aFields )
      IF aFields[ nPos ][1] == cField
         cRet := aFields[ nPos ][2]
      ENDIF
      nPos ++
   ENDDO
RETURN cRet

************************************************************
* Default replies
*
FUNCTION CreateReply( nCode, cDesc, cContent, cMimeType )
   LOCAL cReply, CRLF := InetCRLF()

   IF cMimeType == NIL
      cMimeType := "text/html"
   ENDIF

   cReply := "HTTP/1.1 "+ AllTrim( Str( nCode ) ) + " " + cDesc + CRLF +;
             "Server: Clipper Server" + CRLF +;
             "Content-Type: " + cMimeType + CRLF +;
             "Pragma: No-Cache" + CRLF +;
             "Content-Length: " + RTrim( Str( Len( cContent ) ) ) + CRLF +;
             CRLF +;
             cContent

RETURN cReply


FUNCTION PageDown()
   LOCAL cContent

   cContent := "<HTML><HEAD><TITLE>Page Down</TITLE></HEAD>" + ;
               "<BODY><H1>Sorry</H1><P>We are currently down; retry later</BODY>"

RETURN CreateReply( 501, "Site down", cContent )

FUNCTION PageNotFound( cPage )
   LOCAL cContent

   cContent := "<HTML><HEAD><TITLE>Page NotFound</TITLE></HEAD>" + ;
               "<BODY><H1>Sorry</H1><P>Page " + cPage + " has not been found.</BODY></HTML>"

RETURN CreateReply( 501, "Site down", cContent )

************************************************************
* Girello
*
PROCEDURE Looping( nProgress,  nR, nC )

   LOCAL nRow := Row(), nCol := Col()

   IF nProgress > 3 .OR. nProgress < 0
      nProgress := 0
   ENDIF

   @ nR, nC SAY "[ ]"
   @ nRow, nCol

   DO CASE
      CASE nProgress == 0
           @  nR, nC + 1 SAY "-"
      CASE nProgress == 1
           @  nR, nC + 1 SAY "\"
      CASE nProgress == 2
           @  nR, nC + 1 SAY "|"
      CASE nProgress == 3
           @  nR, nC + 1 SAY "/"
   ENDCASE

   nProgress++

   @ nRow, nCol

RETURN


                  