/**********************************************
* tipurl.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id$
************************************************/
#include "hbclass.ch"

/*
* An URL:
* http://gian:passwd@www.niccolai.ws/mypages/mysite/page.html?avar=0&avar1=1
* ^--^   ^--^ ^----^ ^-------------^ ^----------------------^ ^------------^
* cProto  UID  PWD      cServer             cPath                 cQuery
*                                    ^------------^ ^-------^
*                                      cDirectory     cFile
*                                                   ^--^ ^--^
*                                                 cFname cExt
*/

CLASS tURL
   DATA cAddress
   DATA cProto
   DATA cServer
   DATA cPath
   DATA cQuery
   DATA cFile
   DATA cFname
   DATA cExt
   DATA bDynamic
   DATA nPort
   DATA cUserid
   DATA cPassword

   METHOD New( cUrl )
   METHOD SetAddress( cUrl )
   METHOD BuildAddress()
   METHOD BuildQuery( )

ENDCLASS


METHOD New( cUrl ) CLASS tURL
   ::SetAddress( cUrl )
RETURN Self


METHOD SetAddress( cUrl ) CLASS tURL
   LOCAL nPos

   ::cAddress := NIL
   ::cProto := NIL
   ::cUserid := NIL
   ::cPassword := NIL
   ::cServer := NIL
   ::cPath := NIL
   ::cQuery := NIL
   ::cFile := NIL
   ::cFname := NIL
   ::cExt := NIL
   ::bDynamic := .F.
   ::nPort := -1

   IF Empty( cUrl ) .or. Len( cUrl ) == 0
      RETURN .T.
   ENDIF

   ::cAddress := cUrl

   // Parse protocol e.g. http://
   nPos := At( "://", cUrl )
   IF nPos != 0
      ::cProto := Lower(Substr( cUrl, 1 , nPos -1 ))
      cUrl := Substr( cUrl, nPos + 3 )
   ENDIF

   // parse userid/password
   nPos := At("@", cUrl )
   IF At( "/", cUrl ) == 0 .or. At( "/", cUrl ) > nPos
      ::cUserid := Substr( cUrl, 1 , nPos -1 )
      cUrl := Substr( cUrl, nPos + 1 )
      nPos := At( ":", ::cUserid )
      IF nPos > 0
         ::cPassword := Substr( ::cUserid, nPos + 1 )
         ::cUserid := Substr( ::cUserid, 1 , nPos - 1 )
      ENDIF
      IF Empty( ::cPassword )
         ::cPassword = ""
      ENDIF
      ::cUserid := StrTran( ::cUserid, "&at;", "@" )
      ::cPassword := StrTran( ::cPassword, "&at;", "@" )
   ENDIF
   //Parse server e.g. www.altavista.com
   nPos := At("/", cUrl )
   IF nPos != 1
      IF nPos == 0      // No more slashes?
         ::cServer := cUrl
         ::cPath := "/"
         RETURN .T.
      ELSE
         ::cServer := Substr( cUrl, 1, nPos -1 )
         cUrl := Substr( cUrl, nPos +1 )
      ENDIF
      // finds the port
      nPos := At( ":", ::cServer )
      IF nPos > 1
         ::nPort := Val( Substr( ::cServer, nPos+1 ) )
         ::cServer := Substr( ::cServer , 1, nPos-1 )
      ENDIF
   ELSE
      cUrl := SubStr( cUrl, 2 )
   ENDIF

   //now get pat
   nPos := Rat("/", cUrl )
   IF nPos != 0
      ::cPath := "/" + Substr( cUrl, 1, nPos -1 )
      cUrl := Substr( cUrl, nPos + 1 )
   ELSE
      ::cPath := "/"
   ENDIF

   // get query portion
   nPos := At( "?", cUrl )
   IF nPos > 0
      ::bDynamic := .T.
      ::cQuery := Substr( cUrl, nPos +1 )
      cUrl := Substr( cUrl, 1, nPos -1 )
   ENDIF

   // now let's get the file
   IF Len( cUrl ) > 0
      ::cFile := cUrl
      nPos := Rat(".", cUrl )
      IF nPos > 0
         ::cFname := Substr( cUrl, 1, nPos -1 )
         ::cExt := Substr( cUrl, nPos +1 )
      ENDIF
   ENDIF

RETURN .F.


METHOD BuildAddress() CLASS tURL
   LOCAL cRet := ""

   IF ::cProto != NIL
      ::cProto := Lower( ::cProto )
   ENDIF

   IF .not. Empty( ::cProto ) .and. .not. Empty( ::cServer )
      cRet := ::cProto + "://"
   ENDIF

   IF ! Empty( ::cUserid )
      cRet += ::cUserid
      IF ! Empty( ::cPassword )
         cRet+= ":" + ::cPassword
      ENDIF
      cRet += "@"
   ENDIF

   IF ! Empty( ::cServer )
      cRet += ::cServer
      IF ::nPort > 0
         cRet += ":" + AllTrim( Str( ::nPort ) )
      ENDIF
   ENDIF

   IF .not. Empty( ::cPath ) .or. .not. Empty( ::cFile ) .or. .not. Empty( ::cQuery )
      cRet += "/"
      IF .not. Empty( ::cPath )
         IF At( "/", ::cPath ) == 1
            cRet += Substr( ::cPath, 2 ) + "/"
         ELSE
            cRet += ::cPath + "/"
         ENDIF
      ENDIF
      IF .not. Empty( ::cFile )
         cRet += ::cFile
      ENDIF

      IF ! Empty( ::cQuery )
         cRet += "?" + ::cQuery
      ENDIF
   ENDIF

   IF Len( cRet ) == 0
      cRet := NIL
   ELSE
      ::cAddress := cRet
   ENDIF

RETURN cRet



METHOD BuildQuery( ) CLASS tURL
   LOCAL cLine

   cLine := ::cPath
   IF .not. Empty( ::cFile )
      cLine += ::cFile
      IF .not. Empty( ::cQuery )
         cLine += "?" + ::cQuery
      ENDIF
   ENDIF

RETURN cLine

