/**********************************************
* tipurl.prg
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tipurl.prg,v 1.2 2003/06/16 00:41:28 jonnymind Exp $
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
   DATA nPort
   DATA cUserid
   DATA cPassword

   METHOD New( cUrl )
   METHOD SetAddress( cUrl )
   METHOD BuildAddress()
   METHOD BuildQuery( )

HIDDEN:
   CLASSDATA   cREuri   INIT HB_RegexComp("(?:(.*)://)?([^?/]*)/?([^?]*)?\??(.*)")
   CLASSDATA   cREServ  INIT HB_RegexComp("(?:([^:@]*):?([^@:]*)@|)([^:]+):?(.*)")
   CLASSDATA   cREFile  INIT HB_RegexComp("^((?:/.*/)|/)*(.*)$")

ENDCLASS


METHOD New( cUrl ) CLASS tURL
   ::SetAddress( cUrl )
RETURN Self


METHOD SetAddress( cUrl ) CLASS tURL
   LOCAL aMatch, cServer, cPath

   ::cAddress := ""
   ::cProto := ""
   ::cUserid := ""
   ::cPassword := ""
   ::cServer := ""
   ::cPath := ""
   ::cQuery := ""
   ::cFile := ""
   ::nPort := -1

   IF Empty( cUrl ) .or. Len( cUrl ) == 0
      RETURN .T.
   ENDIF

   // TOPLEVEL url parsing
   aMatch:= HB_Regex( ::cREuri, cUrl )

   //May fail
   IF Empty( aMatch )
      RETURN .F.
   ENDIF

   ::cProto := Lower( aMatch[2] )
   cServer := aMatch[3]
   cPath := aMatch[4]
   ::cQuery := aMatch[5]

   ? "Main match", ValToPRg( aMatch )
   // server parsing (can't fail)
   aMatch := HB_Regex( ::cREServ, cServer )
   ::cUserId := aMatch[2]
   ::cPassword := aMatch[3]
   ::cServer := aMatch[4]
   ::nPort := Val(aMatch[5])
   ? "Server match", ValToPRg( aMatch )

   // Parse path and file (can't fail )
   aMatch := HB_Regex( ::cREFile, cPath )
   ::cPath := aMatch[2]
   ::cFile := aMatch[3]
   ? "Path match", ValToPRg( aMatch )

RETURN .T.


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

   IF Len( ::cPath ) == 0 .or. ::cPath[-1] != "/"
      ::cPath += "/"
   ENDIF

   cRet += ::cPath + ::cFile
   IF .not. Empty( ::cQuery )
      cRet += "?" + ::cQuery
   ENDIF

   IF Len( cRet ) == 0
      cRet := NIL
   ELSE
      ::cAddress := cRet
   ENDIF

RETURN cRet



METHOD BuildQuery( ) CLASS tURL
   LOCAL cLine

   IF Len( ::cPath ) == 0 .or. ::cPath[-1] != "/"
      ::cPath += "/"
   ENDIF

   cLine := ::cPath + ::cFile
   IF .not. Empty( ::cQuery )
      cLine += "?" + ::cQuery
   ENDIF

RETURN cLine

