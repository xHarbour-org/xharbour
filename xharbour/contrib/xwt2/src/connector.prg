/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: framewindow.prg,v 1.4 2003/10/09 23:18:33 jonnymind Exp $

   Connector class. Special widgets that have just one child.
*/

#include "hbclass.ch"
#include "xwt.ch"


CLASS XWTConnector FROM XWTWidget
   METHOD New( xProps )         CONSTRUCTOR
   METHOD Connect( oChild )
   METHOD Disconnect()
   METHOD GetChild()            INLINE ::oChild

HIDDEN:
   DATA oChild
   
ENDCLASS

METHOD New( xProps ) CLASS XWTConnector
   Super:New( xProps )
RETURN Self

METHOD Connect( oChild ) CLASS XWTConnector
   LOCAL bRet 
   
   bRet := .not. XWT_FastRiseEvent( "connect", Self, oChild )
   
   IF bRet
      IF ::oChild != NIL
         bRet := .not. XWT_FastRiseEvent( "disconnect-from", ::oChild, Self )
         XWT_disconnect( ::oRawWidget, ::oChild:oRawWidget )
         ::oChild:oOwner := NIL
      ENDIF
      
      IF bRet
         XWT_connect( ::oRawWidget, oChild:oRawWidget )
         XWT_FastRiseEvent( "connect-to", oChild, Self )
         oChild:oOwner := Self
         ::oChild := oChild
      ENDIF
   ENDIF

RETURN bRet

METHOD Disconnect() CLASS XWTConnector
   LOCAL bRet := .F.
   
   IF ::oChild != NIL
      bRet := .not. XWT_FastRiseEvent( "disconnect", Self, ::oChild )
      
      IF bRet
         bRet := .not. XWT_FastRiseEvent( "disconnect-from", ::oChild, Self )
         XWT_disconnect( ::oRawWidget, ::oChild:oRawWidget )
         ::oChild:oOwner := NIL
      ENDIF
   ENDIF   
RETURN bRet

