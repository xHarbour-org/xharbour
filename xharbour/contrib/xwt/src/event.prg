/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: event.prg,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   Event class
*/

#include "hbclass.ch"

CLASS XWTEvent
   DATA nType
   DATA oSender
   DATA aParams

   Method New( nType, oEmitter, aParams)
ENDCLASS

METHOD New( nType, oSender, aParams ) CLASS XWTEvent
   ::nType := nType
   ::oSender := oSender
   IF .not. Empty( aParams )
      ::aParams := aParams
   ELSE
      ::aParams := {}
   ENDIF
RETURN Self


