/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: event.prg,v 1.1.1.1 2003/03/21 22:48:52 gian Exp $

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
   IF Empty( aParams )
      ::aParams := aParams
   ELSE
      ::aParams := {}
   ENDIF
RETURN Self


