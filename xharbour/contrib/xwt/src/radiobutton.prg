/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: widget.prg,v 1.3 2003/04/07 22:06:39 jonnymind Exp $

   Radio box - A certain cooperation of the container widget is needed
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTRadioButton FROM XWTWidget
   METHOD New( cText )
   METHOD GetStatus()
   METHOD SetStatus( bValue )

   METHOD AddedToContainer( oEvent )
ENDCLASS


METHOD New( cText ) CLASS XWTRadioButton
   ::Super:New()

   ::oRawWidget := XWT_Create( Self, XWT_TYPE_RADIOBUTTON )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

   ::AddEventListener( XWT_E_ADDEDTO, Self, "AddedToContainer" )
RETURN Self


METHOD GetStatus() CLASS XWTRadioButton
   LOCAL bStatus := .F.

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_STATUS, @bStatus )
      RETURN bStatus
   ENDIF
RETURN .F.


METHOD SetStatus( bStatus ) CLASS XWTRadioButton
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_STATUS, bStatus )


METHOD AddedToContainer( oEvent ) CLASS XWTRadioButton
   LOCAL oContainer := oEvent:aParams[1]

   IF oContainer:oRadioBox  == NIL
      oContainer:oRadioBox := ::oRawWidget
   ELSE
      RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_RADIOGROUP, oContainer:oRadioBox )
   ENDIF
   // Managed
RETURN .T.

