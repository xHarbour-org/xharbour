/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: radiobutton.prg,v 1.3 2003/08/30 21:37:52 lculik Exp $

   Radio box - A certain cooperation of the container widget is needed
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTRadioButton FROM XWTWidget
   METHOD New( cText ,cFont, nFontSize , cColor , cBgColor ,cBaseClr, cTxtClr)
   METHOD GetStatus()
   METHOD SetStatus( bValue )

   METHOD AddedToContainer( oEvent )
ENDCLASS


METHOD New( cText ,cFont, nFontSize , cColor , cBgColor ,cBaseClr, cTxtClr) CLASS XWTRadioButton
   Local cFontString :=""

   ::Super:New()
   ::nWidgetType := XWT_TYPE_RADIOBUTTON
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_RADIOBUTTON )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

   IF !Empty( cColor )
      ::SetColor( cColor )
   ENDIF

   IF !Empty( cBgColor )
      ::SetColor( cBgColor, XWT_PROP_BGCOLOR )
   ENDIF

   IF !Empty( cBaseClr )
      ::SetColor( cBaseClr, XWT_PROP_BASECOLOR )
   ENDIF

   IF !Empty( cTxtClr )
      ::SetColor( cTxtClr, XWT_PROP_TEXTCOLOR )
   ENDIF

   IF Valtype( cFont )  == "C"
      cFontString += cFont
      IF Valtype( nFontSize ) == "N"
         cFontString += " "+ Str(nFontSize,2,0)
      ENDIF
      XWT_SetProperty( ::oRawWidget, XWT_PROP_FONT, cFontString )
   ENDIF

  IF Valtype( nFontSize ) == "N"
     cFontString :=  Str( nFontSize ,2 )
     XWT_SetProperty( ::oRawWidget, XWT_PROP_FONT, cFontString )
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

