/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: button.prg,v 1.4 2003/08/29 02:06:38 lculik Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTButton FROM XWTWidget
   METHOD New( cText, x, y, oParent ,cFont, nFontSize , cColor ,cBgColor ,cBaseClr, cTxtClr )
ENDCLASS

METHOD New( cText, x, y, oParent ,cFont, nFontSize , cColor , cBgColor ,cBaseClr, cTxtClr) CLASS XWTButton
   Local cFontString :=""

   ::Super:New()
   ::nWidgetType := XWT_TYPE_BUTTON
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_BUTTON )

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

   IF Valtype( x ) == "N" .and. Valtype( y ) == "N"
      ::Move(x,y)
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self
