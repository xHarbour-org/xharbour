/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: label.prg,v 1.5 2003/08/27 02:46:16 lculik Exp $

   Label class. Just a concrete widget implementation
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTLabel FROM XWTWidget
   METHOD New( cText,x,y,oParent, cFont, nFontSize , cColor)
ENDCLASS

METHOD New( cText, x, y, oParent ,cFont, nFontSize , cColor) CLASS XWTLabel
   Local cFontString :=""
   Local cColorText  :=""
   Local aColor
   Local c

   ::Super:New()
   tracelog(cText, x, y, oParent ,cFont, nFontSize , cColor)
   // sooner or later I will want to add event management for labels,
   // so, I put here the Self parameter needed for event callbacks
   ::nWidgetType := XWT_TYPE_LABEL
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_LABEL )

   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

   IF !Empty( cColor )
      IF "," in cColor // is an RGB String so Convert

      aColor := HB_aTokens( cColor ,",")
      cColorText := "#"

      FOR EACH c in aColor
         cColorText += DecToHexa(Str(c,3))
      NEXT
         
      ELSE
         cColorText := cColor
      ENDIF
      XWT_SetProperty( ::oRawWidget, XWT_PROP_FGCOLOR,  cColorText )
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

   IF ValType( x ) == "N" .and. ValType( y ) == "N"
      ::Move( x, y )
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF
   
RETURN Self
