/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: button.prg,v 1.3 2003/07/21 18:14:40 xthefull Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTButton FROM XWTWidget
   METHOD New( cText, x, y, oParent ,cFont, nFontSize , cColor ,cBgColor ,cBaseClr, cTxtClr )
ENDCLASS

METHOD New( cText, x, y, oParent ,cFont, nFontSize , cColor , cBgColor ,cBaseClr, cTxtClr) CLASS XWTButton
   Local cFontString :=""
   Local cColorText  :=""
   Local aColor
   Local c

   ::Super:New()
   ::nWidgetType := XWT_TYPE_BUTTON
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_BUTTON )

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

   IF !Empty( cBgColor )
   cColorText := ""
      IF "," in cBgColor // is an RGB String so Convert

      aColor := HB_aTokens( cBgColor ,",")
      cColorText := "#"

      FOR EACH c in aColor
         cColorText += DecToHexa(Str(c,3))
      NEXT
         
      ELSE
         cColorText := cBgColor
      ENDIF
      XWT_SetProperty( ::oRawWidget, XWT_PROP_BGCOLOR,  cColorText )
   ENDIF

   IF !Empty( cBaseClr )
   cColorText := ""
      IF "," in cBaseClr // is an RGB String so Convert

      aColor := HB_aTokens(  cBaseClr ,",")
      cColorText := "#"

      FOR EACH c in aColor
         cColorText += DecToHexa(Str(c,3))
      NEXT
         
      ELSE
         cColorText :=  cBaseClr
      ENDIF
      XWT_SetProperty( ::oRawWidget, XWT_PROP_BASECOLOR,  cColorText )
   ENDIF

   IF !Empty( cTxtClr )
   cColorText := ""
      IF "," in cTxtClr // is an RGB String so Convert

      aColor := HB_aTokens(  cTxtClr ,",")
      cColorText := "#"

      FOR EACH c in aColor
         cColorText += DecToHexa(Str(c,3))
      NEXT
         
      ELSE
         cColorText :=  cTxtClr
      ENDIF
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXTCOLOR,  cColorText )
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
