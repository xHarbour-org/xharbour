/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: radiobutton.prg,v 1.2 2003/05/11 15:14:43 jonnymind Exp $

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
   Local cColorText  :=""
   Local aColor
   Local c

   ::Super:New()
   ::nWidgetType := XWT_TYPE_RADIOBUTTON
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_RADIOBUTTON )
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

