/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: menuitem.prg,v 1.4 2003/07/22 16:04:11 xthefull Exp $

   Menuitem class.
*/
#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTMenuItem FROM XWTWidget
   DATA aCallback
   DATA nId
   CLASSDATA nAutoId INIT 1

   METHOD New( cStr, nId, oCalled, oMethod ,cIcon, oMenu, cFont, nSize, cColor)
   METHOD SetIcon( cFileName )

ENDCLASS

METHOD New( cStr, nId, oCalled, oMethod, cIcon, oMenu , cFont, nFontSize, cColor) CLASS XWTMenuItem
   Local cFontString :=""
   Local cColorText  :=""
   Local aColor
   Local c

   ::Super:New()

   IF nId != NIL
      ::nId := nId
   ELSE
      ::nId := ::nAutoId++
   ENDIF

   ::nWidgetType := XWT_TYPE_MENUITEM
   IF .not. Empty( oCalled )
      ::AddEventListener( XWT_E_CLICKED, oCalled, oMethod )
   ENDIF
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_MENUITEM )

   IF .not. Empty( cStr )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cStr )
   ENDIF

   IF cIcon != NIL
      ::SetIcon( cIcon )
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

   IF oMenu != NIL
      oMenu:Add( Self )
   ENDIF

RETURN Self

METHOD SetIcon( cFileName ) CLASS XWTMenuItem
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_IMAGE, cFilename )

