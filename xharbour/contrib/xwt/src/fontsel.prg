/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Luiz Rafael

   $Id: filesel.prg,v 1.4 2003/05/11 15:14:43 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTFontSel FROM XWTWidget
   METHOD New( cText, cFontName )
   METHOD GetFile()
   METHOD SetFile( cFileName )
   METHOD DoModal()
ENDCLASS

METHOD New( cText, cFileName ) CLASS XWTFontSel
   ::Super:New()
   ::nWidgetType := XWT_TYPE_FONTSEL
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_FONTSEL )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF
   IF .not. Empty( cFileName )
      ::SetFile( cFileName )
   ENDIF
RETURN Self

METHOD GetFile() Class XWTFontSel
   LOCAL cFile := Space( 1024 )

   IF  XWT_GetProperty( ::oRawWidget, XWT_PROP_FONTNAME, @cFile)
      RETURN cFile
   ENDIF

RETURN ""


METHOD SetFile( cFileName ) Class XWTFontSel
   IF  XWT_SetProperty( ::oRawWidget, XWT_PROP_FONTNAME, cFileName )
      RETURN .T.
   ENDIF
RETURN .F.


METHOD DoModal() Class XWTFontSel
   LOCAL cFile

   ::Show()
   XWT_Modal( ::oRawWidget )
   cFile := ::GetFile()
   ::Destroy()

RETURN cFile
