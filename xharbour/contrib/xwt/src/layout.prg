/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: layout.prg,v 1.4 2003/04/12 23:47:15 jonnymind Exp $

   Pane class. Each widget is arranged below the
   previous one.
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTLayout FROM XWTLayContainer

   METHOD New( nMode, nWidth, nHeight ) CONSTRUCTOR

   METHOD SetPadding( iPadding )
   METHOD GetPadding( iPadding )

ENDCLASS

METHOD New( nMode, nWidth, nHeight ) CLASS XWTLayout
   ::Super:New()
   ::nWidgetType := XWT_TYPE_LAYOUT
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_LAYOUT )

   XWT_SetProperty( ::oRawWidget, XWT_PROP_LAYMODE, nMode )
   IF .not. Empty( nWidth ) .and. .not. Empty( nHeight )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_SIZE, nWidth, nHeight )
   ENDIF
RETURN Self


METHOD SetPadding( iPadding ) CLASS XWTLayout
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_PADDING, iPadding )


METHOD GetPadding() CLASS XWTLayout
   LOCAL nPadding

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_PADDING, @nPadding )
      RETURN nPadding
   ENDIF
RETURN -1

