/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: window.prg,v 1.5 2003/03/28 14:44:40 gian Exp $

   Widget class - basic widget & event management
*/
#include "hbclass.ch"
#include "xwt.ch"


CLASS XWTWindow FROM XWTContainer

   METHOD New( cText )
   /*
   METHOD SetFixed( bFix )
   METHOD SetMinimize( bMinimize )
   METHOD SetModal( bMinimize )
   */
ENDCLASS


METHOD New( cText ) CLASS XWTWindow
   ::Super:New()
   ::oRawWidget := XWT_drv_CreateWindow( Self, cText )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

RETURN Self

/*
METHOD SetFixed( bFix ) CLASS XWTWindow
   LOCAL bRes := XWT_WidgetSet( ::oRawWidget, XWT_SET_FIXED, bFix )
   
   IF bRes
      ::bFixed := bFix
   ENDIF
   
RETURN bRes
   
METHOD SetMinimize( bMinimize ) CLASS XWTWindow
   LOCAL bRes := XWT_WidgetSet( ::oRawWidget, XWT_SET_MINIMIZE, bMinimize )
   
   IF bRes
      ::bCanMinimize := bMinimize
   ENDIF
   
RETURN bRes


METHOD SetModal( bModal ) CLASS XWTWindow
   LOCAL bRes := XWT_WidgetSet( ::oRawWidget, XWT_SET_MODAL, bModal )
   
   IF bRes
      ::bModal := bModal
   ENDIF
   
RETURN bRes
*/