/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: layout.prg,v 1.1 2003/04/07 10:27:45 jonnymind Exp $

   Pane class. Each widget is arranged below the
   previous one.
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTLayout FROM XWTContainer

   METHOD New( nMode, nWidth, nHeight )

   METHOD SetPadding( iPadding )
   METHOD SetExpand( bExpand )
   METHOD SetFill( bFill )
   METHOD SetHomogeneous( bHomogeneous )

   METHOD GetPadding( iPadding )
   METHOD GetExpand( bExpand )
   METHOD GetFill( bFill )
   METHOD GetHomogeneous( bHomogeneous )

   METHOD SetBox( bHasBox, cBoxTitle )
   METHOD HasBox()

   METHOD SetBorder( iBorder )
   METHOD GetBorder()

ENDCLASS

METHOD New( nMode, nWidth, nHeight ) CLASS XWTLayout
   ::Super:New()
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_LAYOUT )

   XWT_SetProperty( ::oRawWidget, XWT_PROP_LAYMODE, nMode )
   IF .not. Empty( nWidth ) .and. .not. Empty( nHeight )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_SIZE, nWidth, nHeight )
   ENDIF
RETURN Self


METHOD SetPadding( iPadding ) CLASS XWTLayout
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_PADDING, iPadding )


METHOD SetExpand( bExpand ) CLASS XWTLayout
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_EXPAND, bExpand )


METHOD SetFill( bFill ) CLASS XWTLayout
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_FILL, bFill )


METHOD SetHomogeneous( bHomogeneous ) CLASS XWTLayout
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_HOMOGENEOUS, bHomogeneous )


METHOD GetPadding() CLASS XWTLayout
   LOCAL nPadding

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_PADDING, @nPadding )
      RETURN nPadding
   ENDIF
RETURN -1


METHOD GetExpand() CLASS XWTLayout
   LOCAL bExpand

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_EXPAND, @bExpand )
      RETURN bExpand
   ENDIF
RETURN .F.


METHOD GetFill() CLASS XWTLayout
   LOCAL bFill

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_FILL, @bFill )
      RETURN bFill
   ENDIF
RETURN .F.


METHOD GetHomogeneous() CLASS XWTLayout
   LOCAL bHomogeneous

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_HOMOGENEOUS, @bHomogeneous )
      RETURN bHomogeneous
   ENDIF
RETURN .F.


METHOD SetBox( bHasBox, cBoxTitle ) CLASS XWTLayout

   IF XWT_SetProperty( ::oRawWidget, XWT_PROP_BOX, bHasBox )
      IF .not. Empty( cBoxTitle )
         RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cBoxTitle )
      ENDIF
      RETURN .T.
   ENDIF
RETURN .F.


METHOD HasBox() CLASS XWTLayout
   LOCAL bBox

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_BOX, @bBox )
      RETURN bBox
   ENDIF
RETURN .F.


METHOD SetBorder( iBorder ) CLASS XWTLayout
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_BORDER, iBorder )


METHOD GetBorder() CLASS XWTLayout
   LOCAL iBorder

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_BORDER, @iBorder )
      RETURN iBorder
   ENDIF
RETURN -1
