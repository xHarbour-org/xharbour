/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: layout.prg,v 1.2 2003/04/07 15:41:06 jonnymind Exp $

   Layer container class is an "abstract class that should be
   used to create leaf container classes.

*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTLayContainer FROM XWTContainer

   METHOD New()
   METHOD SetExpand( bExpand )
   METHOD SetFill( bFill )
   METHOD SetHomogeneous( bHomogeneous )
   METHOD GetExpand( bExpand )
   METHOD GetFill( bFill )
   METHOD GetHomogeneous( bHomogeneous )

ENDCLASS

METHOD New() CLASS XWTLayContainer
   ::Super:New()
RETURN Self


METHOD SetExpand( bExpand ) CLASS XWTLayContainer
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_EXPAND, bExpand )


METHOD SetFill( bFill ) CLASS XWTLayContainer
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_FILL, bFill )


METHOD SetHomogeneous( bHomogeneous ) CLASS XWTLayContainer
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_HOMOGENEOUS, bHomogeneous )


METHOD GetExpand() CLASS XWTLayContainer
   LOCAL bExpand

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_EXPAND, @bExpand )
      RETURN bExpand
   ENDIF
RETURN .F.


METHOD GetFill() CLASS XWTLayContainer
   LOCAL bFill

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_FILL, @bFill )
      RETURN bFill
   ENDIF
RETURN .F.


METHOD GetHomogeneous() CLASS XWTLayContainer
   LOCAL bHomogeneous

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_HOMOGENEOUS, @bHomogeneous )
      RETURN bHomogeneous
   ENDIF
RETURN .F.


