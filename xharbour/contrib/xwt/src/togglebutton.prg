/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Rafa Carmona ( Thefull )

   $Id: togglebutton.prg,v 1.1 2003/05/11 15:14:43 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTToggleButton FROM XWTWidget
   METHOD New( cText, bStatus, nX, nY, oParent )
   METHOD SetStatus( bStatus )
   METHOD GetStatus()
ENDCLASS

METHOD New( cText, bStatus, nX, nY, oParent ) CLASS XWTToggleButton
   ::Super:New()
   ::nWidgetType := XWT_TYPE_TOGGLEBUTTON
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_TOGGLEBUTTON )

   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

   IF ValType( bStatus ) == "L" .and. bStatus
      XWT_SetProperty( ::oRawWidget, XWT_PROP_STATUS, 1 )
   ENDIF

   IF ValType( nX ) == "N" .and. ValType( nY ) == "N"
      ::Move( nX, nY )
   ENDIF

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF

RETURN Self

METHOD SetStatus( bStatus ) CLASS XWTToggleButton
   IF ValType( bStatus ) == "L" .and. bStatus
      RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_STATUS, 1 )
   ENDIF
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_STATUS, 0 )

METHOD GetStatus() CLASS XWTToggleButton
   LOCAL bRet

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_STATUS, @bRet )
      RETURN bRet
   ENDIF

RETURN .F.
