/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: button.prg,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTCheckbox FROM XWTWidget
   METHOD New( cText, bStatus )
   METHOD SetStatus( bStatus )
   METHOD GetStatus()
ENDCLASS

METHOD New( cText, bStatus, nX, nY ) CLASS XWTCheckbox
   ::Super:New()
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_CHECKBOX )
   IF .not. Empty( cText )
      XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF
   IF ValType( bStatus ) == "L" .and. bStatus
      XWT_SetProperty( ::oRawWidget, XWT_PROP_STATUS, 1 )
   ENDIF
   IF ValType( nX ) == "N" .and. ValType( nY ) == "N"
      ::Move( nX, nY )
   ENDIF
RETURN Self

METHOD SetStatus( bStatus ) CLASS XWTCheckbox
   IF ValType( bStatus ) == "L" .and. bStatus
      RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_STATUS, 1 )
   ENDIF
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_STATUS, 0 )

METHOD GetStatus() CLASS XWTCheckbox
   LOCAL bRet

   IF XWT_GetProperty( ::oRawWidget, XWT_PROP_STATUS, @bRet )
      RETURN bRet
   ENDIF

RETURN .F.
