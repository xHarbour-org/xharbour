/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: widget.prg,v 1.4 2003/04/12 23:47:15 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"
#include "xwt.ch"

Extern XwtEvent

CLASS XWTWidget

   /* Pointer to internal data used by current subsystem */
   DATA oRawWidget
   DATA x
   DATA y
   DATA bEventsToParent

   /* The identifier can be a number or a string to uniquely identify an
      item in your application */
   DATA Identity

   DATA aEventListeners

   DATA oOwner

   METHOD New()

   METHOD Show()
   METHOD Hide()

   METHOD Move( x, y )
   METHOD Resize( width, height )
   METHOD Reposition( x,y, width, height )

   METHOD SetFocus()
   METHOD HasFocus()
   METHOD SetText( cText )
   METHOD GetText()

   METHOD GetType()        INLINE   ::nWidgetType

   METHOD SetSensible()
   METHOD IsSensible()

   METHOD AddListener( oListener, mthListener )
   METHOD AddEventListener( nType, oListener, mthListener )
   METHOD RemoveListener( oListener, mthListener )

   METHOD RiseEvent( oEvent )
   METHOD Destroy()

PROTECTED:
   DATA nWidgetType

ENDCLASS


METHOD New() CLASS XWTWidget
   /* IMPORTANT! oRawWidget MUST be initialized by subclasses */
   ::aEventListeners := {}
   /* By default we'll use the text as an identifier */
   ::x = 0
   ::y = 0
   ::bEventsToParent := .T.

RETURN Self


METHOD AddListener( oListener, mthListener ) CLASS XWTWidget
   LOCAL aList

   aList := {0, oListener, mthListener }

   AAdd( ::aEventListeners, aList )
RETURN .T.


METHOD AddEventListener( nType, oListener, mthListener ) CLASS XWTWidget
   LOCAL aList

   aList := {nType, oListener, mthListener }
   AAdd( ::aEventListeners, aList )
RETURN .T.


METHOD RemoveListener( oListener, mthListener ) CLASS XWTWidget
   LOCAL aList, i := 1

   DO WHILE i < Len( ::aEventListeners )
      aList := ::aEventListeners[ i ]
      IF oListener == aList[2]
         IF Empty( mthListener ) .or. mthListener == aList[3]
            Adel( ::aEventListeners, i )
            Asize( ::aEventListeners, Len( ::aEventListeners )-1 )
         ELSE
            i ++
         ENDIF
      ELSE
         i++
      ENDIF
   ENDDO

RETURN .T.


METHOD RiseEvent( oEvent ) CLASS XWTWidget
   LOCAL aList, aCall, bRes

   FOR EACH aList IN ::aEventListeners
      IF aList[1] == 0 .or. oEvent:nType == aList[1]
         IF Empty( aList[3] )
            aCall := { aList[2], oEvent }
         ELSE
            aCall := { aList[2], aList[3], oEvent }
         ENDIF
         bRes := HB_ExecFromArray( aCall )
         IF .not. Empty( bRes ) .and. bRes
            RETURN .T.
         ENDIF
      ENDIF
   NEXT

   /* Back broadcasting */
   IF ::bEventsToParent .and. ::oOwner != NIL
      RETURN ::oOwner:RiseEvent( oEvent )
   ENDIF
RETURN .F.

/*****************************************************/

METHOD Show() CLASS XWTWidget
   LOCAL bRaw

   bRaw:= XWT_FastRiseEvent( XWT_E_SHOW, Self )

   IF .not. bRaw
      bRaw := .not. XWT_SetProperty( ::oRawWidget, XWT_PROP_VISIBILITY, XWT_VIS_NORMAL )
   ENDIF

RETURN .not. bRaw


METHOD Hide() CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( XWT_E_HIDE, Self )

   IF .not. bRaw
      bRaw := .not. XWT_SetProperty( ::oRawWidget, XWT_PROP_VISIBILITY, XWT_VIS_HIDDEN )
   ENDIF

RETURN .not. bRaw


METHOD Move( x, y ) CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( XWT_E_MOVE, Self, x, y )

   IF .not. bRaw
      bRaw := .not. XWT_SetProperty( ::oRawWidget, XWT_PROP_POSITION, x, y )
      ::x := x
      ::y := y
   ENDIF

RETURN .not. bRaw


METHOD Resize( width, height ) CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( XWT_E_RESIZE, Self, width, height )

   IF .not. bRaw
      bRaw := .not. XWT_SetProperty( ::oRawWidget, XWT_PROP_SIZE, width, height )
   ENDIF

RETURN .not. bRaw


METHOD Reposition( x, y, width, height ) CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( XWT_E_MOVE, Self, x, y )

   IF .not. bRaw
      XWT_SetProperty( ::oRawWidget, XWT_PROP_SIZE, width, height )
      bRaw := XWT_FastRiseEvent( XWT_E_RESIZE, Self, width, height )
      IF .not. bRaw
         XWT_SetProperty( ::oRawWidget, XWT_PROP_POSITION, x, y )
         ::x := x
         ::y := y
      ENDIF
   ENDIF
RETURN .not. bRaw


METHOD SetText( cText ) CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( XWT_E_TEXT, Self, cText )

   IF .not. bRaw
      bRaw := .not. XWT_SetProperty( ::oRawWidget, XWT_PROP_TEXT, cText )
   ENDIF

RETURN .not. bRaw


METHOD GetText() CLASS XWTWidget
   LOCAL cText

   IF .not. XWT_GetProperty( ::oRawWidget, XWT_PROP_TEXT, @cText )
      cText := ""
   ENDIF
RETURN cText


METHOD SetFocus CLASS XWTWidget
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_FOCUS, .T. )


METHOD HasFocus CLASS XWTWidget
   LOCAL bRet

   IF .not. XWT_GetProperty( ::oRawWidget, XWT_PROP_FOCUS, @bRet )
      bRet := .F.
   ENDIF
RETURN bRet


METHOD Destroy( ) CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( XWT_E_DESTROY, Self )
   IF .not. bRaw
         XWT_Destroy( ::oRawWidget )
   ENDIF
RETURN .not. bRaw


METHOD SetSensible( bSense ) CLASS XWTWidget

   IF ValType( bSense ) != "L"
      bSense := .T.
   ENDIF

RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_SENSIBLE, bSense )

METHOD IsSensible() CLASS XWTWidget
   LOCAL bSense

   IF .not. XWT_GetProperty( ::oRawWidget, XWT_PROP_SENSIBLE, @bSense )
      bSense := .F.
   ENDIF

RETURN bSense
