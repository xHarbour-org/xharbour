/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: widget.prg,v 1.2 2004/05/17 09:27:11 jonnymind Exp $

   Widget class - basic widget & event management
*/

#include "hbclass.ch"

Extern XwtEvent

CLASS XWTWidget
   /** Parses styles in format NAME: VALUE; */
   CLASSDATA cRegexStyle   INIT HB_RegexComp('\s*([^:]+):\s*(("?)(.*?)\3)\s*(;|$)')

   /** Pointer to internal data used by current subsystem */
   DATA oRawWidget

   /** Owner of this object */
   DATA oOwner

   METHOD New( xStyle )         CONSTRUCTOR
   //METHOD GetPosition( x, y, width, height )

   METHOD Show()
   METHOD Hide()

   METHOD Move( x, y )
   METHOD Resize( width, height )
   METHOD Reposition( x, y, width, height )

   METHOD SetFocus()
   METHOD HasFocus()

   METHOD SetText( cText )      INLINE   ::setProperty( "text", cText )
   METHOD GetText()             INLINE   ::getProperty( "text" )

   METHOD GetType()        INLINE   ::nWidgetType

   METHOD AddListener( oListener, mthListener )
   METHOD AddEventListener( cType, oListener, mthListener )
   METHOD RemoveListener( oListener, mthListener )

   METHOD RiseEvent( oEvent )
   METHOD RiseOneEvent( aListeners, oEvent )

   // cProperty may be a hash with a set of properties to set.
   METHOD SetProperty( cProperty, xValue )    INLINE XWT_SetProperty( ::oRawWidget, cProperty, xValue )
   METHOD SetStyle( cStr )
   METHOD GetProperty( cProperty )            INLINE XWT_GetProperty( ::oRawWidget, cProperty )
   METHOD GetProperties()                     INLINE XWT_GetAllProperties( ::oRawWidget )
   METHOD GetStyle()

   METHOD SetSensible( bSense )
   METHOD IsSensible()

PROTECTED:
   DATA nWidgetType

   /** Objects that are listening for this object events */
   DATA hEventListeners

ENDCLASS

/** Standard properties:
   fgcolor
   bgcolor
   textcolor
   sensibility
   visibility
   x
   y
   width
   height
   id
*/


METHOD New( xStyle ) CLASS XWTWidget
   /* IMPORTANT! oRawWidget MUST be initialized by subclasses */
   ::hEventListeners := { "all" => {} }
   IF ::oRawWidget != NIL .and. ! Empty( xStyle )
      IF ValType( xStyle ) == "H"
         ::SetProperty( xStyle )
      ELSEIF ValType( xStyle ) == "C"
         ::SetStyle( xStyle )
      ENDIF
   ENDIF

RETURN Self


METHOD AddListener( oListener, mthListener ) CLASS XWTWidget
   LOCAL aList

   aList := { oListener, mthListener }
   AAdd( ::hEventListeners["all"], aList )
RETURN .T.


METHOD AddEventListener( cType, oListener, mthListener ) CLASS XWTWidget
   LOCAL aList, nPos, aPrevList

   aList := { oListener, mthListener }
   nPos := HGetPos( ::hEventListeners, cType )
   IF nPos == 0
      ::hEventListeners[ cType ] := { aList }
  ELSE
      aPrevList := HGetValueAt( ::hEventListeners, nPos )
      ASize( aPrevList, Len( aPrevList ) + 1)
      AIns( aPrevList , 1 )
      aPrevList[1] := aList
   ENDIF

RETURN .T.


METHOD RemoveListener( oListener, mthListener ) CLASS XWTWidget
   LOCAL aList, i := 1, j

   DO WHILE i < Len( ::hEventListeners )
      aList := HGetValueAt( ::hEventListeners, i )
      j := 1
      DO WHILE j < Len( aList )
         IF oListener == aList[j][1]
            IF Empty( mthListener ) .or. mthListener == aList[j][2]
               Adel( aList, j )
               Asize( aList, Len( aList )-1 )
            ELSE
               j ++
            ENDIF
         ELSE
            j++
         ENDIF
      ENDDO
      i++
   ENDDO

RETURN .T.


METHOD RiseEvent( oEvent ) CLASS XWTWidget
   LOCAL nPos

   nPos := HGetPos( ::hEventListeners, oEvent:cType )
   IF nPos != 0
      IF ::RiseOneEvent( HGetValueAt( ::hEventListeners, nPos ), oEvent )
         RETURN .T.
      ENDIF
   ENDIF

   nPos := HGetPos( ::hEventListeners, "all" )
   IF nPos != 0
      IF ::RiseOneEvent( HGetValueAt( ::hEventListeners, nPos ) , oEvent )
         RETURN .T.
      ENDIF
   ENDIF

   /* Back broadcasting */
   IF ::oOwner != NIL .and. ::GetProperty( "broadcast" )
      RETURN ::oOwner:RiseEvent( oEvent )
   ENDIF

   IF .not. ::GetProperty( "broadcast" )
      ? "Not Broadcast:", ::getProperty("id"), ::getProperty("text")
   ENDIF
RETURN .F.

METHOD RiseOneEvent( aListeners, oEvent ) CLASS XWTWidget
   LOCAL aList, aCall, bRes

   FOR EACH aList IN aListeners
      IF Empty( aList[2] )
         aCall := { aList[1], oEvent }
      ELSE
         aCall := { aList[1], aList[2], oEvent }
      ENDIF
      bRes := HB_ExecFromArray( aCall )
      IF ValType( bRes ) == "L" .and. bRes
         RETURN .T.
      ENDIF
   NEXT

RETURN .F.

/*****************************************************/

METHOD SetStyle( cRequest ) CLASS XWTWidget
   LOCAL aMatch, nValue
   LOCAL hProps := Hash()

   cRequest := AllTrim( cRequest )

   DO WHILE Len( cRequest ) > 0
      aMatch := HB_Regex( ::cRegexStyle, cRequest )
      IF aMatch == NIL
         // MALFORMED: Signal error
         RETURN .F.
      ELSE
         nValue := Val( aMatch[5] )
         IF (nValue != 0 .or. aMatch[5][1] == "0") .and. Len(AllTrim( Str( nValue) ) ) == Len( aMatch[5])
            hProps[ aMatch[2] ] := nValue
         ELSEIF aMatch[5] == ".T."
            hProps[ aMatch[2] ] := .T.
         ELSEIF aMatch[5] == ".F."
            hProps[ aMatch[2] ] := .F.
         ELSE
            hProps[ aMatch[2] ] := aMatch[5]
         ENDIF
         cRequest := Substr( cRequest, Len( aMatch[1] ) + 1)
      ENDIF
   ENDDO

   IF Len( hProps ) > 0
      RETURN XWT_SetProperty( ::oRawWidget, hProps )
   ENDIF
RETURN .F.

METHOD GetStyle() CLASS XWTWidget
   LOCAL hProps, cProps, nPos
   LOCAL cKey, xValue

   cProps := ""
   hProps := ::GetProperties()
   IF Len( hProps ) == 0
      RETURN ""
   ENDIF

   FOR nPos := 1 TO Len( hProps )
      cKey := HGetKeyAt( hProps, nPos )
      xValue := HGetValueAt( hProps, nPos )
      IF ValType(xValue) == "C"
         xValue := StrTran( xValue, '"', '\"' )
         IF At( " ", xValue ) != 0 .or. At( ";", xValue ) != 0
            xValue := '"' + xValue + '"'
         ENDIF
      ELSE
         xValue := Alltrim( CStr( xValue ) )
      ENDIF
      cProps += cKey + ": " + xValue + "; "
   NEXT

RETURN Substr( cProps, 1, Len( cProps ) -2 )

METHOD Show() CLASS XWTWidget
   LOCAL bRaw

   bRaw:= XWT_FastRiseEvent( "show", Self )

   IF .not. bRaw
      bRaw := .not. XWT_SetProperty( ::oRawWidget, "visibility", "normal" )
   ENDIF

RETURN .not. bRaw


METHOD Hide() CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( "hide", Self )

   IF .not. bRaw
      bRaw := .not. XWT_SetProperty( ::oRawWidget, "visibility", "hidden" )
   ENDIF

RETURN .not. bRaw


METHOD Move( x, y ) CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( "move", Self, x, y )
   IF .not. bRaw
      bRaw := .not. XWT_SetProperty( ::oRawWidget, { "x" => x, "y" => y } )
   ENDIF

RETURN .not. bRaw


METHOD Resize( width, height ) CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( "resize", Self, width, height )

   IF .not. bRaw
      bRaw := .not. XWT_SetProperty( ::oRawWidget, {"width" => width, "height" => height } )
   ENDIF

RETURN .not. bRaw


METHOD Reposition( x, y, width, height ) CLASS XWTWidget
   LOCAL bRaw := XWT_FastRiseEvent( "move", Self, x, y )

   IF .not. bRaw
      bRaw := XWT_FastRiseEvent( "resize", Self, width, height )
      IF .not. bRaw
         XWT_SetProperty( ::oRawWidget, { "x" => x, "y" => y, "width" => width, "height" => height } )
      ENDIF
   ENDIF
RETURN .not. bRaw


METHOD SetFocus CLASS XWTWidget
RETURN XWT_SetProperty( ::oRawWidget, "focus", .T. )


METHOD HasFocus CLASS XWTWidget
   LOCAL bRet

   bRet := XWT_GetProperty( ::oRawWidget, "focus" )
   IF bRet == NIL
      bRet := .F.
   ENDIF
RETURN bRet


METHOD SetSensible( bSense ) CLASS XWTWidget

   IF ValType( bSense ) != "L"
      bSense := .T.
   ENDIF

RETURN XWT_SetProperty( ::oRawWidget, "sensible", bSense )

METHOD IsSensible() CLASS XWTWidget
   LOCAL bSense, bRet

   bRet := XWT_GetProperty( ::oRawWidget, "sensible" )
   IF bRet == NIL
      bRet := .F.
   ENDIF

RETURN bSense

