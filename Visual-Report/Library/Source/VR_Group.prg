/*
 * $Id$
 */

//-----------------------------------------------------------------------------------------------
// Copyright   WinFakt! / SOCS BVBA  http://www.WinFakt.com
//
// This source file is an intellectual property of SOCS bvba.
// You may NOT forward or share this file under any conditions!
//-----------------------------------------------------------------------------------------------

#include "debug.ch"
#include "vxh.ch"
#include "hbxml.ch"

#define  acObjectTypeFrame          2

CLASS VrGroup INHERIT VrObject, GroupBox
   DATA ForeColor     EXPORTED INIT GetSysColor( COLOR_BTNTEXT )
   DATA ClsName       EXPORTED INIT "Group"
   DATA ShowRectangle EXPORTED INIT .T.
   DATA Objects       EXPORTED INIT {}

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD Draw()
   METHOD WriteProps()
   METHOD Configure()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS VrGroup
   IF oParent != NIL
      Super:Init( oParent )
      AADD( ::aProperties, { "ForeColor",  "Color"   } )
      AADD( ::aProperties, { "Width",  "Size"  } )
      AADD( ::aProperties, { "Height", "Size" } )
   ENDIF
RETURN Self

METHOD Create() CLASS VrGroup
   IF ::__ClsInst == NIL // Runtime
      RETURN ::Draw()
   ENDIF
   WITH OBJECT ::EditCtrl := __VrGroup( ::Parent )
      :Cargo   := Self
      :Caption := ::Text
      :Left    := ::Left
      :Top     := ::Top
      :Create()
   END
   Super:Create()
RETURN Self

METHOD Configure() CLASS VrGroup
   IF ::lUI
      WITH OBJECT ::EditCtrl
         :ForeColor := ::ForeColor     
         :Width     := ::Width
         :Height    := ::Height
      END
   ENDIF
RETURN Self

METHOD WriteProps( oXmlControl ) CLASS VrGroup
   LOCAL oXmlValue, oXmlFont
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "ForeColor", NIL, XSTR( ::ForeColor ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Left", NIL, XSTR( ::Left ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Top", NIL, XSTR( ::Top ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Width", NIL, XSTR( ::Width ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Height", NIL, XSTR( ::Height ) )
   oXmlControl:addBelow( oXmlValue )
RETURN Self

METHOD Draw( hDC ) CLASS VrGroup
   LOCAL x, y, cName, cx, cy, nX, nY
   IF ::ShowRectangle
      cName := "Group" + AllTrim( Str( ::Parent:nText++ ) )
      
      nX := GetDeviceCaps( hDC, LOGPIXELSX )
      nY := GetDeviceCaps( hDC, LOGPIXELSY )

      x  := ( ::nPixPerInch / nX ) * ::Left
      y  := ::Parent:nRow + ( ( ::nPixPerInch / nY ) * ::Top )
      cx := ( ::nPixPerInch / nX ) * ::Width
      cy := ( ::nPixPerInch / nY ) * ::Height

      ::Parent:oPDF:CreateObject( acObjectTypeFrame, cName )

      WITH OBJECT ::PDFCtrl := ::Parent:oPDF:GetObjectByName( cName )
         :Attribute( "Left",    x )
         :Attribute( "Top",     y )
         :Attribute( "Right",   x+cx )
         :Attribute( "Bottom",  y+cy )

         :Attribute( "TextColor", PADL( DecToHexa( ::ForeColor ), 6, "0" ) )
      END
   ENDIF
RETURN Self

CLASS __VrGroup INHERIT GroupBox
   DATA aSize EXPORTED INIT {.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.}
   METHOD OnLButtonDown()
   METHOD OnMouseMove(n,x,y) INLINE MouseMove( Self, n, x, y )
   METHOD OnMouseLeave()     INLINE ::Parent:Cursor := NIL, NIL
   METHOD OnKeyDown(n)       INLINE KeyDown( Self, n )
   METHOD OnGetDlgCode()     INLINE DLGC_WANTMESSAGE + DLGC_WANTCHARS + DLGC_WANTARROWS + DLGC_HASSETSEL
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonDown(n,x,y) CLASS __VrGroup 
   LOCAL aRect, oCtrl
   ::Parent:SetCapture()

   IF ::Application:Props:ToolBox:ActiveItem != NIL
      ::Parent:CreateControl( "Vr"+::Application:Props:ToolBox:ActiveItem:Caption, x, y, Self )
    
    ELSEIF ::Application:Props:PropEditor:ActiveObject != NIL
      oCtrl := ::Application:Props:PropEditor:ActiveObject:EditCtrl
      TRY
         IF oCtrl != NIL
            aRect := oCtrl:GetRectangle()
            aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
            oCtrl:Parent:InvalidateRect( aRect, .F. )
            aRect := ::GetRectangle()
            aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
            ::Parent:InvalidateRect( aRect, .F. )
            ::Parent:nDownPos := {x,y}
         ENDIF
      CATCH
      END
      ::SetFocus()
   ENDIF
   Super:OnLButtonDown()
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------------
