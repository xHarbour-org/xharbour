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

CLASS VrGroup INHERIT VrObject
   DATA ClsName       EXPORTED INIT "Group"
   DATA ShowRectangle EXPORTED INIT .T.
   DATA Objects       EXPORTED INIT {}
   DATA GroupBy       EXPORTED

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD WriteProps()
   METHOD Configure()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS VrGroup
   IF oParent != NIL
      Super:Init( oParent )
      ::aProperties := {}
      AADD( ::aProperties, { "GroupBy", "General" } )
      AADD( ::aProperties, { "Top",    "Position" } )
      AADD( ::aProperties, { "Height", "Size"     } )
      AADD( ::aProperties, { "Name",   "Object"   } )
   ENDIF
RETURN Self

METHOD Create() CLASS VrGroup
   IF ::__ClsInst == NIL // Runtime
      RETURN ::Draw()
   ENDIF
   WITH OBJECT ::EditCtrl := __VrGroup( ::Parent )
      //:Caption := ::Text
      :BackColor := ::System:Color:White
      :Cargo   := Self
      :Left    := -1
      :Top     := ::Top
      :Height  := ::Height
      :Create()
      :Width   := ::Parent:Width
   END
   Super:Create()
RETURN Self

METHOD Configure() CLASS VrGroup
   WITH OBJECT ::EditCtrl
      :Width  := ::Parent:Width
      :Height := ::Height
   END
RETURN Self

METHOD WriteProps( oXmlControl ) CLASS VrGroup
   LOCAL oXmlValue, oXmlFont
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Top", NIL, XSTR( ::Top ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Height", NIL, XSTR( ::Height ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "GroupBy", NIL, XSTR( ::GroupBy ) )
   oXmlControl:addBelow( oXmlValue )
RETURN Self

//-----------------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------------

CLASS __VrGroup INHERIT RepEdit
   DATA aSize EXPORTED INIT {.F.,.F.,.F.,.T.,.F.,.F.,.F.,.T.}
   DATA FlatCaption EXPORTED INIT .T.
   DATA Type INIT "Group"
   METHOD OnLButtonDown()
   METHOD OnMouseMove()
   METHOD OnMouseLeave()     INLINE ::Parent:Cursor := NIL, NIL
   METHOD OnKeyDown(n)       INLINE KeyDown( Self, n )
   METHOD OnGetDlgCode()     INLINE DLGC_WANTMESSAGE + DLGC_WANTCHARS + DLGC_WANTARROWS + DLGC_HASSETSEL
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonDown(n,x,y) CLASS __VrGroup 
   LOCAL aRect, oCtrl
   ::Parent:SetCapture()
   IF ::Application:Props:ToolBox:ActiveItem != NIL
      ::Parent:CreateControl( "Vr"+::Application:Props:ToolBox:ActiveItem:Caption, x, y, ::Cargo )
    
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
RETURN NIL

//-----------------------------------------------------------------------------------------------------------------------------------

METHOD OnMouseMove(n,x,y) CLASS __VrGroup 
   LOCAL oCtrl
   IF n == MK_LBUTTON
      oCtrl := ::Application:Props:PropEditor:ActiveObject
      IF !(oCtrl == Self)
         Super:OnMouseMove(n,x,y)
      ENDIF
    ELSE
      MouseMove( Self, n, x, y )
   ENDIF
RETURN NIL

