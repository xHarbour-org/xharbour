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
#define LINEHEIGHT 2
#define  acObjectTypeLine           1
//-----------------------------------------------------------------------------------------------

CLASS VrLine INHERIT VrObject
   DATA SysForeColor EXPORTED  INIT RGB(0,0,0)
   DATA ForeColor    PUBLISHED INIT RGB(0,0,0)
   DATA ClsName EXPORTED INIT "Line"
   
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD WriteProps()
   METHOD Draw()
   METHOD Configure()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS VrLine
   Super:Init( oParent )
   AADD( ::aProperties, { "Name",      "Object" } )
   AADD( ::aProperties, { "Width",     "Size"   } )
   AADD( ::aProperties, { "ForeColor", "Color"  } )
RETURN Self

METHOD Create() CLASS VrLine

   IF ::__ClsInst == NIL // Runtime
      RETURN ::Draw()
   ENDIF
   
   WITH OBJECT ::EditCtrl := __VrLabel( IIF( ::Parent:ClsName == "PanelBox", ::Parent, ::Parent:EditCtrl ) )
      :Cargo     := Self
      :Left      := ::Left
      :Top       := ::Top
      :Width     := ::Width
      :Height    := LINEHEIGHT
      :BackColor := RGB(0,0,0)
      :Create()
   END
   Super:Create()
RETURN Self

METHOD WriteProps( oXmlControl ) CLASS VrLine
   LOCAL oXmlValue, oXmlFont
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Left", NIL, XSTR( ::Left ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Top", NIL, XSTR( ::Top ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Width", NIL, XSTR( ::Width ) )
   oXmlControl:addBelow( oXmlValue )
RETURN Self

METHOD Draw( hDC ) CLASS VrLine
   LOCAL nX, nY, x, y, cx, cy, cUnderline, cText, cItalic, cName := "Line" + AllTrim( Str( ::Parent:nLine++ ) )

   nX := GetDeviceCaps( hDC, LOGPIXELSX )
   nY := GetDeviceCaps( hDC, LOGPIXELSY )

   x  := ( ::nPixPerInch / nX ) * ::Left
   y  := ::Parent:nRow + ( ( ::nPixPerInch / nY ) * ::Top )
   cx := ( ::nPixPerInch / nX ) * ::Width
 
   ::Parent:oPDF:CreateObject( acObjectTypeLine, cName )
   ::PDFCtrl := ::Parent:oPDF:GetObjectByName( cName )
   WITH OBJECT ::PDFCtrl
      :Attribute( "Left",   x )
      :Attribute( "Top",    y )
      :Attribute( "Right",  x + cx )
      :Attribute( "Bottom", y + 2 )
      IF ::ForeColor != ::SysForeColor
         :Attribute( "StrokeColor", PADL( DecToHexa( ::ForeColor ), 6, "0" ) )
      ENDIF
   END
RETURN Self

METHOD Configure() CLASS VrLine
   WITH OBJECT ::EditCtrl
      :ForeColor      := ::ForeColor     
      :xLeft          := ::Left
      :xTop           := ::Top
      :xWidth         := ::Width
      :xHeight        := LINEHEIGHT
      :MoveWindow()
   END
RETURN Self
