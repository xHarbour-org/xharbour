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
//-----------------------------------------------------------------------------------------------

CLASS VrLine INHERIT VrObject
   DATA ClsName EXPORTED INIT "Line"
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD WriteProps()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS VrLine
   ::Width := 200
   Super:Init( oParent )
   AADD( ::aProperties, { "Name",   "Object" } )
   AADD( ::aProperties, { "Width",  "Size"   } )
RETURN Self

METHOD Create() CLASS VrLine

   IF ::__ClsInst == NIL // Runtime
      RETURN ::Draw()
   ENDIF
   
   WITH OBJECT ::EditCtrl := __VrLabel( ::Parent )
      :Left      := ::Left
      :Top       := ::Top
      :Width     := ::Width
      :Height    := 2
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
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Width", NIL, XSTR( ::Left ) )
   oXmlControl:addBelow( oXmlValue )
RETURN Self

