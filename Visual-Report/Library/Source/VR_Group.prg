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

CLASS VrGroupHeader INHERIT VrObject
   DATA ClsName       EXPORTED INIT "GroupHeader"
   DATA BackColor     EXPORTED
   DATA Objects       EXPORTED INIT {}
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD WriteProps()
   METHOD Configure()
   METHOD Draw()
   METHOD InvalidateRect(a,l) INLINE ::EditCtrl:InvalidateRect(a,l)
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS VrGroupHeader
   ::Height := 20
   ::BackColor := ::System:Color:Wheat
   IF oParent != NIL
      Super:Init( oParent )
      ::aProperties := {}
      AADD( ::aProperties, { "Top",    "Position" } )
      AADD( ::aProperties, { "Height", "Size"     } )
      AADD( ::aProperties, { "Name",   "Object"   } )
   ENDIF
RETURN Self

METHOD Create() CLASS VrGroupHeader
   IF ::__ClsInst == NIL // Runtime
      RETURN ::Draw()
   ENDIF
   #ifndef VRDLL
      WITH OBJECT ::EditCtrl := __VrGroup( ::Parent )
         :BackColor := ::BackColor
         :Cargo     := Self
         :Left      := -1
         :Top       := ::Top
         :Height    := ::Height
         :Create()
         :Width     := ::Parent:Width
      END
      Super:Create()
   #endif
RETURN Self

METHOD Configure() CLASS VrGroupHeader
   WITH OBJECT ::EditCtrl
      :Width  := ::Parent:Width
      :Height := ::Height
   END
RETURN Self

METHOD WriteProps( oXmlControl ) CLASS VrGroupHeader
   LOCAL oXmlValue, oXmlFont
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Top", NIL, XSTR( ::Top ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Height", NIL, XSTR( ::Height ) )
   oXmlControl:addBelow( oXmlValue )
RETURN Self

METHOD Draw( hDC ) CLASS VrGroupHeader
   LOCAL oLabel
   FOR EACH oLabel IN ::Objects
   NEXT
RETURN Self

//-----------------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------------------------------------

CLASS VrGroupFooter INHERIT VrGroupHeader
   DATA ClsName EXPORTED INIT "GroupFooter"
   METHOD Init() CONSTRUCTOR
ENDCLASS

METHOD Init( oParent ) CLASS VrGroupFooter
   Super:Init( oParent )
   ::BackColor := ::System:Color:LtSteelBlue
RETURN Self