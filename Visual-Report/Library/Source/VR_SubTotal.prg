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

#define  acObjectTypeText           5

CLASS VrSubTotal INHERIT VrLabel
   DATA SubTotalField EXPORTED
   DATA ClsName       EXPORTED  INIT "SubTotal"
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD WriteProps()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS VrSubTotal
   IF oParent != NIL
      Super:Init( oParent )
      AADD( ::aProperties, { "SubTotalField", "Process"  } )
   ENDIF
RETURN Self

METHOD Create() CLASS VrSubTotal
   Super:Create()
RETURN Self

METHOD WriteProps( oXmlControl ) CLASS VrSubTotal
   LOCAL oXmlValue, oXmlFont
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "SubTotalField", NIL, ::SubTotalField )
   oXmlControl:addBelow( oXmlValue )
   Super:WriteProps( oXmlControl )
RETURN Self

