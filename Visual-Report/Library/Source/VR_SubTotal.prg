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

CLASS VrSubtotal INHERIT VrLabel
   DATA OnLabel EXPORTED INIT ""
   DATA ClsName EXPORTED  INIT "Subtotal"
   METHOD Init()  CONSTRUCTOR
   METHOD WriteProps()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS VrSubtotal
   IF oParent != NIL
      Super:Init( oParent )
      AADD( ::aProperties, { "OnLabel", "Process"  } )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD WriteProps( oXmlControl ) CLASS VrSubtotal
   LOCAL oXmlValue, oXmlFont
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "OnLabel", NIL, ::OnLabel )
   oXmlControl:addBelow( oXmlValue )
   Super:WriteProps( oXmlControl )
RETURN Self

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
CLASS VrTotal INHERIT VrSubtotal
   DATA ClsName EXPORTED  INIT "Total"
ENDCLASS
