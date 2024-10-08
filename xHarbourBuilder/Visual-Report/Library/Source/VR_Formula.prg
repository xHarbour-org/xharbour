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

CLASS VrFormula INHERIT VrObject
   DATA Value    EXPORTED INIT ""
   DATA ClsName  EXPORTED INIT "Formula"
   DATA lUI      EXPORTED INIT .F.
   DATA Button   EXPORTED
   METHOD Init()  CONSTRUCTOR
   METHOD WriteProps()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS VrFormula
   IF oParent != NIL
      Super:Init( oParent )
      ::aProperties := {}
      AADD( ::aProperties, { "Value", "General"  } )
      AADD( ::aProperties, { "Name",   "Object"  } )
   ENDIF
RETURN Self

METHOD WriteProps( oXmlControl ) CLASS VrFormula
   LOCAL oXmlValue
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Value", NIL, ::Value )
   oXmlControl:addBelow( oXmlValue )
RETURN Self

