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

#define  acObjectTypeText           5

CLASS VrFormula INHERIT VrObject
   DATA Value    EXPORTED INIT ""
   DATA ClsName  EXPORTED INIT "Formula"
   DATA Button   EXPORTED
   METHOD Init()  CONSTRUCTOR
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS VrFormula
   IF oParent != NIL
      Super:Init( oParent )
      ::aProperties := {}
      AADD( ::aProperties, { "Value",  "General"  } )
      AADD( ::aProperties, { "Name",   "Object"  } )
   ENDIF
RETURN Self

