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

CLASS VrTheme INHERIT VrLabel
   DATA ClsName  EXPORTED INIT "Theme"
   DATA lUI      EXPORTED INIT .F.
   DATA Button   EXPORTED
   METHOD Init()  CONSTRUCTOR
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS VrTheme
   LOCAL n
   IF oParent != NIL
      Super:Init( oParent )
      ::aProperties := {}
      AADD( ::aProperties, { "BackColor",  "Color"   } )
      AADD( ::aProperties, { "ForeColor",  "Color"   } )
      AADD( ::aProperties, { "Font",       "General" } )
      AADD( ::aProperties, { "Name",       "Object"  } )
      AADD( ::aProperties, { "AutoResize", "Size"    } )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
CLASS VrTotal INHERIT VrLabel
   DATA lUI     EXPORTED INIT .T.
   DATA ClsName EXPORTED  INIT "Total"
ENDCLASS
