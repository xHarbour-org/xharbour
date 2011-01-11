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
//-----------------------------------------------------------------------------------------------

CLASS VrLine INHERIT VrObject
   DATA ClsName EXPORTED INIT "Line"
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
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
