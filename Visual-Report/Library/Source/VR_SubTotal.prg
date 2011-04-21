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
   DATA ClsName EXPORTED  INIT "Subtotal"
   METHOD Init()  CONSTRUCTOR
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS VrSubtotal
   LOCAL n
   IF oParent != NIL
      Super:Init( oParent )
      IF ( n := ASCAN( ::aProperties, {|a| a[1]=="Text"} ) ) > 0
         ADEL( ::aProperties, n, .T. )
      ENDIF
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
CLASS VrTotal INHERIT VrSubtotal
   DATA ClsName EXPORTED  INIT "Total"
ENDCLASS
