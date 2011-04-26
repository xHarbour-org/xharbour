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
   DATA ClsName  EXPORTED INIT "Subtotal"
   DATA lUI      EXPORTED INIT .F.
   DATA Button   EXPORTED
   METHOD Init()  CONSTRUCTOR
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS VrSubtotal
   LOCAL n
   IF oParent != NIL
      Super:Init( oParent )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
CLASS VrTotal INHERIT VrSubtotal
   DATA ClsName EXPORTED  INIT "Total"
ENDCLASS
