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

CLASS VrDataTable INHERIT VrObject
   DATA FileName         EXPORTED INIT ""
   DATA Alias            EXPORTED INIT ""
   DATA lUI              EXPORTED INIT .F.

   DATA DataTable        EXPORTED
   DATA ClsName          EXPORTED INIT "DataTable"
   DATA SysBackColor     EXPORTED INIT GetSysColor( COLOR_WINDOW )
   DATA SysForeColor     EXPORTED INIT GetSysColor( COLOR_BTNTEXT )

   DATA BackColor        EXPORTED INIT GetSysColor( COLOR_WINDOW )
   DATA ForeColor        EXPORTED INIT GetSysColor( COLOR_BTNTEXT )
   DATA Order            EXPORTED INIT ""
   DATA bFilter          EXPORTED  INIT ""
   DATA __ExplorerFilter EXPORTED  INIT { { "DataTable *.dbf", "*.dbf" } }

   DATA Button           EXPORTED
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS VrDataTable
   IF oParent != NIL
      Super:Init( oParent )
      ::aProperties := {}
      AADD( ::aProperties, { "FileName",  "General"  } )
      AADD( ::aProperties, { "Alias",     "General"  } )
      AADD( ::aProperties, { "bFilter",   "General"  } )
      AADD( ::aProperties, { "Name",      "Object"   } )
      AADD( ::aProperties, { "Order",     "Index"    } )
   ENDIF
RETURN Self

METHOD Create() CLASS VrDataTable

   WITH OBJECT ::EditCtrl := DataTable( ::Parent )
      :FileName := ::FileName
      IF !EMPTY( ::Alias )
         :Alias := ::Alias
      ENDIF
      :Create()
      IF ! EMPTY( ::bFilter )
         :SetFilter( &(::bFilter) )
      ENDIF
      IF ! EMPTY( ::Order )
         :OrdSetFocus( ::Order )
      ENDIF
   END

   Super:Create()
RETURN Self

