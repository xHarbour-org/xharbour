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
#include "sqlrdd.ch"

#define  acObjectTypeText           5

CLASS VrDataTable INHERIT VrObject
   DATA FileName         EXPORTED INIT ""
   DATA Alias            EXPORTED INIT ""
   DATA lUI              EXPORTED INIT .F.
   DATA Driver           EXPORTED INIT ""
   DATA DataTable        EXPORTED
   DATA ClsName          EXPORTED INIT "DataTable"
   DATA SysBackColor     EXPORTED INIT GetSysColor( COLOR_WINDOW )
   DATA SysForeColor     EXPORTED INIT GetSysColor( COLOR_BTNTEXT )
   DATA ConnectionFile   EXPORTED INIT ""
   DATA Server           EXPORTED INIT CONNECT_ODBC
   DATA EnumServer       EXPORTED INIT { { "AutoDetect", "ODBC", "RPC", "MySQL", "Postgres", "Oracle", "Firebird" }, {0,1,2,3,4,5,6} }

   DATA BackColor        EXPORTED INIT GetSysColor( COLOR_WINDOW )
   DATA ForeColor        EXPORTED INIT GetSysColor( COLOR_BTNTEXT )
   DATA Filter           EXPORTED INIT ""
   DATA __ExplorerFilter EXPORTED INIT { { "DataTable *.dbf", "*.dbf" }, { "DataTable *.soc", "*.soc" } }

   DATA Button           EXPORTED

   DATA Order            EXPORTED INIT ""
   DATA RelationTable    EXPORTED INIT ""
   DATA RelationExp      EXPORTED INIT ""

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD WriteProps()
   METHOD Configure()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS VrDataTable
   IF oParent != NIL
      Super:Init( oParent )
      ::aProperties := {}
      AADD( ::aProperties, { "FileName",         "General"  } )
      AADD( ::aProperties, { "Alias",            "General"  } )
      AADD( ::aProperties, { "Filter",           "General"  } )
      AADD( ::aProperties, { "RelationTable",    "Relation" } )
      AADD( ::aProperties, { "RelationExp",      "Relation" } )
      AADD( ::aProperties, { "Name",             "Object"   } )
      AADD( ::aProperties, { "Driver",           "Object"   } )
      AADD( ::aProperties, { "Order",            "Index"    } )
      AADD( ::aProperties, { "Server",           "SQL" } )
      AADD( ::aProperties, { "ConnectionFile",   "SQL" } )
   ENDIF
RETURN Self

METHOD Create() CLASS VrDataTable

   WITH OBJECT ::EditCtrl := DataTable( ::Parent )
      :Cargo    := Self
      :FileName := ::FileName
      :Driver   := ::Driver
      IF !EMPTY( ::Alias )
         :Alias := ::Alias
      ENDIF
      IF :Driver != "SQLRDD"
         :Create()
         IF ! EMPTY( ::Filter )
            :SetFilter( &(::Filter) )
         ENDIF
         IF ! EMPTY( ::Order )
            :OrdSetFocus( ::Order )
         ENDIF
      ENDIF
   END

   Super:Create()
RETURN Self

METHOD Configure() CLASS VrDataTable
   LOCAL cAlias, nCnn, oIni, cEntry
   WITH OBJECT ::EditCtrl
      :xFileName := ::FileName
      :Driver   := ::Driver
      
      IF ::Driver != "SQLRDD"
         IF !EMPTY( ::Alias )
            :Alias := ::Alias
            IF ::EditMode
               :Alias += "_desMode"
            ENDIF
          ELSE
            cAlias := SUBSTR( ::FileName, RAT("\",::FileName)+1 )
            cAlias := SUBSTR( cAlias, 1, RAT(".",cAlias)-1 )
            IF ::EditMode
               :Alias := cAlias + "_desMode"
            ENDIF
         ENDIF

         :Create()
         IF ! EMPTY( ::Filter )
            :SetFilter( &(::Filter) )
         ENDIF
         IF ! EMPTY( ::Order )
            :OrdSetFocus( ::Order )
         ENDIF
      ENDIF
   END
RETURN Self

METHOD WriteProps( oXmlControl ) CLASS VrDataTable
   LOCAL oXmlValue, oXmlFont
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "FileName", NIL, XSTR( ::FileName ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Alias", NIL, XSTR( ::Alias ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Filter", NIL, XSTR( ::Filter ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Order", NIL, XSTR( ::Order ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Driver", NIL, XSTR( ::Driver ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Server", NIL, XSTR( ::Server ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "RelationTable", NIL, ::RelationTable )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "RelationExp", NIL, ::RelationExp )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "ConnectionFile", NIL, ::ConnectionFile )
   oXmlControl:addBelow( oXmlValue )
RETURN Self

#pragma BEGINDUMP
   #pragma comment( lib, "libmysql.lib" )
   #pragma comment( lib, "libpq.lib" )
   #pragma comment( lib, "fbclient_ms.lib" )
#pragma ENDDUMP
