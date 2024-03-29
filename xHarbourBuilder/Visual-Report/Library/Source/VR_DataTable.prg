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
   DATA Filter           EXPORTED
   DATA __ExplorerFilter EXPORTED INIT { { "DataTable *.dbf", "*.dbf" }, { "DataTable *.soc", "*.soc" } }

   DATA Button           EXPORTED

   DATA Order            EXPORTED INIT ""
   DATA Relation         EXPORTED INIT ""
   DATA RelationTable    EXPORTED INIT ""

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
      AADD( ::aProperties, { "Relation",         "Relation" } )
      AADD( ::aProperties, { "Name",             "Object"   } )
      AADD( ::aProperties, { "Driver",           "Object"   } )
      AADD( ::aProperties, { "Order",            "Index"    } )
      AADD( ::aProperties, { "Server",           "SQL" } )
      AADD( ::aProperties, { "ConnectionFile",   "SQL" } )
   ENDIF
RETURN Self

METHOD Create( lSuper ) CLASS VrDataTable
   DEFAULT lSuper TO .T.
   IF ::EditCtrl == NIL
      WITH OBJECT ::EditCtrl := DataTable( ::Parent )
         :Cargo    := Self
         :FileName := ::FileName
         :Driver   := ::Driver
         IF !EMPTY( ::Alias )
            :Alias := ::Alias
         ENDIF
         IF :Driver != "SQLRDD"
            :Create()
            IF ! EMPTY( ::Order )
               :OrdSetFocus( ::Order )
            ENDIF
         ENDIF
      END
   ENDIF
   IF lSuper
      Super:Create()
   ENDIF
RETURN Self

METHOD Configure() CLASS VrDataTable
   LOCAL cAlias, e

   WITH OBJECT ::EditCtrl
      :FileName := ::FileName
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
         IF ! FILE( ::FileName )
            MessageBox( 0, "File not found", ::FileName )
          ELSE

             TRY
                :Create()
             CATCH e
                MessageBox( GetActiveWindow(), "An error has ocurred opening the main file please check the correct driver has been set for this file" + CRLF + CRLF +;
                                                                                                                      "Description: " + e:Description + CRLF +;
                                                                                                                      "Operation: " + e:Operation + CRLF +;
                                                                                                                      "Code: " + xStr(e:GenCode) + CRLF +;
                                                                                                                      "SubCode: " + xStr(e:SubCode), "Error Opening " + ::FileName )
                RETURN .F.
             END

         ENDIF
      ENDIF
   END
RETURN Self

METHOD WriteProps( oControl ) CLASS VrDataTable
   LOCAL oValue, oFilter, oExp, n, oAsk
   oValue := TXmlNode():new( HBXML_TYPE_TAG, "FileName", NIL, ::FileName )
   oControl:addBelow( oValue )
   oValue := TXmlNode():new( HBXML_TYPE_TAG, "Alias", NIL, ::Alias )
   oControl:addBelow( oValue )
   oValue := TXmlNode():new( HBXML_TYPE_TAG, "Order", NIL, ::Order )
   oControl:addBelow( oValue )
   oValue := TXmlNode():new( HBXML_TYPE_TAG, "Driver", NIL, ::Driver )
   oControl:addBelow( oValue )
   oValue := TXmlNode():new( HBXML_TYPE_TAG, "Server", NIL, XSTR(::Server) )
   oControl:addBelow( oValue )
   oValue := TXmlNode():new( HBXML_TYPE_TAG, "Relation", NIL, ::Relation )
   oControl:addBelow( oValue )
   oValue := TXmlNode():new( HBXML_TYPE_TAG, "ConnectionFile", NIL, ::ConnectionFile )
   oControl:addBelow( oValue )
   IF VALTYPE( ::Filter ) == "H"
      oFilter := TXmlNode():new( , "Filter" )
         oValue := TXmlNode():new( HBXML_TYPE_TAG, "ANDRadio", NIL, ::Filter:ANDRadio )
         oFilter:addBelow( oValue )
         FOR n := 1 TO LEN( ::Filter:Expressions )
             oExp := TXmlNode():new( , "Expression"+XSTR(n) )
                oValue := TXmlNode():new( HBXML_TYPE_TAG, "AndOr", NIL, ::Filter:Expressions[n]:AndOr )
                oExp:addBelow( oValue )
                oValue := TXmlNode():new( HBXML_TYPE_TAG, "Field", NIL, ::Filter:Expressions[n]:Field )
                oExp:addBelow( oValue )
                oValue := TXmlNode():new( HBXML_TYPE_TAG, "FieldSel", NIL, XSTR( ::Filter:Expressions[n]:FieldSel ) )
                oExp:addBelow( oValue )
                oValue := TXmlNode():new( HBXML_TYPE_TAG, "ExpSel", NIL, XSTR( ::Filter:Expressions[n]:ExpSel ) )
                oExp:addBelow( oValue )
                oValue := TXmlNode():new( HBXML_TYPE_TAG, "FieldType", NIL, ::Filter:Expressions[n]:FieldType )
                oExp:addBelow( oValue )
                oValue := TXmlNode():new( HBXML_TYPE_TAG, "Exp1", NIL, ::Filter:Expressions[n]:Exp1 )
                oExp:addBelow( oValue )
                oValue := TXmlNode():new( HBXML_TYPE_TAG, "Exp2", NIL, ::Filter:Expressions[n]:Exp2 )
                oExp:addBelow( oValue )
                IF HGetPos( ::Filter:Expressions[n], "AskMeLater" ) > 0  .AND. ::Filter:Expressions[n]:AskMeLater != NIL
                   oAsk := TXmlNode():new( , "AskMeLater" )
                      oValue := TXmlNode():new( HBXML_TYPE_TAG, "Title", NIL, ::Filter:Expressions[n]:AskMeLater:Title )
                      oAsk:addBelow( oValue )
                      oValue := TXmlNode():new( HBXML_TYPE_TAG, "GroupText", NIL, ::Filter:Expressions[n]:AskMeLater:GroupText )
                      oAsk:addBelow( oValue )
                      oValue := TXmlNode():new( HBXML_TYPE_TAG, "Search", NIL, ::Filter:Expressions[n]:AskMeLater:Search )
                      oAsk:addBelow( oValue )
                   oExp:addBelow( oAsk )
                ENDIF
             oFilter:addBelow( oExp )
         NEXT
      oControl:addBelow( oFilter )
   ENDIF
RETURN Self

CLASS VrAdsDataTable INHERIT VrDataTable
   DATA ClsName          EXPORTED INIT "AdsDataTable"
   DATA __ExplorerFilter EXPORTED INIT { { "AdsDataTable *.adt", "*.adt;*.dbf" } }
   METHOD Create()
   METHOD Configure()
ENDCLASS

METHOD Create( lSuper ) CLASS VrAdsDataTable
   DEFAULT lSuper TO .T.
   WITH OBJECT ::EditCtrl := AdsDataTable( ::Parent )
      :Cargo    := Self
      :FileName := ::FileName
      :Driver   := ::Driver
      IF !EMPTY( ::Alias )
         :Alias := ::Alias
      ENDIF
      :Create()
      IF ! EMPTY( ::Order )
         :OrdSetFocus( ::Order )
      ENDIF
   END
   Super:Create( lSuper )
RETURN Self

METHOD Configure() CLASS VrAdsDataTable
   LOCAL cAlias, e

   WITH OBJECT ::EditCtrl
      :FileName := ::FileName
      :Driver   := ::Driver

      IF !EMPTY( ::Alias )
         :Alias := ::Alias
         IF ::EditMode
            :Alias += "_des"
         ENDIF
       ELSE
         cAlias := SUBSTR( ::FileName, RAT("\",::FileName)+1 )
         cAlias := SUBSTR( cAlias, 1, RAT(".",cAlias)-1 )
         IF ::EditMode
            :Alias := cAlias + "1"
         ENDIF
      ENDIF
      IF ! FILE( ::FileName )
         MessageBox( 0, "File not found", ::FileName )
       ELSE
          TRY
             :Create()
          CATCH e
             MessageBox( GetActiveWindow(), "An error has ocurred opening the main file please check the correct driver has been set for this file" + CRLF + CRLF +;
                                                                                                                   "Description: " + e:Description + CRLF +;
                                                                                                                   "Operation: " + e:Operation + CRLF +;
                                                                                                                   "Code: " + xStr(e:GenCode) + CRLF +;
                                                                                                                   "SubCode: " + xStr(e:SubCode), "Error Opening " + ::FileName )
             RETURN .F.
          END

      ENDIF
   END
RETURN Self

#pragma BEGINDUMP
   #pragma comment( lib, "libmysql.lib" )
   #pragma comment( lib, "libpq.lib" )
   #pragma comment( lib, "fbclient_ms.lib" )
#pragma ENDDUMP
