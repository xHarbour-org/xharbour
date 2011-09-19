/*
 * $Id$
 */

#include "debug.ch"
#include "vxh.ch"
#include "colors.ch"
#include "sqlrdd.ch"

//-------------------------------------------------------------------------------------------------------
CLASS SqlConnector INHERIT Component
   DATA ConnectionString PUBLISHED
   DATA AutoConnect      PUBLISHED INIT .T.
   
   DATA Server           PUBLISHED INIT CONNECT_ODBC
   
   DATA Sql              EXPORTED
   DATA ConnectionID     EXPORTED          // to be used as dbUseArea() parameter
   DATA Connected        EXPORTED INIT .F.


   // Private use
   DATA EnumServer       EXPORTED INIT { { "AutoDetect", "ODBC", "RPC", "MySQL", "Postgres", "Oracle", "Firebird" }, {0,1,2,3,4,5,6} }
   DATA aIncLibs         EXPORTED INIT   { NIL, NIL, NIL, "libmysql.lib", "libpq", "oci", "fbclient_ms.lib" }
   DATA Events           EXPORTED INIT {  {"General", { { "OnConnect"     , "", "" },;
                                                        { "OnDisconnect" , "", "" } } } }

   METHOD Init() CONSTRUCTOR
   METHOD Connect( cConnString )
   METHOD Disconnect()
   METHOD Create()
   METHOD Commit()
   METHOD RollBack()
   METHOD Execute( cCommand )
   METHOD Exec( cCommand, lMsg, lFetch, aArray, cFile, cAlias, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate )
   METHOD Fetch( aLine )
   METHOD FieldGet( nField, aField, cFromWhere, nFieldJoin, nHandle, lTranslate )
   METHOD Getline( aFields, lTranslate, nHandle, nStart )
   METHOD GetStruct( cTable )
   METHOD BuildString()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS SqlConnector
   DEFAULT oOwner TO ::Application
   ::Connected     := .F.
   ::ConnectionID  := 0
   ::__xCtrlName   := "SqlConnector"
   ::ClsName       := "SqlConnector"
   ::ComponentType := "SqlConnector"
   ::lCreated      := .T.
   ::Super:Init( oOwner )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create() CLASS SqlConnector
   LOCAL cStr, cLib
   IF ::__ClassInst != NIL
      IF ::Server > 0
         cStr := ::aIncLibs[ ::Server + 1 ]
         IF cStr != NIL .AND. ASCAN( ::Application:Project:__ExtraLibs, cStr,,, .T. ) == 0
            AADD( ::Application:Project:__ExtraLibs, cStr )
         ENDIF
       ELSE
         FOR EACH cLib IN ::aIncLibs
            IF cLib != NIL .AND. ASCAN( ::Application:Project:__ExtraLibs, cLib,,, .T. ) == 0
               AADD( ::Application:Project:__ExtraLibs, cLib )
            ENDIF
         NEXT
      ENDIF
   ENDIF
   IF ::AutoConnect .AND. ::ConnectionString != NIL
      ::Connect()
   ENDIF
RETURN Self


//-------------------------------------------------------------------------------------------------------
METHOD BuildString() CLASS SqlConnector
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Connect( cConnString ) CLASS SqlConnector
   LOCAL nCnn, cEvent, nServer

   DEFAULT cConnString TO ::ConnectionString
   
   If valtype( ::Sql ) == "O"         // reconnect ?
      ::Sql:End()
   EndIf

   ::Connected     := .F.
   ::ConnectionID  := 0
   ::Sql           := NIL
   
   IF VALTYPE( ::Server ) == "C"
      ::Server := ASCAN( ::EnumServer[1], {|c| UPPER(c)==UPPER(::Server)} )
   ENDIF
   nServer := ::Server
   IF nServer == 0
      nServer := DetectDBFromDSN( cConnString )
   ENDIF
   
   IF ::__ClassInst == NIL .AND. ( nCnn := SR_AddConnection( nServer, cConnString ) ) > 0 
      ::Connected     := .T.
      ::ConnectionID  := nCnn
      ::Sql           := SR_GetConnection( nCnn )

      IF HGetPos( ::EventHandler, "OnConnect" ) != 0
         cEvent := ::EventHandler[ "OnConnect" ]
         IF __objHasMsg( ::Form, cEvent )
            ::Form:&cEvent( Self )
         ENDIF
      ENDIF
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Disconnect() CLASS SqlConnector
   If valtype( ::Sql ) == "O"         // reconnect ?
      ::Sql:End()
   EndIf

   ::Connected     := .F.
   ::ConnectionID  := 0
   ::Sql           := NIL
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Commit() CLASS SqlConnector
   IF ::Connected
      ::Sql:Commit()
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD RollBack() CLASS SqlConnector
   If ::Connected
      ::Sql:RollBack()
   EndIf
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Execute( cCommand ) CLASS SqlConnector
   If ::Connected
      ::Sql:Execute( cCommand )
   EndIf
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Exec( cCommand, lMsg, lFetch, aArray, cFile, cAlias, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate ) CLASS SqlConnector
   If ::Connected
      ::Sql:Exec( cCommand, lMsg, lFetch, aArray, cFile, cAlias, nMaxRecords, lNoRecno, cRecnoName, cDeletedName, lTranslate )
   EndIf
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Fetch( aLine ) CLASS SqlConnector
   If ::Connected
      ::Sql:Fetch( aLine )
   EndIf
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD FieldGet( nField, aField, cFromWhere, nFieldJoin, nHandle, lTranslate ) CLASS SqlConnector
   If ::Connected
      ::Sql:FieldGet( nField, aField, cFromWhere, nFieldJoin, nHandle, lTranslate )
   EndIf
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Getline( aFields, lTranslate, nHandle, nStart ) CLASS SqlConnector
   If ::Connected
      ::Sql:Getline( aFields, lTranslate, nHandle, nStart )
   EndIf
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD GetStruct( cTable ) CLASS SqlConnector
   If ::Connected
      ::Sql:GetStruct( cTable )
   EndIf
RETURN Self

//-------------------------------------------------------------------------------------------------------

CLASS BindingSource INHERIT SqlConnector
ENDCLASS

CLASS DataConnector INHERIT SqlConnector
ENDCLASS