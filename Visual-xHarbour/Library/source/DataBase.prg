/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// DataBase.prg                                                                                         *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"
#include "colors.ch"

#include "ord.ch"

#define EF_NONE                         0
#define EF_CANRETRY                     1
#define EF_CANSUBSTITUTE                2
#define EF_CANDEFAULT                   4

#define COMPILE(c) &("{||" + c + "}")

REQUEST HB_MEMIO

static nMemSel := 100

//-------------------------------------------------------------------------------------------------------

CLASS DataTable INHERIT Component

   PROPERTY Alias        ROOT "General"    SET ::__SetAlias(@v)
   PROPERTY MemoType     ROOT "General"    SET ::__SetMemoType(v) DEFAULT RddInfo( RDDI_MEMOTYPE )
   PROPERTY Shared       ROOT "General"                           DEFAULT .T.
   PROPERTY CodePage     ROOT "General"
   PROPERTY Socket       ROOT "General"
   PROPERTY Structure    ROOT "General"
   PROPERTY Table        ROOT "General"                           DEFAULT {}
   PROPERTY Driver       ROOT "General"    SET ::xDriver := IIF( !EMPTY( v ), v, NIL )

   PROPERTY FileName     ROOT "Path"       SET ::SetFileName(@v)
   PROPERTY Path         ROOT "Path"       DEFAULT ""

   PROPERTY SqlConnector ROOT "Connection" GET __ChkComponent( Self, @::xSqlConnector )

   PROPERTY ReadOnly     ROOT "Behavior"   DEFAULT .F.
   PROPERTY AutoOpen     ROOT "Behavior"   DEFAULT .T.



   ACCESS Exists           INLINE ::IsOpen

   DATA EnumMemoType       EXPORTED INIT { { "None", "DBT", "FTP", "SMT" }, { DB_MEMO_NONE, DB_MEMO_DBT, DB_MEMO_FPT, DB_MEMO_SMT } }

   DATA __lMemory          EXPORTED  INIT .F.

   DATA IDEButton          EXPORTED
   DATA Fields             EXPORTED
   DATA FieldCtrl          EXPORTED  INIT {=>}

   DATA IndexOrder         EXPORTED  INIT 0

   DATA Border             EXPORTED
   DATA Text               EXPORTED

   ACCESS Caption     INLINE ::Text
   ASSIGN Caption(c)  INLINE ::Text := c

   ACCESS BindingSource    INLINE ::SqlConnector
   ASSIGN BindingSource(o) INLINE ::SqlConnector := o

   ASSIGN DataConnector(o) INLINE ::BindingSource := o
   ACCESS IsNew            INLINE ::__lNew

   DATA Connection         EXPORTED
   DATA Font               EXPORTED
   DATA ToolTip            EXPORTED
   DATA Id                 EXPORTED
   DATA BackColor          EXPORTED
   DATA ForeColor          EXPORTED
   DATA AllowClose         EXPORTED
   DATA AllowUndock        EXPORTED
   DATA Dock               EXPORTED
   DATA Width              EXPORTED
   DATA Height             EXPORTED
   DATA ClipChildren       EXPORTED
   DATA ClipSiblings       EXPORTED
   DATA OwnerDraw          EXPORTED  INIT .F.
   DATA Transparent        EXPORTED
   DATA Visible            EXPORTED

   DATA Events             EXPORTED  INIT { ;
                                          {"Object",  {;
                                                      { "OnInit"       , "", "" },;
                                                      { "OnError"      , "", "" } } },;
                                          {"General", {;
                                                      { "OnCreate"     , "", "" },;
                                                      { "OnOpen"       , "", "" },;
                                                      { "OnClose"      , "", "" },;
                                                      { "OnConnect"    , "", "" },;
                                                      { "OnDisconnect" , "", "" } } } }
   DATA bOnFileNameChanged EXPORTED
   DATA bOnFileClosed      EXPORTED
   DATA bSave              EXPORTED
   DATA Area               EXPORTED
   DATA __ExplorerFilter   EXPORTED  INIT { { "DataTable *.dbf", "*.dbf" } }
   DATA aScatter           EXPORTED

   DATA Bitmap             PROTECTED
   DATA MDIClient          PROTECTED
   DATA xDriver            PROTECTED INIT RddSetDefault()

   DATA Connector          EXPORTED

   DATA __aTmpStruct       PROTECTED
   DATA __lNew             PROTECTED INIT .F.
   DATA __aData            EXPORTED INIT {}

   DATA __hClass           PROTECTED
   DATA __aSavedPos        PROTECTED INIT {}

   DATA CurTopScope, CurBottomScope     EXPORTED

   METHOD Init() CONSTRUCTOR

   METHOD Open()
   METHOD CreateFields()
   METHOD Create()
   METHOD MemoExt()                           INLINE ::Connector:MemoExt()
   METHOD TableExt()                          INLINE ::Connector:TableExt()
   METHOD Gather()                            INLINE ::Connector:Gather()
   METHOD Scatter( aData )                    INLINE ::Connector:Scatter( aData )
   METHOD SetScope( xScope )                  INLINE ::CurTopScope := ::CurBottomScope := xScope, ::Connector:SetScope( xScope )
   METHOD SetTopScope( xScope )               INLINE ::CurTopScope := xScope, ::Connector:SetTopScope( xScope )
   METHOD SetBottomScope( xScope )            INLINE ::CurBottomScope := xScope, ::Connector:SetBottomScope( xScope )
   METHOD KillScope()                         INLINE ::CurTopScope := ::CurBottomScope := NIL, ::Connector:KillScope()
   METHOD Reindex()                           INLINE ::Connector:Reindex()
   METHOD Commit()                            INLINE ::Connector:Commit()
   METHOD RecLock()                           INLINE ::Connector:RecLock()
   METHOD NetError()                          INLINE ::Connector:NetError()
   METHOD FileLock()                          INLINE ::Connector:FileLock()
   METHOD UnLock()                            INLINE ::Connector:UnLock()
   METHOD dbEval(b)                           INLINE ::Connector:dbEval(b)
   METHOD UnLockAll()                         INLINE ::Connector:UnLockAll()
   METHOD FCount()                            INLINE ::Connector:FCount()
   METHOD Bof()                               INLINE ::Connector:Bof()
   METHOD Eof()                               INLINE ::Connector:Eof()
   METHOD Deleted()                           INLINE ::Connector:Deleted()
   METHOD RecCount()                          INLINE ::Connector:RecCount()
   METHOD RecNo()                             INLINE ::Connector:RecNo()
   METHOD GoTop()                             INLINE ::Cancel(), ::Connector:GoTop()
   METHOD GoTo( nRec )                        INLINE ::Cancel(), ::Connector:GoTo( nRec )
   METHOD GoBottom()                          INLINE ::Cancel(), ::Connector:GoBottom()
   METHOD Skip( n )                           INLINE ::Connector:Skip( n )
   METHOD Close()
   METHOD Append()                            INLINE ::Cancel(), ::Connector:Append()
   METHOD OrdSetFocus( cOrder )               INLINE IIF( cOrder != NIL, ::IndexOrder := ::IndexOrd(),), ::Connector:OrdSetFocus( cOrder )
   METHOD SetIndex( cIndex )                  INLINE ::IndexOrder := ::IndexOrd(), ::Connector:SetIndex( cIndex )
   METHOD SetRelation( oData,xKey,lAdditive ) INLINE ::Connector:SetRelation( oData, xKey, lAdditive )
   METHOD ClearRelations()                    INLINE ::Connector:ClearRelations(), Self
   METHOD SetOrder( nOrder )                  INLINE ::Connector:SetOrder( nOrder )
   METHOD Select( cAlias )                    INLINE ::Connector:Select( cAlias )
   METHOD SelectArea()                        INLINE ::Connector:SelectArea()
   METHOD Delete()                            INLINE ::Connector:Delete()
   METHOD Recall()                            INLINE ::Connector:Recall()
   METHOD Seek( xKey, lSoft )                 INLINE ::Connector:Seek( xKey, lSoft )
   METHOD Found()                             INLINE ::Connector:Found()
   METHOD SetDriver( cDriver )                INLINE ::Driver := cDriver, ::Connector:SetDriver( cDriver )
   METHOD CreateIndex( cName, xKey, lUnique ) INLINE ::Connector:CreateIndex( cName, xKey, lUnique )

   METHOD IndexOrd()                          INLINE ::Connector:IndexOrd()
   METHOD Zap()                               INLINE ::Connector:Zap()
   METHOD OrdCount()                          INLINE ::Connector:OrdCount()
   METHOD OrdName(n)                          INLINE ::Connector:OrdName(n)
   METHOD OrdNumber(cOrd,cBag)                INLINE ::Connector:OrdNumber(cOrd,cBag)
   METHOD OrdBagName(n)                       INLINE ::Connector:OrdBagName(n)
   METHOD OrdKey(n)                           INLINE ::Connector:OrdKey(n)
   METHOD Struct()                            INLINE ::Connector:Struct()
   METHOD OrdKeyGoTo( nPos )                  INLINE ::Connector:OrdKeyGoTo( nPos )
   METHOD SetFilter( c )                      INLINE ::Connector:SetFilter( c )
   METHOD GetFilter()                         INLINE ::Connector:GetFilter()
   METHOD OrdKeyCount()                       INLINE ::Connector:OrdKeyCount()
   METHOD Used()                              INLINE ::Connector:Used()

   METHOD FieldPut()
   METHOD FieldGet()

   METHOD FieldPos( cField )                  INLINE ::Connector:FieldPos( cField )
   METHOD FieldType( nField )                 INLINE ::Connector:FieldType( nField )

   METHOD OrdKeyRelPos(n)                     INLINE ::Connector:OrdKeyRelPos( n )
   METHOD OrdKeyNo()                          INLINE ::Connector:OrdKeyNo()
   METHOD OrdKeyVal()                         INLINE ::Connector:OrdKeyVal()
   METHOD OrdBagExt()                         INLINE ::Connector:OrdBagExt()

   METHOD OrdKeyNoRaw()                       INLINE ::Connector:OrdKeyNoRaw()

   METHOD RddInfo( nType, nSetting )          INLINE ::Connector:RddInfo( nType, nSetting )
   METHOD DbInfo( nType, nSetting )           INLINE ::Connector:DbInfo( nType, nSetting )

   METHOD OrdDescend( cOrder, lDescend )      INLINE ::Connector:OrdDescend( cOrder, lDescend )

   METHOD CheckAlias()

   METHOD Destroy(lNotify)                    INLINE IIF( ::IsOpen, ::Close(lNotify),), ::Super:Destroy()
   METHOD __SetAlias()
   METHOD SetFileName()
   METHOD CreateOrder()
   ACCESS IsOpen                              INLINE ::Structure != NIL //Select( ::Area ) > 0
   METHOD FromAlias()
   METHOD Insert()                            INLINE ::Append()
   METHOD __SetMemoType( nMemo )              INLINE ::RddInfo( RDDI_MEMOTYPE, nMemo )

   METHOD CreateTable()
   METHOD EnableFieldCtrl()

   METHOD Blank()
   METHOD Load()
   METHOD Save()
   METHOD Cancel() INLINE ::__lNew := .F., ::__aData := {}
   METHOD DestroyFields()
   METHOD NewInstance()
   METHOD SavePos()
   METHOD RestPos()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oOwner, hConnection ) CLASS DataTable
   DEFAULT ::__xCtrlName TO "DataTable"
   DEFAULT ::ClsName     TO "DataTable"
   ::ComponentType := "DataSource"
   ::Connection    := hConnection
   ::Super:Init( oOwner )
   ::Connector  := DataRdd( Self )
   HSetCaseMatch( ::FieldCtrl, .F. )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Close( lNotify ) CLASS DataTable
   IF ::Connector != NIL
      ::Connector:Close()
   ENDIF
   ::DestroyFields()
   IF ::FieldCtrl != NIL
      HEVAL( ::FieldCtrl, {|cField| ::FieldCtrl[ cField ] := NIL } )
      ::FieldCtrl := {=>}
   ENDIF
   ::Connector := NIL
   ::Structure := NIL
   ::Fields    := NIL
   IF VALTYPE( lNotify ) == "L" .AND. lNotify
      __Evaluate( ::bOnFileClosed, Self )
      ExecuteEvent( "OnClose", Self )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD EnableFieldCtrl( lEnable ) CLASS DataTable
   LOCAL n, cField
   FOR n := 1 TO LEN( ::Structure )
       cField := ::Structure[n][1]
       IF HGetPos( ::FieldCtrl, cField ) > 0
          ::FieldCtrl[ cField ]:Enabled := lEnable
       ENDIF
   NEXT
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD SavePos() CLASS DataTable
   AADD( ::__aSavedPos, { ::OrdSetFocus(),;
                          ::Recno(),;
                          (::Area)->( OrdScope( TOPSCOPE ) ),;
                          (::Area)->( OrdScope( BOTTOMSCOPE ) ),;
                          (::Area)->( dbFilter() ) } )
RETURN LEN( ::__aSavedPos )

//-------------------------------------------------------------------------------------------------------
METHOD RestPos() CLASS DataTable
   LOCAL n := LEN( ::__aSavedPos )
   IF n > 0
      ::OrdSetFocus( ::__aSavedPos[n][1] )
      (::Area)->( OrdScope( TOPSCOPE, ::__aSavedPos[n][3] ) )
      (::Area)->( OrdScope( BOTTOMSCOPE, ::__aSavedPos[n][4] ) )
      (::Area)->( dbSetFilter( ::__aSavedPos[n][5] ) )
      ::Goto( ::__aSavedPos[n][2] )
      ADEL( ::__aSavedPos, n, .T. )
   ENDIF
RETURN LEN( ::__aSavedPos )

//-------------------------------------------------------------------------------------------------------
METHOD NewInstance( lSetCurPos ) CLASS DataTable
   LOCAL n, oNewTable := DataTable( ::Owner, ::Connection )
   n := 1
   WHILE Select( ::Alias+xStr(n) ) > 0
      n++
   ENDDO
   oNewTable:Area      := NIL
   oNewTable:xAlias    := ::Alias+xStr(n)
   oNewTable:xFileName := ::FileName
   oNewTable:Path      := ::Path
   oNewTable:Driver    := ::Driver
   oNewTable:Shared    := ::Shared
   oNewTable:bSave     := ::bSave
   oNewTable:Open()

   DEFAULT lSetCurPos TO .F.

   IF ! oNewTable:IsOpen
      oNewTable := NIL
   ELSEIF lSetCurPos
      oNewTable:OrdSetFocus( ::OrdSetFocus() )
      oNewTable:Goto( ::Recno() )
   ENDIF
RETURN oNewTable
//-------------------------------------------------------------------------------------------------------
METHOD Blank() CLASS DataTable
   LOCAL xValue, n
   ::__lNew  := .T.
   ::__aData := Array( LEN( ::Structure ) )
   FOR n := 1 TO LEN( ::Structure )
       DO CASE
          CASE ::Structure[n][2] == "C"
               xValue := SPACE( ::Structure[n][3] )
          CASE ::Structure[n][2] == "N"
               xValue := 0
          CASE ::Structure[n][2] == "L"
               xValue := .F.
          CASE ::Structure[n][2] == "D"
               xValue := CTOD("")
          CASE ::Structure[n][2] == "M"
               xValue := ""
          CASE ::Structure[n][2] == "TIMESTAMP"
               xValue := StoT( "" )
          OTHERWISE
               xValue := ""
       ENDCASE

       IF HGetPos( ::FieldCtrl, ::Structure[n][1] ) > 0
          IF ::FieldCtrl[ ::Structure[n][1] ]:bSetValue != NIL
             Eval( ::FieldCtrl[ ::Structure[n][1] ]:bSetValue, xValue )
             IF ::FieldCtrl[ ::Structure[n][1] ]:IsWindowVisible()
                ::FieldCtrl[ ::Structure[n][1] ]:UpdateWindow()
             ENDIF
          ENDIF
       ELSE
          ::__aData[n] := xValue
       ENDIF
   NEXT
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Save() CLASS DataTable
   LOCAL n, cField, oCtrl
   IF ::bSave != NIL .AND. ! Eval( ::bSave, Self )
      ::__lNew := .F.
      ::__aData := {}
      RETURN Self
   ENDIF
   IF ::__lNew
      (::Area)->( dbAppend() )
   ELSEIF ::Shared
      WHILE ! (::Area)->( RLock() )
         sleep(1000)
      ENDDO
   ENDIF
   FOR n := 1 TO LEN( ::Structure )
       cField := ::Structure[n][1]
       IF HGetPos( ::FieldCtrl, cField ) > 0
          oCtrl := ::FieldCtrl[ cField ]
          IF oCtrl:bGetValue != NIL
             ::Connector:FieldPut( n, Eval( oCtrl:bGetValue ) )
          ENDIF
       ELSEIF LEN( ::__aData ) >= n .AND. ::__aData[n] != NIL
          ::Connector:FieldPut( n, ::__aData[n] )
       ENDIF
   NEXT
   IF ::__lNew .OR. ::Shared
      ::Unlock()
      ::Commit()
   ENDIF
   ::__lNew := .F.
   ::__aData := {}
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD FieldPut( n, xVal ) CLASS DataTable
   IF Len( ::__aData ) >= n
      ::__aData[n] := xVal
    ELSE
      ::Connector:FieldPut( n, xVal )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD FieldGet( n ) CLASS DataTable
   LOCAL xVal
   IF Len( ::__aData ) >= n .AND. ::__aData[n] != NIL
      xVal := ::__aData[n]

   ELSEIF ::IsNew

      DO CASE
         CASE ::Structure[n][2] == "C"
              xVal := SPACE( ::Structure[n][3] )
         CASE ::Structure[n][2] == "N"
              xVal := 0
         CASE ::Structure[n][2] == "L"
              xVal := .F.
         CASE ::Structure[n][2] == "D"
              xVal := CTOD("")
         CASE ::Structure[n][2] == "M"
              xVal := ""
         CASE ::Structure[n][2] == "TIMESTAMP"
              xVal := StoT( "" )

         OTHERWISE
              xVal := ""
      ENDCASE

   ELSE
      xVal := ::Connector:FieldGet( n )
   ENDIF
RETURN xVal

//-------------------------------------------------------------------------------------------------------
METHOD Load( lAvoidNew ) CLASS DataTable
   LOCAL n, cField, oCtrl, xData
   DEFAULT lAvoidNew TO .F.
   IF !lAvoidNew
      ::__lNew := .F.
   ENDIF
   ::__aData := Array( LEN( ::Structure ) )

   FOR n := 1 TO LEN( ::Structure )
       xData := ::FieldGet( n )

       cField := ::Structure[n][1]
       IF HGetPos( ::FieldCtrl, cField ) > 0
          oCtrl := ::FieldCtrl[ cField ]
          IF oCtrl:bSetValue != NIL

             Eval( oCtrl:bSetValue, xData )

             IF oCtrl:IsWindowVisible()
                oCtrl:UpdateWindow()
             ENDIF
          ENDIF
       ELSEIF Len( ::__aData ) >= n
          ::__aData[n] := xData
       ENDIF
   NEXT
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD FromAlias( cAlias )
   IF cAlias != NIL .AND. (cAlias)->( Used() )
      IF ::Fields != NIL
         ::Fields := NIL
      ENDIF
      ::Connector := DataRdd( Self )
      ::xAlias    := cAlias
      ::Area      := ::Select( cAlias )
      ::Structure := ::Struct()
      ::CreateFields()
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create( lIgnoreAO ) CLASS DataTable
   LOCAL lChanged, n, cFileName, cPath, cMemo, cData
   IF ValType( ::Socket ) == "C" .AND. Ascan( ::Form:__hObjects:Keys, {|c| Upper(c) == Upper(::Socket) } ) > 0
      ::Socket := ::Form:__hObjects[ ::Socket ]
      IF ! ::DesignMode
         ::Connector := SocketRdd( Self )
      ENDIF
   ENDIF
   ::Connector:Create( lIgnoreAO )
   IF ! Empty( ::__aTmpStruct ) .AND. ! Empty ( ::Structure )
      lChanged := LEN(::__aTmpStruct) <> LEN(::Structure)
      IF ! lChanged
         FOR n := 1 TO LEN( ::__aTmpStruct )
             lChanged := Upper(::__aTmpStruct[n][1]) != Upper(::Structure[n][1]) .OR.;
                         ::__aTmpStruct[n][2] != ::Structure[n][2] .OR.;
                         ::__aTmpStruct[n][3] != ::Structure[n][3] .OR.;
                         ::__aTmpStruct[n][4] != ::Structure[n][4]
             IF lChanged
                EXIT
             ENDIF
         NEXT
      ENDIF
      IF lChanged
         cFileName := ::FileName
         n         := RAT( "\", cFileName )
         cData     := SUBSTR( cFileName, 1, n-1 )
         cPath     := GetTempPath()

         cFileName := SUBSTR( cFileName, n+1 )

         dbCreate( cPath + "__" + cFileName, ::__aTmpStruct, ::Driver, , , , , 0 )

         IF FILE( cPath + "__" + cFileName )
            dbCloseArea( ::Area )
            ::DestroyFields()

            dbUseArea( ! ::__lMemory, ::Driver, cPath + "__" + cFileName, "modstru", .F., .F.,, 0 )

            SELECT "modstru"
            APPEND FROM (::FileName) VIA (::Driver)
            modstru->( dbCloseArea() )

            FERASE( ::FileName )

            FRENAME( cPath + "__" + cFileName, ::FileName )

            n := RAT( ".", cFileName )
            cMemo := Left( cFileName, n-1 ) + ::MemoExt()

            FERASE( cData + "\" + cMemo )
            FRENAME( cPath + "__" + cMemo, cData + "\" + cMemo )

            cFileName := ::FileName

            dbSelectArea( ::Area )
            dbUseArea( .F., ::Driver, cFileName, ::Alias, ::Shared, ::ReadOnly, ::CodePage )
            ::Structure := (::Area)->( dbStruct() )
            ::CreateFields()
         ENDIF


      ENDIF
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD CreateTable( aStruc, cFile ) CLASS DataTable
   DEFAULT cFile  TO ::FileName
   DEFAULT aStruc TO ::Structure
   IF ! Empty( cFile ) .AND. ! File( cFile ) .AND. ! Empty( aStruc )
      ::Connector:CreateTable( cFile, aStruc, ::Driver )

   ELSEIF File( cFile ) .AND. ! Empty( aStruc )
      ::__aTmpStruct := aClone( aStruc )

   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD CreateFields() CLASS DataTable
   LOCAL aField, cField

   ::__hClass := __ClsNew( "DATA_" + ::Alias, 0, 0, { Data():ClassH } )

   FOR EACH aField IN ::Structure
       cField := aField[1]
       __clsAddMsg( ::__hClass,       cField, &( "{|Self| ::FieldGet( " + Str( HB_EnumIndex() ) + " ) }" ), HB_OO_MSG_INLINE, NIL, 1 )
       __clsAddMsg( ::__hClass, "_" + cField, &( "{|Self, xVal| ::FieldPut( " + Str( HB_EnumIndex() ) + ", xVal ) }" ), HB_OO_MSG_INLINE, NIL, 1 )
   NEXT

   //::CheckAlias()

   ::Fields := __clsInst( ::__hClass ):Init( Self )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD DestroyFields() CLASS DataTable
   LOCAL aField, cField
   IF ::Structure != NIL
      FOR EACH aField IN ::Structure
          IF aField != NIL
             cField := aField[1]
             __clsDelMsg( ::__hClass,       cField )
             __clsDelMsg( ::__hClass, "_" + cField )
          ENDIF
      NEXT
   ENDIF
   ::__hClass := NIL
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD CreateOrder( cOrderBagName, cTag, cKey, cFor, bFor, bWhile, bEval, nEvery, nRecNo, nNext, nRecord, lRest, lUnique, lDescend, lAll ) CLASS DataTable
   LOCAL n, cFileName, cPath

   IF (::Area)->( OrdNumber( cTag, cOrderBagName ) ) == 0 .OR. ! Upper(cKey) == Upper((::Area)->( IndexKey( OrdNumber( cTag, cOrderBagName ) ) ))
      cFileName := ::FileName
      n         := RAT( "\", cFileName )
      cPath     := Left( cFileName, n-1 )
      cFileName := SubStr( cFileName, n+1 )

      dbCloseArea( ::Area )

      dbSelectArea( ::Area )
      dbUseArea( .F., ::Driver, cPath + "\" + cFileName, ::Alias, .F., ::ReadOnly, ::CodePage )

      (::Area)->( OrdCondSet( cFor, bFor, lAll, bWhile, bEval, nEvery, nRecNo, nNext, nRecord, lRest, lDescend ) )
      (::Area)->( OrdCreate( cOrderBagName, cTag, cKey,, lUnique ) )

      dbCloseArea( ::Area )

      dbSelectArea( ::Area )
      dbUseArea( .F., ::Driver, cFileName, ::Alias, ::Shared, ::ReadOnly, ::CodePage )

      RETURN (::Area)->( OrdNumber( cTag, cOrderBagName ) ) > 0 .AND. Upper((::Area)->( IndexKey( OrdNumber( cTag, cOrderBagName ) ) )) == Upper(cKey)
   ENDIF
RETURN .T.

//-------------------------------------------------------------------------------------------------------
METHOD Open() CLASS DataTable
   DEFAULT ::Connector TO DataRdd( Self )
   ::Create(.T.)
RETURN ::lCreated

//-------------------------------------------------------------------------------------------------------
METHOD CheckAlias() CLASS DataTable
   LOCAL n, aField
   LOCAL hClass, cField

   DEFAULT ::xAlias TO ::Alias()
   DEFAULT ::Area   TO ::Select()

   IF ::xAlias != NIL
      IF Select( ::xAlias ) > 0 .AND. ::Structure == NIL
         IF !::__lMemory
            ::xFileName  := (::Area)->( dbInfo( DBI_FULLPATH ) )
            n := RAT( "\", ::xFileName )
            ::Path      := SUBSTR( ::xFileName, 1, n-1 )
            ::xFileName := SUBSTR( ::xFileName, n+1 )
         ENDIF

         ::Structure  := ::Struct()

         hClass := __ClsNew( "DATA_" + ::Alias, 0, 0, { Data():ClassH } )

         FOR EACH aField IN ::Structure
             cField := aField[1]
             __clsAddMsg( hClass,       cField, &( "{|Self| ::FieldGet( " + Str( HB_EnumIndex() ) + " ) }" ), HB_OO_MSG_INLINE, NIL, 1 )
             __clsAddMsg( hClass, "_" + cField, &( "{|Self, xVal| ::FieldPut( " + Str( HB_EnumIndex() ) + ", xVal ) }" ), HB_OO_MSG_INLINE, NIL, 1 )
         NEXT

         ::Fields := __clsInst( hClass ):Init( ::Fields:Parent )
      ENDIF

   ENDIF

RETURN ::xAlias

//-------------------------------------------------------------------------------------------------------

METHOD __SetAlias( cAlias ) CLASS DataTable
   LOCAL lCreate := .F.
   IF ! ::__lMemory
      IF EMPTY( cAlias )
         cAlias := NIL
      ENDIF
      IF ::IsOpen
         ::Close()
         lCreate := .T.
      ENDIF
      ::xAlias := cAlias
      IF lCreate
         ::Create()
      ENDIF
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD SetFileName( cFileName ) CLASS DataTable
   IF EMPTY( cFileName )
      cFileName := NIL
   ENDIF

   IF ::lCreated .AND. ::IsOpen
      ::xAlias := NIL
      ::Close()
   ENDIF
   ::xFileName := cFileName

   TRY // the object might be destroyed so we cannot TEST if Self != NIL the only way is to trap the error
      IF ::lCreated
         ::Create()
         IF ::bOnFileNameChanged != NIL
            EVAL( ::bOnFileNameChanged, Self )
         ENDIF
      ENDIF
   CATCH
   END
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS Data
   DATA Parent

   METHOD Init() CONSTRUCTOR
   METHOD Put()
   METHOD FieldPut( nField, xVal )  INLINE ::Parent:FieldPut( nField, xVal )
   METHOD FieldGet( nField )        INLINE ::Parent:FieldGet( nField )
   METHOD FieldType( nField )       INLINE ::Parent:FieldType( nField )
   METHOD FieldName( nField )       INLINE ::Parent:FieldName( nField )
ENDCLASS

//-------------------------------------------------------------------------------------------------------

METHOD Init( o ) CLASS Data
   ::Parent := o
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Put(xVal, cName) CLASS Data
   IF xVal != NIL
      (::Parent:Area)->&cName := xVal
    ELSE
      RETURN (::Parent:Area)->&cName
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS DataRdd
   DATA Owner           EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD SetTopScope( xScope )               INLINE (::Owner:Area)->( OrdScope( TOPSCOPE, xScope ) )
   METHOD SetBottomScope( xScope )            INLINE (::Owner:Area)->( OrdScope( BOTTOMSCOPE, xScope ) )

   METHOD SetScope( xScope )                  INLINE (::Owner:Area)->( OrdScope( TOPSCOPE, xScope ) ),;
                                                     (::Owner:Area)->( OrdScope( BOTTOMSCOPE, xScope ) )

   METHOD KillScope()                         INLINE (::Owner:Area)->( OrdScope( TOPSCOPE, NIL ) ),;
                                                     (::Owner:Area)->( OrdScope( BOTTOMSCOPE, NIL ) )

   METHOD Alias()                             INLINE (::Owner:Area)->( Alias() )
   METHOD Reindex()                           INLINE (::Owner:Area)->( dbReindex() )
   METHOD Commit()                            INLINE (::Owner:Area)->( dbCommit() )
   METHOD RecLock()                           INLINE (::Owner:Area)->( RLOCK() )
   METHOD NetError()                          INLINE (::Owner:Area)->( NETERR() )
   METHOD FileLock()                          INLINE (::Owner:Area)->( FLOCK() )
   METHOD UnLock()                            INLINE (::Owner:Area)->( dbUnlock() )
   METHOD UnLockAll()                         INLINE (::Owner:Area)->( dbUnlockAll() )
   METHOD dbEval(b)                           INLINE (::Owner:Area)->( dbEval(b) )
   METHOD Bof()                               INLINE (::Owner:Area)->( Bof() )
   METHOD FCount()                            INLINE (::Owner:Area)->( FCount() )
   METHOD Eof()                               INLINE (::Owner:Area)->( Eof() )
   METHOD Deleted()                           INLINE (::Owner:Area)->( Deleted() )
   METHOD RecCount()                          INLINE (::Owner:Area)->( Reccount() )
   METHOD RecNo()                             INLINE (::Owner:Area)->( Recno() )
   METHOD GoTop()                             INLINE (::Owner:Area)->( dbgotop() )
   METHOD GoTo( nRec )                        INLINE (::Owner:Area)->( dbgoto( nRec ) )
   METHOD GoBottom()                          INLINE (::Owner:Area)->( dbgobottom() )
   METHOD Skip( n )                           INLINE (::Owner:Area)->( dbSkip( n ) )
   METHOD Close()                             INLINE (::Owner:Area)->( dbCloseArea() )
   METHOD Append()                            INLINE (::Owner:Area)->( dbAppend() )
   METHOD OrdSetFocus( cOrder )               INLINE (::Owner:Area)->( OrdSetFocus( cOrder ) )

   METHOD OrdDescend( cOrder, lDescend )      INLINE (::Owner:Area)->( OrdDescend( cOrder, , lDescend ) )

   METHOD SetIndex( cIndex )                  INLINE (::Owner:Area)->( dbSetIndex( cIndex ) )
   METHOD SetOrder( nOrder )                  INLINE (::Owner:Area)->( dbSetOrder( nOrder ) )
   METHOD Select( cAlias )                    INLINE IIF( ! EMPTY(cAlias), Select(cAlias), dbSelectArea( ::Owner:Area ) )
   METHOD SelectArea()                        INLINE dbSelectArea( ::Owner:Area )
   METHOD Delete()                            INLINE (::Owner:Area)->( dbDelete() )
   METHOD Recall()                            INLINE (::Owner:Area)->( dbRecall() )
   METHOD Seek( xKey, lSoft )                 INLINE (::Owner:Area)->( dbSeek( xKey, lSoft ) )
   METHOD Found()                             INLINE (::Owner:Area)->( Found() )
   METHOD SetDriver( cDriver )                INLINE (::Owner:Area)->( dbSetDriver( cDriver ) )
   METHOD CreateIndex( cName, xKey, lUnique ) INLINE (::Owner:Area)->( dbCreateIndex( cName, xKey, &("{||"+xKey+"}"), lUnique ) )
   METHOD IndexOrd()                          INLINE (::Owner:Area)->( IndexOrd() )
   METHOD Zap()                               INLINE (::Owner:Area)->( __dbZap() )
   METHOD OrdCount()                          INLINE (::Owner:Area)->( OrdCount() )
   METHOD OrdName(n)                          INLINE (::Owner:Area)->( OrdName(n) )
   METHOD OrdNumber(cOrd,cBag)                INLINE (::Owner:Area)->( OrdNumber(cOrd,cBag) )
   METHOD OrdBagName(n)                       INLINE (::Owner:Area)->( OrdBagName(n) )
   METHOD OrdKey(n)                           INLINE (::Owner:Area)->( OrdKey(n) )
   METHOD Struct()                            INLINE (::Owner:Area)->( dbStruct() )
   METHOD OrdKeyGoTo( nPos )                  INLINE (::Owner:Area)->( OrdKeyGoTo( nPos ) )
   METHOD SetFilter()


   METHOD GetFilter()                         INLINE (::Owner:Area)->( dbFilter() )
   METHOD OrdKeyCount()                       INLINE (::Owner:Area)->( OrdKeyCount() )
   METHOD Used()                              INLINE (::Owner:Area)->( Used() )
//   METHOD OrdDescend(cnOrder,cFile,lDescend ) INLINE (::Owner:Area)->( OrdDescend( cnOrder, cFile, lDescend ) )
   METHOD SetRelation()
   METHOD ClearRelations()                    INLINE (::Owner:Area)->( dbClearRel() )
   METHOD FieldPut( nField, xVal )            INLINE (::Owner:Area)->( FieldPut( nField, xVal ) )
   METHOD FieldGet( nField )                  INLINE (::Owner:Area)->( FieldGet( nField ) )
   METHOD FieldPos( cField )                  INLINE (::Owner:Area)->( FieldPos( cField ) )
   METHOD FieldType( nField )                 INLINE (::Owner:Area)->( FieldType( nField ) )
   METHOD OrdBagExt()                         INLINE (::Owner:Area)->( OrdBagExt() )
   METHOD TableExt()                          INLINE (::Owner:Area)->( RddInfo( RDDI_TABLEEXT ) )
   METHOD MemoExt()                           INLINE (::Owner:Area)->( RddInfo( RDDI_MEMOEXT ) )

   METHOD CreateTable( cFile, aStru, cDriver) INLINE dbCreate( cFile, aStru, cDriver )
   METHOD Gather()
   METHOD Scatter( aData )
   METHOD OrdKeyRelPos(n)
   METHOD OrdKeyNo()
   METHOD OrdkeyNoRaw()
   METHOD OrdKeyVal()                         INLINE (::Owner:Area)->( OrdKeyVal() )
   METHOD RddInfo( nType, nSetting )          INLINE (::Owner:Area)->( RddInfo( nType, nSetting, ::Owner:Driver ) )
   METHOD DbInfo( nType, nSetting )           INLINE (::Owner:Area)->( DbInfo( nType, nSetting ) )
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS DataRdd
   ::Owner := oOwner
RETURN Self

METHOD SetFilter( cFilter ) CLASS DataRdd
   IF cFilter != NIL
      IF ValType( cFilter ) == "C"
         (::Owner:Area)->( dbSetFilter( &("{||" + cFilter + "}"), cFilter ) )
      ELSE
         (::Owner:Area)->( dbSetFilter( cFilter ) )
      ENDIF
   ELSE
      (::Owner:Area)->( dbClearFilter() )
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OrdKeyRelPos(n) CLASS DataRdd
   LOCAL nRelPos := 0
   IF ::Owner:IsOpen
      nRelPos := (::Owner:Area)->( OrdKeyRelPos(n) )
      IF VALTYPE(nRelPos)=="C"
         IF EMPTY(nRelPos)
            nRelPos := NIL
          ELSE
            nRelPos := VAL(nRelPos)
         ENDIF
      ENDIF
      DEFAULT nRelPos TO (::Owner:Area)->( OrdKeyNo() )
      IF VALTYPE(nRelPos)=="C"
         nRelPos := VAL(nRelPos)
      ENDIF
   ENDIF
RETURN nRelPos

//-------------------------------------------------------------------------------------------------------
METHOD OrdKeyNo() CLASS DataRdd
   LOCAL nPos := (::Owner:Area)->( OrdKeyNo() )
   DEFAULT nPos TO (::Owner:Area)->( Recno() )
RETURN nPos

//-------------------------------------------------------------------------------------------------------
METHOD OrdKeyNoRaw() CLASS DataRdd
   LOCAL nPos := (::Owner:Area)->( dbOrderInfo( DBOI_KEYNORAW ) )
   DEFAULT nPos TO ::OrdKeyNo()
RETURN nPos


//-------------------------------------------------------------------------------------------------------
METHOD SetRelation( oData, xKey, lAdditive ) CLASS DataRdd
   LOCAL bKey, cKey
   DEFAULT lAdditive TO FALSE

   IF !lAdditive
      ::ClearRelations()
   ENDIF

   IF oData != NIL
      bKey := xKey
      IF VALTYPE( xKey ) == "C"
         cKey := "{||"+xKey+"}"
         bKey := &cKey
         cKey := xKey
       ELSE
         cKey := NIL
      ENDIF

      (::Owner:Area)->( dbSetRelation( oData:Alias(), bKey, cKey ) )
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create( lIgnoreAO ) CLASS DataRdd
   LOCAL nAlias, cAlias, oErr
   LOCAL cEvent, n, cFile, lDef := .T.
   DEFAULT lIgnoreAO TO .F.

   IF VALTYPE( ::Owner:SqlConnector ) == "C"
      RETURN .F.
   ENDIF

   IF ( ::Owner:AutoOpen .OR. lIgnoreAO ) .AND. ( ( !::Owner:IsOpen .AND. !EMPTY( ::Owner:FileName ) ) .OR. ::Owner:__lMemory )
      ExecuteEvent( "OnInit", ::Owner )

      IF !::Owner:__lMemory .AND. ::Owner:Connection == NIL
         IF (::Owner:Driver IN { "SQLRDD", "SQLEX" })
            IF ::Owner:DesignMode
               RETURN ::Owner
            ENDIF
            cFile := ::Owner:FileName
          ELSEIF !EMPTY( ALLTRIM( ::Owner:Path ) )
            cFile := ALLTRIM( ::Owner:Path ) + "\" + ::Owner:FileName
            IF !FILE( cFile )
               IF ! ::Owner:DesignMode
                  Throw( ErrorNew( "DataTable", 21, 1010, ::Owner:FileName, "The specified file could not be found", (EF_CANRETRY | EF_CANDEFAULT) ) )
                ELSE
                  ::Owner:Application:MainForm:MessageBox( "The specified file could not be found", ::Owner:FileName, "Error" )
               ENDIF
               RETURN ::Owner
            ENDIF
          ELSEIF ( n := RAT( "\", ::Owner:FileName ) ) > 0
            cFile := ALLTRIM( ::Owner:FileName )
            IF !FILE( cFile )
               IF ! ::Owner:DesignMode
                  Throw( ErrorNew( "DataTable", 21, 1010, ::Owner:FileName, "The specified file could not be found", (EF_CANRETRY | EF_CANDEFAULT) ) )
                ELSE
                  ::Owner:Application:MainForm:MessageBox( "The specified file could not be found", ::Owner:FileName, "Error" )
               ENDIF
               RETURN ::Owner
            ENDIF
          ELSE
            cFile := ::Owner:Path + IIF( ! Empty(::Owner:Path), "\", "" ) + ::Owner:FileName
         ENDIF
      ELSEIF ::Owner:Connection != NIL
         cFile := ::Owner:FileName
      ENDIF
      IF HGetPos( ::Owner:EventHandler, "OnCreate" ) != 0
         cEvent := ::Owner:EventHandler[ "OnCreate" ]
         IF __objHasMsg( ::Owner:Form, cEvent )
            ::Owner:Form:&cEvent( ::Owner )
         ENDIF
      ENDIF

      nAlias := 1
      cAlias := ::Owner:xAlias
      IF EMPTY( cAlias ) .AND. ! Empty( cFile )
         cAlias := SUBSTR( cFile, RAT("\",cFile)+1 )
         cAlias := SUBSTR( cAlias, 1, RAT(".",cAlias)-1 )
      ENDIF

      IF ! EMPTY( cAlias )
         IF cAlias[1] == "&"
            cAlias := SUBSTR( cAlias, 2 )
            IF ::Owner:DesignMode
               TRY
                  cAlias := &cAlias
               CATCH
               END
             ELSE
               cAlias := &cAlias
            ENDIF
            ::Owner:xAlias := cAlias
          ELSEIF ::Owner:__lMemory
            IF Select( cAlias ) > 0
               WHILE Select( cAlias + XSTR( nAlias ) ) > 0
                  nAlias ++
               ENDDO
               ::Owner:xAlias := cAlias + XSTR( nAlias )
            ENDIF
         ENDIF
      ENDIF

      IF ::Owner:__lMemory
         cFile := "mem:"+::Owner:xAlias
         IF EMPTY( ::Owner:Structure )
            RETURN ::Owner
         ENDIF
         nMemSel++
         ::Owner:Area := nMemSel
         Select( nMemSel )

         IF !FILE( cFile )
            TRY
               dbCreate( cFile, ::Owner:Structure, ::Owner:Driver )
               dbCloseArea( nMemSel )
               dbSelectArea( nMemSel )
            CATCH
               RETURN ::Owner
            END
         ENDIF
      ENDIF

      IF ::Owner:Area != NIL
         Select( ::Owner:Area )
      ENDIF

      IF ::Owner:Connection != NIL .AND. ( n := RAT( "\", cFile ) ) > 0
         cFile := SUBSTR( cFile, n+1 )
      ENDIF

      IF ::Owner:xAlias == NIL
         ::Owner:xAlias := cFile
         IF ( n := RAT( "\", ::Owner:xAlias ) ) > 0
            ::Owner:xAlias := SUBSTR( ::Owner:xAlias, n+1 )
         ENDIF
         IF ( n := RAT( ".", ::Owner:xAlias ) ) > 0
            ::Owner:xAlias := SUBSTR( ::Owner:xAlias, 1, n-1 )
         ENDIF
         ::Owner:xAlias := Upper( ::Owner:xAlias )
         IF Len( ::Owner:xAlias ) > 10
            ::Owner:xAlias := Left( ::Owner:xAlias, 10 )
         ENDIF
      ENDIF

      dbUseArea( ::Owner:Area == NIL, ::Owner:Driver, cFile, ::Owner:Alias, ::Owner:Shared, ::Owner:ReadOnly, ::Owner:CodePage, IIF( ::Owner:SqlConnector != NIL, ::Owner:SqlConnector:ConnectionID, ) )

      IF ! ::Owner:__lMemory .AND. NETERR()
         ::Owner:Error := oErr
         ExecuteEvent( "OnError", ::Owner )
         RETURN .F.
      ENDIF

      IF ! ::Owner:MemoType > 0
         ::Owner:__SetMemoType( ::Owner:MemoType )
      ENDIF

      DEFAULT ::Owner:xAlias TO Alias()

      ::Owner:Area := Select()
      ::Owner:Structure := (::Owner:Area)->( dbStruct() )

      AEVAL( ::Owner:Structure, {|a,n| (a), ASIZE( ::Owner:Structure[n], 4 )} )

      ::Owner:CreateFields()

      IF HGetPos( ::Owner:EventHandler, "OnOpen" ) != 0
         cEvent := ::Owner:EventHandler[ "OnOpen" ]
         IF __objHasMsg( ::Owner:Form, cEvent )
            ::Owner:Form:&cEvent( ::Owner )
         ENDIF
      ENDIF
      ::Owner:lCreated := .T.
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Scatter() CLASS DataRdd
   ::Owner:aScatter := ARRAY( LEN( ::Owner:Structure ) )
   aEval( ::Owner:aScatter, {|a,n| (a), ::Owner:aScatter[n] := (::Owner:Area)->( FieldGet(n) ) } )
   ::Owner:Load( .T. )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Gather() CLASS DataRdd
   aEval( ::Owner:aScatter, {|a,n| (a), (::Owner:Area)->( FieldPut(n, ::Owner:aScatter[n] ) ) } )
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS SocketRdd
   DATA Owner           EXPORTED

   METHOD Init() CONSTRUCTOR

   METHOD Create()
   METHOD Alias()                             INLINE ::Request( "Alias" )
   METHOD Reindex()                           INLINE ::Request( "dbReindex" )
   METHOD Commit()                            INLINE ::Request( "dbCommit" )
   METHOD RecLock()                           INLINE ::Request( "RLOCK" )
   METHOD FileLock()                          INLINE ::Request( "FLOCK" )
   METHOD UnLock()                            INLINE ::Request( "dbUnlock" )
   METHOD UnLockAll()                         INLINE ::Request( "dbUnlockAll" )
   METHOD Bof()                               INLINE ::Request( "Bof" )
   METHOD FCount()                            INLINE ::Request( "FCount" )
   METHOD Eof()                               INLINE ::Request( "Eof" )
   METHOD Deleted()                           INLINE ::Request( "Deleted" )
   METHOD RecCount()                          INLINE ::Request( "Reccount" )
   METHOD RecNo()                             INLINE ::Request( "Recno" )
   METHOD GoTop()                             INLINE ::Request( "dbgotop" )
   METHOD GoTo( nRec )                        INLINE ::Request( "dbgoto", {nRec} )
   METHOD GoBottom()                          INLINE ::Request( "dbgobottom" )
   METHOD Skip( n )                           INLINE ::Request( "dbSkip", {n} )
   METHOD Close()                             INLINE ::Request( "dbCloseArea" )
   METHOD Append()                            INLINE ::Request( "dbAppend" )
   METHOD OrdSetFocus( cOrder )               INLINE ::Request( "OrdSetFocus", {cOrder} )
   METHOD SetIndex( cIndex )                  INLINE ::Request( "dbSetIndex", {cIndex} )
   METHOD SetOrder( nOrder )                  INLINE ::Request( "dbSetOrder", {nOrder} )
   METHOD Select()                            INLINE ::Request( "Select" )
   METHOD SelectArea()                        INLINE ::Request( "dbSelectArea", {::Owner:Area} )
   METHOD Delete()                            INLINE ::Request( "dbDelete" )
   METHOD Recall()                            INLINE ::Request( "dbRecall" )
   METHOD Seek( xKey, lSoft )                 INLINE ::Request( "dbSeek", {xKey, lSoft} )
   METHOD Found()                             INLINE ::Request( "Found" )
   METHOD SetDriver( cDriver )                INLINE ::Request( "dbSetDriver", {cDriver} )
   METHOD CreateIndex( cName, xKey, lUnique ) INLINE ::Request( "dbCreateIndex", {cName, xKey, lUnique} )
   METHOD IndexOrd()                          INLINE ::Request( "IndexOrd" )
   METHOD Zap()                               INLINE ::Request( "__dbZap" )
   METHOD OrdCount()                          INLINE ::Request( "OrdCount" )
   METHOD OrdName(n)                          INLINE ::Request( "OrdName", {n} )
   METHOD OrdNumber(cOrd,cBag)                INLINE ::Request( "OrdNumber", {cOrd,cBag} )
   METHOD OrdBagName(n)                       INLINE ::Request( "OrdBagName", {n} )
   METHOD OrdKey(n)                           INLINE ::Request( "OrdKey", {n} )
   METHOD Struct()                            INLINE ::Request( "dbStruct" )
   METHOD OrdKeyGoTo( nPos )                  INLINE ::Request( "OrdKeyGoTo", {nPos} )
   METHOD SetFilter( c )                      INLINE ::Request( "dbSetFilter", {c} )
   METHOD OrdKeyCount()                       INLINE ::Request( "OrdKeyCount" )
   METHOD Used()                              INLINE ::Request( "Used" )
//   METHOD OrdDescend(cnOrder,cFile,lDescend ) INLINE ::Request( "OrdDescend", {cnOrder, cFile, lDescend} )
   METHOD SetTopScope( xScope )               INLINE ::Request( "OrdScope", {TOPSCOPE, xScope} )
   METHOD SetBottomScope( xScope )            INLINE ::Request( "OrdScope", {BOTTOMSCOPE, xScope} )
   METHOD SetScope( xScope )                  INLINE ::Request( "OrdScope", {TOPSCOPE, xScope} ),;
                                                     ::Request( "OrdScope", {BOTTOMSCOPE, xScope} )
   METHOD KillScope()                         INLINE ::Request( "OrdScope", {TOPSCOPE, NIL} ),;
                                                     ::Request( "OrdScope", {BOTTOMSCOPE, NIL} )
   METHOD FieldPut( nField, xVal )            INLINE ::Request( "FieldPut", { nField, xVal } )
   METHOD FieldGet( nField )                  INLINE ::Request( "FieldGet", { nField } )
   METHOD FieldType( nField )                 INLINE ::Request( "FieldType", { nField } )
   METHOD Gather()                            INLINE ::Request( "Gather" )
   METHOD Scatter( aData )                    INLINE ::Request( "Scatter", {aData} )
   METHOD SetRelation()
   METHOD Request()
   METHOD OrdKeyRelPos(n)                     INLINE ::Request( "OrdKeyRelPos", {n} )
   METHOD OrdKeyNo()                          INLINE ::Request( "OrdKeyNo" )
   METHOD OrdKeyRaw()                         INLINE ::Request( "OrdKeyNoRaw" )
   METHOD OrdKeyVal()                         INLINE ::Request( "OrdKeyVal" )
   METHOD OrdBagExt()                         INLINE ::Request( "OrdBagExt" )
ENDCLASS

METHOD Init( oOwner ) CLASS SocketRdd
   ::Owner := oOwner
RETURN Self

METHOD Create() CLASS SocketRdd
   LOCAL nSecs := SECONDS()
   WHILE ! ::Owner:Socket:Connected .AND. SECONDS() - nSecs < 30
      ::Owner:Socket:Connect()
   ENDDO
   IF ! ::Owner:Socket:Connected
      ExecuteEvent( "OnError", ::Owner )
      RETURN Self
   ENDIF
   ::Request( "Create", {::Owner:Path, ::Owner:FileName} )
   ::Owner:Structure := ::Struct()
   ::Owner:CreateFields()
RETURN Self

METHOD SetRelation( oData, xKey, lAdditive ) CLASS SocketRdd
   LOCAL bKey, cKey
   DEFAULT lAdditive TO FALSE

   IF !lAdditive
      ::Request( "dbClearRel" )
   ENDIF

   IF oData != NIL
      bKey := xKey
      IF VALTYPE( xKey ) == "C"
         cKey := "{||"+xKey+"}"
         bKey := &cKey
         cKey := xKey
       ELSE
         cKey := NIL
      ENDIF
      ::Request( "dbSetRelation", oData:Alias, bKey, cKey )
   ENDIF
RETURN Self

METHOD Request( cFuncName, aParams ) CLASS SocketRdd
   LOCAL cData, cRecData, hSock, nSecs
   LOCAL lRet := .F., n, cSendStr := "SOCKETRDD|Request|"+::Owner:Alias+"|"+cFuncName
   DEFAULT aParams TO {}

   FOR n := 1 TO LEN( aParams )
       cSendStr += "|" + ValToPrgExp( aParams[n] )
   NEXT

   IF ! ::Owner:DesignMode .AND. ::Owner:Socket:Connected
      ::Owner:Socket:Send( cSendStr )

      hSock := ::Owner:Socket:Handle
      cRecData := ""

      nSecs := SECONDS()
      WHILE EMPTY( cRecData ) .AND. SECONDS() - nSecs < 10

         WHILE InetDataReady( hSock ) > 0
            cData := SPACE(1024)
            InetRecv( hSock, @cData, 1024 )
            IF ( n := AT( InetCRLF(), ALLTRIM( cData ) ) ) > 0
               cData := LEFT( ALLTRIM( cData ), n-1 )
            ENDIF
            IF EMPTY( cData )
               EXIT
            ENDIF
            cRecData += cData
         ENDDO

      ENDDO
      IF !EMPTY( cRecData )
         cRecData := &cRecData
       ELSE
         cRecData := NIL
      ENDIF
   ENDIF

RETURN cRecData

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS RddSQL
   DATA Owner           EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
/*
   METHOD RecLock()                           INLINE ::Request( "RLOCK" )
   METHOD FileLock()                          INLINE ::Request( "FLOCK" )
   METHOD UnLock()                            INLINE ::Request( "dbUnlock" )
   METHOD Bof()                               INLINE ::Request( "Bof" )
   METHOD Eof()                               INLINE ::Request( "Eof" )
   METHOD Deleted()                           INLINE ::Request( "Deleted" )
   METHOD RecCount()                          INLINE ::Request( "Reccount" )
   METHOD RecNo()                             INLINE ::Request( "Recno" )
   METHOD GoTop()                             INLINE ::Request( "dbgotop" )
   METHOD GoTo( nRec )                        INLINE ::Request( "dbgoto", {nRec} )
   METHOD GoBottom()                          INLINE ::Request( "dbgobottom" )
   METHOD Skip( n )                           INLINE ::Request( "dbSkip", {n} )
   METHOD Close()                             INLINE ::Request( "dbCloseArea" )
   METHOD OrdSetFocus( cOrder )               INLINE ::Request( "OrdSetFocus", {cOrder} )
   METHOD SetIndex( cIndex )                  INLINE ::Request( "dbSetIndex", {cIndex} )
   METHOD SetOrder( nOrder )                  INLINE ::Request( "dbSetOrder", {nOrder} )
   METHOD Select()                            INLINE ::Request( "Select" )
   METHOD Struct()                            INLINE ::Request( "dbStruct" )
   METHOD OrdKeyGoTo( nPos )                  INLINE ::Request( "OrdKeyGoTo", {nPos} )
   METHOD OrdKeyCount()                       INLINE ::Request( "OrdKeyCount" )
   METHOD FieldGet( nField )                  INLINE ::Request( "FieldGet", { nField } )
   METHOD Gather()                            INLINE ::Request( "Gather" )
   METHOD Scatter( aData )                    INLINE ::Request( "Scatter", {aData} )
   METHOD OrdKeyRelPos(n)                     INLINE ::Request( "OrdKeyRelPos", {n} )
   METHOD OrdKeyNo()                          INLINE ::Request( "OrdKeyNo" )
*/
ENDCLASS

METHOD Init( oOwner ) CLASS RddSQL
   ::Owner := oOwner
RETURN Self

METHOD Create() CLASS RddSQL
   IF ::Owner:SqlConnector != NIL .AND. ::Owner:SqlConnector:Sql != NIL
      ::Owner:Structure := ::Owner:SqlConnector:Sql:GetStruct( ::Owner:FileName )
      ::Owner:CreateFields()
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS Database INHERIT Component
   PROPERTY SqlConnector
   DATA Parent        EXPORTED
   METHOD Init() CONSTRUCTOR
ENDCLASS

METHOD Init( oOwner ) CLASS Database
   ::__xCtrlName   := "Database"
   ::ClsName       := "Database"
   ::ComponentType := "Database"
   ::Super:Init( oOwner )
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS SqlTable INHERIT DataTable
   DATA Socket             PROTECTED
   METHOD Init() CONSTRUCTOR
   METHOD Create()
ENDCLASS

METHOD Init( oOwner ) CLASS SqlTable
   ::__xCtrlName   := "SqlTable"
   ::ClsName       := "SqlTable"
   ::Super:Init( oOwner )
   ::ComponentType := "DataSource"
RETURN Self

METHOD Create( lIgnoreAO ) CLASS SqlTable
   ::Connector := RddSQL( Self )
   ::Connector:Create( lIgnoreAO )
RETURN Self

