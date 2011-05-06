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

REQUEST HB_MEMIO

//-------------------------------------------------------------------------------------------------------

CLASS DataTable INHERIT Component
   PROPERTY Alias          READ xAlias          WRITE SetAlias
   PROPERTY FileName       READ xFileName       WRITE SetFileName

   ACCESS BindingSource    INLINE ::SqlConnector
   ASSIGN BindingSource(o) INLINE ::SqlConnector := o

   ASSIGN DataConnector(o) INLINE ::BindingSource := o
   
   DATA Shared             PUBLISHED INIT .T.
   DATA ReadOnly           PUBLISHED INIT .F.
   DATA AutoOpen           PUBLISHED INIT .T.
   DATA SqlConnector       PUBLISHED
   DATA Path               PUBLISHED INIT ""
   DATA CodePage           PUBLISHED
   
   DATA Socket             PUBLISHED

   DATA __lMemory          EXPORTED  INIT .F.
   DATA Structure          PUBLISHED

   DATA IDEButton          EXPORTED
   DATA Parent             EXPORTED
   DATA Fields             EXPORTED
   DATA IndexOrder         EXPORTED  INIT 0

   DATA Table              PUBLISHED  INIT {}
   DATA Border             EXPORTED
   DATA Caption            EXPORTED
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
   DATA ClientEdge         EXPORTED
   DATA ClipChildren       EXPORTED
   DATA ClipSiblings       EXPORTED
   DATA OwnerDraw          EXPORTED  INIT .F.
   DATA StaticEdge         EXPORTED
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

   DATA Area               EXPORTED
   DATA __ExplorerFilter   EXPORTED  INIT { { "DataTable *.dbf", "*.dbf" } }
   DATA aScatter           EXPORTED
   
   DATA Bitmap             PROTECTED
   DATA MDIClient          PROTECTED
   DATA xDriver            PROTECTED INIT RddSetDefault()
   
   DATA Connector          EXPORTED
   
   ACCESS Driver           INLINE ::xDriver PERSISTENT
   ASSIGN Driver(c)        INLINE ::xDriver := IIF( !EMPTY( c ), c, NIL )
   ACCESS Exists           INLINE ::IsOpen

   METHOD Init() CONSTRUCTOR

   METHOD Open()
   METHOD CreateFields()
   METHOD Create()
   METHOD Gather()                            INLINE ::Connector:Gather()
   METHOD Scatter( aData )                    INLINE ::Connector:Scatter( aData )
   METHOD SetTopScope( xScope )               INLINE ::Connector:SetTopScope( xScope )
   METHOD SetBottomScope( xScope )            INLINE ::Connector:SetBottomScope( xScope )
   METHOD KillScope()                         INLINE ::Connector:KillScope()
   METHOD Reindex()                           INLINE ::Connector:Reindex()   
   METHOD Commit()                            INLINE ::Connector:Commit()    
   METHOD RecLock()                           INLINE ::Connector:RecLock()   
   METHOD FileLock()                          INLINE ::Connector:FileLock()  
   METHOD UnLock()                            INLINE ::Connector:UnLock()    
   METHOD UnLockAll()                         INLINE ::Connector:UnLockAll() 
   METHOD Bof()                               INLINE ::Connector:Bof()       
   METHOD Eof()                               INLINE ::Connector:Eof()       
   METHOD Deleted()                           INLINE ::Connector:Deleted()   
   METHOD RecCount()                          INLINE ::Connector:RecCount()  
   METHOD RecNo()                             INLINE ::Connector:RecNo()     
   METHOD GoTop()                             INLINE ::Connector:GoTop()     
   METHOD GoTo( nRec )                        INLINE ::Connector:GoTo( nRec )
   METHOD GoBottom()                          INLINE ::Connector:GoBottom()  
   METHOD Skip( n )                           INLINE ::Connector:Skip( n )   
   METHOD Close(lNotify)                      INLINE ::Connector:Close(), ::Structure := NIL, ::Fields := NIL,;
                                                                      IIF( VALTYPE( lNotify ) == "L" .AND. lNotify, __Evaluate( ::bOnFileClosed, Self ), ),;
                                                                      ExecuteEvent( "OnClose", Self )
   METHOD Append()                            INLINE ::Connector:Append()
   METHOD OrdSetFocus( cOrder )               INLINE IIF( cOrder != NIL, ::IndexOrder := ::IndexOrd(),), ::Connector:OrdSetFocus( cOrder )
   METHOD SetIndex( cIndex )                  INLINE ::IndexOrder := ::IndexOrd(), ::Connector:SetIndex( cIndex )
   METHOD SetRelation()                       INLINE ::Connector:SetRelation()
   METHOD SetOrder( nOrder )                  INLINE ::Connector:SetOrder( nOrder )
   METHOD Select()                            INLINE ::Connector:Select()
   METHOD SelectArea()                        INLINE ::Connector:SelectArea()
   METHOD Delete()                            INLINE ::Connector:Delete()
   METHOD Recall()                            INLINE ::Connector:Recall()
   METHOD Seek( xKey, lSoft )                 INLINE ::Connector:Seek( xKey, lSoft )
   METHOD Found()                             INLINE ::Connector:Found()
   METHOD SetDriver( cDriver )                INLINE ::Driver := cDriver, ::Connector:SetDriver( cDriver )
   METHOD CreateIndex( cName, xKey, lUnique ) INLINE ::Connector:CreateIndex( cName, xKey, lUnique )
   
   METHOD IndexOrd()                          INLINE ::Connector:IndexOrd()
   METHOD Zap()                               INLINE ::Connector:Zap()
   METHOD OrdName(n)                          INLINE ::Connector:OrdName(n)
   METHOD OrdKey(n)                           INLINE ::Connector:OrdKey(n)
   METHOD Struct()                            INLINE ::Connector:Struct()
   METHOD OrdKeyGoTo( nPos )                  INLINE ::Connector:OrdKeyGoTo( nPos )
   METHOD SetFilter( b, c )                   INLINE ::Connector:SetFilter( b, c )
   METHOD OrdKeyCount()                       INLINE ::Connector:OrdKeyCount()
   METHOD Used()                              INLINE ::Connector:Used()

   METHOD FieldPut( nField, xVal )            INLINE ::Connector:FieldPut( nField, xVal )
   METHOD FieldGet( nField )                  INLINE ::Connector:FieldGet( nField )

   METHOD OrdKeyRelPos(n)                     INLINE ::Connector:OrdKeyRelPos( n )
   METHOD OrdKeyNo()                          INLINE ::Connector:OrdKeyNo()

   METHOD Connectorescend(cnOrder,cFile,lDescend ) INLINE ::Connector:OrdDescend( cnOrder, cFile, lDescend )
   
   METHOD CheckAlias()
   METHOD AdsSetServerType(n)   VIRTUAL
   
   METHOD Destroy(lNotify)                    INLINE IIF( ::IsOpen, ::Close(lNotify),), ::Super:Destroy()
   METHOD SetAlias()
   METHOD SetFileName()
   METHOD CreateOrder()
   ACCESS IsOpen INLINE ::Structure != NIL //Select( ::Area ) > 0
   METHOD FromAlias()
   METHOD Insert() INLINE ::Append()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS DataTable
   DEFAULT ::__xCtrlName TO "DataTable"
   DEFAULT ::ClsName     TO "DataTable"
   ::ComponentType := "DataSource"
   ::Super:Init( oOwner )
   ::Connector := DataRdd( Self )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD FromAlias( cAlias )
   IF cAlias != NIL .AND. (cAlias)->( Used() )
      IF ::Fields != NIL
         ::Fields := NIL
      ENDIF
      ::Connector := DataRdd( Self )
      ::xAlias    := cAlias
      ::Area      := ::Select()
      ::Structure := ::Struct()
      ::CreateFields()
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create( lIgnoreAO ) CLASS DataTable
   IF VALTYPE( ::Socket ) == "C" .AND. ASCAN( ::Form:Property:Keys, {|c| UPPER(c) == UPPER(::Socket) } ) > 0
      ::Socket := ::Form:Property[ ::Socket ]
      IF ::__ClassInst == NIL
         ::Connector := SocketRdd( Self )
      ENDIF
   ENDIF
   ::Connector:Create( lIgnoreAO )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD CreateFields() CLASS DataTable
   LOCAL n, aField
   LOCAL hClass, cField

   hClass := __ClsNew( "DATA_" + ::Alias, 0, 0, { Data():ClassH } )

   FOR EACH aField IN ::Structure
      cField := aField[1]
      __clsAddMsg( hClass,       cField, &( "{|Self| ::FieldGet( " + Str( HB_EnumIndex() ) + " ) }" ), HB_OO_MSG_INLINE, NIL, 1 )
      __clsAddMsg( hClass, "_" + cField, &( "{|Self, xVal| ::FieldPut( " + Str( HB_EnumIndex() ) + ", xVal ) }" ), HB_OO_MSG_INLINE, NIL, 1 )
   NEXT

   ::CheckAlias()
   
   ::Fields := __clsInst( hClass ):Init( Self )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD CreateOrder( cOrderBagName, cTag, cKey, cFor, bFor, bWhile, bEval, nEvery, nRecNo, nNext, nRecord, lRest, lUnique, lAll )
   (::Area)->( OrdCondSet( cFor, bFor, lAll, bWhile, bEval, nEvery, nRecNo, nNext, nRecord, lRest ) )
   (::Area)->( OrdCreate( cOrderBagName, cTag, cKey,, lUnique ) )
RETURN Self
   
//-------------------------------------------------------------------------------------------------------
METHOD Open() CLASS DataTable
   ::Create(.T.)
RETURN ::lCreated

//-------------------------------------------------------------------------------------------------------
METHOD CheckAlias() CLASS DataTable
   LOCAL n, aField
   LOCAL hClass, cField

   IF ::xAlias == NIL
      ::xAlias := ::Alias()
      ::Area   := ::Select()
   ENDIF

   IF ::xAlias != NIL
      IF Select( ::xAlias ) > 0 .AND. ::Structure == NIL
         IF !::__lMemory
            ::xFileName  := (::Area)->( dbInfo( DBI_FULLPATH ) )
            n := RAT( "\", ::xFileName )
            ::xFileName := SUBSTR( ::xFileName, n+1 )
            ::Path     := SUBSTR( ::xFileName, 1, n-1 )
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

METHOD SetAlias( cAlias ) CLASS DataTable
   LOCAL lCreate := .F.
   ::xAlias := cAlias
   IF !::__lMemory
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
   
   IF ::lCreated
      ::Create()
      IF ::bOnFileNameChanged != NIL
         EVAL( ::bOnFileNameChanged, Self )
      ENDIF
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS Data
   DATA Parent
   DATA __ClassInst
   
   METHOD Init() CONSTRUCTOR
   METHOD Put()
   METHOD FieldPut( nField, xVal )  INLINE ::Parent:FieldPut( nField, xVal )
   METHOD FieldGet( nField )        INLINE ::Parent:FieldGet( nField )
   METHOD FieldName( nField )       INLINE ::Parent:FieldName( nField )
ENDCLASS

//-------------------------------------------------------------------------------------------------------

METHOD Init( o ) CLASS Data
   ::Parent := o
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Put(xVal, cName) CLASS Data
   LOCAL cRet

   IF xVal != NIL
      (::Parent:Alias)->&cName := xVal
      //__ObjSetValueList( Self, { { "x"+cName, xVal } } )
    ELSE
      RETURN (::Parent:Alias)->&cName
      //RETURN &("QSelf():x"+cName)
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS DataRdd
   DATA Owner           EXPORTED
   
   METHOD Init() CONSTRUCTOR
   METHOD Create()
   
   METHOD SetTopScope( xScope )               INLINE (::Owner:Alias)->( OrdScope( TOPSCOPE, xScope ) )
   METHOD SetBottomScope( xScope )            INLINE (::Owner:Alias)->( OrdScope( BOTTOMSCOPE, xScope ) )
   METHOD KillScope()                         INLINE (::Owner:Alias)->( OrdScope( TOPSCOPE, NIL ) ),;
                                                     (::Owner:Alias)->( OrdScope( BOTTOMSCOPE, NIL ) )
   METHOD Alias()                             INLINE (::Owner:Alias)->( Alias() )
   METHOD Reindex()                           INLINE (::Owner:Alias)->( dbReindex() )
   METHOD Commit()                            INLINE (::Owner:Alias)->( dbCommit() )
   METHOD RecLock()                           INLINE (::Owner:Alias)->( RLOCK() )
   METHOD FileLock()                          INLINE (::Owner:Alias)->( FLOCK() )
   METHOD UnLock()                            INLINE (::Owner:Alias)->( dbUnlock() )
   METHOD UnLockAll()                         INLINE (::Owner:Alias)->( dbUnlockAll() )
   METHOD Bof()                               INLINE (::Owner:Alias)->( Bof() )
   METHOD Eof()                               INLINE (::Owner:Alias)->( Eof() )
   METHOD Deleted()                           INLINE (::Owner:Alias)->( Deleted() )
   METHOD RecCount()                          INLINE (::Owner:Alias)->( Reccount() )
   METHOD RecNo()                             INLINE (::Owner:Alias)->( Recno() )
   METHOD GoTop()                             INLINE (::Owner:Alias)->( dbgotop() )
   METHOD GoTo( nRec )                        INLINE (::Owner:Alias)->( dbgoto( nRec ) )
   METHOD GoBottom()                          INLINE (::Owner:Alias)->( dbgobottom() )
   METHOD Skip( n )                           INLINE (::Owner:Alias)->( dbSkip( n ) )
   METHOD Close()                             INLINE (::Owner:Alias)->( dbCloseArea() )
   METHOD Append()                            INLINE (::Owner:Alias)->( dbAppend() )
   METHOD OrdSetFocus( cOrder )               INLINE (::Owner:Alias)->( OrdSetFocus( cOrder ) )
   METHOD SetIndex( cIndex )                  INLINE (::Owner:Alias)->( dbSetIndex( cIndex ) )
   METHOD SetOrder( nOrder )                  INLINE (::Owner:Alias)->( dbSetOrder( nOrder ) )
   METHOD Select()                            INLINE (::Owner:Alias)->( Select() )
   METHOD SelectArea()                        INLINE dbSelectArea( ::Owner:Area )
   METHOD Delete()                            INLINE (::Owner:Alias)->( dbDelete() )
   METHOD Recall()                            INLINE (::Owner:Alias)->( dbRecall() )
   METHOD Seek( xKey, lSoft )                 INLINE (::Owner:Alias)->( dbSeek( xKey, lSoft ) )
   METHOD Found()                             INLINE (::Owner:Alias)->( Found() )
   METHOD SetDriver( cDriver )                INLINE (::Owner:Alias)->( dbSetDriver( cDriver ) )
   METHOD CreateIndex( cName, xKey, lUnique ) INLINE (::Owner:Alias)->( dbCreateIndex( cName, xKey, &("{||"+xKey+"}"), lUnique ) )
   METHOD IndexOrd()                          INLINE (::Owner:Alias)->( IndexOrd() )
   METHOD Zap()                               INLINE (::Owner:Alias)->( __dbZap() )
   METHOD OrdName(n)                          INLINE (::Owner:Alias)->( OrdName(n) )
   METHOD OrdKey(n)                           INLINE (::Owner:Alias)->( OrdKey(n) )
   METHOD Struct()                            INLINE (::Owner:Alias)->( dbStruct() )
   METHOD OrdKeyGoTo( nPos )                  INLINE (::Owner:Alias)->( OrdKeyGoTo( nPos ) )
   METHOD SetFilter( b, c )                   INLINE (::Owner:Alias)->( dbSetFilter( b, c ) )
   METHOD OrdKeyCount()                       INLINE (::Owner:Alias)->( OrdKeyCount() )
   METHOD Used()                              INLINE (::Owner:Alias)->( Used() )
   METHOD OrdDescend(cnOrder,cFile,lDescend ) INLINE (::Owner:Alias)->( OrdDescend( cnOrder, cFile, lDescend ) )
   METHOD SetRelation()
   METHOD FieldPut( nField, xVal )            INLINE (::Owner:Alias)->( FieldPut( nField, xVal ) )
   METHOD FieldGet( nField )                  INLINE (::Owner:Alias)->( FieldGet( nField ) )
   METHOD Gather()
   METHOD Scatter( aData )
   METHOD OrdKeyRelPos(n)
   METHOD OrdKeyNo()

ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS DataRdd
   ::Owner := oOwner
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD OrdKeyRelPos(n) CLASS DataRdd
   LOCAL nRelPos := (::Owner:Alias)->( OrdKeyRelPos(n) )
   DEFAULT nRelPos TO (::Owner:Alias)->( OrdKeyNo() )
RETURN nRelPos

//-------------------------------------------------------------------------------------------------------
METHOD OrdKeyNo() CLASS DataRdd
   LOCAL nPos := (::Owner:Alias)->( OrdKeyNo() )
   DEFAULT nPos TO (::Owner:Alias)->( Recno() )
RETURN nPos

//-------------------------------------------------------------------------------------------------------
METHOD SetRelation( oData, xKey, lAdditive ) CLASS DataRdd
   LOCAL bKey, cKey
   DEFAULT lAdditive TO FALSE
   
   IF !lAdditive
      (::Owner:Alias)->( dbClearRel() )
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
      (::Owner:Alias)->( dbSetRelation( oData:Alias, bKey, cKey ) )
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Create( lIgnoreAO ) CLASS DataRdd

   LOCAL nSecs, nAlias, cAlias
   LOCAL oErr
   LOCAL cEvent, n, nServer, cFile, lDef := .T.
   DEFAULT lIgnoreAO TO .F.
   
   IF VALTYPE( ::Owner:SqlConnector ) == "C"
      RETURN .F.
   ENDIF
   
   IF ( ::Owner:AutoOpen .OR. lIgnoreAO ) .AND. ( ( !::Owner:IsOpen .AND. !EMPTY( ::Owner:FileName ) ) .OR. ::Owner:__lMemory )
      ExecuteEvent( "OnInit", ::Owner )
      
      IF !::Owner:__lMemory
         IF ::Owner:Driver IN { "SQLRDD", "SQLEX" }
            IF ::Owner:__ClassInst != NIL
               RETURN ::Owner
            ENDIF
            cFile := ::Owner:FileName
          ELSEIF !EMPTY( ALLTRIM( ::Owner:Path ) )
            cFile := ALLTRIM( ::Owner:Path ) + "\" + ::Owner:FileName
            IF !FILE( cFile )
               IF ::Owner:__ClassInst == NIL
                  Throw( ErrorNew( "DataTable", 21, 1010, ::Owner:FileName, "The specified file could not be found", EF_CANRETRY | EF_CANDEFAULT ) )
                ELSE
                  ::Owner:Application:MainForm:MessageBox( "The specified file could not be found", ::Owner:FileName, "Error" )
               ENDIF
               RETURN ::Owner
            ENDIF
          ELSEIF ( n := RAT( "\", ::Owner:FileName ) ) > 0
            cFile := ALLTRIM( ::Owner:FileName )
            IF !FILE( cFile )
               IF ::Owner:__ClassInst == NIL
                  Throw( ErrorNew( "DataTable", 21, 1010, ::Owner:FileName, "The specified file could not be found", EF_CANRETRY | EF_CANDEFAULT ) )
                ELSE
                  ::Owner:Application:MainForm:MessageBox( "The specified file could not be found", ::Owner:FileName, "Error" )
               ENDIF
               RETURN ::Owner
            ENDIF
          ELSE
            IF ::Owner:__ClassInst != NIL
               cFile := ::Owner:Application:Project:AppObject:SetDefault
               IF EMPTY( cFile )
                  lDef := .F.
                  cFile := NIL
               ENDIF
               DEFAULT cFile TO ::Owner:Application:Project:Properties:Path + "\" + ::Owner:Application:Project:Properties:Binary
               cFile += ( "\" + ::Owner:FileName )

               IF !FILE( cFile ) .AND. !lDef
                  RETURN ::Owner
               ENDIF
             ELSE
               cFile := ::Owner:Application:SetDefault
               IF EMPTY( cFile )
                  cFile := NIL
               ENDIF
               DEFAULT cFile TO ::Owner:Application:Path
               cFile += ( "\" + ::Owner:FileName )
            ENDIF
         ENDIF
      ENDIF
      IF HGetPos( ::Owner:EventHandler, "OnCreate" ) != 0
         cEvent := ::Owner:EventHandler[ "OnCreate" ]
         IF __objHasMsg( ::Owner:Form, cEvent )
            ::Owner:Form:&cEvent( ::Owner )
         ENDIF
      ENDIF

      IF !::Owner:__lMemory .AND. ::Owner:Driver != NIL .AND. ::Owner:Driver != NIL .AND. UPPER( ::Owner:Driver ) $ "ADSCDXADSNTXADSADT"
         nServer := ::Owner:AdsSetServerType( ::Owner:ServerType )
      ENDIF
      
      IF ::Owner:__lMemory
         cFile := "mem:"+::Owner:Name
         IF EMPTY( ::Owner:Structure )
            RETURN ::Owner
         ENDIF
         IF !FILE( cFile )
            TRY
               dbCreate( cFile, ::Owner:Structure, ::Owner:Driver )
               CLOSE
            CATCH
               RETURN ::Owner
            END
         ENDIF
      ENDIF
      
      nAlias := 1
      cAlias := ::Owner:xAlias
      IF Select( cAlias ) > 0
         WHILE Select( cAlias + XSTR( nAlias ) ) > 0
            nAlias ++
         ENDDO
         ::Owner:xAlias := cAlias + XSTR( nAlias )
      ENDIF
      TRY
         dbUseArea( .T., ::Owner:Driver, cFile, ::Owner:Alias, ::Owner:Shared, ::Owner:ReadOnly, ::Owner:CodePage, IIF( ::Owner:SqlConnector != NIL, ::Owner:SqlConnector:ConnectionID, ) )
       CATCH oErr
         n := NIL
         IF oErr:GenCode == 21 .AND. oErr:SubCode IN { 6060, 6420 } .AND. ::Owner:__ClassInst != NIL
            //   6060  Advantage Database Server not started/loaded on specified server
            //   6420  Unable to "discover" the Advantage Database Server
            IF MessageBox(0, "Error " + alltrim(str(oErr:SubCode)) + ": Advantage Server Not Found"+CHR(13)+;
                             "Use local server instead?", "Open Error", MB_YESNO | MB_ICONQUESTION ) == IDYES
               n := ::Owner:AdsSetServerType(1)
            ENDIF
         ENDIF
         IF oErr:GenCode == 18 .OR. oErr:GenCode == 17
            ::Owner:xAlias := ::Owner:xAlias + "1"//::Owner:Name
         ENDIF
         dbUseArea( .T., ::Owner:Driver, cFile, ::Owner:Alias, ::Owner:Shared, ::Owner:ReadOnly, ::Owner:CodePage, IIF( ::Owner:SqlConnector != NIL, ::Owner:SqlConnector:ConnectionID, ) )
         IF !::Owner:__lMemory .AND. n != NIL
            ::Owner:AdsSetServerType(n)
         ENDIF
      END
      IF !::Owner:__lMemory .AND. NETERR()
         ::Owner:Error := oErr
         ExecuteEvent( "OnError", ::Owner )
         RETURN .F.
      ENDIF

      IF !::Owner:__lMemory .AND. nServer != NIL
         ::Owner:AdsSetServerType( nServer )
      ENDIF

      DEFAULT ::Owner:xAlias TO Alias()

      ::Owner:Area := Select()
      ::Owner:Structure := (::Owner:Alias)->( dbStruct() )
      
      AEVAL( ::Owner:Structure, {|a,n| ASIZE( ::Owner:Structure[n], 4 )} )
      
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
   aEval( ::Owner:aScatter, {|a,n| ::Owner:aScatter[n] := (::Owner:Alias)->( FieldGet(n) ) } )
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD Gather( aData ) CLASS DataRdd
   aEval( ::Owner:aScatter, {|a,n| (::Owner:Alias)->( FieldPut(n, ::Owner:aScatter[n] ) ) } )
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
   METHOD OrdName(n)                          INLINE ::Request( "OrdName", {n} )
   METHOD OrdKey(n)                           INLINE ::Request( "OrdKey", {n} )
   METHOD Struct()                            INLINE ::Request( "dbStruct" )
   METHOD OrdKeyGoTo( nPos )                  INLINE ::Request( "OrdKeyGoTo", {nPos} )
   METHOD SetFilter( b, c )                   INLINE ::Request( "dbSetFilter", {b, c} )
   METHOD OrdKeyCount()                       INLINE ::Request( "OrdKeyCount" )
   METHOD Used()                              INLINE ::Request( "Used" )
   METHOD OrdDescend(cnOrder,cFile,lDescend ) INLINE ::Request( "OrdDescend", {cnOrder, cFile, lDescend} )
   METHOD SetTopScope( xScope )               INLINE ::Request( "OrdScope", {TOPSCOPE, xScope} )
   METHOD SetBottomScope( xScope )            INLINE ::Request( "OrdScope", {BOTTOMSCOPE, xScope} )
   METHOD KillScope()                         INLINE ::Request( "OrdScope", {TOPSCOPE, NIL} ),;
                                                     ::Request( "OrdScope", {BOTTOMSCOPE, NIL} )
   METHOD FieldPut( nField, xVal )            INLINE ::Request( "FieldPut", { nField, xVal } )
   METHOD FieldGet( nField )                  INLINE ::Request( "FieldGet", { nField } )
   METHOD Gather()                            INLINE ::Request( "Gather" ) 
   METHOD Scatter( aData )                    INLINE ::Request( "Scatter", {aData} ) 
   METHOD SetRelation()
   METHOD Request()
   METHOD OrdKeyRelPos(n)                     INLINE ::Request( "OrdKeyRelPos", {n} ) 
   METHOD OrdKeyNo()                          INLINE ::Request( "OrdKeyNo" ) 
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
   LOCAL nSent, cData, cRecData, hSock, nSecs
   LOCAL lRet := .F., n, cSendStr := "SOCKETRDD|Request|"+::Owner:Alias+"|"+cFuncName
   DEFAULT aParams TO {}
   
   FOR n := 1 TO LEN( aParams )
       cSendStr += "|" + ValToPrgExp( aParams[n] )
   NEXT
   
   IF ::Owner:__ClassInst == NIL .AND. ::Owner:Socket:Connected
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
   DATA Parent        EXPORTED
   DATA SqlConnector PUBLISHED
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

