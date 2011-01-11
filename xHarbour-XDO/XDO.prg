#pragma BEGINDUMP
   #define CLS_Name "xHarbour.DataObjects"
   #define CLS_ID   "{EA72B21F-33D4-4ab5-8A6F-C9C006C363B9}"

   #include "OleServer.h"
#pragma ENDDUMP

REQUEST HB_GT_NUL_DEFAULT

// If we intend to use WinAPI or any UI, like Alert().
//#include "what32.ch"

#include "hbclass.ch"

#define DLL_PROCESS_ATTACH  1
#define DLL_PROCESS_DETACH  0

/*
   XDO exports only one Function <Open( cDbf [, cRdd ] )

   The Open() function creates a Dynamic Class Wrapper for the given table,
   and returns an instance Object, which is thus returned as an OLE Server
   exposing all of it's PUBLIC Methods and Properties.
 */

/*
  OPTIONAL Procedure - Here we can create PUBLIC Variables to be exported
  and intialize any values. This Procedure will be called once when loading
  the Dll with nReason == DLL_PROCESS_ATTACH, and once again when unloading
  the Dll with nReason == DLL_PROCESS_DETACH.
*/
PROCEDURE DllMain( hInstance, nReason )

   (hInstance)

   SWITCH nReason
      CASE DLL_PROCESS_ATTACH
         //Alert( "Dll Loaded." )
         EXIT

      CASE DLL_PROCESS_DETACH
         //Alert( "Dll UNloaded." )
         EXIT

      DEFAULT
         TraceLog( "UNEXPECTED nReason in DllMain!" )
   END

RETURN

/*
  OPTIONAL FUNCTION - if exists it will set the Server to a specific Object Instance.
  or you may return the same Object previously created if you want all OLE intanaces
  to refer to just one Object.
*/
FUNCTION CreateInstance

   //Alert( "CreateInstance" )

   /*
     If you return an Object, it becomes *the* Server, otherwise the non ststic PROCEDURES and FUNCTIONS
     of this modules will be the exported Methods, and PUBLICS MEMVARS will be the exported Properties.
    */

RETURN NIL

CLASS XDO
   PROTECTED:
      DATA nRDD
      DATA nArea
      DATA nOrder INIT 0

   PUBLIC:
      METHOD CreateIndex( cTag, cExp )
      METHOD SetOrder( xOrder )
      METHOD Seek( xExp )
      METHOD Locate( cExp )
      METHOD Field( cField ) INLINE ( ::nArea )->( FieldGet( FieldPos( cField ) ) )
      METHOD RecNo() INLINE ( ::nArea )->( RecNo() )
      METHOD RecCount() INLINE ( ::nArea )->( RecCount() )
      METHOD GoTo( nRecNo ) INLINE ( ::nArea )->( dbGoto( nRecNo ) )
      METHOD GoTop() INLINE ( ::nArea )->( dbGoTop() )
      METHOD GoBottom() INLINE ( ::nArea )->( dbGoBottom() )
      METHOD Bof() INLINE ( ::nArea )->( Bof() )
      METHOD Eof() INLINE ( ::nArea )->( Eof() )
      METHOD Skip( nRecs ) INLINE IIF( nRecs == NIL, nRecs := 1, ), ( ::nArea )->( dbSkip( nRecs ) )
      METHOD Next() INLINE ( ::nArea )->( dbSkip( 1 ) )
      METHOD Previous() INLINE ( ::nArea )->( dbSkip( -1 ) )
      METHOD Delete() INLINE ( ::nArea )->( dbDelete() )
      METHOD Deleted() INLINE ( ::nArea )->( Deleted() )
      METHOD Recall() INLINE ( ::nArea )->( dbRecall() )
      METHOD Pack() INLINE ( ::nArea )->( __dbPack() )
      METHOD Zap() INLINE ( ::nArea )->( __dbZap() )
      METHOD CloseIndex( cTag )
      METHOD CloseIndexs()
      METHOD Close() INLINE ( ::nArea )->( dbCloseArea() )
      METHOD ReIndex() INLINE ( ::nArea )->( dbReindex() )
ENDCLASS

METHOD CreateIndex( cTag, cExp, lUnique ) CLASS XDO

   Select( ::nArea )
   SET ORDER TO 0

   IF ! HB_ISSTRING( cExp )
      cExp := cTag
   ENDIF

   IF ! HB_ISLOGICAL( lUnique )
      lUnique := .F.
   ENDIF

   TRY
      SET ORDER TO OrdNumber( cTag )
      ::nOrder := IndexOrd()
   CATCH
   END

   IF IndexOrd() > 0
      RETURN IndexOrd()
   ENDIF

   //TraceLog( ::nArea, Alias(), ::nRdd, cTag, cExp, lUnique )

   IF ::nRDD == 1
      ordCreate( Alias(), cTag, cExp, &( "{||" + cExp + "}" ), lUnique )
   ELSE
      ordCreate( cTag, NIL, cExp, &( "{||" + cExp + "}" ), lUnique )
   ENDIF

   ::nOrder := IndexOrd()

   //TraceLog( OrdKey() )

RETURN IndexOrd()

METHOD SetOrder( xOrder ) CLASS XDO

   Select( ::nArea )

   IF HB_ISNUMERIC( xOrder )
      SET ORDER TO ( xOrder )
   ELSE
      IF ::nRDD == 1
         SET ORDER TO TAG ( xOrder )
      ELSE
         SET INDEX TO ( xOrder )
      ENDIF
   ENDIF

   ::nOrder := IndexOrd()

RETURN Self

METHOD Seek( xExp ) CLASS XDO

   Select( ::nArea )
   SET ORDER TO ::nOrder

   //TraceLog( xExp, IndexOrd(), IndexKey(0) )

   SEEK xExp

   //TraceLog( RecNo(), Eof() )

RETURN Self

METHOD Locate( cExp ) CLASS XDO

   Select( ::nArea )

   LOCATE FOR &cExp

RETURN Self

METHOD CloseIndex( cTag ) CLASS XDO

   Select( ::nArea )

   IF ::nRDD == 1
      ordDestroy( cTag )
   ELSE
   ENDIF

RETURN self

METHOD CloseIndexs() CLASS XDO

   Select( ::nArea )

   ordListClear()

RETURN Self

FUNCTION Open( cDBF, cRDD )

   LOCAL nFields, oClass, nField, oXDO, oErr

   TraceLog( cDBF, cRDD )

   IF ! HB_ISSTRING( cRDD )
      cRDD := "DBFCDX"
   ENDIF

   TRY
      SELECT 0
      USE ( cDBF ) VIA cRDD

      oClass := HBClass():New( "XDO." + Alias(), "XDO" )

      nFields := FCount()
      FOR nField := 1 TO nFields
         oClass:AddInline( FieldName( nField ), EarlyFieldBlock( nField ), HB_OO_CLSTP_EXPORTED )
      NEXT

      oClass:Create()
      oXDO := oClass:Instance()

      oXDO:nRDD := IIF( cRDD == "DBFCDX", 1, 2 )
      oXDO:nArea := Select()

   CATCH oErr
      TraceLog( ValToPrg( oErr ) )
      RETURN oErr
      //Alert( oErr:Operation )
      //Alert( oErr:Description )
   END

RETURN oXDO

STATIC FUNCTION EarlyFieldBlock( nField )

RETURN {|Self, xVal| IIF( PCount() == 2, ( Self:nArea )->( FieldPut( nField, xVal ) ), ( Self:nArea )->( FieldGet( nField ) ) ) }
//RETURN {|Self, xVal| IIF( PCount() == 2, FieldPut( nField, xVal ), FieldGet( nField ) ) }