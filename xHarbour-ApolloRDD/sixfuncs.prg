/*
 *   Apollo RDD for Harbour
 *   Copyright 2002 Patrick Mast
 *
 *   Written by Alexander S.Kresin <alex@belacy.belgorod.su>, December 2002
 */

#include "ord.ch"
#include "set.ch"
#include "dbinfo.ch"
#include "Apollo.ch"

STATIC M6Temp:=""

Function SixNsx
Return sx_SetFileType( SDENSX )

Function Sxchar( nLen, Value )
   IF Value != Nil
      Return Padr( Value,nLen )
   ELSEIF nLen == Nil
      nLen := 10
   ENDIF
Return Space( nLen )

Function m6_Init()
Return .T.

Function m6_dbEval( bEval, cFor, bFor )
Local cFilter, nFilter

   IF cFor != Nil .AND. m6_IsOptimize( cFor ) > 0
      cFilter := dbFilter()
      nFilter := m6_GetCurrentFilter()
      IF bFor == Nil
         bFor := &( "{||" + cFor + "}" )
      ENDIF
      m6_SetFilter( bFor,cFor )
      dbEval( bEval, bFor )
      IF nFilter != 0
         m6_SetAreaFilter( nFilter )
      ELSEIF !Empty( cFilter )
         dbSetFilter( &( "{||" + cFilter + "}" ), cFilter )
      ELSE
         dbClearFilter()
      ENDIF
   ELSE
      dbEval( bEval, bFor )
   ENDIF

Return Nil

Function m6_IsFilter()
Return m6_IsOptimize( dbFilter() )

Function m6_ordCondSet( cFor, bFor, lAll, bWhile, bEval, nEvery, nRecno, nNext, nRec, lRest, lDescend, p12, p13, lUseCurrent, p15, p16, lAdditive )
   (p12)
   (p13)
   (p15)
   (p16)   
Return ordCondSet( cFor, bFor, lAll, bWhile, bEval, nEvery, nRecno, nNext, nRec, lRest, lDescend, lAdditive, lUseCurrent )

Function m6_ordCreate( cOrdBagName, cTagName, cKeyExpr, bKeyExpr, lUnique )
Return OrdCreate( cOrdBagName, cTagName, cKeyExpr, bKeyExpr, lUnique )

Function m6_RefreshFilter()
Return Nil

Function m6_SetFilter( bFilter, cFilter, lNoOptimize )
Local lOptimize

   IF lNoOptimize == Nil ; lNoOptimize := .F. ; ENDIF
   lOptimize := Set( _SET_OPTIMIZE,!lNoOptimize )

   dbSetFilter( bFilter, cFilter )

   Set( _SET_OPTIMIZE,lOptimize )

Return Nil

Function m6_SetTemp( cTempPath )
Local cPath:=M6Temp

  IF cPath==""
     cPath:=GetEnv( "M6TEMP" )
  ENDIF
  IF ValType( cTempPath ) <> "C"
    Return cPath
  ENDIF
  M6Temp:=cTempPath

Return cPath

Function M6_Version()
Return Sx_Version()

Function Sx_FileOrder()
Return Indexord()

Function Sx_IndexName( order )
Return OrdBagName( order )

Function Sx_IndexCount()
Return dbOrderInfo( DBOI_ORDERCOUNT )

Function Sx_KeyNo( order, filename )
Return OrdKeyNo( order,filename )

Function Sx_KeyCount( order, filename )
Return OrdKeyCount( order,filename )

Function Sx_KeyData( order, filename )
Return dbOrderInfo( DBOI_KEYVAL, order, filename )

Function Sx_MemoExt()
Return dbInfo( DBI_MEMOEXT )

Function Sx_MemoPack( nBlock )

   IF nBlock != Nil
      sx_SetMemoBlock( nBlock )
   ENDIF
   sx_Pack()

Return .T.

Function Sx_SeekLast( key, lSoft )
Return dbSeek( key, lSoft, .T. )

Function Sx_ClrScope( nScope )
Return OrdScope( nScope, Nil )

Function Sx_SetScope( nScope, value )
   IF Pcount() > 1
      Return OrdScope( nScope, value )
   ENDIF
Return OrdScope( nScope )

Function Sx_SetTag( order,filename )
Local cType := Valtype( order )

   IF Empty( alias() ) .OR. order == NIL
      Return .F.
   ELSEIF Empty( sx_IndexName(1) )
      Return .T.         
   ELSE
      IF cType == "N"
         IF dbOrderInfo( DBOI_ORDERCOUNT ) < order
            Return .F.
         ENDIF
         ordSetFocus( order,filename )
         Return ( order == Sx_TagNo() )
      ELSEIF cType == "C"
         ordSetFocus( order,filename )
         Return ( UPPER(order) == UPPER(Sx_TagName()) )
      ENDIF
   ENDIF

Return .F.

Function Sx_TagNo()
Return dbOrderInfo( DBOI_NUMBER )

Function Sx_TagName()
Return dbOrderInfo( DBOI_NAME )

Function Sx_TableName()
Local i, path := dbInfo( DBI_FULLPATH )

   IF ( i := Rat( "\",path ) ) != 0
      Return Substr( path,i+1 )
   ENDIF

Return path

Function Sx_TagInfo( cIndexFile )
Local i, nTags, nFirst
Local aInfo := {}

   nTags := sx_TagCount( cIndexFile, @nFirst )
   FOR i := 1 TO nTags
      SET ORDER TO nFirst+i-1
      Aadd( aInfo,{ sx_TagName(), OrdKey(), "", dbOrderInfo(DBOI_UNIQUE), ;
                      dbOrderInfo(DBOI_ISDESC), dbOrderInfo(DBOI_CUSTOM) } )
   NEXT

Return aInfo

Function Sx_WildSeek( cKey, lCont )
Local i := Min( At( '*',cKey ), At( '?',cKey ) ), cSubkey
Local cExp := Ordkey(), bExp

   __dbSetFound( .F. )
   IF Empty( cExp )
      Return .F.
   ENDIF
   bExp := &( "{||" + cExp + "}" )
   IF lCont == Nil ; lCont := .F. ; ENDIF
   IF i == 0
      IF lCont
         SKIP
         DO WHILE !Sx_WildMatch( cKey, Eval(bExp) )
            SKIP
         ENDDO
         IF !Eof()
            __dbSetFound( .F. )
            Return .T.
         ENDIF
      ELSE
         Return dbSeek( cKey )
      ENDIF
   ELSEIF i == 1
      IF !lCont
         GO TOP
      ELSE
         SKIP
      ENDIF
      DO WHILE !Sx_WildMatch( cKey, Eval(bExp) )
         SKIP
      ENDDO
      IF !Eof()
         __dbSetFound( .F. )
         Return .T.
      ENDIF
   ELSE
      cSubkey := Left( cKey,i-1 )
      IF lCont
         IF Left( Eval( bExp ),i-1 ) < cSubKey
            IF !dbSeek( cSubkey )
               Return .F.
            ENDIF
         ELSEIF Left( Eval( bExp ),i-1 ) > cSubKey
            Return .F.
         ENDIF
         SKIP
      ELSE
         IF !dbSeek( cSubkey )
            Return .F.
         ENDIF
      ENDIF
      DO WHILE !Sx_WildMatch( cKey, Eval(bExp) )
         SKIP
      ENDDO
      IF !Eof()
         __dbSetFound( .F. )
         Return .T.
      ENDIF
   ENDIF

Return .F.