/*
 * $Id: cstruct.prg,v 1.3 2002/06/14 18:00:10 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 * C Structure Support.
 *
 * Copyright 2000 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hboo.ch"
#include "cstruct.ch"

#define CLASS_PROPERTIES 5

static s_aActiveStructure
static s_aClasses := {}
static s_aArrayClasses := {}

//---------------------------------------------------------------------------//
Function __ActiveStructure( cStructure, nAlign )

   IF PCount() == 2
      cStructure := Upper( cStructure )

      IF aScan( s_aClasses, { | aClassInfo | IIF( aClassInfo[1] == cStructure, .T., .F. ) } ) == 0
         //TraceLog( "Registered: " + cStructure )
         aAdd( s_aClasses, { cStructure, NIL, {}, IIF( ValType( nAlign ) == "N", nAlign, 8 ) } )
      END

      s_aActiveStructure := {}
   ENDIF

Return s_aActiveStructure

//---------------------------------------------------------------------------//
Function HB_Member( aDefinition )

  LOCAL cMember := aDefinition[1], nLen, nAt

  IF cMember[-1] == "]"
     nAt := At( "[", cMember )
     nLen := Val( SubStr( cMember, nAt + 1, Len( cMember ) ) )

     aDefinition[1] := Left( cMember, nAt - 1 )
     aDefinition[2] := HB_CTypeArrayID( aDefinition[2], nLen )
  ENDIF

Return aDefinition

//---------------------------------------------------------------------------//
Function HB_CStructureID( cStructure, lInplace )

   cStructure := Upper( cStructure )

Return aScan( s_aClasses, { | aClassInfo | IIF( aClassInfo[1] == cStructure, .T., .F. ) } ) + IIF( lInplace, CTYPE_STRUCTURE, CTYPE_STRUCTURE_PTR )

Function HB_CStructure( cStructure, aStructure, nAlign )

   LOCAL nLen := Len( aStructure )
   LOCAL hClass
   LOCAL oStructure
   LOCAL Counter
   LOCAL nID
   LOCAL acTypes
   LOCAL aMemberDefinition

   cStructure := Upper( cStructure )
   nID        := aScan( s_aClasses, { | aClassInfo | IIF( aClassInfo[1] == cStructure, .T., .F. ) } )

   IF nId == 0
      Alert( "Class: " + cStructure + " was not initialized with __ActiveStructure()" )
      Return NIL
   ENDIF

   acTypes := s_aClasses[nID][3]

   IF s_aClasses[nID][2] == NIL

      //TraceLog( "Created: " + Str( nId ) )

      hClass := __clsNew( "C Structure " + cStructure, nLen + CLASS_PROPERTIES )

      s_aClasses[nID][2] := hClass

      __clsAddMsg( hClass,  "Reset"     , @Reset()      , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Buffer"    , @Buffer()     , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Value"     , @Value()      , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "DeValue"   , @DeValue()    , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Array"     , @ArrayMethod(), HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "SayMembers", @SayMembers() , HB_OO_MSG_METHOD )

      Counter := 1
      FOR EACH aMemberDefinition IN aStructure
         __clsAddMsg( hClass,       aMemberDefinition[1], Counter  , HB_OO_MSG_DATA )
         __clsAddMsg( hClass, "_" + aMemberDefinition[1], Counter++, HB_OO_MSG_DATA )
      NEXT

      __clsAddMsg( hClass,  "aCTypes"       , Counter, HB_OO_MSG_DATA )
      __clsAddMsg( hClass, "_aCTypes"       , Counter, HB_OO_MSG_DATA )

      Counter++

      __clsAddMsg( hClass,  "nAlign"        , Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass, "_nAlign"        , Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )

      Counter++

      __clsAddMsg( hClass,  "SizeOf"        , Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass, "_SizeOf"        , Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )

      Counter++

      __clsAddMsg( hClass,  "nID", Counter, HB_OO_MSG_DATA )
      __clsAddMsg( hClass, "_nID", Counter, HB_OO_MSG_DATA )

      // WARNING InternalBuffer *MUST* remain the *LAST* Property!!!
      Counter++

      __clsAddMsg( hClass,  "InternalBuffer", Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass, "_InternalBuffer", Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )

      FOR EACH aMemberDefinition IN aStructure
         aAdd( aCTypes, aMemberDefinition[2] )
      NEXT

      //TraceLog( Len( acTypes ), acTypes[1], acTypes )

   ELSE

      hClass := s_aClasses[nId][2]

      //TraceLog( "Reused: " + Str( nId ) )

   ENDIF

   oStructure := __clsInst( hClass )

   // All instances will share the same definitions array.
   oStructure:aCTypes := acTypes // same as: s_aClasses[nID][3]

   oStructure:nAlign := IIF( ValType( nAlign ) == "N", nAlign, s_aClasses[nID][4] )

   oStructure:SizeOf := HB_SizeOfCStructure( acTypes, oStructure:nAlign )

   oStructure:nId := nID + CTYPE_STRUCTURE

   AllocateMembers( oStructure )

   //TraceLog( "ID: " + Str( oStructure:nID ) )

Return oStructure

//---------------------------------------------------------------------------//
static Procedure AllocateMembers( oStructure )

   LOCAL aCTypes := oStructure:acTypes, CType, Counter := 1

   //TraceLog( "Scaning: " + oStructure:ClassName )

   FOR EACH CType IN aCTypes
      IF CType > CTYPE_STRUCTURE .AND. CType < CTYPE_STRUCTURE_PTR
         oStructure[Counter] := HB_CStructureFromID( CType, , .F. )
         AllocateMembers( oStructure[Counter] )
      ENDIF

      Counter++
   NEXT

   //TraceLog( "Finished: " + oStructure:ClassName )

Return

//---------------------------------------------------------------------------//
Function HB_CStructureFromID( nID, nAlign )

   LOCAL hClass, oStructure, lInplace
   LOCAL Counter, CType

   //TraceLog( nId, s_aClasses )

   IF nID > CTYPE_STRUCTURE_PTR
      lInplace := .F.
      nID -= CTYPE_STRUCTURE_PTR
   ELSEIF nID > CTYPE_STRUCTURE
      lInplace := .T.
      nID -= CTYPE_STRUCTURE
   ELSE
      Alert( "ID out of range!" )
      Return NIL
   ENDIF

   IF s_aClasses[nID][2] == NIL
      Alert( "Invalid ID: " + Str( nID ) )
      Return NIL
   ELSE
      hClass := s_aClasses[nId][2]
   ENDIF

   oStructure := __clsInst( hClass )

   // All instances will share the same definitions array.
   oStructure:aCTypes := s_aClasses[nID][3]

   oStructure:nAlign := IIF( ValType( nAlign ) == "N", nAlign, s_aClasses[nID][4] )

   oStructure:SizeOf := HB_SizeOfCStructure( oStructure:acTypes, oStructure:nAlign )

   oStructure:nID = nID + IIF( lInplace, CTYPE_STRUCTURE, CTYPE_STRUCTURE_PTR )

Return oStructure

//---------------------------------------------------------------------------//
Function HB_CTypeArrayID( CType, nLen )

   LOCAL hClass
   LOCAL Counter
   LOCAL nID
   LOCAL acTypes
   LOCAL cArrayClassName := "C Array of [" + LTrim( Str( nLen ) ) + "] CType: " + Str( CType )

   nID := aScan( s_aArrayClasses, { | aArrayDefinitions | aArrayDefinitions[1] == CType .AND. aArrayDefinitions[2] == nLen } )

   IF nID == 0
      hClass := __clsNew( "C Structure " + cArrayClassName, nLen + CLASS_PROPERTIES )

      aAdd( s_aClasses, { cArrayClassName, hClass, Array( nLen ), 1 } )
      nID := Len( s_aClasses )

      __clsAddMsg( hClass,  "Reset"     , @Reset()      , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Buffer"    , @Buffer()     , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Value"     , @Value()      , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "DeValue"   , @DeValue()    , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Array"     , @ArrayMethod(), HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "SayMembers", @SayMembers() , HB_OO_MSG_METHOD )

      FOR Counter := 1 TO nLen
         __clsAddMsg( hClass,       "Index" + LTrim( Str( Counter ) ), Counter, HB_OO_MSG_DATA )
         __clsAddMsg( hClass, "_" + "Index" + LTrim( Str( Counter ) ), Counter, HB_OO_MSG_DATA )
      NEXT

      __clsAddMsg( hClass,  "aCTypes"       , Counter, HB_OO_MSG_DATA )
      __clsAddMsg( hClass, "_aCTypes"       , Counter, HB_OO_MSG_DATA )

      Counter++

      __clsAddMsg( hClass,  "nAlign"        , Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass, "_nAlign"        , Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )

      Counter++

      __clsAddMsg( hClass,  "SizeOf"        , Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass, "_SizeOf"        , Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )

      Counter++

      __clsAddMsg( hClass,  "nID", Counter, HB_OO_MSG_DATA )
      __clsAddMsg( hClass, "_nID", Counter, HB_OO_MSG_DATA )

      // WARNING InternalBuffer *MUST* remain the *LAST* Property!!!
      Counter++

      __clsAddMsg( hClass,  "InternalBuffer", Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass, "_InternalBuffer", Counter, HB_OO_MSG_DATA, , HB_OO_CLSTP_READONLY )

      aCTypes := s_aClasses[nID][3]
      aFill( acTypes, CType )

      aAdd( s_aArrayClasses, { CType, nLen, nID } )

      //TraceLog( "Registered: " + cArrayClassName, nID, Len( s_aArrayClasses ) )
   ELSE
      nID := s_aArrayClasses[nID][3]
      TraceLog( "Reused: " + s_aClasses[nID][1], nID )
   ENDIF

Return nID + CTYPE_STRUCTURE

//---------------------------------------------------------------------------//
Function HB_IS_CStructure( x )

Return Left( x:ClassName(), 11 ) == "C Structure"

//---------------------------------------------------------------------------//
Static Function SayMembers( cPad )

   LOCAL xProperty

   IF cPad == NIL
      cPad := ""
   ENDIF

   IF ProcName( 1 ) != ProcName(0)
      QOut( cPad + SubStr( QSelf():ClassName, 13 ) )
      QOut( cPad + Replicate( "-", Len( SubStr( QSelf():ClassName, 13 ) ) ) )
   ENDIF

   FOR EACH xProperty IN QSelf():Array
      IF HB_IS_CStructure( xProperty )
         QOut( cPad + SubStr( xProperty:ClassName, 13 ) )
         QOut( cPad + Replicate( "-", Len( SubStr( xProperty:ClassName, 13 ) ) ) )

         xProperty:SayMembers( cPad + cPad )
      ELSE
         QOut( cPad + ":", xProperty )
      END
   NEXT

Return QSelf()

//---------------------------------------------------------------------------//
STATIC Function Reset()

   LOCAL Counter, nLen := Len( QSelf() ) - CLASS_PROPERTIES

   aFill( QSelf(), NIL, 1, nLen )

Return QSelf()

//---------------------------------------------------------------------------//
STATIC Function Buffer( Buffer )

   IF ValType( Buffer ) == "C"
      QSelf():InternalBuffer := Buffer
      QSelf():DeValue()
   ENDIF

   IF ValType( QSelf():InternalBuffer ) != "C"
      QSelf():InternalBuffer := QSelf():Value()
   ENDIF

Return QSelf():InternalBuffer

//---------------------------------------------------------------------------//
STATIC Function Value()

   LOCAL aValues := {}
   LOCAL nLen := Len( QSelf() ) - CLASS_PROPERTIES

   aScan( QSelf(), {|xVal| aAdd( aValues, xVal ) }, 1, nLen )

   QSelf():InternalBuffer := HB_ArrayToStructure( aValues, QSelf():aCTypes, QSelf():nAlign )

Return QSelf():InternalBuffer

//---------------------------------------------------------------------------//
STATIC Function DeValue()

   LOCAL aValues
   LOCAL Counter, nLen := Len( QSelf() ) - CLASS_PROPERTIES
   LOCAL Buffer := QSelf():InternalBuffer

   //TraceLog( QSelf():ClassName(), QSelf():nAlign, Buffer, Len( Buffer ) )

   IF ValType( Buffer ) != "C" .OR. Len( Buffer ) == 0
      aValues := Array( nLen )
   ELSE
      aValues := HB_StructureToArray( Buffer, QSelf():aCTypes, QSelf():nAlign  )
   ENDIF

   FOR Counter := 1 TO nLen
      QSelf()[Counter] := aValues[Counter]
   NEXT

Return aValues

//---------------------------------------------------------------------------//
STATIC Function ArrayMethod()

   LOCAL aValues := {}
   LOCAL nLen := Len( QSelf() ) - CLASS_PROPERTIES

   aScan( QSelf(), {|xVal| aAdd( aValues, xVal ) }, 1, nLen )

Return aValues
//---------------------------------------------------------------------------//
