/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __DBCOPYSTRUCT(), __DBCOPYXSTRUCT(), __DBCREATE() functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * dbModifyStructure( <cFile> )           -> lSuccess
 * dbImport( <cFile|nArea> )              -> lSuccess
 * dbMerge( <cFile|nArea> [, <lAppend>] ) -> lSuccess
 *
 * Copyright 2009 Ron Pinkas <Ron.Pinkas at xHarbour.com>
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

#include "hbsetup.ch"

#include "common.ch"
#include "dbstruct.ch"

#include "dbinfo.ch"
#include "error.ch"

FUNCTION __dbCopyStruct( cFileName, aFieldList, cRddName )
   RETURN dbCreate( cFileName, __dbStructFilter( dbStruct(), aFieldList ), cRddName )

FUNCTION __dbCopyXStruct( cFileName )
   LOCAL nOldArea
   LOCAL aStruct

   LOCAL oError
   LOCAL lError := .F.

   IF Empty( aStruct := dbStruct() )
      RETURN .F.
   ENDIF

   nOldArea := Select()

   BEGIN SEQUENCE

      dbSelectArea( 0 )
      __dbCreate( cFileName, NIL, NIL, .F., ProcName() )

      AEval( aStruct, {| aField | iif( aField[ DBS_TYPE ] == "C" .AND. aField[ DBS_LEN ] > 255,;
                                     ( aField[ DBS_DEC ] := Int( aField[ DBS_LEN ] / 256 ), aField[ DBS_LEN ] := aField[ DBS_LEN ] % 256 ), ),;
                                  dbAppend(),;
                                  FIELD->FIELD_NAME := aField[ DBS_NAME ],;
                                  FIELD->FIELD_TYPE := aField[ DBS_TYPE ],;
                                  FIELD->FIELD_LEN := aField[ DBS_LEN ],;
                                  FIELD->FIELD_DEC := aField[ DBS_DEC ] } )

   /* NOTE: CA-Cl*pper has a bug, where only a plain RECOVER statement is
            used here (without the USING keyword), so oError will always be NIL. */
   RECOVER USING oError
      lError := .T.
   END SEQUENCE

   IF Select() != nOldArea
      dbCloseArea()
      dbSelectArea( nOldArea )
   ENDIF

   IF lError
      Break( oError )
   ENDIF

   RETURN .T.

/* NOTE: Compared to CA-Cl*pper, Harbour has two extra parameters
         (cCodePage, nConnection). */

FUNCTION __dbCreate( cFileName, cFileFrom, cRDD, lNew, cAlias, cCodePage, nConnection )
   LOCAL nOldArea := Select()
   LOCAL aStruct := {}

   LOCAL oError

   DEFAULT lNew TO .F.

   IF cAlias == NIL
      hb_FNameSplit( cFileName, NIL, @cAlias )
   ENDIF

   IF Used() .AND. !lNew
      dbCloseArea()
   ENDIF

   BEGIN SEQUENCE

      IF Empty( cFileFrom )

         dbCreate( cFileName, { { "FIELD_NAME", "C", 10, 0 }, ;
                                { "FIELD_TYPE", "C",  1, 0 }, ;
                                { "FIELD_LEN" , "N",  3, 0 }, ;
                                { "FIELD_DEC" , "N",  3, 0 } }, ;
                   cRDD, .F., cAlias, NIL, cCodePage, nConnection )
      ELSE
         dbUseArea( lNew, NIL, cFileFrom, "" )

         dbEval( {|| AAdd( aStruct, { FIELD->FIELD_NAME, ;
                                      FIELD->FIELD_TYPE, ;
                                      FIELD->FIELD_LEN, ;
                                      FIELD->FIELD_DEC } ) } )
         dbCloseArea()

         IF lNew
            dbSelectArea( nOldArea )
         ENDIF

         /* Type detection is more in sync with dbCreate() logic in Harbour, as lowercase "C"
            and padded/continued strings ("C ", "C...") are also accepted. */

         AEval( aStruct, {| aField | iif( Upper( Left( aField[ DBS_TYPE ], 1 ) ) == "C" .AND. aField[ DBS_DEC ] != 0, ;
            ( aField[ DBS_LEN ] += aField[ DBS_DEC ] * 256, ;
              aField[ DBS_DEC ] := 0 ), NIL ) } )

         dbCreate( cFileName, aStruct, cRDD, lNew, cAlias, NIL, cCodePage, nConnection )

      ENDIF

   RECOVER USING oError
      dbCloseArea()
      Break( oError )
   END SEQUENCE

   RETURN Used()

/* NOTE: Internal helper function, CA-Cl*pper name is: __FLEDIT() */

FUNCTION __dbStructFilter( aStruct, aFieldList )
   LOCAL aStructFiltered
   LOCAL bFindName
   LOCAL cName

   IF Empty( aFieldList )
      RETURN aStruct
   ENDIF

   /* Build a filtered list of the requested fields. */

   aStructFiltered := {}
   bFindName := {| aField | aField[ DBS_NAME ] == cName }

   AEval( aFieldList, {| cFieldName, nIndex | ;
         cName := RTrim( Upper( cFieldName ) ),;
         nIndex := AScan( aStruct, bFindName ),;
         iif( nIndex == 0, NIL, AAdd( aStructFiltered, aStruct[ nIndex] ) ) } )

   RETURN aStructFiltered

/*
  xHarbour extensions by Ron Pinkas
 */
//----------------------------------------------------------------------------//
FUNCTION dbModifyStructure( cFile )

   LOCAL lRet
   LOCAL cExt
   LOCAL cTable
   LOCAL cBakFile
   LOCAL cStructureFile
   LOCAL cNewFile
   LOCAL oErr
   LOCAL nPresetArea := Select()
   LOCAL nSourceArea
   LOCAL cDateTime   := SubStr( dtos( Date() ), 3 ) + "." + StrTran( Left( Time(), 5 ), ":", "." )

   TRY
      // Open exclusively, get name info, and create the structure db.
      //-------------------------------------------------------------//
      USE ( cFile ) ALIAS ModifySource EXCLUSIVE NEW
      nSourceArea := Select()

      cFile := dbInfo( DBI_FULLPATH )
      cExt  := dbInfo( DBI_TABLEEXT )

      hb_FNameSplit( cFile,, @cTable )

      cBakFile       := cTable + ".bak." + cDateTime + cExt
      cStructureFile := cTable + ".str." + cDateTime + cExt
      cNewFile       := cTable + ".new." + cDateTime + cExt

      COPY STRUCTURE EXTENDED TO ( cStructureFile )
      //-------------------------------------------------------------//

      // Let user modify the structure.
      //-------------------------------------------------------------//
      USE ( cStructureFile ) ALIAS NewStructure EXCLUSIVE NEW

      Browse( 0, 0, Min( 20, MaxRow() - 1 ), Min( MaxCol() - 30, 50 ) )

      PACK
      CLOSE

      CREATE ( cNewFile ) FROM ( cStructureFile ) ALIAS NEW_MODIFIED NEW
      //-------------------------------------------------------------//


      // Import data into the new file, and close it
      //-------------------------------------------------------------//
      lRet := dbImport( nSourceArea )
      CLOSE

      SELECT ( nSourceArea )
      CLOSE

      SELECT ( nPresetArea )
      //-------------------------------------------------------------//

      // Rename original as backup, and new file as the new original.
      //-------------------------------------------------------------//
      IF lRet
         IF FRename( cFile, cBakFile ) == -1
            BREAK
         ENDIF

         IF FRename( cNewFile, cFile ) == -1
            // If we can't then try to restore backup as original
            IF FRename( cBakFile, cFile ) == -1
               // Oops - must advise the user!
               oErr := ErrorNew()
               oErr:severity     := ES_ERROR
               oErr:genCode      := EG_RENAME
               oErr:subSystem    := "DBCMD"
               oErr:canDefault   := .F.
               oErr:canRetry     := .F.
               oErr:canSubtitute := .F.
               oErr:operation    := cFile
               oErr:subCode      := 1101
               oErr:args         := { cNewFile, cBakFile }

               BREAK oErr
            ENDIF
         ENDIF
      ENDIF
      //-------------------------------------------------------------//

   CATCH oErr
      IF oErr:ClassName == "ERROR"
         IF oErr:genCode == EG_RENAME
            // This kind of error must be reported
            lRet := Throw( oErr )
         ELSE
            lRet := .F.
         ENDIF
      ELSE
         lRet := .F.
      ENDIF
   END

   SELECT ( nPresetArea )

RETURN lRet

//----------------------------------------------------------------------------//
FUNCTION dbImport( xSource )

RETURN dbMerge( xSource )

//----------------------------------------------------------------------------//
FUNCTION dbMerge( xSource, lAppend )

   LOCAL nArea, nSource, nRecNo
   LOCAL aFields
   LOCAL cField, xField
   LOCAL nSourcePos, aTranslate := {}, aTranslation //, oErr
   LOCAL cTargetType

   // Safety
   //-------------------------------------------------------------//
   IF LastRec() > 0
      IF ! ( lAppend == .T. )
         RETURN .F.
      ENDIF
   ENDIF
   //-------------------------------------------------------------//

   // Validate args
   //-------------------------------------------------------------//
   SWITCH ValType( xSource )
   CASE 'C'
      nArea := Select()

      USE ( xSource ) ALIAS MergeSource EXCLUSIVE NEW
      nSource := Select()

      SELECT ( nArea )
      EXIT
   CASE 'N'
      nSource := xSource
      EXIT
   DEFAULT
      RETURN .F.
   ENDSWITCH
   //-------------------------------------------------------------//

   // Temp working record
   IF LastRec() == 0
      APPEND BLANK
   ENDIF

   // Create translation plan
   //-------------------------------------------------------------//
   aFields := Array( FCount() )
   aFields( aFields )

   FOR EACH cField IN aFields
      nSourcePos := (nSource)->( FieldPos( cField ) )

      IF nSourcePos > 0
         TRY
            // Save
            xField := FieldGet( HB_EnumIndex() )

            // Test type compatability
            FieldPut( HB_EnumIndex(), (nSource)->( FieldGet( nSourcePos ) ) )

            // Restore
            FieldPut( HB_EnumIndex(), xField )

            // Ok to process
            aAdd( aTranslate, { HB_EnumIndex(), nSourcePos, {|xSource| xSource } } )
         CATCH
            cTargetType := ValType( FieldGet( HB_EnumIndex() ) )

            TRY
               // Test type compatability
               FieldPut( HB_EnumIndex(), ValToType( (nSource)->( FieldGet( nSourcePos ) ), cTargetType ) )

               // Restore
               FieldPut( HB_EnumIndex(), xField )

               // Ok to process
               aAdd( aTranslate, { HB_EnumIndex(), nSourcePos, {|xSource| ValToType( xSource, cTargetType ) } } )
            CATCH //oErr
               //TraceLog( oErr:Description, oErr:Operation )
            END
         END
      ENDIF
   NEXT
   //-------------------------------------------------------------//

   // Reset
   //-------------------------------------------------------------//
   IF LastRec() == 1 .AND. ! ( lAppend == .T. )
      DELETE
      ZAP
   ENDIF
   //-------------------------------------------------------------//

   // Process
   //-------------------------------------------------------------//
   nRecNo := (nSource)->( RecNo() )
   (nSource)->( dbGoTop(1) )

   WHILE ! (nSource)->( Eof() )
      APPEND BLANK

      FOR EACH aTranslation IN aTranslate
         FieldPut( aTranslation[1], Eval( aTranslation[3], (nSource)->( FieldGet( aTranslation[2] ) ) ) )
      NEXT

      (nSource)->( dbSkip() )
   ENDDO

   (nSource)->( dbGoTo( nRecNo ) )
   //-------------------------------------------------------------//

   // Reset
   //-------------------------------------------------------------//
   IF ! Empty( nArea )
      SELECT ( nSource )
      CLOSE
      SELECT ( nArea )
   ENDIF
   //-------------------------------------------------------------//

RETURN .T.

//----------------------------------------------------------------------------//
