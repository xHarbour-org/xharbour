/*
 * $Id: dbsdf.prg,v 1.7 2003/10/01 17:41:47 paultucker Exp $
 */

/*
 * Harbour Project source code:
 * Copies the contents of a database to an SDF text file.
 * Appends the contents of an SDF text file to a database.
 *
 * Copyright 2001-2002 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
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

#include "hbcommon.ch"
#include "fileio.ch"
#include "error.ch"

#define AppendEOL( handle ) FWrite( handle, CHR( 13 ) + CHR( 10 ) )
#define AppendEOF( handle ) FWrite( handle, CHR( 26 ) )
#define SkipEOL( handle ) FSEEK( handle, 2, FS_RELATIVE )

PROCEDURE __dbSDF( lExport, cFile, aFields, bFor, bWhile, nNext, nRecord, lRest )
   LOCAL index, handle, cFileName := cFile, nStart, nCount, oErr, nFileLen, cField
   LOCAL lLineEnd

   // Process the file name argument.
   index := RAT( ".", cFileName )
   IF index > 0
      // The file name might include a file extension.
      IF RAT( "/", cFileName ) > index ;
      .OR. RAT( "\", cFileName ) > index
         // No, the file extension is in a directory name.
         index := 0
      ENDIF
   ENDIF
   IF index <= 0
      // No file name extension, so provide the default.
      cFileName += ".txt"
   ENDIF

   // Determine where to start and how many records to process.
   IF nRecord != NIL
      // The RECORD clause has the highest priority.
      nStart := nRecord
      nCount := 1
   ELSEIF nNext != NIL
      // The NEXT clause has the next highest priority.
      nStart := -1
      nCount := nNext
   ELSEIF bWhile != NIL .OR. lRest
      // The WHILE and REST clauses have equal priority.
      nStart := -1
      nCount := -1
   ELSE
      // Followed by the FOR clause or the ALL clause.
      nStart := 0
      nCount := -1
   ENDIF
   IF EMPTY( bFor )
      // This simplifies the test that determines whether or not to
      // use (i.e., import or export) any given processed record.
      bFor := {||.T.}
   ENDIF

   IF lExport
      // COPY TO SDF
      handle := FCREATE( cFileName )
      IF handle == F_ERROR
         oErr := ErrorNew()
         oErr:severity := ES_ERROR
         oErr:genCode := EG_CREATE
         oErr:subSystem := "SDF"
         oErr:subCode := 1002
         oErr:description := HB_LANGERRMSG( oErr:genCode )
         oErr:canRetry := .T.
         oErr:canDefault := .T.
         oErr:fileName := cFileName
         oErr:osCode := FERROR()
         Eval(ErrorBlock(), oErr)
      ELSE
         IF nStart > -1
            // Only reposition if a starting record was specified or implied.
            IF nStart == 0
               GO TOP
            ELSE
               GO (nStart)
            ENDIF
         ENDIF
         IF EMPTY( bWhile )
            // This simplifies the looping logic.
            bWhile := {||.T.}
         ENDIF
         // Process the records to copy SDF.
         WHILE EVAL( bWhile ) .AND. ( nCount == -1 .OR. nCount > 0 ) ;
          .AND. !BOF() .AND. !EOF()
            IF EVAL( bFor )
               IF EMPTY( aFields )
                  // Process all fields.
                  FOR index := 1 TO FCOUNT()
                     ExportFixed( handle, FIELDGET( index ) )
                  NEXT index
               ELSE
                  // Process the specified fields.
                  FOR EACH cField IN aFields
                     ExportFixed( handle, FIELDGET( FieldPos( cField ) ) )
                  NEXT
               ENDIF
               // Set up for the start of the next record.
               AppendEOL( handle )
            ENDIF
            IF nCount != -1
               nCount--
            ENDIF
            SKIP
         END WHILE
         AppendEOF( handle )
         FClose( handle )
      ENDIF
   ELSE
      // APPEND FROM SDF
      handle := FOPEN( cFileName )
      IF handle == F_ERROR
         oErr := ErrorNew()
         oErr:severity := ES_ERROR
         oErr:genCode := EG_OPEN
         oErr:subSystem := "SDF"
         oErr:subCode := 1001
         oErr:description := HB_LANGERRMSG( oErr:genCode )
         oErr:canRetry := .T.
         oErr:canDefault := .T.
         oErr:fileName := cFileName
         oErr:osCode := FERROR()
         Eval(ErrorBlock(), oErr)
      ELSE
         IF EMPTY( bWhile )
            // This simplifies the looping logic.
            bWhile := {||.T.}
         ENDIF
         nFileLen := FSEEK( handle,0,FS_END )
         FSEEK( handle,0 )
         WHILE FSEEK( handle,0,FS_RELATIVE ) + 1 < nFileLen
            APPEND BLANK
            lLineEnd := .F.
            IF EMPTY( aFields )
               // Process all fields.
               FOR index := 1 TO FCOUNT()
                  if ! FieldType( index ) == "M"
                     FieldPut( index, ImportFixed( handle, index, @lLineEnd ) )
                  Endif
                  IF lLineEnd                   // Se é Fim-de-Linha vai p/ o proximo registro
                     EXIT
                  ENDIF
               NEXT index
            ELSE
               // Process the specified fields.
               FOR EACH cField IN aFields
                  if ! FieldType( index := FieldPos( cField ) ) == "M"
                     FieldPut( index, ImportFixed( handle, index, @lLineEnd ) )
                  endif
                  IF lLineEnd                   // Se é Fim-de-Linha vai p/ o proximo registro
                     EXIT
                  ENDIF
               NEXT
            ENDIF
            // Set up for the start of the next record.
            SkipEOL( handle )
         ENDDO
         FClose( handle )
      ENDIF
   ENDIF
Return

STATIC FUNCTION ExportFixed( handle, xField )
   SWITCH VALTYPE( xField )
      CASE "C"
         FWrite( handle, xField )
         EXIT
      CASE "D"
         FWrite( handle, DTOS( xField ) )
         EXIT
      CASE "L"
         FWrite( handle, iif( xField, "T", "F" ) )
         EXIT
      CASE "N"
         FWrite( handle, STR( xField ) )
         EXIT
      DEFAULT
         Return .F.
   END
Return .T.

STATIC FUNCTION ImportFixed( handle, index, lLineEnd )
   LOCAL cBuffer, nCR, nBytes, cType

   if ! Empty( cType := FieldType( index ) )

      cBuffer := Space( nCr := FieldLen( index ) )
      nBytes  := FRead( handle, @cBuffer, nCr )
      nCR     := at( CHR(13), cBuffer )

      IF ( lLineEnd := (nCR > 0) )
         cBuffer  := SubStr( cBuffer, 1, nCR - 1 )
         FSEEK( handle, (nBytes - nCR +1) * -1, FS_RELATIVE )
      ENDIF
      
      Do Case // don't switch()
         CASE cType == "C"                     /* default return */
         CASE cType == "N" .or. ;
              cType == "DOUBLE" .or. ;         /* ADS ADT extensions */
              cType == "INTEGER" .or. ;
              cType == "SHORTINT" .or. ;
              cType == "CURDOUBLE" .or. ;
              cType == "AUTOINC"
            Return VAL( cBuffer )
         CASE cType == "D"
            Return HB_STOD( cBuffer )
         CASE cType == "L"
            Return cBuffer == "T"
      ENDCASE
   ENDIF

Return cBuffer
