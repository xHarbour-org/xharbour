/*
 * $Id: dbdelim.prg,v 1.7 2004/03/09 11:17:29 mlombardo Exp $
 */

/*
 * Harbour Project source code:
 * Copies the contents of a database to a delimited text file.
 * Appends the contents of a delimited text file to a database.
 *
 * Copyright 2001-2002 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
 * APPEND FROM code submitted by Marco Braida <marcobra@elart.it>
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

HB_FILE_VER( "$Id: dbdelim.prg,v 1.7 2004/03/09 11:17:29 mlombardo Exp $" )

#define AppendEOL( handle )       FWrite( handle, CHR( 13 ) + CHR( 10 ) )
#define AppendEOF( handle )       FWrite( handle, CHR( 26 ) )
#define AppendSep( handle, cSep ) FWrite( handle, cSep )

#define UNIX_EOL        chr(10)
#define WINDOWS_EOL     chr(13)+chr(10)

PROCEDURE __dbDelim( lExport, cFileName, cDelimArg, aFields, bFor, bWhile, nNext, nRecord, lRest )

   local index, handle, lWriteSep, nStart, nCount, oErr
   local cSeparator := ",", cDelim := CHR( 34 )
   local Pos
   local nPosFl
   local nDimBuff:=65535
   local cByte
   local cont_r
   local Lfinefile:=.f.
   local nFileLen
   local cCharEol:=HB_OSNewLine()
   local nPosLasteol
   local lcisonoeol
   local lNoTerm
   local aStruct
   local nOptmLen := 0
   local cLine

#ifndef __USE_OLD__
   // Arrays From Delimited Text File To Be Appended
   local aTextContent
#endif

   // Process the delimiter argument.
   IF !EMPTY( cDelimArg )
      IF UPPER( cDelimArg ) == "BLANK"
         cDelim := ""
         cSeparator := " "
      ELSE
         cDelim := LEFT( cDelimArg, 1 )
      ENDIF
   ENDIF

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
   ELSEIF bWhile != NIL .OR. (lRest != NIL .and. lRest)
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
      // COPY TO DELIMITED
      handle := FCREATE( cFileName )
      IF handle == F_ERROR
         oErr := ErrorNew()
         oErr:severity   := ES_ERROR
         oErr:genCode    := EG_CREATE
         oErr:subSystem  := "DELIM"
         oErr:subCode    := 1002
         oErr:description:= HB_LANGERRMSG( oErr:genCode )
         oErr:canRetry   := .T.
         oErr:canDefault := .T.
         oErr:fileName   := cFileName
         oErr:osCode     := FERROR()
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
         // Set up for the start of the first record.
         lWriteSep := .F.
         // Process the records to copy delimited.
         WHILE EVAL( bWhile ) .AND. ( nCount == -1 .OR. nCount > 0 ) ;
         .AND. !BOF() .AND. !EOF()
            IF EVAL( bFor )
               IF EMPTY( aFields )
                  // Process all fields.
                  FOR index := 1 TO FCOUNT()
                     IF lWriteSep
                        AppendSep( handle, cSeparator )
                     ENDIF
                     lWriteSep := ExportVar( handle, FIELDGET( index ), cDelim )
                  NEXT index
               ELSE
                  // Process the specified fields.
                  FOR index := 1 TO LEN( aFields )
                     IF lWriteSep
                        AppendSep( handle, cSeparator )
                     ENDIF
                     lWriteSep := ExportVar( handle, FIELDGET( FIELDPOS( aFields[ index ] ) ), cDelim )
                  NEXT index
               ENDIF
               // Set up for the start of the next record.
               AppendEOL( handle )
               lWriteSep := .F.
            ENDIF
            IF nCount != -1
               nCount--
            ENDIF
            SKIP
         ENDDO
         AppendEOF( handle )
         FClose( handle )
      ENDIF
   ELSE
      // APPEND FROM DELIMITED
      handle := FOPEN( cFileName )
      IF handle == F_ERROR
         oErr := ErrorNew()
         oErr:severity   := ES_ERROR
         oErr:genCode    := EG_OPEN
         oErr:subSystem  := "DELIM"
         oErr:subCode    := 1001
         oErr:description:= HB_LANGERRMSG( oErr:genCode )
         oErr:canRetry   := .T.
         oErr:canDefault := .T.
         oErr:fileName   := cFileName
         oErr:osCode     := FERROR()
         Eval(ErrorBlock(), oErr)
      ELSE
         IF EMPTY( bWhile )
            // This simplifies the looping logic.
            bWhile := {||.T.}
         ENDIF

         aStruct  := DBStruct()

#ifdef __USE_OLD__
         nFileLen := FSeek(handle,0,FS_END)
         nDimBuff := Min(nFileLen,nDimBuff)
         cByte    := Space(nDimBuff)
         FSeek(handle,0)

         Do While FSEEK( handle,0,FS_RELATIVE ) + 1 < nFileLen

            HB_FReadLine( handle, @cLine, { WINDOWS_EOL, UNIX_EOL }, nOptmLen )

            /* Next HB_FReadLine will be optimized */
            nOptmLen := len( cLine ) + 1

            If !Empty( cLine )
               // Blank lines are not imported
               AppendToDb( cLine, cSeparator, aStruct )
            EndIf

         enddo
         FClose( handle )
#else
/*
   AJ: Enhancement in speed
   2003-03-10
*/
         FClose( handle )
         IF !Empty( aTextContent := FParseEX( cFileName ) )
            AppendToDb( aTextContent, aStruct )
         ENDIF

#endif
      endif
   endif
RETURN

STATIC FUNCTION ExportVar( handle, xField, cDelim )
   SWITCH VALTYPE( xField )
      CASE "C"
         FWrite( handle, cDelim + TRIM( xField ) + cDelim )
         EXIT
      CASE "D"
         FWrite( handle, DTOS( xField ) )
         EXIT
      CASE "L"
         FWrite( handle, iif( xField, "T", "F" ) )
         EXIT
      CASE "N"
         FWrite( handle, LTRIM( STR( xField ) ) )
         EXIT
      DEFAULT
         RETURN .F.
   END
RETURN .T.

#ifdef __USE_OLD__
STATIC FUNCTION AppendToDb(cLine,cDelim,aStruct)

   local lenrow:=len(cLine)
   local aMyVal:={}
   local ii
   local npos, nPosNext
   local nDBFFields
   local cBuffer
   local vRes

   // if there is one field and no Delim, this is added

   cLine  += cDelim
   nPos := atToken(cDelim,cLine)
   aadd( aMyval,Substr(cLine,1,nPos-1) )

   do while .t.
      nPosNext := AtToken(cDelim,cLine,npos+2)
      if nPosNext = 0
         exit
      endif
      aadd( aMyVal,Substr(cLine,npos+1,nPosnext-npos-1) )
      nPos := nPosnext
      if nPos > lenrow
         exit
      endif
   enddo

   nDBFfields := min(len(aMyVal),len(aStruct))
   append blank

   for ii := 1 to nDBFfields
      cBuffer:=RemoveQuote(aMyval[ii])
      SWITCH aStruct[ ii,2 ]
         CASE "D"
            vRes := HB_STOD( cBuffer )
            EXIT
         CASE "L"
            vRes := Upper( cBuffer ) $ "T1Y"
            EXIT
         CASE "N"
            vRes := VAL( cBuffer )
            EXIT
         DEFAULT
            vRes := cBuffer
      END

      FIELDPUT(ii,vRes)
   next
#else
/*
   AJ: Enhancement in speed
   2003-03-10
*/
STATIC FUNCTION AppendToDb( aTextContent, aStruct )

   local aContent
   local nDBFfields
   local nStructLen := Len( aStruct )
   local ii
   local vRes
   local cBuffer

   FOR EACH aContent IN aTextContent
      nDBFfields := min( len( aContent ), nStructLen )
      append blank
      for ii := 1 to nDBFfields
         cBuffer := aContent[ ii ]
         SWITCH aStruct[ ii,2 ]
            CASE "D"
               vRes := HB_STOD( cBuffer )
               EXIT
            CASE "L"
               vRes := Upper( cBuffer ) $ "T1Y"
               EXIT
            CASE "N"
               vRes := VAL( cBuffer )
               EXIT
            DEFAULT
               vRes := cBuffer
         END
         FIELDPUT(ii,vRes)
      next
   NEXT

#endif
return .T.

Static Function AtToken( cToken, cLine, nStart )

   local nPos := 0, cChar, lOpenPair := .F., i

   If nStart == NIL
      nStart := 1
   EndIf

   For i = nStart to len( cLine )
      cChar := cLine[i]
      If cChar == '"'
         lOpenPair := !lOpenPair
      EndIf
      If (!lOpenPair) .and. cChar == cToken
         nPos := i
         Exit
      EndIf
   Next

Return nPos

Static Function RemoveQuote( cStr )

   // This function can be optimized!!

   cStr := alltrim(cStr)
   If SubStr(cStr,1,1) == '"'
      cStr := SubStr(cStr,2)
   EndIf
   If SubStr(cStr,len(cStr),1) == '"'
      cStr := SubStr(cStr,1,len(cStr)-1)
   EndIf

Return cStr
