/*
 * $Id: persist.prg,v 1.19 2004/01/11 20:35:29 peterrees Exp $
 */

/*
 * Harbour Project source code:
 * Class HBPersistent
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
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
 * The following parts are Copyright of the individual authors.
 *
 * Copyright 2003 Walter Negro <anegro@overnet.com.ar>
 *    Module optimized using xHarbour syntax extensions.
 *
 * Copyright 2003 Ron Pinkas <ron@ronpinks.com>
 *    LoadFromText()
 *    SaveToText()
 *    General rewrite.
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz>
 *    ReplaceBracketsInString()
 *    ReplaceCRLFInString()
 *    ArrayFromLFString()
 *    + Update to handle strings > 254 characters & embedded CHR(13)/CHR(10)/'[]'
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "common.ch"

extern HB_STOD

CLASS HBPersistent

   METHOD CreateNew() INLINE Self
   METHOD LoadFromFile( cFileName, lIgnoreBadIVars ) INLINE ::LoadFromText( MemoRead( cFileName ), lIgnoreBadIVars )
   METHOD LoadFromText( cObjectText, lIgnoreBadIVars )
   METHOD SaveToText( cObjectName )
   METHOD SaveToFile( cFileName ) INLINE MemoWrit( cFileName, ::SaveToText() )

ENDCLASS

METHOD LoadFromText( cObjectText, lIgnoreBadIVars ) CLASS HBPersistent

   EXTERN HB_RestoreBlock
   LOCAL aLines, nLines, nLine := 1, cLine
   LOCAL lStart := .T., aObjects := {}, nObjectLevel := 0
   LOCAL nAt
   LOCAL cVersion2:= "// HBPersistent Ver 2.0"
   MEMVAR oObject
   PRIVATE oObject := QSelf()
   IF lIgnoreBadIVars == nil
      lIgnoreBadIVars := .f.
   ENDIF
   aLines := ArrayFromLFString(cObjectText)
   nLines := LEN(aLines)
   IF LEFT(cObjectText, LEN(cVersion2)) == cVersion2
      nLine  := 2
   ELSE
      FOR nLine := 1 TO nLines
         cLine := LTRIM(aLines[nLine])
         IF cLine[1] == ':'
            nAt := At( '=', cLine )
            IF nAt > 1
              cLine[ nAt - 1] := ':'
            ENDIF
            aLines[nLine]:= cLine
         ENDIF
      NEXT nLine
      nLine  := 1
   ENDIF

   DO WHILE nLine <= nLines
      cLine := RTrim( LTrim( aLines[nLine] ) )

      IF Empty( cLine )
         nLine++
         LOOP
      ENDIF

      //TraceLog( cLine )
      DO CASE
         CASE Left( cLine, 2 ) == "::"
            cLine := "oObject" + SubStr( cLine, 2 )

            //TraceLog( cLine )
            IF lIgnoreBadIVars
               TRY
                  &( cLine )
               END
            ELSE
               &( cLine )
            ENDIF

         CASE Left( cLine, 6 ) == "ARRAY "
            cLine = SubStr( cLine, 8 )
            cLine := "oObject" + cLine
            cLine := StrTran( cLine, " LEN ", " := Array( " ) + ")"
            //TraceLog( cLine )
            &( cLine )

         CASE Left( cLine, 7 ) == "OBJECT "
            IF lStart == .T.
               lStart := .F.
            ELSE
               nObjectLevel++
               aSize( aObjects, nObjectLevel )
               aObjects[ nObjectLevel ] := M->oObject

               cLine = SubStr( cLine, 9 )
               cLine := "oObject" + cLine
               cLine := StrTran( cLine, " AS ", " := " ) + "()"

               //TraceLog( cLine )
               M->oObject := &( cLine )
            ENDIF

         CASE Left( cLine, 9 ) == "ENDOBJECT"
            IF nObjectLevel > 0
               M->Self := aObjects[ nObjectLevel ]
               nObjectLevel--
            ENDIF

         OTHERWISE
            // TraceLog( cLine )
      ENDCASE

      nLine++
   ENDDO

RETURN .T.

METHOD SaveToText( cObjectName ) CLASS HBPersistent

   LOCAL cObject
   LOCAL aBasePropertiesAndValues, aPropertiesAndValues, aPropertyAndValue, cType, xValue

   STATIC nIndent := -3

   DEFAULT cObjectName TO "o" + ::ClassName()

   nIndent += 3

   IF nIndent == 0
      cObject := "// HBPersistent Ver 2.0" + HB_OsNewLine()
      //cObject := HB_OsNewLine()
   ELSE
      cObject := HB_OsNewLine() + Space( nIndent )
   ENDIF

   cObject += "OBJECT "

   IF nIndent > 0
      cObject += "::"
   ENDIF

   cObject += cObjectName + " AS " + ::ClassName()
   cObject += HB_OsNewLine()

   aBasePropertiesAndValues := __ClsGetPropertiesAndValues( __ClsInst( QSelf():ClassH ) )
   aPropertiesAndValues     := __ClsGetPropertiesAndValues( QSelf() )

   FOR EACH aPropertyAndValue IN aPropertiesAndValues
      xValue := aPropertyAndValue[2]
      cType  := ValType( xValue )

      IF HB_EnumIndex() > Len( aBasePropertiesAndValues ) .OR. ;
         cType != ValType( aBasePropertiesAndValues[ HB_EnumIndex() ][ 2 ] ) .OR. ;
         ValType( xValue ) == 'B' .OR. ;
         ! ( xValue == aBasePropertiesAndValues[ HB_EnumIndex() ][ 2 ] )

         SWITCH cType
            CASE "A"
               nIndent += 3
               cObject += ArrayToText( xValue, aPropertyAndVAlue[1], nIndent, Self )
               cObject += HB_OsNewLine()
               nIndent -= 3

               EXIT

            CASE "O"
               IF __objDerivedFrom( xValue, "HBPERSISTENT" )
                  cObject += xValue:SaveToText( aPropertyAndValue[1] )
                  cObject += HB_OsNewLine()
               ENDIF

               EXIT

            DEFAULT
               cObject += Space( nIndent ) + "   ::" + aPropertyAndVAlue[1] + " := " + ValToText( xValue)
               cObject += HB_OsNewLine()
         END
      ENDIF
   NEXT

   cObject += Space( nIndent ) + "ENDOBJECT" + HB_OsNewLine()
   nIndent -= 3

RETURN cObject

STATIC FUNCTION ArrayToText( aArray, cName, nIndent, Self )

   LOCAL cArray := HB_OsNewLine(), xValue, cType

   cArray += Space( nIndent ) + "ARRAY ::" + cName + " LEN " + AllTrim( Str( Len( aArray ) ) ) + HB_OsNewLine()

   FOR EACH xValue IN aArray
      cType  := ValType( xValue )

      SWITCH cType
         CASE "A"
            nIndent += 3
            cArray += ArrayToText( xValue, cName + "[ " + AllTrim( Str( HB_EnumIndex() ) ) + " ]", nIndent, Self )
            cArray += HB_OsNewLine()
            nIndent -= 3

            EXIT

         CASE "O"
            IF __objDerivedFrom( xValue, "HBPERSISTENT" )
               cArray += xValue:SaveToText( cName + "[ " + AllTrim( Str( HB_EnumIndex() ) ) + " ]" )
               cArray += HB_OsNewLine()
            ENDIF

            EXIT

         DEFAULT
            cArray += Space( nIndent ) + "   ::" + cName + "[ " + AllTrim( Str( HB_EnumIndex() ) ) + " ]" + " := " + ;
                      ValToText( xValue ) + HB_OsNewLine()
      END
   NEXT

   cArray += Space( nIndent ) + "ENDARRAY" + HB_OsNewLine()

RETURN cArray

STATIC FUNCTION ValToText( xValue )

   LOCAL cType := ValType( xValue )
   LOCAL cText, cQuote := '"'

   PRIVATE oObject

   SWITCH cType
      CASE "C"
         IF cQuote IN xValue
            cQuote := "'"
            IF cQuote IN xValue
               xValue:= ReplaceBracketsInString(xValue)
               cText := "["+ xValue + "]"
            ELSE
               cText := cQuote + xValue + cQuote
            ENDIF
         ELSE
            cText := cQuote + xValue + cQuote
         ENDIF
         cText:= ReplaceCRLFInString(cText)
         EXIT

      CASE "N"
         cText := AllTrim( Str( xValue ) )

         EXIT

      CASE "D"
         cText := 'HB_STOD( "' + DToS( xValue ) + '" )'

      CASE "B"
         cText := ValToPrgExp( xValue  )

         EXIT
      CASE "L"
         cText := IIF(  xValue, ".T.", ".F."  )
         EXIT

      DEFAULT
         cText := HB_ValToStr( xValue )
   END

RETURN cText


STATIC FUNCTION ReplaceCRLFInString(cText)
  LOCAL cCR:= CHR(13), cLF:= CHR(10), cQuoteOpen, cQuoteClose

  cQuoteClose:= cQuoteOpen:= cText[1]

  IF cQuoteOpen=='['
    cQuoteClose:= ']'
  ENDIF

  IF cCR IN cText
    cText:= STRTRAN(cText, cCR, cQuoteClose+"+CHR(13)+"+cQuoteOpen)
  ENDIF

  IF cLF IN cText
    cText:= STRTRAN(cText, cLF, cQuoteClose+"+CHR(10)+"+cQuoteOpen)
  ENDIF

  RETURN(cText)

STATIC FUNCTION ReplaceBracketsInString(cText)
  LOCAL cTemp, nPos, nLen

  IF '[' IN cText .OR. ']' IN cText
    cTemp:= cText
    cText:= ""
    nLen:= LEN(cTemp)

    FOR nPos:= 1 TO nLen

      SWITCH cTemp[nPos]
        CASE '['
          cText+="]+CHR(91)+["
          EXIT
        CASE ']'
          cText+="]+CHR(93)+["
          EXIT
        DEFAULT
          cText+= cTemp[nPos]
      END

    NEXT nPos

  ENDIF

  RETURN(cText)

FUNCTION ArrayFromLFString(cString)
  LOCAL cDelim:= CHR(10), nStart:=1, nStop, aResult:= {}, nLen
  LOCAL nCnt:= 0

  cString:= STRTRAN(cString,CHR(13)) // Get rid of CR

  IF RIGHT(cString,1) != cDelim
    cString+= cDelim // Add a trailing LF if last character is not one
  ENDIF

  nLen:= LEN(cString)

  DO WHILE !EMPTY(nStop:= AT(cDelim, cString, nStart)) .AND. nStart<= nLen
    AADD(aResult, SUBSTR(cString, nStart, nStop- nStart))
    nStart:= nStop+1
  ENDDO

  RETURN(aResult)

