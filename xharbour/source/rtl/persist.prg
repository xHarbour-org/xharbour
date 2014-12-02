/*
 * $Id$
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

extern STOD

CLASS HBPersistent

   METHOD CreateNew() INLINE Self
   METHOD LoadFromFile( cFileName, lIgnoreBadIVars, lPropertiesOnly ) INLINE ::LoadFromText( MemoRead( cFileName ), lIgnoreBadIVars, lPropertiesOnly )
   METHOD LoadFromText( cObjectText, lIgnoreBadIVars, lPropertiesOnly )
   METHOD SaveToText( cObjectName )
   METHOD SaveToFile( cFileName ) INLINE MemoWrit( cFileName, ::SaveToText() )

ENDCLASS

METHOD LoadFromText( cObjectText, lIgnoreBadIVars, lPropertiesOnly ) CLASS HBPersistent

   EXTERN HB_RestoreBlock

   LOCAL oPure
   LOCAL aLines, cLine
   LOCAL nAt
   LOCAL xProperty
   LOCAL lFix
   LOCAL nWith
   LOCAL oError

   #define HB_PERSIST_VER_2 "// HBPersistent Ver 2.0"

   PRIVATE oObject

   oPure := __ClsInst( QSelf():ClassH )

   IF lPropertiesOnly == nil        // for objects with a long lifetime that may call this multiple times,
      lPropertiesOnly := .f.        // you may not want non-Property ivars cleared
   ENDIF

   // Start with default values of clean instance.
   IF lPropertiesOnly
      aLines := __CLSGetPropertiesAndValues( oPure )
      FOR EACH xProperty IN aLines     // This re-initializes PROPERTIES only
         __objSendMsg( Self, aLines[ HB_EnumIndex(), 1], aLines[ HB_EnumIndex(), 2] )
      NEXT
      aLines := nil
   ELSE
      // This re-initializes ALL ivars
      FOR EACH xProperty IN Self
         xProperty := oPure[ HB_EnumIndex() ]
      NEXT
   ENDIF

   // To support macros down below.
   HB_SetWith( Self )
   nWith := 1

   TRY
      IF lIgnoreBadIVars == nil
         lIgnoreBadIVars := .f.
      ENDIF

      aLines := HB_aTokens( StrTran( cObjectText, Chr(13), "" ), Chr(10) )

      IF Left( cObjectText, Len( HB_PERSIST_VER_2 ) ) == HB_PERSIST_VER_2
         lFix := .F.

         // Remove version info.
         aDel( aLines, 1, .T. )
      ELSE
         lFix := .T.
      ENDIF

      // Remove Outer OBJECT line.
      aDel( aLines, 1, .T. )

      // Remove Outer ENDOBJECT
      aSize( aLines, Len( aLines ) - 1 )

      FOR EACH cLine IN aLines
         cLine := RTrim( LTrim( cLine ) )

         IF lFix .AND. cLine[1] == ':'
            nAt := At( '=', cLine )

            IF nAt > 1
              cLine[ nAt - 1] := ':'
            ENDIF
         ENDIF

         IF Empty( cLine )
            LOOP
         ENDIF

         //TraceLog( cLine )

         DO CASE
            CASE Left( cLine, 2 ) == "::"
               cLine := "HB_QWith()" + SubStr( cLine, 2 )

               //TraceLog( cLine )
               IF lIgnoreBadIVars
                  TRY
                     &( cLine )
                  CATCH
                     // Ignore.
                  END
               ELSE
                  &( cLine )
               ENDIF

            CASE Left( cLine, 6 ) == "ARRAY "
               cLine := SubStr( cLine, 8 )
               cLine := "HB_QWith()" + cLine
               cLine := StrTran( cLine, " LEN ", " := Array( " ) + ")"
               //TraceLog( cLine )
               &( cLine )

            CASE Left( cLine, 7 ) == "OBJECT "
               cLine := SubStr( cLine, 9 )
               cLine := "HB_QWith()" + cLine
               cLine := StrTran( cLine, " AS ", " := " ) + "()"

               //TraceLog( cLine )
               HB_SetWith( &( cLine ) )
               nWith++

            CASE Left( cLine, 9 ) == "ENDOBJECT"
               HB_SetWith()
               nWith--

            OTHERWISE
               // TraceLog( cLine )
         ENDCASE
      NEXT
   CATCH oError
      TraceLog( ValToPrg( oError ) )

   FINALLY
      WHILE nWith-- > 0
         HB_SetWith()
      ENDDO
   END

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
         cType == 'B' .OR. ! ( xValue == aBasePropertiesAndValues[ HB_EnumIndex() ][ 2 ] )

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

            CASE "B"
               cObject += Space( nIndent ) + "   ::" + aPropertyAndVAlue[1] + " := " + ValToPrgExp( xValue, , , .T. )
               cObject += HB_OsNewLine()
               EXIT

            DEFAULT
               cObject += Space( nIndent ) + "   ::" + aPropertyAndVAlue[1] + " := " + ValToPrg( xValue )
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
            cArray += Space( nIndent ) + "   ::" + cName + "[ " + AllTrim( Str( HB_EnumIndex() ) ) + " ]" + " := " + ValToPrg( xValue ) + HB_OsNewLine()
      END
   NEXT

   cArray += Space( nIndent ) + "ENDARRAY" + HB_OsNewLine()

RETURN cArray
