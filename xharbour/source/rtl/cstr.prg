/*
 * $Id: cstr.prg,v 1.12 2003/05/26 00:19:15 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 * CStr( xAnyType ) -> String
 *
 * Copyright 2001 Ron Pinkas <ron@@ronpinkas.com>
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
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "error.ch"

/*
   For performance NOT using OS indpendant R/T function,
   this define only used in ValTpPrg() which currently only used in win32.
 */
#undef CRLF
#define CRLF Chr(13) + Chr(10)

//--------------------------------------------------------------//
FUNCTION CStr( xExp )

   LOCAL cType

   IF xExp == NIL
      RETURN 'NIL'
   ENDIF

   cType := ValType( xExp )

   SWITCH cType
      CASE 'C'
         RETURN xExp

      CASE 'D'
         RETURN dToc( xExp )

      CASE 'L'
         RETURN IIF( xExp, '.T.', '.F.' )

      CASE 'N'
         RETURN Str( xExp )

      CASE 'M'
         RETURN xExp

      CASE 'A'
         RETURN "{ Array of " +  LTrim( Str( Len( xExp ) ) ) + " Items }"

      CASE 'B'
         RETURN '{|| Block }'

      CASE 'O'
         RETURN "{ " + xExp:ClassName() + " Object }"

      DEFAULT
         RETURN "Type: " + cType
   END

RETURN ""

//--------------------------------------------------------------//
FUNCTION CStrToVal( cExp, cType )

   IF ValType( cExp ) != 'C'
      Throw( ErrorNew( "CSTR", 3101, ProcName(), "Argument error", { cExp, cType } ) )
   ENDIF

   SWITCH cType
      CASE 'C'
         RETURN cExp

      CASE 'D'
         IF cExp[3] >= '0' .AND. cExp[3] <= '9' .AND. cExp[5] >= '0' .AND. cExp[5] <= '9'
            RETURN cToD( cExp )
         ELSE
            RETURN sToD( cExp )
         ENDIF

      CASE 'L'
         RETURN IIF( cExp[1] == 'T' .OR. cExp[2] == 'Y' .OR. cExp[2] == 'T' .OR. cExp[2] == 'Y', .T., .F. )

      CASE 'N'
         RETURN Val( cExp )

      CASE 'M'
         RETURN cExp

      /*
      CASE 'A'
         Throw( ErrorNew( "CSTR", 3101, ProcName(), "Argument error", { cExp, cType } ) )

      CASE 'B'
         Throw( ErrorNew( "CSTR", 3101, ProcName(), "Argument error", { cExp, cType } ) )

      CASE 'O'
         Throw( ErrorNew( "CSTR", 3101, ProcName(), "Argument error", { cExp, cType } ) )
      */

      DEFAULT
         Throw( ErrorNew( "CSTR", 3101, ProcName(), "Argument error", { cExp, cType } ) )
   END

RETURN NIL

//--------------------------------------------------------------//
FUNCTION ValToPrg( xVal, cName, nPad, aObjs )

   LOCAL cType := ValType( xVal )
   LOCAL aVars, aVar, cRet, cPad, nObj

   //TraceLog( xVal, cName, nPad, aObjs )

   SWITCH cType
      CASE 'C'
         IF ! '"' IN xVal
            RETURN '"' + xVal + '"'
         ELSEIF ! "'" IN xVal
            RETURN "'" + xVal + "'"
         ELSEIF ( ! "[" IN xVal ) .AND. ( ! "]" IN xVal )
            RETURN "[" + xVal + "]"
         ELSE
            Throw( ErrorNew( "CSTR", 3102, ProcName(), "Can't stringify", { xVal } ) )
            EXIT
         ENDIF

      CASE 'D'
         RETURN "cToD( '" + dToC( xVal ) + "' )"

      CASE 'L'
         RETURN IIF( xVal, ".T.", ".F." )

      CASE 'N'
         RETURN Str( xVal )

      CASE 'M'
         RETURN xVal

      CASE 'A'
         cRet := "{ "

         FOR EACH aVar IN xVal
            cRet += ( ValToPrg( aVar ) + ", " )
         NEXT

         IF cRet[ -2 ] == ','
            cRet[ -2 ] := ' '
         ENDIF
         cRet[ -1 ] := '}'

         RETURN cRet

      CASE 'B'
         RETURN ValToPrgExp( xVal )

      CASE 'O'
         IF nPad == NIL
            nPad := 3
         ENDIF

         IF cName == NIL
            cName := "Unnamed: " + xVal:ClassName
         ENDIF

         cPad  := sPace( nPad + 3 )
         aVars := __objGetValueDiff( xVal )
         cRet  := Space( nPad ) + "OBJECT " + cName + " IS " + xVal:ClassName + CRLF

         IF aObjs == NIL
            aObjs := { { xVal, cName } }
         ELSE
            aAdd( aObjs, { xVal, cName } )
         ENDIF

         FOR EACH aVar IN aVars
            IF ValType( aVar[2] ) == 'O'
               IF ( nObj := aScan( aObjs, {|a| a[1] == aVar[2] } ) ) > 0
                  cRet += cPad + ":" + aVar[1] + " := " + "/* Cyclic into outer property: */ " + aObjs[ nObj ][2] + CRLF
               ELSE
                  cRet += ValToPrg( aVar[2], aVar[1], nPad + 3, aObjs ) + CRLF
               ENDIF
            ELSE
               //TraceLog( aVar[1], aVar[2] )
               cRet += cPad + ":" + aVar[1] + " := " + ValToPrg( aVar[2] ) + CRLF
            ENDIF
         NEXT

         RETURN cRet + sPace( nPad ) + "END OBJECT"

      DEFAULT
         //TraceLog( xVal, cName, nPad )
         IF xVal == NIL
            cRet := "NIL"
         ELSE
            Throw( ErrorNew( "CSTR", 3103, ProcName(), "Unsupported type", { xVal } ) )
         ENDIF
   END

   //TraceLog( cRet )

RETURN cRet
//--------------------------------------------------------------//

FUNCTION ValToPrgExp( xVal, aObjs )

   LOCAL cType := ValType( xVal )
   LOCAL aVars, aVar, cRet, nObj
   LOCAL cChar

   //TraceLog( xVal, cName, nPad, aObjs )

   SWITCH cType
      CASE 'C'
         IF ! '"' IN xVal
            RETURN '"' + xVal + '"'
         ELSEIF ! "'" IN xVal
            RETURN "'" + xVal + "'"
         ELSEIF ( ! "[" IN xVal ) .AND. ( ! "]" IN xVal )
            RETURN "[" + xVal + "]"
         ELSE
            Throw( ErrorNew( "CSTR", 3102, ProcName(), "Can't stringify", { xVal } ) )
            EXIT
         ENDIF

      CASE 'D'
         RETURN "cToD( '" + dToC( xVal ) + "' )"

      CASE 'L'
         RETURN IIF( xVal, ".T.", ".F." )

      CASE 'N'
         RETURN Str( xVal )

      CASE 'M'
         RETURN xVal

      CASE 'A'
         cRet := "{ "

         FOR EACH aVar IN xVal
            cRet += ( ValToPrgExp( aVar ) + ", " )
         NEXT

         IF cRet[ -2 ] == ','
            cRet[ -2 ] := ' '
         ENDIF
         cRet[ -1 ] := '}'

         RETURN cRet

      CASE 'B'
         cRet := "HB_RestoreBlock( {"
         xVal := HB_SaveBlock( xVal )

         cRet += '"' + xVal[1] + [", ]

         FOR EACH cChar IN xVal[2]
             cRet += "Chr(" + Str( cChar, 3 ) + ")+"
         NEXT

         cRet[-1] := ", "
         cRet += Str( xVal[3], 3 ) + "} )"

         RETURN cRet

      CASE 'O'
         aVars := __objGetValueDiff( xVal )
         cRet  := "M->_1 := " + xVal:ClassName + "(), "

         IF aObjs == NIL
            aObjs := { xVal }
         ELSE
            aAdd( aObjs, xVal )
         ENDIF

         FOR EACH aVar IN aVars
            IF ValType( aVar[2] ) == 'O'
               IF ( nObj := aScan( aObjs, xVal ) ) > 0
                  cRet += "M->_1:" + aVar[1] + " := " + "/* Cyclic into outer property: */ " + aObjs[ nObj ][2] + ", "
               ELSE
                  cRet += ValToPrgExp( aVar[2], aObjs ) + ", "
               ENDIF
            ELSE
               //TraceLog( aVar[1], aVar[2] )
               cRet += "M->_1:" + aVar[1] + " := " + ValToPrgExp( aVar[2] ) + ", "
            ENDIF
         NEXT

         RETURN cRet + "M->_1"

      DEFAULT
         //TraceLog( xVal, cName, nPad )
         IF xVal == NIL
            cRet := "NIL"
         ELSE
            Throw( ErrorNew( "CSTR", 3103, ProcName(), "Unsupported type", { xVal } ) )
         ENDIF
   END

   TraceLog( cRet )

RETURN cRet
//--------------------------------------------------------------//
