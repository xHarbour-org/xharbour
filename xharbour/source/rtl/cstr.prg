/*
 * $Id: cstr.prg,v 1.3 2002/10/09 06:46:59 ronpinkas Exp $
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

   DO CASE
      CASE cType = 'C'
         RETURN xExp

      CASE cType = 'D'
         RETURN dToc( xExp )

      CASE cType = 'L'
         RETURN IIF( xExp, '.T.', '.F.' )

      CASE cType = 'N'
         RETURN Str( xExp )

      CASE cType = 'M'
         RETURN xExp

      CASE cType = 'A'
         RETURN "{ Array of " +  LTrim( Str( Len( xExp ) ) ) + " Items }"

      CASE cType = 'B'
         RETURN '{|| Block }'

      CASE cType = 'O'
         RETURN "{ " + xExp:ClassName() + " Object }"

      OTHERWISE
         RETURN "Type: " + cType
   ENDCASE

RETURN ""

//--------------------------------------------------------------//
FUNCTION CStrToVal( cExp, cType )

   IF ValType( cExp ) != 'C'
      __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
   ENDIF

   DO CASE
      CASE cType = 'C'
         RETURN cExp

      CASE cType = 'D'
         IF cExp[3] >= '0' .AND. cExp[3] <= '9' .AND. cExp[5] >= '0' .AND. cExp[5] <= '9'
            RETURN cToD( cExp )
         ELSE
            RETURN sToD( cExp )
         ENDIF

      CASE cType = 'L'
         RETURN IIF( cExp[1] == 'T' .OR. cExp[2] == 'Y' .OR. cExp[2] == 'T' .OR. cExp[2] == 'Y', .T., .F. )

      CASE cType = 'N'
         RETURN Val( cExp )

      CASE cType = 'M'
         RETURN cExp

      /*
      CASE cType = 'A'
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )

      CASE cType = 'B'
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )

      CASE cType = 'O'
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
      */

      OTHERWISE
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
   ENDCASE

RETURN NIL

//--------------------------------------------------------------//
FUNCTION ValToPrg( xVal, cName, nPad, aObjs )

   LOCAL cType := ValType( xVal )
   LOCAL aVars, aVar, cRet, cPad, nObj

   DO CASE
      CASE cType = 'C'
         IF ! '"' $ xVal
            RETURN '"' + xVal + '"'
         ELSEIF ! "'" $ xVal
            RETURN "'" + xVal + "'"
         ELSEIF ( ! "[" $ xVal ) .AND. ( ! "]" $ xVal )
            RETURN "[" + xVal + "]"
         ELSE
            __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
         ENDIF

      CASE cType = 'D'
         RETURN "cToD( '" + dToC( xVal ) + "' )"

      CASE cType = 'L'
         RETURN IIF( xVal, ".T.", ".F." )

      CASE cType = 'N'
         RETURN Str( xVal )

      CASE cType = 'M'
         RETURN xVal

      CASE cType = 'A'
         cRet := "{ "

         FOR EACH aVar IN xVal
            cRet += ( ValToPrg( aVar ) + ", " )
         NEXT

         IF cRet[ -2 ] == ','
            cRet[ -2 ] := ' '
         ENDIF
         cRet[ -1 ] := '}'

      /*
      CASE cType = 'B'
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
      */

      CASE cType == 'O'
         cPad  := sPace( nPad + 3 )
         aVars := __objGetValueDiff( xVal )
         cRet  := Space( nPad ) + "OBJECT " + cName + " IS " + xVal:ClassName + CRLF + CRLF

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
               cRet += cPad + ":" + aVar[1] + " := " + ValToPrg( aVar[2] ) + CRLF
            ENDIF
         NEXT

         cRet += CRLF + sPace( nPad ) + "END OBJECT" + CRLF

      OTHERWISE
         return ""
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName(), 1, { xVal, cName, nPad } )
   ENDCASE

   //TraceLog( cRet )

RETURN cRet
//--------------------------------------------------------------//
