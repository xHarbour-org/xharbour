/*
 * $Id: cstr.prg,v 1.7 2003/01/27 03:37:23 walito Exp $
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
      __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
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
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )

      CASE 'B'
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )

      CASE 'O'
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
      */

      DEFAULT
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
   END

RETURN NIL

//--------------------------------------------------------------//
FUNCTION ValToPrg( xVal, cName, nPad, aObjs )

   LOCAL cType := ValType( xVal )
   LOCAL aVars, aVar, cRet, cPad, nObj

   SWITCH cType
      CASE 'C'
         IF ! '"' IN xVal
            RETURN '"' + xVal + '"'
         ELSEIF ! "'" IN xVal
            RETURN "'" + xVal + "'"
         ELSEIF ( ! "[" IN xVal ) .AND. ( ! "]" IN xVal )
            RETURN "[" + xVal + "]"
         ELSE
            __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
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

      /*
      CASE 'B'
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName() )
      */

      CASE 'O'
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
               cRet += cPad + ":" + aVar[1] + " := " + ValToPrg( aVar[2] ) + CRLF
            ENDIF
         NEXT

         RETURN cRet + sPace( nPad ) + "END OBJECT"

      DEFAULT
         __ErrRT_BASE( EG_ARG, 3101, NIL, ProcName(), 1, { xVal, cName, nPad } )
   END

   //TraceLog( cRet )

RETURN cRet
//--------------------------------------------------------------//
