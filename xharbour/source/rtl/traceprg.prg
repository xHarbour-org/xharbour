/*
 * $Id: traceprg.prg,v 1.1 2002/01/31 05:08:31 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 * PRG Tracing System
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

#DEFINE  CRLF HB_OsNewLine()

//--------------------------------------------------------------//
FUNCTION TraceLog( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15 )

   LOCAL FileHandle, ProcName, Counter := 1, aParams, nParams := PCount()

   IF ! SET( _SET_TRACE )
      RETURN .T.
   ENDIF

   aParams := { p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15 }

   FileHandle := FOpen( 'Trace.Log', 1 )

   FSeek( FileHandle, 0, 2 )

   FWrite( FileHandle, '[' + ProcName(1) + '] (' + Str( Procline(1), 5 ) + ') Called from: '  + CRLF )

   DO WHILE ! ( ( ProcName := ProcName( ++Counter ) ) == '' )
      FWrite( FileHandle, space(30) + ProcName + '(' + Str( Procline( Counter), 5 ) + ')' + CRLF )
   ENDDO

   FOR Counter := 1 to nParams
      FWrite( FileHandle, '>>>' + CStr( aParams[Counter] ) + '<<<' + CRLF )
   NEXT

   FWrite( FileHandle, CRLF )

   FClose(FileHandle)

RETURN .T.

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

