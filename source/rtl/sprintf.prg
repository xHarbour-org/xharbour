/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * sprintf() function
 *
 * Copyright 2003 Mauricio Abre <maurifull@datafull.com>
 * www - http://www.xharbour.org
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

FUNCTION sprintf( ... )

   LOCAL aPar, cStr, nPar, nPos, cTok
   LOCAL nLen := 0, lUns, l0 := .F. , lSign := .F. , nDec

   aPar := hb_AParams()
   cStr := ""
   nPar := 2
   WHILE !Empty( aPar[1] )
      nPos := Len( aPar[1] ) + 1
      cTok := Nil
      IF '%' $ aPar[1]
         nPos := At( '%', aPar[1] )
         cTok := '%'
      End
      IF '\' $ aPar[1] .AND. At( '\', aPar[1] ) < nPos
         nPos := At( '\', aPar[1] )
         cTok := '\'
      End
	
      cStr += Left( aPar[1], nPos - 1 )

      DO CASE
      CASE cTok == Nil
         EXIT
      CASE cTok == '\'
         Switch aPar[1, ++nPos]
         CASE 't'
            cStr += '    '
            EXIT
         CASE 'n'
            cStr += hb_osNewLine()
            EXIT
            DEFAULT
            cStr += aPar[1, nPos]
            EXIT
         End
      CASE cTok == '%'
         lUns := .F.
         Switch aPar[1, ++nPos]
         CASE '%'
            cStr += '%'
            EXIT
         CASE '+'
            aPar[1] := Left( aPar[1], nPos - 1 ) + SubStr( aPar[1], nPos + 1 )
            nPos := At( '%', aPar[1] ) - 1
            lSign := .T.
            EXIT
         CASE '0'
            aPar[1] := Left( aPar[1], nPos - 1 ) + SubStr( aPar[1], nPos + 1 )
            nPos := At( '%', aPar[1] ) - 1
            l0 := .T.
            EXIT
         CASE '1'
         CASE '2'
         CASE '3'
         CASE '4'
         CASE '5'
         CASE '6'
         CASE '7'
         CASE '8'
         CASE '9'
            nLen := Val( SubStr( aPar[1], nPos ) )
            cTok := Left( aPar[1], nPos - 1 )
            WHILE aPar[1, nPos] $ '1234567890.'
               nPos++
            End
            aPar[1] := cTok + SubStr( aPar[1], nPos )
            nPos := At( '%', aPar[1] ) - 1
            EXIT
         CASE 'u'
            lUns := .T.
            nPos++
         CASE 'd'
         CASE 'l'
         CASE 'f'
            IF nLen != 0
               IF nLen - Int( nLen ) > 0.0
                  nDec := Str( nLen )
                  WHILE Right( nDec, 1 ) == '0'
                     nDec := Left( nDec, Len( nDec ) - 1 )
                  End
                  nDec := Val( SubStr( nDec, At('.', nDec ) + 1 ) )
               ELSE
                  nDec := 0
               End
               cTok := Str( iif( lUns, Abs(aPar[nPar++] ), aPar[nPar++] ), nLen, nDec )
            ELSE
               cTok := LTrim( Str( iif(lUns, Abs(aPar[nPar++] ), aPar[nPar++] ) ) )
            End
            IF l0
               IF '-' $ cTok .AND. cTok[1] != '-'
                  cTok := StrTran( cTok, '-', ' ' )
                  cTok[1] := '-'
               End
               cTok := StrTran( cTok, ' ', '0' )
               l0 := .F.
            End
            IF lSign .AND. cTok[1] != '-'
               IF nLen == 0
                  cTok := '+' + cTok
               ELSE
                  cTok[1] := '+'
               End
               lSign := .F.
            End
            nLen := 0
            cStr += cTok
            EXIT
         CASE 'c'
         CASE 's'
            IF nLen == 0
               nLen := Len( aPar[nPar] )
            End
            cStr += PadL( aPar[nPar++], nLen )
            nLen := 0
            l0 := .F.
            lSign := .F.
            EXIT
         End
      End

      aPar[1] := SubStr( aPar[1], nPos + 1 )
   End

   RETURN cStr
