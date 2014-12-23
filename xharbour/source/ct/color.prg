/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 video functions (color-like functions)
 *
 *     Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>:
 *                        - INVERTATTR()
 *
 *     Copyright 2002 Walter Negro <anegro@overnet.com.ar>:
 *                        - NTOCOLOR()
 *                        - COLORTON()
 *
 *     Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>:
 *                        - ENHANCED()
 *                        - STANDARD()
 *                        - UNSELECTED()
 *
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

#include "color.ch"
#include "common.ch"

FUNCTION INVERTATTR( xAttr )

   LOCAL n := ColorToN( xAttr )

   RETURN ( n & 0x0F ) << 4 + ( n >> 4 )

FUNCTION NTOCOLOR( nColor, lChar )

   LOCAL nColorFore
   LOCAL nColorBack
   LOCAL lHiColor
   LOCAL lBlinking
   LOCAL cColor := ""

   DEFAULT lChar TO .F.

   IF HB_ISNUMERIC( nColor ) .AND. nColor >= 0 .AND. nColor < 256

      nColorFore := nColor % 16
      nColorBack := Int( nColor / 16 )

      IF !lChar

         cColor := StrZero( nColorFore, 2 ) + "/" + StrZero( nColorBack, 2 )

      ELSE

         lHiColor   := nColorFore > 7
         lBlinking  := nColorBack > 7

         nColorFore := nColorFore % 8
         nColorBack := nColorBack % 8

         cColor := n2c( nColorFore ) + if( lHiColor, "+", "" ) + "/" + ;
            n2c( nColorBack ) + if( lBlinking, "*", "" )

      ENDIF
   ENDIF

   RETURN cColor

STATIC FUNCTION n2c( nColor )

   SWITCH INT( nColor )
   CASE 0
      RETURN "N"
   CASE 1
      RETURN "B"
   CASE 2
      RETURN "G"
   CASE 3
      RETURN "BG"
   CASE 4
      RETURN "R"
   CASE 5
      RETURN "BR"
   CASE 6
      RETURN "GR"
   CASE 7
      RETURN "W"
   ENDSWITCH

   RETURN ""

STATIC FUNCTION c2n( cColor )

   LOCAL nColor := 0

   cColor := Upper( cColor )

   nColor += if( "B" $ cColor, 1, 0 )
   nColor += if( "G" $ cColor, 2, 0 )
   nColor += if( "R" $ cColor, 4, 0 )
   nColor += if( "W" $ cColor, 7, 0 )

   RETURN nColor

FUNCTION COLORTON( cColor )

   LOCAL cColorFore, cColorBack
   LOCAL nColorFore, nColorBack
   LOCAL lHiColor := .F., lBlinking := .F.
   LOCAL nSep

   IF HB_ISNUMERIC( cColor )
      RETURN cColor
   ENDIF

   IF HB_ISSTRING( cColor )

      IF ( nSep := At( ",", cColor ) ) <> 0
         cColor := Left( cColor, nSep - 1 )
      ENDIF

      IF ( nSep := At( "/", cColor ) ) == 0

         cColorFore := cColor
         cColorBack := ""
      ELSE

         cColorFore := AllTrim( SubStr( cColor, 1, nSep - 1 ) )
         cColorBack := AllTrim( SubStr( cColor, nSep + 1 ) )
      ENDIF

      IF "+" $ cColorFore .OR. "+" $ cColorBack
         lHiColor   := .T.
         cColorFore := StrTran( cColorFore, "+", "" )
         cColorBack := StrTran( cColorBack, "+", "" )
      ENDIF

      IF "*" $ cColorFore .OR. "*" $ cColorBack
         lBlinking  := .T.
         cColorFore := StrTran( cColorFore, "*", "" )
         cColorBack := StrTran( cColorBack, "*", "" )
      ENDIF

      nColorFore := Val( cColorFore )
      nColorBack := Val( cColorBack )

      IF nColorFore > 0 .OR. nColorBack > 0
         RETURN nColorFore + nColorBack * 16
      ENDIF

      IF Len( cColorFore ) > 2 .OR. Len( cColorBack ) > 2
         RETURN 0
      ENDIF

      nColorFore := c2n( cColorFore )
      nColorBack := c2n( cColorBack )

      IF nColorFore > 7 .OR. nColorBack > 7
         RETURN 0
      ENDIF

      nColorFore += if( lHiColor, 8, 0 )
      nColorBack += if( lBlinking, 8, 0 )

      RETURN nColorFore + nColorBack * 16
   ENDIF

   RETURN 0

FUNCTION ENHANCED()

   ColorSelect( CLR_ENHANCED )

   RETURN ""

FUNCTION STANDARD()

   ColorSelect( CLR_STANDARD )

   RETURN ""

FUNCTION UNSELECTED()

   ColorSelect( CLR_UNSELECTED )

   RETURN ""
