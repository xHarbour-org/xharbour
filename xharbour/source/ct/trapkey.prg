/*
 * $Id$
 */
/*
 * Harbour Project source code:
 *   CT3 functions: TRAPINPUT(), TRAPSHIFT(), INKEYTRAP()
 *
 * Copyright 2007 Pavel Tsarenko <tpe2@mail.ru>
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

#include "common.ch"

STATIC s_Proc1, s_Param
STATIC s_Proc2, s_Bitmap
STATIC bTrap := {|| Trap() }

FUNCTION TrapInput( cProc, lParam )

   DEFAULT lParam TO .F.

   s_Proc1 := cProc
   s_Param := lParam

   SetInkeyAfterBlock( if( ! Empty(s_Proc1 ) .OR. ! Empty(s_Proc2 ), bTrap, nil ) )

   RETURN s_Proc1

FUNCTION TrapShift( cProc, nBitmap )

   s_Proc2 := cProc
   s_Bitmap := nBitmap

   SetInkeyAfterBlock( if( ! Empty(s_Proc1 ) .OR. ! Empty(s_Proc2 ), bTrap, nil ) )

   RETURN s_Proc2

STATIC FUNCTION Trap

   LOCAL nKbdStat, iRet := 0

   IF ! Empty( s_Proc1 )
      IF s_Param
         DO( s_Proc1, ProcName( 1 ), ProcLine( 1 ), "" )
      ELSE
         DO( s_Proc1 )
      ENDIF
   ENDIF

   IF ! Empty( s_Proc2 )
      nKbdStat := KbdStat()

      IF ( ( nKbdStat & 1 ) .AND. ( s_Bitmap & 3 ) ) .OR. ;   // Shift
         ( ( nKbdStat & 4 ) .AND. ( s_Bitmap & 8 ) ) .OR. ;   // Control
         ( ( nKbdStat & 8 ) .AND. ( s_Bitmap & 8 ) )         // Alt

         DO( s_Proc2 )

      ENDIF

   ENDIF

   RETURN iRet

FUNCTION InkeyTrap( nDelay )

   LOCAL nKey := Inkey( 0 )
   LOCAL bKeyBlock

   HB_SYMBOL_UNUSED( nDelay )

   IF nKey # 0 .AND. ( bKeyBlock := SetKey( nKey ) ) <> NIL
      Eval( bKeyBlock, ProcName( 1 ), ProcLine( 1 ), "" )
   ENDIF

   RETURN nKey
