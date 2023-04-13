/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 GetSecret function
 *   Generic Password reader
 *
 * Copyright 2004 Luiz Rafael Culik Guimaraes <culikr@uol.com.br>
 *
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


#include "setcurs.ch"

#command @ <row>, <col> GET <var>              ;
      [<clauses,...>]                          ;
      PASSWORD                                 ;
      [<moreClauses,...>]                      ;
                                               ;
      => @ < row > , < col > GET < var >       ;
      [<clauses>]                              ;
      SEND reader := {|oGet|                   ;
      GetPassword( oGet, Len( < var > )  ) }   ;
      [<moreClauses>]

#include "getexit.ch"
#include "inkey.ch"
#include "common.ch"

PROCEDURE GetPassword( oGet, nLen )

   LOCAL nKey
   LOCAL nSaveCursor := 0

// read the GET if the WHEN condition is satisfied
   IF ( GetPreValidate( oGet ) )
      // activate the GET for reading
      oGet:SetFocus()

      oGet:cargo := ""
      DO WHILE ( oGet:exitState == GE_NOEXIT )
         // check for initial typeout (no editable positions)
         IF ( oGet:typeOut )
            oGet:exitState := GE_ENTER
         ENDIF

         // apply keystrokes until exit
         DO WHILE ( oGet:exitState == GE_NOEXIT )
            SetCursor( iif( nSaveCursor == SC_NONE, SC_NORMAL, nSaveCursor ) )
            nKey := Inkey( 0 )
            SetCursor( SC_NONE )
            IF nKey >= 32 .AND. nKey <= 255
               oGet:cargo += Chr( nKey )
               GetApplyKey( oGet, Asc( "*" ) )
            ELSEIF nKey == K_BS
               oGet:cargo := SubStr( oGet:cargo, 1, Len( oGet:cargo ) - 1 )
               GetApplyKey( oGet, nKey )
            ELSEIF nKey == K_ESC
               oGet:Undo()
               oGet:ExitState := GE_ESCAPE
               EXIT
            ELSEIF nKey == K_ENTER
               GetApplyKey( oGet, nKey )
            ENDIF
         ENDDO

         // disallow exit if the VALID condition is not satisfied
         IF ( !GetPostValidate( oGet ) )
            oGet:exitState := GE_NOEXIT
         ENDIF
      ENDDO
      // de-activate the GET
      oGet:KillFocus()
   ENDIF

   IF oGet:exitState != GE_ESCAPE
      IF nLen = NIL
         nLen := Len( oGet:varget() )
      ENDIF
      oGet:varPut( oGet:cargo + Space( nLen - Len( oGet:cargo ) ) )
   ENDIF

   RETURN

FUNCTION GetSecret( cDef, nRow, nCol, lSay, lPrompt )

   LOCAL OldRow := Row()
   LOCAL OldCol := Col()
   LOCAL GetList := {}
   LOCAL cVar   := Space( Len( cDef ) )

   DEFAULT nRow TO Row(), nCol TO Col(), lSay TO .F.

   IF lPrompt <> Nil
      @ nRow, nCol SAY lPrompt GET cVar PASSWORD
   ELSE
      @ nRow, nCol GET cVar PASSWORD
   ENDIF

   READ SAVE
   SetPos( OldRow, OldCol )

   RETURN cVar
