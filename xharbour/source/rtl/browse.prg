/*
 * $Id: browse.prg,v 1.2 2003/01/27 03:37:23 walito Exp $
 */

/*
 * Harbour Project source code:
 * Quick Clipper Browse()
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include "inkey.ch"
static bBlock := { || setcursor(iif( readinsert(!readinsert()), 1, 2 ;
   )) }

function Browse( nTop, nLeft, nBottom, nRight )

   local oBrw
   local cOldScreen
   local n, nOldCursor
   local nKey := 0
   local lExit := .f.
   local lGotKey := .f.
   Local lBottom,lRefresh
   Local bAction
   Local lKeyPressed

   if ! Used()
      return .f.
   end

   if PCount() < 4
      nTop    := 1
      nLeft   := 0
      nBottom := MaxRow()
      nRight  := MaxCol()
   endif

   nOldCursor := SetCursor( 0 )
   cOldScreen := SaveScreen( nTop, nLeft, nBottom, nRight )

   @ nTop, nLeft TO nBottom, nRight
   @ nTop + 3, nLeft SAY Chr( 198 )
   @ nTop + 3, nRight SAY Chr( 181 )
   @ nTop + 1, nLeft + 1 SAY Space( nRight - nLeft - 1 )

   oBrw := TBrowseDB( nTop + 2, nLeft + 1, nBottom - 1, nRight - 1 )
   oBrw:HeadSep := " " + Chr( 205 )
   oBrw:skipblock({ |_1| skipped(_1, lBottom) })

   for n := 1 to FCount()
      oBrw:AddColumn( TBColumnNew( FieldName( n ), FieldBlock( FieldName( n ) ) ) )
   next

   if ( EOF() )
      goto top
   endif

   lBottom := lRefresh := .F.

   oBrw:ForceStable()

   if ( LastRec() == 0 )
      nKey := 24
      lKeyPressed := .T.
   else
      lKeyPressed := .F.
   endif

   while ! lExit

      if ( !lKeyPressed)       
         while !oBrw:stabilize() 
            if ( ( nKey := InKey() ) != 0 )
               lKeyPressed := .T.
               exit
            endif
         enddo
      endif

      if (!lKeyPressed)
           if ( oBrw:hitbottom() .AND. ( !lBottom .OR. RecNo() != ;
               LastRec() + 1 ) )
             if ( lBottom )
               oBrw:refreshcurrent()
               do while ( !oBrw:stabilize() )
               enddo
               goto bottom
            else
               lBottom := .T.
               setcursor(iif( readinsert(), 2, 1 ))
            endif
            oBrw:down()
            do while ( !oBrw:stabilize() )
            enddo
         endif

         Statline( oBrw )
         oBrw:forceStable()
         nKey := Inkey( 0 )
         if ( bAction := SetKey( nKey ) ) != nil
            Eval( bAction, ProcName( 1 ), ProcLine( 1 ), "" )
            loop
         endif
      else
         lKeyPressed := .f.

      endif
      switch nKey
         case K_DOWN
            if ( lBottom  )
               oBrw:hitbottom := .T.
            else
               oBrw:Down()
            endif
            exit

         case K_UP
            if ( lBottom )
               lRefresh := .T.
            else
               oBrw:Up()
            endif
            exit

         case K_PGUP
            if ( lBottom )
               lRefresh := .T.
            else
               oBrw:PageUp()
            endif
            exit


         case K_PGDN
            if ( lBottom )
               oBrw:hitbottom := .T.
            else
               oBrw:PageDown()
            endif
            exit

         case K_ESC
            lExit := .t.
            exit

         case K_END
            oBrw:End()
            exit

         case K_HOME
            oBrw:Home()
            exit

         case K_LEFT
            oBrw:Left()
            exit

         case K_RIGHT
            oBrw:Right()
           exit

         case K_CTRL_PGUP
            if ( lBottom )
               lRefresh := .T.
            else
               oBrw:GoTop()
            endif
            exit

         case K_CTRL_PGDN
            if ( lBottom )
               lRefresh := .T.
            else
               oBrw:GoBottom()
            endif
            exit

         case K_CTRL_LEFT
            oBrw:panLeft()
            exit

         case K_CTRL_RIGHT
            oBrw:panRight()
            exit

         case K_CTRL_HOME
            oBrw:panHome()
            exit

         case K_CTRL_END
            oBrw:panEnd()
            exit
         case K_INS
            if ( lBottom )
               eval(bBlock)
            endif
            exit
        case K_DEL
            if ( RecNo() != LastRec() + 1 )
               if ( Deleted() )
                  recall
               else
                  delete
               endif
            endif
            exit

        case K_ENTER
            if ( lBottom .OR. RecNo() != LastRec() + 1 )
               nKey := doget(oBrw, lBottom)
               lKeyPressed := nKey != 0
            else
               nKey := 24
               lKeyPressed := .T.
            endif
            exit
        default
          keyboard Chr(13) + Chr(nKey)
          exit
        end
      if ( lRefresh )
         lRefresh := .F.
         lBottom := .F.
         freshorder(oBrw)
         setcursor(0)
      endif

   end

   RestScreen( nTop, nLeft, nBottom, nRight, cOldScreen )
   SetCursor( nOldCursor )

return .t.

static procedure Statline( oBrw ,lBottom)

   local nTop   := oBrw:nTop - 1
   local nRight := oBrw:nRight

   @ nTop, nRight - 27 SAY "Record "

   if LastRec() == 0
      @ nTop, nRight - 20 SAY "<none>               "
   elseif RecNo() == LastRec() + 1
      @ nTop, nRight - 40 SAY "         "
      @ nTop, nRight - 20 SAY "                <new>"
   else
      @ nTop, nRight - 40 SAY iif( Deleted(), "<Deleted>", "         " )
      @ nTop, nRight - 20 SAY PadR( LTrim( Str( RecNo() ) ) + "/" +;
                                    Ltrim( Str( LastRec() ) ), 16 ) +;
                              iif( oBrw:hitTop, "<bof>", "     " )
   endif

return


static function DOGET( oBrowse, lValue )

   local bIns, lScore, lExit, cData, oGet, nExitState, nIndexKey, ;
      xKeyValue, lSuccess, nCursor, xData, cForExp
   oBrowse:hittop(.F.)
   statline(oBrowse, lValue)
   do while ( !oBrowse:stabilize() )
   enddo
   lScore := Set(_SET_SCOREBOARD, .F.)
   lExit := Set(_SET_EXIT, .T.)
   bIns := SetKey(K_INS, bBlock)
   nCursor := setcursor(iif( readinsert(), 2, 1 ))
   nIndexKey := indexkey(0)
   if ( !Empty(nIndexKey) )
      xKeyValue := &nIndexKey
   endif
   cData := oBrowse:getcolumn(oBrowse:colpos())
   xData := eval(cData:block())
   oGet := getnew(Row(), Col(), { |_1| iif( PCount() == 0, xData, ;
      xData := _1 ) }, "mGetVar", Nil, oBrowse:colorspec())
   lSuccess := .F.
   if ( ReadModal({oGet}) )
      if ( lValue .AND. RecNo() == LastRec() + 1 )
         append blank
      endif
      eval(cData:block(), xData)
      if ( !lValue .AND. !Empty(cForExp := ordfor(indexord())) .AND. ;
            !&cForExp )
         goto top
      endif
      if ( !lValue .AND. !Empty(nIndexKey) .AND. xKeyValue != &nIndexKey )
         lSuccess := .T.
      endif
   endif
   if ( lSuccess )
      freshorder(oBrowse)
      nExitState := 0
   else
      oBrowse:refreshcurrent()
      nExitState := exitkey(lValue)
   endif
   setcursor(nCursor)
   set scoreboard (lScore)
   Set(_SET_EXIT, lExit)
   SetKey(K_INS, bIns)
   return nExitState

static function EXITKEY( lExit )

   local nReturn := LastKey()
   do case
   case nReturn == 3
      if ( lExit )
         nReturn := 0
      else
         nReturn := 24
      endif
   case nReturn == 18
      if ( lExit )
         nReturn := 0
      else
         nReturn := 5
      endif
   case nReturn == 13 .OR. nReturn >= 32 .AND. nReturn <= 255
      nReturn := 4
   case nReturn != 5 .AND. nReturn != 24
      nReturn := 0
   endcase
   return nReturn

static function FRESHORDER( oBrowse )

   local nRec := RecNo()
   oBrowse:refreshall()
   do while ( !oBrowse:stabilize() )
   enddo
   if ( nRec != LastRec() + 1 )
      do while ( RecNo() != nRec .AND. !BOF() )
         oBrowse:up()
         do while ( !oBrowse:stabilize() )
         enddo
      enddo
   endif
   return Nil

static function SKIPPED( nSkip, lSkip )

   local nPos := 0
   if ( LastRec() != 0 )
      if ( nSkip == 0 )
         skip 0
      elseif ( nSkip > 0 .AND. RecNo() != LastRec() + 1 )
         do while ( nPos < nSkip )
            skip 
            if ( EOF() )
               if ( lSkip )
                  nPos++
               else
                  skip -1
               endif
               exit
            endif
            nPos++
         enddo
      elseif ( nSkip < 0 )
         do while ( nPos > nSkip )
            skip -1
            if ( BOF() )
               exit
            endif
            nPos--
         enddo
      endif
   endif
   return nPos
