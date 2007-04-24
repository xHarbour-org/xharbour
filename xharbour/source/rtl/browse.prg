/*
 * $Id: browse.prg,v 1.8 2007/04/16 15:33:14 modalsist Exp $
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
#include "setcurs.ch"

static bBlock := { || setcursor( iif( readinsert(!readinsert()), SC_NORMAL, SC_INSERT )) }

function Browse( nTop, nLeft, nBottom, nRight )

   local oBrw
   local cOldScreen
   local n, nOldCursor
   local nKey := 0
   local lExit := .f.
   local lGotKey := .f.
   Local lRefresh
   Local bAction
   Local lKeyPressed
   Local aRect, cField, oCol

   if ! Used()
      return .f.
   end

   if PCount() < 4
      nTop    := 1
      nLeft   := 0
      nBottom := MaxRow()
      nRight  := MaxCol()
   endif

   nOldCursor := SetCursor( SC_NONE )
   cOldScreen := SaveScreen( nTop, nLeft, nBottom, nRight )

   @ nTop, nLeft TO nBottom, nRight
   @ nTop + 3, nLeft SAY Chr( 198 )
   @ nTop + 3, nRight SAY Chr( 181 )
   @ nTop + 1, nLeft + 1 SAY Space( nRight - nLeft - 1 )

   lRefresh := .F.

   oBrw := TBrowseDB( nTop + 2, nLeft + 1, nBottom - 1, nRight - 1 )
   oBrw:HeadSep := " " + Chr( 205 )
   oBrw:Cargo := .f. // to assign append mode

   oBrw:skipblock := { |_1| Skipped(_1, oBrw ) }

   for n := 1 to FCount()
      cField := FieldName( n )
      if valtype( fieldget( n ) ) == "M"
         oBrw:AddColumn( TBColumnNew( cField, &( "{|| ' <Memo> '}" ) ) )
      else
         oBrw:AddColumn( TBColumnNew( cField, FieldBlock( cField ) ) )
      endif
   next

   if eof()
      dbgotop()
   endif

   oBrw:ForceStable()

   if IsEmpty()
      nKey := K_DOWN
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

         Statline( oBrw )

         oBrw:forceStable()

         if eof()
            CursorOn()
            aRect := {oBrw:RowPos,1,oBrw:RowPos,oBrw:ColCount}
            oBrw:ColorRect(aRect,{2,1})
         else
            CursorOff()
         endif

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
            if IsBottom() .or. IsEmpty()
               oBrw:Cargo := .t.
            endif
            oBrw:Down()
            exit

         case K_UP
            if eof()
               lRefresh := .t.
            endif
            oBrw:Up()
            exit

         case K_PGUP
            if eof()
               lRefresh := .t.
            endif
            oBrw:PageUp()
            exit

         case K_PGDN
            if IsBottom() .or. IsEmpty()
               oBrw:Cargo := .t.
               oBrw:Down()
            else
               oBrw:PageDown()
            endif
            exit

         case K_CTRL_PGUP
            if eof()
               lRefresh := .t.
            endif
            oBrw:GoTop()
            exit

         case K_CTRL_PGDN
            if eof()
               lRefresh := .T.
            endif
            oBrw:GoBottom()
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
            eval(bBlock)
            exit

        case K_DEL
            if ! eof()
               if ( Deleted() )
                  dbrecall()
               else
                  dbdelete()
               endif
            endif
            exit

        case K_ENTER
            nKey := doget( oBrw )
            lKeyPressed := (nKey != 0)
            exit
        default
            keyboard Chr(13) + Chr(nKey)
            exit
      end

      if ( lRefresh )
         lRefresh := .F.
         freshorder(oBrw)
         CursorOff()
      endif

   enddo

   RestScreen( nTop, nLeft, nBottom, nRight, cOldScreen )
   SetCursor( nOldCursor )

return .t.

*--------------------------------
static procedure Statline( oBrw )

   local nTop   := oBrw:nTop - 1
   local nRight := oBrw:nRight

   @ nTop, nRight - 27 SAY "Record "

   if IsEmpty()
      @ nTop, nRight - 20 SAY "<none>               "
   elseif eof()
      @ nTop, nRight - 40 SAY "         "
      @ nTop, nRight - 20 SAY "                <new>"
   else
      @ nTop, nRight - 40 SAY iif( Deleted(), "<Deleted>", "         " )
      @ nTop, nRight - 20 SAY PadR( LTrim( Str( RecNo() ) ) + "/" +;
                                    Ltrim( Str( LastRec() ) ), 16 ) +;
                              iif( oBrw:hitTop, "<bof>", "     " )
   endif

return

*-------------------------------
static function DOGET( oBrowse )

   local bIns, lScore, lExit, oCol, oGet, nExitState, nIndexKey, ;
      xKeyValue, lSuccess, nCursor, xData, cForExp, lSave, ;
      cMemoColor, nTop, nLeft, nBottom, nRight, cMemoScreen, ;
      cMemoField, lMemo

   oBrowse:hittop := .F.
   oBrowse:hitbottom := .F.
   lMemo := .f.

   statline( oBrowse )

   do while ( !oBrowse:stabilize() )
   enddo

   lScore    := Set(_SET_SCOREBOARD, .F.)
   lExit     := Set(_SET_EXIT, .T.)
   bIns      := SetKey(K_INS, bBlock)
   nCursor   := CursorOn()
   nIndexKey := indexkey(0)

   if ( !Empty(nIndexKey) )
      xKeyValue := &nIndexKey
   endif

   oCol := oBrowse:getcolumn(oBrowse:colpos())

   lMemo := ( ValType( FieldGet( FieldPos( oCol:Heading ) ) ) == 'M' )
   IF lMemo
      cMemoField := oCol:Heading 
      xData := FieldGet( FieldPos( cMemoField ) )
      nTop    :=  oBrowse:nTop+5
      nLeft   :=  oBrowse:nLeft+10
      nBottom :=  oBrowse:nBottom-5
      nRight  :=  oBrowse:nRight-10
      if nBottom - nTop <= 10
         nTop := 5
         nBottom := MaxRow()-5
      endif
      if nRight - nLeft <= 10
         nLeft := 10
         nRight := MaxCol()-10
      endif
      cMemoScreen := SaveScreen( nTop, nLeft, nBottom, nRight )
      cMemoColor := SetColor( SubStr(oBrowse:ColorSpec, At(",",oBrowse:ColorSpec)+1 ) )
      @ nTop, nLeft CLEAR TO nBottom, nRight
      @ nTop, nLeft TO nBottom, nRight
      @ nTop, nLeft+1 Say Padc( "[ "+oCol:heading+" ]" , nRight - nLeft - 2, chr(196) )
      xData := MemoEdit( xData , nTop+1, nLeft+1, nBottom-1, nRight-1, .T. )
      SetColor( cMemoColor )
      RestScreen( nTop, nLeft, nBottom, nRight, cMemoScreen )
#ifdef HB_EXT_INKEY
      lSave := ( LastKey() == K_CTRL_W .or. LastKey() == K_ALT_W )
#else
      lSave := ( LastKey() == K_CTRL_W )
#endif
      oBrowse:RefreshAll()
   ELSE
      xData := eval(oCol:block())
      oGet  := getnew(Row(), Col(), { |_1| iif( PCount() == 0, xData, xData := _1 ) }, "mGetVar", Nil, oBrowse:colorspec )
      lSave := ReadModal({oGet})
   ENDIF

   lSuccess := .F.

   if lSave
      if eof() 
         dbappend()
      endif
      if !lMemo
         eval(oCol:block(), xData)
      else
         FieldPut( FieldPos(cMemoField), xData )
      endif
      if ( !eof() .AND. !Empty(cForExp := ordfor(indexord())) .AND. ;
            !&cForExp )
         dbgotop()
      endif
      if ( !eof() .AND. !Empty(nIndexKey) .AND. xKeyValue != &nIndexKey )
         lSuccess := .T.
      endif
   endif

   if ( lSuccess )
      freshorder(oBrowse)
      nExitState := 0
   else
      oBrowse:refreshcurrent()
      nExitState := ExitKey( eof(), oBrowse )
   endif

   SetCursor(nCursor)
   Set(_SET_SCOREBOARD, lScore)
   Set(_SET_EXIT, lExit)
   SetKey(K_INS, bIns)

return nExitState

*-------------------------------------
static function EXITKEY( lExit, oBrw )

   local nReturn := LastKey()

   do case
   case nReturn == K_PGDN
      if ( lExit )
         nReturn := 0
      else
         nReturn := K_DOWN
      endif
   case nReturn == K_PGUP
      if ( lExit )
         nReturn := 0
      else
         nReturn := K_UP
      endif
   case nReturn == K_ENTER .OR. ( nReturn >= K_SPACE .AND. nReturn <= 255 )
      // xHarbour extension: After reach last column the browse jump to the
      // first column of the next row.
      //
      if oBrw:ColPos == oBrw:ColCount
         if !IsBottom() .and. !IsEmpty() 
            oBrw:PanHome()
            nReturn := K_DOWN
         else
            nReturn := 0
         endif
      else
         nReturn := K_RIGHT
      endif
   case nReturn != K_UP .AND. nReturn != K_DOWN
      nReturn := 0
   endcase

return nReturn

*------------------------------------
static function FRESHORDER( oBrowse )

   local nRec := RecNo()
   oBrowse:refreshall()
   do while ( !oBrowse:stabilize() )
   enddo
   if eof()
      do while ( RecNo() != nRec .AND. !BOF() )
         oBrowse:up()
         do while ( !oBrowse:stabilize() )
         enddo
      enddo
   endif

return Nil

*--------------------------------------
static function SKIPPED( nSkip, oBrw )

local nSkipped := 0
local lAppend := oBrw:Cargo

   if !IsEmpty()

       if ( nSkip == 0 )
         dbskip( 0 )

      elseif ( nSkip > 0 .AND. !eof() )

         do while ( nSkipped < nSkip )

            dbskip()

            if ( eof() )

               if ( lAppend )
                  nSkipped ++
                  oBrw:Cargo := .f.
               else
                  dbskip( -1 )
               endif

               exit

            endif

            nSkipped ++

         enddo

      elseif ( nSkip < 0 )

         do while ( nSkipped > nSkip )

            dbskip( -1 )

            if ( BOF() )
               exit
            endif

            nSkipped --

         enddo

      endif
   endif

return nSkipped

*-------------------------
STATIC FUNCTION IsBottom()
RETURN ( if( IndexOrd() == 0 , Recno() == LastRec() , OrdKeyNo() == OrdKeyCount() ) )

*------------------------
STATIC FUNCTION IsEmpty()
RETURN ( if( IndexOrd() == 0 , LastRec() == 0 , OrdKeyCount() == 0 ) )

*-------------------------
STATIC FUNCTION CursorOn()
RETURN SetCursor( if( readinsert(), SC_INSERT, SC_NORMAL ) )

*--------------------------
STATIC FUNCTION CursorOff()
RETURN SetCursor( SC_NONE )
