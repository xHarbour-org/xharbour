/*
 * $Id$
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
#include "dbinfo.ch"
#include "common.ch"

static bBlock := { || setcursor( iif( readinsert(!readinsert()), SC_NORMAL, SC_INSERT )) }
static s_ldbEmpty
static s_ldbBottom
static s_ldbAppend

function Browse( nTop, nLeft, nBottom, nRight )

   local oBrw
   local cOldScreen
   local n, nOldCursor
   local nKey := 0
   local lExit := .f.
   Local lRefresh
   Local bAction
   Local lKeyPressed
   Local aRect, cField, lShared
   Local lReadOnly

   if ! Used()
      return .f.
   end

   lShared := dbInfo( DBI_SHARED )
   lReadOnly := dbInfo( DBI_ISREADONLY )

   s_ldbBottom := dbBottom()
   s_ldbEmpty  := dbEmpty()
   s_ldbAppend  := .f.

   if PCount() < 4
      nTop    := 1
      nLeft   := 0
      nBottom := MaxRow()
      nRight  := MaxCol()
   endif

   nOldCursor := CursorOff()
   cOldScreen := SaveScreen( nTop, nLeft, nBottom, nRight )

   dispbegin()

   DispBox( nTop, nLeft, nBottom, nRight )

   DispOutAt( nTop+3, nLeft, chr(198))
   DispOutAt( nTop+3, nRight, chr(181))
   DispOutAt( nTop+1, nLeft+1, Space( nRight - nLeft - 1 ) )

   lRefresh := .f.

   oBrw := TBrowseDB( nTop + 2, nLeft + 1, nBottom - 1, nRight - 1 )
   oBrw:HeadSep := " " + Chr( 205 )

   oBrw:skipblock := { |_1| Skipped(_1, s_ldbAppend ) }

   for n := 1 to FCount()
      cField := FieldName( n )
      if HB_ISMEMO( fieldget( n ) )
         oBrw:AddColumn( TBColumnNew( cField, &( "{|| ' <Memo> '}" ) ) )
      else
         oBrw:AddColumn( TBColumnNew( cField, FieldBlock( cField ) ) )
      endif
   next

   if eof()
      dbgotop()
   endif

   oBrw:ForceStable()

   dispend()

   if s_ldbEmpty
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

      s_ldbBottom := dbBottom()
      s_ldbEmpty  := dbEmpty()

      switch nKey

         case K_DOWN
            if !eof()
               s_ldbAppend := ( s_ldbBottom .or. s_ldbEmpty )
               IF s_ldbAppend
                  IF lReadOnly
                     Tone( 261.70, 0.1 )
                  ELSE
                     oBrw:Down()
                  ENDIF
               ELSE
                  oBrw:Down()
               ENDIF
            endif
            exit

         case K_UP

            if eof()
               oBrw:RefreshCurrent()
               lRefresh := .t.
            endif
            oBrw:Up()
            exit

         case K_PGUP

            if eof()
               oBrw:RefreshCurrent()
               oBrw:Up()
               lRefresh := .t.
            else
               oBrw:PageUp()
            endif
            exit

         case K_PGDN

            if !eof()
               s_ldbAppend := ( s_ldbBottom .or. s_ldbEmpty )

               IF s_ldbAppend
                  IF lReadOnly
                     Tone( 261.70, 0.1 )
                  ELSE
                     oBrw:Down()
                  ENDIF
               ELSE
                  oBrw:PageDown()
               ENDIF
            endif
            exit

         case K_CTRL_PGUP

            if eof()
               oBrw:RefreshCurrent()
               oBrw:Up()
               lRefresh := .t.
            else
               oBrw:GoTop()
            endif
            exit

         case K_CTRL_PGDN

            if eof()
               lRefresh := .t.
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
            IF lReadOnly
               Tone( 261.70, 0.1 )
               EXIT
            ENDIF

            if ( ! eof() ) .and. if( lShared, RLock(), .t. )
               if ( Deleted() )
                  dbRecall()
               else
                  dbDelete()
               endif
               if(lShared, dbUnlock(), .t. )
               lRefresh := .t.
            endif
            exit

        case K_ENTER
            IF lReadOnly
               Tone( 261.70, 0.1 )
               EXIT
            ENDIF

            nKey := doget( oBrw, lShared )
            lRefresh    := .t.
            lKeyPressed := (nKey != 0)
            exit

        default
            IF lReadOnly
               Tone( 261.70, 0.1 )
               EXIT
            ENDIF
            keyboard Chr(K_ENTER) + Chr(nKey)
            exit
      end

      if lRefresh
         s_ldbAppend := .f.
         freshorder(oBrw, lRefresh)
         lRefresh := .f.
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

   dispbegin()

   DispOutAt( nTop, nRight - 27, "Record " )

   if s_ldbEmpty
      DispOutAt( nTop, nRight - 20 , "<none>               " )
   elseif eof()
      DispOutAt( nTop, nRight - 40 , "         " )
      DispOutAt( nTop, nRight - 20 , "                <new>" )
   else
      DispOutAt( nTop, nRight - 40 , iif( Deleted(), "<Deleted>", "         " ) )
      DispOutAt( nTop, nRight - 20 , Padr( LTrim( Str( RecNo() ) ) + "/" +;
                                    Ltrim( Str( LastRec() ) ), 16 ) +;
                              iif( oBrw:hitTop, "<bof>", "     " )  )
   endif

   dispend()

return

*----------------------------------------
static function DOGET( oBrowse, lShared )

   local bIns, lScore, lExit, oCol, oGet, nExitState, nIndexKey, ;
      xKeyValue, lSuccess, nCursor, xData, cForExp, lSave, ;
      cMemoColor, nTop, nLeft, nBottom, nRight, cMemoScreen, ;
      cMemoField, lMemo, lOK, cPict

   oBrowse:hittop := .F.
   oBrowse:hitbottom := .F.

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

   lMemo := HB_ISMEMO( FieldGet( FieldPos( oCol:Heading ) ) )

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
      DispBegin()
      @ nTop, nLeft CLEAR TO nBottom, nRight
      DispBox( nTop, nLeft, nBottom, nRight )
      DispOutAt( nTop, nLeft+1 , Padc( "[ "+oCol:heading+" ]" , nRight - nLeft - 2, chr(196) ) )
      DispEnd()
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
      if HB_ISSTRING( xData )
         if Len(xData) > ( oBrowse:nRight - oBrowse:nLeft - 2 )
            cPict := "@S"+ Ltrim(Str( oBrowse:nRight - oBrowse:nLeft - 2 ))
         else
            cPict := NIL
         endif
      else
         cPict := NIL
      endif
      oGet  := getnew(Row(), Col(), { |_1| iif( PCount() == 0, xData, xData := _1 ) }, "mGetVar", cPict, oBrowse:colorspec )
      lSave := ReadModal({oGet})
   ENDIF

   lSuccess := .F.

   if lSave

      if eof()
         dbAppend()
         lOK := !NetErr()
         s_ldbAppend := .f.
      else
         lOK := if( lShared, RLock(), .t. )
      endif

      if lOK

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

         if( lShared, dbUnlock(), .t. )

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
      // first column of the next row, if is not the last.
      //
      if oBrw:ColPos == oBrw:ColCount
         if !s_ldbBottom .and. !s_ldbEmpty
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

*-----------------------------------------------*
static function FRESHORDER( oBrowse, lRefresh )

local nRec := RecNo()

default lRefresh to .f.

   if lRefresh
      oBrowse:RefreshAll()
   endif

   if ! oBrowse:Stable
      oBrowse:ForceStable()
   endif

   if eof()
      while ( RecNo() != nRec .AND. !BOF() )
         oBrowse:up()
         oBrowse:forcestable()
      enddo
   endif

return Nil

*----------------------------------------
static function SKIPPED( nSkip, lAppend )

local nSkipped := 0

 if !( s_ldbEmpty )

   if ( nSkip == 0 )
      dbskip( 0 )

   elseif ( nSkip > 0 .AND. !eof() )

      do while ( nSkipped < nSkip )

         dbskip()

         if ( eof() )

            if ( lAppend )
               nSkipped ++
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
STATIC FUNCTION dbBottom()
*-------------------------
Local lBot, nRec

// 2007/MAY/15 - E.F. - Don't use OrdKey..() functions because decrease performance.
//RETURN ( if( IndexOrd() == 0 , Recno() == LastRec() , OrdKeyNo() == OrdKeyCount() ) )

 lBot := eof()

 if ! lBot
    nRec := recno()
    dbskip()
    lBot := eof()
    dbgoto(nRec)
 endif

RETURN ( lBot )

*------------------------
STATIC FUNCTION dbEmpty()
// 2007/MAY/15 - E.F. - Don't use OrdKey..() functions because decrease performance.
//RETURN ( if( IndexOrd() == 0 , LastRec() == 0 , OrdKeyCount() == 0 ) )
RETURN ( if( IndexOrd() == 0 , LastRec() == 0 , ( bof() .and. eof() ) ) )

*-------------------------
STATIC FUNCTION CursorOn()
RETURN SetCursor( if( readinsert(), SC_INSERT, SC_NORMAL ) )

*--------------------------
STATIC FUNCTION CursorOff()
RETURN SetCursor( SC_NONE )
