/*
 * $Id$
 */

/* Sample on how to handle Mouse Click in a tbrowse session.
 * I don't think the method used herein is the smartest one, especially
 * regarding the horizontal movement, thus revisions to this sample
 * are welcome :-).
 *
 * Budyanto Dj.
 */

#include "inkey.ch"

* Browse area
#DEFINE _TB_TOP     1
#DEFINE _TB_LEFT    1
#DEFINE _TB_BOTTOM 20
#DEFINE _TB_RIGHT  78

function Main()
   local oBrowse := TBrowseNew( _TB_TOP, _TB_LEFT, _TB_BOTTOM, _TB_RIGHT )
   local i
   local nKey
   local lEnd
   local nCursor
   local cColor
   local nRow, nCol
   local dbinfo := {}, bBlock

   USE TEST ALIAS TEST SHARED NEW

   * build array of fields
   dbinfo:= DbStruct()

   oBrowse:colorSpec     = "W+/B, N/BG"
   oBrowse:ColSep        = "|"
   oBrowse:HeadSep        = "+-"
   oBrowse:FootSep        = "+-"
   oBrowse:GoTopBlock    = { || dbGoTop() }
   oBrowse:GoBottomBlock = { || dbGoBottom() }
   oBrowse:SkipBlock     = { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

   for i := 1 to len( dbinfo )
      bBlock := FieldBlock(i)
      oBrowse:AddColumn( TBColumnNew( dbinfo[ i,1 ], bBlock ) )
   next

   i := alert("How many columns do you want to freeze?",;
              {"0", "1", "2"})

   oBrowse:Freeze = iif(i==0,0,i-1)

   nCursor := SetCursor( 0 )
   cColor := SetColor( "W+/B" )

   nRow := Row()
   nCol := Col()

   @ _TB_TOP-1,_TB_LEFT-1,_TB_BOTTOM+1,_TB_RIGHT+1 BOX "/-\|/-\| "

   * mask events you want to handle:
   SET(_SET_EVENTMASK, INKEY_KEYBOARD + INKEY_LDOWN + INKEY_LUP)

   lEnd := .f.
   do while !lEnd
      dispbegin()
      oBrowse:ForceStable()
      dispend()

      nKey = InKey( 0 )

      do case
         case nKey == K_ESC
              SetPos( _TB_BOTTOM+2 , 0 )
              lEnd = .t.

         case nKey == K_DOWN
              oBrowse:Down()

         case nKey == K_UP
              oBrowse:Up()

         case nKey == K_LEFT
              oBrowse:Left()

         case nKey == K_RIGHT
              oBrowse:Right()

         case nKey = K_PGDN
              oBrowse:pageDown()

         case nKey = K_PGUP
              oBrowse:pageUp()

         case nKey = K_CTRL_PGUP
              oBrowse:goTop()

         case nKey = K_CTRL_PGDN
              oBrowse:goBottom()

         case nKey = K_HOME
              oBrowse:home()

         case nKey = K_END
              oBrowse:end()

         case nKey = K_CTRL_LEFT
              oBrowse:panLeft()

         case nKey = K_CTRL_RIGHT
              oBrowse:panRight()

         case nKey = K_CTRL_HOME
              oBrowse:panHome()

         case nKey = K_CTRL_END
              oBrowse:panEnd()

         * detect whatever mouse events you want here:
         case nKey == K_LBUTTONUP .or. nKey == K_LBUTTONDOWN
              * then pass it to the handler:
              HandleMouse(oBrowse, nKey, mrow(), mcol())

         * Clipper compatibility note:
         * The following vertical movements works in Clipper,
         * but not in xHarbour:
         //case chr(nKey) >= "1" .and. chr(nKey) <= "5"
         //     oBrowse:Dehilite()
         //     oBrowse:RowPos := val(chr(nKey))
         //     oBrowse:hilite()
      endcase
   enddo

   DBCloseArea()

   DevPos( nRow, nCol )
   SetColor( cColor )
   SetCursor( nCursor )

return nil //main

****************************************************
*
* GENERAL TBROWSE SUPPORTERS
*
****************************************************

STATIC FUNCTION FieldBlock(i)
RETURN  { || fieldget(i) }

STATIC FUNCTION DbSkipBlock(n, oBrowse)
LOCAL nSkipped := 0

   if n == 0
      DBSkip( 0 )
   elseif n > 0
      do while nSkipped != n .and. TBNext(oBrowse)
         nSkipped++
      enddo
   else
      do while nSkipped != n .and. TBPrev(oBrowse)
         nSkipped--
      enddo
   endif

RETURN  nSkipped

STATIC FUNCTION TBNext( oBrowse )
LOCAL nSaveRecNum := recno()
LOCAL lMoved := .T.

   if Eof()
      lMoved := .F.
   else
      DBSkip( 1 )
      if Eof()
         lMoved := .F.
         DBGoTo( nSaveRecNum )
      endif
   endif

RETURN lMoved

STATIC FUNCTION TBPrev( oBrowse )
LOCAL nSaveRecNum := Recno()
LOCAL lMoved := .T.

   DBSkip(-1)
   if Bof()
      DBGoTo( nSaveRecNum )
      lMoved := .F.
   endif

RETURN lMoved

****************************************************
*
* SAMPLE MOUSE EVENT HANDLER
*
****************************************************

procedure HandleMouse(oBrowse, nKey, mRow, mCol)
* nKey: mouse event code (from inkey())
* mRow,mCol: mouse cursor coordinates
*
* For simplicity, number of Heading and Footing lines are assigned here.
* These constants "should" be checked by caller and passed here.
local nHeadLines := 2 //including separator, ASSUMED!
local nFootLines := 1 //including separator, ASSUMED!

local i, mrowpos, mcolpos, lFrozen, mleftbound, mrightbound, lDone,;
      msepwidth, mcolwidth, numrow, numcol
local mMinPos, ndirection

   * we will do the action on left button press event only
   if nKey<>K_LBUTTONDOWN
      return
   endif

   * this simplest method is unfortunately not working
   * oBrowse:mGotoYX(mROw,mCOl)

   * is mrow and mcol in bound?
   if ! ( mrow >= oBrowse:nTop+nHeadlines .and.;
          mrow <= oBrowse:nBottom-nFootlines .and.;
          mcol >= oBrowse:nLeft .and.;
          mcol <= oBrowse:nRight)
      * event occurs outside the browse data area
      return
   endif

   DISPBEGIN()

   * moving vertically, simple
   mrowpos := mrow - (oBrowse:nTop+nHeadlines) + 1
   if oBrowse:RowPos > mrowpos
      numrow := oBrowse:RowPos - mrowpos
      for i := 1 to numrow
         oBrowse:up()
         oBrowse:forcestable()
      next
   elseif oBrowse:RowPos < mrowpos
      numrow :=  mrowpos - oBrowse:RowPos
      for i := 1 to numrow
         oBrowse:down()
         oBrowse:forcestable()
      next
   endif

   * moving horizontally
   * this is a bit more complex than the vertical one
   * ASSUME: col() is at the leftmost of current col.
   *         (This assumption is not always valid, see eg. MARRIED field)
   lFrozen := oBrowse:Freeze > 0
   mMinPos := iif(lFrozen,1,oBrowse:LeftVisible)

   lDone := .f.
   mcolpos := oBrowse:colpos
   msepwidth := NextSepWidth(oBrowse, mcolpos)
   mcolwidth := oBrowse:ColWidth(mcolpos)

   if mcol < col()
      nDirection := -1   //move to left
   elseif mcol > col() + mcolwidth + msepwidth - 1
      nDirection := 1    //move to right
   else
      * click on current column, no move req'd
      lDone := .t.
   endif

   do while !lDone
      mcolpos := oBrowse:colpos
      msepwidth := NextSepWidth(oBrowse, mcolpos)
      mcolwidth := oBrowse:ColWidth(mcolpos)

      /*
      tracelog("loop..." + hb_osnewline() +;
               "mcol = " + alltrim(str(mcol)) + hb_osnewline() +;
               "col() = " + alltrim(str(col())) + hb_osnewline() +;
               "mcolwidth = " + alltrim(str(mcolwidth)) + hb_osnewline() +;
               "msepwidth = " + alltrim(str(msepwidth)) + hb_osnewline() +;
               "")
      */

      if mcol < col() .and. nDirection < 0 //to left
         * a guess, move left one column
         mcolpos := iif(lFrozen .and. mcolpos==oBrowse:LeftVisible, ;
                        oBrowse:Freeze, mcolpos-1)
         if mcolpos < mMinPos
            lDone := .t.
         else
            numcol := oBrowse:ColPos - mcolpos
            for i := 1 to numcol
               oBrowse:left()
               oBrowse:forcestable()
            next
         endif
      elseif mcol > col() + mcolwidth + msepwidth - 1 .and.;
             nDirection > 0 //to right
         * a guess, move right one col
         mcolpos := iif(lFrozen .and. mcolpos==oBrowse:Freeze, ;
                        oBrowse:LeftVisible, mcolpos+1)
         if mcolpos > oBrowse:RightVisible
            lDone := .t.
         else
            //tracelog("move right...")

            numcol := mcolpos - oBrowse:ColPos
            for i := 1 to numcol
               oBrowse:right()
               oBrowse:forcestable()
            next
         endif
      else
         * we have arrived at the designated column
         lDone := .t.
      endif
   enddo

   DISPEND()

return //HandleMouse()

static function NextSepWidth(oBrowse, mcolpos)
* returns separator width to the right of column mcolpos
local lFrozen := oBrowse:Freeze > 0
local mrightcolpos, oCol, nHeadWidth, nColWidth, nFootWidth
   if mcolpos >= oBrowse:RightVisible
      return 0
   elseif lFrozen .and. mcolpos == oBrowse:Freeze
      mrightcolpos := oBrowse:LeftVisible
   else
      mrightcolpos := mcolpos+1
   endif

   * an inserted move: if mrightcolpos is a zero width column,
   * find first column to the right of it that is not zero width
   do while mrightcolpos < oBrowse:RightVisible .and.;
            empty(oBrowse:ColWidth(mrightcolpos))
      mrightcolpos++
   enddo
   if mrightcolpos >= oBrowse:RightVisible
      return 0
   endif

   oCol := oBrowse:GetColumn(mrightcolpos)

   //nHeadWidth := iif(valtype(oCol:HeadSep)=="C", len(oCol:HeadSep),;
   //                  iif(valtype(oBrowse:HeadSep)=="C", len(oBrowse:HeadSep), 0))

   //nFootWidth := iif(valtype(oCol:FootSep)=="C", len(oCol:FootSep),;
   //                  iif(valtype(oBrowse:FootSep)=="C", len(oBrowse:FootSep), 0))

   nColWidth := iif(valtype(oCol:ColSep)=="C", len(oCol:ColSep),;
                     iif(valtype(oBrowse:ColSep)=="C", len(oBrowse:ColSep), 0))

//return max(nHeadWidth, max(nColWidth, nFootWidth))
return nColWidth
