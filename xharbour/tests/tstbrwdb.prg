/*
 * $Id: testbrw.prg,v 1.2 2003/11/12 12:58:45 andijahja Exp $
 */

// Harbour Class TBrowse and TBColumn sample

#include "inkey.ch"

function Main()

   local linit   := .F., i
   local oBrowse
   local n
   local nKey
   local lEnd    := .f.
   local nCursor := SetCursor( 0 )
   local cColor
   local nRow, nCol

   CLEAR SCREEN
   USE test

   oBrowse := TBrowseDB( 1, 2, MaxRow()-1, MaxCol()-1 )

   oBrowse:colorSpec     = "W+/B, N/BG"
   oBrowse:ColSep        = "Ё"
   oBrowse:HeadSep        = "ям"
   oBrowse:FootSep        = "ом"

   FOR n := 1 TO FCount()
       oBrowse:AddColumn( TBColumnNew( FieldName( n ), FieldBlock( FieldName( n ) ) ) )
   NEXT

   oBrowse:Configure()
    oBrowse:ForceStable()

   While !lEnd
      dispbegin()
      oBrowse:ForceStable()
      dispend()
      nKey = InKey( 0 )

      do case
         case nKey == K_ESC
              SetPos( 17, 0 )
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

         case nKey = K_TAB
              nTmpRow := ROW()
              nTmpCol := COL()
              @ 0, 0 SAY TIME()
              DevPos( nTmpRow, nTmpCol )

      endcase

   end

   SetCursor( nCursor )

return nil
