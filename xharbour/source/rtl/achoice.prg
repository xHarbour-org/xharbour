/*
 * $Id: achoice.prg,v 1.45 2008/10/18 17:08:54 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 * ACHOICE() function
 *
 * Copyright 2004  Vicente Guerra <vicente@guerra.com.mx>
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
 * As a special exception, the xHarbour Project gives permission for
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
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * xHarbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "achoice.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "color.ch"
#include "hbclass.ch"
#include "common.ch"

FUNCTION AChoice( nTop,;
                  nLeft,;
                  nBottom,;
                  nRight,;
                  acItems,;
                  uSelect,;
                  uUserFunc,;
                  nOption,;
                  nFirstRow )
LOCAL oAChoice
LOCAL xItem

/* Clipper compliant. */
DEFAULT nTop    TO 0
DEFAULT nLeft   TO 0
DEFAULT nBottom TO 0
DEFAULT nRight  TO 0

  // Parameters check.

  If !Hb_IsNumeric( nTop ) .or.;
     !Hb_IsNumeric( nLeft ) .or.;
     !Hb_IsNumeric( nBottom ) .or.;
     !Hb_IsNumeric( nRight )

     Throw( ErrorNew( "BASE", 0, 1127, Procname()+" <nTop,nLeft,nBottom,nRight>", "Argument type error" ) )

  Else

     If nTop > nBottom
        Throw( ErrorNew( "BASE", 0, 1127, Procname(), "Argument error: <nTop> greater than <nBottom>" ) )
     Endif

     If nLeft > nRight
        Throw( ErrorNew( "BASE", 0, 1127, Procname(), "Argument error: <nLeft> greater than <nRight>"  ) )
     Endif

  Endif


  If Hb_IsNil(acItems) .OR. !Hb_IsArray( acItems ) .OR. Empty( acItems ) // This is Clipper compatible
    RETURN( 0 )
   Endif

  IF !Hb_IsNil( uSelect )
     If !Hb_IsArray( uSelect ) .AND. !Hb_IsLogical( uSelect )
        Throw( ErrorNew( "BASE", 0, 1127,  Procname()+" <alSelectableItems | lSelectableItems>", "Argument type error: <"+valtype(uSelect)+">" ) )
     Elseif Hb_IsArray( uSelect ) .and. !Empty( uSelect )
        For Each xItem In uSelect
            If !Hb_IsLogical( xItem ) .and. !Hb_IsString( xItem )
               Throw( ErrorNew( "BASE", 0, 1127, Procname(), "Argument error: <alSelectableItems | lSelectableItems> should contain logical or string values" ) )
            Endif
        Next
     Endif
  Endif

  IF !Hb_IsNil( uUserFunc ) .AND. !Hb_IsString( uUserFunc )
     Throw( ErrorNew( "BASE", 0, 1127, Procname()+" <cUserFunc>", "Argument type error: <"+valtype(uUserFunc)+">" ) )
  ELSE
      If Hb_IsString( uUserFunc ) .AND. Empty( uUserFunc )
         uUserFunc := NIL
      Endif
  ENDIF

  If !Hb_IsNil( nOption ) .AND.  !Hb_IsNumeric( nOption )
     Throw( ErrorNew( "BASE", 0, 1127, Procname()+" <nInitialItem>", "Argument type error: <"+valtype(nOption)+">" ) )
  Endif

  If !Hb_IsNil( nFirstRow ) .AND.  !Hb_IsNumeric( nFirstRow )
     Throw( ErrorNew( "BASE", 0, 1127, Procname()+" <nWindowRow>", "Argument type error: <"+valtype(nFirstRow)+">" ) )
  Endif

  oAChoice := TAChoice():New( nTop, nLeft, nBottom, nRight, acItems, uSelect, uUserFunc, nOption, nFirstRow )
  oAChoice:cProcName := ProcName( 1 )
  oAChoice:nProcLine := ProcLine( 1 )

RETURN IF( oAChoice:nItems == 0, 0, oAChoice:Loop( AC_CONT ) )

// I had made AChoice() as a class for avoid pass all memvars to each
// static function. Anyway, it can be easyly converted to a "GET subclass".

#define AC_NO_USER_FUNCTION -1         // Don't process user's function
#define AC_MAXVALUE         AC_REDRAW  // Highest user's return value

CLASS TAChoice

   VAR    nTop, nLeft, nBottom, nRight   // Screen coords.
   VAR    acItems                        // Array of items
   VAR    uSelect                        // Determines if any item is selectable
   VAR    nItems                         // Count of valid items in array
   VAR    nOption                        // Selected item
   VAR    nFirstRow                      // Item in first screen's row
   VAR    lUserFunc                      // If there's an user's function
   VAR    uUserFunc                      // User's function
   VAR    nArraySize                     // Last array's size

   // NOTE: Clipper does in this way (it doesn't validates each time).
   //       It's for a simple way to add "this compatibility".
   VAR    alSelect                       // Pre-verified items' selectable flags

   VAR    cProcName                      // Calling procedure name
   VAR    nProcLine                      // Calling procedure line

   VAR    nSize                          // Rows to skip, used to move cursor.

   METHOD New    CONSTRUCTOR             // Initializes TAChoice
   METHOD Loop                           // Main loop

   METHOD ValidateArray                  // Verifies if the array have selectable elements, and if the array has changed
   METHOD MoveCursor                     // Changes selected option, and validates if it's selectable
   METHOD DrawRows                       // "Paints" items on screen
   METHOD HitTest                        // Checks if the mouse is over the screen's area
   METHOD Refresh()                      // Refresh achoice content.

ENDCLASS

// #define IsAvailableItem( nItem )     ( ::alSelect[ ( nItem ) ] )
#define IsAvailableItem( nItem )     ( IsItemSelectable( ( nItem ), ::nItems, ::uSelect, ::acItems ) )

METHOD New( nTop, nLeft, nBottom, nRight, acItems, uSelect, uUserFunc, nOption, nFirstRow ) CLASS TAChoice
Local aItems

   ::nTop    := IF( HB_ISNUMERIC( nTop ),    nTop,    0 )
   ::nLeft   := IF( HB_ISNUMERIC( nLeft ),   nLeft,   0 )
   ::nBottom := IF( HB_ISNUMERIC( nBottom ), nBottom, MaxRow() )
   ::nRight  := IF( HB_ISNUMERIC( nRight ),  nRight,  MaxCol() )

   ::nTop    := Max( Min( ::nTop,    MaxRow() ) , 0 )
   ::nLeft   := Max( Min( ::nLeft,   MaxCol() ) , 0 )
   ::nBottom := Max( Min( ::nBottom, MaxRow() ) , ::nTop )
   ::nRight  := Max( Min( ::nRight,  MaxCol() ) , ::nLeft )

   ::nSize   := ::nBottom - ::nTop

   if hb_IsArray( acItems[1] )
      aItems := {}
      AEval( acItems, {|a| aadd( aItems, hb_Valtostr(a[1])) } )
      ::acItems := aItems
   else
      ::acItems := acItems
   endif

   ::uSelect := uSelect
   ::nItems  := 0

   ::Refresh(.f.)

   ::nOption   := IF( HB_ISNUMERIC( nOption ), nOption, 1 )
   ::nOption   := Min( Max( ::nOption, 1 ), ::nItems )
   ::nFirstRow := IF( HB_ISNUMERIC( nFirstRow ), nFirstRow, 0 )
   ::nFirstRow := Max( Min( ::nFirstRow, ::nBottom - ::nTop ), 0 )   // Inside range
   ::nFirstRow := Max( ::nOption - ::nFirstRow, 1 )                  // Initial row

   ::uUserFunc := uUserFunc
   ::lUserFunc := ( VALTYPE( ::uUserFunc ) IN "CBM" .AND. ! EMPTY( ::uUserFunc ) )

   IF ::nItems != 0
      ::nArraySize := 0
      ::ValidateArray()
      ::DrawRows( 0, ::nSize, .F. )
   ENDIF

RETURN Self

METHOD Refresh(lRedraw) CLASS TAchoice

 ::nItems := 0

 default lRedraw to .t.

 if HB_ISARRAY( ::acItems )

    while ::nItems < LEN( ::acItems ) .AND. HB_ISSTRING( ::acItems[ ::nItems + 1 ] ) .AND. ! ::acItems[ ::nItems + 1 ] == ""
       ::nItems++
    enddo

    if lRedraw .and. ::nItems > 0
       ::DrawRows( 0, ::nSize, .F. )
    endif

 endif

RETURN Self

METHOD Loop( nMode ) CLASS TAChoice

LOCAL nRet, nUserMode, lNoItems
LOCAL nKey, bAction, nAux
LOCAL nSaveCsr := SetCursor( SC_NONE )
LOCAL nGotoItem
LOCAL nPage := ::nSize + 1

   lNoItems := ! ::ValidateArray()

   nRet := 0
   nUserMode := AC_NOITEM     // Something different to AC_IDLE

   // Main loop

   DO WHILE nMode > AC_ABORT

      // Refresh?
      IF nMode == AC_REDRAW
         ::DrawRows( 0, ::nSize, .F. )
      ENDIF

      /* 2008/JAN/17 - E.F. Force to process pending key, if any */
      IF nMode == AC_SELECT
         if NextKey() == 0 .or. nUserMode == AC_NO_USER_FUNCTION 
            EXIT
         else
            ::DrawRows( ::nOption - ::nFirstRow, ::nOption - ::nFirstRow, .F. )
         endif
      ENDIF

      // What will do?
      nKey := 0

      IF lNoItems
         // There aren't selectable items
         nUserMode := AC_NOITEM
         nMode := AC_ABORT
      ELSEIF NextKey() != 0
         // There are pending keys
         nKey := INKEY()
         nUserMode := AC_EXCEPT
         nMode := AC_GOTO
      ELSEIF nUserMode == AC_IDLE
         // AC_IDLE state was processed by user's function. Wait for a key
         ::DrawRows( ::nOption - ::nFirstRow, ::nOption - ::nFirstRow, .T. )
         nKey := INKEY( 0 )
         ::DrawRows( ::nOption - ::nFirstRow, ::nOption - ::nFirstRow, .F. )
         nUserMode := AC_EXCEPT
         nMode := AC_GOTO
      ELSE
         // Send AC_IDLE to user's function
         nUserMode := AC_IDLE
         nMode := AC_CONT
         LOOP
      ENDIF

      IF ( bAction := SetKey( nKey ) ) != NIL
         Eval( bAction, ::cProcName, ::nProcLine, "" )

         // Key was processed
         nUserMode := AC_NO_USER_FUNCTION

         nMode := AC_GOTO
         nKey := 0

         IF ! ::ValidateArray()
            nUserMode := AC_NOITEM
            nMode := AC_ABORT
         ENDIF

      ENDIF

      SWITCH nKey
         CASE K_UP             // Moves up
            nAux := ::nOption
            ::MoveCursor( -1, -1, 0 )
            nUserMode := IF( nAux > ::nOption, AC_IDLE, AC_HITTOP )
            EXIT

         CASE K_DOWN           // Moves down
            nAux := ::nOption
            ::MoveCursor( 1, 1, 0 )
            nUserMode := IF( nAux < ::nOption, AC_IDLE, AC_HITBOTTOM )
            EXIT

         CASE K_CTRL_HOME      // Top of the window
            ::MoveCursor( - ( ::nOption - ::nFirstRow ), 1, 0 )
            nUserMode := AC_IDLE
            EXIT

         CASE K_CTRL_END       // Bottom of the window
            ::MoveCursor( ::nSize - ( ::nOption - ::nFirstRow ), -1, 0 )
            nUserMode := AC_IDLE
            EXIT

         CASE K_MWFORWARD
         CASE K_PGUP           // Previous screen
            nAux := ::nOption
            // 2006/NOV/10 - E.F. Adjusted to which item skip backward.
            nGotoItem := nPage
            if (::nFirstRow - nPage)  < 1
               nGotoItem := nPage - Abs( ::nFirstRow - nPage ) - 1
               if nGotoItem <= 0
                  nGotoItem := nPage
               endif
            endif
            ::MoveCursor( - MAX( nGotoItem, 1 ), -1, - MAX( nGotoItem, 1 ) )
            nUserMode := IF( nAux > ::nOption, AC_IDLE, AC_HITTOP )
            EXIT

         CASE K_MWBACKWARD
         CASE K_PGDN           // Next screen
            nAux := ::nOption
            // 2006/NOV/10 - E.F. Adjusted to which item skip forward.
            nGotoItem := nPage
            if (::nFirstRow + nPage + nPage ) > ::nItems
               nGotoItem := nPage - ( (::nFirstRow + nPage + nPage) - ::nItems ) + 1
               if nGotoItem <= 0
                  nGotoItem := nPage
               endif
            endif
            ::MoveCursor( MAX( nGotoItem, 1 ), 1, MAX( nGotoItem, 1 ) )
            nUserMode := IF( nAux < ::nOption, AC_IDLE, AC_HITBOTTOM )
            EXIT

         CASE K_HOME
            IF ::lUserFunc
               nUserMode := AC_EXCEPT
            ELSE
               ::MoveCursor(1, -1, 1)
               nUserMode := AC_NO_USER_FUNCTION
            ENDIF
            EXIT

         CASE K_CTRL_PGUP      // First item
            ::MoveCursor( - ( ::nOption - 1 ), -1, ::nFirstRow - 1 )
            nUserMode := AC_IDLE
            EXIT

         CASE K_END
            IF ::lUserFunc
               nUserMode := AC_EXCEPT
            ELSE
               ::MoveCursor(::nItems - ::nOption, 1, ::nItems - ::nOption)
               nUserMode := AC_NO_USER_FUNCTION
            ENDIF
            EXIT

         CASE K_CTRL_PGDN      // Last item
            ::MoveCursor( ::nItems - ::nOption, 1, Max( ::nItems - ::nSize - ::nFirstRow, 0 ) )
            nUserMode := AC_IDLE
            EXIT

         CASE K_ENTER          // Select item
            IF ! ::lUserFunc
               nUserMode := AC_NO_USER_FUNCTION
               nMode := AC_SELECT
            ELSE
               nUserMode := AC_EXCEPT
            ENDIF
            EXIT

         CASE K_LEFT
         CASE K_RIGHT
            IF ! ::lUserFunc
               nUserMode := AC_NO_USER_FUNCTION
               nMode := AC_ABORT
            ELSE
               nUserMode := AC_EXCEPT
            ENDIF
            EXIT

         CASE K_CTRL_LEFT
         CASE K_CTRL_RIGHT
         CASE K_CTRL_UP
         CASE K_CTRL_DOWN
            nUserMode := AC_EXCEPT
            EXIT

         CASE K_ESC            // Exits ACHOICE
            IF ! ::lUserFunc
               nUserMode := AC_NO_USER_FUNCTION
               nMode := AC_ABORT
            ELSE
               nUserMode := AC_EXCEPT
            ENDIF
            EXIT

         CASE K_LDBLCLK        // Double click mouse button
         CASE K_LBUTTONDOWN    // Click mouse button
            nAux := ::HitTest( MRow(), MCol() )
            IF nAux != 0 .AND. IsAvailableItem( ::nFirstRow + nAux - 1 )
               ::MoveCursor( ::nFirstRow + nAux - 1 - ::nOption, 1, 0 )
               IF nKey == K_LDBLCLK
                  IF ::lUserFunc
                     // EMULATE ENTER
                     hb_SetLastKey( K_ENTER )
                  ELSE
                     nUserMode := AC_NO_USER_FUNCTION
                     nMode := AC_SELECT
                  ENDIF
               ELSE
                  nUserMode := AC_NO_USER_FUNCTION
               ENDIF
            ENDIF
            EXIT
      END


      IF ::lUserFunc .AND. nUserMode != AC_NO_USER_FUNCTION

         nMode := Do( ::uUserFunc, nUserMode, ::nOption, ::nOption - ::nFirstRow )

         /* 2007/FEB/12 - E.F. Abort when UDF returns NIL. Clipper compliance. */
         DEFAULT nMode TO 0   // AC_ABORT

         IF nMode < 0 .OR. nMode > AC_MAXVALUE
            nMode := AC_CONT
         ENDIF

         IF nMode != AC_ABORT .and. ::nArraySize != Len( ::acItems )
            ::Refresh()
         ENDIF

         IF ! ::ValidateArray()
            nMode := AC_ABORT
         ENDIF

      ENDIF


      IF nMode == AC_SELECT
         nRet := ::nOption
      ELSEIF nMode == AC_GOTO
         IF nKey >= 32 .AND. nKey <= 255
            nAux := ::nOption
            DO WHILE .T.
               nAux++
               IF nAux > ::nItems
                  nAux := 1
               ENDIF
               IF ::nOption == nAux
                  EXIT
               ENDIF
               IF UPPER( LEFT( ::acItems[ nAux ], 1 ) ) == UPPER( CHR( nKey ) ) .AND. IsAvailableItem( nAux )
                  ::MoveCursor( nAux - ::nOption, 1, 0 )
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDIF

   ENDDO

   ::DrawRows( ::nOption - ::nFirstRow, ::nOption - ::nFirstRow, .F. )

   SetCursor( nSaveCsr )

RETURN nRet

METHOD ValidateArray() CLASS TAChoice
LOCAL lValid
   lValid := .T.
   IF ::nArraySize != LEN( ::acItems ) .OR. ! IsAvailableItem( ::nOption )

      ::Refresh(.f.)
      ::nArraySize := Len( ::acItems)
      ::alSelect := ARRAY( ::nItems )
      AEVAL( ::alSelect, { |/*x*/,i| ::alSelect[ i ] := IsItemSelectable( i, ::nItems, ::uSelect, ::acItems ) } )

      lValid := ::MoveCursor( 0, 1, 0 )
   ENDIF
RETURN lValid

METHOD MoveCursor( nMove, nDirection, nMoveScreen ) CLASS TAChoice
LOCAL nBounce := 0
LOCAL nLastFirstRow := ::nFirstRow
LOCAL nBottom
LOCAL nDiff := 0

   IF ::nItems == 0
      RETURN .F.
   ENDIF
   ::DrawRows( ::nOption - ::nFirstRow, ::nOption - ::nFirstRow, .F., .f.)
   ::nFirstRow := Max( Min( ::nFirstRow + nMoveScreen, ::nItems - ::nSize ), 1 )
   DO WHILE nBounce < 2
      ::nOption += nMove
      IF ::nOption < 1
         ::nOption := 1
         nDirection := 1
         nMove := 0
         nBounce++
         ::nFirstRow := 1
      ELSEIF ::nOption > ::nItems
         ::nOption := ::nItems
         nDirection := -1
         nMove := 0
         nBounce++
         ::nFirstRow := ::nItems - ::nSize
      ELSE
         nMove := nDirection
         IF IsAvailableItem( ::nOption )
            EXIT
         ENDIF
      ENDIF
   ENDDO
   ::nFirstRow := Max( Min( Max( Min( ::nFirstRow, ::nOption ), ::nOption - ::nSize ), ::nItems - ::nSize ), 1 )
   nBottom := Min( ::nTop + ::nItems - 1, ::nBottom )
   IF nBounce != 2
      IF ::nFirstRow != nLastFirstRow
         IF ABS( ::nFirstRow - nLastFirstRow ) > ::nSize
            ::DrawRows( 0, ::nSize, .F. )
         ELSEIF ::nFirstRow < nLastFirstRow
            ScrollFixed( ::nTop, ::nLeft, nBottom, ::nRight, ::nFirstRow - nLastFirstRow )
            ::DrawRows( 0, nLastFirstRow - ::nFirstRow - 1, .F. )
         ELSE
            ScrollFixed( ::nTop, ::nLeft, nBottom, ::nRight, ::nFirstRow - nLastFirstRow )
            ::DrawRows( ::nSize - ( ::nFirstRow - nLastFirstRow ) + 1, ::nSize, .F. )
         ENDIF
      ENDIF
      ::DrawRows( ::nOption - ::nFirstRow, ::nOption - ::nFirstRow, .F., .F. )
   ENDIF
RETURN ( nBounce != 2 )

METHOD DrawRows( nFrom, nTo, lHilite, lOut ) CLASS TAChoice
LOCAL nCurOption

   IF VALTYPE( lOut ) != "L" .OR. lOut
      DispBegin()
      DO WHILE nFrom <= nTo
         nCurOption := ::nFirstRow + nFrom
         IF nCurOption > ::nItems
            EXIT
         ELSEIF nCurOption == ::nOption .AND. lHilite
            ColorSelect( CLR_ENHANCED )
         ELSEIF IsAvailableItem( nCurOption )
            ColorSelect( CLR_STANDARD )
         ELSE
            ColorSelect( CLR_UNSELECTED )
         ENDIF
         DispOutAt( ::nTop + nFrom, ::nLeft, PadR( ::acItems[ ::nFirstRow + nFrom ], ::nRight - ::nLeft + 1 ) )
         nFrom++
      ENDDO
      DispEnd()
   ENDIF
   ColorSelect( CLR_STANDARD )
   SetPos( ::nTop + ::nOption - ::nFirstRow, ::nLeft )
RETURN nil

METHOD HitTest( nRow, nCol ) CLASS TAChoice
LOCAL nRet
   IF nCol >= ::nLeft .AND. nCol <= ::nRight .AND. ;
      nRow >= ::nTop  .AND. nRow <= ::nBottom
      nRet := nRow - ::nTop + 1
   ELSE
      nRet := 0
   ENDIF
RETURN nRet


#pragma BEGINDUMP
#include "hbapi.h"
#include "hbvm.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbstack.h"

// Determines if any element is selectable
HB_FUNC_STATIC( ISITEMSELECTABLE )
{
   UINT     uiItem  = hb_parni( 1 );               // Item to verify
   UINT     uiCount = hb_parni( 2 );               // Count of items
   PHB_ITEM pSelect = hb_param( 3, HB_IT_ANY );    // xSelectable
   PHB_ITEM pData   = hb_param( 4, HB_IT_ANY );    // Array of items
   BOOL     bResult = TRUE;
   PHB_ITEM pSelectItem = NULL;

   if( uiItem > uiCount )
   {
      bResult = FALSE;
   }
   else if( pSelect )
   {
      if( HB_IS_ARRAY( pSelect ) )
      {
         if( uiItem >= 1 && uiItem <= pSelect->item.asArray.value->ulLen )
         {
            pSelectItem = hb_itemArrayGet( pSelect, uiItem );
            pSelect = pSelectItem;
         }
      }

      if( HB_IS_LOGICAL( pSelect ) )
      {
         bResult = pSelect->item.asLogical.value;
      }
      else if( HB_IS_STRING( pSelect ) )
      {
         HB_MACRO_PTR pMacro;

         pMacro = hb_macroCompile( pSelect->item.asString.value );

         if( pMacro )
         {
            hb_macroRun( pMacro );
            hb_macroDelete( pMacro );
            pSelect = hb_stackItemFromTop( -1 );

            if( pSelect && HB_IS_LOGICAL( pSelect ) )
            {
               bResult = pSelect->item.asLogical.value;
            }

            hb_stackPop();
         }
      }
      else if( HB_IS_BLOCK( pSelect ) )
      {
         PHB_ITEM pItem = hb_arrayGetItemPtr( pData, uiItem );
         PHB_ITEM pIndex = hb_itemPutNI( NULL, uiItem );

         hb_evalBlock( pSelect, pItem, pIndex, NULL );
         pSelect = hb_param( -1, HB_IT_ANY );

         hb_itemRelease( pIndex );

         if( pSelect && HB_IS_LOGICAL( pSelect ) )
         {
            bResult = pSelect->item.asLogical.value;
         }
      }

      if( pSelectItem )
      {
         hb_itemRelease( pSelectItem );
      }
   }

   hb_retl( bResult );
}
#pragma ENDDUMP
