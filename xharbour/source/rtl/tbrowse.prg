/*
 * $Id: tbrowse.prg,v 1.195 2008/05/20 00:57:59 modalsist Exp $
 */

/*
 * xHarbour Project: TBrowse Class
 *
 * Copyright 2008 xHarbour Dev Team 
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

/*
 * The following parts are Copyright of the individual authors.
 *
 * From original Harbour project:
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * From xHarbour project:
 *
 * Copyright 2000, '01, '02 Maurilio Longo <maurilio.longo@libero.it>
 * Cursor movement handling, stabilization loop, multi-line headers and footers support
 * ::PageUp(), ::PageDown(), ::Down(), ::Up(), ::GoBottom(), ::GoTop(), ::Stabilize()
 * ::DispCell(), ::WriteMLineText(), ::RedrawHeaders(),
 * ::SetFrozenCols(), ::SetColumnWidth()
 *
 * Copyright 2005 Maurilio Longo <maurilio.longo@libero.it>
 * TDataCache class - BottomUp repainting - full review of painting code, in particular
 * ::Invalidate() instead of Refreshall() when possibile
 *
 * Copyright 2001 Manu Exposito <maex14@dipusevilla.es>
 * Activate data PICTURE DispCell(nColumn, nColor)
 *
 * Copyright 2008 Eduardo Fernandes <modalsist@yahoo.com.br>
 * Strongly modified to perform general optimizations.
 *
 */


#include "hbclass.ch"
#include "color.ch"
#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "button.ch"
#include "tbrowse.ch"

/* TBrowse movement constants. */
#define TBM_UP        1
#define TBM_DOWN      2
#define TBM_PGUP      3
#define TBM_PGDN      4
#define TBM_TOP       5
#define TBM_BOTTOM    6
#define TBM_RIGHT     7
#define TBM_LEFT      8
#define TBM_HOME      9
#define TBM_END      10
#define TBM_PANHOME  11
#define TBM_PANEND   12
#define TBM_PANLEFT  13
#define TBM_PANRIGHT 14

/* TBColumn info constants. */
#define TBCI_OBJ             1   // Object TBColumn
#define TBCI_TYPE            2   // Type of Data in Column
#define TBCI_WIDTH           3   // Column Width
#define TBCI_HEADING         4   // Column Headings
#define TBCI_FOOTING         5   // Column Footings
#define TBCI_PICT            6   // Column Picture
#define TBCI_WIDTHCELL       7   // Width of the Cell
#define TBCI_COLSEP          8   // Column Seperator
#define TBCI_SEPWIDTH        9   // Width of the Separator
#define TBCI_DEFCOLOR       10   // Array with index of color
#define TBCI_SETWIDTH       11   // If True, only SetFrozen can change TBCI_WIDTH
#define TBCI_LCOLSEP        12   // Should column separator be drawn
#define TBCI_SCRCOLPOS      13   // Temporary column position on screen

#define TBC_CLR_STANDARD  1  // first index value to set unselected data color.
#define TBC_CLR_ENHANCED  2  // second index value to set selected data color.

#ifdef HB_COMPAT_C53
#define TBC_CLR_HEADING   3  // third index value to set heading color.
#define TBC_CLR_FOOTING   4  // fourth index value to set footing color.
#else
#define TBC_CLR_HEADING   TBC_CLR_STANDARD
#define TBC_CLR_FOOTING   TBC_CLR_STANDARD
#endif

/* TBrowse ColorRect constants. */
#define TBCRECT_TOP        1  // colorrect top rectangle coordinate
#define TBCRECT_LEFT       2  // colorrect lef rectangle coordinate
#define TBCRECT_BOTTOM     3  // colorrect bottom rectangle coordinate
#define TBCRECT_RIGHT      4  // colorrect rith retangle coordinate
#define TBCRECT_COLOR      5  // colorrect color array

/* TBrowse ColorRect Color constants. */
#define TBCRECT_STD        1  // Colorrect color standard
#define TBCRECT_ENH        2  // Colorrect color enhanced


*------------------------*
CLASS TDataCell STATIC
*------------------------*

   ACCESS Value         INLINE ::xValue
   ACCESS Color         INLINE ::aColor
   ACCESS ColorRect     INLINE ::aColorRect

   ASSIGN Value(x)      INLINE ::xValue := x
   ASSIGN Color(a)      INLINE ::aColor := a
   ASSIGN ColorRect(a)  INLINE ::aColorRect := a

   METHOD New()         INLINE Self

PROTECTED:

   DATA  xValue        // Cell value
   DATA  aColor        // Cell Color array
   DATA  aColorRect    // Cell color rectangle area - { <nTop>,<nLeft>,<nBottom>,<nRight>,<aColorIndex> }
                       // Color index is an array that apoint to ColorSpec.
END CLASS

*-----------------------*
CLASS TDataCache STATIC
*-----------------------*

   ACCESS   CurRow       INLINE ::nCurRow
   ASSIGN   CurRow(nRow) INLINE ::nCurRow := if( hb_isnumeric(nRow), Max(1,Min(nRow,::nMaxRow)) , ::nCurRow )

   ACCESS   LastRow      INLINE ::nLastRow

   METHOD   New( oBrowse )               // Which tbrowse this cache is bound to

   METHOD   Skip( nRowsToSkip )          // moves datasource rows 
   METHOD   GoTop()                      // moves datasource to first row
   METHOD   GoBottom()                   // moves datasource to last row

   METHOD   GetCellValue( nRow, nCol )   // Retrieves the content of nRow/nCol. 
   METHOD   GetCellColor( nRow, nCol )   // Retrieves the color   of nRow/nCol. 

   METHOD   Invalidate(nRow)             // Invalidate the data cache.                
   METHOD   PerformInvalidation()        // Executes invalidation.

   METHOD   ClearColorRect()             // Restore all column colorrect to nil.
   METHOD   DeleteColorRect()            // Delete all/one row/col colored.
   METHOD   SetColorRect( aRect )        // Sets colorrect to cells defined by aRect, aRect is an array { top, left, bottom, right, aColors }
   METHOD   ResetColorRect()             // Reset colorrect area.

HIDDEN:

   DATA  nCurRow                         // Current Row inside cache
   DATA  nLastRow                        // Last row inside cache 
   DATA  nMaxRow                         // Max row allowed inside cache.

   DATA  nCachedColLeft                  // Leftmost cached column.
   DATA  nCachedColRight                 // Rightmost cached column.
   DATA  nCachedCols                     // Amount cached columns into ::aCache array.

   DATA  aCache                          // Array with cached data
   DATA  oBrowse                         // TBrowse object I'm caching
   DATA  aRect                           // Array with pending color-rects

   DATA  lInvalid                        // if true all rows will be invalidated. 
   DATA  lNewRow                         // if true indicate that new row is active (append mode).

   METHOD  InitCache()                   // Init DataCache vars.
   METHOD  FillRow( nRow )               // Fills a row of aCache with data from datasource
   METHOD  ColIndex()                    // Determine the relative column index into ::aCache array.

   METHOD  SetCachedCols()               // Calculate what columns will be cached.
   METHOD  SyncData(nRow,nCol)           // Syncronize data cache cursor with Browse row.

END CLASS

*--------------------------------------*
METHOD New( oBrowse ) CLASS TDataCache
*--------------------------------------*

   ::oBrowse     := oBrowse
   ::aRect       := {}
   ::nCurRow     := 1
   ::lNewRow     := .f.

   ::InitCache()

RETURN Self

*-----------------------------------------*
METHOD InitCache( lNew ) CLASS TDataCache
*-----------------------------------------*
Default lNew to .f.

   ::nMaxRow  := ::oBrowse:RowCount
   ::nLastRow := Max(1, ::nMaxRow )

   ::lInvalid := .F.
   ::aCache   := Array( ::nMaxRow )

RETURN NIL


*--------------------------------------------*
METHOD Skip( nRowsToSkip ) CLASS TDataCache
*--------------------------------------------*
LOCAL nSkipped, nNewRow, lEof, lOffSet


   if ! hb_IsBlock( ::oBrowse:SkipBlock )
      ::lNewRow := .f.
      Return 0
   endif

   if nRowsToSkip <= 0
      ::lNewRow := .f.
   endif

   if nRowsToSkip == 0 
      Eval( ::oBrowse:SkipBlock, 0 )
      ::FillRow( ::nCurRow )
      Return 0
   endif

   lOffSet := .f.
   lEof    := eof()

   if ! ::lNewRow
      nSkipped := Eval( ::oBrowse:SkipBlock, nRowsToSkip )
      ::lNewRow := ( !lEof .and. eof() ) 
   else
      lOffSet  := .t.
      nSkipped := 0
   endif

   nNewRow := ::nCurRow + nSkipped

   if Empty( nSkipped ) // can be nil also.
      nSkipped := 0

   elseif nSkipped == nRowsToSkip

      if  nNewRow >= 1 .and. nNewRow <= ::nMaxRow
         ::nCurRow += nSkipped
      else

         // Movement greater than PageUp or PageDown
         if Abs( nSkipped ) > ( ::nMaxRow  * 2 ) 

            Eval( ::oBrowse:SkipBlock, -nSkipped )
            ::nCurRow := 1

         else

            // It was PageDown, PageUp, Up or Down movement.
            if Abs( nSkipped ) >= ::nMaxRow 

               // PageDown
               if nSkipped > 0
                  ::nCurRow := ::nLastRow 

               // PageUp
               else
                  ::nCurRow := 1

               endif

               AFill( ::aCache, NIL )

            else

               // I'm at top or bottom of TBrowse so I can scroll
               // Down or Up
               if ::nCurRow == ::nLastRow
                  ADel( ::aCache, 1 )
               else
                  AIns( ::aCache, 1 )
                  if ::nLastRow < ::nMaxRow
                     ::nLastRow ++
                  endif
               endif

            endif

         endif

      endif

   else

      // I couldn't move as far as requested. I need to refresh all rows
      // if I go past current top or bottom row
      if ( nNewRow < 1 .OR.  nNewRow > ::nMaxRow )
         // Don't go past boundaries
         ::nCurRow := if( nSkipped > 0, ::nLastRow, 1 )
         AFill( ::aCache, NIL )

      else

         ::nCurRow += nSkipped

         if CacheOK(::aCache,::nCurRow)
            ::aCache[ ::nCurRow ] := NIL
         endif

      endif

   endif

RETURN nSkipped

*--------------------------------------------*
METHOD GoTop() CLASS TDataCache
*--------------------------------------------*
Local lGoTop

   ::lNewRow := .f.

   lGoTop := hb_IsBlock( ::oBrowse:GoTopBlock )

   if lGoTop
      Eval( ::oBrowse:GoTopBlock )
   endif

   ::nCurRow := 1
   ::FillRow( ::nCurRow)

RETURN NIL

*--------------------------------------------*
METHOD GoBottom() CLASS TDataCache
*--------------------------------------------*
Local nToTop, lGoBottom

   ::lNewRow := .f.

   lGoBottom := hb_IsBlock( ::oBrowse:GoBottomBlock )

   if lGoBottom
      Eval( ::oBrowse:GoBottomBlock )
   endif

   // How many rows are available from top upto bottom.
   nToTop := Eval( ::oBrowse:SkipBlock, - ( ::nMaxRow  - 1 ) )

   if lGoBottom
       // Repos cursor to bottom row again.
       Eval( ::oBrowse:GoBottomBlock )
   endif

   if Empty( nToTop )
      nToTop := 0
   else
      nToTop := Abs( nToTop )
   endif

   ::nCurRow := Min(nToTop+1,::nLastRow)
   ::FillRow( ::nCurRow )

RETURN nToTop

*----------------------------------------------*
METHOD SyncData( nRow, nCol ) CLASS TDataCache
*----------------------------------------------*
Local nSkip, nCurRow 

   if empty(nRow) .or. empty(nCol) .or. nRow > ::nLastRow 
      Return .f.
   endif
  
   nCurRow := ::nCurRow

   if nRow != nCurRow 
      // Sync data cache cursor with browse row.
      nSkip := ( nCurRow - nRow )
      ::Skip( -nSkip )
   endif

   if empty( ::aCache[ nRow ] ) .and. nRow == ::nCurRow
      ::FillRow( nRow )
   endif

RETURN .t.

*----------------------------------------------*
METHOD FillRow( nRow ) CLASS TDataCache
*----------------------------------------------*
Local aCol, oCell, nRectPos, nCol, nColIndex, xValue

   if nRow > ::nMaxRow
      RETURN Self
   endif

   // To speed-up, only visible columns will be used.
   ::SetCachedCols()

   ::aCache[ nRow ] := Array( ::nCachedCols )

   for nCol := ::nCachedColLeft to ::nCachedColRight

       aCol := ::oBrowse:aColsInfo[ nCol ]
       oCell := TDataCell():New()

       with object oCell

          :Value := Eval( aCol[ TBCI_OBJ ]:block )
          xValue := if( :Value = NIL, BlankValue(aCol),:Value )

          if HB_IsBlock( aCol[ TBCI_OBJ ]:ColorBlock )
             :Color := DefColorOK( ::oBrowse:ColorSpec, Eval( aCol[ TBCI_OBJ ]:ColorBlock, xValue ) )
          endif
    
          if ! Empty( ::aRect ) .and.;
             ( nRectPos := AScan( ::aRect, { |item| item[ 1 ] == nRow } ) ) > 0

             if nCol >= ::aRect[ nRectPos ][ 2 ] .and.;
                nCol <= ::aRect[ nRectPos ][ 3 ]

                :ColorRect := ::aRect[ nRectPos ][ 4 ]

             endif

          endif

       end

       nColIndex := ::ColIndex( nCol )
 
       if CacheOK( ::aCache, nRow, nColIndex ) 
          ::aCache[ nRow ][ nColIndex ] := oCell
       endif

   next

Return Self

*--------------------------------------------------*
METHOD GetCellValue( nRow, nCol ) CLASS TDataCache
*--------------------------------------------------*
Local nColIndex, xValue

  // if cursor is on an new row, I need sync it.
  if ::nCurRow > ::nLastRow
     ::nCurRow := ::nLastRow 
  endif

  if ::SyncData( nRow, nCol )

     nColIndex := ::ColIndex( nCol )

     if CacheOK( ::aCache, nRow, nColIndex )
        xValue := ::aCache[ nRow ][ nColIndex ]:Value
     endif

  endif

Return xValue

*--------------------------------------------------*
METHOD GetCellColor( nRow, nCol ) CLASS TDataCache
*--------------------------------------------------*
Local aColor, nColIndex 

   nColIndex := ::ColIndex( nCol ) 

   if CacheOK( ::aCache, nRow, nColIndex )

      aColor := ::aCache[ nRow ][ nColIndex ]:ColorRect

      if Empty( aColor )
         aColor := ::aCache[ nRow ][ nColIndex ]:Color
      endif

   endif

RETURN aColor

*--------------------------------------------------*
METHOD SetColorRect( aRect ) CLASS TDataCache
*--------------------------------------------------*
Local nRow, nCol, nColIndex, i

 if ! empty( aRect )

    ::ClearColorRect()

    // Assign new colorrect from TBrowse. Caution: this array haves 5 items
    // instead 4 of datacache's colorrect array 
    for nRow := aRect[TBCRECT_TOP] to aRect[TBCRECT_BOTTOM]

        // A five elements array shrinks to a four one
        // { top, left, bottom, right, aColors } =>
        // { nRow, left, right, aColors }
        AAdd( ::aRect, { nRow, aRect[TBCRECT_LEFT], aRect[TBCRECT_RIGHT], aRect[TBCRECT_COLOR] } )

    next

    // Reset colorrect to all cached columns
    for i := 1 to LEN(::aRect)

        aRect := ::aRect[i]
        nRow := aRect[1]

        for nCol := aRect[2] to aRect[3]

            nColIndex := ::ColIndex(nCol)

            if CacheOK( ::aCache, nRow, nColIndex )
               ::aCache[ nRow ][ nColIndex ]:ColorRect := aRect[4]
            endif

        next
    next

 endif

Return Self

*--------------------------------------------------*
METHOD ClearColorRect() CLASS TDataCache
*--------------------------------------------------*
LOCAL nRow, nCol, nColIndex

 for nRow := 1 to ::nLastRow

     for nCol := ::oBrowse:LeftVisible to ::oBrowse:RightVisible

         nColIndex := ::ColIndex(nCol)

         if CacheOK( ::aCache, nRow, nColIndex )
            ::aCache[ nRow ][ nColIndex ]:ColorRect := NIL
         endif

     next

 next

RETURN Self

*--------------------------------------------------*
METHOD DeleteColorRect( nRow ) CLASS TDataCache
*--------------------------------------------------*
LOCAL i

  if ! empty( ::aRect )

     while .t.

     i := AScan( ::aRect, { |item| item[1] == nRow } )

       if i > 0
          ADel( ::aRect, i , .T. )
       else
          exit
       endif

     enddo

  endif

Return Self

*---------------------------------------------------------------*
METHOD ResetColorRect( lResetRect, nSkipped ) CLASS TDataCache
*---------------------------------------------------------------*
LOCAL aRect, nRow, nCol, nColIndex, i

   if lResetRect 
      ::ClearColorRect()
      ::aRect := {}
      Return Self
   endif

   aRect := AClone( ::aRect )

   // The cursor was moved.
   if ! empty( aRect ) .and. nSkipped != 0  

      ::ClearColorRect()

      for i := 1 to Len( aRect )
          if nSkipped > 0  // Down/PageDn
             aRect[ i, 1 ] -= nSkipped
          else             // Up/PageUP
             aRect[ i, 1 ] += ABS(nSkipped)
          endif
      next

      ::aRect := {}

      for i := 1 to Len( aRect )
          if aRect[ i,1 ] >= 1 .and. aRect[ i,1 ] <= ::nLastRow
             AAdd( ::aRect, AClone( aRect[ i ] ) )
          endif
      next

      aRect := NIL

      // Reset colorrect to all cached columns
      for i := 1 to Len( ::aRect )

          aRect := ::aRect[ i ]
          nRow  := aRect[1]

          for nCol := aRect[ 2 ] to aRect[ 3 ]

              nColIndex := ::ColIndex(nCol)

              if CacheOK( ::aCache, nRow, nColIndex )
                 ::aCache[ nRow ][ nColIndex ]:ColorRect := aRect[ 4 ]
              endif

          next
      next

   endif

Return Self

*------------------------------------------------------*
METHOD Invalidate( nRow ) CLASS TDataCache
*------------------------------------------------------*

   if nRow == NIL 
      ::lInvalid := .T.
   elseif nRow >= 1 .and. nRow <= Len( ::aCache )
      ::aCache[ nRow ] := NIL
   endif

RETURN Self

*------------------------------------------------------*
METHOD PerformInvalidation() CLASS TDataCache
*------------------------------------------------------*

   if ::lInvalid
      AFill( ::aCache, NIL )
      ::lInvalid := .F.
   endif

RETURN Self

*------------------------------------------------------*
METHOD SetCachedCols() CLASS TDataCache
*------------------------------------------------------*
Local oBrw := ::oBrowse

 ::nCachedColLeft  := Max(1, if( oBrw:Freeze > 0, 1, oBrw:LeftVisible ) )
 ::nCachedColRight := Min( oBrw:RightVisible, oBrw:ColCount )
 ::nCachedCols     := Max(1, Min( oBrw:ColCount, ::nCachedColRight - ::nCachedColLeft + 1 ) )

RETURN Self

*------------------------------------------------------*
METHOD ColIndex( nCol ) CLASS TDataCache
*------------------------------------------------------*
Local nColIndex

   if ::nCachedColLeft == NIL .or. ::nCachedColRight == NIL
      ::SetCachedCols()
   endif

   nColIndex := Min( Max(1, nCol + ::nCachedCols - ::nCachedColRight ), Min(::oBrowse:ColCount,::oBrowse:RightVisible)  )

RETURN nColIndex

*------------------------------------------------------*
CLASS TBrowse STATIC
*------------------------------------------------------*
                                                
   ACCESS AutoLite         INLINE ::lAutoLite     // Logical value to control highlighting
   ASSIGN AutoLite(l)      INLINE ::lAutoLite := if(hb_islogical(l),l,::lAutoLite)

   ACCESS Cargo            INLINE ::uCargo        // User-definable variable
   ASSIGN Cargo(u)         INLINE ::uCargo := u

   ACCESS LeftVisible      INLINE ::nLeftVisible  // Indicates position of leftmost unfrozen column in display
   ASSIGN LeftVisible(n)   INLINE ::nLeftVisible := if(hb_isnumeric(n),n,::nLeftVisible)

   ACCESS RightVisible     INLINE ::nRightVisible // Indicates position of rightmost unfrozen column in display
   ASSIGN RightVisible(n)  INLINE ::nRightVisible := if(hb_isnumeric(n),n,::nRightVisible)

   ACCESS ColCount         INLINE ::nColCount     // Number of TBrowse columns
   ACCESS RowCount         INLINE ::nRowCount     // Number of visible data rows in the TBrowse display

   ACCESS Stable           INLINE ::lStable       // Indicates if the TBrowse object is stable
   ASSIGN Stable(l)        INLINE ::lStable := if(hb_islogical(l),l,::lStable)

#ifdef HB_COMPAT_C53
   DATA nRow                               // Row number for the actual cell
   DATA nCol                               // Col number for the actual cell
   DATA aKeys
   DATA mColPos
   DATA mRowPos
   DATA cMessage
#endif

#ifdef HB_EXTENSION
   DATA aColumnsSep                        // Holds the column position where seperators are marked . for Wvt_DrawGridVert()
#endif

   ACCESS Border               INLINE ::cBorder
   ASSIGN Border( cBorder )    INLINE ::SetBorder( cBorder )

   ACCESS ColorSpec            INLINE ::cColorSpec              // Color table for the TBrowse display
   ASSIGN ColorSpec(cColor)    INLINE ::SetColorSpec( cColor )

   ACCESS ColPos               INLINE ::nColPos
   ASSIGN ColPos( nColPos )    INLINE ::SetColPos( nColPos )

   ACCESS RowPos               INLINE ::nRowPos
   ASSIGN RowPos( nRow )       INLINE ::SetRowPos( nRow )

   ACCESS HitBottom            INLINE ::lHitBottom              // Indicates the end of available data
   ASSIGN HitBottom( lbottom ) INLINE ::lHitBottom := if( hb_islogical(lbottom), lbottom, ::lHitbottom )  

   ACCESS HitTop               INLINE ::lHitTop                 // Indicates the beginning of available data
   ASSIGN HitTop( lTop )       INLINE ::lHitTop := if( hb_islogical(lTop), lTop, ::lHitTop )  

   ACCESS nTop                 INLINE ::GetCoordinate( 0 )
   ASSIGN nTop( nTop )         INLINE ::SetCoordinate( 0, nTop )

   ACCESS nLeft                INLINE ::GetCoordinate( 1 )
   ASSIGN nLeft( nLeft )       INLINE ::SetCoordinate( 1, nLeft )

   ACCESS nBottom              INLINE ::GetCoordinate( 2 )
   ASSIGN nBottom( nBottom )   INLINE ::SetCoordinate( 2, nBottom )

   ACCESS nRight               INLINE ::GetCoordinate( 3 )
   ASSIGN nRight( nRight )     INLINE ::SetCoordinate( 3, nRight )

   ACCESS ColSep               INLINE ::cColSep    // Column separator character
   ASSIGN ColSep( cSep )       INLINE ::SetSeparator( 0, cSep )

   ACCESS FootSep              INLINE ::cFootSep   // Footing separator character
   ASSIGN FootSep( cSep )      INLINE ::SetSeparator( 1, cSep )

   ACCESS HeadSep              INLINE ::cHeadSep   // Head separator character
   ASSIGN HeadSep( cSep )      INLINE ::SetSeparator( 2, cSep )

   ACCESS Freeze               INLINE ::nFrozenCols     // Number of columns to freeze/frozen
   ASSIGN Freeze( nHowMany )   INLINE ::SetFrozenCols( nHowMany, .t. ), ::lConfigured := .f., ::nFrozenCols

   ACCESS GoTopBlock           INLINE ::bGoTopBlock        // Code block executed by TBrowse:goTop()
   ASSIGN GoTopBlock(b)        INLINE ::bGoTopBlock := ::SetMoveBlock(0,b)

   ACCESS GoBottomBlock        INLINE ::bGoBottomBlock     // Code block executed by TBrowse:goBottom()
   ASSIGN GoBottomBlock(b)     INLINE ::bGoBottomBlock := ::SetMoveBlock(1,b)

   ACCESS SkipBlock            INLINE ::bSkipBlock         // Code block executed by TBrowse:SkipBlock()
   ASSIGN SkipBlock(b)         INLINE ::bSkipBlock := ::SetMoveBlock(2,b)    

   METHOD New( nTop, nLeft, nBottom, nRight )  // Constructor

   METHOD Up()                INLINE ::MoveTo( TBM_UP )       // Moves cursor up one row
   METHOD Down()              INLINE ::MoveTo( TBM_DOWN )     // Moves cursor down one row

   METHOD PageUp()            INLINE ::MoveTo( TBM_PGUP )      // Repositions the data source upward
   METHOD PageDown()          INLINE ::MoveTo( TBM_PGDN )      // Repositions the data source downward

   METHOD GoTop()             INLINE ::MoveTo( TBM_TOP )       // Repositions the data source to the top of file
   METHOD GoBottom()          INLINE ::MoveTo( TBM_BOTTOM )    // Repositions the data source to the bottom of file

   METHOD Right()             INLINE ::MoveTo( TBM_RIGHT )     // Moves cursor right one column
   METHOD Left()              INLINE ::MoveTo( TBM_LEFT )      // Moves cursor left one column

   METHOD Home()              INLINE ::MoveTo( TBM_HOME )      // Moves cursor to the leftmost visible data column
   METHOD End()               INLINE ::MoveTo( TBM_END )       // Moves cursor to the rightmost visible data column

   METHOD PanHome()           INLINE ::MoveTo( TBM_PANHOME )   // Moves cursor to the leftmost visible data column
   METHOD PanEnd()            INLINE ::MoveTo( TBM_PANEND )    // Moves cursor to the rightmost data column

   METHOD PanLeft()           INLINE ::MoveTo( TBM_PANLEFT )   // Pans left without changing the cursor position
   METHOD PanRight()          INLINE ::MoveTo( TBM_PANRIGHT )  // Pans right without changing the cursor position

   METHOD AddColumn( oCol )                       // Add a column object to the Tbrowse
   METHOD DelColumn( nPos )                       // Delete a column object from a Tbrowse
   METHOD InsColumn( nPos, oCol )                 // Insert a column object in a Tbrowse

   METHOD GetColumn( nColumn )                    // Gets a specific TBColumn object
   METHOD SetColumn( nColumn, oCol )              // Replaces one TBColumn object with another

   METHOD ColWidth( nColumn )                     // Returns the display width of a particular column

   METHOD ColorRect( aRect, aColors )             // Alters the color of a rectangular group of cells
   METHOD Hilite()             INLINE ::SetHilite(.T.)   // Highlights the current cell
   METHOD DeHilite()           INLINE ::SetHilite(.F.)   // Dehighlights the current cell

   METHOD Configure( nMode )                      // Configures the internal settings of the TBrowse object
   METHOD PerformConfiguration()                  // Call ::Configure() in any circunstancies.
                                                  // nMode is an undocumented parameter in CA-Cl*pper

   METHOD ForceStable()        INLINE ::Stabilize(.T.)  // Performs forced stabilization
   METHOD Stabilize()                                   // Performs incremental stabilization

   METHOD Invalidate()                                  // Forces resdraw during the next stabilization.
   METHOD RefreshAll()          INLINE ::Refresh(.T.)   // Causes refresh all data to be refreshed during the next stabilize
   METHOD RefreshCurrent()      INLINE ::Refresh(.F.)   // Causes current row to be refreshed during the next stabilize

#ifdef HB_COMPAT_C53
   METHOD SetKey( nKey, bBlock )
   METHOD ApplyKey( nKey )
   METHOD InitKeys( Self )
   METHOD TApplyKey( nKey, o )
   METHOD HitTest( nMouseRow,nMouseCol )
   METHOD SetStyle( nStyle,lSetting )
   ACCESS Style INLINE ::aStyle
#endif


PROTECTED:

   METHOD MGotoYX( nRow, nCol )                   // Given screen coordinates nRow, nCol sets TBrowse cursor on underlaying cell
                                                  // _M_GotoXY because this method will mostly be called to handle mouse requests

   DATA bGoBottomBlock                            // Internal code block executed by TBrowse:goBottom()
   DATA bGoTopBlock                               // Internal code block executed by TBrowse:goTop()
   DATA bSkipBlock                                // Internal code block used to reposition data source

HIDDEN: 

   METHOD PosCursor()                             // Positions the cursor to the beginning of the call, used only when autolite==.F.
   METHOD LeftDetermine()                         // Determine leftmost unfrozen column in display
   METHOD DispCell( nRow, nCol, xValue, nColor )  // Displays a single cell and returns position of first char of displayed value if needed
   METHOD HowManyCol()                            // Counts how many cols can be displayed
   METHOD RedrawHeaders()                         // Repaints TBrowse Headers
   METHOD DrawRow()                               // Draw one or all rows in stabilization (early DrawARow() method).
   METHOD SetColorSpec( cColor )

   METHOD Moved(lResetHits, lForceHitsFalse)      // Every time a movement key is issued I need to reset certain properties
                                                  // of TBrowse, I do these settings inside this method
   METHOD EvalSkipBlock( nSkip )                  // Eval skip block
   METHOD MoveTo( nMove )                         // Method called by Tbrowse cursor movement methods.
   METHOD SkipRows()                 
   METHOD SyncRows()                              // Syncronyze tbrowse rows after data source movements (early CheckRowsToBeRedrawn()).
   METHOD ScrollRows()                            // Scrolls screen rows.

   METHOD ResetMove()                             // Reset vertical movement vars.

   METHOD WriteMLineText( cStr, nPadLen, lHeader, cColor ) // Writes a multi-line text where ";" is a line break, lHeader
                                                           // is .T. if it is a header and not a footer
   METHOD SetFrozenCols( nHowMany )               // Handles freezing of columns
   METHOD SetColumnWidth( oCol )                  // Calcs width of given column
   METHOD SetBorder( cBorder )                    // Draw Tbrowse border
   METHOD SetSeparator( nType, cSep )             // Set char separator for colsep, headsep, footsep.
   METHOD SetMoveBlock( nType, bBlock )           // Set codeblock to move for Top, Bottom ,Skip.

   METHOD ColInfo()                               // Set columns values (early AColInfo() method).

   METHOD GetCoordinate( nType )                  // This method calculates variables related to horizontal coordinates
   METHOD SetCoordinate( nType, uValue )          // This method calculates variables related to vertical coordinates

   METHOD SetColPos( nColPos )                    // Performs a "direct jump" to a given column
   METHOD SetRowPos( nRow )                       // Set new row pos.

   METHOD Refresh()                               // Causes refresh all/or one data row to be refreshed during the next stabilization.
   METHOD SetHilite(lHilite)                      // Hilights/Dehighlights the current cell

   DATA lAutolite
   DATA uCargo                                    // User-definable variable
   DATA nLeftVisible                              // Indicates position of leftmost unfrozen column in display
   DATA nRightVisible                             // Indicates position of rightmost unfrozen column in display
   DATA lStable                                   // Indicates if Tbrowse is stable.

   DATA aRect                                     // One or more rectangles and colors associated specified with ColorRect()
   DATA lRectPainting                             // When .T. ::ColorRect() calls ::ForceStable() to repaint a colored region

   DATA aRedraw                                   // Array of logical items indicating, is appropriate row need to be redraw
   DATA lHeaders                                  // Internal variable which indicates whether there are column headers to paint
   DATA lFooters                                  // Internal variable which indicates whether there are column footers to paint

   DATA lHeadSep       INIT .f.                   // Internal variable which indicates whether TBrowse has line headers to paint
   DATA lFootSep       INIT .f.                   // Internal variable which indicates whether TBrowse has line footers to paint
   DATA lColHeadSep    INIT .f.                   // Internal variable which indicates whether at least a TBColumn has line headers to paint
   DATA lColFootSep    INIT .f.                   // Internal variable which indicates whether at least a TBColumn has line footers to paint

   DATA lRedrawFrame                              // True if I need to redraw Headers/Footers
   DATA nColsWidth                                // Total width of visible columns plus ColSep
   DATA nColsVisible                              // Number of columns that fit on the browse width
   DATA lHitTop                                   // Internal Top/Bottom reached flag
   DATA lHitBottom                                // idem
   DATA lForceHitsFalse                           // Used when going Top/Bottom
   DATA nRowsToSkip                               // Rows to skip on next Stabilize()
   DATA nRowsSkipped
   DATA nNewRowPos                                // Next position of data source (after first phase of stabilization)
   DATA nRowData                                  // First row of data

   DATA nColPos                                   // Current cursor column position
   DATA nRowPos                                   // Cursor row position after move.

   DATA nwTop      INIT 0                         // Top row number for the TBrowse display
   DATA nwLeft     INIT 0                         // Leftmost column for the TBrowse display
   DATA nwRight    INIT 0                         // Rightmost column for the TBrowse display
   DATA nwBottom   INIT 0                         // Bottom row number for the TBrowse display

   DATA cBorder    INIT ""                        // Border character. 
   DATA cColorSpec                                // Color specification of TBrowse.

   DATA cColSep                                   // Column separator character
   DATA cFootSep                                  // Footing separator character
   DATA cHeadSep                                  // Head separator character

   DATA nHeaderHeight                             // How many lines is highest Header/Footer and so how many lines of
   DATA nFooterHeight                             // screen space I have to reserve
   DATA nFrozenWidth                              // How many screen column are not available on the left side of TBrowse display
                                                  // > 0 only when there are frozen columns
   DATA nFrozenCols                               // Number of frozen columns on left side of TBrowse
   DATA nColCount                                 // Number of columns added to TBrowse
   DATA nRowCount                                 // Number of rows added to TBrowse
   DATA lNeverDisplayed                           // TBrowse has never been stabilized 

   DATA aColsInfo                                 // Columns configuration array
   DATA nVisWidth                                 // Visible width of Browser
   DATA lConfigured                               // Specifies whether tBrowse is already configured or not
                                  
   DATA nSpacePre                                 // Blank spaces prior to first column
   DATA nSpaceLast                                // Blank spaces after the last column

   DATA oCache                                    // Object Data Cache.
   DATA lPaintBottomUp                            // When .T. after a PG_DN, paints browser from bottom to top, optimizing
                                                  // Skips calls. ( Clipper works at this way )
#ifdef HB_COMPAT_C53
   DATA mRect
   DATA aVisibleCols
   DATA aStyle                                    // See SetStyle() method.
#endif

   DATA aColorSpec                                // Holds colors of Tbrowse:ColorSpec
   DATA nPrevDelColPos                            // Save previous colpos before delcolumn(). For clipper compatibility.
   DATA nMoveTo      INIT 0                       // To control vertical movements

END CLASS

*------------------------------------------------------*
METHOD New( nTop, nLeft, nBottom, nRight ) CLASS TBrowse
*------------------------------------------------------*

DEFAULT  nTop    TO 0
DEFAULT  nLeft   TO 0
DEFAULT  nBottom TO MaxRow()
DEFAULT  nRight  TO MaxCol()

   nTop    := Max(0,nTop)
   nLeft   := Max(0,nLeft)
   nBottom := Min(MaxRow(),nBottom)
   nRight  := Min(MaxCol(),nRight)

   ::nwTop     := nTop
   ::nwLeft    := nLeft     
   ::nwBottom  := nBottom   
   ::nwRight   := nRight    

   ::nRowCount       := (nBottom - nTop + 1)
   ::nVisWidth       := nRight - nLeft + 1
   ::nRowData        := nTop
   ::lAutoLite       := .T.

   ::nLeftVisible    := 1
   ::nRightVisible   := 1
   ::nColPos         := 1
   ::nRowPos         := 1
   ::nNewRowPos      := 1
   ::lHitTop         := .F.
   ::lHitBottom      := .F.
   ::lForceHitsFalse := .F.
   ::lStable         := .F.
   ::lHeaders        := .F.
   ::lFooters        := .F.
   ::lHeadSep        := .F.
   ::lFootSep        := .F.
   ::lColHeadSep     := .F.
   ::lColFootSep     := .F.
   ::lRectPainting   := .F.
   ::lConfigured     := .F.
   ::lPaintBottomUp  := .F.

   ::lRedrawFrame    := .T.
   ::lNeverDisplayed := .T.
   ::aRect           := {}
   ::aColumnsSep     := {}
   ::aRedraw         := {}
   ::aColsInfo       := {}
   ::aColorSpec      := {}

   ::nRowsToSkip     := 0
   ::nRowsSkipped    := 0
   ::nColsWidth      := 0
   ::nColsVisible    := 0
   ::nHeaderHeight   := 0
   ::nFooterHeight   := 0
   ::nFrozenWidth    := 0
   ::nFrozenCols     := 0
   ::nColCount       := 0
   ::nSpacePre       := 0
   ::nSpaceLast      := 0
   ::nPrevDelColPos  := 0

   ::cColorSpec      := SetColor()
   ::cBorder         := ""
   ::cColSep         := " "
   ::cFootSep        := ""
   ::cHeadSep        := ""

   ::oCache          := TDataCache():New( Self )

#ifdef HB_COMPAT_C53
   ::mColPos         := 0
   ::mRowPos         := 0
   ::mRect           := { nTop, nLeft, nBottom, nRight }
   ::aVisibleCols    := {}
   ::cMessage        := ''
   ::nRow            := 0
   ::nCol            := 0
   ::aStyle          := Array( TBR_CUSTOM ) 
   AFill( ::aStyle, .F. )
#endif

Return Self

*------------------------------------------------------*
METHOD Invalidate() CLASS TBrowse
*------------------------------------------------------*

   AFill( ::aRedraw, .T. )
   ::lRedrawFrame := .T.  
   ::oCache:Invalidate() 
   ::lStable := .F.

Return Self

*------------------------------------------------------*
METHOD Refresh( lAll ) CLASS TBrowse
*------------------------------------------------------*
Default lAll to .f.

   if lAll
      AFill( ::aRedraw, .T. )
      ::oCache:Invalidate( NIL , .T. )

   elseif ! Empty( ::aRedraw ) .and. ::nRowPos > 0
      ::aRedraw[ ::nRowPos ] := .T.
      ::oCache:Invalidate( ::nRowPos, .F. )
      ::oCache:DeleteColorRect( ::nRowPos )

   endif

   ::lStable := .F.

Return Self

*------------------------------------------------------*
METHOD Configure( nMode ) CLASS TBrowse
*------------------------------------------------------*
LOCAL n, nHeight, aCol, xVal, nFreeze, aRect

Default nMode to 0    // first configuration

   if nMode > 2 .or. nMode < 0
      nMode := 0
   endif

   if ::nColPos > ::nColCount
      ::nColPos := ::nColCount
   endif

   if nMode < 2 .or. ::lNeverDisplayed

      ::lHeaders     := .F.
      ::lFooters     := .F.
      ::lColHeadSep  := .F.
      ::lColFootSep  := .F.
      ::lRedrawFrame := .T.

      ::nHeaderHeight := 0
      ::nFooterHeight := 0

      // Find out highest column header and footer
      FOR EACH aCol IN ::aColsInfo

          if nMode = 0 .or. ( nMode = 1 .and. ! ::lNeverDisplayed )

             xVal := Eval( aCol[ TBCI_OBJ ]:block )

             aCol[ TBCI_TYPE      ] := valtype( xVal )
             aCol[ TBCI_HEADING   ] := aCol[ TBCI_OBJ ]:heading
             aCol[ TBCI_FOOTING   ] := aCol[ TBCI_OBJ ]:footing
             aCol[ TBCI_PICT      ] := iif( Empty( aCol[ TBCI_OBJ ]:Picture ), "", aCol[ TBCI_OBJ ]:Picture )
             if ! aCol[ TBCI_SETWIDTH ]
                aCol[ TBCI_WIDTH  ] := ::SetColumnWidth( aCol[ TBCI_OBJ ] )
             endif
             aCol[ TBCI_WIDTHCELL ] := Min( aCol[ TBCI_WIDTH ], LenVal( xVal, aCol[ TBCI_TYPE ], aCol[ TBCI_PICT ] ) )
             aCol[ TBCI_COLSEP    ] := iif( aCol[ TBCI_OBJ ]:ColSep != NIL, aCol[ TBCI_OBJ ]:ColSep, ::ColSep )
             aCol[ TBCI_SEPWIDTH  ] := len( aCol[ TBCI_COLSEP ] )
             aCol[ TBCI_DEFCOLOR  ] := DefColorOK( ::cColorSpec, aCol[ TBCI_OBJ ]:DefColor )

             ASize( aCol[ TBCI_DEFCOLOR ], 4 )

             DEFAULT aCol[ TBCI_DEFCOLOR,1 ] TO 1
             DEFAULT aCol[ TBCI_DEFCOLOR,2 ] TO 2
             DEFAULT aCol[ TBCI_DEFCOLOR,3 ] TO 1
             DEFAULT aCol[ TBCI_DEFCOLOR,4 ] TO 1

             if aCol[ TBCI_TYPE ] == 'D' .and. empty( aCol[ TBCI_PICT ] )
                aCol[ TBCI_PICT ] := '@D'
             endif

             aCol[ TBCI_LCOLSEP ] := aCol[ TBCI_WIDTH ] > 0

          endif

          if nMode = 0 .or. nMode = 2 
             aCol[ TBCI_COLSEP ] := iif( aCol[ TBCI_OBJ ]:ColSep != NIL, aCol[ TBCI_OBJ ]:ColSep, ::ColSep )
          endif

          if nMode < 2 .or. ::lNeverDisplayed
             // Are there column headers/footers/separators to paint ?
             if ! Empty( aCol[ TBCI_HEADING ] )
                ::lHeaders := .T.
             endif
             if ! Empty( aCol[ TBCI_FOOTING ] )
                ::lFooters := .T.
             endif
             // as soon as we find one, we stop testing aCol[TBCI_OBJ]:XX to speed things up 
             if ! ::lColHeadSep .AND. ! Empty( aCol[ TBCI_OBJ ]:HeadSep )
                ::lColHeadSep := .T.
             endif
             if ! ::lColFootSep .AND. ! Empty( aCol[ TBCI_OBJ ]:FootSep )
               ::lColFootSep := .T.
             endif
          endif

          if ::lHeaders .AND. !Empty( aCol[ TBCI_HEADING ] )
             nHeight := Len( aCol[ TBCI_HEADING ] ) - Len( StrTran( aCol[ TBCI_HEADING ], ";" ) ) + 1

             if nHeight > ::nHeaderHeight
               ::nHeaderHeight := nHeight
             endif

          endif

          if ::lFooters .AND. !Empty( aCol[ TBCI_FOOTING ] )
             nHeight := Len( aCol[ TBCI_FOOTING ] ) - Len( StrTran( aCol[ TBCI_FOOTING ], ";" ) ) + 1

             if nHeight > ::nFooterHeight
                ::nFooterHeight := nHeight
             endif

          endif

      NEXT

      if Empty( ::aColorSpec )
        ::aColorSpec := Color2Array( ::cColorSpec )
      endif

      // Reduce footer, headers and separator if the data not fit in the
      // visible area. If it didn't fit, it generate error.
      While .t. 

         ::nVisWidth := ::nwRight - ::nwLeft + 1

         // If I add (or remove) header or footer (separator) I have to
         // change number of available rows
         ::nRowCount := ::nwBottom - ::nwTop + 1 - ;
                        iif( ::lHeaders, ::nHeaderHeight, 0 ) - ;
                        iif( ::lFooters, ::nFooterHeight, 0 ) - ;
                        iif( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ) - ;
                        iif( ::lFootSep .OR. ::lColFootSep, 1, 0 )

         if ::nRowCount <= 0

            if ::lFooters
               ::nFooterHeight--
               if ::nFooterHeight == 0
                  ::lFooters := .f.
               endif
               loop
            endif

            if ::lHeaders
               ::nHeaderHeight--
               if ::nHeaderHeight == 0
                  ::lHeaders := .f.
               endif
               loop
            endif

            if ::lFootSep
               ::lFootSep := .f.
               loop
            endif

            if ::lHeadSep
               ::lHeadSep := .f.
               loop
            endif

         endif

         exit

      enddo

   endif

   if nMode = 1
      Return Self
   endif

   // Starting position of data rows excluding headers . Pritpal Bedi
   ::nRowData := ::nwTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + ;
                           iif( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ) - 1

   if Len( ::aRedraw ) != ::nRowCount .and. ::nRowCount > 0 
      ::aRedraw := Array( ::nRowCount )
      // I need a cache of different size
      ::oCache:InitCache()
      for each aRect in ::aRect
          ::oCache:SetColorRect( aRect )
      next
      ::aRect := {}
   endif

   ::Invalidate()

   // Force re-evaluation of space occupied by frozen columns
   nFreeze := ::nFrozenCols
   if nFreeze > 0
      ::SetFrozenCols( 0 )
   endif
   ::SetFrozenCols( nFreeze, .t. )

#ifdef HB_COMPAT_C53
   ::mRect := { ::nwTop + ::nHeaderHeight + if( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ), ::nwLeft, ::nwBottom - ::nFooterHeight - if( ::lFootSep .OR. ::lColFootSep, 1, 0 ), ::nwRight }

   ASize( ::aVisibleCols, ::nwRight - ::nwLeft + 1 )
   n := ::nwLeft - 1
   for each xVal in ::aVisibleCols
      xVal := HB_EnumIndex() + n
   next
#endif

 // Flag that browser has been configured properly
 ::lConfigured := .t.

Return Self

*------------------------------------------------------*
METHOD ColInfo( oCol,lAdd ) CLASS Tbrowse
*------------------------------------------------------*
LOCAL aCol

   DEFAULT lAdd TO .f.

   if ! lAdd .and. hb_IsObject( oCol ) .and. hb_IsBlock( oCol:block )
      aCol := { oCol, valtype( Eval( oCol:block )), ::SetColumnWidth( oCol ),;
                '', '', '', 0, '', 0, oCol:DefColor, .f., .t., 0 }
   else
      aCol := { oCol, '', 0, '', '', '', 0, '', 0, {}, .f., .t., 0 }
   endif

Return aCol

*------------------------------------------------------*
METHOD AddColumn( oCol ) CLASS TBrowse
*------------------------------------------------------*

#ifndef HB_EXTENSION
   // Clipper's TBrowse allow max 64 columns.
   if ::nColCount >= 64
      Return Self
   endif
#endif

   ::Moved()
   AAdd( ::aColsInfo, ::ColInfo( oCol,.t. ) )
   ::nColCount++

   if ::nColCount == 1
      ::nLeftVisible := 1
      ::nColPos := 1
   endif

   if ! ::lNeverDisplayed .or. ::nColCount == 1
      ::Configure( 1 )
   endif
 
   ::lConfigured := .f.

Return Self

*------------------------------------------------------*
METHOD InsColumn( nPos, oCol ) CLASS TBrowse
*------------------------------------------------------*

#ifndef HB_EXTENSION
   // Clipper's TBrowse allow max 64 columns.
   if ::nColCount >= 64
      Return NIL
   endif
#endif

   if 0 < nPos
      ::Moved()

      if nPos > ::nColCount
         aAdd( ::aColsInfo, ::ColInfo( oCol ) )
      else
         aIns( ::aColsInfo, nPos, ::ColInfo( oCol ), .t. )
      endif

      ::nColCount++

      if ! ::lNeverDisplayed
         ::Configure( 1 )
      endif
      ::lConfigured := .f.
   endif

Return oCol

*------------------------------------------------------*
METHOD SetColumn( nColumn, oCol ) CLASS TBrowse
*------------------------------------------------------*
LOCAL oOldCol

   if 0 < nColumn .AND. nColumn <= ::nColCount
      ::Moved()

      oOldCol := ::aColsInfo[ nColumn, TBCI_OBJ ]

      ::aColsInfo[ nColumn ] := ::ColInfo( oCol )

      if ! ::lNeverDisplayed
         ::Configure( 1 )
      endif
      ::lConfigured := .f.
   endif

Return oOldCol

*------------------------------------------------------*
METHOD GetColumn( nColumn ) CLASS TBrowse
*------------------------------------------------------*
Return iif( 0 < nColumn .AND. nColumn <= ::nColCount, ::aColsInfo[ nColumn, TBCI_OBJ ], NIL )

*------------------------------------------------------*
METHOD DelColumn( nPos ) CLASS TBrowse
*------------------------------------------------------*
LOCAL oCol

   if nPos > ::nColCount .or. nPos < 1
      // For clipper compatibility we must call Errorsys with error 1132
      Throw( ErrorNew( "BASE", 0, 1132, , "Bound error: array access" ) )
      return NIL
   endif

   if ::nPrevDelColPos = 0
      ::nPrevDelColPos := ::nColPos
   endif

   //  Need to adjust variables in case last column is deleted
   ::Moved()

   oCol := ::aColsInfo[ nPos, TBCI_OBJ ]

   if nPos == ::nColPos .or. nPos == ::nColCount .or.;
              ::nColPos == ::nColCount .or. ::nRightVisible == ::nColCount

      if ::nLeftVisible == ::nRightVisible .AND. ::nLeftVisible > 1
         ::nLeftVisible--
      endif
      ::nRightVisible--

      if ::nColPos == ::nColCount
         ::nColpos--
      endif
   endif

   ::nColCount--

   aDel( ::aColsInfo, nPos, .t. )
   ::nColCount := Len( ::aColsInfo )

   if ::nColCount < ::nFrozenCols
      ::nFrozenCols := 0
   endif

   if ::nColCount == 0
      ::lNeverDisplayed := .t.
      ::aRedraw := {}
   endif

   if ! ::lNeverDisplayed
      ::Configure( 1 )
   endif
   ::lConfigured := .f.

Return oCol

*------------------------------------------------------*
METHOD ColWidth( nColumn ) CLASS TBrowse
*------------------------------------------------------*
Return iif( 0 < nColumn .AND. nColumn <= ::nColCount, ::aColsInfo[ nColumn, TBCI_WIDTH ], NIL )

*------------------------------------------------------*
METHOD SetFrozenCols( nHowMany, lLeft ) CLASS TBrowse
*------------------------------------------------------*
LOCAL nCol, aCol, nOldFreeze, nOldFrozenWidth

   Default lLeft to .f.

   nOldFreeze      := ::nFrozenCols
   nOldFrozenWidth := ::nFrozenWidth

   ::nFrozenCols  := Min( nHowMany, ::nColCount )

   // Space inside TBrowse window reserved for frozen columns
   ::nFrozenWidth := 0

   if ! ::lNeverDisplayed

      if nHowMany > 0
         for each aCol in ::aColsInfo
            nCol := HB_EnumIndex()
            if nCol <= nHowMany
               ::nFrozenWidth += aCol[ TBCI_WIDTH ]
               if nCol < ::nColCount .and. aCol[ TBCI_WIDTH ] > 0
                  ::nFrozenWidth += ::aColsInfo[ nCol + 1, TBCI_SEPWIDTH ]
               endif
            else
               exit
            endif
         next
      endif

      if ::nFrozenWidth >= ::nVisWidth
         ::nFrozenWidth := 0
         ::nFrozenCols  := 0
         nHowMany       := 0
         AFill( ::aRedraw, .t. )
         ::oCache:Invalidate()
         ::lStable := .F.
      endif

      if ( ::nFrozenCols < nOldFreeze .or. ::nColPos <= ::nFrozenCols .or.;
           ::nFrozenWidth < nOldFrozenWidth ) .and. ::nFrozenCols > 0
         FOR EACH aCol IN ::aColsInfo
            // Reset column widths
            aCol[ TBCI_WIDTH     ] := ::SetColumnWidth( aCol[ TBCI_OBJ ] )
            aCol[ TBCI_WIDTHCELL ] := Min( aCol[ TBCI_WIDTH ], LenVal( Eval( aCol[ TBCI_OBJ ]:block ), aCol[ TBCI_TYPE ], aCol[ TBCI_OBJ ]:Picture ) )
            aCol[ TBCI_SETWIDTH  ] := .f.
         NEXT
      endif

      FOR EACH aCol IN ::aColsInfo
         if HB_EnumIndex() > ::nFrozenCols
            if ::nFrozenCols > 0
               // If there are columns which are larger than TBrowse display width minus
               // frozen columns reserved space, shrihnk them to fit
               if ::nFrozenWidth + aCol[ TBCI_WIDTH ] > ::nVisWidth
                  aCol[ TBCI_WIDTH     ] := ::nVisWidth - ::nFrozenWidth
                  aCol[ TBCI_WIDTHCELL ] := Min( aCol[ TBCI_WIDTH ], LenVal( Eval( aCol[ TBCI_OBJ ]:block ), aCol[ TBCI_TYPE ], aCol[ TBCI_OBJ ]:Picture ) )
                  aCol[ TBCI_SETWIDTH  ] := .t.
               endif

            else
               // Reset column widths
               aCol[ TBCI_WIDTH     ] := ::SetColumnWidth( aCol[ TBCI_OBJ ] )
               aCol[ TBCI_WIDTHCELL ] := Min( aCol[ TBCI_WIDTH ], LenVal( Eval( aCol[ TBCI_OBJ ]:block ), aCol[ TBCI_TYPE ], aCol[ TBCI_OBJ ]:Picture ) )
               aCol[ TBCI_SETWIDTH  ] := .f.
            endif
         endif
      NEXT

      if lLeft
         if ::nFrozenCols > 0
            if ::nColPos <= ::nFrozenCols
               do while .t.
                  ::nLeftVisible := ::LeftDetermine()

                  if ::nLeftVisible == ::nFrozenCols + 1 .or. ::nColCount == ::nFrozenCols
                     exit
                  endif

                  if ::nRightVisible > ::nFrozenCols .and. ::nLeftVisible > ::nFrozenCols + 1
                     ::nRightVisible--
                  else
                     ::nRightVisible++
                  endif
               enddo
            else
               do while .t.
                  ::nLeftVisible := ::LeftDetermine()

                  if ::nColPos >= ::nLeftVisible .and. ::nColPos <= ::nRightVisible
                     exit
                  endif

                  if ::nColPos < ::nRightVisible
                     ::nRightVisible--
                  else
                     ::nRightVisible++
                  endif
               enddo
            endif
         else
            do while .t.
               ::nLeftVisible := ::LeftDetermine()

               if ::nColPos >= ::nLeftVisible
                  exit
               endif

               ::nRightVisible--
            enddo
         endif
      endif
   endif

Return nHowMany

*------------------------------------------------------*
METHOD SetColumnWidth( oCol ) CLASS TBrowse
*------------------------------------------------------*
LOCAL xRes, cType, nTokenPos := 0, nL, cHeading, cFooting
LOCAL nWidth := 0, nHeadWidth := 0, nFootWidth := 0, nLen := 0

   // if oCol has :Width property set I use it
   if oCol:Width <> nil
      nWidth := Min( oCol:Width, ::nVisWidth )

   else
      if ISBLOCK( oCol:block )

         xRes     := Eval( oCol:block )
         cType    := Valtype( xRes )

         nLen := LenVal( xRes, cType, oCol:Picture )

         cHeading := oCol:Heading + ";"
         while ( nL := Len( __StrTkPtr( @cHeading, @nTokenPos, ";" ) ) ) > 0
            nHeadWidth := Max( nHeadWidth, nL )
         enddo

         nTokenPos := 0

         cFooting  := oCol:Footing + ";"
         while ( nL := Len( __StrTkPtr( @cFooting, @nTokenPos, ";" ) ) ) > 0
            nFootWidth := Max( nFootWidth, nL )
         enddo

      endif

      nWidth := Min( Max( nHeadWidth, Max( nFootWidth, nLen ) ), ::nVisWidth )

   endif

Return nWidth

*------------------------------------------------------*
METHOD MoveTo( nMove ) CLASS Tbrowse
*------------------------------------------------------*
Local nTop, leftVis, lForceResetHits, nSkipped

 if ::lStable
    // reset movements only after stabilize.
    ::ResetMove()
 else

    if ::nMoveTo != 0 .and. ::nMoveTo != nMove
       ::forceStable()
    endif

    // configure on any attempt to move before stabilize.
    ::PerformConfiguration()

 endif

 nSkipped := ::nRowsSkipped
 lForceResetHits := ( nMove = TBM_TOP .or. nMove = TBM_BOTTOM )
 ::Moved(,lForceResetHits )

 SWITCH nMove 

   CASE TBM_UP
      ::nRowsToSkip --
      EXIT

   CASE TBM_DOWN
      ::nRowsToSkip ++
      EXIT

   CASE TBM_PGUP
      ::nRowsToSkip := - ( ( ::nRowPos - 1 ) + ::nRowCount )
      EXIT

   CASE TBM_PGDN
      ::nRowsToSkip := ( ::nRowCount - ::nRowPos ) + ::nRowCount 
      ::lPaintBottomUp := .f.
      EXIT

   CASE TBM_TOP
      ::oCache:GoTop()
      ::nRowsSkipped := 0
      ::nRowsToSkip := Max( 1, ::nRowPos - 1 )  
      EXIT

   CASE TBM_BOTTOM
      nTop := ::oCache:GoBottom()
      ::nRowsSkipped := nTop
      ::nRowsToSkip := Min( ::nRowPos + nTop - 1, ::nRowCount - 1 )
      EXIT

   CASE TBM_RIGHT
      if ::nColPos < ::nRightVisible
         ::nColPos++
      else
         if ::nColPos < ::nColCount
            ::nRightVisible++
            ::nLeftVisible := ::LeftDetermine()
            ::nColPos++
            ::Invalidate()
         else
            ::nColPos++
         endif

      endif
      EXIT

   CASE TBM_LEFT
      if ::nColPos > ::nLeftVisible .or. ( ::nColPos <= ::nFrozenCols + 1 .and. ::nColPos > 1 )
         ::nColPos--
      else
         if ::nColPos <= Max( ::nLeftVisible, ::nFrozenCols ) .AND. ::nColPos > 1
            leftVis := ::nLeftVisible
            While leftVis == ::nLeftVisible
               ::nRightVisible--
               ::nLeftVisible := ::LeftDetermine()
            Enddo
            ::nColPos--
            ::Invalidate()
         else
            // Can go "out of bounds", here we behave like clipper
            ::nColPos--
         endif
      endif
      EXIT

   CASE TBM_HOME
     if ::nColPos <> ::nLeftVisible
        ::nColPos := ::nLeftVisible
        ::Invalidate()
     endif
     EXIT

   CASE TBM_END
      if ::nColPos < ::nRightVisible
         ::nColPos := ::nRightVisible
         ::Invalidate()
      else
         // Can go "out of bounds", here we behave like clipper
         ::nColPos := ::nRightVisible
      endif
      EXIT

   CASE TBM_PANHOME
      if ::nColPos > 1
         if ::nLeftVisible > ::nFrozenCols + 1
            ::nLeftVisible  := ::nFrozenCols + 1
            ::nColPos := 1
            ::Invalidate()
         else
            ::nColPos := 1
            ::Refresh(.F.)
         endif
      else
         // Can go "out of bounds", here we behave like clipper
         ::nColPos := 1
      endif
      EXIT

   CASE TBM_PANEND
      if ::nColPos < ::nColCount
         if ::nRightVisible <  ::nColCount
            ::nRightVisible := ::nColCount
            ::nLeftVisible := ::LeftDetermine()
            ::nColPos := ::nRightVisible
            ::Invalidate()
         else
            ::nColPos := ::nRightVisible
            ::Refresh(.F.)
         endif
      else
         // Can go "out of bounds", here we behave like clipper
         ::nColPos := ::nColCount
      endif
      EXIT

   CASE TBM_PANRIGHT
      if ::nRightVisible < ::nColCount

         leftVis := ::nLeftVisible

         While leftVis == ::nLeftVisible
         
           ::nRightVisible++
           ::nLeftVisible  := ::LeftDetermine()

         Enddo

         ::nColPos := Max( ::nColPos, ::nLeftVisible )
         ::Invalidate()
      endif
      EXIT

   CASE TBM_PANLEFT
      // The same as if ::nLeftVisible > iif(::nFrozenCols > 0, ::nFrozenCols + 1, 1)
      if ::nLeftVisible > ::nFrozenCols + 1

         leftVis := ::nLeftVisible

         // While space left available by columns exiting to the right side of tbrowse
         // is not enough to contain a new column to the left (::nLeftVisible doesn't change)
         While leftVis == ::nLeftVisible

           ::nRightVisible--
           ::nLeftVisible := ::LeftDetermine()

         Enddo

         // Since panel "shifts" to the right, ::ncolPos could end up "out of" the
         // right side of tbrowse, so, change it to ::nRightVisible if this happens
         ::nColPos := Min( ::nColPos, ::nRightVisible )
         ::Invalidate()
      endif
      EXIT
 END

 // if browse was moved before stabilize.
 if nSkipped != 0
    ::nRowsSkipped := ::nRowsToSkip
 endif

 ::nMoveTo := nMove

Return SELF

*------------------------------------------------------*
METHOD SkipRows() CLASS Tbrowse
*------------------------------------------------------*

 if ::nMoveTo != TBM_TOP .and. ::nMoveTo != TBM_BOTTOM 

    if ::nMoveTo == TBM_UP .and. ::nRowPos = 1
       ::aRedraw[ ::nRowPos ] := .T.
       ::oCache:Invalidate(::nRowPos)
       ::DrawRow( ::nRowPos )

    elseif ::nMoveTo == TBM_DOWN .and. ::nRowPos = Min(::nRowCount, ::oCache:LastRow)
       ::aRedraw[ ::nRowPos ] := .T.
       ::oCache:Invalidate(::nRowPos)
       ::DrawRow( ::nRowPos )
    endif

    ::nRowsSkipped := ::EvalSkipBlock( ::nRowsToSkip )

 endif

RETURN SELF

*------------------------------------------------------*
METHOD Moved(lResetHits, lForceHitsFalse) CLASS TBrowse
*------------------------------------------------------*

// Forcing Reset of ::HitTop/Bottom flags!  Clipper Compatible
Default lResetHits      to .t.
Default lForceHitsFalse to .f.

   // Internal flags used to set ::HitTop/Bottom during next stabilization
   If lResetHits
      ::lHitTop    := .F.
      ::lHitBottom := .F.
      ::lForceHitsFalse := lForceHitsFalse
   EndIf

   // No need to Dehilite() current cell more than once
   if ::lStable
      if ::lAutoLite
         ::DeHilite()
      else
         ::PosCursor()
      endif
      ::lStable := .F.
   endif

RETURN Self

*------------------------------------------------------*
METHOD ResetMove() CLASS TBrowse
*------------------------------------------------------*

  ::nRowsToSkip := 0
  ::nRowsSkipped := 0
  ::nMoveTo := 0

Return Self

*------------------------------------------------------*
METHOD SetColPos( nColPos ) CLASS TBrowse
*------------------------------------------------------*

   default nColPos to 0

   if ::lNeverDisplayed .OR. (! ::lStable .AND. nColPos <= ::nRightVisible .AND. nColPos >= ::nLeftVisible)
      return ::nColPos := nColPos

   elseif nColPos <> 0 .AND. nColPos <> ::nColPos

      ::Moved()

      // I'm still inside columns currently shown
      if nColPos <= ::nRightVisible  .AND.;
         nColPos >= Max( ::nLeftVisible, ::nFrozenCols )

         ::nColPos := nColpos

      else
         // moving to the right
         if nColPos > ::nColPos

            if nColPos <= ::nColCount

               ::nRightVisible := nColPos
               ::nLeftVisible := ::LeftDetermine()
               ::nColPos := nColPos

            endif

         // moving to the left
         else

            if nColPos >= 1

               // Until the leftmost column has a number higher than the column I want to reach
               while ::nLeftVisible > nColPos

                  ::nRightVisible--
                  ::nLeftVisible := ::LeftDetermine()

               enddo

               ::nColPos := nColPos

            endif

         endif

         ::Invalidate()

      endif

      ::ForceStable()

   endif

Return ::nColPos

*------------------------------------------------------*
METHOD LeftDetermine() CLASS TBrowse
*------------------------------------------------------*
LOCAL nWidth := ::nFrozenWidth
LOCAL nCol   := ::nRightVisible

   // If ::nFrozenCols > 0 I don't need to test nCol > 0, if 0 it is the same test
   while nCol > ::nFrozenCols .AND.;
         ( nWidth += ::aColsInfo[ nCol, TBCI_WIDTH ] + iif( nCol == ::nFrozenCols + 1, 0, ::aColsInfo[ nCol,TBCI_SEPWIDTH ] ) )<= ::nVisWidth
      nCol--
   enddo

   // Clipper compatible: do not let nCol stop at empty column 
   nCol++
   while nCol <= ::nRightVisible .and. ::aColsInfo[ nCol, TBCI_WIDTH ] == 0
      nCol++
   enddo

Return Min(nCol, ::nRightVisible)

*------------------------------------------------------*
METHOD HowManyCol() CLASS TBrowse
*------------------------------------------------------*
LOCAL colPos, nToAdd, nColsVisible, nColsWidth
LOCAL nLeftCol, tryLeftVisible, saveColsWidth 

   colPos       := ::nColPos
   nToAdd       := 0
   nColsWidth   := 0
   nColsVisible := 0

   if ::nFrozenCols > 0

      While nColsVisible < ::nFrozenCols .and. nColsVisible < ::nColCount

         nToAdd := ::aColsInfo[ nColsVisible + 1, TBCI_WIDTH ]

         if nColsVisible >= 1 .and. nColsVisible < ::nColCount .and.;
            ::aColsInfo[ nColsVisible,TBCI_WIDTH ] > 0
            nToAdd += ::aColsInfo[ nColsVisible + 1, TBCI_SEPWIDTH ]
         endif

         if nColsWidth + nToAdd > ::nVisWidth
            exit
         endif

         nColsWidth += nToAdd
         nColsVisible++

      Enddo

      if nColsWidth + nToAdd > ::nVisWidth .and. nColsVisible < ::nFrozenCols
         // NOTE: Why do I change frozen columns here?
         ::Freeze        := 0
         ::nColsWidth    := 0
         ::nRightVisible := nColsVisible
         ::nColsVisible  := nColsVisible
         return Self

      endif

      if ::nLeftVisible <= ::nFrozenCols
         ::nLeftVisible := ::nFrozenCols + 1
      endif

   endif

   saveColsWidth  := nColsWidth
   tryLeftVisible := ::nLeftVisible

   // ::nColPos is to the left of leftVisible
   if ::nFrozenCols == 0 .and. tryLeftVisible > ::nColPos
      tryLeftVisible := ::nColPos
   endif

   while .t.

      nColsVisible := Max( 0, tryLeftVisible - 1 )

      while nColsVisible < ::nColCount
         // which column is displayed to the left of next col?
         if ::nFrozenCols > 0 .and. nColsVisible+1==tryLeftVisible
            nLeftCol := ::nFrozenCols
         else
            nLeftCol := nColsVisible
         endif

         nToAdd := ::aColsInfo[ nColsVisible + 1, TBCI_WIDTH ]

         // next, we must check against [nLeftCol], not [nColsVisible]:
         if ( nColsVisible >= tryLeftVisible .or. ::nFrozenCols > 0 ) .and.;
            (nLeftCol > 0) .and.;
            ::aColsInfo[ nLeftCol,TBCI_WIDTH ] > 0

            nToAdd += ::aColsInfo[ nColsVisible + 1, TBCI_SEPWIDTH ]
         endif

         if nColsWidth + nToAdd > ::nVisWidth
            exit
         endif

         nColsWidth += nToAdd
         nColsVisible++

      enddo

      // check: is ::nColPos fit within these calculated cols?
      if ::nColPos <= ::nFrozenCols .or.;
         (tryLeftVisible <= ::nColPos .and. ::nColPos <= nColsVisible)
         exit
      endif

      // retry
      tryLeftVisible++
      nColsWidth := saveColsWidth

   enddo 

   ::nLeftVisible  := Max( 1, tryLeftVisible )
   ::nRightVisible := Max( 1, nColsVisible )
   ::nColsVisible  := Max( 1, nColsVisible )
   ::nColsWidth    := nColsWidth
   ::nSpacePre     := Int( ( ::nVisWidth - ::nColsWidth ) / 2 ) 
   ::nSpaceLast    := ::nVisWidth - ::nSpacePre - ::nColsWidth 

Return Self

*----------------------------------------*
METHOD RedrawHeaders() CLASS TBrowse
*----------------------------------------*
LOCAL n, nTPos, nBPos, cBlankBox, nScreenRowT, nScreenRowB
LOCAL nLCS, nScreenCol, nColFrom, chSep, cfSep, nLeftCol, ccSep, ncSepWidth
LOCAL lDrawHeaders, lDrawHeadSep, lDrawFootSep, lDrawFooters, nCursor

   DispBegin()

   cBlankBox    := space( 9 )
   lDrawHeaders := ::lHeaders
   lDrawHeadSep := ( ::lHeadSep .OR. ::lColHeadSep )
   lDrawFootSep := ( ::lFootSep .OR. ::lColFootSep )
   lDrawFooters := ::lFooters

   nScreenCol := ::nwLeft + iif( ::nFrozenCols > 0, 0, ::nSpacePre )
   nColFrom := if( ::nFrozenCols > 0, 1, ::nLeftVisible )
   ::aColumnsSep := {}

   for n := nColFrom to ::nRightVisible

      ::aColsInfo[ n, TBCI_SCRCOLPOS ] := nScreenCol
      nScreenCol += ::aColsInfo[ n, TBCI_WIDTH ]

      if n < ::nRightVisible
         if ::aColsInfo[ n,TBCI_WIDTH ] > 0
            aadd( ::aColumnsSep, nScreenCol + int( ::aColsInfo[ n + 1, TBCI_SEPWIDTH ] / 2 ) )
            nScreenCol += ::aColsInfo[ n + 1, TBCI_SEPWIDTH ]
         endif
      endif

      if ::nFrozenCols > 0 .and. n == ::nFrozenCols
         n := ::nLeftVisible - 1
         nScreenCol += ::nSpacePre
      endif
   next

   if lDrawHeaders          // Drawing headers
      // Clear area of screen occupied by headers
      DispBox( ::nwTop, ::nwLeft, ::nwTop + ::nHeaderHeight - 1, ::nwRight, cBlankBox, ::cColorSpec )

      for n := nColFrom to ::nRightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::nLeftVisible
         endif

         nCursor := SetCursor(SC_NONE)
         SetPos( ::nwTop, ::aColsInfo[ n, TBCI_SCRCOLPOS ] )
         SetCursor(nCursor)

         ::WriteMLineText( ::aColsInfo[ n, TBCI_HEADING ], ;
                           ::aColsInfo[ n, TBCI_WIDTH ], .T., ;
                           hb_ColorIndex( ::cColorSpec,  ( DefColorOK( ::cColorSpec, ::aColsInfo[ n,TBCI_OBJ ]:DefColor)[ TBC_CLR_HEADING ] - 1) ))
      next
   endif

   if lDrawHeadSep    //Draw horizontal heading separator line
      nScreenRowT := ::nRowData
   endif

   if lDrawFootSep    //Draw horizontal footing separator line
      nScreenRowB := ::nwBottom - iif( ::lFooters, ::nFooterHeight, 0 )
   endif

   nTPos := nBPos := ::nwLeft

   chSep := ::HeadSep  // default HeadSep
   cfSep := ::FootSep  // default FootSep

   // Draw heading/footing column separator
   for n := iif( ::nFrozenCols > 0, 1, ::nLeftVisible ) to ::nRightVisible

      // colsep's width will be needed later
      ccSep := if( ::aColsInfo[ n,TBCI_OBJ ]:ColSep == nil, ::ColSep, ;
                              ::aColsInfo[ n,TBCI_OBJ ]:ColSep )

      ncSepWidth := if( ccSep == nil, 0, len(ccSep) )

      // which column is displayed to the left of current col?
      if ::nFrozenCols > 0 .and. n == ::nLeftVisible
         nLeftCol := ::nFrozenCols
      else
         nLeftCol := n - 1
      endif

      if (::nFrozenCols > 0  .and. n == ::nFrozenCols + 1) .or.;
         (::nFrozenCols == 0 .and. n == ::nLeftVisible )
         n := ::nLeftVisible

         IF lDrawHeadSep
            // we need to draw headSep for the ::nSpacePre gap
            if ! Empty( chSep := if( ::aColsInfo[ n,TBCI_OBJ ]:HeadSep == nil, ::HeadSep, ;
                                     ::aColsInfo[ n,TBCI_OBJ ]:HeadSep ) )
               if nLeftCol > 0 .and. ::aColsInfo[ nLeftCol, TBCI_WIDTH ] > 0 .and.;
                  ::nFrozenCols > 0
                  DispOutAT( nScreenRowT, nTPos - min( len(chSep), ncSepWidth), chSep, ::cColorSpec )
               endif
               DispOutAT( nScreenRowT, nTPos, Replicate( Right( chSep, 1 ), ::nSpacePre ), ::cColorSpec )

            elseif ::lColHeadSep
               DispOutAT( nScreenRowT, nTPos, Replicate( Space(1), ::nSpacePre ), ::cColorSpec )

            endif
            nTPos += ::nSpacePre
         ENDIF

         // we need to draw footSep for the ::nSpacePre gap
         IF lDrawFootSep
            if ! Empty ( cfSep := if( ::aColsInfo[ n,TBCI_OBJ ]:FootSep == nil, ::FootSep, ;
                                      ::aColsInfo[ n,TBCI_OBJ ]:FootSep ) )

               if nLeftCol > 0 .and. ::aColsInfo[ nLeftCol, TBCI_WIDTH ] > 0 .and. ;
                  ::nFrozenCols > 0
                  DispOutAT( nScreenRowB, nBPos - min( len(cfSep), ncSepWidth), cfSep, ::cColorSpec )
               endif
               DispOutAT( nScreenRowB, nBPos, Replicate( Right( cfSep, 1 ), ::nSpacePre ), ::cColorSpec )

            elseif ::lColFootSep
               DispOutAT( nScreenRowB, nBPos, Replicate( Space(1), ::nSpacePre ), ::cColorSpec )

            endif
            nBPos += ::nSpacePre
         ENDIF

      endif

      // we need to handle even n == ::nRightVisible in the following block
      if ::aColsInfo[ n, TBCI_WIDTH ] > 0 .and. n < ::nRightVisible
         nLCS := ::aColsInfo[ n + 1, TBCI_SEPWIDTH ]
      else
         nLCS := 0
      endif

      IF lDrawHeadSep
         if ! Empty( chSep := if( ::aColsInfo[ n,TBCI_OBJ ]:HeadSep == nil, ::HeadSep, ;
                                  ::aColsInfo[ n,TBCI_OBJ ]:HeadSep ) )

            if nLeftCol>0 .and. n <> ::nLeftVisible .and. ::aColsInfo[ nLeftCol, TBCI_WIDTH ] > 0
               DispOutAT( nScreenRowT, nTPos - min( len(chSep), ncSepWidth), chSep, ::cColorSpec )
            endif
            DispOutAT( nScreenRowT, nTPos, Replicate( Right( chSep, 1 ), ::aColsInfo[ n, TBCI_WIDTH ] ), ::cColorSpec )

            nTPos += ::aColsInfo[ n, TBCI_WIDTH ] + nLCS

         // If I haven't got a default separator or a colsep for current column, there could
         //   be a colsep on a next column, so I have to fill the width of this column with spaces.
         elseif ::lColHeadSep
            DispOutAT( nScreenRowT, nTPos, Replicate( Space(1), ::aColsInfo[ n, TBCI_WIDTH ] + nLCS ), ::cColorSpec )
            nTPos += ::aColsInfo[ n, TBCI_WIDTH ] + nLCS

         endif
      ENDIF

      IF lDrawFootSep
         if ! Empty( cfSep := if( ::aColsInfo[ n,TBCI_OBJ ]:FootSep == nil, ::FootSep, ;
                                  ::aColsInfo[ n,TBCI_OBJ ]:FootSep ) )

            if Valtype(chSep) <> "U" .and. len(chSep) > len(cfSep)
               cfSep += Replicate( Right( cfSep, 1 ), Len( chSep ) - Len( cfSep ) )
            endif

            if nLeftCol > 0 .and. n <> ::nLeftVisible .and. ::aColsInfo[ nLeftCol, TBCI_WIDTH ] > 0
               DispOutAT( nScreenRowB, nBPos - min( len(cfSep), ncSepWidth), cfSep, ::cColorSpec )
            endif
            DispOutAT( nScreenRowB, nBPos, Replicate( Right( cfSep, 1 ), ::aColsInfo[ n, TBCI_WIDTH ] ), ::cColorSpec )

            nBPos += ::aColsInfo[ n, TBCI_WIDTH ] + nLCS

         elseif ::lColFootSep
            DispOutAT( nScreenRowB, nBPos, Replicate( Space(1), ::aColsInfo[ n, TBCI_WIDTH ] + nLCS ), ::cColorSpec )
            nBPos += ::aColsInfo[ n, TBCI_WIDTH ] + nLCS

         endif
      ENDIF

   next

   if ::nSpaceLast > 0

      if lDrawHeadSep
         // right gap of spaces on Header
         if ! Empty( chSep )
            DispOutAT( nScreenRowT, nTPos, Replicate( Right( chSep, 1 ), ::nSpaceLast ), ::cColorSpec )

         elseif ::lColHeadSep
            DispOutAT( nScreenRowT, nTPos, Replicate( Space(1), ::nSpaceLast ), ::cColorSpec )

         endif
      endif

      IF lDrawFootSep
         // right gap of spaces on Footer
         if ! Empty( cfSep )
            DispOutAT( nScreenRowB, nBPos, Replicate( Right( cfSep, 1 ), ::nSpaceLast ), ::cColorSpec )

         elseif ::lColFootSep
            DispOutAT( nScreenRowB, nBPos, Replicate( Space(1), ::nSpaceLast ), ::cColorSpec )

         endif
      ENDIF
   endif

   if lDrawFooters

      // Clear area of screen occupied by footers
      DispBox( ::nwBottom - ::nFooterHeight + 1, ::nwLeft, ::nwBottom, ::nwRight, cBlankBox, ::cColorSpec )

      for n := iif( ::nFrozenCols > 0, 1, ::nLeftVisible ) to ::nRightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::nLeftVisible
         endif

         nCursor := SetCursor(SC_NONE)
         SetPos( ::nwBottom, ::aColsInfo[ n, TBCI_SCRCOLPOS ] )
         SetCursor(nCursor)

         ::WriteMLineText( ::aColsInfo[ n, TBCI_FOOTING ], ;
                           ::aColsInfo[ n, TBCI_WIDTH ], .F., ;
                           hb_ColorIndex( ::cColorSpec, ( DefColorOK(::cColorSpec,::aColsInfo[ n, TBCI_OBJ ]:DefColor)[ TBC_CLR_FOOTING ] - 1 ) ) )
      next

   endif

   DispEnd()

Return Self

*------------------------------------------------------*
METHOD ColorRect( aRect, aColors ) CLASS TBrowse
*------------------------------------------------------*
Local nRow, aColorRect

   if ! HB_IsArray( aRect ) .or. ! HB_IsArray( aColors )
      return self
   endif

   if Len( aRect ) != 4 .or. Len( aColors ) != 2
      return self
   endif

   if ! HB_IsNumeric( aRect[TBCRECT_TOP   ] ) .or.;
      ! HB_IsNumeric( aRect[TBCRECT_LEFT  ] ) .or.;
      ! HB_IsNumeric( aRect[TBCRECT_BOTTOM] ) .or.;
      ! HB_IsNumeric( aRect[TBCRECT_RIGHT ] ) .or.;
      ! HB_IsNumeric( aColors[TBCRECT_STD ] ) .or.;
      ! HB_IsNumeric( aColors[TBCRECT_ENH ] ) 

      return self

   endif

   aColorRect := { Max( aRect[ TBCRECT_TOP    ], 1 ),;
                   Max( aRect[ TBCRECT_LEFT   ], 1 ),;
                   Min( aRect[ TBCRECT_BOTTOM ], ::nRowCount ),;
                   Min( aRect[ TBCRECT_RIGHT  ], ::nColCount ),;
                   aColors } 

   if ::lConfigured

      ::oCache:SetColorRect( aColorRect )

      // and now let's refresh new aRect covered area
      if ! Empty( ::aRedraw )
         for nRow := aRect[ TBCRECT_TOP ] to aRect[ TBCRECT_BOTTOM ]
            ::aRedraw[ nRow ] := .T.
         next
      endif

      ::lStable := .F.
      ::lRectPainting := .T.
      ::ForceStable()

   else
      
      // During ::Configure() I could change cache, so I save colorrect parameters
      AAdd( ::aRect, aColorRect )

   endif

Return Self

*------------------------------------------------------*
METHOD Stabilize( lForceStable ) CLASS TBrowse
*------------------------------------------------------*
LOCAL nOldCursor, colorSpec, nRowToDraw, cCurColor, lDispBegin

   if ::nColCount == 0
      // Return TRUE to avoid infinite loop ( do while !stabilize;end )
      return .t.
   endif

Default lForceStable TO .F.

   lDispBegin := .f.

   IF ::nPrevDelColPos > 0
      ::nColPos := Min(::nPrevDelColPos,::nColCount)
      ::nPrevDelColPos := 0
   ENDIF

   // Configure the browse if not configured yet.
   ::PerformConfiguration()

   // Skip rows, if any pending rows to skip.
   if ::nMoveTo != 0
      ::SkipRows()
   endif

   // Execute a pending invalidation of cache, if any.
   ::oCache:PerformInvalidation()

   // I need to set columns width If TBrowse was never displayed before
   if ::lNeverDisplayed
      if ! lDispBegin
         DispBegin()
         lDispBegin := .t.
      endif

      ::lNeverDisplayed := .F.
      if ::freeze > 0
         ::SetFrozenCols( ::freeze )
      endif

   endif

   ColorSpec  := ::aColorSpec[ 1 ]
   nOldCursor := SetCursor( SC_NONE )

   if ::lRedrawFrame  // Draw border
      if ! lDispBegin
         DispBegin()
         lDispBegin := .t.
      endif
      if Len( ::cBorder ) == 8
         DispBox(::nTop,::nLeft,::nBottom,::nRight,::cBorder,::colorSpec)
      endif

      cCurColor := SetColor( ColorSpec )
      Scroll(::nWTop,::nWLeft,::nWBottom,::nWRight,0)
      SetPos(0,0)
      SetColor( cCurColor )

      // How may columns fit on TBrowse width?
      ::HowManyCol()
      ::RedrawHeaders()

      // Now that browser frame has been redrawn we don't need to redraw
      // it unless displayed columns change
      ::lRedrawFrame := .F.
   endif


   if ! ::lStable

      if ! lDispBegin 
         DispBegin()
      endif

      ::SyncRows()

      While ( nRowToDraw := iif( ::lPaintBottomUp, RAScan( ::aRedraw, .T. ), AScan( ::aRedraw, .T. ) ) ) <> 0

         ::DrawRow( nRowToDraw )
          
         if ! lForceStable
            if ::lPaintBottomUp .and. nRowToDraw != ::nRowPos
               ::DrawRow( ::nRowPos )
            endif
            DispEnd()
            SetCursor( nOldCursor )
            Return .F.
         endif

      Enddo

      ::lStable := .T.
      ::lForceHitsFalse := .f.
      ::lPaintBottomUp := .F.

      DispEnd()

   endif


   if ! ::lRectPainting .AND. ::lAutoLite
      ::Hilite()
   else
      ::PosCursor()
   endif

   ::lRectPainting := .F.

   SetCursor( nOldCursor )

Return .T.

*------------------------------------------------------*
METHOD PerformConfiguration() CLASS TBrowse
*------------------------------------------------------*

   ::nColPos := Max( Min( ::nColPos, ::nColCount ), 1)

   if ! ::lConfigured .or. ::lNeverDisplayed
      if ::lNeverDisplayed
         ::Configure( 0 )
      endif
      ::Configure( 2 )
   endif

Return NIL

*------------------------------------------------------*
METHOD SyncRows() CLASS TBrowse
*------------------------------------------------------*
Local lRefreshAll, lRefreshCurrent, lResetRect, nOldRowPos, nNewRowPos
Local nSkipped

   if  ::nMoveTo != TBM_TOP .and. ::nMoveTo != TBM_BOTTOM .and.;
       ::nRowsToSkip = 0 .and. ::nRowsSkipped = 0

        if ::nRowPos > ::oCache:LastRow
           ::nNewRowPos := ::oCache:LastRow 
           ::nRowPos := ::nNewRowPos
        endif

        ::ResetMove()
        Return NIL

   endif

   lRefreshAll := .f.
   lRefreshCurrent := .f.
   lResetRect := .f.
   nOldRowPos := ::nRowPos

   if ::nMoveTo == TBM_BOTTOM
      ::nNewRowPos := Min( ::nRowCount - 1, ::nRowsSkipped ) + 1
      lRefreshAll := .t.
      lResetRect := .t.

   elseif ::nMoveTo == TBM_TOP
      ::nNewRowPos := 1
      lRefreshAll := .t.
      lResetRect := .t.

   else

      nNewRowPos := ::nRowPos + ::nRowsSkipped 

      if ::nRowsSkipped = 0

         // I've tried to move past top or bottom margin
         if ::nRowsToSkip > 0  
            ::lHitBottom := .T. .and. !::lForceHitsFalse
         elseif ::nRowsToSkip < 0
            ::lHitTop := .T. .and. !::lForceHitsFalse
         endif

         ::nNewRowPos := nNewRowPos 

      elseif ::nRowsSkipped = ::nRowsToSkip 

         // If after movement I'm still inside present TBrowse
         if ( nNewRowPos >= 1 .and. nNewRowPos <= ::nRowCount ) 

            ::nNewRowPos := Min( nNewRowPos, ::oCache:LastRow )

            if Abs(::nRowsSkipped) > 1 .and. ( ::nMoveTo == TBM_UP .or. ::nMoveTo == TBM_DOWN )
               lRefreshAll := .t.
               lResetRect := .t.
            else
               lRefreshCurrent := .t.
            endif

         else

            // It was PageDn, PageUp, Down or Up movements.
            if Abs( ::nRowsSkipped ) >= ::nRowCount // PageDn,PageUp
               lRefreshAll := .t.
            else  // Down or Up. I need scroll.
               if Abs(::nRowsSkipped) > 1
                  lRefreshAll := .t.
                  lResetRect := .t.
               endif
               ::ScrollRows( nNewRowPos )
               lRefreshCurrent := .t.
            endif

         endif

      else

         // I couldn't move as far as requested
         // I need to refresh all rows if I go past current top or bottom row
         if ( nNewRowPos < 1 .or. nNewRowPos > ::nRowCount )
            // don't go past boundaries
            ::nNewRowPos := Max(1, Min( nNewRowPos, ::oCache:LastRow ) )
            lRefreshAll := .t.
            lResetRect := .t.

         else
            ::nNewRowPos := Max(1, Min( nNewRowPos, ::oCache:LastRow ) )
            lRefreshCurrent := .t.

         endif

      endif

   endif

   // Rows are syncronized.
   ::nRowPos := Max(1, Min( ::nNewRowPos, ::oCache:LastRow ) )

   if lRefreshAll 

      AFill( ::aRedraw, .T. )
      ::oCache:Invalidate()

      if ::nMoveTo == TBM_PGUP
         nSkipped := ( nOldRowPos - 1 + ::nRowsSkipped )
         lResetRect := (nSkipped > ::nRowCount)
      elseif ::nMoveTo == TBM_PGDN 
         nSkipped := (nOldRowPos + ::nRowsSkipped - ::nRowCount) 
         lResetRect := (nSkipped > ::nRowCount)
      else // Top/Bottom
         nSkipped := ::nRowsSkipped 
      endif

      ::oCache:ResetColorRect( lResetRect, nSkipped )

      if ::nMoveTo != TBM_PGUP .and. ::nMoveTo != TBM_PGDN
         ::oCache:PerformInvalidation()
      endif

   elseif lRefreshCurrent

      ::aRedraw[ ::nRowPos ] := .T.
      ::oCache:Invalidate( ::nRowPos )

   endif

   ::ResetMove()

RETURN NIL

*------------------------------------------------------*
METHOD ScrollRows( nNewRowPos ) CLASS TBrowse
*------------------------------------------------------*
Local nTop, nBottom, nFirstRow

  // Where does really start first TBrowse row?
  nFirstRow := ::nRowData + 1

  // I'm at top or bottom of TBrowse so I can scroll
  if nNewRowPos >= ::nRowCount  
     // Down
     nTop     := nFirstRow + ::nRowsSkipped - 1
     nBottom  := nFirstRow + ::nRowCount - ::nRowsSkipped

  else
     // Up
     nTop    := nFirstRow
     nBottom := Min(::nwBottom - ::nFooterHeight, nFirstRow + ::nRowCount + ::nRowsSkipped )

  endif

  ScrollFixed( nTop, ::nwLeft, nBottom, ::nwRight, ::nRowsSkipped )
  ::oCache:ResetColorRect(.F., ::nRowsSkipped)

RETURN SELF

*------------------------------------------------------*
METHOD DrawRow( nRow ) CLASS TBrowse
*------------------------------------------------------*
LOCAL colorSpec, cColor, nColFrom, lDisplay
LOCAL nCol, nRow2Fill, nLeftColPos, cColBlanks, nCursor, xCellValue, nGap

   nGap := 0

   ColorSpec := ::aColorSpec[ 1 ]   // first pair of color into array

   nColFrom := if( ::nFrozenCols > 0, 1, ::nLeftVisible )

   xCellValue := ::oCache:GetCellValue( nRow, nColFrom )

   lDisplay := ! ( xCellValue == NIL )

   if lDisplay

      if ::nFrozenCols == 0
         DispOutAt( nRow + ::nRowData, ::nwLeft, space(::nSpacePre) , ColorSpec )

         for nCol := nColFrom to ::nRightVisible
             if nCol > nColFrom // avoid call getcellvalue twice.
                xCellValue := ::oCache:GetCellValue( nRow, nCol )
             endif
             ::DispCell( nRow, nCol, xCellValue, TBC_CLR_STANDARD )

             if nCol < ::nRightVisible .and. ::aColsInfo[ nCol, TBCI_LCOLSEP ]
                DispOut( ::aColsInfo[ nCol + 1, TBCI_COLSEP ], ColorSpec )
             endif
         next

      else 
         nCursor := SetCursor(SC_NONE)
         SetPos( nRow + ::nRowData, ::nwLeft )
         SetCursor(nCursor)

         for nCol := nColFrom to ::nRightVisible
            if nCol == ::nFrozenCols + 1
               nCol := ::nLeftVisible
               DispOut( space(::nSpacePre), ColorSpec )
            endif

            if nCol > nColFrom // avoid call getcellvalue twice.
               xCellValue := ::oCache:GetCellValue( nRow, nCol )
            endif

            ::DispCell( nRow, nCol, xCellValue, TBC_CLR_STANDARD )

            if nCol < ::nRightVisible .and. ::aColsInfo[ nCol,TBCI_LCOLSEP ]
               DispOut( ::aColsInfo[ nCol + 1, TBCI_COLSEP ], ColorSpec )
            endif
         next

      endif

      if ::nColsVisible = 1 .and. ::aColsInfo[1,TBCI_SCRCOLPOS] = ::nwLeft
         nGap := Max(0, ::nVisWidth - ::nColsWidth - ::nSpaceLast )
      endif

      DispOut( space( ::nSpaceLast + nGap ), ColorSpec )

      // Doesn't need to be redrawn. Control loop into Stabilize()
      ::aRedraw[ nRow ] := .f.

   else  // ! lDisplay

      // We paint columns wise, so we need to keep track of the screen column where
      // current tbrowse column starts.
      nLeftColPos := ::nwLeft

      for nCol := nColFrom to ::nRightVisible

         // needed here to calc correct column color
         if ::nFrozenCols > 0 .AND. nCol == ::nFrozenCols + 1
            nCol := ::nLeftVisible
         endif

         cColBlanks := Space( ::aColsInfo[ nCol, TBCI_WIDTH ] )

         // Empty rows have only standard browser color 
         cColor := hb_ColorIndex( ::cColorSpec, (( ::aColsInfo[ nCol, TBCI_DEFCOLOR ])[ TBC_CLR_STANDARD ]) - 1 )

         // Paint all remainig rows up to ::nRowCount
         for nRow2Fill := nRow to ::nRowCount

            if ::nFrozenCols == 0

               if nCol == nColFrom
                  DispOutAt( nRow2Fill + ::nRowData, nLeftColPos, space(::nSpacePre), ColorSpec )
               else
                  nCursor := SetCursor(SC_NONE)
                  SetPos(nRow2Fill + ::nRowData, nLeftColPos)
                  SetCursor(nCursor)
               endif

               DispOut( cColBlanks, cColor )

               if nCol < ::nRightVisible .and. ::aColsInfo[ nCol,TBCI_LCOLSEP ]
                  DispOut( ::aColsInfo[ nCol + 1, TBCI_COLSEP ], ColorSpec )
               endif

            else
               if nCol == ::nLeftVisible
                  DispOutAt( nRow2Fill + ::nRowData, nLeftColPos, space(::nSpacePre), ColorSpec )
               else
                  nCursor := SetCursor(SC_NONE)
                  SetPos( nRow2Fill + ::nRowData, nLeftColPos )
                  SetCursor(nCursor)
               endif

               DispOut( cColBlanks, cColor )

               if nCol < ::nRightVisible .and. ::aColsInfo[ nCol,TBCI_LCOLSEP ]
                  DispOut( ::aColsInfo[ nCol + 1, TBCI_COLSEP ], ColorSpec )
               endif
            endif

            if nCol == ::nRightVisible
               DispOut( space(::nSpaceLast), ColorSpec )
            endif

         next

         // next tbrowse column starts from this screen column
         nLeftColPos := Col()

      next

      // Mark all remaining rows as drawn
      AFill(::aRedraw, .F., nRow)

   endif 

Return Nil

*-----------------------------------------------------------*
METHOD DispCell( nRow, nCol, xValue, nColor ) CLASS TBrowse
*-----------------------------------------------------------*
LOCAL aColsInfo, oCol, nWidth, nLen, nScrColPos, nNotLeftCol
LOCAL cColor, cColorBKG, aCellColor, nCursor, nColorIndex

   aColsInfo := ::aColsInfo[ nCol ]

   // if called when the column type is not defined, then do nothing.
   if Empty( aColsInfo[ TBCI_TYPE ] )
      Return nil
   endif

   oCol       := aColsInfo[ TBCI_OBJ ]
   nWidth     := aColsInfo[ TBCI_WIDTH ]
   nLen       := aColsInfo[ TBCI_WIDTHCELL ]
   nScrColPos := aColsInfo[ TBCI_SCRCOLPOS ]

   // Fix the column screen cursor when it's out of range.
   if Col() != nScrColPos
      nCursor := SetCursor(SC_NONE)
      SetPos( nRow + ::nRowData, nScrColPos )
      SetCursor( nCursor )
   endif

   if xValue == NIL
      xValue := BlankValue( aColsInfo )
   endif

   cColorBKG := hb_ColorIndex( ::cColorSpec, ( DefColorOK(::cColorSpec,oCol:DefColor)[ TBC_CLR_STANDARD ] - 1 ) )

   aCellColor := ::oCache:GetCellColor( nRow, nCol )

   // If cell has not a particular color ( colorblock or colorrect ) use defcolor ( as clipper does )
   if Empty( aCellColor ) 
      nColorIndex := ( DefColorOK( ::cColorSpec, oCol:DefColor )[ nColor ] - 1 )
   else
      nColorIndex := ( aCellColor[ nColor ] - 1 )
   endif

   cColor := hb_ColorIndex( ::cColorSpec, nColorIndex  )

   Switch aColsInfo[ TBCI_TYPE ]
   case "C"
   case "M"
      // If there is not an explicit width use that of the first item
      if oCol:Width == NIL
         DispOut( PadR( Transform( xValue, aColsInfo[ TBCI_PICT ] ), nLen ), cColor )
         DispOut( Space( nWidth - nLen ), cColorBKG )

      else
         DispOut( PadR( Transform( xValue, aColsInfo[ TBCI_PICT ] ), nWidth ), cColor )

      endif

      exit

   case "N"
      if oCol:Width == NIL
         DispOut( Space( nWidth - nLen ), cColorBKG )
         nNotLeftCol := Col()
         DispOut( PadL( Transform( xValue, aColsInfo[ TBCI_PICT ] ), nLen ), cColor )
      else
         DispOut( PadL( Transform( xValue, aColsInfo[ TBCI_PICT ] ), nWidth ), cColor )
      endif

      exit

   case "D"
      DispOut( PadR( Transform( xValue, aColsInfo[ TBCI_PICT ] ), nLen ), cColor )
      DispOut( Space( nWidth - nLen ), cColorBKG )
      exit

   case "L"
      // Always centered inside column
      DispOut( Space( Round( ( nWidth - nLen ) / 2, 0 ) ), cColorBKG )
      nNotLeftCol := Col()
      DispOut( iif( xValue, "T", "F" ), cColor )
      DispOut( Space( Int( ( nWidth - nLen ) / 2 ) ), cColorBKG )
      exit

   default
      DispOut( Space( nWidth ), cColor )

   end

RETURN nNotLeftCol

*------------------------------------------------------*
METHOD PosCursor() CLASS TBrowse
*------------------------------------------------------*
LOCAL nScrRow, nScrCol, nCursor

   nScrRow := ::nRowPos + ::nRowData

   if ::nColPos > 0 .AND. ::nColPos <= ::nColCount

      nScrCol := ::aColsInfo[ ::nColPos, TBCI_SCRCOLPOS ]

      Switch ::aColsInfo[ ::nColPos, TBCI_TYPE ]
      case "N"
         if ::aColsInfo[ ::nColPos, TBCI_OBJ ]:Width == NIL
            nScrCol += ::aColsInfo[ ::nColPos, TBCI_WIDTH ] - ::aColsInfo[ ::nColPos, TBCI_WIDTHCELL ]
         endif
         exit

      case "L"
         // Always centered inside column
         nScrCol += Round( ( ::aColsInfo[ ::nColPos, TBCI_WIDTH ] - ::aColsInfo[ ::nColPos, TBCI_WIDTHCELL ] ) / 2, 0 )
         exit
      end

      nCursor := SetCursor(SC_NONE)
      SetPos( nScrRow, nScrCol )
      SetCursor(nCursor)

#ifdef HB_COMPAT_C53
      ::nRow := nScrRow
      ::nCol := nScrCol
#endif

   endif

Return Self

*------------------------------------------------------*
METHOD SetHilite( lHilite ) CLASS TBrowse
*------------------------------------------------------*
LOCAL nScrRow     // Screen row
LOCAL nScrCol     // Screen col
LOCAL nNotLeftCol // Screen col position of first char of not left justified columns
LOCAL xValue
Local nCursor, nColor

   Default lHilite to .f.

   if lHilite
      nColor := TBC_CLR_ENHANCED
   else
      nColor := TBC_CLR_STANDARD
   endif

   if ::nColPos > 0 .AND. ::nColPos <= ::nColCount .and. ::nRowPos > 0

      nCurSor := SetColor( SC_NONE )

      nScrRow := ::nRowPos + ::nRowData 
      nScrCol := ::aColsInfo[ ::nColPos, TBCI_SCRCOLPOS ]

      SetPos( nScrRow, nScrCol )

      xValue := ::oCache:GetCellValue( ::nRowPos, ::nColPos )

      if xValue == NIL
         xValue := BlankValue( ::aColsInfo[ ::nColPos ] )
      endif

      nNotLeftCol := ::DispCell( ::nRowPos, ::nColPos, xValue, nColor )

      SetPos( nScrRow, iif( nNotLeftCol <> NIL, nNotLeftCol, nScrCol ) )

#ifdef HB_COMPAT_C53
      ::nRow := nScrRow
      ::nCol := iif( nNotLeftCol <> NIL, nNotLeftCol, nScrCol )
#endif

      SetColor( nCursor )

   endif

Return Self

//-------------------------------------------------------------------//
//   NOTE: Not tested yet, could be broken.
//-------------------------------------------------------------------//

*------------------------------------------------------*
METHOD MGotoYX( nRow, nCol ) CLASS TBrowse
*------------------------------------------------------*
LOCAL nColsLen, nI, nNewRow

   // Am I inside TBrowse display area ?
   if nRow > ::nwTop  .AND. nRow < ::nwBottom .AND. ;
      nCol > ::nwLeft .AND. nCol < ::nwRight

      // if not stable force repositioning of data source; maybe this is not first Stabilize() call after
      // TBrowse became unstable, but we need to call Stabilize() al least one time before moving again to be sure
      // data source is under cursor position
      if ! ::lStable
         ::Stabilize()

      else
         ::Moved()

      endif

      // Set new row position
      // nNewRow := nRow - ::nwTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ), 0, 1 ) - 1
      nNewRow       := nRow - ::nRowData
      ::nRowsToSkip := nNewRow - ::nNewRowPos

      // move data source accordingly
      ::Stabilize()

      // Now move to column under nCol
      nColsLen := 0

      // NOTE: I don't think it is correct, have to look up docs
      nI := iif( ::nFrozenCols > 0, ::nFrozenCols, ::nLeftVisible )

      while nColsLen < nCol .AND. nI < ::nRightVisible

         nColsLen += ::aColsInfo[ nI, TBCI_WIDTH ]
         if nI >= 1 .AND. nI < ::nColCount
            nColsLen += ::aColsInfo[ nI+1, TBCI_SEPWIDTH ]
         endif

         nI++

      enddo

      ::nColPos := nI

      // Force redraw of current row with new cell position
      ::Refresh(.F.)

   endif

Return Self

*----------------------------------------------------------------------*
METHOD WriteMLineText( cStr, nPadLen, lHeader, cColor ) CLASS TBrowse
*----------------------------------------------------------------------*
LOCAL n, cS, nCol, nRow, nTokens, nCursor

   nCol := Col()
   nRow := Row()

   // Do I have to write an header or a footer?
   nCursor := SetCursor(SC_NONE)

   if lHeader

      // Simple case, write header as usual
      if ::nHeaderHeight == 1 .and. !( ";" IN cStr )
         DispOut( PadR( cStr, nPadLen ), cColor )

      else
         // __StrToken needs that even last token be ended with token separator
         // Headers are aligned from bottom to top - FSG - 2004/02/27
         nTokens      := __StrTokenCount( cStr, ";" )
         cStr := Replicate( ";", ::nHeaderHeight - nTokens + 1 ) + cStr
         cS := cStr

         for n := ::nHeaderHeight to 1 step -1
            SetPos( nRow + n - 1, nCol )
            DispOut( PadR( __StrToken( @cS, n, ";" ), nPadLen ), cColor )
         next

         setPos( nRow, nCol + nPadLen )

      endif

   // footer
   else

      // Simple case, write footer as usual
      if ::nFooterHeight == 1 .and. !( ";" IN cStr )
         DispOut( PadR( cStr, nPadLen ), cColor )

      else
         // __StrToken needs that even last token be ended with token separator
         cS := cStr + ";"

         for n := 0 to ( ::nFooterHeight - 1 )
            setPos( nRow - n, nCol )
            DispOut( PadR( __StrToken( @cS, ::nFooterHeight - n, ";" ), nPadLen ), cColor )
         next

         setPos( nRow, nCol + nPadLen )

      endif

   endif

   SetCursor( nCursor )

Return Self

*------------------------------------------*
METHOD SetBorder( cBorder ) CLASS TBrowse
*------------------------------------------*

   if ISCHARACTER( cBorder ) .AND.;
      ( Len( cBorder ) == 0 .or. Len( cBorder ) == 8 )

      if ::cBorder == ""
         if cBorder == ""
            // Nothing
         else
            ::cBorder := cBorder
            ::nwTop     := Min(::nwTop+1,MaxRow())
            ::nwLeft    := Min(::nwLeft+1,MaxCol())
            ::nwRight   := Max(0,::nwRight-1)
            ::nwBottom  := Max(0,::nwBottom-1)
         endif
      else
         ::cBorder := cBorder
         if ::cBorder == ""
            ::nwTop     := Max(0,::nwTop-1)
            ::nwLeft    := Max(0,::nwLeft-1)
            ::nwRight   := Min(MaxCol(),::nwRight+1)
            ::nwBottom  := Min(MaxRow(),::nwBottom+1)
         endif
      endif
      ::Configure( 0 )
   endif

Return self

*------------------------------------------------*
METHOD SetSeparator( nType, cSep ) CLASS TBrowse
*------------------------------------------------*

   ::lConfigured := .f.

   if nType == 0 
      ::cColSep  := if( ! EmptyStr( cSep ), cSep, ::cColSep )
   elseif nType == 1
      ::cFootSep := if( ! EmptyStr( cSep ), cSep, ::cFootSep )
      ::lFootSep := ! EmptyStr( ::cFootSep ) 
   elseif nType == 2
      ::cHeadSep := if( ! EmptyStr( cSep ), cSep, ::cHeadSep )
      ::lHeadSep := ! EmptyStr( ::cHeadSep ) 
   endif

RETURN SELF

*---------------------------------------------------*
METHOD SetMoveBlock( nType, bBlock ) CLASS TBrowse
*---------------------------------------------------*
Local b := bBlock

  if ! hb_IsBlock(bBlock) .and. nType=2  // Skipblock
     b := {|i|0}
  else
     b := bBlock  // GoTopBlock or GoBottomBlock
  endif

Return b

*---------------------------------------------------*
METHOD SetRowPos( nRow ) CLASS TBrowse
*---------------------------------------------------*

   if hb_IsNumeric(nRow) .and.;
      nRow >= 1 .and. nRow <= ::nRowCount .and. nRow != ::nRowPos 

      ::ResetMove()
      ::Moved()
      ::nRowPos  := nRow

      if nRow > ::nRowPos
         ::nMoveTo := TBM_DOWN
      else
         ::nMoveTo := TBM_UP
      endif

      ::ForceStable()
      ::oCache:CurRow := nRow

   endif

Return Self

*---------------------------------------------------*
METHOD SetColorSpec( cColor ) CLASS TBrowse
*---------------------------------------------------*

 if ! Empty( cColor )
    ::lConfigured := .f.
    ::aColorSpec := Color2Array( cColor )
    ::cColorSpec := cColor
 endif

RETURN Self

*---------------------------------------------------*
METHOD GetCoordinate( nType ) CLASS TBrowse
*---------------------------------------------------*
Local nBorder := if( ::cBorder == "", 0, 1 )
Local n

 switch nType

    case 0
      n := Max(0, ::nwTop - nBorder )
      exit

    case 1
      n := Max( 0, ::nwLeft - nBorder )
      exit

    case 2
      n := Min( ::nwBottom + nBorder, maxrow() )
      exit

    case 3
      n := Min( ::nwRight + nBorder, maxcol() )
      exit
 end

Return n

*---------------------------------------------------*
METHOD SetCoordinate( nType, nValue )
*---------------------------------------------------*
Local nBorder := if( ::cBorder == "", 0, 1 )
Local n := 0

   default nValue to 0 

   ::lConfigured := .f.

   switch nType

      case 0
        n := Max(0, Min( nValue + nBorder, MaxRow() ) )
        ::nwTop := n
        exit
      case 1
        n := Max(0, Min( nValue + nBorder, MaxCol() ) )
        ::nwLeft := n
        exit
      case 2
        n := Min( MaxRow(), Max( 0, nValue - nBorder ) )
        ::nwBottom := n
        exit
      case 3
        n := Min( MaxCol(), Max( 0, nValue - nBorder ) )
        ::nwRight  := n
        exit
   end

   if nType = 1 .or. nType = 3

      ::nVisWidth := ::nwRight - ::nwLeft + 1

   elseif nType = 0 .or. nType = 2

      ::nRowCount := ::nwBottom - ::nwTop + 1 - ;
                     if( ::lHeaders, ::nHeaderHeight, 0 ) - ;
                     if( ::lFooters, ::nFooterHeight, 0 ) - ;
                     if( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ) - ;
                     if( ::lFootSep .OR. ::lColFootSep, 1, 0 )

   endif

Return n

*---------------------------------------------------*
METHOD EvalSkipBlock( nSkip ) CLASS TBROWSE
*---------------------------------------------------*
LOCAL nSkipped

   nSkipped := ::oCache:Skip( nSkip )

   if nSkipped = NIL
      nSkipped := 0
   endif

RETURN nSkipped

//---------------------------------------------------------------------//
//                      Clipper 5.3b Compatibility
//---------------------------------------------------------------------//

#ifdef HB_COMPAT_C53

*---------------------------------------------------*
METHOD ApplyKey( nKey )  CLASS TBrowse
*---------------------------------------------------*
Return ::TApplyKey( nKey, self )

*---------------------------------------------------*
METHOD InitKeys( o ) CLASS TBrowse
*---------------------------------------------------*

   Default o:aKeys to {;
              { K_DOWN,        {| oB, nKey | oB:Down()    , 0 } } ,;
              { K_END,         {| oB, nKey | oB:End()     , 0 } } ,;
              { K_CTRL_PGDN,   {| oB, nKey | oB:GoBottom(), 0 } } ,;
              { K_CTRL_PGUP,   {| oB, nKey | oB:GoTop()   , 0 } } ,;
              { K_HOME,        {| oB, nKey | oB:Home()    , 0 } } ,;
              { K_LEFT,        {| oB, nKey | oB:Left()    , 0 } } ,;
              { K_PGDN,        {| oB, nKey | oB:PageDown(), 0 } } ,;
              { K_PGUP,        {| oB, nKey | oB:PageUp()  , 0 } } ,;
              { K_CTRL_END,    {| oB, nKey | oB:PanEnd()  , 0 } } ,;
              { K_CTRL_HOME,   {| oB, nKey | oB:PanHome() , 0 } } ,;
              { K_CTRL_LEFT,   {| oB, nKey | oB:PanLeft() , 0 } } ,;
              { K_CTRL_RIGHT,  {| oB, nKey | oB:PanRight(), 0 } } ,;
              { K_RIGHT,       {| oB, nKey | oB:Right()   , 0 } } ,;
              { K_UP,          {| oB, nKey | oB:Up()      , 0 } } ,;
              { K_ESC,         {| oB, nKey | -1               } } ,;
              { K_MWFORWARD,   {| oB, nKey | oB:Up()      , 0 } } ,;
              { K_MWBACKWARD,  {| oB, nKey | oB:Down()    , 0 } } ,;
              { K_LBUTTONDOWN, {| oB, nKey | tbmouse( ob, MRow(), MCol() ) } } }
Return o

*---------------------------------------------------*
METHOD SetKey( nKey,bBlock ) CLASS TBrowse
*---------------------------------------------------*
LOCAL bReturn,nPos

   ::InitKeys( self )

   if ( nPos := ascan( ::aKeys,{| x | x[ 1 ] == nkey } ) ) == 0
      if ISBLOCK( bBlock )
         //bReturn := bBlock
         aadd( ::aKeys, { nKey, bBlock } )
      endif
      bReturn := bBlock

   elseif ISBLOCK( bBlock )
      ::aKeys[ npos ][ 2 ] := bBlock
      bReturn := bBlock

   elseif PCOUNT() == 1
      bReturn := ::aKeys[ npos ][ 2 ]

   elseif ( bReturn := ::aKeys[ nPos ][ 2 ], PCount() == 2 .AND. ;
                                 ISNIL( bBlock ) .AND. nKey != 0 )
      ADel( ::aKeys, nPos, .T. )

   endif

Return bReturn

*---------------------------------------------------*
METHOD TApplyKey( nKey, oBrowse ) CLASS TBrowse
*---------------------------------------------------*
LOCAL bBlock := oBrowse:setkey( nKey )
LOCAL nReturn := TBR_CONTINUE  // 0

   DEFAULT bBlock TO oBrowse:setkey( 0 )

   if ISNIL( bBlock )
      nReturn := TBR_EXCEPTION  // 1
   else
      nReturn := eval( bBlock, oBrowse, nKey )
   endif

Return nReturn

*---------------------------------------------------*
METHOD HitTest( mrow,mcol ) CLASS TBrowse
*---------------------------------------------------*
Local i, nVisCol, nRet

   if ( mRow < ::nwTop  .or. mRow > ::nwBottom ) .or. ;
      ( mCol < ::nwLeft .or. mCol > ::nwRight )
      // if I'm outside browse
      return HTNOWHERE
   endif

   // checking absolute position
   nRet := HTNOWHERE

   if !( ::cBorder == "" )

      if mRow == ::nTop .AND. mCol == ::nLeft
        nRet := HTTOPLEFT
      elseif mRow == ::nTop    .AND. mCol == ::nRight
        nRet := HTTOPRIGHT
      elseif mRow == ::nBottom .AND. mCol == ::nLeft
        nRet := HTBOTTOMLEFT
      elseif mRow == ::nBottom .AND. mCol == ::nRight
        nRet := HTBOTTOMRIGHT
      elseif mRow == ::nTop
        nRet := HTTOP
      elseif mRow == ::nBottom
        nRet := HTBOTTOM
      elseif mCol == ::nwLeft
        nRet := HTLEFT
      elseif mCol == ::nwRight
        nRet := HTRIGHT
      endif

   endif

   if nRet == HTNOWHERE
      if mCol >= ::nLeft + ::nFrozenWidth .AND. mCol <= ::nLeft + ::nFrozenWidth + ::nSpacePre - 1  //Changes per Zeljko Stupar
             // if i'm on left side (also consider spaces on left)
        nRet := HTLEFT
      elseif mCol >= ::nRight - ::nSpaceLast + 1 .AND. mCol <= ::nwRight
             // if i'm on right side (also consider spaces on right)
        nRet := HTRIGHT
      elseif ::lHeadSep     .AND. mRow == ::nTop + ::nHeaderHeight
         // if i'm on header sep
         nRet := HTHEADSEP
//      elseif mRow >= ::nTop .AND. mRow <= ::nTop + ::nHeaderHeight
      elseif mRow >= ::nTop .AND. mRow < ::nTop + ::nHeaderHeight
         // if i'm on header
         nRet := HTHEADING
      elseif ::lFootSep     .AND. mRow == ::nBottom - ::nFooterHeight
         // if i'm on footer sep
         nRet := HTFOOTSEP
      elseif ::lFooters .AND. mRow >= ::nBottom - ::nFooterHeight .AND. mRow <= ::nBottom //Changes per Zeljko Stupar
         // if i'm on footer
         nRet := HTFOOTING
      elseif mCol IN ::aColumnsSep
         nRet := HTCOLSEP
      else
         nRet := HTCELL
      endif

   endif

   ::mRowPos := ::nRowPos
   ::mColPos := ::nColPos

   // move internal mouse pointer to correct position
   IF nRet <> HTNOWHERE

      ::mRowPos := mRow - ::mRect[ 1 ] + 1

      nVisCol := len( ::aColumnsSep )

      if nVisCol == 0
         ::mColPos := 1

      elseif mcol >= ::aColumnsSep[ nVisCol ]
         ::mColPos := nVisCol + ::nLeftVisible - ::nFrozenCols

      else
         for i := 1 to nVisCol
            if mcol < ::aColumnsSep[ i ]
               ::mColPos := i + ::nLeftVisible - ::nFrozenCols - 1
               exit
            endif
         next
      endif

      // if browse has columns that fits exactly horizontally space and
      // I have no border and I'm already on first visible col or on last visible col,
      // then I assume that I want to move horizontally
      IF ::mRowPos == ::nRowPos .AND. ;
         ::mColPos == ::nColPos

         IF ::mColPos == ::nLeftVisible .AND. ;
            ::nLeftVisible - ::nFrozenCols > 1 .and. ::nSpacePre = 0
            ::mColPos--
         ELSEIF ::mColPos == ::nRightVisible .AND. ;
            ::nRightVisible < ::nColCount .and. ::nSpaceLast = 0
            ::mColPos++
         ENDIF

      ENDIF

   endif

Return nRet

*---------------------------------------------------*
METHOD SetStyle( nStyle, lSetting ) CLASS TBrowse
*---------------------------------------------------*
LOCAL n, nLen

nLen := Len( ::aStyle ) 

if Hb_IsNumeric( nStyle )

   if nStyle > nLen

      for n := (nLen+1) to nStyle
          aadd( ::aStyle, .f. )
      next

   endif

   if Hb_IsLogical( lSetting )
      ::aStyle[ nStyle ] := lSetting
   endif

endif

Return Self

*---------------------------------------------------*
FUNCTION TBMOUSE( oBrowse, nMouseRow, nMouseCol )
*---------------------------------------------------*
LOCAL n

   if oBrowse:hittest( nMouseRow, nMouseCol ) == HTCELL

      n := oBrowse:mrowpos - oBrowse:nRowPos

      do while n < 0
         n++
         oBrowse:up():forceStable()
      enddo

      do while n > 0
         n--
         oBrowse:down():forceStable()
      enddo

      n := oBrowse:mcolpos - oBrowse:colpos
      if n < oBrowse:leftVisible - oBrowse:colPos .AND. oBrowse:freeze + 1 < oBrowse:leftVisible
         n += ( oBrowse:freeze + 1 - oBrowse:leftVisible )  // hidden cols
      end
      do while n < 0
         n++
         oBrowse:left()
      enddo

      do while n > 0
         n--
         oBrowse:right()
      enddo

      return 0
   endif

return 1

#endif

*---------------------------------------------------*
STATIC FUNCTION LenVal( xVal, cType, cPict )
*---------------------------------------------------*
   LOCAL nLen

   if !ISCHARACTER( cType )
      cType := Valtype( xVal )
   endif

   Switch cType
      case "L"
         nLen := 1
         exit

      case "N"
      case "C"
      case "D"
         If !Empty( cPict )
            nLen := Len( Transform( xVal, cPict ) )
            exit
         Endif

         Switch cType
            case "N"
               nLen := Len( Str( xVal ) )
               exit

            case "C"
               nLen := Len( xVal )
               exit

            case "D"
               nLen := Len( DToC( xVal ) )
               exit
         end
         exit

      default
         nLen := 0

   end

Return nLen

*---------------------------------------------------*
STATIC FUNCTION Color2Array( cColorSpec )
*---------------------------------------------------*
   LOCAL n
   LOCAL a_ := {}
   LOCAL cToken := ','

   DEFAULT cColorSpec TO SetColor()

   if Empty( cColorSpec )
      cColorSpec := SetColor()
   endif

   n := 1
   do while n > 0
      if ( n := At( cToken, cColorSpec ) ) > 0
         AAdd( a_, SubStr( cColorSpec, 1, n - 1 ) )
         cColorSpec := SubStr( cColorSpec, n + 1 )
      endif
   enddo
   AAdd( a_, Trim( cColorSpec ) )

Return a_

*---------------------------------------------------*
STATIC FUNCTION BlankValue( aColInfo )
*---------------------------------------------------*
Local xValue

switch aColInfo[ TBCI_TYPE ]
   case 'N'
      xValue := 0
      exit
   case 'D'
      xValue := CTOD( "" )
      exit
   case 'L'
      xValue := .F.
      exit
   default
      xValue := Space( aColInfo[ TBCI_WIDTHCELL ] )
end
Return xValue

*---------------------------------------------------*
STATIC FUNCTION CacheOK( a, n, n2 )
*---------------------------------------------------*
* Test the validity of the cache array before assess/assign value for it.
* This test avoid error if array is empty or element is out of bound or nil.
LOCAL lRet := .F.

lRet := ( hb_IsArray(a) .AND.;
         Len(a)>0 .AND.;
         hb_IsNumeric(n) .AND. n>0 .AND.;
         Len(a)>=n .AND. !hb_IsNil(a[n]) )


If lRet .AND. !hb_IsNil(n2) .AND. hb_IsNumeric(n2) .AND. n2>0
   lRet := ( Len( a[n] ) >= n2 )
Endif

Return lRet

*---------------------------------------------------*
STATIC FUNCTION DefColorOK( cColorSpec, aDefColor )
*---------------------------------------------------*
* Check validity of defcolor index array. If any value is
* invalid return default index array.
*---------------------------------------------------*
Local aColorSpec
Local lOK := .T.
Local nIndex := 0

 if Empty( cColorSpec )
    cColorSpec := SetColor()
 endif

 if Empty( aDefColor )
    aDefColor := {1,2,1,1}
 endif

 aColorSpec := Color2Array( cColorSpec )

 FOR EACH nIndex IN aDefColor
     lOK := ( nIndex > 0 .AND. nIndex <= Len(aColorSpec) )
     IF !lOK
        EXIT
     ENDIF
 NEXT

Return iif( lOK, aDefColor, {1,2,1,1} )

*---------------------------------------------------*
STATIC FUNCTION EmptyStr( cStr )
*---------------------------------------------------*
* Check if a string is valid.
*---------------------------------------------------*
Return !( Hb_IsString( cStr ) .and. Len( cStr ) > 0 )


************************************************************************
** To USE TBrowse call TBrowseNew() or TBrwoseDB() functions ONLY.    **
** Don't use TBrowse():New() way. This will return an error.          **
************************************************************************

*---------------------------------------------------*
FUNCTION TBrowseNew( nTop, nLeft, nBottom, nRight )
*---------------------------------------------------*
RETURN ( TBrowse():New( nTop, nLeft, nBottom, nRight ) )

