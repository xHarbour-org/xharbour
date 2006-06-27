/*
 * $Id: tbrowse.prg,v 1.151 2006/06/16 07:16:17 fsgiudice Exp $
 */

/*
 * Harbour Project source code:
 * TBrowse Class
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

 /*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
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
 */


/* NOTE: Don't use SAY in this module, use DispOut(), DispOutAt() instead,
         otherwise it will not be CA-Cl*pper compatible. [vszakats] */

/* TODO: :firstScrCol() --> nScreenCol
         Determines screen column where the first table column is displayed.
         Xbase++ compatible method */

/* TODO: :viewArea() --> aViewArea
         Determines the coordinates for the data area of a TBrowse object.
         Xbase++ compatible method */

//-------------------------------------------------------------------//

#include "hbclass.ch"
#include "color.ch"
#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "button.ch"
#include "tbrowse.ch"
#include "error.ch"

//-------------------------------------------------------------------//
//
//                         Pritpal Bedi
//                 Constants to access ::aColsInfo
//
#define o_Obj             1   // Object Column
#define o_Type            2   // Type of Data in Column
#define o_Width           3   // Column Width
#define o_Heading         4   // Column Headings
#define o_Footing         5   // Column Footings
#define o_Pict            6   // Column Picture
#define o_WidthCell       7   // Width of the Cell
#define o_ColSep          8   // Column Seperator
#define o_SepWidth        9   // Width of the Separator
#define o_DefColor       10   // Array with index of color
#define o_SetWidth       11   // If True, only SetFrozen can change o_Width
/* 02/july/2005 - <maurilio.longo@libero.it>
                  removed, not really needed, but still present inside ::aColsInfo,
                  as a NIL value
#define o_Blank          12   // Spaces corresponding to o_Width
*/
#define o_lColSep        13   // Should column separator be drawn
#define o_ScrColPos      14   // Temporary column position on screen


/* 25/11/2004 - <maurilio.longo@libero.it>
   TBC_ are CLR_ constants increased by one to be used as indexes inside columns color arrays
   HEADING and FOOTING have to be colored like STANDARD colors, clipper does it, and could simply
   be removed
*/
#define TBC_CLR_STANDARD  ( CLR_STANDARD + 1 )
#define TBC_CLR_ENHANCED  ( CLR_ENHANCED + 1 )
#define TBC_CLR_HEADING   TBC_CLR_STANDARD
#define TBC_CLR_FOOTING   TBC_CLR_STANDARD


/* 23/11/2004 - <maurilio.longo@libero.it> Inlined to be somewhat faster
   25/11/2004 - <maurilio.longo@libero.it> Since there are only two colors, this makes every day less sense to me...
*/
#define  ColorToDisp( aColor, nColor )    iif( Len( aColor ) >= nColor, aColor[ nColor ], { 1, 2 }[ nColor ] )


// ===================================================================================================

/* 11/11/2005 - <maurilio.longo@libero.it>
                Every cell inside tbrowse cache
*/
CLASS TDataCell
   DATA  xData                         // Cell value
   DATA  aColor                        // Cell colorBlock result
   DATA  aColorRect  INIT NIL          // Cell color if it is inside a color rect

   METHOD New()      INLINE Self
END CLASS

/* 20/07/2005 - <maurilio.longo@libero.it>
                TBrowse Data Cache class
   17/10/2005   Passes all tests I've done, including DBU
*/
CLASS TDataCache
                   // FSG - 14/06/2006 - Removed READONLY attribute to permit direct access for forced repositioning
   DATA  nCurRow   //READONLY            // Current Row inside cache
   DATA  nLastRow  READONLY            // Last Row inside cache which has data ( it can be < Len( ::aCache ) )

   METHOD   New( oBrowse )             // Which tbrowse this cache is bound to

   METHOD   dbSkip( nRecsToSkip )      // like dbSkip()
   METHOD   dbGoTop()
   METHOD   dbGoBottom()

   METHOD   GetCell( nRow, nCol )      // Retrieves the content of column n of current row

   // Retrieves the color (if any) of column n of current row, it can
   // be called ONLY after GetCell() for the same row,col has been called
   METHOD   GetCellColor( nRow, nCol ) INLINE IIF( CacheOK(::aCache,nRow,nCol), IIF( Empty( ::aCache[ nRow ][ nCol ]:aColorRect ),;
                                                   ::aCache[ nRow ][ nCol ]:aColor,;
                                                   ::aCache[ nRow ][ nCol ]:aColorRect), "" )

   // Sets colorrect to cells defined by aRect, aRect is an array { top, left, bottom, right, aColors }
   METHOD   SetColorRect( aRect )

   /* Please note that here Invalidate() means discard all data and force a reload from
      datasource as need to show a certain row arises. On the other hand, inside a TBrowse ::Invalidate()
      means only that all data will be re-painted onto screen but _not_ reloaded from
      datasource. To reload data a call to RefreshCurrent()/RefreshAll() needs to be done.
   */
   METHOD   Invalidate( nRow )         INLINE  iif( nRow == NIL,;
                                                    ::lInvalid := .T. ,;
                                                    iif( CacheOK(::aCache,nRow ), ::aCache[ nRow ] := NIL, NIL ) ),;
                                               Self

   /* Needed for clipper compatibility, invalidation of cache must happen just before
      stabilization of tbrowse. Note, this handles only full invalidation, not row invalidation
   */
   METHOD   PerformInvalidation()      INLINE  iif( ::lInvalid ,;
                                                    ( AFill( ::aCache, NIL ), ::InitCache(), ::lInvalid := .F., Self ),;
                                                    Self )
   HIDDEN:

   DATA  aCache                        // Array with cached data
   DATA  oCachedBrowse                 // TBrowse object I'm caching
   DATA  aRect                         // Array with pending color-rects
   DATA  lInvalid                      // .T. if ::Invalidate() has been called without a recno

   METHOD   InitCache( lInternal )     // Resets cache
   METHOD   FillRow( nRow )            // Fills a row of aCache with data from datasource

END CLASS



METHOD New( oBrowse ) CLASS TDataCache

   local nRows

   default nRows TO oBrowse:rowCount

   ::aCache := Array( nRows )
   ::oCachedBrowse := oBrowse

   ::nCurRow := 1
   ::nLastRow := 1
   ::aRect := {}
   ::lInvalid := .F.

RETURN Self



METHOD dbSkip( nRecsToSkip ) CLASS TDataCache

   LOCAL nRecsSkipped

   if nRecsToSkip <> 0

      nRecsSkipped := Eval( ::oCachedBrowse:SkipBlock, nRecsToSkip )

      // I've tried to move past top or bottom margin
      //
      if nRecsSkipped == 0

         if nRecsToSkip > 0
            ::nLastRow := ::nCurRow

         endif

      elseif nRecsSkipped == nRecsToSkip
         // If after movement I'm still inside present TBrowse
         //
         if ( ::nCurRow + nRecsSkipped >= 1 ) .AND. ( ::nCurRow + nRecsSkipped <= Len( ::aCache ) )
            ::nCurRow += nRecsSkipped

         else
            // It was K_PGDN or K_PGUP or K_UP or K_DN
            //
            if Abs( nRecsSkipped ) >= Len( ::aCache )
               // K_PGDN
               //
               if nRecsSkipped > 0
                  ::nCurRow := Len( ::aCache )

               else // K_PGUP
                  ::nCurRow := 1

               endif
               AFill( ::aCache, NIL )

            else
               // K_DN or K_UP

               // I'm at top or bottom of TBrowse so I can scroll
               //
               if ::nCurRow == Len( ::aCache )
                  ADel( ::aCache, 1 )

               else
                  AIns( ::aCache, 1 )
                  if ::nLastRow < Len( ::aCache )
                     ::nLastRow += 1
                  endif

               endif

            endif
         endif

      else
         // I couldn't move as far as requested
         // I need to refresh all rows if I go past current top or bottom row
         //
         if ( ::nCurRow + nRecsSkipped < 1 ) .OR. ( ::nCurRow + nRecsSkipped > Len( ::aCache ) )
            // don't go past boundaries
            //
            ::nCurRow := iif( nRecsSkipped > 0, Len( ::aCache ), 1 )
            AFill( ::aCache, NIL )

         else
            ::nCurRow += nRecsSkipped
            IF CacheOK(::aCache,::nCurRow)
               ::aCache[ ::nCurRow ] := NIL
            ENDIF
         endif

      endif

      if ::nLastRow < ::nCurRow
         ::nLastRow := ::nCurRow
      endif

   else
      nRecsSkipped := Eval( ::oCachedBrowse:SkipBlock, 0 )
      ::FillRow( ::nCurRow )
   endif

RETURN nRecsSkipped



METHOD dbGoTop() CLASS TDataCache

   Eval( ::oCachedBrowse:GoTopBlock )

   ::nCurRow := 1

RETURN NIL



METHOD dbGoBottom() CLASS TDataCache

   local nToTop

   Eval( ::oCachedBrowse:GoBottomBlock )

   // How many rows are available to top of datasource
   nToTop := Abs( Eval( ::oCachedBrowse:SkipBlock, - ( Len( ::aCache )  - 1 ) ) )

   ::nCurRow := 1
   ::nLastRow := nToTop + 1

RETURN nToTop



METHOD FillRow( nRow ) CLASS TDataCache

   local aCol, oCell
   local nRectPos,i

   IF nRow > Len( ::aCache )
      RETURN Self
   ENDIF

   ::aCache[ nRow ] := Array( ::oCachedBrowse:colCount )

   for each aCol in ::oCachedBrowse:aColsInfo

      oCell := TDataCell():New()

      i := hb_EnumIndex()

      with object oCell
         :xData := Eval( aCol[ o_Obj ]:block )
         :aColor := IIF( Empty( aCol[ o_Obj ]:colorBlock ),;
                         NIL,;
                         Eval( aCol[ o_Obj ]:colorBlock, :xData ) )

         if ! Empty( ::aRect ) .AND. ( nRectPos := AScan( ::aRect, { |item| item[ 1 ] == nRow } ) ) > 0
            if HB_EnumIndex() >= ::aRect[ nRectPos ][ 2 ] .AND. HB_EnumIndex() <= ::aRect[ nRectPos ][ 3 ]
               :aColorRect := ::aRect[ nRectPos ][ 4 ]
            endif
         endif
      end
      IF CacheOK(::aCache,nRow,i)
         ::aCache[ nRow ][ i ] := oCell
      ENDIF
   next

   if ! Empty( ::aRect ) .AND. nRectPos > 0
      ADel( ::aRect, nRectPos, .T. )
   endif

Return Self



METHOD GetCell( nRow, nCol ) CLASS TDataCache

   local nSkipped


   if Empty( ::aCache[ nRow ] )

      // No more rows contain data
      if nRow > ::nLastRow
         return NIL

      elseif ( nSkipped := ::dbSkip( nRow - ::nCurRow ) ) <> 0 .OR. nRow - ::nCurRow == 0
         // dbSkip( 0 ) already refills current row
         if nSkipped <> 0
            ::FillRow( nRow )
         endif

      else
         return NIL

      endif

   endif

RETURN IF( CacheOK(::aCache,nRow,nCol), ::aCache[ nRow ][ nCol ]:xData, NIL)



METHOD SetColorRect( aRect ) CLASS TDataCache

   local nRow, nCol

   for nRow := aRect[ 1 ] to aRect[ 3 ]

      if Empty( ::aCache[ nRow ] )

         // A five elements array shrinks to a four one
         // { top, left, bottom, right, aColors } -> { nRow, left, right, aColors }
         AAdd( ::aRect, { nRow, aRect[ 2 ], aRect[ 4 ], aRect[ 5 ] } )

      else

         for nCol := aRect[ 2 ] to aRect[ 4 ]
             IF CacheOK(::aCache,nRow,nCol)
                ::aCache[ nRow ][ nCol ]:aColorRect := aRect[ 5 ]
             ENDIF
         next

      endif
   next

return Self



METHOD InitCache() CLASS TDataCache

   local nCurRow

   // this is needed when number of shown rows decreases due to a phantom record
   // being removed; for example, using DBU, dbGoBottom(), Down(), Up()
   // Clipper does this as well! :)
   if ::nCurRow > 1
      ::dbSkip( -( ::nCurRow - 1 ) )
      ::nCurRow := 1
   else
      // This will force a dbSkip( 0 ), when row gets requested by TBrowse, which,
      // in turn, forces a reload of row from datasource, this cannot be done now
      // since data has to be fetched from datasource only during stabilization
      // phase
      if CacheOK(::aCache,::nCurRow)
         ::aCache[ ::nCurRow ] := NIL
      endif
   endif

   ::nLastRow := Len( ::aCache )

RETURN Self

// ==============================================================================================


CLASS TBrowse

   DATA autoLite               // Logical value to control highlighting
   DATA cargo                  // User-definable variable
   ACCESS ColCount      INLINE ::nColumns    // Number of TBrowse columns
   DATA goBottomBlock         // Code block executed by TBrowse:goBottom()
   DATA goTopBlock            // Code block executed by TBrowse:goTop()
   DATA hitBottom             // Indicates the end of available data
   DATA hitTop                // Indicates the beginning of available data
   DATA leftVisible           // Indicates position of leftmost unfrozen column in display
   DATA rightVisible          // Indicates position of rightmost unfrozen column in display
   DATA rowCount              // Number of visible data rows in the TBrowse display
   DATA skipBlock             // Code block used to reposition data source
   DATA stable                // Indicates if the TBrowse object is stable

   #ifdef HB_COMPAT_C53
   DATA nRow                  // Row number for the actual cell
   DATA nCol                  // Col number for the actual cell
   DATA aKeys
   DATA mColpos,mrowPos,message
   #endif

   #ifdef HB_EXTENSION
   DATA aColumnsSep           // Holds the column position where seperators are marked . for Wvt_DrawGridVert()
   #endif

   ACCESS border              INLINE ::cBorder
   ASSIGN border( cBorder )   INLINE ::SetBorder( cBorder )
   ACCESS colorSpec           INLINE ::cColorSpec   // Color table for the TBrowse display
   ASSIGN colorSpec(cColor)   INLINE if( empty( cColor ), ::cColorSpec, ( ::lConfigured := .f., ;
                                     ::aColorSpec := Color2Array( cColor ), ::cColorSpec := cColor ) )

   ACCESS colPos              INLINE ::nColPos
   ASSIGN colPos( nColPos )   INLINE ::SetColPos( nColPos )
   ACCESS rowPos              INLINE ::nRowPos
   ASSIGN rowPos( nRowPos )   INLINE iif( nRowPos >= 1 .AND. nRowPos <= ::rowCount,;
                                          ( ::Moved(), ::nRecsToSkip := nRowPos - ::nRowPos, nRowPos ),;
                                          ::nRowPos )


   ACCESS nBottom             INLINE ::nwBottom +  iif( ::cBorder == "", 0, 1 )
   ASSIGN nBottom( nBottom )  INLINE ::PreConfigVertical(   ::nwBottom := nBottom - iif( ::cBorder == "", 0, 1 ) )
   ACCESS nLeft               INLINE ::nwLeft   -  iif( ::cBorder == "", 0, 1 )
   ASSIGN nLeft( nLeft )      INLINE ::PreConfigHorizontal( ::nwLeft   := nLeft   + iif( ::cBorder == "", 0, 1 ) )
   ACCESS nRight              INLINE ::nwRight  +  iif( ::cBorder == "", 0, 1 )
   ASSIGN nRight( nRight )    INLINE ::PreConfigHorizontal( ::nwRight  := nRight  - iif( ::cBorder == "", 0, 1 ) )
   ACCESS nTop                INLINE ::nwTop    -  iif( ::cBorder == "", 0, 1 )
   ASSIGN nTop( nTop )        INLINE ::PreConfigVertical(   ::nwTop    := nTop    + iif( ::cBorder == "", 0, 1 ) )

   ACCESS colSep  INLINE ::cColSep        // Column separator character
   ASSIGN colSep( cColSep )   INLINE ::lConfigured := .f., ::cColSep  := cColSep

   ACCESS footSep INLINE ::cFootSep       // Footing separator character
   ASSIGN footSep( cFootSep ) INLINE ::lConfigured := .f.,;
                                     ::lFootSep := ! Empty( ::cFootSep := cFootSep )

   ACCESS headSep INLINE ::cHeadSep       // Head separator character
   ASSIGN headSep( cHeadSep ) INLINE ::lConfigured := .f.,;
                                     ::lHeadSep := ! Empty( ::cHeadSep := cHeadSep )

   ACCESS freeze INLINE ::nFrozenCols     // Number of columns to freeze/frozen
   ASSIGN freeze( nHowMany )  INLINE ::SetFrozenCols( nHowMany, .t. ), ::lConfigured := .f., ::nFrozenCols

   METHOD New( nTop, nLeft, nBottom, nRight )  // Constructor
   METHOD Down()                          // Moves the cursor down one row
   METHOD End()                           // Moves the cursor to the rightmost visible data column
   METHOD GoBottom()                      // Repositions the data source to the bottom of file
   METHOD GoTop()                         // Repositions the data source to the top of file
   METHOD Home()                          // Moves the cursor to the leftmost visible data column
   MESSAGE Left() METHOD _Left()          // Moves the cursor left one column
   METHOD PageDown()                      // Repositions the data source downward
   METHOD PageUp()                        // Repositions the data source upward
   METHOD PanEnd()                        // Moves the cursor to the rightmost data column
   METHOD PanHome()                       // Moves the cursor to the leftmost visible data column
   METHOD PanLeft()                       // Pans left without changing the cursor position
   METHOD PanRight()                      // Pans right without changing the cursor position
   MESSAGE Right() METHOD _Right()        // Moves the cursor right one column
   METHOD Up()                            // Moves the cursor up one row

   METHOD AddColumn( oCol )
   METHOD DelColumn( nPos )               // Delete a column object from a browse
   METHOD InsColumn( nPos, oCol )         // Insert a column object in a browse
   METHOD GetColumn( nColumn )            // Gets a specific TBColumn object
   METHOD SetColumn( nColumn, oCol )      // Replaces one TBColumn object with another
   METHOD ColWidth( nColumn )             // Returns the display width of a particular column
   METHOD ColorRect( aRect, aColors )     // Alters the color of a rectangular group of cells
   METHOD Configure( nMode )              // Reconfigures the internal settings of the TBrowse object
                                          // nMode is an undocumented parameter in CA-Cl*pper
   METHOD DeHilite()                      // Dehighlights the current cell
   METHOD ForceStable()                   // Performs a full stabilization
   METHOD Hilite()                        // Highlights the current cell
   METHOD Invalidate()                    // See top of method code
   METHOD RefreshAll()                    // See top of method code
   METHOD RefreshCurrent()                // Same as RefreshAll(), but only for current row

   METHOD Stabilize()                     // Performs incremental stabilization

#ifdef HB_COMPAT_C53
   METHOD SetKey( nKey, bBlock )
   METHOD ApplyKey( nKey )
   METHOD InitKeys( Self )
   METHOD TApplyKey( nKey, o )
   METHOD HitTest( nMouseRow,nMouseCol )
   METHOD SetStyle( nMode,lSetting )
#endif

   PROTECTED:     /* P R O T E C T E D */

   METHOD MGotoYX( nRow, nCol )           // Given screen coordinates nRow, nCol sets TBrowse cursor on underlaying cell
                                          // _M_GotoXY because this method will mostly be called to handle mouse requests

   HIDDEN:        /* H I D D E N */

   METHOD PosCursor()                     // Positions the cursor to the beginning of the call, used only when autolite==.F.
   METHOD LeftDetermine()                 // Determine leftmost unfrozen column in display
   METHOD DispCell( nRow, nCol, xValue, nColor )  // Displays a single cell and returns position of first char of displayed value if needed
   METHOD HowManyCol()                    // Counts how many cols can be displayed
   METHOD RedrawHeaders( nWidth )         // Repaints TBrowse Headers
   METHOD Moved()                         // Every time a movement key is issued I need to reset certain properties
                                          // of TBrowse, I do these settings inside this method
   METHOD EvalSkipBlock( nSkip )          // Eval skip block

   METHOD WriteMLineText( cStr, nPadLen, lHeader, cColor ) // Writes a multi-line text where ";" is a line break, lHeader
                                                           // is .T. if it is a header and not a footer
   METHOD SetFrozenCols( nHowMany )       // Handles freezing of columns
   METHOD SetColumnWidth( oCol )          // Calcs width of given column
   METHOD SetBorder( cBorder )
   METHOD DrawARow()                      // Draws any row in stabilization
   METHOD CheckRowsToBeRedrawn()
   METHOD CheckRowPos()
   METHOD AColInfo()
   METHOD PerformStabilization()          // "Real" stabilization procedure
   METHOD PreConfigHorizontal( uValue )   // This method calculates variables related to horizontal coordinates
   METHOD PreConfigVertical( uValue )     // This method calculates variables related to vertical coordinates
   METHOD SetColPos( nColPos )            // Performs a "direct jump" to a given column

   DATA aRect                             // One or more rectangles and colors associated specified with ColorRect()

   DATA aRedraw                           // Array of logical items indicating, is appropriate row need to be redraw
   DATA lHeaders                          // Internal variable which indicates whether there are column headers to paint
   DATA lFooters                          // Internal variable which indicates whether there are column footers to paint

   DATA lHeadSep                          INIT .f. // Internal variable which indicates whether TBrowse has line headers to paint
   DATA lFootSep                          INIT .f. // Internal variable which indicates whether TBrowse has line footers to paint
   DATA lColHeadSep                       INIT .f. // Internal variable which indicates whether at least a TBColumn has line headers to paint
   DATA lColFootSep                       INIT .f. // Internal variable which indicates whether at least a TBColumn has line footers to paint

   DATA lRedrawFrame                      // True if I need to redraw Headers/Footers
   DATA nColsWidth                        // Total width of visible columns plus ColSep
   DATA nColsVisible                      // Number of columns that fit on the browse width
   DATA lHitTop                           // Internal Top/Bottom reached flag
   DATA lHitBottom
   DATA nRecsToSkip                       // Recs to skip on next Stabilize()
   DATA nNewRowPos                        // Next position of data source (after first phase of stabilization)
   DATA nRowData                          // Row, first row of data

   DATA nColPos
   DATA nRowPos                           // Current cursor row position

   DATA nwBottom                          INIT 0 // Bottom row number for the TBrowse display
   DATA nwLeft                            INIT 0 // Leftmost column for the TBrowse display
   DATA nwRight                           INIT 0 // Rightmost column for the TBrowse display
   DATA nwTop                             INIT 0 // Top row number for the TBrowse display

   DATA cBorder                           INIT ""
   DATA cColorSpec

   DATA cColSep                           // Column separator character
   DATA cFootSep                          // Footing separator character
   DATA cHeadSep                          // Head separator character

   DATA nHeaderHeight                     // How many lines is highest Header/Footer and so how many lines of
   DATA nFooterHeight                     // screen space I have to reserve
   DATA nFrozenWidth                      // How many screen column are not available on the left side of TBrowse display
                                          // > 0 only when there are frozen columns
   DATA nFrozenCols                       // Number of frozen columns on left side of TBrowse
   DATA nColumns                          // Number of columns added to TBrowse
   DATA lNeverDisplayed                   // .T. if TBrowse has never been stabilized()

   DATA aColsInfo                         // Columns configuration array
   DATA nVisWidth                         // Visible width of Browser
   DATA lConfigured                       // Specifies whether tBrowse is already configured or not

   DATA cSpacePre                         // Blank Space prior to first column
   DATA cSpaceLast                        // Blank space after the last column
   DATA cSpaceWidth                       // Spaces of browse width
   DATA lRectPainting                     // .T. when ::ColorRect() calls ::ForceStable() to paint a colored region

   DATA oDataCache
   DATA lPaintBottomUp                    // .T. after a PG_DN, paints browser from bottom to top, optimizing
                                          // dbSkip()s calls. ( Clipper works this way )
#ifdef HB_COMPAT_C53
   DATA rect
   DATA aVisibleCols
   DATA aSetStyle
#endif
   DATA aColorSpec                        // Holds colors of Tbrowse:ColorSpec

   DATA nPrevDelColPos                    // Save previous colpos before delcolumn(). For clipper compatibility.

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( nTop, nLeft, nBottom, nRight ) CLASS TBrowse

   DEFAULT  nTop    TO 0
   DEFAULT  nLeft   TO 0
   DEFAULT  nBottom TO MaxRow()
   DEFAULT  nRight  TO MaxCol()

   ::nwTop           := nTop
   ::nwLeft          := nLeft
   ::nwBottom        := nBottom
   ::nwRight         := nRight

   ::rowCount        := nBottom - nTop + 1
   ::nRowData        := nTop
   ::AutoLite        := .T.
   ::leftVisible     := 1
   ::rightVisible    := 1
   ::nColPos         := 1
   ::HitBottom       := .F.
   ::HitTop          := .F.
   ::lHitTop         := .F.
   ::lHitBottom      := .F.
   ::cColorSpec      := SetColor()
   ::cColSep         := " "
   ::cFootSep        := ""
   ::cHeadSep        := ""
   ::nRowPos         := 1
   ::nNewRowPos      := 1
   ::stable          := .F.
   ::nRecsToSkip     := 0
   ::aRedraw         := {}
   ::lHeaders        := .F.
   ::lFooters        := .F.

   ::lHeadSep := ::lFootSep := ::lColHeadSep := ::lColFootSep := .F.

   ::lRedrawFrame    := .T.
   ::aRect           := {}
   ::nColsWidth      := 0
   ::nColsVisible    := 0
   ::nHeaderHeight   := 0
   ::nFooterHeight   := 0
   ::nFrozenWidth    := 0
   ::nFrozenCols     := 0
   ::nColumns        := 0
   ::lNeverDisplayed := .T.
   ::cBorder         := ""

   ::aColsInfo       := {}
   ::nVisWidth       := nRight - nLeft + 1
   ::lConfigured     := .f.
   ::aColorSpec      := {}

 #ifdef HB_COMPAT_C53
   ::mColPos         := 0
   ::mRowPos         := 0
   ::rect            := { nTop, nLeft, nBottom, nRight }
   ::aVisibleCols    := {}
   ::message         := ''
   ::nRow            := 0
   ::nCol            := 0
   ::aSetStyle       := ARRAY( 5 )

   ::aSetStyle[ TBR_APPEND    ] := .f.
   ::aSetStyle[ TBR_APPENDING ] := .f.
   ::aSetStyle[ TBR_MODIFY    ] := .f.
   ::aSetStyle[ TBR_MOVE      ] := .f.
   ::aSetStyle[ TBR_SIZE      ] := .f.
#endif

   ::aColumnsSep     := {}
   ::cSpacePre       := ''
   ::cSpaceLast      := ''
   ::cSpaceWidth     := ''
   ::lRectPainting   := .f.

   ::oDataCache := TDataCache():New( Self )
   ::lPaintBottomUp  := .F.

   ::nPrevDelColPos   := 0

Return Self

//-------------------------------------------------------------------//
/* from clipper NG:

   Invalidate() --> self

   TBrowse:Invalidate() causes the next stabilization of the TBrowse
   object to redraw the entire TBrowse display, including headings,
   footings, and all data rows.  Note that sending this message has no
   effect on the values in the data rows; it simply forces the display to
   updated during the next stabilization.  To force the data to be
   refreshed from the underlying data source, send the
   TBrowse:RefreshAll() message.
*/

METHOD Invalidate() CLASS TBrowse

   AFill( ::aRedraw, .T. )

   ::stable := .F.
   ::lRedrawFrame := .T.

Return Self

//-------------------------------------------------------------------//
/* from clipper NG:

   RefreshAll() --> self

   Internally marks all data rows as invalid, causing them to be
   refilled and redisplayed during the next stabilize loop.
*/

METHOD RefreshAll() CLASS TBrowse

   AFill( ::aRedraw, .T. )

   IF ! ::lPaintBottomUp
      ::oDataCache:Invalidate()
   ENDIF

   ::Stable := .F.

Return Self

//-------------------------------------------------------------------//

METHOD Configure( nMode ) CLASS TBrowse

   LOCAL n, nHeight, aCol, xVal, nFreeze, oErr, lInitializing := .f.

   default nMode to 0

   if nMode == 3
      nMode := 0
      lInitializing := .t.
   endif

   if ::nColPos > ::nColumns
      ::nColPos := ::nColumns
   endif

   ::nHeaderHeight := 0
   ::nFooterHeight := 0

   if nMode < 2 .or. ::lNeverDisplayed
      ::lHeaders     := .F.
      ::lFooters     := .F.
      ::lColHeadSep  := .F.
      ::lColFootSep  := .F.
      ::lRedrawFrame := .T.
   endif

   // Find out highest header and footer
   FOR EACH aCol IN ::aColsInfo

      if ( nMode <= 1 .and. !::lNeverDisplayed ) .or. lInitializing
         xVal := Eval( aCol[ o_Obj ]:block )

         aCol[ o_Type      ] := valtype( xVal )
         aCol[ o_Heading   ] := aCol[ o_Obj ]:heading
         aCol[ o_Footing   ] := aCol[ o_Obj ]:footing
         aCol[ o_Pict      ] := iif( Empty( aCol[ o_Obj ]:Picture ), "", aCol[ o_Obj ]:Picture )
         if ! aCol[ o_SetWidth ]
            aCol[ o_Width  ] := ::SetColumnWidth( aCol[ o_Obj ] )
         endif
         aCol[ o_WidthCell ] := Min( aCol[ o_Width ], LenVal( xVal, aCol[ o_Type ], aCol[ o_Pict ] ) )
         aCol[ o_ColSep    ] := iif( aCol[ o_Obj ]:ColSep != NIL, aCol[ o_Obj ]:ColSep, ::ColSep )
         aCol[ o_SepWidth  ] := len( aCol[ o_ColSep ] )
         aCol[ o_DefColor  ] := aCol[ o_Obj ]:DefColor

         aSize( aCol[ o_DefColor ], 4 )
         DEFAULT aCol[ o_DefColor,1 ] TO 1
         DEFAULT aCol[ o_DefColor,2 ] TO 2
         DEFAULT aCol[ o_DefColor,3 ] TO 1
         DEFAULT aCol[ o_DefColor,4 ] TO 1

         if aCol[ o_Type ] == 'D' .and. empty( aCol[ o_Pict ] )
            aCol[ o_Pict ] := '@D'
         endif

         aCol[ o_lColSep ] := aCol[ o_Width ] > 0
      endif

      if nMode = 0 .or. nMode = 2 .or. lInitializing
         aCol[ o_ColSep    ] := iif( aCol[ o_Obj ]:ColSep != NIL, aCol[ o_Obj ]:ColSep, ::ColSep )
      endif

      if nMode < 2 .or. ::lNeverDisplayed
         // Are there column headers/footers/separators to paint ?
         if ! Empty( aCol[ o_Heading ] )
            ::lHeaders := .T.
         endif
         if ! Empty( aCol[ o_Footing ] )
            ::lFooters := .T.
         endif
         /* as soon as we find one, we stop testing aCol[o_Obj]:XX to speed things up */
         if ! ::lColHeadSep .AND. ! Empty( aCol[ o_Obj ]:HeadSep )
            ::lColHeadSep := .T.
         endif
         if ! ::lColFootSep .AND. ! Empty( aCol[ o_Obj ]:FootSep )
            ::lColFootSep := .T.
         endif
      endif

      if ::lHeaders .AND. !Empty( aCol[ o_Heading ] )
         nHeight := Len( aCol[ o_Heading ] ) - Len( StrTran( aCol[ o_Heading ], ";" ) ) + 1

         if nHeight > ::nHeaderHeight
            ::nHeaderHeight := nHeight
         endif

      endif

      if ::lFooters .AND. !Empty( aCol[ o_Footing ] )
         nHeight := Len( aCol[ o_Footing ] ) - Len( StrTran( aCol[ o_Footing ], ";" ) ) + 1

         if nHeight > ::nFooterHeight
            ::nFooterHeight := nHeight
         endif

      endif

   next

   if Empty( ::aColorSpec )
      ::aColorSpec := Color2Array( ::cColorSpec )
   endif

   ::cSpaceWidth := space( ::nwRight - ::nLeft + 1 )

   if nMode == 1
      return Self
   endif

   /* 19/10/2005 - <maurilio.longo@libero.it>
                   Are we sure that a real tbrowse raises these errors?
   */
   do while .t.     // Reduce footer, headers and separator if the data
                    // not fit in the visible area.
                    // If it didn't fit, it generate error.

      ::nVisWidth := ::nwRight - ::nwLeft + 1

      /* 20/nov/2000 - maurilio.longo@libero.it
         If I add (or remove) header or footer (separator) I have to change number
         of available rows
      */
      ::RowCount := ::nwBottom - ::nwTop + 1 - iif( ::lHeaders, ::nHeaderHeight, 0 ) - ;
                     iif( ::lFooters, ::nFooterHeight, 0 ) - ;
                     iif( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ) - ;
                     iif( ::lFootSep .OR. ::lColFootSep, 1, 0 )

      // FSG - 14/06/2006 - commented out because otherwise it does not checks if there is space for
      //                    displaying headers and footers, needed for browse with a total height of
      //                    one row
      //if ::lNeverDisplayed
      //   exit
      //endif

      if ::nVisWidth <= 0
         // Generate Error TBROWSE
         //
         oErr := ErrorNew()
         oErr:severity    := ES_ERROR
         oErr:genCode     := EG_LIMIT
         oErr:subSystem   := "TBROWSE"
         oErr:subCode     := 0
         oErr:description := "Width limit exceeded"
         oErr:canRetry    := .F.
         oErr:canDefault  := .F.
         oErr:fileName    := ""
         oErr:osCode      := 0
         Eval( ErrorBlock(), oErr )
      endif

      if ::RowCount <= 0

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

         // Generate Error TBROWSE
         //
         oErr := ErrorNew()
         oErr:severity    := ES_ERROR
         oErr:genCode     := EG_LIMIT
         oErr:subSystem   := "TBROWSE"
         oErr:subCode     := 0
         oErr:description := "High limit exceeded"
         oErr:canRetry    := .F.
         oErr:canDefault  := .F.
         oErr:fileName    := ""
         oErr:osCode      := 0
         Eval( ErrorBlock(), oErr )

      endif

      exit

   enddo

   //   Starting position of data rows excluding headers . Pritpal Bedi
   //
   ::nRowData := ::nwTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + ;
                           iif( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ) - 1

   if Len( ::aRedraw ) <> ::RowCount .AND. ::RowCount > 0
      ::aRedraw := Array( ::RowCount )
      // I need a cache of different size
      ::oDataCache := TDataCache():New( Self )
      // here 'n' is an array
      for each n in ::aRect
         ::oDataCache:SetColorRect( n )
      next
      ::aRect := {}
   else
      ::oDataCache:Invalidate()
   endif

   ::Invalidate()

   // Force re-evaluation of space occupied by frozen columns
   nFreeze := ::nFrozenCols
   if nFreeze > 0
      ::SetFrozenCols( 0 )
   endif
   ::SetFrozenCols( nFreeze, .t. )

   #ifdef HB_COMPAT_C53
   ::Rect := { ::nwTop + ::nHeaderHeight + if( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ), ::nwLeft, ::nwBottom - ::nFooterHeight - if( ::lFootSep .OR. ::lColFootSep, 1, 0 ), ::nwRight }

   ASize( ::aVisibleCols, ::nwRight - ::nwLeft + 1 )
   n := ::nwLeft - 1
   for each xVal in ::aVisibleCols
      xVal := HB_EnumIndex() + n
   next
   #endif

   // FSG - 14/06/2006 - added resetting of data positioning
   //                    that I can get assigninng a value to directly ::RowPos using an array browse
   IF nMode == 0
      IF ::nRecsToSkip <> 0
         ::nNewRowPos := ::nRowPos + ::nRecsToSkip
         ::oDataCache:nCurRow := ::nNewRowPos
         ::nRecsToSkip := 0
      ENDIF
      ::RefreshAll()
   ENDIF

   //   Flag that browser has been configured properly
   ::lConfigured := .t.

Return Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                        Columns Management
//
//-------------------------------------------------------------------//

METHOD AColInfo( oCol,lAdd ) CLASS Tbrowse
   LOCAL aCol

   DEFAULT lAdd TO .f.

   if !lAdd  .and. HB_ISOBJECT( oCol ) .and. ( valtype( oCol:block ) == 'B' )
      aCol := { oCol, valtype( Eval( oCol:block )), ::SetColumnWidth( oCol ),;
                '', '', '', 0, '', 0, oCol:defColor, .f., NIL, .t., 0 }
   else
      aCol := { oCol, '', 0, '', '', '', 0, '', 0, {}, .f., NIL, .t., 0 }
   endif

Return aCol

//-------------------------------------------------------------------//
//
//   Adds a TBColumn object to the TBrowse object
//
METHOD AddColumn( oCol ) CLASS TBrowse

   ::Moved()

   aadd( ::aColsInfo, ::AColInfo( oCol,.t. ) )

   ::nColumns++

   if ::nColumns == 1
      ::leftVisible := 1
      ::nColPos     := 1
   endif

   if !::lNeverDisplayed
      ::Configure( 1 )
   endif
   ::lConfigured := .f.

Return Self

//-------------------------------------------------------------------//
//
//   Inserts a column object in a browse
//
METHOD InsColumn( nPos, oCol ) CLASS TBrowse

   if 0 < nPos
      ::Moved()

      if nPos > ::nColumns
         aAdd( ::aColsInfo, ::AColInfo( oCol ) )
      else
         aIns( ::aColsInfo, nPos, ::AColInfo( oCol ), .t. )
      endif

      ::nColumns++

      if !( ::lNeverDisplayed )
         ::Configure( 1 )
      endif
      ::lConfigured := .f.
   endif

Return oCol

//-------------------------------------------------------------------//
//
//   Replaces one TBColumn object with another
//
METHOD SetColumn( nColumn, oCol ) CLASS TBrowse

   LOCAL oOldCol

   if 0 < nColumn .AND. nColumn <= ::nColumns
      ::Moved()

      oOldCol := ::aColsInfo[ nColumn, o_Obj ]

      ::aColsInfo[ nColumn ] := ::AColInfo( oCol )

      if !( ::lNeverDisplayed )
         ::Configure( 1 )
      endif
      ::lConfigured := .f.
   endif

Return oOldCol

//-------------------------------------------------------------------//
//
//   Gets a specific TBColumn object
//
METHOD GetColumn( nColumn ) CLASS TBrowse

Return iif( 0 < nColumn .AND. nColumn <= ::nColumns, ::aColsInfo[ nColumn, o_Obj ], NIL )

//-------------------------------------------------------------------//
//
//  Delete a column given the column position
//
METHOD DelColumn( nPos ) CLASS TBrowse

   LOCAL oCol

   if nPos > ::nColumns .or. nPos < 1
      // For clipper compatibility we must call Errorsys with error 1132
      Throw( ErrorNew( "BASE", 0, 1132, , "Bound error: array access" ) )
      return NIL
   endif

   IF ::nPrevDelColPos = 0
      ::nPrevDelColPos := ::nColPos
   ENDIF

   //  Need to adjust variables in case last column is deleted
   //  Fixes and important bug
   //
   ::Moved()

   oCol := ::aColsInfo[ nPos, o_Obj ]

   if nPos == ::nColPos .or. nPos == ::nColumns .or.;
              ::nColPos == ::nColumns .or. ::rightVisible == ::nColumns

      if ::leftVisible == ::rightVisible .AND. ::leftVisible > 1
         ::leftVisible--
      endif
      ::rightVisible--
      //::colPos++
      if ::ncolPos == ::nColumns
         ::ncolpos--
      endif
   endif

   ::nColumns--

   aDel( ::aColsInfo, nPos, .t. )

   if ::nColumns < ::nFrozenCols
      ::nFrozenCols := 0
   endif

   if ::nColumns == 0
      ::lNeverDisplayed := .t.
      ::aRedraw := {}
   endif

   if !::lNeverDisplayed
      ::Configure( 1 )
   endif
   ::lConfigured := .f.

Return oCol

//-------------------------------------------------------------------//
//
//   Returns the display width of a particular column
//
METHOD ColWidth( nColumn ) CLASS TBrowse

Return iif( 0 < nColumn .AND. nColumn <= ::nColumns, ::aColsInfo[ nColumn, o_Width ], NIL )

//-------------------------------------------------------------------//

METHOD SetFrozenCols( nHowMany, lLeft ) CLASS TBrowse

   LOCAL nCol, aCol
   LOCAL nOldFreeze      := ::nFrozenCols
   LOCAL nOldFrozenWidth := ::nFrozenWidth

   Default lLeft to .f.

   ::nFrozenCols  := Min( nHowMany, ::nColumns )

   // Space inside TBrowse window reserved for frozen columns
   ::nFrozenWidth := 0

   // If I've never displayed this TBrowse before I cannot calc occupied space since
   // columns:width is not yet set, ::Stabilize() will call me later
   //
   if ! ::lNeverDisplayed

      if nHowMany > 0
         for each aCol in ::aColsInfo
            nCol := HB_EnumIndex()
            if nCol <= nHowMany
               ::nFrozenWidth += aCol[ o_Width ]
               if nCol < ::nColumns .and. aCol[ o_Width ] > 0
                  ::nFrozenWidth += ::aColsInfo[ nCol + 1, o_SepWidth ]
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
         ::RefreshAll()
      endif

      if ( ::nFrozenCols < nOldFreeze .or. ::nColPos <= ::nFrozenCols .or.;
           ::nFrozenWidth < nOldFrozenWidth ) .and. ::nFrozenCols > 0
         FOR EACH aCol IN ::aColsInfo
            // Reset column widths
            //
            aCol[ o_Width     ] := ::SetColumnWidth( aCol[ o_Obj ] )
            aCol[ o_WidthCell ] := Min( aCol[ o_Width ], LenVal( Eval( aCol[ o_Obj ]:block ), aCol[ o_Type ], aCol[ o_Obj ]:Picture ) )
            aCol[ o_SetWidth  ] := .f.
         NEXT
      endif

      FOR EACH aCol IN ::aColsInfo
         if HB_EnumIndex() > ::nFrozenCols
            if ::nFrozenCols > 0
               // If there are columns which are larger than TBrowse display width minus
               // frozen columns reserved space, shrihnk them to fit
               //
               if ::nFrozenWidth + aCol[ o_Width ] > ::nVisWidth
                  aCol[ o_Width     ] := ::nVisWidth - ::nFrozenWidth
                  aCol[ o_WidthCell ] := Min( aCol[ o_Width ], LenVal( Eval( aCol[ o_Obj ]:block ), aCol[ o_Type ], aCol[ o_Obj ]:Picture ) )
                  aCol[ o_SetWidth  ] := .t.
               endif

            else
               // Reset column widths
               //
               aCol[ o_Width     ] := ::SetColumnWidth( aCol[ o_Obj ] )
               aCol[ o_WidthCell ] := Min( aCol[ o_Width ], LenVal( Eval( aCol[ o_Obj ]:block ), aCol[ o_Type ], aCol[ o_Obj ]:Picture ) )
               aCol[ o_SetWidth  ] := .f.
            endif
         endif
      NEXT

      if lLeft
         if ::nFrozenCols > 0
            if ::nColPos <= ::nFrozenCols
               do while .t.
                  ::leftVisible := ::LeftDetermine()

                  if ::leftVisible == ::nFrozenCols + 1 .or. ::nColumns == ::nFrozenCols
                     exit
                  endif

                  if ::rightVisible > ::nFrozenCols .and. ::leftVisible > ::nFrozenCols + 1
                     ::rightVisible--
                  else
                     ::rightVisible++
                  endif
               enddo
            else
               do while .t.
                  ::leftVisible := ::LeftDetermine()

                  if ::nColPos >= ::leftVisible .and. ::nColPos <= ::rightVisible
                     exit
                  endif

                  if ::nColPos < ::rightVisible
                     ::rightVisible--
                  else
                     ::rightVisible++
                  endif
               enddo
            endif
         else
            do while .t.
               ::leftVisible := ::LeftDetermine()

               if ::nColPos >= ::leftVisible
                  exit
               endif

               ::rightVisible--
            enddo
         endif
      endif
   endif

Return nHowMany

//-------------------------------------------------------------------//

METHOD SetColumnWidth( oCol ) CLASS TBrowse

   LOCAL xRes, cType, nTokenPos := 0, nL, cHeading, cFooting
   LOCAL nWidth := 0, nHeadWidth := 0, nFootWidth := 0, nLen := 0

   // if oCol has :Width property set I use it
   //
   if oCol:Width <> nil

      nWidth := Min( oCol:Width, ::nVisWidth )

   else

      if ISBLOCK( oCol:block )

         xRes     := Eval( oCol:block )
         cType    := Valtype( xRes )

         // FSG - 2004/02/27 - Fixed Lenght. It's enough to use transform.
         //nLen   := Len( Transform( xRes, oCol:Picture ) )
         // Paul- 2004/06/18 - Unfortunatly, an array browse of Dates doesn't
         //       work with the above, so back to LenVal() (is the problem transform?)
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

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Up-down movements
//
//-------------------------------------------------------------------//

METHOD Down() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip++

Return Self

//-------------------------------------------------------------------//

METHOD Up() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip--

Return Self

//-------------------------------------------------------------------//

METHOD PageDown() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip := ( ::RowCount - ::nRowPos ) + ::RowCount
   ::lPaintBottomUp := .T.

Return Self

//-------------------------------------------------------------------//

METHOD PageUp() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip := - ( ( ::nRowPos - 1 ) + ::RowCount )

Return Self

//-------------------------------------------------------------------//

METHOD GoBottom() CLASS TBrowse

   LOCAL nToTop

   ::Moved()

   // Skips back from last record as many records as TBrowse can hold
   nToTop := ::oDataCache:dbGoBottom()
   ::nRecsToSkip := ::RowCount - 1

   //   From top of TBrowse new row position is nToTop + 1 records away
   ::nNewRowPos := nToTop + 1

   ::nRowPos := 1
   ::RefreshAll()

Return Self

//-------------------------------------------------------------------//

METHOD GoTop() CLASS TBrowse

   ::Moved()

   ::oDataCache:dbGoTop()
   // required for compatibility
   ::oDataCache:dbSkip( 0 )

   ::nNewRowPos := 1
   ::RefreshAll()

Return Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                        Left-right movemets
//
//-------------------------------------------------------------------//

METHOD Home() CLASS TBrowse

   ::Moved()

   if ::nColPos <> ::leftVisible
      ::nColPos := ::leftVisible
      ::Invalidate()
   endif

Return Self

//-------------------------------------------------------------------//

METHOD End() CLASS TBrowse

   ::Moved()

   if ::nColPos < ::rightVisible
      ::nColPos := ::rightVisible
      ::Invalidate()
   else
      // Can go "out of bounds", here we behave like clipper
      ::nColPos := ::rightVisible
   endif

Return Self

//-------------------------------------------------------------------//

METHOD _Right() CLASS TBrowse

   ::Moved()

   if ::nColPos < ::rightVisible
      ::nColPos++
   else
      if ::nColPos < ::nColumns

         ::rightVisible++
         ::leftVisible := ::LeftDetermine()
         ::nColPos++
         ::Invalidate()
      else
         /* 09/08/2004 - <maurilio.longo@libero.it>
                         In a ! ::stable state clipper moves ::colpos past ::ColCount or
                         before column 1, so here and on _Left(), Home(), End(), PanEnd()
                         PanHome() methods I let it go "out of bounds",
                         PerformStabilization() gives ::nColPos a correct value
         */
         ::nColPos++
      endif
   endif

Return Self

//-------------------------------------------------------------------//

METHOD SetColPos( nColPos ) CLASS TBrowse

   default nColPos to 0

   if ::lNeverDisplayed .OR. (! ::stable .AND. nColPos <= ::rightVisible .AND. nColPos >= ::leftVisible)
      return ::nColPos := nColPos

   elseif nColPos <> 0 .AND. nColPos <> ::nColPos

      ::Moved()

      // I'm still inside columns currently shown
      if nColPos <= ::rightVisible  .AND.;
         nColPos >= Max( ::leftVisible, ::nFrozenCols )

         ::nColPos := nColpos

      else
         // moving to the right
         if nColPos > ::nColPos

            if nColPos <= ::nColumns

               ::rightVisible := nColPos
               ::leftVisible := ::LeftDetermine()
               ::nColPos := nColPos

            endif

         // moving to the left
         else

            if nColPos >= 1

               // Until the leftmost column has a number higher than the column I want to reach
               while ::leftVisible > nColPos

                  ::rightVisible--
                  ::leftVisible := ::LeftDetermine()

               enddo

               ::nColPos := nColPos

            endif

         endif

         ::Invalidate()

      endif

      /* 23/11/2005 - <maurilio.longo@libero.it>
                      Clipper compatibility, after a colpos assignement, clipper tbrowse is ::stable
      */
      ::ForceStable()

   endif

return ::nColPos

//-------------------------------------------------------------------//

METHOD _Left() CLASS TBrowse

   LOCAL leftVis

   ::Moved()

   IF ::nColPos > ::leftVisible .OR.;
         ( ::nColPos <= ::nFrozenCols + 1 .and. ::nColPos > 1 )
      ::nColPos--
   ELSE
      IF ::nColPos <= Max( ::leftVisible, ::nFrozenCols ) .AND. ::nColPos > 1
         leftVis := ::leftVisible
         WHILE leftVis == ::leftVisible
            ::rightVisible--
            ::leftVisible := ::LeftDetermine()
         END
         ::nColPos--
         ::Invalidate()
      ELSE
         // Can go "out of bounds", here we behave like clipper
         ::nColPos--
      ENDIF
   ENDIF

Return Self

//-------------------------------------------------------------------//

METHOD PanEnd() CLASS TBrowse

   ::Moved()

   if ::nColPos < ::nColumns
      if ::rightVisible <  ::nColumns
         ::rightVisible := ::nColumns
         ::leftVisible  := ::LeftDetermine()
         ::nColPos      := ::RightVisible
         ::Invalidate()
      else
         ::nColPos := ::RightVisible
         /* 18/10/2005 - <maurilio.longo@libero.it>
                         This is wrong, should not reload datasource. But ::Invalidate()
                         forces a full repaint, which is overkill. Maybe just a ::aRedraw[ x ] := .T.
         */
         ::RefreshCurrent()
      endif
   else
      // Can go "out of bounds", here we behave like clipper
      ::nColPos := ::nColumns
   endif

Return Self

//-------------------------------------------------------------------//

METHOD PanHome() CLASS TBrowse

   ::Moved()

   if ::nColPos > 1
      if ::leftVisible > ::nFrozenCols + 1
         ::leftVisible  := ::nFrozenCols + 1
         ::nColPos      := 1
         ::Invalidate()
      else
         ::nColPos := 1
         ::RefreshCurrent()
      endif
   else
      // Can go "out of bounds", here we behave like clipper
      ::nColPos := 1
   endif

Return Self

//-------------------------------------------------------------------//

METHOD PanLeft() CLASS TBrowse

   LOCAL leftVis

   ::Moved()

   // The same as if ::leftVisible > iif(::nFrozenCols > 0, ::nFrozenCols + 1, 1)
   if ::leftVisible > ::nFrozenCols + 1

      leftVis := ::leftVisible

      /* While space left available by columns exiting to the right side of tbrowse
         is not enough to contain a new column to the left (::leftVisible doesn't change)
      */
      while leftVis == ::leftVisible

         ::rightVisible--
         ::leftVisible := ::LeftDetermine()

      enddo

      /* Since panel "shifts" to the right, ::ncolPos could end up "out of" the
         right side of tbrowse, so, change it to ::rightvisible if this happens
      */
      ::nColPos := Min( ::nColPos, ::rightVisible )

      ::Invalidate()
   endif

Return Self

//-------------------------------------------------------------------//

METHOD PanRight() CLASS TBrowse

   LOCAL leftVis

   /* 10/08/2004 - <maurilio.longo@libero.it>
                   Since this method can even not execute any task,
                   I think ::Moved() could be called inside outmost
                   if...
   */
   ::Moved()

   if ::rightVisible < ::nColumns

      leftVis := ::leftVisible

      while leftVis == ::leftVisible

         ::rightVisible++
         ::leftVisible  := ::LeftDetermine()

      enddo

      ::nColPos := Max( ::nColPos, ::leftVisible )

      ::Invalidate()
   endif

Return Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//          Movement keys cause TBrowse to become unstable
//
//-------------------------------------------------------------------//

METHOD Moved() CLASS TBrowse

   // Internal flags used to set ::HitTop/Bottom during next stabilization
   //
   ::lHitTop    := .F.
   ::lHitBottom := .F.

   // No need to Dehilite() current cell more than once
   //

   if ::stable
      if ::AutoLite
         ::DeHilite()
      else
         ::PosCursor()
      endif
      ::stable := .F.
   endif

Return Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Utility Routines
//
//-------------------------------------------------------------------//

METHOD LeftDetermine() CLASS TBrowse

   LOCAL nWidth := ::nFrozenWidth
   LOCAL nCol   := ::rightVisible

   // If ::nFrozenCols > 0 I don't need to test nCol > 0, if 0 it is the same test
   while nCol > ::nFrozenCols .AND.;
         ( nWidth += ::aColsInfo[ nCol, o_Width ] + ::aColsInfo[ nCol , o_SepWidth ] ) < ::nVisWidth

      nCol--
   enddo

   /* Clipper compatible: do not let nCol stop at empty column */
   nCol++
   while nCol <= ::rightVisible .and. ::aColsInfo[ nCol, o_Width ] == 0
      nCol++
   enddo

       /* ::rightVisible could be larger then available space, for example because of
          frozen columns, so nCol-- never gets executed  */
Return Min(nCol, ::rightVisible)

//-------------------------------------------------------------------//
//
//  Calculate how many columns fit on the browse width including ColSeps
//
METHOD HowManyCol() CLASS TBrowse
   LOCAL aColsInfo    := ::aColsInfo
   LOCAL colPos       := ::ncolPos
   LOCAL nToAdd       := 0
   LOCAL nColsVisible, nColsWidth, n
   LOCAL nLeftCol, tryLeftVisible, saveColsWidth, oErr

   nColsWidth   := 0
   nColsVisible := 0

   if ::nFrozenCols > 0
      nColsVisible := 0
      while nColsVisible < ::nFrozenCols .and. nColsVisible < ::nColumns
         nToAdd := ::aColsInfo[ nColsVisible + 1, o_Width ]

         if nColsVisible >= 1 .and. nColsVisible < ::nColumns .and.;
                                               ::aColsInfo[ nColsVisible,o_Width ] > 0
            nToAdd += ::aColsInfo[ nColsVisible + 1, o_SepWidth ]
         endif

         if nColsWidth + nToAdd > ::nVisWidth
            exit
         endif

         nColsWidth += nToAdd
         nColsVisible++
      enddo

      if nColsWidth + nToAdd > ::nVisWidth .and. nColsVisible < ::nFrozenCols
         // NOTE: Why do I change frozen columns here?
         //
         ::Freeze       := 0
         ::nColsWidth   := 0
         ::rightVisible := nColsVisible
         ::nColsVisible := nColsVisible
         return Self

      endif

      if ::leftVisible <= ::nFrozenCols
         ::leftVisible := ::nFrozenCols + 1
      endif

   endif

   // BDj notes:
   // Cannot assume that ::leftVisible is correct
   // (eg. if ::colPos was assigned ::rightVisible+1)
   // Must do the following in a loop repeatedly until:
   // (0) ::colPos <= ::nFrozenCols (assume ::colPos > 0)
   // or
   // (1) ::leftVisible <= ::colPos <= ::rightVisible
   // or
   // (2) the above conditions are impossible (runtime error)

   saveColsWidth  := nColsWidth
   tryLeftVisible := ::leftVisible

   // ::nColPos is to the left of leftVisible
   if ::nFrozenCols == 0 .AND. tryLeftVisible > ::nColPos
      tryLeftVisible := ::nColPos
   endif

   do while .t.
      nColsVisible := Max( 0, tryLeftVisible - 1 )

      while nColsVisible < ::nColumns
         // which column is displayed to the left of next col?
         if ::nFrozenCols > 0 .and. nColsVisible+1==tryLeftVisible
            nLeftCol := ::nFrozenCols
         else
            nLeftCol := nColsVisible
         endif

         nToAdd := ::aColsInfo[ nColsVisible + 1, o_Width ]

         // next, we must check against [nLeftCol], not [nColsVisible]:
         if ( nColsVisible >= tryLeftVisible .or. ::nFrozenCols > 0 ) .and.;
            (nLeftCol > 0) .and.;
            ::aColsInfo[ nLeftCol,o_Width ] > 0

            nToAdd += ::aColsInfo[ nColsVisible + 1, o_SepWidth ]
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

      // not ok. can retry?
      if tryLeftVisible==::nColumns
         // cannot fit ::nColPos into display
         // Generate Error TBROWSE
         //
         oErr := ErrorNew()
         oErr:severity    := ES_ERROR
         oErr:genCode     := EG_LIMIT
         oErr:subSystem   := "TBROWSE"
         oErr:subCode     := 0
         oErr:description := "Width limit exceeded"
         oErr:canRetry    := .F.
         oErr:canDefault  := .F.
         oErr:fileName    := ""
         oErr:osCode      := 0
         Eval( ErrorBlock(), oErr )
      endif

      // retry
      tryLeftVisible++
      nColsWidth := saveColsWidth
   enddo //retry until ::nColPos fit into display

   ::leftVisible  := Max( 1, tryLeftVisible )
   ::rightVisible := Max( 1, nColsVisible )
   ::nColsVisible := Max( 1, nColsVisible )
   ::nColsWidth   := nColsWidth
   ::cSpacePre    := space( INT( ( ::nVisWidth - ::nColsWidth ) / 2 ) )
   ::cSpaceLast   := space( ::nVisWidth - len( ::cSpacePre ) - ::nColsWidth )

Return Self

//-------------------------------------------------------------------//
//
// Gets TBrowse width and width of displayed columns plus colsep
//
METHOD RedrawHeaders( nWidth ) CLASS TBrowse

   LOCAL n, nTPos, nBPos
   LOCAL cBlankBox := Space( 9 )
   LOCAL nScreenRowT, nScreenRowB
   LOCAL nLCS
   LOCAL nCol
   LOCAL nColFrom
   LOCAL chSep, cfSep, nSpacePre, nSpaceLast, nLeftCol
   LOCAL ccSep, ncSepWidth
   // FSG - 14/06/2006 - Added below vars to force no displaying in case of limited space
   LOCAL lDrawHeaders := ::lHeaders
   LOCAL lDrawHeadSep := ( ::lHeadSep .OR. ::lColHeadSep )
   LOCAL lDrawFootSep := ( ::lFootSep .OR. ::lColFootSep )
   LOCAL lDrawFooters := ::lFooters

   DispBegin()

   nSpacePre := INT( ( nWidth - ::nColsWidth ) / 2 )
   nSpaceLast := nWidth - nSpacePre - ::nColsWidth

   nCol := ::nwLeft + iif( ::nFrozenCols > 0, 0, nSpacePre )

   nColFrom := iif( ::nFrozenCols > 0, 1, ::leftVisible )

   ::aColumnsSep := {}

   for n := nColFrom to ::rightVisible
      ::aColsInfo[ n, o_ScrColPos ] := nCol

      nCol += ::aColsInfo[ n, o_Width ]

      if n < ::rightVisible
         if ::aColsInfo[ n,o_Width ] > 0
            aadd( ::aColumnsSep, nCol + int( ::aColsInfo[ n + 1, o_SepWidth ] / 2 ) )
            nCol += ::aColsInfo[ n + 1, o_SepWidth ]
         endif
      endif

      if ::nFrozenCols > 0 .and. n == ::nFrozenCols
         n    := ::leftVisible - 1
         nCol += nSpacePre
      endif
   next

   if lDrawHeaders          // Drawing headers
      // Clear area of screen occupied by headers
      //
      DispBox( ::nwTop, ::nwLeft, ::nwTop + ::nHeaderHeight - 1, ::nwRight, cBlankBox, ::cColorSpec )

      for n := nColFrom to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif
         setPos( ::nwTop, ::aColsInfo[ n, o_ScrColPos ] )

         ::WriteMLineText( ::aColsInfo[ n, o_Heading ], ;
                           ::aColsInfo[ n, o_Width ], .T., ;
                           hb_ColorIndex( ::cColorSpec,  ColorToDisp( ::aColsInfo[ n,o_Obj ]:DefColor, TBC_CLR_HEADING ) - 1 ) )
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

   // Draw headin/footing column separator
   for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible

      // colsep's width will be needed later
      ccSep := if( ::aColsInfo[ n,o_Obj ]:ColSep == nil, ::ColSep, ;
                              ::aColsInfo[ n,o_Obj ]:ColSep )

      ncSepWidth := if( ccSep == nil, 0, len(ccSep) )

      // which column is displayed to the left of current col?
      if ::nFrozenCols > 0 .and. n == ::leftVisible
         nLeftCol := ::nFrozenCols
      else
         nLeftCol := n - 1
      endif

      if (::nFrozenCols > 0  .and. n == ::nFrozenCols + 1) .or.;
         (::nFrozenCols == 0 .and. n == ::leftVisible )
         n     := ::leftVisible

         IF lDrawHeadSep
            // we need to draw headSep for the nSpacePre gap
            if ! Empty( chSep := if( ::aColsInfo[ n,o_Obj ]:HeadSep == nil, ::HeadSep, ;
                                     ::aColsInfo[ n,o_Obj ]:HeadSep ) )
               if nLeftCol > 0 .and. ::aColsInfo[ nLeftCol, o_Width ] > 0 .and.;
                  ::nFrozenCols > 0
                  DispOutAT( nScreenRowT, nTPos - min( len(chSep), ncSepWidth), chSep, ::cColorSpec )
               endif
               DispOutAT( nScreenRowT, nTPos, Replicate( Right( chSep, 1 ), nSpacePre ), ::cColorSpec )

            elseif ::lColHeadSep
               DispOutAT( nScreenRowT, nTPos, Replicate( Space(1), nSpacePre ), ::cColorSpec )

            endif
            nTPos += nSpacePre
         ENDIF

         // we need to draw footSep for the nSpacePre gap
         IF lDrawFootSep
            if ! Empty ( cfSep := if( ::aColsInfo[ n,o_Obj ]:FootSep == nil, ::FootSep, ;
                                      ::aColsInfo[ n,o_Obj ]:FootSep ) )

               if nLeftCol > 0 .and. ::aColsInfo[ nLeftCol, o_Width ] > 0 .and. ;
                  ::nFrozenCols > 0
                  DispOutAT( nScreenRowB, nBPos - min( len(cfSep), ncSepWidth), cfSep, ::cColorSpec )
               endif
               DispOutAT( nScreenRowB, nBPos, Replicate( Right( cfSep, 1 ), nSpacePre ), ::cColorSpec )

            elseif ::lColFootSep
               DispOutAT( nScreenRowB, nBPos, Replicate( Space(1), nSpacePre ), ::cColorSpec )

            endif
            nBPos += nSpacePre
         ENDIF

      endif

      // we need to handle even n == ::rightVisible in the following block

      if ::aColsInfo[ n, o_Width ] > 0 .and. n < ::rightVisible
         nLCS := ::aColsInfo[ n + 1, o_SepWidth ]
      else
         nLCS := 0
      endif

      IF lDrawHeadSep
         if ! Empty( chSep := if( ::aColsInfo[ n,o_Obj ]:HeadSep == nil, ::HeadSep, ;
                                  ::aColsInfo[ n,o_Obj ]:HeadSep ) )

            if nLeftCol>0 .and. n <> ::leftVisible .and. ::aColsInfo[ nLeftCol, o_Width ] > 0
               DispOutAT( nScreenRowT, nTPos - min( len(chSep), ncSepWidth), chSep, ::cColorSpec )
            endif
            DispOutAT( nScreenRowT, nTPos, Replicate( Right( chSep, 1 ), ::aColsInfo[ n, o_Width ] ), ::cColorSpec )

            nTPos += ::aColsInfo[ n, o_Width ] + nLCS

         /* If I haven't got a default separator or a colsep for current column, there could
            be a colsep on a next column, so I have to fill the width of this column with spaces.
         */
         elseif ::lColHeadSep
            DispOutAT( nScreenRowT, nTPos, Replicate( Space(1), ::aColsInfo[ n, o_Width ] + nLCS ), ::cColorSpec )
            nTPos += ::aColsInfo[ n, o_Width ] + nLCS

         endif
      ENDIF

      IF lDrawFootSep
         if ! Empty( cfSep := if( ::aColsInfo[ n,o_Obj ]:FootSep == nil, ::FootSep, ;
                                  ::aColsInfo[ n,o_Obj ]:FootSep ) )

            if Valtype(chSep) <> "U" .and. len(chSep) > len(cfSep)
               cfSep += Replicate( Right( cfSep, 1 ), Len( chSep ) - Len( cfSep ) )
            endif

            if nLeftCol > 0 .and. n <> ::leftVisible .and. ::aColsInfo[ nLeftCol, o_Width ] > 0
               DispOutAT( nScreenRowB, nBPos - min( len(cfSep), ncSepWidth), cfSep, ::cColorSpec )
            endif
            DispOutAT( nScreenRowB, nBPos, Replicate( Right( cfSep, 1 ), ::aColsInfo[ n, o_Width ] ), ::cColorSpec )

            nBPos += ::aColsInfo[ n, o_Width ] + nLCS

         elseif ::lColFootSep
            DispOutAT( nScreenRowB, nBPos, Replicate( Space(1), ::aColsInfo[ n, o_Width ] + nLCS ), ::cColorSpec )
            nBPos += ::aColsInfo[ n, o_Width ] + nLCS

         endif
      ENDIF

   next

   if nSpaceLast > 0

      IF lDrawHeadSep
         // right gap of spaces (nSpaceLast) on Header
         if ! Empty( chSep )
            DispOutAT( nScreenRowT, nTPos, Replicate( Right( chSep, 1 ), nSpaceLast ), ::cColorSpec )

         elseif ::lColHeadSep
            DispOutAT( nScreenRowT, nTPos, Replicate( Space(1), nSpaceLast ), ::cColorSpec )

         endif
      ENDIF

      IF lDrawFootSep
         // right gap of spaces (nSpaceLast) on Footer
         if ! Empty( cfSep )
            DispOutAT( nScreenRowB, nBPos, Replicate( Right( cfSep, 1 ), nSpaceLast ), ::cColorSpec )

         elseif ::lColFootSep
            DispOutAT( nScreenRowB, nBPos, Replicate( Space(1), nSpaceLast ), ::cColorSpec )

         endif
      ENDIF
   endif

   if lDrawFooters                // Drawing footers
      // Clear area of screen occupied by footers
      //
      DispBox( ::nwBottom - ::nFooterHeight + 1, ::nwLeft, ::nwBottom, ::nwRight, cBlankBox, ::cColorSpec )

      for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif
         setPos( ::nwBottom, ::aColsInfo[ n, o_ScrColPos ] )

         ::WriteMLineText( ::aColsInfo[ n, o_Footing ], ;
                           ::aColsInfo[ n, o_Width ], .F., ;
                           hb_ColorIndex( ::cColorSpec, ColorToDisp( ::aColsInfo[ n, o_Obj ]:DefColor, TBC_CLR_FOOTING ) - 1 ) )
      next
   endif

   DispEnd()

Return Self

//---------------------------------------------------------------------//

METHOD ColorRect( aRect, aColors ) CLASS TBrowse

   local i

   if ::lConfigured

      ::oDataCache:SetColorRect( { Max( aRect[ 1 ], 1 ),;
                                   Max( aRect[ 2 ], 1 ),;
                                   Min( aRect[ 3 ], ::rowCount ),;
                                   Min( aRect[ 4 ], ::colCount ),;
                                   aColors } )

      /* and now let's refresh new aRect covered area */
      If ! Empty( ::aRedraw )
         for i := Max( aRect[ 1 ], 1 ) to Min( aRect[ 3 ], ::rowCount )
            ::aRedraw[ i ] := .T.
         next
      endif

      ::Stable := .F.
      ::lRectPainting := .T.
      ::ForceStable()

   else

      // During ::Configure() I could change cache, so I save colorrect parameters
      AAdd( ::aRect, { Max( aRect[ 1 ], 1 ),;
                       Max( aRect[ 2 ], 1 ),;
                       Min( aRect[ 3 ], ::rowCount ),;
                       Min( aRect[ 4 ], ::colCount ),;
                       aColors } )
   endif


Return Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Stabilization
//
//-------------------------------------------------------------------//

METHOD CheckRowPos() CLASS TBrowse
   LOCAL nAvail           // How many records are avaialble?
   LOCAL lReset := .F.    // Reposition to row 1 required?

   // If ForceStable() is called after movement of data source
   // (in simple words, the record pointer is moved) where the
   // movement was not effected by TBrowse (e.g user set scope)
   // then TBrowse has to evaluate if painting should start
   // from relative to current cursor row.  If not it should know
   // how many rows of data is available for painting prior
   // to paiting relative to current cursor row.  If sufficient data is not
   // available then reset position to row 1.

   If ::nRecsToSkip == 0  // Ensure that you do not reset/* the painting
                          // of a movement caused by TBrowse methods

      If ::nRowPos <> 1     // No repositioning is required if current
                           // cursor row is one
         nAvail := ::EvalSkipBlock( 0 - ::nRowPos + 1 )

         // You should reposition only if there are too few records
         // available or there are more than sufficient records
         // available.  If there are exact number of records leave it.
         //
         lReset := Abs( nAvail )+1 <> ::nRowPos
         //lReset := Abs( nAvail ) + 1 <> ::nRowPos
         // Place back the data source pointer to where it was
         //
         ::EvalSkipBlock( 0 - nAvail )

      EndIf
   EndIf

//   TraceLog( 'Browser: nRowPos, nAvail, lReset, RecsToSkip', ::nRowPos, nAvail, lReset, ::nRecsToSkip )

   If lReset   // So repositioning was required !
      // nNewRowPos has to be updated
      // as we will entering phase 2 of stabilization
      //
      ::nRowPos := ::nNewRowPos := iif( Abs( nAvail ) > ::nRowPos, ::nRowPos, Abs( nAvail )  )
      ::Moved()

      // To ensure phase 1 is skipped
      ::nRecsToSkip := 0

      ::Invalidate()
      ::lRedrawFrame := .F.
   EndIf

Return Self
//-------------------------------------------------------------------//

METHOD ForceStable() CLASS TBrowse
   ::PerformStabilization( .T. )
Return self

//-------------------------------------------------------------------//

METHOD Stabilize() CLASS TBrowse
Return ::PerformStabilization( .F. )

//-------------------------------------------------------------------//

METHOD PerformStabilization( lForceStable ) CLASS TBrowse

   LOCAL nOldCursor                    // Current shape of cursor (which I remove before stabilization)
   LOCAL colorSpec
   LOCAL nRowToDraw

   if ::nColumns == 0
      // Return TRUE to avoid infinite loop ( do while !stabilize;end )
      return .t.
   endif


   // 2006/Jan/28 - E.F. - Clipper compatibility.
   // Restore ::nColPos after delcolumn(), because delcolumn() method
   // can be called more than one time before stabilization.
   IF ::nPrevDelColPos > 0
      ::nColPos := Min(::nPrevDelColPos,::nColumns)
      ::nPrevDelColPos := 0
   ENDIF


   /* First, since ::nColPos can go "out of bounds" we need
      to put 1 <= ::nColpos <= ::nColumns
      And we need to do this before calling ::Configure() which
      needs a ::nColPos "inside bounds"
   */
   ::nColPos := Max( Min( ::nColPos, ::nColumns ), 1)

   // Configure the browse if not configured . Pritpal Bedi
   //
   if ! ::lConfigured .or. ::lNeverDisplayed
      if ::lNeverDisplayed
         ::configure( 3 )
      endif
      ::configure( 2 )
   endif

   // Execute a pending invalidation of cache
   ::oDataCache:PerformInvalidation()

   // I need to set columns width If TBrowse was never displayed before
   //
   if ::lNeverDisplayed
      // NOTE: It must be before call to ::SetFrozenCols() since this call
      //       tests this iVar value, and I set it to .F. since I'm going to display TBrowse
      //       for first time
      //
      ::lNeverDisplayed := .F.

      // Force re-evaluation of frozen space since I could not calc it before
      // being columns width not set
      //
      if ::freeze > 0
         ::SetFrozenCols( ::freeze )
      endif

      /* 15/10/2005 - <maurilio.longo@libero.it>
                      Do we really need this call? and this method?
      */
      ::CheckRowPos()
   endif

   ColorSpec  := ::aColorSpec[ 1 ]
   nOldCursor := SetCursor( SC_NONE )

   if ::lRedrawFrame
      // Draw border
      //
      if Len( ::cBorder ) == 8
         @::nTop,::nLeft,::nBottom,::nRight BOX ::cBorder COLOR ::colorSpec
      endif

      // How may columns fit on TBrowse width?
      //
      ::HowManyCol()
      ::RedrawHeaders( ::nVisWidth )

      // Now that browser frame has been redrawn we don't need to redraw it unless
      // displayed columns change
      //
      ::lRedrawFrame := .F.

   endif

   /* From this point there is stabilization of rows which is made up of three phases
      - repositioning of data source _and_
      - redrawing of rows, after each row we exit stabilization loop with .F.
      - if all rows have been redrawn we set ::stable state to .T.
   */
   if ! ::stable

      // NOTE: I can enter here because of a movement key or a ::RefreshAll():ForceStable() call

      // Every time I enter stabilization loop I need to draw _at least_ one row.
      ::CheckRowsToBeRedrawn()

      DispBegin()

      while ( nRowToDraw := iif( ::lPaintBottomUp, RAScan( ::aRedraw, .T. ), AScan( ::aRedraw, .T. ) ) ) <> 0

         ::DrawARow( nRowToDraw )

         if ! lForceStable
            DispEnd()
            SetCursor( nOldCursor )
            return .F.
         endif

      enddo

      // If I reach this point I've repainted all rows so I can set ::stable state
      //
      // If I have fewer records than available TBrowse rows, cursor cannot be lower than
      // last record (note ::lHitBottom is set only during a movement)
      //

      // Here I use :nLastRow because I'm testing if cache has fewer records than
      // tbrowse displayable area
      if ::oDataCache:nLastRow < ::nNewRowPos
         ::nNewRowPos := ::oDataCache:nCurRow
      endif

      // If I'm not already under cursor I have to set data source to cursor position
      //
      if ::oDataCache:nCurRow <> ::nNewRowPos
         ::EvalSkipBlock( ::nNewRowPos - ::oDataCache:nCurRow )
      endif

      // new cursor position
      //
      ::nRowPos    := ::nNewRowPos
      ::HitTop    := ::lHitTop
      ::HitBottom := ::lHitBottom

      ::stable := .T.
      ::lPaintBottomUp := .F.
      DispEnd()

   endif

   // NOTE: DBU relies upon current cell being reHilited() even if already stable
   // 18/11/2005 - ::ColorRect() sets ::lRectPainting, when set, ::Hilite should not be called since we're only painting a color-rect region
   if ! ::lRectPainting .AND. ::AutoLite
      ::Hilite()
   else
      ::PosCursor()
   endif

   ::lRectPainting := .F.

   SetCursor( nOldCursor )

Return .T.

//-------------------------------------------------------------------//

METHOD CheckRowsToBeRedrawn() CLASS TBrowse
   LOCAL nRecsSkipped                  // How many records do I really skipped?
   LOCAL nFirstRow                     // Where is on screen first row of TBrowse?

   // If I have a requested movement still to handle
   //
   if ::nRecsToSkip <> 0
      // If I'm not under cursor
      // maybe I've interrupted an ongoing stabilization
      // I have to set data source to cursor position
      //
      if ::oDataCache:nCurRow <> ::nNewRowPos
         ::EvalSkipBlock( ::nNewRowPos - ::oDataCache:nCurRow )
      endif

      nRecsSkipped := ::EvalSkipBlock( ::nRecsToSkip )

      // I've tried to move past top or bottom margin
      //
      if nRecsSkipped == 0
         if ::nRecsToSkip > 0
            ::lHitBottom := .T.

         elseif ::nRecsToSkip < 0
            ::lHitTop := .T.

         endif

      elseif nRecsSkipped == ::nRecsToSkip
         // If after movement I'm still inside present TBrowse
         //
         if ( ::nNewRowPos + nRecsSkipped >= 1 ) .AND. ( ::nNewRowPos + nRecsSkipped <= ::RowCount )
            ::nNewRowPos += nRecsSkipped

            // This is needed since present TBrowse has no cache, so I need to repaint current row
            // rereading it from data source and to force rereading from data source I have to mark
            // row as invalid
            //
            ::aRedraw[ ::nNewRowPos ] := .T.

         else
            // It was K_PGDN or K_PGUP or K_UP or K_DN
            //
            if Abs( nRecsSkipped ) >= ::RowCount
               // K_PGDN
               //
               ::RefreshAll()

            else
               // K_DN or K_UP
               // Where does really start first TBrowse row?
               //
               nFirstRow := ::nRowData + 1

               // I'm at top or bottom of TBrowse so I can scroll
               //
               if ::nNewRowPos == ::RowCount
                  ScrollFixed( nFirstRow + nRecsSkipped - 1,;
                               ::nwLeft,;
                               nFirstRow + ::RowCount - 1,;
                               ::nwRight,;
                               nRecsSkipped )

               else
                  ScrollFixed( nFirstRow,;
                               ::nwLeft,;
                               nFirstRow + ::RowCount + nRecsSkipped,;
                               ::nwRight,;
                               nRecsSkipped )
               endif

               // I've scrolled on screen rows, now I need to scroll ::aRedraw array as well!
               //
               if nRecsSkipped > 0
                  ADel( ::aRedraw, 1 )
                  ::aRedraw[ -1 ] := .F.

               else
                  AIns( ::aRedraw, 1, .F. )

               endif

               ::aRedraw[ ::nNewRowPos ] := .T.
            endif
         endif

      else
         // I couldn't move as far as requested
         // I need to refresh all rows if I go past current top or bottom row
         //
         if ( ::nNewRowPos + nRecsSkipped < 1 ) .OR. ( ::nNewRowPos + nRecsSkipped > ::RowCount )
            // don't go past boundaries
            //
            ::nNewRowPos := iif( nRecsSkipped > 0, ::RowCount, 1 )
            ::RefreshAll()

         else
            ::nNewRowPos += nRecsSkipped
            ::aRedraw[ ::nNewRowPos ] := .T.

         endif

      endif

      // Data source moved, so next time I won't enter this stage of stabilization
      //
      ::nRecsToSkip := 0
      Return .t.
   endif

Return .f.

//-------------------------------------------------------------------//

METHOD DrawARow( nRow ) CLASS TBrowse

   LOCAL colorSpec, cColor
   LOCAL lColorRect
   LOCAL nColFrom
   LOCAL lDisplay
   LOCAL nCol, nRow2Fill, nLeftColPos
   LOCAL nRowsToSkip, nSkipped
   LOCAL cColBlanks                 // Enough Space()s to fill a column

   colorSpec  := ::aColorSpec[ 1 ]

   nColFrom   := iif( ::nFrozenCols > 0, 1, ::leftVisible )

   lDisplay := ! ::oDataCache:GetCell( nRow, iif( ::nFrozenCols > 0, 1, ::leftVisible ) ) == NIL

   if lDisplay

      if ::nFrozenCols == 0
         DispOutAt( nRow + ::nRowData, ::nwLeft, ::cSpacePre, ColorSpec )

         for nCol:= nColFrom to ::rightVisible
            ::DispCell( nRow, nCol, ::oDataCache:GetCell( nRow, nCol ), TBC_CLR_STANDARD )

            if nCol < ::rightVisible .and. ::aColsInfo[ nCol, o_lColSep ]
               DispOut( ::aColsInfo[ nCol + 1, o_ColSep ], ColorSpec )
            endif
         next

      else      //  ::nFrozenCols > 0
         setPos( nRow + ::nRowData, ::nwLeft )

         for nCol := nColFrom to ::rightVisible
            if nCol == ::nFrozenCols + 1
               nCol := ::leftVisible
               DispOut( ::cSpacePre, ColorSpec )
            endif

            ::DispCell( nRow, nCol, ::oDataCache:GetCell( nRow, nCol ), TBC_CLR_STANDARD )

            if nCol < ::rightVisible .and. ::aColsInfo[ nCol,o_lColSep ]
               DispOut( ::aColsInfo[ nCol + 1, o_ColSep ], ColorSpec )
            endif
         next

      endif

      DispOut( ::cSpaceLast, ColorSpec )

      // doesn't need to be redrawn
      ::aRedraw[ nRow ] := .f.

   else  // ! lDisplay
      /* 09/08/2004 - <maurilio.longo@libero.it>
         Here we fill the space from last row which has data up to ::rowcount
         so it is faster to work column wise instead of row wise since this
         saves us a lot of ::colorBlock evaluations to find column color and
         doesn't cost us nothing since we're simply writing spaces without
         calls to oCol:Block.
      */

      // We paint columns wise, so we need to keep track of the screen column where
      // current tbrowse column starts.
      nLeftColPos := ::nwLeft

      for nCol := nColFrom to ::rightVisible

         // needed here to calc correct column color
         if ::nFrozenCols > 0 .AND. nCol == ::nFrozenCols + 1
            nCol := ::leftVisible
         endif

         cColBlanks := Space( ::aColsInfo[ nCol, o_Width ] )

         /* 15/06/2006 - FSG - Empty rows have only standard browser color */
         cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( ::aColsInfo[ nCol, o_DefColor ], TBC_CLR_STANDARD ) - 1 )
         /* 15/06/2006 - FSG - commented out as per above
         // Let's find column color once per column
         if ::aColsInfo[ nCol, o_Obj ]:ColorBlock == NIL
            cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( ::aColsInfo[ nCol, o_DefColor ], TBC_CLR_STANDARD ) - 1 )
         else
            cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( Eval( ::aColsInfo[ nCol, o_Obj ]:ColorBlock, BlankValue( ::aColsInfo[ nCol ] ) ), TBC_CLR_STANDARD ) - 1 )
         endif
         */

         // Paint all remainig rows up to ::rowcount
         for nRow2Fill := nRow to ::rowCount

            if ::nFrozenCols == 0

               if nCol == nColFrom
                  DispOutAt( nRow2Fill + ::nRowData, nLeftColPos, ::cSpacePre, ColorSpec )
               else
                  setPos(nRow2Fill + ::nRowData, nLeftColPos)
               endif

               DispOut( cColBlanks, cColor )

               if nCol < ::rightVisible .and. ::aColsInfo[ nCol,o_lColSep ]
                  DispOut( ::aColsInfo[ nCol + 1, o_ColSep ], ColorSpec )
               endif

            else
               if nCol == ::leftVisible
                  DispOutAt( nRow2Fill + ::nRowData, nLeftColPos, ::cSpacePre, ColorSpec )
               else
                  setPos( nRow2Fill + ::nRowData, nLeftColPos )
               endif

               DispOut( cColBlanks, cColor )

               if nCol < ::rightVisible .and. ::aColsInfo[ nCol,o_lColSep ]
                  DispOut( ::aColsInfo[ nCol + 1, o_ColSep ], ColorSpec )
               endif
            endif

            if nCol == ::rightVisible
               DispOut( ::cSpaceLast, ColorSpec )
            endif

         next

         // next tbrowse column starts from this screen column
         nLeftColPos := Col()

      next

      // Mark all remaining rows as drawn
      AFill(::aRedraw, .F., nRow)

   endif // lDisplay

Return .t.

//-------------------------------------------------------------------//
//
//                              Display
//
//-------------------------------------------------------------------//

/* 10/08/2004 - <maurilio.longo@libero.it>
                Clipper 5.2e TBrowse does not hilite a cell which is not already
                displayed on screen: (1)

                oBrowse:colpos := ::rightVisible + 1
                oBrowse:hilite():forceStable()

                shows next column to the right without cells hilited.
                So, I treat PosCursor(), Hilite() and DeHilite() the same way and
                make them skip their job when ::colPos is not already on screen

                (1) Clipper even has a bug that causes ::hilite() to shows a cell
                    outside of tbrowse area!
*/
METHOD PosCursor() CLASS TBrowse

   LOCAL nRow := ::nRowPos + ::nRowData
   LOCAL nCol

   /* Here I simply take care of being inside available columns,
      but I think there should be a more complex test to see if we are
      inside displayed tbrowse columns
   */
   if ::nColPos > 0 .AND. ::nColPos <= ::nColumns

      nCol := ::aColsInfo[ ::nColPos, o_ScrColPos ]

      Switch ::aColsInfo[ ::nColPos, o_Type ]
      case "N"
         if ::aColsInfo[ ::nColPos, o_Obj ]:Width == NIL
            nCol += ::aColsInfo[ ::nColPos, o_Width ] - ::aColsInfo[ ::nColPos, o_WidthCell ]
         endif
         exit

      case "L"
         // Always centered inside column
         nCol += Round( ( ::aColsInfo[ ::nColPos, o_Width ] - ::aColsInfo[ ::nColPos, o_WidthCell ] ) / 2, 0 )
         exit
      end

      SetPos( nRow, nCol )

      #ifdef HB_COMPAT_C53
      ::nRow := nRow
      ::nCol := nCol
      #endif

   endif

Return Self

//-------------------------------------------------------------------//

METHOD DeHilite() CLASS TBrowse

   LOCAL nRow := ::nRowPos + ::nRowData
   LOCAL nCol
   LOCAL nNotLeftCol    // Screen col position of first char of not left justified columns
   LOCAL xValue


   if ::nColPos > 0 .AND. ::nColPos <= ::nColumns

      nCol := ::aColsInfo[ ::nColPos, o_ScrColPos ]

      SetPos( nRow, nCol )

      xValue := ::oDataCache:GetCell( ::nRowPos, ::nColPos )

      /* 02/11/2005 - <maurilio.longo@libero.it>
                      Happens when browse is not ::stable, clipper does the same thing,
                      it clears current cell
      */
      if xValue == NIL
         xValue := BlankValue( ::aColsInfo[ ::nColPos ] )
      endif

      nNotLeftCol := ::DispCell( ::nRowPos, ::nColPos, xValue, TBC_CLR_STANDARD )

      SetPos( nRow, iif( nNotLeftCol <> NIL, nNotLeftCol, nCol ) )

   endif

Return Self

//-------------------------------------------------------------------//

METHOD Hilite() CLASS TBrowse

   LOCAL nRow := ::nRowPos + ::nRowData
   LOCAL nCol
   LOCAL nNotLeftCol    // Screen col position of first char of not left justified columns
   LOCAL xValue

   if ::nColPos > 0 .AND. ::nColPos <= ::nColumns

      nCol := ::aColsInfo[ ::nColPos, o_ScrColPos ]

      SetPos( nRow, nCol )

      xValue := ::oDataCache:GetCell( ::nRowPos, ::nColPos )

      /* 02/11/2005 - <maurilio.longo@libero.it>
                      Happens when browse is not ::stable, clipper does the same thing,
                      it clears current cell
      */
      if xValue == NIL
         xValue := BlankValue( ::aColsInfo[ ::nColPos ] )
      endif

      nNotLeftCol := ::DispCell( ::nRowPos, ::nColPos, xValue, TBC_CLR_ENHANCED )

      SetPos( nRow, iif( nNotLeftCol <> NIL, nNotLeftCol, nCol ) )

      #ifdef HB_COMPAT_C53
      ::nRow := nRow
      ::nCol := iif( nNotLeftCol <> NIL, nNotLeftCol, nCol )
      #endif

   endif

Return Self

//-------------------------------------------------------------------//

METHOD DispCell( nRow, nColumn, xValue, nColor ) CLASS TBrowse

   LOCAL aColsInfo := ::aColsInfo[ nColumn ]
   LOCAL oCol      := aColsInfo[ o_Obj ]
   LOCAL nWidth    := aColsInfo[ o_Width ]
   LOCAL nLen      := aColsInfo[ o_WidthCell ]

   // Screen col position of first char for not left justified columns
   LOCAL nNotLeftCol
   LOCAL cColor, cColorBKG, aCellColor

   // if called when the column type is not defined, then do nothing
   if Empty( aColsInfo[ o_Type ] )
      Return nil
   endif

   cColorBKG := hb_ColorIndex( ::cColorSpec, ColorToDisp( oCol:DefColor, TBC_CLR_STANDARD ) - 1 )

   aCellColor := ::oDataCache:GetCellColor( nRow, nColumn )

   // If cell has not a particular color ( colorblock or colorrect ) use defcolor ( as clipper does )
   if Empty( aCellColor )
      cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( oCol:DefColor, nColor ) - 1 )
   else
      cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( aCellColor, nColor ) - 1 )
   endif

   Switch aColsInfo[ o_Type ]
   case "C"
   case "M"
      // If there is not an explicit width use that of the first item
      if oCol:Width == NIL
         DispOut( PadR( Transform( xValue, aColsInfo[ o_Pict ] ), nLen ), cColor )
         DispOut( Space( nWidth - nLen ), cColorBKG )
      else
         DispOut( PadR( Transform( xValue, aColsInfo[ o_Pict ] ), nWidth ), cColor )
      endif

      exit

   case "N"
      if oCol:Width == NIL
         DispOut( Space( nWidth - nLen ), cColorBKG )
         nNotLeftCol := Col()
         DispOut( PadL( Transform( xValue, aColsInfo[ o_Pict ] ), nLen ), cColor )
      else
         DispOut( PadL( Transform( xValue, aColsInfo[ o_Pict ] ), nWidth ), cColor )
      endif

      exit

   case "D"
      DispOut( PadR( Transform( xValue, aColsInfo[ o_Pict ] ), nLen ), cColor )
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

Return nNotLeftCol

//-------------------------------------------------------------------//
/* from clipper NG:

   RefreshCurrent() --> self

   Internally marks the current data row as invalid, causing it to be
   refilled and redisplayed during the next stabilize loop.
*/
METHOD RefreshCurrent() CLASS TBrowse

   if ! Empty( ::aRedraw )

      ::aRedraw[ ::nRowPos ] := .T.
      ::oDataCache:Invalidate( ::nRowPos )

   endif

   ::Stable := .F.

Return Self


//-------------------------------------------------------------------//
//
//   NOTE: Not tested, could be broken
//
METHOD MGotoYX( nRow, nCol ) CLASS TBrowse

   LOCAL nColsLen, nI, nNewRow

   // Am I inside TBrowse display area ?
   //
   if nRow > ::nwTop  .AND. nRow < ::nwBottom .AND. ;
      nCol > ::nwLeft .AND. nCol < ::nwRight

      // if not stable force repositioning of data source; maybe this is not first Stabilize() call after
      // TBrowse became unstable, but we need to call Stabilize() al least one time before moving again to be sure
      // data source is under cursor position
      //
      if ! ::stable
         ::Stabilize()

      else
         ::Moved()

      endif

      // Set new row position
      // nNewRow := nRow - ::nwTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ), 0, 1 ) - 1
      nNewRow       := nRow - ::nRowData
      ::nRecsToSkip := nNewRow - ::nNewRowPos

      // move data source accordingly
      //
      ::Stabilize()

      // Now move to column under nCol
      //
      nColsLen := 0

      // NOTE: I don't think it is correct, have to look up docs
      //
      nI := iif( ::nFrozenCols > 0, ::nFrozenCols, ::leftVisible )

      while nColsLen < nCol .AND. nI < ::rightVisible

         nColsLen += ::aColsInfo[ nI, o_Width ]
         if nI >= 1 .AND. nI < ::nColumns
            nColsLen += ::aColsInfo[ nI+1, o_SepWidth ]
         endif

         nI++

      enddo

      ::nColPos := nI

      // Force redraw of current row with new cell position
      //
      ::RefreshCurrent()

   endif

Return Self

//-------------------------------------------------------------------//

METHOD WriteMLineText( cStr, nPadLen, lHeader, cColor ) CLASS TBrowse
   LOCAL n, cS
   LOCAL nCol := Col()
   LOCAL nRow := Row()
   LOCAL nTokens

   // Do I have to write an header or a footer?
   //
   if lHeader
      // Simple case, write header as usual
      //
      if ::nHeaderHeight == 1 .and. !( ";" IN cStr )
         DispOut( PadR( cStr, nPadLen ), cColor )

      else
         // __StrToken needs that even last token be ended with token separator
         //
         // Headers are aligned from bottom to top - FSG - 2004/02/27
         nTokens      := __StrTokenCount( cStr, ";" )
         cStr := Replicate( ";", ::nHeaderHeight - nTokens + 1 ) + cStr
         cS := cStr

         for n := ::nHeaderHeight to 1 step -1
            setPos( nRow + n - 1, nCol )
            DispOut( PadR( __StrToken( @cS, n, ";" ), nPadLen ), cColor )
         next

         setPos( nRow, nCol + nPadLen )

      endif

   // footer
   //
   else

      // Simple case, write footer as usual
      //
      if ::nFooterHeight == 1 .and. !( ";" IN cStr )
         DispOut( PadR( cStr, nPadLen ), cColor )

      else
         // __StrToken needs that even last token be ended with token separator
         //
         cS := cStr + ";"

         for n := 0 to ( ::nFooterHeight - 1 )
            setPos( nRow - n, nCol )
            DispOut( PadR( __StrToken( @cS, ::nFooterHeight - n, ";" ), nPadLen ), cColor )
         next

         setPos( nRow, nCol + nPadLen )

      endif

   endif

Return Self

//---------------------------------------------------------------------//

METHOD SetBorder( cBorder ) CLASS TBrowse

   if ISCHARACTER( cBorder ) .AND.;
      ( Len( cBorder ) == 0 .or. Len( cBorder ) == 8 )

      if ::cBorder == ""
         if cBorder == ""
            // Nothing
         else
            ::cBorder := cBorder
            ::nwTop++
            ::nwLeft++
            ::nwRight--
            ::nwBottom--
         endif
      else
         ::cBorder := cBorder
         if ::cBorder == ""
            ::nwTop--
            ::nwLeft--
            ::nwRight++
            ::nwBottom++
         endif
      endif
      ::Configure()
   endif

Return self

//---------------------------------------------------------------------//

// This method calculates variables related to horizontal coordinates
METHOD PreConfigHorizontal( uValue ) CLASS TBrowse

   ::lConfigured := .f.
   ::nVisWidth := ::nwRight - ::nwLeft + 1

Return uValue

//---------------------------------------------------------------------//

// This method calculates variables related to vertical coordinates
METHOD PreConfigVertical( uValue ) CLASS TBrowse

   ::lConfigured := .f.
   ::rowCount := ::nwBottom - ::nwTop + 1 - iif( ::lHeaders, ::nHeaderHeight, 0 ) - ;
                  iif( ::lFooters, ::nFooterHeight, 0 ) - ;
                  iif( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ) - ;
                  iif( ::lFootSep .OR. ::lColFootSep, 1, 0 )

Return uValue


//---------------------------------------------------------------------//
//
//                      Clipper 5.3b Compatibility
//
//---------------------------------------------------------------------//
#ifdef HB_COMPAT_C53
//---------------------------------------------------------------------//

METHOD ApplyKey( nKey )  CLASS TBrowse

Return ::TApplyKey( nKey, self )

//-------------------------------------------------------------------//

METHOD InitKeys( o ) CLASS TBrowse

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

//-------------------------------------------------------------------//

METHOD SetKey( nKey,bBlock ) CLASS TBrowse
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

//-------------------------------------------------------------------//

METHOD TApplyKey( nKey, oBrowse ) CLASS TBrowse
   LOCAL bBlock := oBrowse:setkey( nKey ), nReturn := TBR_CONTINUE  // 0

   DEFAULT bBlock TO oBrowse:setkey( 0 )

   if ISNIL( bBlock )
      nReturn := TBR_EXCEPTION  // 1
   else
      nReturn := eval( bBlock, oBrowse, nKey )
   endif

Return nReturn

//-------------------------------------------------------------------//
/*
METHOD HitTest( mrow,mcol ) CLASS TBrowse
   LOCAL i, nVisCol, lHitHeader := .f.
   LOCAL nColPos

   ::mRowPos := ::nRowPos
   ::mColPos := ::colPos

   if mRow < ::nTop .or. mRow > ::rect[ 3 ]
      return HTNOWHERE
   endif

   if mCol < ::rect[ 2 ] .or. mCol > ::rect[ 4 ]
      return HTNOWHERE
   endif

   ::mRowPos := mRow - ::rect[ 1 ] + 1
   // Is the header separator part of the "header" when click?
   if ::mRowPos < 1 - if( ::lHeadSep .OR. ::lColHeadSep , 1, 0 )
      lHitHeader := .t.
   endif

   nVisCol := len( ::aColumnsSep )

   if nVisCol == 0
      nColPos := 1

   elseif mcol >= ::aColumnsSep[ nVisCol ]
      nColPos := nVisCol + 1

   else
      for i := 1 to nVisCol
         if mcol < ::aColumnsSep[ i ]
            nColPos := i
            exit
         endif
      next
   endif

   if ::nFrozenCols > 0 .and. nColPos <= ::nFrozenCols
      // Do Nothing
   elseif ::nFrozenCols > 0 .and. nColPos > ::nFrozenCols
      nColPos := ::LeftVisible + nColPos - ::nFrozenCols - 1
   else
      nColPos := ::LeftVisible + nColPos - 1
   endif

   ::mColPos := nColPos

Return if( lHitHeader, HTHEADING, HTCELL )
*/

// FSG - 14/06/2006 - expanded HitTest()
METHOD HitTest( mrow,mcol ) CLASS TBrowse
   Local i, nVisCol
   LOCAL nRet

   if ( mRow < ::nwTop  .or. mRow > ::nwBottom ) .or. ;
      ( mCol < ::nwLeft .or. mCol > ::nwRight )
      // if I'm outside browse
      return HTNOWHERE
   endif

   // checking absolute position

   nRet := HTNOWHERE

   if !( ::cBorder == "" )

      if     mRow == ::nTop    .AND. mCol == ::nLeft
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

      if mCol >= ::nLeft    .AND. mCol <= ::nLeft + Len( ::cSpacePre ) - 1
             // if i'm on left side (also consider spaces on left)
        nRet := HTLEFT
      elseif mCol >= ::nRight - Len( ::cSpaceLast ) + 1 .AND. mCol <= ::nwRight
             // if i'm on right side (also consider spaces on right)
        nRet := HTRIGHT
      elseif ::lHeadSep     .AND. mRow == ::nTop + ::nHeaderHeight
         // if i'm on header sep
         nRet := HTHEADSEP
      elseif mRow >= ::nTop .AND. mRow <= ::nTop + ::nHeaderHeight
         // if i'm on header
         nRet := HTHEADING
      elseif ::lFootSep     .AND. mRow == ::nBottom - ::nFooterHeight
         // if i'm on footer sep
         nRet := HTFOOTSEP
      elseif mRow >= ::nBottom - ::nFooterHeight .AND. mRow <= ::nBottom
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

      ::mRowPos := mRow - ::rect[ 1 ] + 1

      nVisCol := len( ::aColumnsSep )

      if nVisCol == 0
         ::mColPos := 1

      elseif mcol >= ::aColumnsSep[ nVisCol ]
         ::mColPos := nVisCol + ::leftVisible - ::nFrozenCols

      else
         for i := 1 to nVisCol
            if mcol < ::aColumnsSep[ i ]
               ::mColPos := i + ::leftVisible - ::nFrozenCols - 1
               exit
            endif
         next
      endif

      // if browse has columns that fits exactly horizontally space and
      // I have no border and I'm already on first visible col or on last visible col,
      // then I assume that I want to move horizontally
      IF ::mRowPos == ::nRowPos .AND. ;
         ::mColPos == ::nColPos

         IF ::mColPos == ::leftVisible .AND. ;
            ::leftVisible - ::nFrozenCols > 1 .AND. ;
            Len( ::cSpacePre ) == 0
            ::mColPos--
         ELSEIF ::mColPos == ::rightVisible .AND. ;
            ::rightVisible < ::ColCount() .AND. ;
            Len( ::cSpaceLast ) == 0
            ::mColPos++
         ENDIF

      ENDIF

   endif

Return nRet

//-------------------------------------------------------------------//

METHOD SetStyle( nMode, lSetting ) CLASS TBrowse
   LOCAL lRet := .F.

   IF nMode > LEN( ::aSetStyle )
      RETURN .F.
   ENDIF

   lRet := ::aSetStyle[ nMode ]

   IF ISLOGICAL( lSetting )
      ::aSetStyle[ nMode ] := lSetting
   ENDIF

Return lRet

//---------------------------------------------------------------------//

METHOD EvalSkipBlock( nSkip ) CLASS TBROWSE
   LOCAL lSign   := nSkip >= 0
   LOCAL nSkipped := ::oDataCache:dbSkip( nSkip )

   /* 19/10/2005 - <maurilio.longo@libero.it>
                   Why do we do this?
   */
   if ( lSign .and. nSkipped < 0 ) .or. ( !lSign .and. nSkipped > 0 )
      nSkipped := 0
   endif

Return nSkipped

//-------------------------------------------------------------------//

function TBMOUSE( oBrowse, nMouseRow, nMouseCol )
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

//---------------------------------------------------------------------//
#endif
//---------------------------------------------------------------------//

static function LenVal( xVal, cType, cPict )
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

//-------------------------------------------------------------------//

static Function ArrayToList( aArray )

   LOCAL cList := "", a

   for each a in aArray
      cList += AllTrim( str( a ) ) + ", "
   next

   cList := SubStr( cList, 1, Len( cList ) - 2 )

Return cList

//-------------------------------------------------------------------//

static function Color2Array( cColorSpec )

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

//-------------------------------------------------------------------//

static function BlankValue( aColInfo )
local xValue

switch aColInfo[ o_Type ]
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
      xValue := Space( aColInfo[ o_WidthCell ] )
end
Return xValue

*----------------------------------
Static Function CacheOK( a, n, n2 )
*----------------------------------
* 2006/MAR/13 - E.F.
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

//-------------------------------------------------------------------//
//
//                   Function to Activate TBrowse
//
//-------------------------------------------------------------------//

function TBrowseNew( nTop, nLeft, nBottom, nRight )

Return TBrowse():New( nTop, nLeft, nBottom, nRight )

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
