/*
 * $Id: tbrowse.prg,v 1.66 2004/04/07 09:27:53 vouchcac Exp $
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
 * ::GotoXY(), ::DispCell(), ::WriteMLineText(), ::RedrawHeaders(),
 * ::SetFrozenCols(), ::SetColumnWidth()
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

#include "common.ch"
#include "hbclass.ch"
#include "color.ch"
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
#define o_SetWidth       11   // In True, only SetFrozen can change o_With
#define o_Blank          12   // Spaces corresponding o_Width
#define o_lColSep        13   // Should column separator be drawn

#define TBC_CLR_STANDARD  1
#define TBC_CLR_ENHANCED  2
#define TBC_CLR_HEADING   3
#define TBC_CLR_FOOTING   4

//-------------------------------------------------------------------//

CLASS TBrowse

   DATA lInitRow              // Logical value to control initial rowpos
   DATA autoLite              // Logical value to control highlighting
   DATA cargo                 // User-definable variable
   DATA goBottomBlock         // Code block executed by TBrowse:goBottom()
   DATA goTopBlock            // Code block executed by TBrowse:goTop()
   DATA hitBottom             // Indicates the end of available data
   DATA hitTop                // Indicates the beginning of available data
   DATA leftVisible           // Indicates position of leftmost unfrozen column in display
   DATA rightVisible          // Indicates position of rightmost unfrozen column in display
   DATA rowCount              // Number of visible data rows in the TBrowse display
   DATA rowPos                // Current cursor row position
   DATA skipBlock             // Code block used to reposition data source
   DATA stable                // Indicates if the TBrowse object is stable
   DATA aColumns
   DATA aColumnsSep           // Holds the column position where seperators are marked . for Wvt_DrawGridVert()
   DATA aColorSpec            // Holds colors of Tbrowse:ColorSpec
   DATA cSpacePre             // Blank Space prior to first column
   DATA cSpaceLast            // Blank space after the last column
   DATA cSpaceWidth           // Spaces of browse width
   DATA lRect

#ifdef HB_COMPAT_C53
   DATA nRow                  // Row number for the actual cell
   DATA nCol                  // Col number for the actual cell
   DATA aKeys
   DATA mColpos,mrowPos,message
#endif

   ACCESS border              INLINE ::cBorder
   ASSIGN border( cBorder )   INLINE ::SetBorder( cBorder )
   ACCESS colorSpec           INLINE ::cColorSpec   // Color table for the TBrowse display
   ASSIGN colorSpec(cColor)   INLINE if( empty( cColor ), ::cColorSpec, ( ::lConfigured := .f., ;
                                     ::aColorSpec := Color2Array( cColor ), ::cColorSpec := cColor ) )
   ACCESS colPos              INLINE ::nColPos
   ASSIGN colPos(nColPos)     INLINE ::lConfigured := .f., ::nColPos := if( nColPos == nil, ::nColPos, nColPos )

   ACCESS nBottom             INLINE ::nwBottom +  iif(::cBorder=="",0,1)
   ASSIGN nBottom( nBottom )  INLINE ::lConfigured := .f., ::nwBottom := nBottom - iif(::cBorder=="",0,1)
   ACCESS nLeft               INLINE ::nwLeft   -  iif(::cBorder=="",0,1)
   ASSIGN nLeft( nLeft )      INLINE ::lConfigured := .f., ::nwLeft   := nLeft   + iif(::cBorder=="",0,1)
   ACCESS nRight              INLINE ::nwRight  +  iif(::cBorder=="",0,1)
   ASSIGN nRight( nRight )    INLINE ::lConfigured := .f., ::nwRight  := nRight  - iif(::cBorder=="",0,1)
   ACCESS nTop                INLINE ::nwTop    -  iif(::cBorder=="",0,1)
   ASSIGN nTop( nTop )        INLINE ::lConfigured := .f., ::nwTop    := nTop    + iif(::cBorder=="",0,1)

   ACCESS colSep  INLINE ::cColSep        // Column separator character
   ASSIGN colSep( cColSep )   INLINE ::lConfigured := .f., ::cColSep  := cColSep
   ACCESS footSep INLINE ::cFootSep       // Footing separator character
   ASSIGN footSep( cFootSep ) INLINE ::lConfigured := .f., ::cFootSep := cFootSep
   ACCESS headSep INLINE ::cHeadSep       // Head separator character
   ASSIGN headSep( cHeadSep ) INLINE ::lConfigured := .f., ::cHeadSep := cHeadSep

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
   METHOD ColCount() INLINE ::nColumns
   METHOD ColorRect()                     // Alters the color of a rectangular group of cells
   METHOD Configure( nMode )              // Reconfigures the internal settings of the TBrowse object
                                          // nMode is an undocumented parameter in CA-Cl*pper
   METHOD DeHilite()                      // Dehighlights the current cell
   METHOD ForceStable()                   // Performs a full stabilization
   METHOD ForceStabilize()                // Identical to Stabilize but usable with ForceStable()
   METHOD Hilite()                        // Highlights the current cell
   METHOD Invalidate()                    // Forces entire redraw during next stabilization
   METHOD RefreshAll()                    // Causes all data to be recalculated during the next stabilize
   METHOD RefreshCurrent() INLINE;        // Causes the current row to be refilled and repainted on next stabilize
          If( ! Empty( ::aRedraw ), ::aRedraw[ ::RowPos ] := .T., Nil ), ::stable := .F., Self
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
   METHOD DispCell( nColumn, nColor, aColors )  // Displays a single cell and returns cell type as a single letter like Valtype()
   METHOD HowManyCol()                    // Counts how many cols can be displayed
   METHOD RedrawHeaders( nWidth )         // Repaints TBrowse Headers
   METHOD Moved()                         // Every time a movement key is issued I need to reset certain properties
                                          // of TBrowse, I do these settings inside this method

   METHOD WriteMLineText( cStr, nPadLen, lHeader, cColor ) // Writes a multi-line text where ";" is a line break, lHeader
                                                           // is .T. if it is a header and not a footer
   METHOD SetFrozenCols( nHowMany )       // Handles freezing of columns
   METHOD SetColumnWidth( oCol )          // Calcs width of given column
   METHOD SetBorder( cBorder )
   METHOD DrawRows()                      // Draws rows necessarility to be redrawn
   METHOD DrawARow()                      // Draws a single row in stabilize() method
   METHOD DispCellNormal()                // Most often used cell display method, removing all ifs and buts
   METHOD CheckRowsToBeRedrawn()

   DATA aRect                             // The rectangle specified with ColorRect()
   DATA aRectColor                        // The color positions to use in the rectangle specified with ColorRect()
   DATA aRedraw                           // Array of logical items indicating, is appropriate row need to be redraw
   DATA lHeaders                          // Internal variable which indicates whether there are column headers to paint
   DATA lFooters                          // Internal variable which indicates whether there are column footers to paint
   DATA lHeadSep                          // Internal variable which indicates whether there are line headers to paint
   DATA lFootSep                          // Internal variable which indicates whether there are line footers to paint
   DATA lRedrawFrame                      // True if I need to redraw Headers/Footers
   DATA nColsWidth                        // Total width of visible columns plus ColSep
   DATA nColsVisible                      // Number of columns that fit on the browse width
   DATA lHitTop                           // Internal Top/Bottom reached flag
   DATA lHitBottom
   DATA nRecsToSkip                       // Recs to skip on next Stabilize()
   DATA nNewRowPos                        // Next position of data source (after first phase of stabilization)
   DATA nLastRetrieved                    // Position, relative to first row, of last retrieved row (with an Eval(::SkipBlock, n))
   DATA nRowData                          // Row, first row of data
   DATA nColPos

   DATA nwBottom                          // Bottom row number for the TBrowse display
   DATA nwLeft                            // Leftmost column for the TBrowse display
   DATA nwRight                           // Rightmost column for the TBrowse display
   DATA nwTop                             // Top row number for the TBrowse display

   DATA cBorder
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
   DATA lDispBegin

   DATA aColsInfo                         // Columns configuration array
   DATA nVisWidth                         // Visible width of Browser
   DATA lConfigured                       // Specifies whether tBrowse is already configured or not

#ifdef HB_COMPAT_C53
   DATA rect
   DATA aVisibleCols
   DATA aSetStyle
#endif

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
   ::lInitRow        := .F.
   ::nRowData        := nTop
   ::lDispBegin      := .F.
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
   ::RowPos          := 1
   ::nNewRowPos      := 1
   ::stable          := .F.
   ::nLastRetrieved  := 1
   ::nRecsToSkip     := 0
   ::aRedraw         := {}
   ::lHeaders        := .F.
   ::lFooters        := .F.
   ::lHeadSep        := .F.
   ::lFootSep        := .F.
   ::lRedrawFrame    := .T.
   ::aRect           := {}
   ::aRectColor      := {}
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
   ::AColumns        := {}
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
   ::lRect           := .f.

   Return Self

//-------------------------------------------------------------------//

METHOD Invalidate() CLASS TBrowse

   AFill( ::aRedraw, .T. )

   ::stable       := .F.
   ::lRedrawFrame := .T.

   Return Self

//-------------------------------------------------------------------//

METHOD RefreshAll() CLASS TBrowse

   AFill( ::aRedraw, .T. )
   ::stable := .F.

   Return Self

//-------------------------------------------------------------------//

METHOD Configure( nMode ) CLASS TBrowse

   local n, nHeight, aCol, xVal, nFreeze, oErr, lInitializing := .f.

   default nMode to 0

   if nMode == 3
      nMode := 0
      lInitializing := .t.
   endif

   if ::nColPos > ::nColumns
      ::nColPos := ::nColumns
   endif

   if nMode < 2 .or. ::lNeverDisplayed

      ::lHeaders     := .F.
      ::lFooters     := .F.
      ::lRedrawFrame := .T.
      ::lHeadSep     := !Empty( ::cHeadSep )
      ::lFootSep     := !Empty( ::cFootSep )

      // Are there column headers to paint ?
      FOR EACH aCol IN ::aColsInfo
         if !Empty( aCol[ o_Heading ] )
            ::lHeaders := .T.
            exit
         endif
      NEXT

      // Are there column footers to paint ?
      FOR EACH aCol IN ::aColsInfo
         if !Empty( aCol[ o_Footing ] )
            ::lFooters := .T.
            exit
         endif
      NEXT

   endif

   ::nHeaderHeight := 0
   ::nFooterHeight := 0

   // Find out highest header and footer
   FOR EACH aCol IN ::aColsInfo

      if ( nMode <= 1 .and. !::lNeverDisplayed ) .or. lInitializing
         xVal := Eval( aCol[ o_Obj ]:block )

         aCol[ o_Type      ] := valtype( xVal )
         aCol[ o_Heading   ] := aCol[ o_Obj ]:heading
         aCol[ o_Footing   ] := aCol[ o_Obj ]:footing
         aCol[ o_Pict      ] := iif( Empty( aCol[ o_Obj ]:Picture ), "", aCol[ o_Obj ]:Picture )
         if !aCol[ o_SetWidth ]
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

         aCol[ o_Blank ]   := space( aCol[ o_Width ] )
         aCol[ o_lColSep ] := aCol[ o_Width ] > 0
      endif

      if nMode = 0 .or. nMode = 2 .or. lInitializing
         aCol[ o_ColSep    ] := iif( aCol[ o_Obj ]:ColSep != NIL, aCol[ o_Obj ]:ColSep, ::ColSep )
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

   if empty( ::aColorSpec )
      ::aColorSpec := Color2Array( ::cColorSpec )
   endif

   ::cSpaceWidth := space( ::nwRight - ::nLeft + 1 )

   if nMode == 1
      return Self
   endif

   do while .t.     // Reduce footer, headers and separator if the data
                    // not fit in the visible area.
                    // If if didn't fit, it generate error.

      ::nVisWidth := ::nwRight - ::nwLeft + 1

      // 20/nov/2000 - maurilio.longo@libero.it
      // If I add (or remove) header or footer (separator) I have to change number
      // of available rows
      //
      ::RowCount := ::nwBottom - ::nwTop + 1 - iif( ::lHeaders, ::nHeaderHeight, 0 ) - ;
                     iif( ::lFooters, ::nFooterHeight, 0 ) - iif( ::lHeadSep, 1, 0 ) - ;
                     iif( ::lFootSep, 1, 0 )

      if ::lNeverDisplayed
         exit
      endif

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
   ::nRowData := ::nwTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( ::lHeadSep, 1, 0 ) - 1

   if Len( ::aRedraw ) <> ::RowCount
      ::aRedraw := Array( ::RowCount )
   endif

   ::Invalidate()

   // Force re-evaluation of space occupied by frozen columns
   nFreeze := ::nFrozenCols
   if nFreeze > 0
      ::SetFrozenCols( 0 )
   endif
   ::SetFrozenCols( nFreeze, .t. )

#ifdef HB_COMPAT_C53
   ::Rect := { ::nwTop + ::nHeaderHeight + if( ::lHeadSep, 1, 0 ), ::nwLeft, ::nwBottom - ::nFooterHeight - if( ::lFootSep, 1, 0 ), ::nwRight }

   ASize( ::aVisibleCols, ::nwRight - ::nwLeft + 1 )
   n := ::nwLeft - 1
   for each xVal in ::aVisibleCols
      xVal := HB_EnumIndex() + n
   next
#endif

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
//
//   Adds a TBColumn object to the TBrowse object
//
METHOD AddColumn( oCol ) CLASS TBrowse

   ::Moved()

   ::nColumns++

   aadd( ::aColsInfo, { oCol, '', 0, '', '', '', 0, '', 0, {}, .f., '', .t. } )
   ::AColumns := ::aColsInfo

   if ::nColumns == 1
      ::leftVisible := 1
      ::nColPos     := 1
   endif

   if !::lNeverDisplayed
      ::Configure(1)
   endif
   ::lConfigured := .f.

   Return Self

//-------------------------------------------------------------------//
//
//   Inserts a column object in a browse
//
METHOD InsColumn( nPos, oCol ) CLASS TBrowse

   if 0 < nPos //.AND. nPos <= ::nColumns
      ::Moved()

      if nPos > ::nColumns
         aAdd( ::aColsInfo, { oCol, valtype( Eval( oCol:block ) ), ::SetColumnWidth( oCol ),;
                                        '', '', '', 0, '', 0, oCol:DefColor, .f., '', .t. } )
      else
         aIns( ::aColsInfo, nPos, { oCol, valtype( Eval( oCol:block ) ), ::SetColumnWidth( oCol ),;
                                        '', '', '', 0, '', 0, oCol:DefColor, .f., '', .t. }, .t. )
      endif

      ::aColumns := ::aColsInfo

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

      ::aColsInfo[ nColumn ] := { oCol, valtype( Eval( oCol:block ) ), ::SetColumnWidth( oCol ),;
                                              '', '', '', 0, '', 0, oCol:DefColor, .f., '', .t. }

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

   local oCol

   if nPos > ::nColumns .or. nPos < 1
      return NIL
   endif

   //  Need to adjust variables in case last column is deleted
   //  Fixes and important bug
   //

   ::Moved()

   oCol := ::aColsInfo[ nPos, o_Obj ]

   if nPos == ::nColPos .or. nPos == ::nColumns .or.;
              ::nColPos == ::nColumns .or. ::rightVisible == ::nColumns

      if ::leftVisible == ::rightVisible
         ::leftVisible--
      endif
      ::rightVisible--
//      ::colPos++
      if ::ncolPos == ::nColumns
         ::ncolpos--
      endif
   endif

   ::nColumns--

   aDel( ::aColsInfo, nPos, .t. )
   ::aColumns := ::aColsInfo

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
   if !( ::lNeverDisplayed )

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
         nLen     := Len( Transform( xRes, oCol:Picture ) )
         //nLen := LenVal( xRes, cType, oCol:Picture )

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
   ::nRecsToSkip := 1

   Return Self

//-------------------------------------------------------------------//

METHOD Up() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip := -1

   Return Self

//-------------------------------------------------------------------//

METHOD PageDown() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip := ( ::RowCount - ::RowPos ) + ::RowCount

   Return Self

//-------------------------------------------------------------------//

METHOD PageUp() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip := - ( ( ::RowPos - 1 ) + ::RowCount )

   Return Self

//-------------------------------------------------------------------//

METHOD GoBottom() CLASS TBrowse

   local nToTop

   ::Moved()

   Eval( ::goBottomBlock )

   //   Skip back from last record as many records as TBrowse can hold
   nToTop := Abs( EvalSkipBlock( ::SkipBlock, - ( ::RowCount - 1 ) ) )

   //   From top of TBrowse new row position is nToTop + 1 records away
   ::nNewRowPos := nToTop + 1

   //   Last read record is first record inside TBrowse
   ::nLastRetrieved := 1
   ::RefreshAll()

   Return Self

//-------------------------------------------------------------------//

METHOD GoTop() CLASS TBrowse

   ::Moved()

   Eval( ::goTopBlock )
   Eval( ::skipBlock,0 ) // required for compatibility
   ::nLastRetrieved := 1
   ::nNewRowPos     := 1
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

   if ::nColPos != ::leftVisible
      ::nColPos := ::leftVisible
      ::lRedrawFrame := .T.
      ::RefreshCurrent()
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD End() CLASS TBrowse

   ::Moved()

   if ::nColPos < ::rightVisible
      ::nColPos := ::rightVisible
      ::lRedrawFrame := .T.
      ::RefreshCurrent()
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
         ::lRedrawFrame := .T.
         ::RefreshAll()
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD _Left() CLASS TBrowse

   local leftVis := ::leftVisible

   ::Moved()

   if ::nColPos > ::leftVisible .or.;
         ( ::nColPos <= ::nFrozenCols + 1 .and. ::nColPos > 1 )
      ::nColPos--
   else
      if ::nColPos <= Max( ::leftVisible, ::nFrozenCols ) .AND. ::nColPos > 1
         while leftVis == ::leftVisible
            ::rightVisible--
            ::leftVisible := ::LeftDetermine()
         end
         ::nColPos--
         ::lRedrawFrame := .T.
         ::RefreshAll()
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD PanEnd() CLASS TBrowse

   ::Moved()

   if ::nColPos < ::nColumns
      if ::rightVisible <  ::nColumns
         ::rightVisible := ::nColumns
         ::leftVisible  := ::LeftDetermine()
         ::nColPos      := ::RightVisible
         ::lRedrawFrame := .T.
         ::RefreshAll()
      else
         ::nColPos      := ::RightVisible
         ::RefreshCurrent()
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD PanHome() CLASS TBrowse

   ::Moved()

   if ::nColPos > 1
      if ::leftVisible > ::nFrozenCols + 1
         ::leftVisible  := ::nFrozenCols + 1
         ::nColPos      := 1
         ::RefreshAll()
         ::lRedrawFrame := .T.
      else
         ::nColPos      := 1
         ::RefreshCurrent()
      endif
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD PanLeft() CLASS TBrowse

   local n := ::nColPos - ::leftVisible
   local leftVis := ::leftVisible

   ::Moved()

   if ::leftVisible > ::nFrozenCols + 1
      while leftVis == ::leftVisible
         ::rightVisible--
         ::leftVisible := ::LeftDetermine()
      end
      if ::nFrozenCols > 0 .and. ::nColPos <= ::nFrozenCols
      else
         ::nColPos    := Min( ::leftVisible + n, ::rightVisible )
      endif
      ::lRedrawFrame := .T.
      ::RefreshAll()
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD PanRight() CLASS TBrowse

   local n := ::nColPos - ::leftVisible

   ::Moved()

   if ::rightVisible < ::nColumns
      ::rightVisible++
      ::leftVisible  := ::LeftDetermine()
      if ::nFrozenCols > 0 .and. ::nColPos <= ::nFrozenCols
      else
         ::nColPos   := Min( ::leftVisible + n, ::rightVisible )
      endif
      ::lRedrawFrame := .T.
      ::RefreshAll()
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
   Local nWidth := ::nFrozenWidth
   Local nCol   := ::rightVisible

   while nWidth < ::nVisWidth .and. nCol > ::nFrozenCols .and. nCol <= ::nColumns .and. nCol > 0
      nWidth += ::aColsInfo[ nCol, o_Width ]

      if nWidth <= ::nVisWidth
         nCol--
         if nCol > 0 .and. nCol > ::nFrozenCols .and. ::aColsInfo[ nCol,o_Width ] > 0
            nWidth += ::aColsInfo[ nCol + 1,o_SepWidth ]
         endif
      else
         exit
      endif
   enddo

   Return Min( nCol + 1, ::nColumns )

//-------------------------------------------------------------------//
//
//  Calculate how many columns fit on the browse width including ColSeps
//
METHOD HowManyCol() CLASS TBrowse
   Local aColsInfo    := ::aColsInfo
   Local colPos       := ::ncolPos
   Local rightVisible := ::rightVisible
   Local leftVisible  := ::leftVisible
   Local nToAdd       := 0
   Local nColsVisible, nColsWidth, n, nColumns

   ::aColumns := ::aColsInfo
   nColumns   := ::nColumns

   // They were locals, so now I need to clear them (should fix this)
   //
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

   nColsVisible := ::leftVisible - 1

   while nColsVisible < ::nColumns
      nToAdd := ::aColsInfo[ nColsVisible + 1, o_Width ]

      if ( nColsVisible >= ::leftVisible .or. ::nFrozenCols > 0 ) .and.;
                                          ::aColsInfo[ nColsVisible,o_Width ] > 0
         nToAdd += ::aColsInfo[ nColsVisible + 1, o_SepWidth ]
      endif

      if nColsWidth + nToAdd > ::nVisWidth
         exit
      endif

      nColsWidth += nToAdd
      nColsVisible++
   enddo

   ::rightVisible := nColsVisible
   ::nColsVisible := nColsVisible
   ::nColsWidth   := nColsWidth
   ::cSpacePre    := space( INT( ( ::nVisWidth - ::nColsWidth ) / 2 ) )
   ::cSpaceLast   := space( ::nVisWidth - len( ::cSpacePre ) - ::nColsWidth )

   Return Self

//-------------------------------------------------------------------//
//
// Gets TBrowse width and width of displayed columns plus colsep
//
METHOD RedrawHeaders( nWidth ) CLASS TBrowse
   Local n, nTPos, nBPos
   Local cBlankBox := Space( 9 )
   Local nScreenRowT, nScreenRowB
   Local nLCS
   Local aCol, nCol
   Local nColFrom

   aCol     := ARRAY( ::rightVisible )
   nCol     := ::nwLeft + iif( ::nFrozenCols > 0, 0, INT( ( nWidth - ::nColsWidth ) / 2 ) )

   nColFrom := iif( ::nFrozenCols > 0, 1, ::leftVisible )

   ::aColumnsSep := {}

   for n := nColFrom to ::rightVisible
      aCol[ n ] := nCol
      ::aColsInfo[ n,o_Obj ]:ColPos := nCol

      nCol += ::aColsInfo[ n, o_Width ]

      if n < ::rightVisible
         if ::aColsInfo[ n,o_Width ] > 0
            aadd( ::aColumnsSep, nCol + int( ::aColsInfo[ n+1, o_SepWidth ] / 2 ) )
            nCol += ::aColsInfo[ n+1, o_SepWidth ]
         endif
      endif

      if ::nFrozenCols > 0 .and. n == ::nFrozenCols
         n    := ::leftVisible - 1
         nCol += INT( ( nWidth - ::nColsWidth ) / 2 )
      endif
   next

   if ::lHeaders          // Drawing headers
      // Clear area of screen occupied by headers
      //
      DispBox( ::nwTop, ::nwLeft, ::nwTop + ::nHeaderHeight - 1, ::nwRight, cBlankBox, ::cColorSpec )

      for n := nColFrom to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif
         DevPos( ::nwTop, aCol[ n ] )

         ::WriteMLineText( ::aColsInfo[ n, o_Heading ], ;
              ::aColsInfo[ n, o_Width ], .T., ;
                  hb_ColorIndex( ::cColorSpec, ColorToDisp( ::aColsInfo[ n,o_Obj ]:DefColor, TBC_CLR_HEADING ) - 1 ) )
      next
   endif

   if ::lHeadSep                      //Draw horizontal heading separator line
      DispOutAt( ( nScreenRowT := ::nRowData ), ::nwLeft,;
           Replicate( Right( ::HeadSep, 1 ), nWidth ), ::cColorSpec )

   endif

   if ::lFootSep                      //Draw horizontal footing separator line
      DispOutAt( ( nScreenRowB := ::nwBottom - iif( ::lFooters, ::nFooterHeight, 0 ) ), ::nwLeft,;
                                        Replicate( Right( ::FootSep, 1 ), nWidth ), ::cColorSpec )

   endif

   nTPos := nBPos := aCol[ iif( ::nFrozenCols > 0, 1, ::leftVisible ) ]

   // Draw headin/footing column separator
   for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
      if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
         n     := ::leftVisible
         nTPos += INT( ( nWidth - ::nColsWidth ) / 2 )
         nBPos += INT( ( nWidth - ::nColsWidth ) / 2 )
      endif

      if n < ::rightVisible
         if ::aColsInfo[ n,o_Width ] > 0
            nLCS := ::aColsInfo[ n + 1, o_SepWidth ]
         else
            nLCS := 0
         endif

         if ::lHeadSep
            // DispOutAT( nScreenRowT, ( nTPos += ::aColsInfo[ n, o_Width ] ), ::HeadSep, ::cColorSpec )
            DispOutAT( nScreenRowT, ( nTPos += ::aColsInfo[ n,o_Width ] ), ;
                            if( ::aColsInfo[ n,o_Obj ]:HeadSep == nil, ::HeadSep, ;
                                      ::aColsInfo[ n,o_Obj ]:HeadSep ), ::cColorSpec )
            nTPos += nLCS
         endif

         if ::lFootSep
            // DispOutAT( nScreenRowB, ( nBPos += ::aColsInfo[ n, o_Width ] ), ::FootSep, ::cColorSpec )
            DispOutAT( nScreenRowB, ( nBPos += ::aColsInfo[ n, o_Width ] ), ;
                          if(::aColsInfo[ n,o_Obj ]:FootSep = nil, ::FootSep, ;
                                      ::aColsInfo[ n,o_Obj ]:FootSep ), ::cColorSpec )
            nBPos += nLCS
         endif
      endif
   next

   if ::lFooters                // Drawing footers
      // Clear area of screen occupied by footers
      //
      DispBox( ::nwBottom - ::nFooterHeight + 1, ::nwLeft, ::nwBottom, ::nwRight, cBlankBox, ::cColorSpec )

      for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif
         DevPos( ::nwBottom, aCol[ n ] )

         ::WriteMLineText( ::aColsInfo[ n, o_Footing ], ;
              ::aColsInfo[ n, o_Width ], .F., ;
                  hb_ColorIndex( ::cColorSpec, ColorToDisp( ::aColsInfo[ n, o_Obj ]:DefColor, TBC_CLR_FOOTING ) - 1 ) )
      next
   endif

   Return Self

//---------------------------------------------------------------------//

METHOD ColorRect( aRect, aRectColor ) CLASS TBrowse
   Local nTop, nBottom, redraw

   if Len( aRect ) > 0
      ::aRect       := Array( 4 )
      ::aRectColor  := aRectColor

      ::aRect[ 1 ] := nTop    := Max( aRect[ 1 ], 1 )
      ::aRect[ 3 ] := nBottom := Min( aRect[ 3 ], ::rowCount )
      ::aRect[ 2 ] :=            Max( aRect[ 2 ], 1 )
      ::aRect[ 4 ] :=            Min( aRect[ 4 ], ::colCount )
      ::lRect      := .t.

   else
      ::aRect      := {}
      ::aRectColor := {}
      ::lRect      := .f.
   endif

   ::refreshAll()
   ::ForceStable()

   Return Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Stabilization
//
//-------------------------------------------------------------------//

METHOD ForceStable() CLASS TBrowse
   Local nAvail           // How many records are avaialble?
   Local lReset := .F.    // Reposition to row 1 required?

   // This is a hack to force TBrowse honors initial rowpos
   // This may be a very dirty approach

   local i, nInitRow

   If !::lInitRow
      nInitRow  := ::RowPos
      If nInitRow != 1
         for i := 1 to nInitRow - 1
            ::Down()
            ::ForceStabilize()
         next
      else
         ::ForceStabilize()
      Endif
   Endif

   ::lInitRow := .T.

   // If ForceStable() is called after movement of data source
   // (in simple words, the record pointer is moved) where the
   // movement was not effected by TBrowse (e.g user set scope)
   // then TBrowse has to evaluate if painting should start
   // from relative to current cursor row.  If not it should know
   // how many rows of data is available for painting prior
   // to paiting relative to current cursor row.  If sufficient data is not
   // available then reset position to row 1.

   If ::nRecsToSkip == 0  // Ensure that you do not reset the painting
                          // of a movement caused by TBrowse methods

      If ::RowPos # 1     // No repositioning is required if current
                          // cursor row is one

         nAvail := Eval( ::SkipBlock, 0 - ::RowPos - 1 )

         // You should reposition only if there are too few records
         // available or there are more than sufficient records
         // available.  If there are exact number of records leave it.
         //
         lReset := Abs( nAvail ) + 1 # ::RowPos

         // Place back the data source pointer to where it was
         //
         Eval( ::SkipBlock, 0 - nAvail )

      EndIf

   EndIf

   If lReset   // So repositioning was required !

      // nNewRowPos and nLastRetrieved have to be updated
      // as we will entering phase 2 of stabilization
      //
      ::RowPos := ::nNewRowPos := ::nLastRetrieved := ;
                  If( Abs( nAvail ) + 1 > ::RowPos, ::RowPos, Abs( nAvail ) + 1 )
      ::Moved()

      // To ensure phase 1 is skipped
      ::nRecsToSkip := 0

   EndIf

   //  Because forceStable is always needs whole tBrowse be redrawn
   //  no incremental stabilization is needed, so I am of the view that
   //  instead of Stabilize() method be called incrementaly, an identical
   //  method will be more appropriate but with single call.
   //
   ::ForceStabilize()

   Return self

//-------------------------------------------------------------------//

METHOD Stabilize() CLASS TBrowse
   Local nRecsSkipped                  // How many records do I really skipped?
   Local nFirstRow                     // Where is on screen first row of TBrowse?
   Local nOldCursor                    // Current shape of cursor (which I remove before stabilization)
   Local colorSpec
   Local nRowToDraw

   if ::nColumns == 0
      // Return TRUE to avoid infinite loop ( do while !stabilize;end )
      return .t.
   endif

   // Configure the browse if not configured . Pritpal Bedi
   //
   if !::lConfigured .or. ::lNeverDisplayed
      if ::lNeverDisplayed
         ::configure( 3 )
      endif
      ::configure( 2 )
   endif

   // I need to set columns width If TBrowse was never displayed before
   //
   if ::lNeverDisplayed
      //AEVal(::aColumns, {|oCol| ::SetColumnWidth(oCol)} )

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

   // From this point there is stabilization of rows which is made up of three phases
   // 1st repositioning of data source
   // 2nd redrawing of rows, after each row we exit stabilization loop with .F.
   // 3rd if all rows have been redrawn we set ::stable state to .T.
   //
   if !::stable
      // NOTE: I can enter here because of a movement key or a ::RefreshAll():ForceStable() call

      if ::CheckRowsToBeRedrawn()
         // Exit first stage of stabilization
         //
         SetCursor( nOldCursor )
         return .F.

      endif

      //  Draw browse row-by-row
      //
      if ( nRowToDraw := ascan( ::aRedraw, .t. ) ) <> 0
         ::DrawARow( @nRowToDraw )

         SetCursor( nOldCursor )
         return .F.
      endif

      // If I reach this point I've repainted all rows so I can set ::stable state
      //
      // If I have fewer records than available TBrowse rows, cursor cannot be lower than
      // last record (note ::lHitBottom is set only during a movement)
      //
      if ::nLastRetrieved < ::nNewRowPos
         ::nNewRowPos := ::nLastRetrieved
      endif

      // If I'm not already under cursor I have to set data source to cursor position
      //
      if ::nLastRetrieved <> ::nNewRowPos
         EvalSkipBlock( ::SkipBlock, ::nNewRowPos - ::nLastRetrieved )
         ::nLastRetrieved := ::nNewRowPos
      endif

      // new cursor position
      //
      ::RowPos    := ::nNewRowPos

      ::HitTop    := ::lHitTop
      ::HitBottom := ::lHitBottom

      if ::AutoLite
         ::Hilite()
      else
         ::PosCursor()
      endif
      SetCursor( nOldCursor )

      ::stable := .T.

      return .T.

   else
      // NOTE: DBU relies upon current cell being reHilited() even if already stable
      //
      if ::AutoLite
         ::Hilite()
      else
         ::PosCursor()
      endif
      SetCursor( nOldCursor )

      return .T.

   endif

   Return .F.

//-------------------------------------------------------------------//

METHOD ForceStabilize() CLASS TBrowse
   Local nRecsSkipped                  // How many records do I really skipped?
   Local nFirstRow                     // Where is on screen first row of TBrowse?
   Local nOldCursor                    // Current shape of cursor (which I remove before stabilization)

   if ::nColumns == 0
      // Return TRUE to avoid infinite loop ( do while !stabilize;end )
      return .t.
   endif

   // Configure the browse if not configured . Pritpal Bedi
   //
   if !::lConfigured .or. ::lNeverDisplayed
      if ::lNeverDisplayed
         ::configure( 3 )
      endif
      ::configure( 2 )
   endif

   // I need to set columns width If TBrowse was never displayed before
   if ::lNeverDisplayed
      //AEVal(::aColumns, {|oCol| ::SetColumnWidth(oCol)} )

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
   endif

   nOldCursor := SetCursor( SC_NONE )

   if ::lRedrawFrame
      // Draw border
      //
      if Len( ::cBorder ) == 8
         @::nTop,::nLeft,::nBottom,::nRight BOX ::cBorder COLOR ::aColorSpec[ 1 ]
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

   // From this point there is stabilization of rows which is made up of three phases
   // 1st repositioning of data source
   // 2nd redrawing of rows, after each row we exit stabilization loop with .F.
   // 3rd if all rows have been redrawn we set ::stable state to .T.
   //
   if !( ::stable )
      // NOTE: I can enter here because of a movement key or a ::RefreshAll():ForceStable() call

      ::CheckRowsToBeRedrawn()

      //  Redraw Rows required to be redrawn . 14 Mar 2004 . Pritpal Bedi
      //
      ::DrawRows()

      // If I have fewer records than available TBrowse rows, cursor cannot be lower than
      // last record (note ::lHitBottom is set only during a movement)
      //
      if ::nLastRetrieved < ::nNewRowPos
         ::nNewRowPos := ::nLastRetrieved
      endif

      // If I'm not already under cursor I have to set data source to cursor position
      //
      if ::nLastRetrieved <> ::nNewRowPos
         EvalSkipBlock( ::SkipBlock, ::nNewRowPos - ::nLastRetrieved )
         ::nLastRetrieved := ::nNewRowPos
      endif

      // new cursor position
      //
      ::RowPos    := ::nNewRowPos
      ::HitTop    := ::lHitTop
      ::HitBottom := ::lHitBottom

      if ::AutoLite
         ::Hilite()
      else
         ::PosCursor()
      endif
      SetCursor( nOldCursor )

      ::stable := .T.
   endif

   Return .t.

//-------------------------------------------------------------------//

METHOD CheckRowsToBeRedrawn() CLASS TBrowse
   Local nRecsSkipped
   Local nFirstRow

   // If I have a requested movement still to handle
   //
   if ::nRecsToSkip <> 0
      // If I'm not under cursor
      // maybe I've interrupted an ongoing stabilization
      // I have to set data source to cursor position
      //
      if ::nLastRetrieved <> ::nNewRowPos
         EvalSkipBlock( ::SkipBlock, ::nNewRowPos - ::nLastRetrieved )
         ::nLastRetrieved := ::nNewRowPos
      endif

      nRecsSkipped := EvalSkipBlock( ::SkipBlock, ::nRecsToSkip )

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
            ::nNewRowPos     += nRecsSkipped
            ::nLastRetrieved := ::nNewRowPos

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
               if nRecsSkipped > 0
                  ::nLastRetrieved := ::RowCount

               else // K_PGUP
                  ::nLastRetrieved := 1

               endif
               ::RefreshAll()

            else
               // K_DN or K_UP
               // Where does really start first TBrowse row?
               //
               nFirstRow := ::nRowData + 1

               // I'm at top or bottom of TBrowse so I can scroll
               //
               if ::nNewRowPos == ::RowCount
                  ScrollFixed( nFirstRow + nRecsSkipped - 1, ::nwLeft, nFirstRow + ::RowCount - 1, ::nwRight, nRecsSkipped )
                  ::nLastRetrieved := ::RowCount

               else
                  ScrollFixed( nFirstRow, ::nwLeft, nFirstRow + ::RowCount + nRecsSkipped, ::nwRight, nRecsSkipped )
                  ::nLastRetrieved := 1

               endif

               // I've scrolled on screen rows, now I need to scroll ::aRedraw array as well!
               //
               if nRecsSkipped > 0
                  ADel( ::aRedraw, 1 )
                  ::aRedraw[ -1 ] := .F.

               else
                  ADel( ::aRedraw, ::RowCount )
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

         ::nLastRetrieved := ::nNewRowPos

      endif

      // Data source moved, so next time I won't enter this stage of stabilization
      //
      ::nRecsToSkip := 0
      Return .t.
   endif

   Return .f.

//-------------------------------------------------------------------//
//
//           Pritpal Bedi <pritpal@vouchcac.com> 14Mar2004
//
METHOD DrawRows() CLASS TBrowse
   Local colorSpec
   Local aColsInfo
   Local lColorRect
   Local nColFrom
   Local cSpacePre
   Local lRedraw
   Local lDisplay
   Local n
   Local nRow
   Local lInRect
   Local cSpaceLast
   Local nRowsToSkip

   colorSpec  := ::aColorSpec[ 1 ]
   aColsInfo  := ::aColsInfo
   nColFrom   := iif( ::nFrozenCols > 0, 1, ::leftVisible )
   cSpacePre  := ::cSpacePre
   cSpaceLast := space( ::nVisWidth - len( ::cSpacePre ) - ::nColsWidth )

   DispBegin()

   // Data source is alredy at correct record number, now we need
   // to repaint browser accordingly.
   //
   for each lRedraw in ::aRedraw
      // if there is a row to repaint
      //
      if lRedraw
         nRow := HB_EnumIndex()

         // NOTE: If my TBrowse has 20 rows but I have only 3 recs, clipper clears
         //       remaining 17 rows in a single operation, I will, instead, try to skip
         //       17 times. Should be made more clever.
         //
         if nRow <> ::nLastRetrieved
            nRowsToSkip := nRow - ::nLastRetrieved
            if lDisplay := ( EvalSkipBlock( ::SkipBlock, nRowsToSkip ) == nRowsToSkip )
               ::nLastRetrieved := nRow
            endif
         else
            lDisplay := .T.
         endif

         if !( lDisplay )
            if ::nFrozenCols == 0
               DispOutAt( nRow + ::nRowData, ::nwLeft, @cSpacePre, ColorSpec )

               for n := nColFrom to ::rightVisible - 1
                  DispOut( aColsInfo[ n,o_Blank ], ColorSpec )

                  if aColsInfo[ n,o_lColSep ]
                     DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
                  endif
               next
               DispOut( aColsInfo[ n,o_Blank ], ColorSpec )

            else
               DevPos( nRow + ::nRowData, ::nwLeft )

               for n := nColFrom to ::rightVisible
                  if n == ::nFrozenCols + 1
                     n := ::leftVisible
                     DispOut( @cSpacePre, ColorSpec )
                  endif

                  DispOut( aColsInfo[ n,o_Blank ], ColorSpec )

                  if n < ::rightVisible .and. aColsInfo[ n,o_lColSep ]
                     DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
                  endif
               next
            endif
         else      //  lDisplay
            if ::nFrozenCols == 0
               DispOutAt( nRow + ::nRowData, ::nwLeft, @cSpacePre, ColorSpec )

               if !( ::lRect )

                  for n := nColFrom to ::rightVisible - 1
                     ::DispCellNormal( @n )
                     if aColsInfo[ n,o_lColSep ]
                        DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
                     endif
                  next
                  ::DispCellNormal( @n )

               else
                  lInRect := ( ::aRect[ 1 ] <= nRow .and. ::aRect[ 3 ] >= nRow )

                  for n := nColFrom to ::rightVisible
                     if lInRect .and. ::aRect[ 2 ] <= n .and. ::aRect[ 4 ] >= n
                        ::DispCell( @n, TBC_CLR_STANDARD, ::aRectColor )
                     else
                        ::DispCellNormal( @n )
                     endif

                     if n < ::rightVisible .and. aColsInfo[ n,o_lColSep ]
                        DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
                     endif
                  next
               endif

            else      //  ::nFrozenCols > 0
               if ::lRect
                  lInRect := ( ::aRect[ 1 ] <= nRow .and. ::aRect[ 3 ] >= nRow )
               else
                  lInRect := .f.
               endif
               DevPos( nRow + ::nRowData, ::nwLeft )

               for n := nColFrom to ::rightVisible
                  if n == ::nFrozenCols + 1
                     n := ::leftVisible
                     DispOut( @cSpacePre, @ColorSpec )
                  endif

                  if ::lRect .and. lInRect .and. ::aRect[ 2 ] <= n  .and. ::aRect[ 4 ] >= n
                     ::DispCell( @n, TBC_CLR_STANDARD, ::aRectColor )
                  else
                     ::DispCellNormal( @n )
                  endif

                  if n < ::rightVisible .and. aColsInfo[ n,o_lColSep ]
                     DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
                  endif
               next
            endif
         endif  // !( lDisplay )

         DispOut( @cSpaceLast, ColorSpec )

         // doesn't need to be redrawn
         //
         lRedraw := .F.
      endif
   next

   DispEnd()

   Return .t.

//-------------------------------------------------------------------//

METHOD DrawARow( nRow ) CLASS TBrowse
   Local colorSpec
   Local aColsInfo
   Local lColorRect
   Local nColFrom
   Local cSpacePre
   Local lRedraw
   Local lDisplay
   Local n
   Local lInRect
   Local cSpaceLast
   Local nRowsToSkip

   colorSpec  := ::aColorSpec[ 1 ]
   aColsInfo  := ::aColsInfo
   nColFrom   := iif( ::nFrozenCols > 0, 1, ::leftVisible )
   cSpacePre  := ::cSpacePre
   cSpaceLast := space( ::nVisWidth - len( ::cSpacePre ) - ::nColsWidth )
   lInRect    := .f.

   // Data source is alredy at correct record number, now we need
   // to repaint browser accordingly.
   //
   DispBegin()

   if nRow <> ::nLastRetrieved
      nRowsToSkip := nRow - ::nLastRetrieved
      if lDisplay := ( EvalSkipBlock( ::SkipBlock, nRowsToSkip ) == nRowsToSkip )
         ::nLastRetrieved := nRow
      endif
   else
      lDisplay := .T.
   endif

   if !( lDisplay )
      if ::nFrozenCols == 0
         DispOutAt( nRow + ::nRowData, ::nwLeft, @cSpacePre, ColorSpec )

         for n := nColFrom to ::rightVisible
            DispOut( aColsInfo[ n,o_Blank ], ColorSpec )

            if n < ::rightVisible .and. aColsInfo[ n,o_lColSep ]
               DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
            endif
         next

      else
         DevPos( nRow + ::nRowData, ::nwLeft )

         for n := nColFrom to ::rightVisible
            if n == ::nFrozenCols + 1
               n := ::leftVisible
               DispOut( @cSpacePre, ColorSpec )
            endif

            DispOut( aColsInfo[ n,o_Blank ], ColorSpec )

            if n < ::rightVisible .and. aColsInfo[ n,o_lColSep ]
               DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
            endif
         next
      endif

   else
      //  This if statement is just to speed up the refresh process
      //  It is almost identical, but please do not try to merge it in one.
      //
      if ::nFrozenCols == 0
         DispOutAt( nRow + ::nRowData, ::nwLeft, @cSpacePre, ColorSpec )

         if !( ::lRect )
            for n := nColFrom to ::rightVisible
               ::DispCellNormal( @n )

               if n < ::rightVisible .and. aColsInfo[ n,o_lColSep ]
                  DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
               endif
            next
         else
            lInRect := ( ::aRect[ 1 ] <= nRow .and. ::aRect[ 3 ] >= nRow )

            for n := nColFrom to ::rightVisible
               if lInRect .and. ::aRect[ 2 ] <= n .and. ::aRect[ 4 ] >= n
                  ::DispCell( @n, TBC_CLR_STANDARD, ::aRectColor )
               else
                  ::DispCellNormal( @n )
               endif

               if n < ::rightVisible .and. aColsInfo[ n,o_lColSep ]
                  DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
               endif
            next
         endif

      else      //  ::nFrozenCols > 0
         DevPos( nRow + ::nRowData, ::nwLeft )
         if ::lRect
            lInRect := ( ::aRect[ 1 ] <= nRow .and. ::aRect[ 3 ] >= nRow )
         endif

         for n := nColFrom to ::rightVisible
            if n == ::nFrozenCols + 1
               n := ::leftVisible
               DispOut( @cSpacePre, ColorSpec )
            endif
            if ::lRect .and. lInRect .and. ::aRect[ 2 ] <= n  .and. ::aRect[ 4 ] >= n
               ::DispCell( @n, TBC_CLR_STANDARD, ::aRectColor )
            else
               ::DispCell( @n, TBC_CLR_STANDARD )
            endif

            if n < ::rightVisible .and. aColsInfo[ n,o_lColSep ]
               DispOut( aColsInfo[ n + 1, o_ColSep ], ColorSpec )
            endif
         next

      endif
   endif  // !( lDisplay )

   DispOut( @cSpaceLast, ColorSpec )

   // doesn't need to be redrawn
   //
   ::aRedraw[ nRow ] := .f.
   DispEnd()

   Return .t.

//-------------------------------------------------------------------//
//
//                              Display
//
//-------------------------------------------------------------------//

METHOD PosCursor() CLASS TBrowse
   local nRow := ::RowPos + ::nRowData
   local nCol := ::aColsInfo[ ::nColPos, o_Obj ]:ColPos

   SetPos( nRow, nCol )

 #ifdef HB_COMPAT_C53
   ::nRow := nRow
   ::nCol := nCol
 #endif

   Return Self

//-------------------------------------------------------------------//

METHOD DeHilite() CLASS TBrowse
   Local nRow := ::RowPos + ::nRowData
   Local nCol := ::aColsInfo[ ::nColPos, o_Obj ]:colPos

   SetPos( nRow, nCol )

   if ::lRect .and.;
              ::aRect[ 1 ] <= nRow .and. ::aRect[ 3 ] >= nRow .and.;
              ::aRect[ 2 ] <= nCol .and. ::aRect[ 4 ] >= nCol
      ::DispCell( ::nColPos, TBC_CLR_STANDARD, ::aRectColor )
   else
      ::DispCell( ::nColPos, TBC_CLR_STANDARD )
   endif

   SetPos( nRow, nCol )

   Return Self

//-------------------------------------------------------------------//

METHOD Hilite() CLASS TBrowse
   Local nRow := ::RowPos + ::nRowData
   Local nCol := ::aColsInfo[ ::nColPos, o_Obj ]:ColPos

   SetPos( nRow, nCol )

   if ::lRect .and.;
              ::aRect[ 1 ] <= nRow .and. ::aRect[ 3 ] >= nRow .and.;
              ::aRect[ 2 ] <= nCol .and. ::aRect[ 4 ] >= nCol
      ::DispCell( ::nColPos, TBC_CLR_ENHANCED, ::aRectColor )
   else
      ::DispCell( ::nColPos, TBC_CLR_ENHANCED )
   endif

   SetPos( nRow, nCol )

#ifdef HB_COMPAT_C53
   ::nRow := nRow
   ::nCol := nCol
#endif

   Return Self

//-------------------------------------------------------------------//

METHOD DispCell( nColumn, nColor, aColors ) CLASS TBrowse

   LOCAL aColsInfo := ::aColsInfo[ nColumn ]
   LOCAL oCol      := aColsInfo[ o_Obj       ]
   LOCAL nWidth    := aColsInfo[ o_Width     ]
   LOCAL nLen      := aColsInfo[ o_WidthCell ]
   LOCAL ftmp      := Eval( oCol:block )

   // NOTE: When nColor is used as an array index we need to increment
   // it by one since CLR_STANDARD is 0
   //
   LOCAL cColor

   // if called when the column type is not defined, then do nothing
   if empty( aColsInfo[ o_Type ] )
      Return nil // nCol
   endif

   if aColors == NIL
      if oCol:ColorBlock == NIL
         cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( oCol:DefColor, nColor ) - 1 )
//         cColor := ::aColorSpec[ ColorToDisp( oCol:DefColor, nColor ) ]
      else
         cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( Eval( oCol:ColorBlock, ftmp ), nColor ) - 1 )
//         cColor := ::aColorSpec[ ColorToDisp( Eval( oCol:ColorBlock, ftmp ), nColor ) ]
      endif
   else
      cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( aColors, nColor ) - 1 )
//       cColor := ::aColorSpec[ ColorToDisp( aColors, nColor ) ]
   endif

   Switch aColsInfo[ o_Type ]
   case "C"
   case "M"
      DispOut( PadR( Transform( ftmp, aColsInfo[ o_Pict ] ), nWidth ), cColor )
      exit

   case "N"
      DispOut( PadL( Transform( ftmp, aColsInfo[ o_Pict ] ), nWidth ), cColor )
      exit

   case "D"
      DispOut( PadR( Transform( ftmp, aColsInfo[ o_Pict ] ), nWidth ), cColor )
      exit

   case "L"
      DispOut( padc( iif( ftmp, "T", "F" ),nWidth ), cColor )
      exit

   default
      DispOut( Space( nWidth ), cColor )

   end

   Return nil

//-------------------------------------------------------------------//

METHOD DispCellNormal( nColumn ) CLASS TBrowse
   Local ftmp := Eval( ::aColsInfo[ nColumn, o_Obj ]:block )
   Local cColor

   if ::aColsInfo[ nColumn, o_Obj ]:ColorBlock == NIL
      cColor := ::aColorSpec[ ::aColsInfo[ nColumn, o_DefColor, TBC_CLR_STANDARD ] ]
   else
      cColor := ::aColorSpec[ ColorToDisp( Eval( ::aColsInfo[ nColumn, o_Obj ]:ColorBlock, ftmp ), TBC_CLR_STANDARD ) ]
   endif

   Switch ::aColsInfo[ nColumn, o_Type  ]

   case "C"
   case "M"
      DispOut( PadR( Transform( ftmp, ::aColsInfo[ nColumn,o_Pict ] ), ::aColsInfo[ nColumn,o_Width ] ), @cColor )
      exit

   case "N"
      DispOut( PadL( Transform( ftmp, ::aColsInfo[ nColumn,o_Pict ] ), ::aColsInfo[ nColumn,o_Width ] ), @cColor )
      exit

   case "D"
      DispOut( PadR( Transform( ftmp, ::aColsInfo[ nColumn,o_Pict ] ), ::aColsInfo[ nColumn,o_Width ] ), @cColor )
      exit

   case "L"
      DispOut( padc( iif( ftmp, "T", "F" ), ::aColsInfo[ nColumn,o_Width ] ), @cColor )
      exit

   default
      DispOut( Space( ::aColsInfo[ nColumn,o_Width ] ), @cColor )

   end

   Return nil

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//   NOTE: Not tested, could be broken
//
METHOD MGotoYX( nRow, nCol ) CLASS TBrowse

   local nColsLen, nI, nNewRow

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
//      nNewRow := nRow - ::nwTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ), 0, 1 ) - 1
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
   Local n, cS
   Local nCol := Col()
   Local nRow := Row()
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
            DevPos( nRow + n - 1, nCol )
            DispOut( PadR( __StrToken( @cS, n, ";" ), nPadLen ), cColor )
         next

         DevPos( nRow, nCol + nPadLen )

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
            DevPos( nRow - n, nCol )
            DispOut( PadR( __StrToken( @cS, ::nFooterHeight - n, ";" ), nPadLen ), cColor )
         next

         DevPos( nRow, nCol + nPadLen )

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
//
//                      Clipper 5.3b Compatibility
//
//---------------------------------------------------------------------//
#ifdef HB_COMPAT_C53
//---------------------------------------------------------------------//

METHOD ApplyKey( nKey )  CLASS TBrowse

   Return ::TApplyKey( nKey, self )

//-------------------------------------------------------------------//

METHOD InitKeys( o ) CLASS TBROWSE

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
              { K_LBUTTONDOWN, {| oB, nKey | tbmouse( ob, mrow(), mcol() ) } } }
   Return o

//-------------------------------------------------------------------//

METHOD SetKey( nKey,bBlock ) CLASS TBrowse
   Local bReturn,nPos

   ::InitKeys( self )

   if ( nPos := ascan( ::aKeys,{| x | x[ 1 ] == nkey } ) ) == 0
      if ISBLOCK( bBlock )
//         bReturn := bBlock
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
      adel( ::aKeys, nPos, .T. )

   endif

   Return bReturn

//-------------------------------------------------------------------//

METHOD TApplyKey( nKey, oBrowse ) CLASS tBrowse
   Local bBlock := oBrowse:setkey( nKey ), nReturn := TBR_CONTINUE  // 0

   DEFAULT bBlock TO oBrowse:setkey( 0 )

   if ISNIL( bBlock )
      nReturn := TBR_EXCEPTION  // 1
   else
      nReturn := eval( bBlock, oBrowse, nKey )
   endif

   Return nReturn

//-------------------------------------------------------------------//

Method hitTest( mrow,mcol ) CLASS TBROWSE
   Local i, nVisCol, lHitHeader := .f.

   ::mRowPos := ::rowPos
   ::mColPos := ::colPos

   if mRow < ::nTop .or. mRow > ::rect[ 3 ]
      return HTNOWHERE
   elseif mRow = ::nTop
      lHitHeader := .t.
   endif

   if mCol < ::rect[ 2 ] .or. mCol > ::rect[ 4 ]
      return HTNOWHERE
   endif

   ::mRowPos := mRow - ::rect[ 1 ] + 1

   nVisCol := len( ::aColumnsSep )

   if nVisCol == 0
      ::mColPos := 1

   elseif mcol >= ::aColumnsSep[ nVisCol ]
      ::mColPos := nVisCol + 1

   else
      for i := 1 to nVisCol
         if mcol < ::aColumnsSep[ i ]
            ::mColPos := i
            exit
         endif
      next
   endif

   if lHitHeader
      return -5122
   endif

   Return HTCELL

//-------------------------------------------------------------------//

METHOD SetStyle( nMode, lSetting ) CLASS TBROWSE
  LOCAL lRet := .F.

  IF nMode > LEN( ::aSetStyle )
     RETURN .F.
  ENDIF

  lRet := ::aSetStyle[ nMode ]

  IF ISLOGICAL( lSetting )
     ::aSetStyle[ nMode ] := lSetting
  ENDIF

   Return lRet

//-------------------------------------------------------------------//

static Function EvalSkipBlock( bBlock, nSkip )
   local lSign   := nSkip >= 0
   local nSkiped := Eval( bBlock, nSkip )

   if ( lSign .and. nSkiped < 0 ) .or. ( !lSign .and. nSkiped > 0 )
      nSkiped := 0
   endif

   Return nSkiped

//-------------------------------------------------------------------//

function TBMOUSE( oBrowse, nMouseRow, nMouseCol )
   Local n

   if oBrowse:hittest( nMouseRow, nMouseCol ) == -5121

      n := oBrowse:mrowpos - oBrowse:rowpos

      do while ( n < 0 )
         n++
         oBrowse:up()
      enddo

      do while ( n > 0 )
         n--
         oBrowse:down()
      enddo

      n := oBrowse:mcolpos - oBrowse:colpos

      do while ( n < 0 )
         n++
         oBrowse:left()
      enddo

      do while ( n > 0 )
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

static function ColorToDisp( aColor, nColor )

   if Len( aColor ) >= nColor
      return aColor[ nColor ]
   endif

   Return { 1, 2, 1, 1 }[ nColor ]

//-------------------------------------------------------------------//

static Function ArrayToList( aArray )
   Local cList := "", a

   for each a in aArray
      cList += alltrim( str( a ) ) + ", "
   next

   cList := substr( cList,1,len( cList )-2 )

   Return cList

//-------------------------------------------------------------------//

static function Color2Array( cColorSpec )
   Local n
   Local a_:= {}
   Local cToken := ','

   DEFAULT cColorSpec TO SetColor()

   if empty( cColorSpec )
      cColorSpec := SetColor()
   endif

   n := 1
   do while n > 0
      if ( n := at( cToken, cColorSpec ) ) > 0
         aadd( a_, substr( cColorSpec,1,n-1 ) )
         cColorSpec := substr( cColorSpec,n+1 )
      endif
   enddo
   aadd( a_, trim( cColorSpec ) )

   Return a_

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


