/*
 * $Id: tbrowse.prg,v 1.115 0000/00/00 00:00:00 modalsist Exp $
 */

/*
 * Harbour Project source code:
 * TBrowse Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
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
 * Copyright 2001 Manu Exposito <maex14@dipusevilla.es>
 * Activate data PICTURE DispCell(nRow, nCol, nColor)
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

/* 2005/Sep/03 - changed by Eduardo Fernandes */

#define COLINFO_OBJ             1   // Object Column
#define COLINFO_TYPE            2   // Type of Data in Column
#define COLINFO_WIDTH           3   // Column Width
#define COLINFO_HEADING         4   // Column Headings
#define COLINFO_FOOTING         5   // Column Footings
//#define COLINFO_PICTURE         6   // Column Picture
#define COLINFO_CELLWIDTH       7   // Width of the Cell
#define COLINFO_COLSEP          8   // Column Seperator
#define COLINFO_SEPWIDTH        9   // Width of the Separator
//#define COLINFO_DEFCOLOR       10   // Array with index of default color
#define COLINFO_ISSETWIDTH     11   // If True, only SetFrozen can change COLINFO_WIDTH

/* 02/july/2005 - <maurilio.longo@libero.it>
                  removed, not really needed, but still present inside ::aColsInfo,
                  as a NIL value
#define COLINFO_BLANK          12   // Spaces corresponding to COLINFO_WIDTH
*/
#define COLINFO_DRAW_COLSEP    13   // Should column separator be drawn
#define COLINFO_COLPOS         14   // Temporary column position on screen


/* 25/11/2004 - <maurilio.longo@libero.it>
   TBC_ are CLR_ constants increased by one to be used as indexes inside columns color arrays
   HEADING and FOOTING have to be colored like STANDARD colors, clipper does it, and could simply
   be removed
*/
#define TBC_CLR_STANDARD  ( CLR_STANDARD + 1 )  // = 1
#define TBC_CLR_ENHANCED  ( CLR_ENHANCED + 1 )  // = 2
#define TBC_CLR_HEADING   TBC_CLR_STANDARD
#define TBC_CLR_FOOTING   TBC_CLR_STANDARD


/* 23/11/2004 - <maurilio.longo@libero.it> Inlined to be somewhat faster
   25/11/2004 - <maurilio.longo@libero.it> Since there are only two colors, this makes every day less sense to me...
*/
#define  ColorToDisp( aColor, nColor )    iif( Len( aColor ) >= nColor, aColor[ nColor ], { 1, 2 }[ nColor ] )

/* 2005/Aug/30 - Eduardo Fernandes <modalsist>
Rectangle area for ColorRect control
*/
#define COLORRECT_TOP     1    
#define COLORRECT_LEFT    2   
#define COLORRECT_BOTTOM  3   
#define COLORRECT_RIGHT   4   

/* 2005/Sep/15 - Eduardo Fernandes <modalsist>
TBrowse up-down movements for ColorRect control.
*/

#define TBR_DOWN_MOVED      1
#define TBR_UP_MOVED        2
#define TBR_PGDOWN_MOVED    3
#define TBR_PGUP_MOVED      4
#define TBR_BOTTOM_MOVED    5
#define TBR_TOP_MOVED       6


//-------------------------------------------------------------------//

CLASS TBrowse


#ifdef HB_COMPAT_C53
   DATA nRow                  // Row number for the actual cell
   DATA nCol                  // Col number for the actual cell
#endif

   ACCESS Autolite            INLINE ::lAutolite
   ASSIGN Autolite(lAuto)     INLINE ::lAutolite := iif(valtype(lAuto)="L",lAuto,::lAutolite)

   ACCESS Border              INLINE ::cBorder
   ASSIGN Border(cNewBorder)  INLINE ::SetBorder( cNewBorder )

   ACCESS Cargo               INLINE ::uCargo
   ASSIGN Cargo(uAnyValue)    INLINE ::uCargo := uAnyValue

   ACCESS ColCount            INLINE ::nColCount    
   ACCESS LeftVisible         INLINE ::nLeftVisible
   ACCESS RightVisible        INLINE ::nRightVisible
   ACCESS RowCount            INLINE ::nRowCount

   ACCESS ColorSpec           INLINE ::cColorSpec   // Color table for the TBrowse display
   ASSIGN ColorSpec(cColor)   INLINE if( empty( cColor ), ::cColorSpec, ( ::lConfigured := .f., ;
                                     ::aColorSpec := Color2Array( cColor ), ::cColorSpec := cColor ) )

   ACCESS ColPos              INLINE ::nColPos
   ASSIGN ColPos(nNewColPos)  INLINE ::nColPos := iif( valtype(nNewColPos) != "N", ::nColPos, nNewColPos ),;
                                     iif(  (::nColPos < ::nLeftVisible .and. ::nFrozenCols+1<::nLeftVisible)  .OR. ::nColPos > ::nRightVisible, ::lConfigured := .F., NIL),::nColPos

   ACCESS RowPos              INLINE ::nRowPos
   ASSIGN RowPos(nNewRow)     INLINE ::SetRowPos( nNewRow )

   ACCESS nTop                INLINE ::nwTop    -  iif( ::cBorder == "", 0, 1 )
   ASSIGN nTop( nTop )        INLINE ::PreConfigVertical(   ::nwTop    := nTop    + iif( ::cBorder == "", 0, 1 ) )

   ACCESS nLeft               INLINE ::nwLeft   -  iif( ::cBorder == "", 0, 1 )
   ASSIGN nLeft( nLeft )      INLINE ::PreConfigHorizontal( ::nwLeft   := nLeft   + iif( ::cBorder == "", 0, 1 ) )

   ACCESS nBottom             INLINE ::nwBottom +  iif( ::cBorder == "", 0, 1 )
   ASSIGN nBottom( nBottom )  INLINE ::PreConfigVertical(   ::nwBottom := nBottom - iif( ::cBorder == "", 0, 1 ) )

   ACCESS nRight              INLINE ::nwRight  +  iif( ::cBorder == "", 0, 1 )
   ASSIGN nRight( nRight )    INLINE ::PreConfigHorizontal( ::nwRight  := nRight  - iif( ::cBorder == "", 0, 1 ) )


   ACCESS ColSep              INLINE ::cColSep        // Column separator character
   ASSIGN ColSep( cColSep )   INLINE ::lConfigured := .f., ::cColSep  := cColSep

   ACCESS FootSep             INLINE ::cFootSep       // Footing separator character
   ASSIGN FootSep( cFootSep ) INLINE ::lConfigured := .f.,;
                                     ::lFootSep := ! Empty( ::cFootSep := cFootSep )

   ACCESS HeadSep             INLINE ::cHeadSep       // Head separator character
   ASSIGN HeadSep( cHeadSep ) INLINE ::lConfigured := .f.,;
                                     ::lHeadSep := ! Empty( ::cHeadSep := cHeadSep )

   ACCESS Freeze              INLINE ::nFrozenCols     // Number of columns to freeze/frozen
   ASSIGN Freeze( nHowMany )  INLINE ::SetFrozenCols( nHowMany, .t. ), ::lConfigured := .f., ::nFrozenCols

   ACCESS GoBottomBlock       INLINE ::bGoBottomBlock
   ASSIGN GoBottomBlock( bBlock )  INLINE ::bGoBottomBlock := iif(Valtype(bBlock)="B",bBlock,::bGoBottomBlock),::nLastPhysRow := iif(!::lDataSource,Eval(bBlock), ::nLastPhysRow), iif(!::lDataSource .AND. ::bGoTopBlock!=NIL, Eval(::bGoTopBlock),.T.)

   ACCESS GoTopBlock          INLINE ::bGoTopBlock
   ASSIGN GoTopBlock( bBlock )  INLINE ::bGoTopBlock := iif(Valtype(bBlock)="B",bBlock,::bGoTopBlock)


   ACCESS HitBottom           INLINE ::lHitBottom
   ASSIGN HitBottom(l)        INLINE ::lHitBottom := iif(valtype(l)="L",l,::lHitBottom)

   ACCESS HitTop              INLINE ::lHitTop
   ASSIGN HitTop(l)           INLINE ::lHitTop := iif(valtype(l)="L",l,::lHitTop)


#ifdef HB_COMPAT_C53

   ACCESS MColPos             INLINE ::nMColPos
   ASSIGN MColPos(nCol)       INLINE ::nMColPos := iif(Valtype(nCol)="N",nCol,::nMColPos)

   ACCESS MRowPos             INLINE ::nMRowPos
   ASSIGN MRowPos(nRow)       INLINE ::nMRowPos := iif(Valtype(nRow)="N",nRow,::nMRowPos)

   ACCESS Message             INLINE ::cMessage
   ASSIGN Message(cMsg)       INLINE ::cMessage := iif(Valtype(cMsg)="C",cMsg,::cMessage)

#endif


   ACCESS SkipBlock           INLINE ::bSkipBlock
   ASSIGN SkipBlock( bBlock ) INLINE ::bSkipBlock := iif(valtype(bBlock)="B",bBlock,::bSkipBlock) 

   ACCESS Stable              INLINE ::lStable
   ASSIGN Stable(l)           INLINE ::lStable := iif(valtype(l)="L",l,::lStable)


/*
2008/Aug/30 - Eduardo Fernandes <modalsist>
The PhysRow accesses are for ColorRect tests.
*/
   ACCESS PhysRowPos      INLINE ::nPhysRow
   ACCESS PhysRowTop      INLINE ::nPhysRowTop
   ACCESS PhysRowBottom   INLINE ::nPhysRowBottom
   ACCESS LastPhysRow     INLINE ::nLastPhysRow

   METHOD New( nTop, nLeft, nBottom, nRight, lDataBase )  // Constructor
   METHOD Down()                          // Moves the cursor down one row
   METHOD End()                           // Moves the cursor to the rightmost visible data column
   METHOD GoBottom()                      // Repositions the data source to the bottom of file
   METHOD GoTop()                         // Repositions the data source to the top of file
   METHOD Home()                          // Moves the cursor to the leftmost visible data column
   METHOD Left()                          // Moves the cursor left one column
   METHOD PageDown()                      // Repositions the data source downward
   METHOD PageUp()                        // Repositions the data source upward
   METHOD PanEnd()                        // Moves the cursor to the rightmost data column
   METHOD PanHome()                       // Moves the cursor to the leftmost visible data column
   METHOD PanLeft()                       // Pans left without changing the cursor position
   METHOD PanRight()                      // Pans right without changing the cursor position
   METHOD Right()                         // Moves the cursor right one column
   METHOD Up()                            // Moves the cursor up one row

   METHOD AddColumn( oCol )
   METHOD DelColumn( nPos )                // Delete a column object from a browse
   METHOD InsColumn( nPos, oCol )          // Insert a column object in a browse
   METHOD GetColumn( nColumn )             // Gets a specific TBColumn object
   METHOD SetColumn( nColumn, oCol )       // Replaces one TBColumn object with another
   METHOD ColWidth( nColumn )              // Returns the display width of a particular column
   METHOD ColorRect( aNewArea, aNewColor ) // Alters the color of a rectangular group of cells
   METHOD Configure( nMode )               // Reconfigures the internal settings of the TBrowse object
                                           // nMode is an undocumented parameter in CA-Cl*pper
   METHOD Hilite()                         // Highlights the current cell
   METHOD DeHilite()                       // Dehighlights the current cell
   METHOD Invalidate()                     // Forces entire redraw during next stabilization
   METHOD RefreshAll()                     // Causes all data to be recalculated during the next stabilize
   METHOD RefreshCurrent()                 // Causes the current row to be refilled and repainted on next stabilization loop
                                                                                                                    
   METHOD ForceStable()                    // Performs a full stabilization
   METHOD Stabilize()                      // Performs incremental stabilization

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

   /* 06/08/2004 - <maurilio.longo@libero.it>
                   next two DATAs should not be public, at least protected
   */
   DATA aColumnsSep           // Holds the column position where seperators are marked . for Wvt_DrawGridVert()
   DATA aColorSpec            // Holds colors of Tbrowse:ColorSpec


   HIDDEN:        /* H I D D E N */

   METHOD PosCursor()                     // Positions the cursor to the beginning of the call, used only when autolite==.F.
   METHOD LeftDetermine()                 // Determine leftmost unfrozen column in display
   METHOD DispCell( nRow, nCol, nColor )  // Displays a single cell and returns position of first char of displayed value if needed
   METHOD HowManyCol()                    // Counts how many cols can be displayed
   METHOD RedrawHeaders( nWidth )         // Repaints TBrowse Headers
   METHOD Moved()                         // Every time a movement key is issued I need to reset certain properties
                                          // of TBrowse, I do these settings inside this method
   METHOD EvalSkipBlock( nSkip )          // Eval skip block

   METHOD WriteMLineText( cStr, nPadLen, lHeader, cColor ) // Writes a multi-line text where ";" is a line break, lHeader
                                                           // is .T. if it is a header and not a footer
   METHOD SetFrozenCols( nHowMany )       // Handles freezing of columns
   METHOD SetColumnWidth( oCol )          // Calcs width of given column
   METHOD SetBorder( cBorder )            // Assign characters for TBrowse border
   METHOD DrawRow(nRow)                   // Draw one or all rows in stabilization
   METHOD CheckRowsToBeRedrawn()
   METHOD CheckRowPos()
   METHOD ColInfoArray(oCol,lAdd)         // Build array with column info.
   METHOD PerformStabilization()          // "Real" stabilization procedure
   METHOD PreConfigHorizontal( uValue )   // This method calculates variables related to horizontal coordinates
   METHOD PreConfigVertical( uValue )     // This method calculates variables related to vertical coordinates

/*
2005/Sep/15 - Eduardo Fernandes <modalsist>
For ColorRect control
*/

   METHOD CheckColorRect(aRect)                // Check if aRect already exist 
   METHOD DeleteColorRect(nRect)               // Delete one or all colorrects areas.
   METHOD DrawColorRect(aRect,lForceDraw)      // Draw colorrect area
   METHOD FindColorRect( nRow,nCol )           // Return colorrect that correspond to nRow and nCol
   METHOD IsCellInColorRect(nRow,nCol)         // Return true if <nRow> and <nCol> are in any colorrect area.                                 
   METHOD RefreshColorRect()                   // Refresh colorrect area after refreshcurrent called by user.
   METHOD RefreshRow()                         // Internal Refresh current.
   METHOD ResetColorRect()                     // Reset colorrect area after stabilization.
   METHOD SplitColorRect(nRow,nRect)           // Split colorrect area where nRow is in nRect colorrrect.
   METHOD UpdateColorRect(nRect,aRect,aColor)  // Update colorrect area 


   METHOD RefreshPhysRow()                  // Set Top and Bottom Physical rows.
   METHOD SetRowPos(nNewRow)                // Set position of ::nRowPos to the new row position


   DATA aColorRect                        // ColorRect array
   DATA lIsColorRect                      // True if ColorRect was called, otherwise false.

   DATA nLastPhysRow                      // Last physical row in tbrowse.
   DATA nPhysRow                          // Current physical row position. On the contrary of ::nRowPos ::nPhysRow return the real row pos, starting at the nBeginRowData and ending at last one.
   DATA nPhysRowTop                       // First screen physical row
   DATA nPhysRowBottom                    // Last screen physical row
   DATA nPrevPhysRowTop                   // The previous physical row top before cursor movement

   DATA lDataSource                       // Flag to identify if data base is used in TBrowse (called by TBrowseDB function).
   DATA cAlias                            // Alias name of dbf attached in TBrowse
   DATA lEnableHilite                     // enable/disable hilite for colorrect call.
   DATA nWhatMoved                        // return what method has moved the cursor.

/*****/

   DATA aRowsToRedraw                     // Array of logical items indicating, is appropriate row need to be redraw
   DATA lHeaders                          // Internal variable which indicates whether there are column headers to paint
   DATA lFooters                          // Internal variable which indicates whether there are column footers to paint

   DATA lHeadSep                 INIT .f. // Internal variable which indicates whether TBrowse has line headers to paint
   DATA lFootSep                 INIT .f. // Internal variable which indicates whether TBrowse has line footers to paint
   DATA lColHeadSep              INIT .f. // Internal variable which indicates whether at least a TBColumn has line headers to paint
   DATA lColFootSep              INIT .f. // Internal variable which indicates whether at least a TBColumn has line footers to paint

   DATA lRedrawFrame                      // True if I need to redraw Headers/Footers
   DATA nColsWidth                        // Total width of visible columns plus ColSep
   DATA nColsVisible                      // Number of columns that fit on the browse width

   DATA lIntHitTop                        // Internal Bottom reached flag
   DATA lIntHitBottom                     // Internal Top reached flag

   DATA lHitBottom                        // Indicates the end of available data
   DATA lHitTop                           // Indicates the beginning of available data

   DATA nRecsToSkip                       // Recs to skip on next Stabilize()
   DATA nNewRowPos                        // Next position of data source (after first phase of stabilization)
   DATA nLastRetrieved                    // Position, relative to first row, of last retrieved row (with an Eval(::bSkipBlock, n))
   DATA nBeginRowData                     // Begin Row data, the first row data visible into browse.
   DATA nColPos                           // Current cursor col position
   DATA nRowPos                           // Current cursor row position

   DATA nwBottom                   INIT 0 // Bottom row number for the TBrowse display
   DATA nwLeft                     INIT 0 // Leftmost column for the TBrowse display
   DATA nwRight                    INIT 0 // Rightmost column for the TBrowse display
   DATA nwTop                      INIT 0 // Top row number for the TBrowse display

   DATA lAutoLite                         // Logical value to control highlighting
   DATA cBorder                    INIT ""

   DATA uCargo                            // User-definable variable (any data type)

   DATA cColorSpec
   DATA cColSep                           // Column separator character
   DATA cFootSep                          // Footing separator character
   DATA cHeadSep                          // Head separator character

   DATA nHeaderHeight                     // How many lines is highest Header/Footer and so how many lines of
   DATA nFooterHeight                     // screen space I have to reserve
   DATA nFrozenWidth                      // How many screen column are not available on the left side of TBrowse display
                                          // > 0 only when there are frozen columns
   DATA nFrozenCols                       // Number of frozen columns on left side of TBrowse
   DATA nColCount                         // Number of total columns in TBrowse
   DATA lNeverDisplayed                   // .T. if TBrowse has never been stabilized()

   DATA aColsInfo                         // Columns configuration array
   DATA nVisWidth                         // Visible width of Browser
   DATA lConfigured                       // Specifies whether tBrowse is already configured or not

   DATA cSpacePre                         // Blank Space prior to first column
   DATA cSpaceLast                        // Blank space after the last column
   DATA cSpaceWidth                       // Spaces of browse width

/* 2005/Sep/15 - Eduardo Fernandes
I think that aHighCellColor isn't necessary anymore*/
// DATA aHighCellColor                    // Result of colorblock evaluation of currently highlighted cell

   DATA bGoBottomBlock                    // Code block executed by TBrowse:goBottom()
   DATA bGoTopBlock                       // Code block executed by TBrowse:goTop()
   DATA nLeftVisible                      // Indicates position of leftmost unfrozen column in display
   DATA nRightVisible                     // Indicates position of rightmost unfrozen column in display
   DATA nRowCount                         // Number of visible data rows in the TBrowse display
   DATA bSkipBlock                        // Code block used to reposition data source
   DATA lStable                           // Indicates if the TBrowse object is stable


#ifdef HB_COMPAT_C53
   DATA Rect
   DATA aVisibleCols
   DATA aSetStyle
   DATA cMessage                          // Message to send for GET object, if any.
   DATA nMColpos                          // Mouse col position
   DATA nMRowPos                          // Mouse row position
   DATA aKeys
#endif


ENDCLASS

//-------------------------------------------------------------------//

METHOD New( nTop, nLeft, nBottom, nRight, lDataBase ) CLASS TBrowse

/* 2005/Sep/15 - Eduardo Fernandes
The <lDataBase> parameter is necessary to assign ::nLastPhysRow that is
used to control ColorRect. This value differ if a database is used or not.
See TBrowseNew() and TBrowseDB() at bottom of this source code.
Please, don't remove it. */


   DEFAULT  nTop    TO 0
   DEFAULT  nLeft   TO 0
   DEFAULT  nBottom TO MaxRow()
   DEFAULT  nRight  TO MaxCol()
   DEFAULT  lDataBase TO .F.


   ::nwTop           := nTop
   ::nwLeft          := nLeft
   ::nwBottom        := nBottom
   ::nwRight         := nRight
   ::nRowCount       := nBottom - nTop + 1
   ::nBeginRowData   := nTop
   ::lAutoLite       := .T.
   ::nLeftVisible    := 1
   ::nRightVisible   := 1
   ::nColPos         := 1
   ::lHitBottom      := .F.
   ::lIntHitBottom   := .F.
   ::lHitTop         := .F.
   ::lIntHitTop      := .F.
   ::cColorSpec      := SetColor()
   ::cColSep         := " "
   ::cFootSep        := ""
   ::cHeadSep        := ""
   ::nRowPos         := 1
   ::nNewRowPos      := 1
   ::lStable         := .F.
   ::nLastRetrieved  := 1
   ::nRecsToSkip     := 0
   ::aRowsToRedraw   := {}
   ::lHeaders        := .F.
   ::lFooters        := .F.

   ::lHeadSep := ::lFootSep := ::lColHeadSep := ::lColFootSep := .F.

   ::lRedrawFrame    := .T.
   ::nColsWidth      := 0
   ::nColsVisible    := 0
   ::nHeaderHeight   := 0
   ::nFooterHeight   := 0
   ::nFrozenWidth    := 0
   ::nFrozenCols     := 0
   ::nColCount       := 0
   ::lNeverDisplayed := .T.
   ::cBorder         := ""

   ::aColsInfo       := {}
   ::nVisWidth       := nRight - nLeft + 1
   ::lConfigured     := .f.
   ::aColorSpec      := {}

 #ifdef HB_COMPAT_C53
   ::nMColPos         := 0
   ::nMRowPos         := 0
   ::rect            := { nTop, nLeft, nBottom, nRight }
   ::aVisibleCols    := {}
   ::cMessage        := ''
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

/* 2008/Aug/30 - Eduardo Fernandes <modalsist>
For ColorRect() control
*/
   ::aColorRect      := {} 
   ::lIsColorRect    := .F.
   ::lDataSource     := ( lDataBase .AND. Used() )

   IF ::lDataSource
      ::cAlias := Alias()
      IF !Empty(::cAlias)
         ::nLastPhysRow := &(::cAlias)->( LastRec() )
      ELSE
        ::lDataSource := .F.
        ::nLastPhysRow := ::nRowCount
      ENDIF
   ELSE
      // nLastPhysRow can change in GoBotttom ASSIGN.
      ::nLastPhysRow := ::nRowCount 
   ENDIF

   ::nPhysRow         := 1
   ::nPhysRowTop      := 1
   ::nPhysRowBottom   := Min(::nRowCount,::nLastPhysRow) 
   ::lEnableHilite    := .T.
   ::nWhatMoved       := 0
   ::nPrevPhysRowTop  := ::nPhysRowTop

/*****************/


   Return Self

//-------------------------------------------------------------------//

METHOD Invalidate() CLASS TBrowse

   ::RefreshAll()
   ::lRedrawFrame := .T.

   Return Self

//-------------------------------------------------------------------//

METHOD RefreshAll() CLASS TBrowse


   AFill( ::aRowsToRedraw, .T. )

   ::lStable := .F.

   Return Self

//-------------------------------------------------------------------//

METHOD Configure( nMode ) CLASS TBrowse

 LOCAL n, nHeight, aCol, xVal, nFreeze, oErr, lInitializing := .f.
 LOCAL oCol

   default nMode to 0

   if nMode == 3
      nMode := 0
      lInitializing := .t.
   endif

   if ::nColPos > ::nColCount
      ::nColPos := ::nColCount
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

      oCol := aCol[ COLINFO_OBJ ]

      if ( nMode <= 1 .and. !::lNeverDisplayed ) .or. lInitializing

         xVal := Eval( oCol:Block )

         
         aCol[ COLINFO_TYPE      ] := valtype( xVal )
         aCol[ COLINFO_HEADING   ] := oCol:heading
         aCol[ COLINFO_FOOTING   ] := oCol:footing
         if ! aCol[ COLINFO_ISSETWIDTH ]
            aCol[ COLINFO_WIDTH  ] := ::SetColumnWidth( oCol )
         endif
         aCol[ COLINFO_CELLWIDTH ] := Min( aCol[ COLINFO_WIDTH ], LenVal( xVal, aCol[ COLINFO_TYPE ], oCol:Picture ) )
         aCol[ COLINFO_COLSEP    ] := iif( oCol:ColSep != NIL, oCol:ColSep, ::ColSep )
         aCol[ COLINFO_SEPWIDTH  ] := Len( aCol[ COLINFO_COLSEP ] )

         if aCol[ COLINFO_TYPE ] == 'D' .and. empty( oCol:Picture )
            oCol:Picture := '@D'
         endif

         aCol[ COLINFO_DRAW_COLSEP ] := aCol[ COLINFO_WIDTH ] > 0
      endif

      if nMode = 0 .or. nMode = 2 .or. lInitializing
         aCol[ COLINFO_COLSEP ] := iif( oCol:ColSep != NIL, oCol:ColSep, ::ColSep )
      endif

      if nMode < 2 .or. ::lNeverDisplayed
         // Are there column headers/footers/separators to paint ?
         if ! Empty( aCol[ COLINFO_HEADING ] )
            ::lHeaders := .T.
         endif
         if ! Empty( aCol[ COLINFO_FOOTING ] )
            ::lFooters := .T.
         endif
         /* as soon as we find one, we stop testing aCol[COLINFO_OBJ]:XX to speed things up */
         if ! ::lColHeadSep .AND. ! Empty( oCol:HeadSep )
            ::lColHeadSep := .T.
         endif
         if ! ::lColFootSep .AND. ! Empty( oCol:FootSep )
            ::lColFootSep := .T.
         endif
      endif

      if ::lHeaders .AND. !Empty( aCol[ COLINFO_HEADING ] )
         nHeight := Len( aCol[ COLINFO_HEADING ] ) - Len( StrTran( aCol[ COLINFO_HEADING ], ";" ) ) + 1

         if nHeight > ::nHeaderHeight
            ::nHeaderHeight := nHeight
         endif

      endif

      if ::lFooters .AND. !Empty( aCol[ COLINFO_FOOTING ] )
         nHeight := Len( aCol[ COLINFO_FOOTING ] ) - Len( StrTran( aCol[ COLINFO_FOOTING ], ";" ) ) + 1

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
                    // If it didn't fit, it generate error.

      ::nVisWidth := ::nwRight - ::nwLeft + 1

      // 20/nov/2000 - maurilio.longo@libero.it
      // If I add (or remove) header or footer (separator) I have to change number
      // of available rows
      //
      ::nRowCount := ::nwBottom - ::nwTop + 1 - iif( ::lHeaders, ::nHeaderHeight, 0 ) - ;
                     iif( ::lFooters, ::nFooterHeight, 0 ) - ;
                     iif( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ) - ;
                     iif( ::lFootSep .OR. ::lColFootSep, 1, 0 )

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
   ::nBeginRowData := ::nwTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + ;
                           iif( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ) - 1


   if Len( ::aRowsToRedraw ) <> ::nRowCount
      ::aRowsToRedraw := Array( ::nRowCount )
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

METHOD ColInfoArray( oCol,lAdd ) CLASS TBrowse
   LOCAL aCol,aDefColor

   DEFAULT lAdd TO .f.

   if !lAdd  .and. HB_ISOBJECT( oCol ) .and. ( valtype( oCol:block ) == 'B' )

      aDefColor := DefColorToDisp( oCol:defColor, ::aColorSpec )
      oCol:defColor := aDefColor

      aCol := { oCol, valtype( Eval( oCol:block )), ::SetColumnWidth( oCol ),;
                '', '', '', 0, '', 0, oCol:defColor, .f., NIL, .t., 0 }
   else
      aCol := { oCol, '', 0, '', '', '', 0, '', 0, {}, .f., NIL, .t., 0 }
   endif

Return (aCol)


//-------------------------------------------------------------------//
//
//   Adds a TBColumn object to the TBrowse object
//
METHOD AddColumn( oCol ) CLASS TBrowse

   ::Moved()

   aadd( ::aColsInfo, ::ColInfoArray( oCol, .T.) )

   ::nColCount++

   if ::nColCount == 1
      ::nLeftVisible := 1
      ::nColPos := 1
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

      if nPos > ::nColCount
         aAdd( ::aColsInfo, ::ColInfoArray( oCol ) )
      else
         aIns( ::aColsInfo, nPos, ::ColInfoArray( oCol ), .t. )
      endif

      ::nColCount++

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

   if 0 < nColumn .AND. nColumn <= ::nColCount
      ::Moved()

      oOldCol := ::aColsInfo[ nColumn, COLINFO_OBJ ]

      ::aColsInfo[ nColumn ] := ::ColInfoArray( oCol )

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

   Return iif( 0 < nColumn .AND. nColumn <= ::nColCount, ::aColsInfo[ nColumn, COLINFO_OBJ ], NIL )

//-------------------------------------------------------------------//
//
//  Delete a column given the column position
//
METHOD DelColumn( nPos ) CLASS TBrowse

   LOCAL oCol

   if nPos > ::nColCount .or. nPos < 1
      return NIL
   endif

   //  Need to adjust variables in case last column is deleted
   //  Fixes and important bug
   //

   ::Moved()

   oCol := ::aColsInfo[ nPos, COLINFO_OBJ ]

   if nPos == ::nColPos .or. nPos == ::nColCount .or.;
              ::nColPos == ::nColCount .or. ::nRightVisible == ::nColCount

      if ::nLeftVisible == ::nRightVisible
         ::nLeftVisible--
      endif
      ::nRightVisible--
      //::colPos++
      if ::ncolPos == ::nColCount
         ::ncolpos--
      endif
   endif

   ::nColCount--

   ADel( ::aColsInfo, nPos, .T. )

   if ::nColCount < ::nFrozenCols
      ::nFrozenCols := 0
   endif

   if ::nColCount == 0
      ::lNeverDisplayed := .t.
      ::aRowsToRedraw := {}
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

Return iif( 0 < nColumn .AND. nColumn <= ::nColCount, ::aColsInfo[ nColumn, COLINFO_WIDTH ], 0 )

//-------------------------------------------------------------------//

METHOD SetFrozenCols( nHowMany, lLeft ) CLASS TBrowse

   LOCAL nCol, aCol
   LOCAL nOldFreeze      := ::nFrozenCols
   LOCAL nOldFrozenWidth := ::nFrozenWidth
   LOCAL oCol

   Default lLeft to .f.

   ::nFrozenCols  := Min( nHowMany, ::nColCount )

   // Space inside TBrowse window reserved for frozen columns
   ::nFrozenWidth := 0

   // If I've never displayed this TBrowse before I cannot calc occupied space since
   // columns:width is not yet set, ::Stabilize() will call me later
   //
   if ! ::lNeverDisplayed

      if nHowMany > 0
         for each aCol in ::aColsInfo
            nCol := HB_EnumIndex()
            oCol := aCol[ COLINFO_OBJ ]
            if nCol <= nHowMany
               ::nFrozenWidth += aCol[ COLINFO_WIDTH ]
               if nCol < ::nColCount .and. aCol[ COLINFO_WIDTH ] > 0
                  ::nFrozenWidth += ::aColsInfo[ nCol + 1, COLINFO_SEPWIDTH ]
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
            oCol := aCol[ COLINFO_OBJ ]
            aCol[ COLINFO_WIDTH     ]  := ::SetColumnWidth( oCol )
            aCol[ COLINFO_CELLWIDTH ]  := Min( aCol[ COLINFO_WIDTH ], LenVal( Eval( oCol:block ), aCol[ COLINFO_TYPE ], oCol:Picture ) )
            aCol[ COLINFO_ISSETWIDTH ] := .f.
         NEXT
      endif

      FOR EACH aCol IN ::aColsInfo
         oCol := aCol[ COLINFO_OBJ ]
         if HB_EnumIndex() > ::nFrozenCols
            if ::nFrozenCols > 0
               // If there are columns which are larger than TBrowse display width minus
               // frozen columns reserved space, shrihnk them to fit
               //

               if ::nFrozenWidth + aCol[ COLINFO_WIDTH ] > ::nVisWidth
                  aCol[ COLINFO_WIDTH     ] := ::nVisWidth - ::nFrozenWidth
                  aCol[ COLINFO_CELLWIDTH ] := Min( aCol[ COLINFO_WIDTH ], LenVal( Eval( oCol:block ), aCol[ COLINFO_TYPE ], oCol:Picture ) )
                  aCol[ COLINFO_ISSETWIDTH  ] := .t.
               endif

            else
               // Reset column widths
               //

               aCol[ COLINFO_WIDTH     ] := ::SetColumnWidth( aCol[ COLINFO_OBJ ] )
               aCol[ COLINFO_CELLWIDTH ] := Min( aCol[ COLINFO_WIDTH ], LenVal( Eval( oCol:block ), aCol[ COLINFO_TYPE ], oCol:Picture ) )
               aCol[ COLINFO_ISSETWIDTH  ] := .f.

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
//                         Up-down (Row) movements
//
//-------------------------------------------------------------------//

METHOD Down() CLASS TBrowse


   ::Moved()

   ::nRecsToSkip++

   IF ::lIsColorRect
      ::DrawColorRect() // redraw all colorrects 
   ENDIF

   ::nPrevPhysRowTop := ::nPhysRowTop
   ::nPhysRow++

   IF ::nPhysRow > ::nLastPhysRow
      ::nPhysRow := ::nLastPhysRow
   ENDIF


   ::nWhatMoved := TBR_DOWN_MOVED


Return Self

//-------------------------------------------------------------------//

METHOD Up() CLASS TBrowse


   ::Moved()

   ::nRecsToSkip--

   IF ::lIsColorRect
      ::DrawColorRect() // redraw all colorrects 
   ENDIF

   ::nPrevPhysRowTop := ::nPhysRowTop

   ::nPhysRow--

   IF ::nPhysRow < 1
      ::nPhysRow := 1
   ENDIF


   ::nWhatMoved := TBR_UP_MOVED

Return Self

//-------------------------------------------------------------------//

METHOD PageDown() CLASS TBrowse


   ::Moved()

   ::nRecsToSkip := ( ::nRowCount - ::nRowPos ) + ::nRowCount

   ::nPrevPhysRowTop := ::nPhysRowTop

   IF ::lDataSource
      ::nPhysRow += ::nRowCount
   ELSE
      ::nPhysRow += ::nRecsToSkip
   ENDIF

   IF ::nPhysRow > ::nLastPhysRow
      ::nPhysRow := ::nLastPhysRow
   ENDIF

   ::nWhatMoved := TBR_PGDOWN_MOVED


   Return Self

//-------------------------------------------------------------------//

METHOD PageUp() CLASS TBrowse

   ::Moved()

   ::nRecsToSkip := - ( ( ::nRowPos - 1 ) + ::nRowCount )

   ::nPrevPhysRowTop := ::nPhysRowTop

   IF ::lDataSource
      ::nPhysRow -= ::nRowCount
   ELSE
      ::nPhysRow -= Abs( ::nRecsToSkip )
   ENDIF

   IF ::nPhysRow < 1
      ::nPhysRow := 1
   ENDIF

   ::nWhatMoved := TBR_PGUP_MOVED

   Return Self

//-------------------------------------------------------------------//

METHOD GoBottom() CLASS TBrowse

   LOCAL nToTop

   ::Moved()

   ::nPrevPhysRowTop := ::nPhysRowTop

   Eval( ::bGoBottomBlock )

   //   Skip back from last record as many records as TBrowse can hold
   nToTop := Abs( ::EvalSkipBlock( - ( ::nRowCount - 1 ) ) )

   //   From top of TBrowse new row position is nToTop + 1 records away
   ::nNewRowPos := nToTop + 1

   //   Last read record is first record inside TBrowse
   ::nLastRetrieved := 1
   ::nRowPos := ::nRowCount
   ::RefreshAll()

   ::nPhysRow := ::nLastPhysRow

   ::nWhatMoved := TBR_BOTTOM_MOVED

   Return Self

//-------------------------------------------------------------------//

METHOD GoTop() CLASS TBrowse

   ::Moved()

   ::nPrevPhysRowTop := ::nPhysRowTop

   Eval( ::bGoTopBlock )
   ::EvalSkipBlock( 0 ) // required for compatibility
   ::nLastRetrieved := 1
   ::nNewRowPos     := 1
   ::RefreshAll()

   ::nPhysRow := 1

   ::nWhatMoved := TBR_TOP_MOVED

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

   IF ::lIsColorRect
      ::DrawColorRect( ::nRowPos )
   ENDIF

   if ::nColPos <> ::nLeftVisible
      ::nColPos := ::nLeftVisible
      ::lRedrawFrame := .T.
      ::RefreshRow()
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD End() CLASS TBrowse

   ::Moved()

   IF ::lIsColorRect
      ::DrawColorRect( ::nRowPos )
   ENDIF

   if ::nColPos < ::nRightVisible
      ::nColPos := ::nRightVisible
      ::lRedrawFrame := .T.
      ::RefreshRow()
   else
      // Can go "out of bounds", here we behave like clipper
      ::nColPos := ::nRightVisible
   endif


   Return Self

//-------------------------------------------------------------------//

METHOD Right() CLASS TBrowse

   ::Moved()
  
   IF ::lIsColorRect
      ::DrawColorRect( ::nRowPos )
   ENDIF

   if ::nColPos < ::nRightVisible
      ::nColPos++
   else
      if ::nColPos < ::nColCount

         ::nRightVisible++
         ::nLeftVisible := ::LeftDetermine()
         ::nColPos++
         ::lRedrawFrame := .T.
         ::RefreshAll()
      else
         /* 09/08/2004 - <maurilio.longo@libero.it>
                         In a ! ::lStable state clipper moves ::colpos past ::nColCount or
                         before column 1, so here and on _Left(), Home(), End(), PanEnd()
                         PanHome() methods I let it go "out of bounds",
                         PerformStabilization() gives ::nColPos a correct value
         */
         ::nColPos++
      endif
   endif


   Return Self

//-------------------------------------------------------------------//

METHOD Left() CLASS TBrowse
LOCAL leftVis

   ::Moved()

   IF ::lIsColorRect
      ::DrawColorRect( ::nRowPos )
   ENDIF

   IF ::nColPos > ::nLeftVisible .OR.;
      ( ::nColPos <= ::nFrozenCols + 1 .and. ::nColPos > 1 )
      ::nColPos--
   ELSE
      IF ::nColPos <= Max( ::nLeftVisible, ::nFrozenCols ) .AND. ::nColPos > 1
         leftVis := ::nLeftVisible
         WHILE leftVis == ::nLeftVisible
            ::nRightVisible--
            ::nLeftVisible := ::LeftDetermine()
         END
         ::nColPos--
         ::lRedrawFrame := .T.
         ::RefreshAll()
      ELSE
         // Can go "out of bounds", here we behave like clipper
         ::nColPos--
      ENDIF
   ENDIF


   Return Self

//-------------------------------------------------------------------//

METHOD PanEnd() CLASS TBrowse

   ::Moved()

   IF ::lIsColorRect
      ::DrawColorRect( ::nRowPos )
   ENDIF

   if ::nColPos < ::nColCount
      if ::nRightVisible <  ::nColCount
         ::nRightVisible := ::nColCount
         ::nLeftVisible  := ::LeftDetermine()
         ::nColPos      := ::nRightVisible
         ::lRedrawFrame := .T.
         ::RefreshAll()
      else
         ::nColPos      := ::nRightVisible
         ::RefreshRow()
      endif
   else
      // Can go "out of bounds", here we behave like clipper
      ::nColPos := ::nColCount
   endif

   Return Self

//-------------------------------------------------------------------//

METHOD PanHome() CLASS TBrowse

   ::Moved()

   IF ::lIsColorRect
      ::DrawColorRect( ::nRowPos )
   ENDIF

   if ::nColPos > 1
      if ::nLeftVisible > ::nFrozenCols + 1
         ::nLeftVisible  := ::nFrozenCols + 1
         ::nColPos      := 1
         ::RefreshAll()
         ::lRedrawFrame := .T.
      else
         ::nColPos      := 1
         ::RefreshRow()
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

   IF ::lIsColorRect
      ::DrawColorRect( ::nRowPos )
   ENDIF

   // The same as if ::nLeftVisible > iif(::nFrozenCols > 0, ::nFrozenCols + 1, 1)
   if ::nLeftVisible > ::nFrozenCols + 1

      leftVis := ::nLeftVisible

      /* While space left available by columns exiting to the right side of tbrowse
         is not enough to contain a new column to the left (::nLeftVisible doesn't change)
      */
      while leftVis == ::nLeftVisible

         ::nRightVisible--
         ::nLeftVisible := ::LeftDetermine()

      enddo

      /* Since panel "shifts" to the right, ::ncolPos could end up "out of" the
         right side of tbrowse, so, change it to ::nRightVisible if this happens
      */
      ::nColPos := Min( ::nColPos, ::nRightVisible )

      ::lRedrawFrame := .T.
      ::RefreshAll()
   endif

Return Self

//-------------------------------------------------------------------//

METHOD PanRight() CLASS TBrowse

   LOCAL leftVis

   /* 10/08/2004 - <maurilio.longo@libero.it>
                   Since this method can even don't execute any task,
                   I think ::Moved() could be called inside outmost
                   if...
   */
   ::Moved()

   IF ::lIsColorRect
      ::DrawColorRect( ::nRowPos )
   ENDIF

   if ::nRightVisible < ::nColCount

      leftVis := ::nLeftVisible

      while leftVis == ::nLeftVisible

         ::nRightVisible++
         ::nLeftVisible  := ::LeftDetermine()

      enddo

      ::nColPos := Max( ::nColPos, ::nLeftVisible )

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


   // Internal flags used to set ::lHitTop/Bottom during next stabilization
   //
   ::lIntHitTop    := .F.
   ::lIntHitBottom := .F.

   // No need to Dehilite() current cell more than once
   //

   if ::lStable
      if ::lAutoLite     // true by default (see method new)
         ::DeHilite()
      else
         ::PosCursor()
      endif
      ::lStable := .F.
   endif


   Return Self

//---------------------------------------------------------------------//

METHOD EvalSkipBlock( nSkip ) CLASS TBrowse
   LOCAL lSign   := (nSkip >= 0)
   LOCAL nSkiped := Eval( ::bSkipBlock, nSkip )

   if ( lSign .and. nSkiped < 0 ) .or. ( !lSign .and. nSkiped > 0 )
      nSkiped := 0
   endif

   Return nSkiped

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Utility Routines
//
//-------------------------------------------------------------------//

METHOD LeftDetermine() CLASS TBrowse

   LOCAL nWidth := ::nFrozenWidth
   LOCAL nCol   := ::nRightVisible

   // If ::nFrozenCols > 0 I don't need to test nCol > 0, if 0 it is the same test
   while nCol > ::nFrozenCols .AND.;
         ( nWidth += ::aColsInfo[ nCol, COLINFO_WIDTH ] + ::aColsInfo[ nCol , COLINFO_SEPWIDTH ] ) < ::nVisWidth

      nCol--
   enddo

   /* Clipper compatible: do not let nCol stop at empty column */
   nCol++
   while nCol <= ::nRightVisible .and. ::aColsInfo[ nCol, COLINFO_WIDTH ] == 0
      nCol++
   enddo

       /* ::nRightVisible could be larger then available space, for example because of
          frozen columns, so nCol-- never gets executed  */
Return Min(nCol, ::nRightVisible)

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

   // They were locals, so now I need to clear them (should fix this)
   //
   nColsWidth   := 0
   nColsVisible := 0

   if ::nFrozenCols > 0
      nColsVisible := 0
      while nColsVisible < ::nFrozenCols .and. nColsVisible < ::nColCount
         nToAdd := ::aColsInfo[ nColsVisible + 1, COLINFO_WIDTH ]

         if nColsVisible >= 1 .and. nColsVisible < ::nColCount .and.;
            ::aColsInfo[ nColsVisible,COLINFO_WIDTH ] > 0
            nToAdd += ::aColsInfo[ nColsVisible + 1, COLINFO_SEPWIDTH ]
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
         ::nRightVisible := nColsVisible
         ::nColsVisible := nColsVisible
         return Self

      endif

      if ::nLeftVisible <= ::nFrozenCols
         ::nLeftVisible := ::nFrozenCols + 1
      endif

   endif

   // BDj notes:
   // Cannot assume that ::nLeftVisible is correct
   // (eg. if ::colPos was assigned ::nRightVisible+1)
   // Must do the following in a loop repeatedly until:
   // (0) ::colPos <= ::nFrozenCols (assume ::colPos > 0)
   // or
   // (1) ::nLeftVisible <= ::colPos <= ::nRightVisible
   // or
   // (2) the above conditions are impossible (runtime error)

   saveColsWidth  := nColsWidth
   tryLeftVisible := ::nLeftVisible

   // ::nColPos is to the left of leftVisible
   if ::nFrozenCols==0 .and. tryLeftVisible > ::nColPos
      tryLeftVisible := ::nColPos
   endif

   do while .t.

      nColsVisible := tryLeftVisible-1

      while nColsVisible < ::nColCount

         // which column is displayed to the left of next col?

         if ::nFrozenCols > 0 .and. nColsVisible+1==tryLeftVisible
            nLeftCol := ::nFrozenCols
         else
            nLeftCol := nColsVisible
         endif

         nToAdd := ::aColsInfo[ nColsVisible + 1, COLINFO_WIDTH ]

         // next, we must check against [nLeftCol], not [nColsVisible]:

         if ( nColsVisible >= tryLeftVisible .or. ::nFrozenCols > 0 ) .and.;
            (nLeftCol > 0) .and.;
            ::aColsInfo[ nLeftCol,COLINFO_WIDTH ] > 0

            nToAdd += ::aColsInfo[ nColsVisible + 1, COLINFO_SEPWIDTH ]
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
      if tryLeftVisible==::nColCount
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

   ::nLeftVisible  := tryLeftVisible //x
   ::nRightVisible := nColsVisible
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

   LOCAL n, nTPos, nBPos
   LOCAL cBlankBox := Space( 9 )
   LOCAL nScreenRowT, nScreenRowB
   LOCAL nLCS
   LOCAL nCol
   LOCAL nColFrom
   LOCAL chSep, cfSep, nSpacePre, nSpaceLast, nLeftCol
   LOCAL ccSep, ncSepWidth
   LOCAL cColor

   DispBegin()

   nSpacePre := INT( ( nWidth - ::nColsWidth ) / 2 )
   nSpaceLast := nWidth - nSpacePre - ::nColsWidth

   nCol := ::nwLeft + iif( ::nFrozenCols > 0, 0, nSpacePre )

   nColFrom := iif( ::nFrozenCols > 0, 1, ::nLeftVisible )

   ::aColumnsSep := {}

   for n := nColFrom to ::nRightVisible
      ::aColsInfo[ n, COLINFO_COLPOS ] := nCol

      nCol += ::aColsInfo[ n, COLINFO_WIDTH ]

      if n < ::nRightVisible
         if ::aColsInfo[ n,COLINFO_WIDTH ] > 0
            aadd( ::aColumnsSep, nCol + int( ::aColsInfo[ n + 1, COLINFO_SEPWIDTH ] / 2 ) )
            nCol += ::aColsInfo[ n + 1, COLINFO_SEPWIDTH ]
         endif
      endif

      if ::nFrozenCols > 0 .and. n == ::nFrozenCols
         n    := ::nLeftVisible - 1
         nCol += nSpacePre
      endif
   next

   if ::lHeaders          // Drawing headers
      // Clear area of screen occupied by headers
      //
      DispBox( ::nwTop, ::nwLeft, ::nwTop + ::nHeaderHeight - 1, ::nwRight, cBlankBox, ::cColorSpec )

      for n := nColFrom to ::nRightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::nLeftVisible
         endif
         setPos( ::nwTop, ::aColsInfo[ n, COLINFO_COLPOS ] )

         cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp(::aColsInfo[n,COLINFO_OBJ]:DefColor,::aColorSpec) , TBC_CLR_HEADING ) - 1 )

         ::WriteMLineText( ::aColsInfo[ n, COLINFO_HEADING ], ;
                           ::aColsInfo[ n, COLINFO_WIDTH ], .T., ;
                           cColor )
      next
   endif

   if ::lHeadSep .OR. ::lColHeadSep    //Draw horizontal heading separator line
      nScreenRowT := ::nBeginRowData

   endif

   if ::lFootSep .OR. ::lColFootSep    //Draw horizontal footing separator line
      nScreenRowB := ::nwBottom - iif( ::lFooters, ::nFooterHeight, 0 )

   endif

   nTPos := nBPos := ::nwLeft

   chSep := ::HeadSep  // default HeadSep
   cfSep := ::FootSep  // default FootSep

   // Draw headin/footing column separator
   for n := iif( ::nFrozenCols > 0, 1, ::nLeftVisible ) to ::nRightVisible

      // colsep's width will be needed later
      ccSep := if( ::aColsInfo[ n,COLINFO_OBJ ]:ColSep == nil, ::ColSep, ;
                              ::aColsInfo[ n,COLINFO_OBJ ]:ColSep )

      ncSepWidth := if( ccSep == nil, 0, len(ccSep) )

      // which column is displayed to the left of current col?
      if ::nFrozenCols > 0 .and. n == ::nLeftVisible
         nLeftCol := ::nFrozenCols
      else
         nLeftCol := n - 1
      endif

      if (::nFrozenCols > 0  .and. n == ::nFrozenCols + 1) .or.;
         (::nFrozenCols == 0 .and. n == ::nLeftVisible )
         n     := ::nLeftVisible

         // we need to draw headSep for the nSpacePre gap
         if ! Empty( chSep := if( ::aColsInfo[ n,COLINFO_OBJ ]:HeadSep == nil, ::HeadSep, ;
                                  ::aColsInfo[ n,COLINFO_OBJ ]:HeadSep ) )
            if nLeftCol > 0 .and. ::aColsInfo[ nLeftCol, COLINFO_WIDTH ] > 0 .and.;
               ::nFrozenCols > 0
               DispOutAT( nScreenRowT, nTPos - min( len(chSep), ncSepWidth), chSep, ::cColorSpec )
            endif
            DispOutAT( nScreenRowT, nTPos, Replicate( Right( chSep, 1 ), nSpacePre ), ::cColorSpec )

         elseif ::lColHeadSep
            DispOutAT( nScreenRowT, nTPos, Replicate( Space(1), nSpacePre ), ::cColorSpec )

         endif

         // we need to draw footSep for the nSpacePre gap
         if ! Empty ( cfSep := if( ::aColsInfo[ n,COLINFO_OBJ ]:FootSep == nil, ::FootSep, ;
                                   ::aColsInfo[ n,COLINFO_OBJ ]:FootSep ) )

            if nLeftCol > 0 .and. ::aColsInfo[ nLeftCol, COLINFO_WIDTH ] > 0 .and. ;
               ::nFrozenCols > 0
               DispOutAT( nScreenRowB, nBPos - min( len(cfSep), ncSepWidth), cfSep, ::cColorSpec )
            endif
            DispOutAT( nScreenRowB, nBPos, Replicate( Right( cfSep, 1 ), nSpacePre ), ::cColorSpec )

         elseif ::lColFootSep
            DispOutAT( nScreenRowB, nBPos, Replicate( Space(1), nSpacePre ), ::cColorSpec )

         endif

         nTPos += nSpacePre
         nBPos += nSpacePre
      endif

      // we need to handle even n == ::nRightVisible in the following block

      if ::aColsInfo[ n, COLINFO_WIDTH ] > 0 .and. n < ::nRightVisible
         nLCS := ::aColsInfo[ n + 1, COLINFO_SEPWIDTH ]
      else
         nLCS := 0
      endif

      if ! Empty( chSep := if( ::aColsInfo[ n,COLINFO_OBJ ]:HeadSep == nil, ::HeadSep, ;
                               ::aColsInfo[ n,COLINFO_OBJ ]:HeadSep ) )

         if nLeftCol>0 .and. n <> ::nLeftVisible .and. ::aColsInfo[ nLeftCol, COLINFO_WIDTH ] > 0
            DispOutAT( nScreenRowT, nTPos - min( len(chSep), ncSepWidth), chSep, ::cColorSpec )
         endif
         DispOutAT( nScreenRowT, nTPos, Replicate( Right( chSep, 1 ), ::aColsInfo[ n, COLINFO_WIDTH ] ), ::cColorSpec )

         nTPos += ::aColsInfo[ n, COLINFO_WIDTH ] + nLCS

      /* If I haven't got a default separator or a colsep for current column, there could
         be a colsep on a next column, so I have to fill the width of this column with spaces.
      */
      elseif ::lColHeadSep
         DispOutAT( nScreenRowT, nTPos, Replicate( Space(1), ::aColsInfo[ n, COLINFO_WIDTH ] + nLCS ), ::cColorSpec )
         nTPos += ::aColsInfo[ n, COLINFO_WIDTH ] + nLCS

      endif

      if ! Empty( cfSep := if( ::aColsInfo[ n,COLINFO_OBJ ]:FootSep == nil, ::FootSep, ;
                               ::aColsInfo[ n,COLINFO_OBJ ]:FootSep ) )

         if Valtype(chSep) <> "U" .and. len(chSep) > len(cfSep)
            cfSep += Replicate( Right( cfSep, 1 ), Len( chSep ) - Len( cfSep ) )
         endif

         if nLeftCol > 0 .and. n <> ::nLeftVisible .and. ::aColsInfo[ nLeftCol, COLINFO_WIDTH ] > 0
            DispOutAT( nScreenRowB, nBPos - min( len(cfSep), ncSepWidth), cfSep, ::cColorSpec )
         endif
         DispOutAT( nScreenRowB, nBPos, Replicate( Right( cfSep, 1 ), ::aColsInfo[ n, COLINFO_WIDTH ] ), ::cColorSpec )

         nBPos += ::aColsInfo[ n, COLINFO_WIDTH ] + nLCS

      elseif ::lColFootSep
         DispOutAT( nScreenRowB, nBPos, Replicate( Space(1), ::aColsInfo[ n, COLINFO_WIDTH ] + nLCS ), ::cColorSpec )
         nBPos += ::aColsInfo[ n, COLINFO_WIDTH ] + nLCS

      endif

   next

   if nSpaceLast > 0
      // right gap of spaces (nSpaceLast) on Header
      if ! Empty( chSep )
         DispOutAT( nScreenRowT, nTPos, Replicate( Right( chSep, 1 ), nSpaceLast ), ::cColorSpec )

      elseif ::lColHeadSep
         DispOutAT( nScreenRowT, nTPos, Replicate( Space(1), nSpaceLast ), ::cColorSpec )

      endif

      // right gap of spaces (nSpaceLast) on Footer
      if ! Empty( cfSep )
         DispOutAT( nScreenRowB, nBPos, Replicate( Right( cfSep, 1 ), nSpaceLast ), ::cColorSpec )

      elseif ::lColFootSep
         DispOutAT( nScreenRowB, nBPos, Replicate( Space(1), nSpaceLast ), ::cColorSpec )

      endif
   endif

   if ::lFooters                // Drawing footers
      // Clear area of screen occupied by footers
      //
      DispBox( ::nwBottom - ::nFooterHeight + 1, ::nwLeft, ::nwBottom, ::nwRight, cBlankBox, ::cColorSpec )

      for n := iif( ::nFrozenCols > 0, 1, ::nLeftVisible ) to ::nRightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::nLeftVisible
         endif
         setPos( ::nwBottom, ::aColsInfo[ n, COLINFO_COLPOS ] )

         ::WriteMLineText( ::aColsInfo[ n, COLINFO_FOOTING ], ;
                           ::aColsInfo[ n, COLINFO_WIDTH ], .F., ;
                           hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp(::aColsInfo[ n, COLINFO_OBJ ]:DefColor,::aColorSpec) , TBC_CLR_FOOTING) - 1 ) )
      next
   endif

   DispEnd()

Return Self

//-------------------------------------------------------------------//

METHOD RefreshPhysRow() CLASS TBrowse


  ::nPhysRowTop := Max(1, ::nPhysRow - ::nRowPos + 1  )

  ::nPhysRowBottom := ::nPhysRowTop + ::nRowCount - 1

  IF ::nPhysRowBottom > ::nLastPhysRow
     ::nPhysRowBottom := ::nLastPhysRow
  ENDIF


RETURN SELF

//-------------------------------------------------------------------//

METHOD SetRowPos( nNewRow )  CLASS TBrowse

LOCAL nCurColPos,nCurRowPos,i,nSkipRows

 IF nNewRow >= 1 .AND. nNewRow <= ::nRowCount

   nCurColPos := ::nColPos
   nCurRowPos := ::nRowPos

   IF nNewRow < nCurRowPos // bakcward

      nSkipRows := nCurRowPos - nNewRow

      FOR i := 1 to nSkipRows
          ::Up()
      NEXT

   ELSEIF nNewRow > nCurRowPos  // forward

      nSkipRows := nNewRow - nCurRowPos 

      FOR i := 1 to nSkipRows
          ::Down()
      NEXT

   ENDIF

 ENDIF

RETURN SELF

//---------------------------------------------------------------------//

METHOD ColorRect( aNewArea, aNewColor ) CLASS TBrowse

LOCAL nTop,nLeft,nBottom,nRight,lOK,nRect
 

 IF Valtype( aNewArea )="A" .AND. ValType( aNewColor )="A"

   IF Len( aNewArea ) = 4 .AND. Len( aNewColor ) = 2

      lOK := .F.

      // Assign the new aRect region
      nTop    := aNewArea[1]
      nLeft   := aNewArea[2]
      nBottom := aNewArea[3]
      nRight  := aNewArea[4]

      IF valtype(nTop)="N" .AND.  nTop >= 1 .AND. nTop <= ::nRowCount
         lOK := .T.
      ENDIF

      IF lOK
         IF valtype(nLeft)="N" .AND.  nLeft >= 1 .AND. nLeft <= ::nColCount
            lOK := .T.
         ELSE
            lOK := .F.
         ENDIF

         IF lOK
            IF valtype(nBottom)="N" .AND.  nBottom >= 1 .AND. nBottom <= ::nRowCount .AND. ( (nBottom-nTop) >= 0 )
               lOK := .T.
            ELSE
               lOK := .F.
            ENDIF

            IF lOK
               IF valtype(nRight)="N" .AND. nRight >= 1 .AND. nRight <= ::nColCount .AND. ( (nRight - nLeft) >= 0 )
                  lOK := .T.
               ELSE
                  lOK := .F.
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF lOK

         // add or update new colorect area to colorrect array
         nRect := ::CheckColorRect( aNewArea )
         IF nRect==0
            AAdd( ::aColorRect, { aNewArea, DefColorToDisp(aNewColor,::aColorSpec) } )
         ELSE
            ::UpdateColorRect( nRect, aNewArea, DefColorToDisp(aNewColor,::aColorSpec) )
         ENDIF

         ::lIsColorRect := .T.

         // Draw the new colorrect area
         ::lEnableHilite := .F.
         ::DrawColorRect( aNewArea, .T. )

      ENDIF

   ENDIF

 ENDIF

RETURN SELF

//-------------------------------------------------------------------//

METHOD FindColorRect( nRow, nCol ) CLASS TBrowse
// Return an array with all indexes where nRow and/or nCol are in.

LOCAL aRect,nRect,aRet

 aRet := {}

 IF !Empty( ::aColorRect )

   FOR nRect := 1 to Len( ::aColorRect )

       aRect := ::aColorRect[ nRect, 1 ]

       IF nCol == NIL

          IF nRow >= aRect[ COLORRECT_TOP ] .AND.;
             nRow <= aRect[ COLORRECT_BOTTOM ] 

             aadd( aRet, nRect )

          ENDIF

       ELSE

          IF nRow >= aRect[ COLORRECT_TOP ]    .AND.;
             nRow <= aRect[ COLORRECT_BOTTOM ] .AND.;
             nCol >= aRect[ COLORRECT_LEFT ]   .AND.;
             nCol <= aRect[ COLORRECT_RIGHT ]

             AAdd( aRet, nRect )

          ENDIF

       ENDIF

   NEXT

 ENDIF

RETURN ( aRet )

//-------------------------------------------------------------------//

METHOD DrawColorRect( uRect, lForceDraw ) CLASS TBrowse

LOCAL nRow,lRedraw,nRect

DEFAULT lForceDraw TO .F.

IF !Empty( ::aRowsToRedraw )  .AND. ::lIsColorRect

   lRedraw := .F.
   AFill( ::aRowsToRedraw, .F. )

   IF uRect = NIL  // draw all rows of all colorrect arrays

      FOR nRect := 1 TO Len( ::aColorRect )
          uRect := ::aColorRect[ nRect ,1 ]
          FOR nRow := uRect[ COLORRECT_TOP ] TO uRect[ COLORRECT_BOTTOM ]
             ::aRowsToRedraw[ nRow ] := .T.
             lRedraw := .T.
          NEXT
      NEXT

   ELSEIF ValType( uRect ) == "A"  // draw all rows into colorrect array informed

      FOR nRow := uRect[ COLORRECT_TOP ] TO uRect[ COLORRECT_BOTTOM ]
          ::aRowsToRedraw[ nRow ] := .T.
          lRedraw := .T.
      NEXT

   ELSEIF Valtype( uRect ) = "N" // draw a row 

          ::aRowsToRedraw[ uRect ] := .T.
          lRedraw := .T.

   ENDIF


   IF lRedraw .OR. lForceDraw
      ::DrawRow() // redraw all rows
   ENDIF


ENDIF

RETURN SELF

//-------------------------------------------------------------------//

METHOD RefreshColorRect() CLASS TBrowse

LOCAL nRect,aRect,nTop,nBottom,nRow


  nRow := ::nRowPos


  FOR nRect := 1 TO Len( ::aColorRect )

      aRect := AClone( ::aColorRect[ nRect, 1 ] )


      // If colorect has one row, so I need delete it.
      IF nRow = aRect[ COLORRECT_TOP ] .AND.;
         nRow = aRect[ COLORRECT_BOTTOM ]

         ::DeleteColorRect( nRect )
         nRect--

      ELSE  // ColorRect requirit adjusts

         // RowPos is first row in colorrect area,
         // so I need adjust it.
         IF nRow = aRect[ COLORRECT_TOP ] .AND.;
            nRow < aRect[ COLORRECT_BOTTOM ]

             nTop := aRect[ COLORRECT_TOP ] + 1

             IF nTop > ::nRowCount .OR. nTop > aRect[ COLORRECT_BOTTOM ]
                ::DeleteColorRect( nRect )
                nRect--

             ELSE
                aRect[ COLORRECT_TOP ] := Min( aRect[ COLORRECT_BOTTOM ], nTop )
                ::UpdateColorRect( nRect, aRect )

             ENDIF

         // RowPos is last row in colorrect area,
         // so I need adjust it.
         ELSEIF nRow = aRect[ COLORRECT_BOTTOM ] .AND.; 
                nRow > aRect[ COLORRECT_TOP ]

             nBottom := aRect[ COLORRECT_BOTTOM ] - 1

             IF nBottom < 1 .OR. nBottom < aRect[ COLORRECT_TOP ]
                ::DeleteColorRect( nRect )
                nRect--

             ELSE
                aRect[ COLORRECT_BOTTOM ] := Max( aRect[ COLORRECT_TOP ], nBottom )
                ::UpdateColorRect( nRect, aRect )

             ENDIF


         // RowPos is inside of a colorrect area,
         // so I need split it.
         ELSEIF nRow > aRect[ COLORRECT_TOP ] .AND.;
                nRow < aRect[ COLORRECT_BOTTOM ]

                ::SplitColorRect( nRow, nRect )
                
         ENDIF

         ::DrawRow( nRow )

      ENDIF

      aRect := NIL

  NEXT

RETURN Self

//-------------------------------------------------------------------//

METHOD ResetColorRect() CLASS TBrowse

LOCAL n,nRect,aRect,nRowsMoved,nNewTop,nNewBottom,nOldTop,nOldBottom


    nRowsMoved := 0


    IF ::nWhatMoved = TBR_DOWN_MOVED 


      IF ::nRowPos = ::nRowCount .AND.;
         ::nPhysRow > ::nRowCount .AND.;
         ::nPrevPhysRowTop != ::nPhysRowTop 


         FOR nRect := 1 to Len( ::aColorRect )

             aRect := ::aColorRect[ nRect ,1 ]

             nNewTop    := aRect[ COLORRECT_TOP ] - 1
             nNewBottom := aRect[ COLORRECT_BOTTOM ] - 1

             IF nNewTop < 1 .AND. nNewBottom < 1
                ::DeleteColorRect( nRect )
                nRect--
             ELSE
                aRect[ COLORRECT_TOP ]    := Max(1,nNewTop)
                aRect[ COLORRECT_BOTTOM ] := Max(1,nNewBottom) 
                ::UpdateColorRect( nRect, aRect )
             ENDIF

         NEXT

         IF !::lDataSource
            ::DrawColorRect(,.T.)
         ELSE
            ::DrawColorRect(::nRowPos,.T.)
         ENDIF

         IF nNewBottom < ::nRowCount .AND. ::nRowPos = ::nRowCount
            ::DrawRow( ::nRowPos )
            ::Hilite()
         ENDIF

      ENDIF



    ELSEIF ::nWhatMoved = TBR_UP_MOVED 



      IF ::nRowPos = 1 .AND.;
         ::nPhysRow = ::nPhysRowTop .AND.;
         ::nPrevPhysRowTop != ::nPhysRowTop

         FOR nRect := 1 TO Len( ::aColorRect )

             aRect := ::aColorRect[ nRect, 1]

             nNewTop    := aRect[ COLORRECT_TOP ] + 1
             nNewBottom := aRect[ COLORRECT_BOTTOM ] + 1

             IF nNewTop > ::nRowCount  
                ::DeleteColorRect( nRect )
                nRect--
             ELSE 
                aRect[ COLORRECT_TOP ]    := Min(::nRowCount,nNewTop)
                aRect[ COLORRECT_BOTTOM ] := Min(::nRowCount,nNewBottom) 
                ::UpdateColorRect( nRect, aRect )
             ENDIF


         NEXT

         ::DrawColorRect(,.T.)
         IF nNewTop > 1 .AND. ::nRowPos=1
            ::DrawRow( ::nRowPos )
            ::Hilite()
         ENDIF

      ENDIF



    ELSEIF ::nWhatMoved = TBR_PGDOWN_MOVED .OR.;
           ::nWhatMoved = TBR_BOTTOM_MOVED


      nRowsMoved := ( ::nPhysRowTop - ::nPrevPhysRowTop )


      IF nRowsMoved > 0 .AND. nRowsMoved < ::nRowCount


         FOR nRect := 1 TO Len( ::aColorRect )

             aRect := ::aColorRect[ nRect, 1 ]

             nOldTop    := aRect[ COLORRECT_TOP ]
             nOldBottom := aRect[ COLORRECT_BOTTOM ]

             nNewTop    := aRect[ COLORRECT_TOP ] - nRowsMoved
             nNewBottom := aRect[ COLORRECT_BOTTOM ] - nRowsMoved

             IF nNewTop < 1 .AND. nNewBottom < 1
                ::DeleteColorRect( nRect )
                nRect--

             ELSE

                aRect[ COLORRECT_TOP ]    := Max(1,nNewTop)
                aRect[ COLORRECT_BOTTOM ] := Max(1,nNewBottom)
                ::UpdateColorRect( nRect , aRect )
             ENDIF

             FOR n :=  nOldTop TO nOldBottom
                ::DrawRow( n )
             NEXT

         NEXT

         ::DrawColorRect()

         IF ::nRowPos = ::nRowCount .AND. nNewBottom < ::nRowCount
            ::DrawRow( ::nRowPos )
            ::Hilite()
         ENDIF


      ELSEIF nRowsMoved >= ::nRowCount

         ::DeleteColorRect()
         AFill( ::aRowsToRedraw, .T. )
         ::DrawRow()

      ENDIF



    ELSEIF ::nWhatMoved = TBR_PGUP_MOVED .OR.;
           ::nWhatMoved = TBR_TOP_MOVED



      nRowsMoved := ( ::nPrevPhysRowTop - ::nPhysRowTop )

      IF nRowsMoved > 0


         FOR nRect := 1 TO Len( ::aColorRect )

             aRect := ::aColorRect[ nRect, 1 ]

             nOldTop    := aRect[ COLORRECT_TOP ]
             nOldBottom := aRect[ COLORRECT_BOTTOM ]

             nNewTop    := aRect[ COLORRECT_TOP ] + nRowsMoved
             nNewBottom := aRect[ COLORRECT_BOTTOM ] + nRowsMoved

             IF nNewTop > ::nRowCount 
                ::DeleteColorRect( nRect )
                nRect--
             ELSE
                aRect[ COLORRECT_TOP ]    := Min(nNewTop,::nRowCount)
                aRect[ COLORRECT_BOTTOM ] := Min(nNewBottom,::nRowCount)
                ::UpdateColorRect( nRect , aRect )
             ENDIF

             FOR n :=  nOldTop TO nOldBottom
               ::DrawRow( n )
             NEXT

         NEXT

         ::DrawColorRect(,.T.)
         IF ::nRowPos = 1 .AND. nNewTop > 1
            ::DrawRow( ::nRowPos )
            ::Hilite()
         ENDIF

      ENDIF


    ENDIF

    ::nPrevPhysRowTop := ::nPhysRowTop
    ::nWhatMoved := 0


RETURN Self

//-------------------------------------------------------------------//

METHOD DeleteColorRect( nRect ) CLASS TBrowse

  IF valtype(nRect)=="N"

     IF Len( ::aColorRect ) >= nRect
        ADel( ::aColorRect, nRect, .T. )
        ::lIsColorRect := !Empty( ::aColorRect )
     ENDIF

  ELSE

     ::aColorRect := {}
     ::lIsColorRect := .F.

  ENDIF

RETURN Self

//-------------------------------------------------------------------//

METHOD IsCellInColorRect( nRow, nCol ) CLASS TBrowse

LOCAL aRect,aIndex,lReturn,nIndex

   lReturn := .F.


   IF ::lIsColorRect

      aIndex := ::FindColorRect( nRow )

      IF !Empty( aIndex )

         FOR nIndex := 1 TO Len( aIndex )

             aRect := ::aColorRect[ nIndex , 1 ]

             IF nCol >= aRect[ COLORRECT_LEFT ] .AND.;
                nCol <= aRect[ COLORRECT_RIGHT ] 

                lReturn := .T.

             ENDIF

         NEXT

      ENDIF

   ENDIF


RETURN ( lReturn )

//-------------------------------------------------------------------//

METHOD SplitColorRect( nRowPos, nRect ) CLASS TBrowse
Local nRow,aRect,aSplit1,aSplit2,aColor


   aRect  := AClone( ::aColorRect[ nRect,1 ] )
   aColor := AClone( ::aColorRect[ nRect,2 ] )

   aSplit1 := AClone( aRect )
   aSplit2 := AClone( aRect )


   FOR nRow := aRect[ COLORRECT_TOP ]  TO  aRect[ COLORRECT_BOTTOM ]

       IF nRow >= aRect[ COLORRECT_TOP ] .AND. nRow < nRowPos 

          aSplit1[ COLORRECT_TOP ]    := aRect[ COLORRECT_TOP ]
          aSplit1[ COLORRECT_BOTTOM ] := nRow

       ELSEIF nRow > nRowPos .AND. nRow <= aRect[ COLORRECT_BOTTOM ]

          aSplit2[ COLORRECT_TOP ]    := nRowPos+1
          aSplit2[ COLORRECT_BOTTOM ] := nRow

       ENDIF

   NEXT

   ::aColorRect[ nRect,1 ] := aSplit1
   AAdd(::aColorRect,{} ) // this is necessary before AIns(), dont't remove.
   AIns(::aColorRect, nRect+1 )
   ::aColorRect[ nRect+1 ] := { aSplit2, aColor }


RETURN Self

//-------------------------------------------------------------------//

METHOD UpdateColorRect( nIndex, aRect, aColor ) CLASS TBrowse
LOCAL nRow

   ::aColorRect[ nIndex,1 ] := aRect

   IF valtype(aColor)=="A"
     ::aColorRect[ nIndex,2 ] := aColor
   ENDIF

RETURN SELF

//-------------------------------------------------------------------//

METHOD CheckColorRect( aNewRect ) CLASS TBrowse

LOCAL nIndex,nReturn
LOCAL aRect
LOCAL nTop,nLeft,nBottom,nRight

 nReturn := 0

 IF !Empty( ::aColorRect )

   nTop    := aNewRect[ COLORRECT_TOP ]
   nLeft   := aNewRect[ COLORRECT_LEFT ]
   nBottom := aNewRect[ COLORRECT_BOTTOM ]
   nRight  := aNewRect[ COLORRECT_RIGHT ]

   FOR nIndex := 1 to Len( ::aColorRect )

       aRect := ::aColorRect[ nIndex, 1 ]

       IF nTop    == aRect[ COLORRECT_TOP ]    .AND.;
          nLeft   == aRect[ COLORRECT_LEFT ]   .AND.;
          nBottom == aRect[ COLORRECT_BOTTOM ] .AND.;
          nRight  == aRect[ COLORRECT_RIGHT ]

          nReturn := nIndex

          EXIT

       ENDIF

   NEXT

 ENDIF

RETURN ( nReturn )

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

   If ::nRecsToSkip == 0  // Ensure that you do not reset the painting
                          // of a movement caused by TBrowse methods

      If ::nRowPos # 1     // No repositioning is required if current
                          // cursor row is one

         nAvail := ::EvalSkipBlock( 0 - ::nRowPos - 1 )

         // You should reposition only if there are too few records
         // available or there are more than sufficient records
         // available.  If there are exact number of records leave it.
         //
         lReset := Abs( nAvail ) + 1 # ::nRowPos

         // Place back the data source pointer to where it was
         //
         ::EvalSkipBlock( 0 - nAvail )

      EndIf
   EndIf

   // TraceLog( 'Browser: RowPos, nAvail, lReset, RecsToSkip', ::nRowPos, nAvail, lReset, ::nRecsToSkip )

   If lReset   // So repositioning was required !
      // nNewRowPos and nLastRetrieved have to be updated
      // as we will entering phase 2 of stabilization
      //
      ::nRowPos := ::nNewRowPos := ::nLastRetrieved := ;
                  If( Abs( nAvail ) + 1 > ::nRowPos, ::nRowPos, Abs( nAvail ) + 1 )
      ::Moved()

      // To ensure phase 1 is skipped
      ::nRecsToSkip := 0

      ::RefreshAll()
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

   LOCAL nOldCursor   // Current shape of cursor (which I remove before stabilization)
   LOCAL colorSpec
   LOCAL nRowToDraw

   if ::nColCount == 0
      // Return TRUE to avoid infinite loop ( do while !stabilize;end )
      return .t.
   endif

   /* First, since ::nColPos can go "out of bounds" we need
      to put 1 <= ::nColpos <= ::nColCount
      And we need to do this before calling ::Configure() which
      needs a ::nColPos "inside bounds"
   */
   ::nColPos := Max( Min( ::nColPos, ::nColCount ), 1)

   // Configure the browse if not configured . Pritpal Bedi
   //
   if ! ::lConfigured .or. ::lNeverDisplayed
      if ::lNeverDisplayed
         ::configure( 3 )
      endif
      ::configure( 2 )
   endif

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

   // From this point there is stabilization of rows which is made up of three phases
   // 1st repositioning of data source
   // 2nd redrawing of rows, after each row we exit stabilization loop with .F.
   // 3rd if all rows have been redrawn we set ::lStable state to .T.
   //
   if ! ::lStable
      // NOTE: I can enter here because of a movement key or a ::RefreshAll():ForceStable() call

      if ::CheckRowsToBeRedrawn()
         // Exit first stage of stabilization
         //
         SetCursor( nOldCursor )
         if ! lForceStable
            return .F.
         endif

      endif

      if ! lForceStable
         //  Draw browse row-by-row
         //
         if ( nRowToDraw := AScan( ::aRowsToRedraw, .t. ) ) <> 0
            ::DrawRow( @nRowToDraw )

            SetCursor( nOldCursor )
            return .F.
         endif
      else
         // Draw all rows
         ::DrawRow()
      endif

      // If I reach this point I've repainted all rows so I can set ::lStable state
      //
      // If I have fewer records than available TBrowse rows, cursor cannot be lower than
      // last record (note ::lIntHitBottom is set only during a movement)
      //
      if ::nLastRetrieved < ::nNewRowPos
         ::nNewRowPos := ::nLastRetrieved
      endif

      // If I'm not already under cursor I have to set data source to cursor position
      //
      if ::nLastRetrieved <> ::nNewRowPos
         ::EvalSkipBlock( ::nNewRowPos - ::nLastRetrieved )
         ::nLastRetrieved := ::nNewRowPos
      endif

      // new cursor position
      //
      ::nRowPos     := ::nNewRowPos
      ::lHitTop     := ::lIntHitTop
      ::lHitBottom  := ::lIntHitBottom
      ::lStable := .T.

   endif

   // NOTE: DBU relies upon current cell being reHilited() even if already stable
   //
   if ::lAutoLite
      IF ::lEnableHilite
         ::Hilite()
      ELSE
         ::lEnableHilite := .T.
      ENDIF
   else
      ::PosCursor()
   endif

   SetCursor( nOldCursor )

   /* 2005/Sep/15 - Eduardo Fernandes
      After stabilize I need refresh physical rows and colorrect areas.
   */
   IF ::lStable

      // Reread datasource to verify if a new record was added
      IF ::lDataSource
         IF ::nLastPhysRow < &(::cAlias)->( LastRec() ) .AND. ::nRowPos = ::nRowCount
            ::nPhysRow++
            ::nLastPhysRow := &(::cAlias)->( LastRec() )
         ENDIF
      ELSE
         IF ::nLastPhysRow < ::nRowCount .AND. ::nRowPos = ::nRowCount
            ::nPhysRow++
            ::nLastPhysRow := ::nRowCount
         ENDIF
      ENDIF

      ::RefreshPhysRow()

      IF ::lIsColorRect  .AND. (!::lHitTop .AND. !::lHitBottom)
         ::ResetColorRect()
      ENDIF

   ENDIF

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
      if ::nLastRetrieved <> ::nNewRowPos
         ::EvalSkipBlock( ::nNewRowPos - ::nLastRetrieved )
         ::nLastRetrieved := ::nNewRowPos
      endif

      nRecsSkipped := ::EvalSkipBlock( ::nRecsToSkip )

      // I've tried to move past top or bottom margin
      //
      if nRecsSkipped == 0
         if ::nRecsToSkip > 0
            ::lIntHitBottom := .T.

         elseif ::nRecsToSkip < 0
            ::lIntHitTop := .T.

         endif

      elseif nRecsSkipped == ::nRecsToSkip
         // If after movement I'm still inside present TBrowse
         //
         if ( ::nNewRowPos + nRecsSkipped >= 1 ) .AND. ( ::nNewRowPos + nRecsSkipped <= ::nRowCount )
            ::nNewRowPos     += nRecsSkipped
            ::nLastRetrieved := ::nNewRowPos

            // This is needed since present TBrowse has no cache, so I need to repaint current row
            // rereading it from data source and to force rereading from data source I have to mark
            // row as invalid
            //
            ::aRowsToRedraw[ ::nNewRowPos ] := .T.

         else
            // It was K_PGDN or K_PGUP or K_UP or K_DN
            //
            if Abs( nRecsSkipped ) >= ::nRowCount
               // K_PGDN
               //
               if nRecsSkipped > 0
                  ::nLastRetrieved := ::nRowCount

               else // K_PGUP
                  ::nLastRetrieved := 1

               endif
               ::RefreshAll()

            else
               // K_DN or K_UP
               // Where does really start first TBrowse row?
               //
               nFirstRow := ::nBeginRowData + 1

               // I'm at top or bottom of TBrowse so I can scroll
               //
               if ::nNewRowPos == ::nRowCount
                  ScrollFixed( nFirstRow + nRecsSkipped - 1, ::nwLeft, nFirstRow + ::nRowCount - 1, ::nwRight, nRecsSkipped )
                  ::nLastRetrieved := ::nRowCount

               else
                  ScrollFixed( nFirstRow, ::nwLeft, nFirstRow + ::nRowCount + nRecsSkipped, ::nwRight, nRecsSkipped )
                  ::nLastRetrieved := 1

               endif

               // I've scrolled on screen rows, now I need to scroll ::aRowsToRedraw array as well!
               //
               if nRecsSkipped > 0
                  ADel( ::aRowsToRedraw, 1 )
                  ::aRowsToRedraw[ -1 ] := .F.

               else
                  ADel( ::aRowsToRedraw, ::nRowCount )
                  AIns( ::aRowsToRedraw, 1, .F. )

               endif

               ::aRowsToRedraw[ ::nNewRowPos ] := .T.
            endif
         endif

      else
         // I couldn't move as far as requested
         // I need to refresh all rows if I go past current top or bottom row
         //
         if ( ::nNewRowPos + nRecsSkipped < 1 ) .OR. ( ::nNewRowPos + nRecsSkipped > ::nRowCount )
            // don't go past boundaries
            //
            ::nNewRowPos := iif( nRecsSkipped > 0, ::nRowCount, 1 )
            ::RefreshAll()

         else
            ::nNewRowPos += nRecsSkipped
            ::aRowsToRedraw[ ::nNewRowPos ] := .T.

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

METHOD DrawRow( nRowToDraw ) CLASS TBrowse

   LOCAL ColorSpec, cColor
   LOCAL nColFrom
   LOCAL lDisplay
   LOCAL nCol, nRow2Fill, nLeftColPos
   LOCAL nRowsToSkip, nSkipped
   LOCAL lDrawAllRows
   LOCAL cColBlanks  // Enough Space()s to fill a column
   LOCAL oCol

   ColorSpec := ::aColorSpec[ 1 ]

   nColFrom := iif( ::nFrozenCols > 0, 1, ::nLeftVisible )

   // We paint columns wise, so we need to keep track of the screen
   // column where current tbrowse column starts.

   nLeftColPos := ::nwLeft


   // If nRowToDraw is ommited or equal to 0, redraws all pending rows
   if Valtype( nRowToDraw ) == "N" .AND. nRowToDraw > 0 
      lDrawAllRows := .F.
   else
      lDrawAllRows := .T.
      // Check for first row to be refreshed
      nRowToDraw := AScan( ::aRowsToRedraw, .t. )
   endif

   // Data source is already at correct record number, now we need
   // to repaint browser accordingly.
   //
   DispBegin()

   do while nRowToDraw >= 1 .and. nRowToDraw <= ::nRowCount

      if nRowToDraw <> ::nLastRetrieved

         nRowsToSkip := nRowToDraw - ::nLastRetrieved
         nSkipped := ::EvalSkipBlock( nRowsToSkip )

         if nSkipped != nRowsToSkip

            // There're less rows in the screen
            if nRowsToSkip < 0   // Going up... browse must be in top row
               ::nRowPos := MIN( MAX( ::nRowPos + nRowsToSkip - nSkipped, 1 ), ::nRowCount )
               ::nNewRowPos := MIN( MAX( ::nNewRowPos + nRowsToSkip - nSkipped, 1 ), ::nRowCount )
               ::nLastRetrieved := 1
               nRowToDraw := 1
               lDisplay := .T.
               ::refreshAll()
            else                 // Going down...
               ::nLastRetrieved += nSkipped
               nRowToDraw := ::nLastRetrieved + 1
               ::nRowPos := MIN( ::nRowPos, ::nLastRetrieved )
               ::nNewRowPos := MIN( MAX( ::nNewRowPos, 1 ), ::nRowCount )
               lDisplay := .F.
            endif
         else
            ::nLastRetrieved += nSkipped
            lDisplay := .T.
         endif
      else
         lDisplay := .T.
      endif

      if lDisplay

         if ::nFrozenCols == 0

            DispOutAt( nRowToDraw + ::nBeginRowData, nLeftColPos, ::cSpacePre, ColorSpec )

            for nCol := nColFrom to ::nRightVisible

                ::DispCell( nRowToDraw, nCol, TBC_CLR_STANDARD )

                if nCol < ::nRightVisible .and. ::aColsInfo[ nCol, COLINFO_DRAW_COLSEP ]
                   DispOut( ::aColsInfo[ nCol + 1, COLINFO_COLSEP ], ColorSpec )
                endif

            next

         else      //  ::nFrozenCols > 0

            SetPos( nRowToDraw + ::nBeginRowData, nLeftColPos )

            for nCol := nColFrom to ::nRightVisible

               if nCol == ::nFrozenCols + 1

                  nCol := ::nLeftVisible
                  DispOut( ::cSpacePre, ColorSpec )

               endif

               ::DispCell( nRowToDraw, nCol, TBC_CLR_STANDARD )

               if nCol < ::nRightVisible .and. ::aColsInfo[ nCol,COLINFO_DRAW_COLSEP ]
                  DispOut( ::aColsInfo[ nCol + 1, COLINFO_COLSEP ], ColorSpec )
               endif

            next

         endif

         DispOut( ::cSpaceLast, ColorSpec )

         // doesn't need to be redrawn
         ::aRowsToRedraw[ nRowToDraw ] := .f.

         if lDrawAllRows
            // Check next row to be refreshed
            nRowToDraw := AScan( ::aRowsToRedraw, .t. )
         else
            nRowToDraw := 0
         endif

      else  // ! lDisplay

         /* 09/08/2004 - <maurilio.longo@libero.it>
            Here we fill the space from last row which has data up to ::nRowCount
            so it is faster to work column wise instead of row wise since this
            saves us a lot of ::colorBlock evaluations to find column color and
            doesn't cost us nothing since we're simply writing spaces without
            calls to oCol:Block.
         */


         for nCol := nColFrom to ::nRightVisible

            oCol := ::aColsInfo[ nCol, COLINFO_OBJ ]

            // needed here to calc correct column color
            if ::nFrozenCols > 0 .AND. nCol == ::nFrozenCols + 1
               nCol := ::nLeftVisible
            endif

            cColBlanks := Space( ::aColsInfo[ nCol, COLINFO_WIDTH ] )

            // Let's find column color once per column
            if ::aColsInfo[ nCol, COLINFO_OBJ ]:ColorBlock == NIL
               cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp( oCol:DefColor,::aColorSpec), TBC_CLR_STANDARD ) - 1 )
            else

               // if number of rows are smaller than bottom, paint cells
               // with defcolor instead colorblock. 2005/Aug/21 - Eduardo Fernandes
               IF ::nRowCount < ::nBottom
                  cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp( oCol:DefColor,::aColorSpec), TBC_CLR_STANDARD ) - 1 )
               ELSE
                  cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp( Eval( oCol:ColorBlock, Eval( oCol:Block )) , ::aColorSpec)  , TBC_CLR_STANDARD )  - 1 )
               ENDIF

            endif


            // Paint all remainig rows up to ::nRowCount

            for nRow2Fill := nRowToDraw to ::nRowCount

               if ::nFrozenCols == 0

                  if nCol == nColFrom
                     DispOutAt( nRow2Fill + ::nBeginRowData, nLeftColPos, ::cSpacePre, ColorSpec )
                  else
                     SetPos(nRow2Fill + ::nBeginRowData, nLeftColPos)
                  endif

                  DispOut( cColBlanks, cColor )

                  if nCol < ::nRightVisible .and. ::aColsInfo[ nCol,COLINFO_DRAW_COLSEP ]
                     DispOut( ::aColsInfo[ nCol + 1, COLINFO_COLSEP ], ColorSpec )
                  endif

               else

                  if nCol == ::nLeftVisible
                     DispOutAt( nRow2Fill + ::nBeginRowData, nLeftColPos, ::cSpacePre, ColorSpec )
                  else
                     setPos( nRow2Fill + ::nBeginRowData, nLeftColPos )
                  endif

                  DispOut( cColBlanks, cColor )

                  if nCol < ::nRightVisible .and. ::aColsInfo[ nCol,COLINFO_DRAW_COLSEP ]
                     DispOut( ::aColsInfo[ nCol + 1, COLINFO_COLSEP ], ColorSpec )
                  endif
               endif

               if nCol == ::nRightVisible
                  DispOut( ::cSpaceLast, ColorSpec )
               endif

            next

            // next tbrowse column starts from this screen column
            nLeftColPos := Col()

         next

         // Mark all remaining rows as drawn
         AFill(::aRowsToRedraw, .F., nRowToDraw)
         exit

      endif // lDisplay

   enddo

   DispEnd()


Return SELF

//-------------------------------------------------------------------//
//
//                              Display
//
//-------------------------------------------------------------------//

/* 10/08/2004 - <maurilio.longo@libero.it>
                Clipper 5.2e TBrowse does not hilite a cell which is not already
                displayed on screen: (1)

                oBrowse:colpos := ::nRightVisible + 1
                oBrowse:hilite():forceStable()

                shows next column to the right without cells hilited.
                So, I treat PosCursor(), Hilite() and DeHilite() the same way and
                make them skip their job when ::colPos is not already on screen

                (1) Clipper even has a bug that causes ::hilite() to shows a cell
                    outside of tbrowse area!
*/

//-------------------------------------------------------------------//

METHOD PosCursor() CLASS TBrowse

   LOCAL nRow := ::nRowPos + ::nBeginRowData
   LOCAL nCol

   /* Here I simply take care of being inside available columns,
      but I think there should be a more complex test to see if we are
      inside displayed tbrowse columns
   */
   if ::nColPos > 0 .AND. ::nColPos <= ::nColCount

      nCol := ::aColsInfo[ ::nColPos, COLINFO_COLPOS ]

      Switch ::aColsInfo[ ::nColPos, COLINFO_TYPE ]
      case "N"
         if ::aColsInfo[ ::nColPos, COLINFO_OBJ ]:Width == NIL
            nCol += ::aColsInfo[ ::nColPos, COLINFO_WIDTH ] - ::aColsInfo[ ::nColPos, COLINFO_CELLWIDTH ]
         endif
         exit

      case "L"
         // Always centered inside column
         nCol += Round( ( ::aColsInfo[ ::nColPos, COLINFO_WIDTH ] - ::aColsInfo[ ::nColPos, COLINFO_CELLWIDTH ] ) / 2, 0 )
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

LOCAL nCursorRow := ::nRowPos + ::nBeginRowData
LOCAL nCursorCol
LOCAL cScr, cColor, i, cCell, oCol
LOCAL nBlanks,cColorBKG


   if ::nColPos > 0 .AND. ::nColPos <= ::nColCount

      dispbegin()

      nCursorCol := ::aColsInfo[ ::nColPos, COLINFO_COLPOS ]

      SetPos( nCursorRow, nCursorCol )

      oCol := ::aColsInfo[ ::nColPos, COLINFO_OBJ ]


      if oCol:ColorBlock == NIL
         cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp(oCol:DefColor,::aColorSpec), TBC_CLR_STANDARD ) - 1 )
      else
         cColor := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp( Eval( oCol:ColorBlock, Eval(oCol:Block)),::aColorSpec), TBC_CLR_STANDARD ) - 1 )
      endif



      // Get highlighted cell area from screen
      cCell := SaveScreen(nCursorRow, nCursorCol, nCursorRow, nCursorCol + ::aColsInfo[ ::nColPos, COLINFO_WIDTH ] - 1)


      // Write first char with dehighlighted attribute
      DispOut( cCell[1], cColor )


      // Get dehighlighted attribute from screen
      cScr := SaveScreen(nCursorRow, nCursorCol, nCursorRow, nCursorCol + 1)

      // Replace highlighted attribute with dehighlighted inside string
      // representing highlighted cell
      for i := 2 to len(cCell) step 2
          cCell[i] := cScr[2]
      next


      // Write back cell, now dehighlighted.
      RestScreen(nCursorRow, nCursorCol, nCursorRow, nCursorCol + ::aColsInfo[ ::nColPos, COLINFO_WIDTH ] - 1, cCell)

      // Adjust highlighted area to cell width, instead column width.
      nBlanks := ( ::aColsInfo[ ::nColPos, COLINFO_WIDTH ] - ::aColsInfo[ ::nColPos, COLINFO_CELLWIDTH ] )
      IF nBlanks > 0
         cColorBKG := ::aColorSpec[ TBC_CLR_STANDARD ]
         IF ::aColsInfo[ ::nColPos, COLINFO_TYPE ]=="C" // strings are right aligned
            SetPos( nCursorRow, nCursorCol + ::aColsInfo[ ::nColPos, COLINFO_CELLWIDTH ]  )
            DispOut( space(nBlanks), cColorBKG )
         ELSEIF ::aColsInfo[ ::nColPos, COLINFO_TYPE ]=="N" // numbers are left aligned
            SetPos( nCursorRow, nCursorCol )
            DispOut( space(nBlanks), cColorBKG )
         ENDIF
      ENDIF

      SetPos( nCursorRow, nCursorCol )

      dispend()

   endif

Return Self

//-------------------------------------------------------------------//

METHOD Hilite() CLASS TBrowse

   LOCAL nRow := ::nRowPos + ::nBeginRowData
   LOCAL nCol
   LOCAL nNotLeftCol    // Screen col position of first char of not left justified columns

   if ::nColPos > 0 .AND. ::nColPos <= ::nColCount

      nCol := ::aColsInfo[ ::nColPos, COLINFO_COLPOS ]

      SetPos( nRow, nCol )

      nNotLeftCol := ::DispCell( ::nRowPos, ::nColPos, TBC_CLR_ENHANCED )

      SetPos( nRow, iif( nNotLeftCol <> NIL, nNotLeftCol, nCol ) )

      #ifdef HB_COMPAT_C53
        ::nRow := nRow
        ::nCol := iif( nNotLeftCol <> NIL, nNotLeftCol, nCol )
      #endif

   endif

Return Self

//-------------------------------------------------------------------//

METHOD DispCell( nRow, nCol, nColor ) CLASS TBrowse

   LOCAL aColsInfo  := ::aColsInfo[ nCol ]
   LOCAL oCol       := aColsInfo[ COLINFO_OBJ ]
   LOCAL nColWidth  := aColsInfo[ COLINFO_WIDTH ]
   LOCAL nCellWidth := aColsInfo[ COLINFO_CELLWIDTH ]
   LOCAL uCellValue := Eval( oCol:block )
   LOCAL cStdColor  := ::aColorSpec[ TBC_CLR_STANDARD ]

   // Screen col position of first char for not left justified columns
   LOCAL nNotLeftCol
   LOCAL cColor, cColorBKG
   LOCAL aColorRectColor
   LOCAL aIndex,aColorBlock
   LOCAL lUseColorBlock,lUseDefColor

   // if called when the column type is not defined, then do nothing
   if Empty( aColsInfo[ COLINFO_TYPE ] )
      Return nil
   endif


   lUseColorBlock := .F.
   lUseDefColor   := !lUseColorBlock

   IF ::lIsColorRect .AND. ::IsCellInColorRect( nRow,nCol )

      // should return at least one array with one element
      aIndex := ::FindColorRect( nRow, nCol )

      IF Len( aIndex ) = 1

         lUseColorBlock := .F.
         lUseDefColor   := .F.

         // only the first color array is relevant here
         aColorRectColor := ::aColorRect[ aIndex[1], 2 ]

         cColor    := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp(aColorRectColor,::aColorSpec), nColor ) - 1 )
         cColorBKG := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp(oCol:DefColor,::aColorSpec), TBC_CLR_STANDARD ) - 1 )

      ELSE

         lUseColorBlock := (oCol:ColorBlock != NIL)
         lUseDefColor   := !lUseColorBlock

      ENDIF

   ELSE

      lUseColorBlock := ( oCol:ColorBlock != NIL )
      lUseDefColor   := !lUseColorBlock

   ENDIF


   IF lUseColorBlock

      aColorBlock := Eval( oCol:ColorBlock, uCellValue )

      // If colorblock is empty, clipper uses the defcolor, if any.
      IF Empty( aColorBlock )
         aColorBlock := DefColorToDisp(oCol:DefColor,::aColorSpec)
      ELSE
         aColorBlock := DefColorToDisp(aColorBlock,::aColorSpec)
      ENDIF

      cColor    := hb_ColorIndex( ::cColorSpec, ColorToDisp( aColorBlock, nColor ) - 1 )
      cColorBKG := hb_ColorIndex( ::cColorSpec, ColorToDisp( aColorBlock, TBC_CLR_STANDARD ) - 1 )

   ELSEIF lUseDefColor

      cColor    := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp(oCol:DefColor,::aColorSpec), nColor ) - 1 )
      cColorBKG := hb_ColorIndex( ::cColorSpec, ColorToDisp( DefColorToDisp(oCol:DefColor,::aColorSpec), TBC_CLR_STANDARD ) - 1 )

   ENDIF


   Switch aColsInfo[ COLINFO_TYPE ]

   case "C"
      // This is necessary to display only cell width, instead column width.
      DispOut( PadR( Transform( uCellValue, oCol:Picture ), nCellWidth ), cColor )
      IF nCellWidth < nColWidth
         DispOut( Space( nColWidth - nCellWidth ), cStdColor )
      ENDIF
      exit

   case "M"
      // If there is not an explicit width use that of the first item
      if oCol:Width == NIL
         DispOut( PadR( Transform( uCellValue, oCol:Picture ), nCellWidth ), cColor )
         DispOut( Space( nColWidth - nCellWidth ), cColorBKG )
      else
         DispOut( PadR( Transform( uCellValue, oCol:Picture ), nColWidth ), cColor )
      endif

      exit

   case "N"
      if oCol:Width == NIL
//         DispOut( Space( nColWidth - nCellWidth ), cColorBKG )
         DispOut( Space( nColWidth - nCellWidth ), cStdColor )
         nNotLeftCol := Col()
         DispOut( PadL( Transform( uCellValue, oCol:Picture ), nCellWidth ), cColor )
      else
         DispOut( PadL( Transform( uCellValue, oCol:Picture ), nColWidth ), cColor )
      endif

      exit

   case "D"
      DispOut( PadR( Transform( uCellValue, oCol:Picture ), nCellWidth ), cColor )
      DispOut( Space( nColWidth - nCellWidth ), cColorBKG )
      exit

   case "L"
      // Always centered inside column
      DispOut( Space( Round( ( nColWidth - nCellWidth ) / 2, 0 ) ), cColorBKG )
      nNotLeftCol := Col()
      DispOut( iif( uCellValue, "T", "F" ), cColor )
      DispOut( Space( Int( ( nColWidth - nCellWidth ) / 2 ) ), cColorBKG )
      exit

   default
      DispOut( Space( nColWidth ), cColor )

   end


Return nNotLeftCol

//-------------------------------------------------------------------//

METHOD RefreshCurrent() CLASS TBrowse

  
   IF ::lIsColorRect
      ::RefreshColorRect()
   ENDIF

   ::RefreshRow()

Return Self

//-------------------------------------------------------------------//

METHOD RefreshRow() CLASS TBrowse

  IF !Empty( ::aRowsToRedraw )
     ::aRowsToRedraw[ ::nRowPos ] := .T.
  ENDIF

  ::lStable := .F.

RETURN Self

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
      if ! ::lStable
         ::Stabilize()

      else
         ::Moved()

      endif

      // Set new row position
      // nNewRow := nRow - ::nwTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ), 0, 1 ) - 1
      nNewRow       := nRow - ::nBeginRowData
      ::nRecsToSkip := nNewRow - ::nNewRowPos

      // move data source accordingly
      //
      ::Stabilize()

      // Now move to column under nCol
      //
      nColsLen := 0

      // NOTE: I don't think it is correct, have to look up docs
      //
      nI := iif( ::nFrozenCols > 0, ::nFrozenCols, ::nLeftVisible )

      while nColsLen < nCol .AND. nI < ::nRightVisible

         nColsLen += ::aColsInfo[ nI, COLINFO_WIDTH ]
         if nI >= 1 .AND. nI < ::nColCount
            nColsLen += ::aColsInfo[ nI+1, COLINFO_SEPWIDTH ]
         endif

         nI++

      enddo

      ::nColPos := nI

      // Force redraw of current row with new cell position
      //
      ::RefreshRow()

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

METHOD SetBorder( cNewBorder ) CLASS TBrowse

   if ISCHARACTER( cNewBorder ) .AND.;
      ( Len( cNewBorder ) == 0 .or. Len( cNewBorder ) == 8 )

      if ::cBorder == ""
         if cNewBorder == ""
            // Nothing
         else
            ::cBorder := cNewBorder
            ::nwTop++
            ::nwLeft++
            ::nwRight--
            ::nwBottom--
         endif
      else
         ::cBorder := cNewBorder
         if ::cBorder == ""
            ::nwTop--
            ::nwLeft--
            ::nwRight++
            ::nwBottom++
         endif
      endif
      ::Configure()
   endif

   Return Self

//---------------------------------------------------------------------//

// This method calculates variables related to horizontal coordinates
METHOD PreConfigHorizontal( uValue ) CLASS TBrowse

   ::lConfigured := .f.
   ::nVisWidth := ::nwRight - ::nwLeft + 1

return uValue

//---------------------------------------------------------------------//

// This method calculates variables related to vertical coordinates
METHOD PreConfigVertical( uValue ) CLASS TBrowse

   ::lConfigured := .f.
   ::nRowCount := ::nwBottom - ::nwTop + 1 - iif( ::lHeaders, ::nHeaderHeight, 0 ) - ;
                  iif( ::lFooters, ::nFooterHeight, 0 ) - ;
                  iif( ::lHeadSep .OR. ::lColHeadSep, 1, 0 ) - ;
                  iif( ::lFootSep .OR. ::lColFootSep, 1, 0 )

return uValue


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
              { K_LBUTTONDOWN, {| oB, nKey | tbmouse( ob, mrow(), mcol() ) } } }
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

Method HitTest( mrow,mcol ) CLASS TBrowse
   LOCAL i, nVisCol, lHitHeader := .f.
   LOCAL nColPos

   ::nMRowPos := ::nRowPos
   ::nMColPos := ::colPos

   if mRow < ::nTop .or. mRow > ::rect[ 3 ]
      return HTNOWHERE
   endif

   if mCol < ::rect[ 2 ] .or. mCol > ::rect[ 4 ]
      return HTNOWHERE
   endif

   ::nMRowPos := mRow - ::rect[ 1 ] + 1
   // Is the header separator part of the "header" when click?
   if ::nMRowPos < 1 - if( ::lHeadSep .OR. ::lColHeadSep , 1, 0 )
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
      nColPos := ::nLeftVisible + nColPos - ::nFrozenCols - 1
   else
      nColPos := ::nLeftVisible + nColPos - 1
   endif

   ::nMColPos := nColPos

   Return if( lHitHeader, HTHEADING, HTCELL )

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

//-------------------------------------------------------------------//

function TBMOUSE( oBrowse, nMouseRow, nMouseCol )
   LOCAL n

   if oBrowse:hittest( nMouseRow, nMouseCol ) == HTCELL

      n := oBrowse:nMRowPos - oBrowse:rowpos

      do while n < 0
         n++
         oBrowse:up():forceStable()
      enddo

      do while n > 0
         n--
         oBrowse:down():forceStable()
      enddo

      n := oBrowse:nMColPos - oBrowse:ColPos

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
      cList += alltrim( str( a ) ) + ", "
   next

   cList := substr( cList,1,len( cList )-2 )

   Return cList

//-------------------------------------------------------------------//

static function Color2Array( cColorSpec )
   LOCAL n
   LOCAL a_:= {}
   LOCAL cToken := ','

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

/* 2005/Aug/21 - Eduardo Fernandes <modalsist>
if any index into oCol:defcolor is out of scope, return
default index color, if not, return original one.
Clipper compatibility */


STATIC FUNCTION DefColorToDisp( aColor, aColorSpec )
LOCAL aReturn, i

 IF Valtype( aColor )=="A" .AND. Len(aColor) > 1

    aReturn := aColor

    FOR i := 1 to Len( aColor )
        IF aColor[ i ] > Len( aColorSpec)
           aReturn := {1,2,1,1}
           EXIT
        ENDIF
    NEXT

 ELSE
    aReturn := {1,2,1,1}
 ENDIF


RETURN ( aReturn )


//-------------------------------------------------------------------//
//
//                   Functions to Activate TBrowse
//
//-------------------------------------------------------------------//


FUNCTION TBrowseNew( nTop, nLeft, nBottom, nRight )
/* 2005/Sep/15 - Eduardo Fernandes
Please, don't remove/change 5th parameter <.F.> passed to TBrowse:New(). 
See TBrowse:New() method for more details.
*/
RETURN ( TBrowse():New( nTop, nLeft, nBottom, nRight, .F. ) )


//-------------------------------------------------------------------//
