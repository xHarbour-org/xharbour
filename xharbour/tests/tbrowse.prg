/*
 * $Id: tbrowse.prg,v 1.14 2002/09/27 20:51:49 map Exp $
 */

/*
 * xHarbour Project source code:
 * TBrowse Class
 *
 * Copyright 2003 Francesco Saverio Giudice <info@fsgiudice.com>
 * www - http://www.xharbour.org http://www.harbour-project.org
 *
 * NOTE:
 * 2004/01/04 Completely rewrote almost from scratch
 * The original TBrowse authors are reported below
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
 * Original author
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
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

#command __OutDebug( <x,...> ) =>

#define HB_CLS_NOTOBJECT  // this will avoid any default object to be inherited - for binary compatibility

#include "common.ch"
#include "hbclass.ch"
#include "classex.ch"
#include "color.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "button.ch"
#include "tbrowse.ch"


/*
  FSG 01/11/2003 21.30 - Modifica importante. Cambio ragionamento: non utilizzo un array che contiene le
                         colonne. Sono le colonne stesse che portano con se le informazioni che mi servono
                         per la loro visualizzazione.

                         Nei campi delle colonne clonate metter• le informazioni calcolate
                         come width, colsep, ecc.
                         Utilizzo il campo cargo delle colonne clonate per mettere la dimensione
                         complessiva della colonna compresa del separatore

                         Cosi' mi e' piu' facile tenere traccia delle informazioni

*/

FUNCTION TBrowseNew(nTop, nLeft, nBottom, nRight)
RETURN TBrowse():New(nTop, nLeft, nBottom, nRight)

// TBrowseDB() function is defined in another PRG

CLASS TBrowse

   // ----------------------- BEGIN OF PUBLIC VARIABLES AND METHODS ---------------------------------------

   // LEAVE DATA IN THIS ORDER TO BE BINARY COMPATIBLE!

   DATA     cargo                                            // User-definable variable
   PROPERTY nTop           READ FnTop       WRITE SetTop     // Top row number for the TBrowse display
   PROPERTY nLeft          READ FnLeft      WRITE SetLeft    // Leftmost column for the TBrowse display
   PROPERTY nBottom        READ FnBottom    WRITE SetBottom  // bottom row number for the TBrowse display
   PROPERTY nRight         READ FnRight     WRITE SetRight   // Rightmost column for the TBrowse display
   DATA     aColumns       HIDDEN                            // Array to hold all browse columns
   DATA     headSep                                          // Heading separator character
   PROPERTY colSep         READ FcolSep     WRITE SetColSep  // Column separator character
   DATA     footSep                                          // Footing separator character
   DATA     colorSpec                                        // Color table for the TBrowse display
   DATA     goTopBlock                                       // Code block executed by TBrowse:goTop()
   DATA     goBottomBlock                                    // Code block executed by TBrowse:goBottom()
   DATA     skipBlock                                        // Code block used to reposition data source

   DATA     autoLite                                         // Logical value to control highlighting
   PROPERTY colCount       READ FcolCount                    // Number of browse columns. Equivalent to Len(::aColumns) but faster
   DATA     colPos                                           // Current cursor column position  - changing this not alter configurazion
   PROPERTY freeze         READ Ffreeze     WRITE SetFreeze  // Number of columns to freeze/frozen
   DATA     hitBottom                                        // Indicates the end of available data
   DATA     hitTop                                           // Indicates the beginning of available data
   PROPERTY leftVisible    READ FleftVisible                 // Indicates position of leftmost unfrozen column in display
   PROPERTY rightVisible   READ FrightVisible                // Indicates position of rightmost unfrozen column in display
   PROPERTY rowCount       READ GetRowCount                  // Number of visible data rows in the TBrowse display
   DATA     rowPos                                           // Current cursor row position
   DATA     stable                                           // Indicates if the TBrowse object is stable

#ifdef HB_COMPAT_C53
   DATA     nRow                                             // Row number for the actual cell
   DATA     nCol                                             // Col number for the actual cell
   DATA     aKeys
   DATA     mColpos,mrowPos,message
#endif

   // Cursor movement methods
   METHOD Down()                                            // Moves the cursor down one row
   METHOD End()                                             // Moves the cursor to the rightmost visible data column
   METHOD GoBottom()                                        // Repositions the data source to the bottom of file
   METHOD GoTop()                                           // Repositions the data source to the top of file
   METHOD Home()                                            // Moves the cursor to the leftmost visible data column
   MESSAGE Left() METHOD _Left()                             // Moves the cursor left one column
   METHOD PageDown()                                        // Repositions the data source downward
   METHOD PageUp()                                          // Repositions the data source upward
   METHOD PanEnd()                                          // Moves the cursor to the rightmost data column
   METHOD PanHome()                                         // Moves the cursor to the leftmost visible data column
   METHOD PanLeft()                                         // Pans left without changing the cursor position
   METHOD PanRight()                                        // Pans right without changing the cursor position
   MESSAGE Right() METHOD _Right()                           // Moves the cursor right one column
   METHOD Up()                                              // Moves the cursor up one row

   // Miscellaneous methods
   METHOD AddColumn( oCol )
   METHOD ColorRect()                                       // Alters the color of a rectangular group of cells
   METHOD ColWidth( nColumn )                               // Returns the display width of a particular column
   METHOD Configure( nMode )                                // Reconfigures the internal settings of the TBrowse object
                                                            // nMode is an undocumented parameter in CA-Cl*pper
   METHOD DeHilite()                                        // Dehighlights the current cell
   METHOD DelColumn( nPos )                                 // Delete a column object from a browse
   METHOD ForceStable()                                     // Performs a full stabilization
   METHOD GetColumn( nColumn )                              // Gets a specific TBColumn object
   METHOD Hilite()                                          // Highlights the current cell
   METHOD InsColumn( nPos, oCol )                           // Insert a column object in a browse
   METHOD Invalidate()                                      // Forces entire redraw during next stabilization
   METHOD RefreshAll()                                      // Causes all data to be recalculated during the next stabilize
   METHOD RefreshCurrent()                                  // Causes the current row to be refilled and repainted on next stabilize
   METHOD SetColumn( nColumn, oCol )                        // Replaces one TBColumn object with another
   METHOD Stabilize()                                       // Performs incremental stabilization

#ifdef HB_COMPAT_C53
   METHOD SetKey(nKey, bBlock)
   METHOD ApplyKey(nKey)
   METHOD InitKeys(Self)
   METHOD TApplyKey(nKey, o)
   METHOD HitTest(nMouseRow,nMouseCol)
   METHOD SetStyle(nMode,lSetting)
#endif

   // ----------------------- END OF PUBLIC VARIABLES AND METHODS ---------------------------------------

   METHOD New(nTop, nLeft, nBottom, nRight) CONSTRUCTOR     // Constructor

#ifdef HB_COMPAT_C53

   PROTECTED:     /* P R O T E C T E D */

   METHOD MGotoYX(nRow, nCol)             // Given screen coordinates nRow, nCol sets TBrowse cursor on underlaying cell
                                          // _M_GotoXY because this method will mostly be called to handle mouse requests
#endif

   HIDDEN:         /* H I D D E N */

   DATA aColumnsCloned                    // Array to hold all browse columns as defined before a ::configure()
                                          // in cloned objects we store all internal column values to display data

   DATA aVisibleColumns                   // Array with displayed TBrowse's columns
                                          // Contains arrays { column number, column width }
   ACCESS nVisibleColumns        INLINE Len( ::aVisibleColumns )  // Number of columns that fit on the browse width

   DATA lConfigured                       // .T. if all ok
   DATA lUserDeHilite                     // change as user request, start as .F.
   DATA lHilited                          // .T. if current cell is hilited

   DATA lHeaders                          // Internal variable which indicates if there are column headers to paint
   DATA nHeadersHeight                    // How many lines is highest Header ?
   DATA lHeaderSep                        // Is there a Header Sep ?

   DATA lFooters                          // Internal variable which indicates if there are column footers to paint
   DATA nFootersHeight                    // How many lines is highest Footer ?
   DATA lFooterSep                        // Is there a Footer Sep ?

   DATA nPendingSkip                      // Number of pending skips that must be performed before TBrowse is stable

   DATA nSkipped                          // Number of record really skipped
   DATA lRedrawFrame                      // True if I need to redraw Headers/Footers
   DATA lRefreshAll
   DATA lRefreshCurrent
   DATA lDisplayAll
   DATA lInvalidate

   // --------------- Virtual Screen - Begin ------------------- //
   DATA cVirtualScreen                    // This contains the screen of data
   DATA nVSBlockSize                      // Lenght of each row of data in VS
   DATA nVSDataFirstRow                   // First row of data in VS
   DATA nVSDataLastRow                    // Last row of data in VS
   DATA nVSLastRowOfData                  // Last row in VS in case of RowCount() > LastRec()
   DATA nVSHeadSepRow                     // Row of VS where is Head Separator (if not present value is -1)
   DATA nVSFootSepRow                     // Row of VS where is Foot Separator (if not present value is -1)
   DATA nVSHeadFirstRow                   // First Row in VS where begin Heading (is always 1)
   DATA nVSFootFirstRow                   // First Row in VS where begin Footing

   DATA nVSFreezeSize                     // Lenght of freeze part of blocksize
   DATA nVSUnFrzVisPos                    // Position from Left of Row of Unfrozen Visible Column
   DATA nVSUnFrzVisLen                    // Lenght of Row of Unfrozen Visible Column
   DATA nVSLenLeftSep                     // Width of left separator of First UNFrozen Column
   DATA nVSSpaceLeft                      // Number of spaces on Left of First UNFrozen Column
   DATA nVSSpaceRight                     // Number of spaces on Right of last visible column
   DATA lVSMoveDown                       // if .t. i go down, otherwise i move up during refreshall

   METHOD VSLenght()                   INLINE ::nVSBlockSize * ( ::nVSLastRowOfData + 2 )  // Ghost + 1
   METHOD VSClear()                    INLINE ::cVirtualScreen := ""
   METHOD VSPrepare()                  INLINE ::cVirtualScreen := Space( ::VSLenght() )
   METHOD VSMake()
   METHOD VSBuildRow()
   METHOD VSBuildEmptyRow()
   METHOD VSCalcVisColumns()
   // the follow method extract a row from VS
   METHOD VSGetRow( nRow )             INLINE Substr( ::cVirtualScreen, (nRow) * ::nVSBlockSize + 1, ::nVSBlockSize )
   // the follow method insert a row in VS
   METHOD VSInsertRow( nRow, cString ) INLINE ::cVirtualScreen := Stuff( ::cVirtualScreen, (nRow) * ::nVSBlockSize + 1, 0, cString )
   // the follow method change a row in VS
   METHOD VSSetRow( nRow, cString )    INLINE ::cVirtualScreen := Stuff( ::cVirtualScreen, (nRow) * ::nVSBlockSize + 1, ::nVSBlockSize, cString )
   // the follow method extracts the part of unfrozen visible column of a row from VS
   METHOD VSGetFreezePart( nRow )      INLINE Left( ::VSGetRow( nRow ), ::nVSFreezeSize )
   //METHOD VSGetUnFrzVisPart( nRow )    INLINE Substr( ::VSGetRow( nRow ), ::nVSUnFrzVisPos + IIF( ::FleftVisible > 1, 1, 0 ), ::nVSUnFrzVisLen )
   METHOD VSGetUnFrzVisPart( nRow )    INLINE Substr( ::VSGetRow( nRow ), ::nVSUnFrzVisPos, ::nVSUnFrzVisLen )
   METHOD VSGetVisiblePart( nRow )     INLINE ::VSGetFreezePart( nRow ) + IIF( ::Ffreeze == 0, ;
                                                                               Space( ::nVSSpaceLeft ) + ::VSGetUnFrzVisPart( nRow ),;
                                                                               Left( ::VSGetUnFrzVisPart( nRow ), ::nVSLenLeftSep ) + Space( ::nVSSpaceLeft ) + Substr( ::VSGetUnFrzVisPart( nRow ), ::nVSLenLeftSep + 1 ) ;
                                                                             )
   METHOD VSGhostRow()                 INLINE ::nVSLastRowOfData + 1
   METHOD VSDisplayRow()
   METHOD VSDisplayDataRow()
   METHOD VSDataRows()                 INLINE ::nVSDataLastRow - ::nVSDataFirstRow
   METHOD VSGetOffSet( nCol )          INLINE ::aColumnsCloned[ nCol ]:nOffset + ;
                                              IIF( ( ::Ffreeze > 0 .AND. nCol == 1 ) .OR. ;
                                                   ( ::Ffreeze == 0 .AND. nCol == 1 ) /*::FleftVisible*/, ;
                                                   0, ;
                                                   Len( ::aColumnsCloned[ nCol ]:colSep ) + 1 )


   // --------------- Virtual Screen - End   ------------------- //

   METHOD DoConfiguration()               // Perform a configuration
   METHOD BrowseWidth()          INLINE ::FnRight - ::FnLeft + 1

   METHOD MoveCursorToColumn()   // Internal method, move display cursor between columns
   METHOD MoveRow( nToSkip )
   METHOD CalculateVisibleColumns()
   METHOD ConfigureHeaders()
   METHOD ConfigureFooters()
   METHOD CalculateColumnsWidth()
   METHOD RedrawHeaders()                 // Repaints TBrowse Headers
   METHOD DisplayCell()

   // Informative methods
   METHOD TotalHeadersHeight()       INLINE IIF( ::lHeaders, ::nHeadersHeight, 0 ) + IIF( ::lHeaderSep, 1, 0 )
   METHOD TotalFootersHeight()       INLINE IIF( ::lFooters, ::nFootersHeight, 0 ) + IIF( ::lFooterSep, 1, 0 )

   METHOD GetHeaderLine()
   METHOD GetFooterLine()

   // Property get methods
   METHOD GetRowCount()         INLINE ::FnBottom - ::FnTop + 1 - ::TotalHeadersHeight() - ::TotalFootersHeight()

   // Property set methods
   METHOD SetColSep( cStr )     INLINE ::FColSep := IIF( cStr == NIL, " ", cStr )
   METHOD SetFreeze( n )        INLINE ( IIF( n > 0 .AND. n < ::FcolCount, ::Ffreeze := n, NIL ), ::invalidate() )
   METHOD SetBottom( nBottom )  INLINE ::lConfigured := .F., ::FnBottom := nBottom
   METHOD SetLeft( nLeft )      INLINE ::lConfigured := .F., ::FnLeft   := nLeft
   METHOD SetRight( nRight )    INLINE ::lConfigured := .F., ::FnRight  := nRight
   METHOD SetTop( nTop )        INLINE ::lConfigured := .F., ::FnTop    := nTop

#ifdef HB_COMPAT_C53
   DATA rect
   DATA aVisibleCols
   DATA aSetStyle
#endif
ENDCLASS

// ------------------- BEGIN INTERNAL CONFIGURATION METHODS -------------------------------

METHOD New(nTop, nLeft, nBottom, nRight) CLASS TBrowse

   DEFAULT nTop    TO 0
   DEFAULT nLeft   TO 0
   DEFAULT nBottom TO MaxRow()
   DEFAULT nRight  TO MaxCol()

   // Assign Defaults to public
   ::autoLite        := .T.
   ::cargo           := NIL
   ::colorSpec       := SetColor()
   ::colPos          := 1
   ::colSep          := " "
   ::footSep         := ""
   ::Ffreeze         := 0      // ::freeze access value
   ::goBottomBlock   := {|| NIL }
   ::goTopBlock      := {|| NIL }
   ::headSep         := ""
   ::hitBottom       := .F.
   ::hitTop          := .F.
   ::FnBottom        := nBottom
   ::FnLeft          := nLeft
   ::FnRight         := nRight
   ::FnTop           := nTop
   ::rowPos          := 1
   ::skipBlock       := {|| NIL }
   ::stable          := .F.

   // defaults
   ::FleftVisible    := 0  // ::leftVisible access value
   ::FrightVisible   := 0  // ::rightVisible access value

   // Internals
   ::aColumns        := {} // Column objects array
   ::aColumnsCloned  := {} // Column cloned objects array
   ::FcolCount       := 0
   ::lConfigured     := .F.
   ::lRedrawFrame    := .T.
   ::lUserDeHilite   := .F.
   ::lHilited        := .F.
   ::lInvalidate     := .T.
   ::lDisplayAll     := .F.
   ::lRefreshAll     := .T.
   ::lRefreshCurrent := .F.
   ::nSkipped        := 0

   // Internal Virtual Screen defaults
   ::cVirtualScreen    := ""               // This contains the screen of data
   ::nVSBlockSize      := 0                // Lenght of each row of data in VS
   ::nVSDataFirstRow   := 0                // First row of data in VS
   ::nVSDataLastRow    := 0                // Last row of data in VS
   ::nVSLastRowOfData  := 0                // Last row in VS in case of RowCount() > LastRec()
   ::nVSHeadSepRow     := 0                // Row of VS where is Head Separator (if not present value is -1)
   ::nVSFootSepRow     := 0                // Row of VS where is Foot Separator (if not present value is -1)
   ::nVSHeadFirstRow   := 0                // First Row in VS where begin Heading (is always 1)
   ::nVSFootFirstRow   := 0                // First Row in VS where begin Footing
   ::nVSFreezeSize     := 0                // Lenght of freeze part of blocksize
   ::nVSUnFrzVisPos    := 0                // Position from Left of Row of Unfrozen Visible Column
   ::nVSUnFrzVisLen    := 0                // Lenght of Row of Unfrozen Visible Column
   ::nVSLenLeftSep     := 0                // Lenght of Left Separato of first unfrozen column
   ::nVSSpaceLeft      := 0                // Number of spaces on Left of First UNFrozen Column
   ::nVSSpaceRight     := 0
   ::lVSMoveDown       := .T.

   ::lHeaders   := .F.
   ::lFooters   := .F.
   ::lHeaderSep := .F.
   ::lFooterSep := .F.
   ::nHeadersHeight := 0
   ::nFootersHeight := 0

 #ifdef HB_COMPAT_C53
   ::mColPos         := 0
   ::mRowPos         := 0
   ::rect            := { nTop, nLeft, nBottom, nRight }
   ::aVisibleCols    := {}
   ::message         := ''
   ::nRow            := 0
   ::nCol            := 0
   ::aSetStyle       := ARRAY( 5 )

   ::aSetStyle[ TBR_APPEND ]    := .F.
   ::aSetStyle[ TBR_APPENDING ] := .F.
   ::aSetStyle[ TBR_MODIFY ]    := .F.
   ::aSetStyle[ TBR_MOVE ]      := .F.
   ::aSetStyle[ TBR_SIZE ]      := .F.

 #endif

RETURN Self

// This method create the virtual screen
METHOD VSMake() CLASS TBrowse
   LOCAL cGhost, oCol, cString, n
   // The virtual screen contains:
   // n row of headers
   // 1 row of head separators
   // n row of data
   // 1 row of foot separators
   // n row of footers
   // 1 ghost row that is like a data row but contains space instead of data

   ::cVirtualScreen   := ""
   ::nVSLastRowOfData := 0         // First time RowPos contains 1, during first stabilization it
                                   // must be incremented by 1 until it reach the RowCount or
                                   // EOF is reached

   // First perform a header configuration
   ::ConfigureHeaders()

   // Second perform a footer configuration
   ::ConfigureFooters()

   // Now we have to calculate single column width
   ::CalculateColumnsWidth()

   // Define last internal counter
   ::nVSLastRowOfData := ::TotalHeadersHeight()    + ;
                         ::GetRowCount()      + ;
                         ::TotalFootersHeight()    - 1 // -1 is why internal VS counters starts from 0

   __OutDebug( "::nHeadersHeight, ::nFootersHeight", ::nHeadersHeight, ::nFootersHeight )

   // Ok, now we have all informations. So we can start to build
   // internal screen

   // Prepare the buffer of Virtual Screen
   ::VSPrepare()


   //__OutDebug( "Len(::cVirtualScreen)", Len(::cVirtualScreen) )
   //__OutDebug( "::FnTop, ::FnLeft, ::FnBottom, ::FnRight", ::FnTop, ::FnLeft, ::FnBottom, ::FnRight )
   //__OutDebug( "::nVSBlockSize", ::nVSBlockSize )


   // Now begin to fill

   // Start with the ghost row
   cGhost  := ""
   FOR EACH oCol IN ::aColumnsCloned

       IF HB_EnumIndex() == 1
          // Column separator is NOT added to the first column.
          cGhost += Space( oCol:width )
       ELSE
          cGhost += oCol:colSep + Space( oCol:width )
       ENDIF

   NEXT
   // Save Ghost row
   ::VSSetRow( ::VSGhostRow(), cGhost )
   //__OutDebug( "GhostRow", HB_OemToAnsi( cGhost ) )

   IF ::lHeaders
      // Getting headers

       // Build header that has space and not column separator
       FOR n := 1 TO ::nHeadersHeight
          cString := ""
          FOR EACH oCol IN ::aColumnsCloned
              //__OutDebug( "::GetHeaderLine( oCol, n )", ::GetHeaderLine( oCol, n ) )

              IF HB_EnumIndex() == 1
                 cString += PadR( ::GetHeaderLine( oCol, n ), oCol:nFullWidth ) // oCol:cargo contains the full width
              ELSE
                 cString += PadR( Space( Len( oCol:colSep ) ) + ::GetHeaderLine( oCol, n ), oCol:nFullWidth ) // oCol:cargo contains the full width
              ENDIF
          NEXT
          ::VSSetRow( n-1, cString )
          //TraceLog( "Headers cString", HB_OemToAnsi( cString ) )
       NEXT

   ENDIF
   //TraceLog( "Header: ::cVirtualScreen", hb_oemtoansi( ::cVirtualScreen ) )

   IF ::lHeaderSep
      cString := ""
      FOR EACH oCol IN ::aColumnsCloned
          IF HB_EnumIndex() == 1
             cString += PadR( "", oCol:nFullWidth, Right( oCol:headSep, 1 ) )
          ELSE
             cString += PadR( Left( oCol:headSep, 1 ), oCol:nFullWidth, Right( oCol:headSep, 1 ) )
          ENDIF
          //__OutDebug( "oCol:Cargo:nFullWidth, oCol:headSep", oCol:Cargo:nFullWidth, HB_OemToAnsi( oCol:headSep ) )
      NEXT
      ::nVSHeadSepRow := ::nHeadersHeight
      ::VSSetRow( ::nVSHeadSepRow, cString )
      //TraceLog( "HeadSep cString, ::nVSHeadSepRow", HB_OemToAnsi( cString ), ::nVSHeadSepRow )
   ELSE
      ::nVSHeadSepRow := -1
   ENDIF
   //TraceLog( "HeadSep: ::cVirtualScreen", hb_oemtoansi( ::cVirtualScreen ) )

   IF ::lFooterSep
      cString := ""
      FOR EACH oCol IN ::aColumnsCloned
          IF HB_EnumIndex() == 1
             cString += PadR( "", oCol:nFullWidth, Right( oCol:footSep, 1 ) )
          ELSE
             cString += PadR( Left( oCol:footSep, 1 ), oCol:nFullWidth, Right( oCol:footSep, 1 ) )
          ENDIF
          //__OutDebug( "oCol:Cargo:nFullWidth, oCol:footSep", oCol:Cargo:nFullWidth, HB_OemToAnsi( oCol:footSep ) )
      NEXT
      ::nVSFootSepRow := ::TotalHeadersHeight() + ;
                         ::GetRowCount()
      ::VSSetRow( ::nVSFootSepRow, cString )
      //TraceLog( "FootSep cString, ::nVSFootSepRow", HB_OemToAnsi( cString ), ::nVSFootSepRow )
   ELSE
      ::nVSFootSepRow := -1
   ENDIF
   //TraceLog( "FootSep: ::cVirtualScreen", hb_oemtoansi( ::cVirtualScreen ) )

   IF ::lFooters
      // Getting footers

       // Build footer that has space and not column separator
       ::nVSFootFirstRow := IIF( ::nVSFootSepRow == -1, ;
                                 ::TotalHeadersHeight() + ;
                                 ::GetRowCount(),;
                                 ::nVSFootSepRow + 1 )
       FOR n := 1 TO ::nFootersHeight
          cString := ""
          FOR EACH oCol IN ::aColumnsCloned
              IF HB_EnumIndex() == 1
                 cString += PadR( ::GetFooterLine( oCol, n ), oCol:nFullWidth ) // oCol:cargo contains the full width
              ELSE
                 cString += PadR( Space( Len( oCol:colSep ) ) + ::GetFooterLine( oCol, n ), oCol:nFullWidth ) // oCol:cargo contains the full width
              ENDIF
          NEXT
          ::VSSetRow( ::nVSFootFirstRow + n - 1, cString )
          //TraceLog( "Footers cString, ::nVSFootFirstRow + n - 1", HB_OemToAnsi( cString ), ::nVSFootFirstRow + n - 1 )
       NEXT

   ENDIF
   //TraceLog( "Footers: ::cVirtualScreen", hb_oemtoansi( ::cVirtualScreen ) )

    // Finished, now it's time to stabilize
   ::nVSDataFirstRow := IIF( ::nVSHeadSepRow == -1, ::nHeadersHeight, ::nVSHeadSepRow + 1 )
   ::nVSDataLastRow  := IIF( ::nVSFootSepRow == -1, ::TotalHeadersHeight() + ;
                             ::GetRowCount(), ::nVSFootSepRow - 1 )


RETURN Self

METHOD VSBuildRow( nLine )
   LOCAL cGhost, cString, xValue, cType, oCol, n
   LOCAL cTemp
   LOCAL nRow := nLine - 1 + ::nVSDataFirstRow
   //__OutDebug( "nRow, ::nVSDataFirstRow, ::RowPos", nRow, ::nVSDataFirstRow, ::RowPos )
   //__OutDebug( "::VSGhostRow(), ::VSGetRow( nRow )", ::VSGhostRow(), ::VSGetRow( nRow ) )

   IF nRow >= ::nVSDataFirstRow .AND. nRow <= ::nVSDataLastRow
      ::VSSetRow( nRow, ::VSGetRow( ::VSGhostRow() ) )
      FOR EACH oCol IN ::aColumns
          xValue  := Eval( oCol:block )
          cType   := ValType( xValue )

          DO CASE
             CASE cType == "L"
                  cString := PadC( Transform( xValue, oCol:Picture ), ::aColumnsCloned[ HB_EnumIndex() ]:width )
             CASE cType == "N"
                  cString := PadL( Transform( xValue, oCol:Picture ), ::aColumnsCloned[ HB_EnumIndex() ]:width )
             OTHERWISE
                  cString := PadR( Transform( xValue, oCol:Picture ), ::aColumnsCloned[ HB_EnumIndex() ]:width )
          ENDCASE
          cTemp := ::VSGetRow( nRow )
          //__OutDebug( "cTemp, cString", cTemp, cString )
          ::VSSetRow( nRow, Stuff( cTemp, ::VSGetOffset( HB_EnumIndex() ) , ::aColumnsCloned[ HB_EnumIndex() ]:width, cString ) )
      NEXT
   ELSE
      ::VSSetRow( nRow, ::VSGetRow( ::VSGhostRow() ) )
   ENDIF
RETURN Self

METHOD VSBuildEmptyRow( nLine )
   LOCAL nRow := nLine - 1 + ::nVSDataFirstRow
   IF nRow >= ::nVSDataFirstRow .AND. nRow <= ::nVSDataLastRow
      ::VSSetRow( nRow, ::VSGetRow( ::VSGhostRow() ) )
   ENDIF
RETURN Self

METHOD VSCalcVisColumns()
   LOCAL nWidth, oCol, nCol

   // Here we have to calculate visible columns
   // We have these starting points:
   // 1) the freeze part
   // 2) ColPos
   // 3) the browse width
   // so we have to:
   // a) calculate the freeze part and check if this part fit in browse width
   //    if not we release the frozen columns defining freeze := 0
   // b) start from last visible

   // First we calculate the freeze part
   nWidth := 0
   FOR nCol := 1 TO ::Ffreeze
       oCol := ::aColumnsCloned[ nCol ]
       nWidth += oCol:nFullWidth
   NEXT

   // Frozen columns width is larger than browse width, so release freeze
   IF nWidth > ::BrowseWidth()
      ::Ffreeze := 0
      nWidth    := 0
   ENDIF
   ::nVSFreezeSize := nWidth
   __OutDebug( "::nVSFreezeSize", ::nVSFreezeSize )

   // Now we calculate the width of visible columns
   nWidth  := 0
   IF ::FleftVisible == 0
      ::FleftVisible := ::Ffreeze + 1
   ENDIF
   FOR nCol := ::FleftVisible TO ::FColCount
       oCol := ::aColumnsCloned[ nCol ]
       IF ::nVSFreezeSize + nWidth + IIF( ::Ffreeze == 0 .AND. nCol == ::FleftVisible, oCol:nFullWidth - Len( oCol:colSep ), oCol:nFullWidth ) <= ::BrowseWidth()
          nWidth  +=  IIF( ::Ffreeze == 0 .AND. nCol == ::FleftVisible, oCol:nFullWidth - Len( oCol:colSep ), oCol:nFullWidth )
          //IF nCol == ::FleftVisible .AND. ::Ffreeze == 0
          //    nWidth -= IIF( nCol > ::FFreeze, Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ), 0 )
          //ENDIF
       ELSE
          nCol--
          EXIT
       ENDIF
   NEXT
   //__OutDebug( "nCol, ::aColumnsCloned[ ::FleftVisible ]:cargo:nOffset, Len( ::aColumnsCloned[ ::FleftVisible ]:colSep )", nCol, ::aColumnsCloned[ ::FleftVisible ]:cargo:nOffset, Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ) )
   //__OutDebug( "IIF( ::FFreeze == 0 .AND. nCol > 1, Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ), 0 )",;
   //             IIF( ::FFreeze == 0 .AND. nCol > 1, Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ), 0 ) )

   ::FrightVisible  := nCol
   ::nVSUnFrzVisPos := ::aColumnsCloned[ ::FleftVisible ]:nOffset + IIF( ::Ffreeze == 0, Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ) + 1, 1 )
   ::nVSUnFrzVisLen := nWidth //- IIF( ::FFreeze == 0, Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ), 0 )
   ::nVSLenLeftSep  := Len( ::aColumnsCloned[ ::FleftVisible ]:colSep )
   ::nVSSpaceLeft   := Int( ( ::BrowseWidth() - ::nVSFreezeSize - nWidth) / 2 )
   ::nVSSpaceRight  := Int( ( ::BrowseWidth() - ::nVSFreezeSize - nWidth) - ::nVSSpaceLeft )
   __OutDebug( "::BrowseWidth(), ::nVSFreezeSize, nWidth, ::ColPos, ::freeze, ::FleftVisible, ::FrightVisible", ;
                ::BrowseWidth(), ::nVSFreezeSize, nWidth, ::ColPos, ::freeze, ::FleftVisible, ::FrightVisible )
   __OutDebug( "::nVSUnFrzVisPos, ::nVSUnFrzVisLen, ::nVSSpaceLeft, ::nVSSpaceRight", ;
                ::nVSUnFrzVisPos, ::nVSUnFrzVisLen, ::nVSSpaceLeft, ::nVSSpaceRight )
   //TraceLog( "::BrowseWidth(), nWidth, ::nVSUnFrzVisPos, ::nVSSpaceLeft", ::BrowseWidth(), nWidth, ::nVSUnFrzVisPos, ::nVSSpaceLeft )

RETURN Self

METHOD GetHeaderLine( oCol, nLine )
   LOCAL cLine := ""
   LOCAL cS := oCol:heading // Save in a local var because __StrToken work by reference
   IF ::lHeaders .AND. nLine >= 1 .AND. nLine <= ::nHeadersHeight
      // Headers are padded right
      cLine := PadR( __StrToken(@cS, nLine, ";"), oCol:width )
   ENDIF
RETURN cLine

METHOD GetFooterLine( oCol, nLine )
   LOCAL cLine := ""
   LOCAL cS := oCol:footing // Save in a local var because __StrToken work by reference
   IF ::lFooters .AND. nLine >= 1 .AND. nLine <= ::nFootersHeight
      // Headers are padded right
      cLine := PadR( __StrToken(@cS, nLine, ";"), oCol:width )
   ENDIF
RETURN cLine

METHOD DoConfiguration(nMode) CLASS TBrowse
   LOCAL n, nHeight
   LOCAL nLeft,nRight
   LOCAL oCol, oCargo

   IF !::lConfigured

      // First clone column objects
      ::aColumnsCloned := {}
      FOR EACH oCol IN ::aColumns
          aAdd( ::aColumnsCloned, ;
                TBColumnInfo( HB_EnumIndex(), oCol:width, oCol:Heading, oCol:Footing, oCol:headSep, oCol:footSep, oCol:colSep ) ;
              )
      NEXT

      //// Check header presence and configure them
      ////tracelog("Avvio configurazione headers" )
      //::ConfigureHeaders()
      //
      //// Check footer presence and configure them
      //::ConfigureFooters()
      //
      //// Recalculate columns width
      //::CalculateColumnsWidth()
      //
      //// 20/nov/2000 - maurilio.longo@libero.it
      //// If I add (or remove) header or footer (separator) I have to change number
      //// of available rows
      //::FrowCount := ::FnBottom - ::FnTop + 1 - ;
      //              iif( ::lHeaders, ::nHeadersHeight, 0 ) - ;
      //              iif( ::lFooters, ::nFootersHeight, 0 ) - ;
      //              iif( Empty( ::HeadSep ), 0, 1 ) - ;
      //              iif( Empty( ::FootSep ), 0, 1 )
      //
      //// Define internal column objects
      //FOR EACH oCol IN ::aColumnsCloned
      //    oCargo := oCol:cargo
      //    oCargo:aRows := Array( ::FrowCount )
      //NEXT
      ::VSMake()

      // We have to invalidate the screen during configuration
      ::Invalidate()
      ::RefreshAll()

      #ifdef HB_COMPAT_C53
      //::rect:={::FnTop+::nHeadersHeight,::FnLeft,::FnBottom-::nHeadersHeight,::FnRight}
      //FOR n := ::FnLeft TO ::FnRight
      //   // aadd(::aVisibleCols,n)
      //NEXT
      #endif

      ::lConfigured := .T.

   ENDIF

RETURN Self

METHOD ConfigureHeaders() CLASS TBrowse
   LOCAL oCol, nMaxHeight := 0
   LOCAL nTokens

   ::lHeaders := .F.

   // Are there column headers to paint ?
   FOR EACH oCol IN ::aColumnsCloned
       IF !::lHeaders .AND. !Empty( oCol:Heading )
          ::lHeaders := .T.
          //nMaxHeight := 0
       ENDIF

       // Put the headSep in cloned columns
       oCol:headSep := IIF( oCol:headSep <> NIL, oCol:headSep, ::headSep )
       // Check if there are headSep
       IF !Empty( oCol:headSep )
          ::lHeaderSep := .T.
       ENDIF

       // Here i determine how is high max header
       nTokens      := __StrTokenCount( oCol:Heading, ";" )
       nMaxHeight := Max( nMaxHeight, nTokens )

       // Using same loop to prepare column separator (column separator goes left to the column)
       IF HB_EnumIndex() == 1
          oCol:colSep  := "" // First column doesn't have column separator
       ELSE
          oCol:colSep  := IIF( oCol:colSep <> NIL, oCol:colSep, ::colSep )
       ENDIF

   NEXT
   ::nHeadersHeight := nMaxHeight
   __OutDebug( "ConfigureHeaders() - ::nHeadersHeight", ::nHeadersHeight )

   // Now i can align headers
   FOR EACH oCol IN ::aColumnsCloned
       // Headers are aligned from bottom to top
       nTokens      := __StrTokenCount( oCol:Heading, ";" )
       oCol:Heading := Replicate( ";", ::nHeadersHeight - nTokens + 1 ) + oCol:Heading
       __OutDebug( "oCol:Heading", oCol:Heading )
   NEXT

RETURN Self

METHOD ConfigureFooters() CLASS TBrowse
   LOCAL oCol, nMaxHeight := 0
   LOCAL nTokens

   ::lFooters := .F.

   // Are there column footers to paint ?
   FOR EACH oCol IN ::aColumnsCloned
       IF !::lFooters .AND. !Empty( oCol:footing )
          ::lFooters := .T.
          nMaxHeight := 0
       ENDIF

       // Here i determine how is high max header
       nTokens      := __StrTokenCount( oCol:footing, ";" )
       nMaxHeight := Max( nMaxHeight, nTokens )

       // Put the footSep in cloned columns
       oCol:footSep := IIF( oCol:footSep <> NIL, oCol:footSep, ::footSep )
       // Check if there are footSep
       IF !Empty( oCol:footSep )
          ::lFooterSep := .T.
       ENDIF
   NEXT
   ::nFootersHeight := nMaxHeight
   __OutDebug( "ConfigureFooters() - ::nFootersHeight", ::nFootersHeight )

   // Now i can align footers
   FOR EACH oCol IN ::aColumnsCloned
       // Footers are aligned from top to bottom
       nTokens      := __StrTokenCount( oCol:footing, ";" )
       oCol:footing := oCol:footing + Replicate( ";", ::nFootersHeight - nTokens + 1 )
   NEXT

RETURN Self

METHOD CalculateColumnsWidth() CLASS TBrowse
   LOCAL oCol, nLenHeader, nLenFooter, nLenData, nTokenPos, nL, cHeading, cFooting, xValue
   LOCAL nTokens, n, nWidth

   ::nVSBlockSize := 0 // Max row width

   FOR EACH oCol IN ::aColumnsCloned

       IF oCol:width == NIL  // IF <> NIL it's already ok because are user defined

          nLenHeader := 0
          nLenFooter := 0
          nLenData   := 0

          // First calculate lenght of headers
          IF ::lHeaders
             nTokenPos := 0
             cHeading  := oCol:heading
             nTokens   := __StrTokenCount( cHeading, ";" )
             FOR n := 1 TO nTokens
                 nL := Len( __StrToken(@cHeading, n, ";") )
                 nLenHeader := Max( nLenHeader, nL )
             NEXT
          ENDIF

          // Second calculate lenght of footers
          IF ::lFooters
             nTokenPos := 0
             cFooting  := oCol:Footing + ";"
             nTokens   := __StrTokenCount( cFooting, ";" )
             FOR n := 1 TO nTokens
                 nL := Len( __StrToken(@cFooting, n, ";") )
                nLenFooter := Max( nLenFooter, nL )
             NEXT
          ENDIF

          // Third eval column data to take lenght after transform it to a string
          xValue   := Eval( ::GetColumn( HB_EnumIndex() ):block )
          nLenData := Len( Transform( xValue, ::GetColumn( HB_EnumIndex() ):picture ) )

          // Calc max value and store in width property
          oCol:width := Max( nLenData, Max( nLenHeader, nLenFooter ) )

       ENDIF

       // Store in cargo object of cloned columns the type of original value
       //oCol:cargo:cType := ValType( xValue )

       // Add total width
       nWidth := 0
       IF HB_EnumIndex() == 1
          // As Column separator are displayed at the left of the column it is NOT added to the first column.
          nWidth := oCol:width
       ELSE
          nWidth := Max( Len( ::colSep ), Len( oCol:colSep ) ) + oCol:width
       ENDIF

       // Put in cargo the full width of column (with colSep)
       oCol:nFullWidth := nWidth
       // Save the offset
       oCol:nOffset    := ::nVSBlockSize

       ::nVSBlockSize += nWidth
   NEXT

RETURN Self

// Calculate how many columns fit on the browse width including ColSeps
METHOD CalculateVisibleColumns() CLASS TBrowse
   LOCAL nMaxWidth := ::BrowseWidth(), nWidth := 0
   LOCAL oCol, n, nVisibleColumns := 0, aCol
   LOCAL nFreezeColsWidth := 0, nVisibleColsWidth := 0

   //::aVisibleColumns := {}

   // First look at frozen columns
   FOR n := 1 TO ::freeze  // If ::freeze == 0 this FOR ... NEXT will not be evaluated
       oCol := ::aColumnsCloned[ n ]
       nWidth := oCol:width + Len( IIF( n > 1, IIF( oCol:colSep <> NIL, oCol:colSep, ::colSep ), "" ) )

       IF nFreezeColsWidth + nWidth <= nMaxWidth
          nFreezeColsWidth += nWidth
          //aAdd( ::aVisibleColumns, { n, nWidth } )
       ELSE
          EXIT
       ENDIF
   NEXT

   //tracelog( "freeze, nFreezeColsWidth, nWidth", ::freeze, nFreezeColsWidth, nWidth )

   IF n <= ::freeze  // here n must to be ::freeze + 1, if this not happens it means that
                     // the width of frozen cols is larger than browse width
                     // so freeze not permited, reset to 0

      ::Ffreeze           := 0
      ::FleftVisible      := 1
      //::FrightVisible := n
      nFreezeColsWidth    := 0
      //::aVisibleColumns := {}
   ELSE
      // if we are in configuration mode i have to define which is lef column
      IF ::FleftVisible == NIL
         ::FleftVisible  := n
         IF ::FleftVisible > ::FcolCount
            ::FleftVisible := ::FcolCount
         ENDIF
      ENDIF
   ENDIF

   // Then calculate width until max width of browse
   //TraceLog( "::FleftVisible, ::FcolCount", ::FleftVisible, ::FcolCount )
   FOR n := ::FleftVisible TO ::FcolCount
       oCol := ::aColumnsCloned[ n ]

       nWidth := oCol:width + Len( IIF( n > 1, IIF( oCol:colSep <> NIL, oCol:colSep, ::colSep ), "" ) )
       //oCol:cargo:nWidth := nWidth

       //tracelog( "columns: col:width, nWidth, nMaxWidth, nTotalWidth", oCol:width, nWidth, nMaxWidth, nTotalWidth )
       //TraceLog( "nFreezeColsWidth, nVisibleColsWidth, nWidth, nMaxWidth, nFreezeColsWidth + nVisibleColsWidth + nWidth", nFreezeColsWidth, nVisibleColsWidth, nWidth, nMaxWidth, nFreezeColsWidth + nVisibleColsWidth + nWidth )
       IF nFreezeColsWidth + nVisibleColsWidth + nWidth <= nMaxWidth
          // Found a visible column
          nVisibleColsWidth += nWidth
          ::FrightVisible := n

       ELSE
          // We are out of range, but before exit we have to check for padding
          //IF ::freeze > 0   // Pad Right
          //   IF nTotalWidth < nMaxWidth
          //      aCol := ATail( ::aVisibleColumns )
          //      aCol[ COL_WIDTH ] += ( nMaxWidth - nTotalWidth )
          //    ENDIF
          //ELSE              // Centered
          //   IF nFreezeColsWidth + nVisibleColsWidth < nMaxWidth
          //      aCol := ::aVisibleColumns[ 1 ]
          //      aCol[ COL_WIDTH ] += Int( ( nMaxWidth - nTotalWidth ) / 2 )
          //      aCol := ATail( ::aVisibleColumns )
          //      aCol[ COL_WIDTH ] += ( nMaxWidth - nTotalWidth ) - Int( ( nMaxWidth - nTotalWidth ) / 2 )
          //    ENDIF
          //ENDIF
          EXIT
       ENDIF
   NEXT
   ::nVSFreezeSize     := nFreezeColsWidth
   ::nVisibleColsWidth := nVisibleColsWidth
   //::nColsWidth := nTotalWidth

   //TraceLog( "::aVisibleColumns", hb_dumpvar( ::aVisibleColumns ) )

RETURN Self


// Gets TBrowse width and width of displayed columns plus colsep
METHOD RedrawHeaders() CLASS TBrowse
   LOCAL nRow, cStr

   IF ::lRedrawFrame
      DispBegin()

      IF ::lHeaders
         // Drawing headers
         //TraceLog( "::nVSDataFirstRow", ::nVSDataFirstRow )
         FOR nRow := 0 TO IIF( ::nVSHeadSepRow <> -1, ::nVSHeadSepRow, ::nVSDataFirstRow - 1 )
             cStr := PadR( ::VSGetVisiblePart( nRow ), ::BrowseWidth() )
             __OutDebug( "nRow, cStr", nRow, hb_OemToAnsi( cStr ) )
             ::VSDisplayRow( nRow, cStr )
         NEXT
      ENDIF

      ////Draw horizontal heading separator line
      IF ::lHeaderSep
         nRow := ::nVSHeadSepRow
         //cStr := ::VSGetVisiblePart( nRow )
         __OutDebug( "::VSGetFreezePart( nRow ), ::VSGetUnFrzVisPart( nRow )", hb_OemToAnsi( ::VSGetFreezePart( nRow ) ), hb_OemToAnsi(::VSGetUnFrzVisPart( nRow ) ) )
         IF ::Ffreeze > 0
            cStr := PadR( ::VSGetFreezePart( nRow ) + Left( ::VSGetUnFrzVisPart( nRow ), 1 ) + Replicate( Substr( ::VSGetUnFrzVisPart( nRow ), 2, 1 ), ::nVSSpaceLeft ) +;
                          Substr( ::VSGetUnFrzVisPart( nRow ), 2 ), ::BrowseWidth(), Right( ::VSGetUnFrzVisPart( nRow ), 1 ) )
         ELSE
            cStr := PadR( ::VSGetFreezePart( nRow ) + Replicate( Left( ::VSGetUnFrzVisPart( nRow ), 1 ), ::nVSSpaceLeft ) +;
                          ::VSGetUnFrzVisPart( nRow ), ::BrowseWidth(), Right( ::VSGetUnFrzVisPart( nRow ), 1 ) )
         ENDIF
         ::VSDisplayRow( nRow, cStr )
      ENDIF

      //Draw horizontal footing separator line
      IF ::lFooterSep
         nRow := ::nVSFootSepRow
         //cStr := ::VSGetVisiblePart( nRow )
         IF ::Ffreeze > 0
            cStr := PadR( ::VSGetFreezePart( nRow ) + Left( ::VSGetUnFrzVisPart( nRow ), 1 ) + Replicate( Substr( ::VSGetUnFrzVisPart( nRow ), 2, 1 ), ::nVSSpaceLeft ) +;
                          Substr( ::VSGetUnFrzVisPart( nRow ), 2 ), ::BrowseWidth(), Right( ::VSGetUnFrzVisPart( nRow ), 1 ) )
         ELSE
            cStr := PadR( ::VSGetFreezePart( nRow ) + Replicate( Left( ::VSGetUnFrzVisPart( nRow ), 1 ), ::nVSSpaceLeft ) +;
                          ::VSGetUnFrzVisPart( nRow ), ::BrowseWidth(), Right( ::VSGetUnFrzVisPart( nRow ), 1 ) )
         ENDIF
         ::VSDisplayRow( nRow, cStr )
      ENDIF

      IF ::lFooters
         // Drawing headers
         FOR nRow := ::nVSFootFirstRow TO ::nVSLastRowOfData
             cStr := PadR( ::VSGetVisiblePart( nRow ), ::BrowseWidth() )
             ::VSDisplayRow( nRow, cStr )
         NEXT
      ENDIF
      DispEnd()
      ::lRedrawFrame := .F.
   ENDIF

RETURN Self

METHOD VSDisplayRow( nRow, cString )
   SetPos( ::FnTop + nRow - iif(::lHeaders, 0, 1 ), ::FnLeft )
   DispOut( PadR( cString, ::BrowseWidth() ), ::ColorSpec )
RETURN Self

METHOD VSDisplayDataRow( nLine )
   LOCAL cString
   LOCAL nRow := nLine - 1 + ::nVSDataFirstRow
   IF nRow >= ::nVSDataFirstRow .AND. nRow <= ::nVSDataLastRow
      cString := ::VSGetVisiblePart( nRow )
   ELSE //IF nRow < ::FrowCount
      cString := ::VSGetVisiblePart( ::VSGhostRow() )
   ENDIF
   ::VSDisplayRow( nRow, cString )
RETURN Self

// Display data
STATIC PROCEDURE DisplayRowData( Self, oCol, nRow, xValue, nColor )
   LOCAL cStr
   DEFAULT nColor TO CLR_STANDARD

   //TraceLog( "DisplayRowData: Self, oCol, nRow, xValue ", Self, oCol, nRow, xValue )
   SetPos( ::FnTop + iif(::lHeaders, ::nHeadersHeight, -1 ) + nRow, oCol:nScreenCol ) // [ COLUMN_COL ] )
      //cStr := Space( nSpaceLeft ) + PadR( cValue,  oCol:width + nSpaceRight ) + cColSep
   cStr := Eval( oCol:bDisplay, xValue )
   DispOut( cStr, hb_ColorIndex( ::ColorSpec, nColor ) ) // FSG - TODO - impostare il colore della colonna

RETURN

STATIC FUNCTION DisplayByType( xValue, oCol )
   LOCAL cStr := ""
   LOCAL cType := oCol:cargo:cType //[ COLUMN_TYPE ]
   //TraceLog( "DisplayByType: xValue, oCol, cType, oCol:cargo", xValue, oCol, cType, hb_dumpvar( oCol:cargo ) )
   DO CASE
      CASE cType == "L"
           cStr := PadC( xValue, oCol:width )
      CASE cType == "N"
           cStr := PadL( xValue, oCol:width )
      OTHERWISE
           cStr := PadR( xValue, oCol:width )
   ENDCASE
RETURN cStr

METHOD DisplayCell( nRow, nCol, nColor )
   LOCAL oCol, nVisCol


   //__OutDebug( nRow, nCol, nColor )
   // First frozen columns

       oCol    := ::aColumnsCloned[ nCol ]
       //SetPos( ::FnTop + nRow - 1 + ::nVSDataFirstRow, ;
       //        ::FnLeft + ::VSGetOffSet( nCol ) - ::VSGetOffSet( ::FleftVisible ) - Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ) + ;
       //        ::nVSSpaceLeft + IIF( nCol == 1, 0, -( IIF( nCol == ::FleftVisible, -Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ), 1 ) ) ) )

       IF ::Ffreeze > 0 .AND. nCol <= ::Ffreeze

          SetPos( ::FnTop + nRow - 1 - iif(::lHeaders, 0, 1 ) + ::nVSDataFirstRow, ;
                  ::FnLeft + ::VSGetOffSet( nCol ) - IIF( nCol > 1, 1, 0 ) ; //- ::VSGetOffSet( ::FleftVisible ) - ; //- Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ) + ;
                    ; //IIF( ::FleftVisible == 1 .AND. nCol > 1, 1, 0 ) ;
                )

       ELSE

          SetPos( ::FnTop + nRow - 1 - iif(::lHeaders, 0, 1 ) + ::nVSDataFirstRow, ;
                  ::FnLeft + ::nVSFreezeSize + ::nVSSpaceLeft + IIF( nCol > ::Ffreeze, ::nVSLenLeftSep, 0 ) + ::VSGetOffSet( nCol ) - ::VSGetOffSet( ::FleftVisible ) - ; //- Len( ::aColumnsCloned[ ::FleftVisible ]:colSep ) + ;
                   IIF( ::Ffreeze > 0 .OR. nCol == 1, 0, 1 ) ;
                )
       ENDIF

       __OutDebug( "nCol, ::FleftVisible, ::FnLeft, ::VSGetOffSet( nCol ), ::VSGetOffSet( ::FleftVisible ), ::nVSSpaceLeft, Len( ::aCC[ ::FleftVisible ]:colSep )", ;
                    nCol, ::FleftVisible, ::FnLeft, ::VSGetOffSet( nCol ), ::VSGetOffSet( ::FleftVisible ), ::nVSSpaceLeft, Len( ::aColumnsCloned[ ::FleftVisible ]:colSep )  )

       //DispOut( Substr( ::VSGetRow( nRow - 1 + ::nVSDataFirstRow ), ::VSGetOffSet( nCol ) + ;
       //                 IIF( nCol > 1 .AND. nCol == ::FleftVisible, ;
       //                      Len( ::aColumnsCloned[ ::FleftVisible ]:colSep )+1, ;
       //                      0 ),;
       //                 oCol:width ),;
       //                 hb_ColorIndex( ::ColorSpec, nColor ) )
       //

       DispOut( Substr( ::VSGetRow( nRow - 1 + ::nVSDataFirstRow ), ::VSGetOffSet( nCol ) + ;
                        0,;
                        oCol:width ),;
                        hb_ColorIndex( ::ColorSpec, nColor ) )

       __OutDebug( "Cell: ", Substr( ::VSGetRow( nRow - 1 + ::nVSDataFirstRow ), ::VSGetOffSet( nCol ) + ;
                        0,;
                        oCol:width ),;
                        hb_ColorIndex( ::ColorSpec, nColor ) )


       //IF nCol == ::FleftVisible .OR. nCol <= ::Ffreeze
       //   SetPos( ::FnTop + nRow - 1 + ::nVSDataFirstRow, ::FnLeft + ::VSGetOffSet( nCol ) - ::VSGetOffSet( ::FleftVisible ) )
       //   DispOut( Space( ::nVSSpaceLeft ) + Substr( ::VSGetRow( nRow - 1 + ::nVSDataFirstRow ), ::VSGetOffSet( nCol ), oCol:width ), hb_ColorIndex( ::ColorSpec, nColor ) )
       //ELSEIF nCol > ::FleftVisible .AND. nCol < ::FrightVisible
       //   SetPos( ::FnTop + nRow - 1 + ::nVSDataFirstRow, ::FnLeft + ::VSGetOffSet( nCol ) - ::VSGetOffSet( ::FleftVisible ) + ::nVSSpaceLeft - 1 )
       //   DispOut( Substr( ::VSGetRow( nRow - 1 + ::nVSDataFirstRow ), ::VSGetOffSet( nCol ), oCol:width ), hb_ColorIndex( ::ColorSpec, nColor ) )
       //ELSE
       //   SetPos( ::FnTop + nRow - 1 + ::nVSDataFirstRow, ::FnLeft + ::VSGetOffSet( nCol ) - ::VSGetOffSet( ::FleftVisible ) + ::nVSSpaceLeft - 1 )
       //   DispOut( Substr( ::VSGetRow( nRow - 1 + ::nVSDataFirstRow ), ::VSGetOffSet( nCol ), oCol:width ) + Space( ::nVSSpaceRight ) , hb_ColorIndex( ::ColorSpec, nColor ) )
       //ENDIF




RETURN Self

// ------------------- END   INTERNAL CONFIGURATION METHODS -------------------------------


// ------------------- BEGIN PUBLIC MISCELLANEOUS METHODS   -------------------------------


// Forces redraw during next stabilization
//
//     invalidate() --> self
//
//        TBrowse:invalidate() causes the next stabilization of the TBrowse
//        object to redraw the entire TBrowse display, including headings,
//        footings, and all data rows.  Note that sending this message has no
//        effect on the values in the data rows; it simply forces the display to be
//        updated during the next stabilization.  To force the data to be
//        refreshed from the underlying data source, send the
//        TBrowse:refreshAll() message.
//
METHOD Invalidate() CLASS TBrowse

   ::lInvalidate  := .T.
   ::lRedrawFrame := .T.
   //aEval( ::aVisibleRows, {|e| e[ROW_DISPLAYED] := .F. } )
   ::stable       := .F.

RETURN Self

// Causes all data to be refreshed during the next stabilize
//
//     refreshAll() --> self
//
//        Internally marks all data rows as invalid, causing them to be
//        refilled and redisplayed during the next stabilize loop.
//
METHOD RefreshAll() CLASS TBrowse

   ::lRefreshAll := .T.
   //aEval( ::aVisibleRows, {|e| e[ROW_UPDATED] := .F. } )
   ::stable := .F.

RETURN Self

// Causes the current row to be refreshed on next stabilize
//
//     refreshCurrent() --> self
//
//        Internally marks the current data row as invalid, causing it to be
//        refilled and redisplayed during the next stabilize loop.
//
METHOD RefreshCurrent() CLASS TBrowse

   ::lRefreshCurrent := .T.
   //::aVisibleRows[ ::RowPos ][ROW_UPDATED] := .F.
   ::stable := .F.

RETURN Self

// Reconfigures the internal settings of the TBrowse object
//
//     configure() --> self
//
//        Causes the TBrowse object to reexamine all instance variables and
//        TBColumn objects, reconfiguring its internal settings as required.
//        This message can force reconfiguration when a TBColumn object is
//        modified directly.
METHOD Configure() CLASS TBrowse

   ::lConfigured := .F.

RETURN Self

// Adds a TBColumn object to the TBrowse object
//
//     addColumn(<objColumn>) --> self
//
//        Adds a new TBColumn object to the TBrowse object and TBrowse:colCount
//        is increased by one.
//
METHOD AddColumn( oCol ) CLASS TBrowse

   AAdd( ::aColumns, oCol )
   ::FcolCount++
   ::lConfigured := .F.

RETURN Self


// Insert a column object in a browse
//
//     insColumn(<nPos>, <objColumn>) --> objColumn
//
//        This method allows a column object to be inserted into the middle of
//        a browse.  The return value is a reference to the column object being
//        inserted.
//
// NOTE: no range check performed intentionally so bad parameters raise an error
//
METHOD InsColumn( nPos, oCol ) CLASS TBrowse

   ASize( ::aColumns, ++::FcolCount)
   AIns( ::aColumns, nPos )
   ::aColumns[ nPos ] := oCol
   ::lConfigured := .F.

RETURN oCol

// Delete a column object from a browse
//
//     delColumn(<nPos>) --> objColumn
//
//        This new method allows a column to be deleted from a browse.  The
//        return value is a reference to the column object being deleted so
//        that the column object may be preserved.
//
// NOTE: no range check performed intentionally so a bad parameter raise an error
//
METHOD DelColumn( nPos ) CLASS TBrowse

   LOCAL oCol := ::aColumns[ nPos ]
   LOCAL n

   ADel( ::aColumns, nPos )
   ASize( ::aColumns, --::FcolCount)
   ::lConfigured := .F.

   // ADel( ::aColsWidth, nPos)
   // ASize( ::aColsWidth, ::FcolCount)
   //
   // ::Configure( 2 )

RETURN oCol

// Gets a specific TBColumn object
//
//     getColumn(<nColumn>) --> objColumn
//
//        Returns the TBColumn object specified by <nColumn>.
//
// NOTE: no range check performed intentionally so a bad parameter raise an error
//
METHOD GetColumn( nColumn ) CLASS TBrowse
RETURN ::aColumns[ nColumn ]

// Replaces one TBColumn object with another
//
//     refreshCurrent() --> self
//
//        Internally marks the current data row as invalid, causing it to be
//        refilled and redisplayed during the next stabilize loop.
//
// NOTE: no range check performed intentionally so a bad parameter raise an error
//
METHOD SetColumn( nColumn, oCol ) CLASS TBrowse
   LOCAL oOldCol := ::aColumns[ nColumn ]

   ::aColumns[ nColumn ] := oCol
   ::lConfigured  := .F.

RETURN oOldCol

// Alters the color of a rectangular group of cells
//
//     colorRect(<aRect>, <aColors>) --> self
//
//        Directly alters the color of a rectangular group of cells.  <aRect>
//        is an array of four numbers (top, left, bottom, and right).  The
//        numbers refer to cells within the data area of the browse display,
//        not to screen coordinates.  <aColors> is an array of two numbers.
//        The numbers are used as indexes into the color table for the browse.
//        These colors will become the normal and highlighted colors for the
//        cells within the specified rectangle.
//
//        Cells that are colored using colorRect retain their color until they
//        are scrolled off the screen up or down.  Horizontal panning has no
//        affect on these colors and, in fact, cells that are currently off
//        screen left or right can be colored even if they are not visible.
//
//        This example colors the entire virtual window (on and off screen):
//
//        aRect := {1, 1, browse:rowCount, browse:colCount}
//        browse:colorRect( aRect, {2, 1} )

// TODO: fix to work
METHOD ColorRect( aRect, aRectColor ) CLASS TBrowse

   ::aRect       := aRect
   ::aRectColor  := aRectColor

RETURN Self

// Returns the display width of a particular column
//
//     colWidth(<nColumn>) --> nWidth
//
//        Returns the display width of column number <nColumn> as known to the
//        browse.  If <nColumn> is out of bounds or not supplied or not a
//        number, the method returns zero.
METHOD ColWidth( nColumn ) CLASS TBrowse
  IF !::lConfigured
     ::DoConfiguration()
  ENDIF
RETURN IIF( ValType( nColumn ) == "N" .AND. 0 < nColumn .AND. nColumn <= ::FcolCount, ;
            ::aColumnsCloned[ nColumn ]:width, 0 )

// Dehighlights the current cell
//
//     deHilite() --> self
//
//        Causes the current cell (the cell to which the browse cursor is
//        positioned) to be dehighlighted.  This method is designed for use
//        when TBrowse:autoLite is set to false (.F.).
//
METHOD DeHilite() CLASS TBrowse
   //TraceLog( "DeHilite()" )
   IF ::lHilited
      ::DisplayCell( ::RowPos, ::ColPos, CLR_STANDARD )
      ::lHilited := FALSE
   ENDIF
   //LOCAL nRow := ::FnTop + ::RowPos + iif(::lHeaders, ::nHeadersHeight, 0 ) + iif(Empty(::HeadSep), 0, 1) - 1
   //LOCAL cType
   //
   //SetPos( nRow, ::aColumns[ ::ColPos ]:ColPos )
   //
   //cType := ::DispCell(::ColPos, CLR_STANDARD)
   //
   //SetPos(nRow, ::aColumns[ ::ColPos ]:ColPos + iif(cType == "L", ::aColsWidth[::ColPos] / 2, 0 ))

RETURN Self

// Performs a full stabilization
//
//     forceStable()
//
//        Performs a full stabilization of the TBrowse.  It is analogous to the
//        following code, only slightly faster:
//
//        DO WHILE .NOT. oBrowse:stabilize()
//        ENDDO
//
METHOD ForceStable() CLASS TBrowse
   //TraceLog( "ForceStable()" )
   //::RefreshAll()
   do while !::Stabilize()
   end

RETURN Self

// Highlights the current cell
//
//     hilite() --> self
//
//        Causes the current cell (the cell to which the browse cursor is
//        positioned) to be highlighted.  This method is designed for use when
//        TBrowse:autoLite is set to false (.F.).
//
METHOD Hilite() CLASS TBrowse
  //TraceLog( "Hilite()" )
   IF !::lHilited
      ::DisplayCell( ::RowPos, ::ColPos, CLR_ENHANCED )
      ::lHilited := TRUE
   ENDIF
//   LOCAL nRow, nCol
//   LOCAL cType
//
//   nRow := ::FnTop + ::RowPos + iif(::lHeaders, ::nHeadersHeight, 0) + iif(Empty(::HeadSep), 0, 1) - 1
//   nCol := ::aColumns[ ::ColPos ]:ColPos
//
//   // Start of cell
//   SetPos( nRow, nCol)
//
//   cType := ::DispCell(::ColPos, CLR_ENHANCED)
//   nCol  += iif(cType == "L", ::aColsWidth[::ColPos] / 2, 0 )
//
//   // Put cursor back on first char of cell value
//   SetPos(nRow, nCol)
//
// #ifdef HB_COMPAT_C53
//   ::nRow := nRow
//   ::nCol := nCol
// #endif

RETURN Self

// Performs incremental stabilization
//
//     stabilize() --> lStable
//
//        Performs incremental stabilization.  Each time this message is sent,
//        some part of the stabilization process is performed.  Stabilization
//        is performed in increments so that it can be interrupted by a
//        keystroke or other asynchronous event.
//
//        If the TBrowse object is already stable, a value of true (.T.) is
//        returned.  Otherwise, a value of false (.F.) is returned indicating
//        that further stabilize messages should be sent.  The browse is
//        considered stable when all data has been retrieved and displayed, the
//        data source has been repositioned to the record corresponding to the
//        browse cursor, and the current cell has been highlighted.
//
METHOD Stabilize() CLASS TBrowse

   LOCAL nRow, n, nEmptyRow, nOldRow
   LOCAL nWidth := ::FnRight - ::FnLeft + 1 // Visible width of the browse
   LOCAL cColColor                        // Column color to use
   LOCAL oStartCol, oEndCol
   LOCAL lDisplay                      // Is there something to show inside current cell?
   LOCAL nRecsSkipped                  // How many records do I really skipped?
   LOCAL nFirstRow                     // Where is on screen first row of TBrowse?
   LOCAL nOldCursor                    // Current shape of cursor (which I remove before stabilization)
   LOCAL aRow
   LOCAL nCol, oCol, nToSkip, nSkipped, nTotalSkipped
   LOCAL nPadLeft, nPadRight, nSpaceLeft, nSpaceRight
   LOCAL nMaxWidth := ::BrowseWidth()
   LOCAL cColSep, oCargo

   //TraceLog( "Stabilize()" )
   IF !::lConfigured
      ::DoConfiguration()
   ENDIF

   nOldCursor := SetCursor(SC_NONE)

   //TraceLog( "Stabilize() - ::lRedrawFrame", ::lRedrawFrame )
   IF ::lRedrawFrame
      ::VSCalcVisColumns()
      //::CalculateVisibleColumns()

      ::RedrawHeaders()

      // Now that browser frame has been redrawn we don't need to redraw it unless
      // displayed columns change
      ::lRedrawFrame := .F.

   ENDIF

   // From this point there is stabilization of rows which is made up of three phases
   // 1st repositioning of data source
   // 2nd redrawing of rows, after each row we exit stabilization loop with .F.
   // 3rd if all rows have been redrawn we set ::stable state to .T.

   //TraceLog( "Stabilize() - ::stable", ::stable )
   IF !::stable
      IF ::lHilited
         ::DeHilite()
      ENDIF
      IF ::nSkipped <> 0

         // Move or Clear rows
         //__OutDebug( "::nSkipped, ::rowCount, ::rowPos", ::nSkipped, ::rowCount, ::rowPos )
         //__OutDebug( "::nSkipped > ::rowCount, ::rowPos + ::nSkipped - ::rowCount <= ::rowCount", ::nSkipped > ::rowCount, ::rowPos + ::nSkipped - ::rowCount <= ::rowCount )
         IF ::nSkipped > ::rowCount .AND. ::rowPos + ::nSkipped - ::rowCount <= ::rowCount
            // scroll
            //__OutDebug( "Scroll 1: ::rowPos + ::nSkipped - ::rowCount", ::rowPos + ::nSkipped - ::rowCount )
            //__OutDebug( "Scroll( ::FnTop + IIF( ::lHeaders, ::nHeadersHeight, 0 ) + IIF( ::lHeaderSep, 1, 0 ), ::FnLeft, ::FnBottom - IIF( ::lFooters, ::nFootersHeight, 0 ) - IIF( ::lFooterSep, 1, 0 ), ::FnRight, ::rowPos + ::nSkipped - ::rowCount )", ::FnTop + IIF( ::lHeaders, ::nHeadersHeight, 0 ) + IIF( ::lHeaderSep, 1, 0 ), ::FnLeft, ::FnBottom - IIF( ::lFooters, ::nFootersHeight, 0 ) - IIF( ::lFooterSep, 1, 0 ), ::FnRight, ::rowPos + ::nSkipped - ::rowCount )
            Scroll( ::FnTop + IIF( ::lHeaders, ::nHeadersHeight, 0 ) + IIF( ::lHeaderSep, 1, 0 ), ::FnLeft, ::FnBottom - IIF( ::lFooters, ::nFootersHeight, 0 ) - IIF( ::lFooterSep, 1, 0 ), ::FnRight, ::rowPos + ::nSkipped - ::rowCount )
         ELSEIF ::nSkipped < -::rowCount .AND. ::rowPos - ( ::rowCount + ::nSkipped ) >= 1
            Scroll( ::FnTop + IIF( ::lHeaders, ::nHeadersHeight, 0 ) + IIF( ::lHeaderSep, 1, 0 ), ::FnLeft, ::FnBottom - IIF( ::lFooters, ::nFootersHeight, 0 ) - IIF( ::lFooterSep, 1, 0 ), ::FnRight, ::rowPos + ::nSkipped - 1 )
         ELSEIF     ::nSkipped == 1 .AND. ::rowPos == ::rowCount .OR. ;
                ::nSkipped == -1 .AND. ::rowPos == 1  //        .OR. ;
                //Abs( ::nSkipped ) > ::rowCount
                Scroll( ::FnTop + IIF( ::lHeaders, ::nHeadersHeight, 0 ) + IIF( ::lHeaderSep, 1, 0 ), ::FnLeft, ::FnBottom - IIF( ::lFooters, ::nFootersHeight, 0 ) - IIF( ::lFooterSep, 1, 0 ), ::FnRight, ::nSkipped )
         ENDIF

      ENDIF

      IF ::lRefreshAll

         IF ::nSkipped <> 0 .AND. ;
            ( ( ::nSkipped < 0 .AND. ::rowPos + ::nSkipped >= 1 )  .OR. ;
              ( ::nSkipped > 0 .AND. ::rowPos + ::nSkipped <= ::rowCount ) )
            ::rowPos += ::nSkipped
         ELSEIF ::lVSMoveDown
            nTotalSkipped := 0
            nOldRow  := ::RowPos
            //nToSkip  := 1 - ::rowPos
            //nSkipped := Eval( ::skipBlock, nToSkip )
            nToSkip  := 1
            //__OutDebug( "::RowPos, ::VSDataRows(), ::nVSDataLastRow, ::nVSDataFirstRow, ::rowCount", ::RowPos, ::VSDataRows(), ::nVSDataLastRow, ::nVSDataFirstRow, ::rowCount )
            FOR nRow := 1 TO ::rowCount
                ::RowPos := nRow

                //__OutDebug( "::RowPos, ::VSDataRows(), ::nVSDataLastRow, ::nVSDataFirstRow, ::rowCount", ::RowPos, ::VSDataRows(), ::nVSDataLastRow, ::nVSDataFirstRow, ::rowCount )

                ::VSBuildRow( nRow )
                ::VSDisplayDataRow( nRow )
                // to next row
                nSkipped := Eval( ::skipBlock, nToSkip )

                //__OutDebug( "nToSkip, nSkipped", nToSkip, nSkipped )
                IF nSkipped <> nToSkip
                   EXIT
                ELSE
                   nTotalSkipped++
                ENDIF
                nToSkip  := 1
            NEXT
            // Display empty lines
            FOR nEmptyRow := nRow+1 TO ::rowCount
                ::RowPos := nEmptyRow
                ::VSBuildEmptyRow( nEmptyRow )
                ::VSDisplayDataRow( nEmptyRow )
            NEXT
            nToSkip  := nOldRow - ( nTotalSkipped + 1 )
            //__OutDebug( "nToSkip, ::rowPos, nTotalSkipped", nToSkip, ::rowPos, nTotalSkipped )
            nSkipped := Eval( ::skipBlock, nToSkip )
            //__OutDebug( "nSkipped", nSkipped )
            ::RowPos := nOldRow

         ELSE
            nTotalSkipped := 0
            nOldRow  := ::RowPos
            //nToSkip  := ::rowCount - ::rowPos
            //nSkipped := Eval( ::skipBlock, nToSkip )
            nToSkip  := 0
            //__OutDebug( "::RowPos, ::VSDataRows(), ::nVSDataLastRow, ::nVSDataFirstRow, ::rowCount", ::RowPos, ::VSDataRows(), ::nVSDataLastRow, ::nVSDataFirstRow, ::rowCount )
            FOR nRow := ::rowCount TO 1 STEP -1
                ::RowPos := nRow

                //__OutDebug( "::RowPos, ::VSDataRows(), ::nVSDataLastRow, ::nVSDataFirstRow, ::rowCount", ::RowPos, ::VSDataRows(), ::nVSDataLastRow, ::nVSDataFirstRow, ::rowCount )

                // to next row
                nSkipped := Eval( ::skipBlock, nToSkip )
                ::VSBuildRow( nRow )
                ::VSDisplayDataRow( nRow )

                //__OutDebug( "nToSkip, nSkipped", nToSkip, nSkipped )
                IF nSkipped <> nToSkip
                   EXIT
                ELSE
                   nTotalSkipped--
                ENDIF
                nToSkip  := -1
            NEXT
            // Display empty lines
            FOR nEmptyRow := nRow-1 TO 1 STEP -1
                ::RowPos := nEmptyRow
                ::VSBuildEmptyRow( nEmptyRow )
                ::VSDisplayDataRow( nEmptyRow )
            NEXT
            nToSkip  := nOldRow - ( nRow + 1 )//nTotalSkipped
            //__OutDebug( "nToSkip, ::rowPos, nTotalSkipped", nToSkip, ::rowPos, nTotalSkipped )
            nSkipped := Eval( ::skipBlock, nToSkip )
            //__OutDebug( "nSkipped", nSkipped )
            ::RowPos := nOldRow

         ENDIF
         ::lRefreshAll := .F.
      ELSEIF ::lRefreshCurrent

         //__OutDebug( "::nSkipped, ::rowPos", ::nSkipped, ::rowPos )
         IF Abs( ::nSkipped ) > 0
            ::rowPos += ::nSkipped
            IF ::rowPos < 1
               ::rowPos := 1
            ELSEIF ::rowPos > ::rowCount
               ::rowPos := ::rowCount
            ENDIF
         ENDIF
         //__OutDebug( "::rowPos", ::rowPos )

         ::VSBuildRow( ::rowPos )
         ::VSDisplayDataRow( ::rowPos )

         ::lRefreshCurrent := .F.

      ELSE //IF ::lDisplayAll

            DispBegin()
            FOR nRow := 1 TO ::rowCount
                ::VSDisplayDataRow( nRow )
            NEXT
            DispEnd()
            ::lDisplayAll := .F.
      ENDIF

      __OutDebug( "Righe in VS:" )

      // header
      FOR nRow := ::nVSHeadFirstRow TO ::nVSHeadSepRow
          __OutDebug( hb_OemToAnsi( ::VSGetRow( nRow ) ) )
      NEXT
      // data
      FOR nRow := ::nVSDataFirstRow TO ::nVSDataLastRow
          __OutDebug( hb_OemToAnsi( ::VSGetRow( nRow ) ) )
      NEXT
      // footer
      FOR nRow := ::nVSFootSepRow TO ::nVSLastRowOfData
          __OutDebug( hb_OemToAnsi( ::VSGetRow( nRow ) ) )
      NEXT

      ::stable := .T.

   ENDIF

   //* NOTE: DBU relies upon current cell being reHilited() even if already stable */
   IF ::AutoLite
      ::Hilite()
   ////else
   ////   ::PosCursor()
   ENDIF

   SetCursor(nOldCursor)

RETURN ::stable

// ------------------- END   PUBLIC MISCELLANEOUS METHODS   -------------------------------


// ------------------- BEGIN MOVEMENT METHODS ---------------------------------------------

// ------- BEGIN CURSOR COLUMN MOVEMENTS --------
// Column movements do not updated data, affect only display

// Moves the cursor to the leftmost visible data column
//
//     home() --> self
//
//        Moves the browse cursor to the leftmost unfrozen column on the
//        display.
//
METHOD Home() CLASS TBrowse

  ::MoveCursorToColumn( ::FleftVisible )

RETURN Self

// Moves the cursor to the rightmost visible data column
//
//     end() --> self
//
//        Moves the browse cursor to the rightmost data column currently
//        visible.
//
METHOD End() CLASS TBrowse

   ::MoveCursorToColumn( ::FrightVisible )

RETURN Self

// Moves the cursor left one column
//
//     left() --> self
//
//        Moves the browse cursor left one data column.  If the cursor is
//        already on the leftmost displayed column, the display is panned and
//        the previous data column (if there is one) is brought into view.
//
METHOD _Left() CLASS TBrowse

   ::MoveCursorToColumn( ::colPos - 1 )

RETURN Self

// Moves the cursor right one column
//
//     right() --> self
//
//        Moves the browse cursor right one data column.  If the cursor is
//        already at the right edge, the display is panned and the next data
//        column (if there is one) is brought into view.
//
METHOD _Right() CLASS TBrowse

   ::MoveCursorToColumn( ::colPos + 1 )

RETURN Self

// Moves the cursor to the leftmost visible data column
//
//     panHome() --> self
//
//        Moves the browse cursor to the leftmost data column, causing the
//        display to be panned all the way to the left.
//
METHOD PanHome() CLASS TBrowse

   ::MoveCursorToColumn( 1 )

RETURN Self

// Moves the cursor to the rightmost data column
//
//     panEnd() --> self
//
//        Moves the browse cursor to the rightmost data column, causing the
//        display to be panned completely to the right.
//
METHOD PanEnd() CLASS TBrowse

   ::MoveCursorToColumn( ::FcolCount )

RETURN Self

// Pans left without changing the cursor position
//
//     panLeft() --> self
//
//        Pans the display without changing the browse cursor, if possible.
//        When the screen is panned with TBrowse:panLeft(), at least one data
//        column out of view to the left is brought into view, while one or
//        more columns are panned off screen to the right.
//
METHOD PanLeft() CLASS TBrowse

   ::PanColumns( -1 )
   // LOCAL n := ::ColPos - ::FleftVisible
   // LOCAL leftVis := ::FleftVisible
   //
   // ::Moved()
   //
   // IF ::FleftVisible > ::Ffreeze + 1
   //    while leftVis == ::FleftVisible
   //       ::FrightVisible--
   //       ::FleftVisible := ::LeftDetermine()
   //    end
   //    ::ColPos := Min( ::FleftVisible + n, ::FrightVisible )
   //    ::lRedrawFrame := .T.
   //    ::RefreshAll()
   // ENDIF

RETURN Self

// Pans right without changing the cursor position
//
//     panRight() --> self
//
//        Pans the display without changing the browse cursor, if possible.
//        When the screen is panned with TBrowse:panRight(), at least one data
//        column out of view to the right is brought into view, while one or
//        more columns are panned off the screen to the left.
//
METHOD PanRight() CLASS TBrowse

   ::PanColumns( 1 )

   // LOCAL n := ::ColPos - ::FleftVisible
   //
   // ::Moved()
   //
   // IF ::FrightVisible < ::FcolCount
   //    ::FrightVisible++
   //    ::FleftVisible := ::LeftDetermine()
   //    ::ColPos := Min( ::FleftVisible + n, ::FrightVisible )
   //    ::lRedrawFrame := .T.
   //    ::RefreshAll()
   // ENDIF

RETURN Self

// **** Internal method, move display cursor between columns ****
METHOD MoveCursorToColumn( nNewCol ) CLASS TBrowse
  LOCAL nCol, nWidth, oCol

  //TraceLog( "MoveCursorToColumn: ::ColPos, nNewCol", ::ColPos, nNewCol )
  // Before check if nNewCol is in range and differs from current colpos
  IF ( 0 < nNewCol .AND. nNewCol <= ::FcolCount ) .AND. nNewCol <> ::colPos

     //TraceLog( "MoveCursorToColumn: ( 0 < nNewCol .AND. nNewCol <= ::FcolCount ), nNewCol <> ::colPos", ( 0 < nNewCol .AND. nNewCol <= ::FcolCount ), nNewCol <> ::colPos )
     // Ok, if user not already dehilite we have to do it
     IF !::lUserDeHilite
        ::deHilite()
     ENDIF

     // Then check if is the new column is already visible
     //__OutDebug( "nNewCol, ::ColPos, ::FleftVisible, ::FrightVisible, ::nVSUnFrzVisPos, ::nVSUnFrzVisLen, ::nVSSpaceLeft, ::nVSSpaceRight, ::freeze", ;
     //             nNewCol, ::ColPos, ::FleftVisible, ::FrightVisible, ::nVSUnFrzVisPos, ::nVSUnFrzVisLen, ::nVSSpaceLeft, ::nVSSpaceRight, ::freeze )

     IF ( 0 < nNewCol .AND. nNewCol <= ::freeze ) .OR. ;
        ( ::FleftVisible <= nNewCol .AND. nNewCol <= ::FrightVisible )

        //TraceLog( "MoveCursorToColumn - in range: ::ColPos, nNewCol", ::ColPos, nNewCol )
        ::ColPos := nNewCol

     ELSE // We have to move away from displayed screen, so invalidate

        IF nNewCol > ::FrightVisible
           nWidth := 0
           FOR nCol := nNewCol TO IIF( ::Ffreeze > 0, ::Ffreeze + 1, 1 ) STEP -1
               oCol := ::aColumnsCloned[ nCol ]
               IF ::nVSFreezeSize + nWidth + oCol:nFullWidth <= ::BrowseWidth()
                  nWidth  += oCol:nFullWidth
               ELSE
                  nCol++
                  EXIT
               ENDIF
           NEXT
           ::FleftVisible  := nCol
           ::FrightVisible := nNewCol
        ELSEIF nNewCol < ::FleftVisible
           nWidth := 0
           FOR nCol := nNewCol TO ::colCount
               oCol := ::aColumnsCloned[ nCol ]
               IF ::nVSFreezeSize + nWidth + oCol:nFullWidth <= ::BrowseWidth()
                  nWidth  += oCol:nFullWidth
               ELSE
                  nCol--
                  EXIT
               ENDIF
           NEXT
           ::FleftVisible  := nNewCol
           ::FrightVisible := nCol
        ENDIF
        //TraceLog( "MoveCursorToColumn - Invalidate: ::ColPos, nNewCol", ::ColPos, nNewCol )
        //__OutDebug( "::ColPos, nNewCol, ::freeze, ::FleftVisible, ::FrightVisible", ::ColPos, nNewCol, ::freeze, ::FleftVisible, ::FrightVisible )
        ::ColPos := nNewCol
        //::VSCalcVisColumns()
        //::invalidate()
        ::lRedrawFrame := TRUE
        ::lDisplayAll  := TRUE
        ::stable       := FALSE

     ENDIF
     //

     // Ok, if user not already dehilite we have to do it
     IF !::lUserDeHilite .AND. ::AutoLite
        ::Hilite()
     ENDIF
  ENDIF
RETURN Self

// ------- END CURSOR COLUMN MOVEMENTS --------

// ------- BEGIN CURSOR ROW MOVEMENTS --------
// row movements updated data when changes rows retrieving data of the new row

//  Moves the cursor up one row
//
//     up() --> self
//
//        Moves the browse cursor up one row.  If the cursor is already on the
//        top data row, the display is scrolled down and a new row is brought
//        into view.  If the data source is already at the logical beginning of
//        file and the browse cursor is already on the top data row,
//        TBrowse:hitTop is set true (.T.).
//
METHOD Up() CLASS TBrowse

   ::MoveRow( -1 )

RETURN Self

// Moves the cursor down one row
//
//     down() --> self
//
//        Moves the browse cursor down one row.  If the cursor is already on
//        the bottom row, the display is scrolled up and a new row is brought
//        into view.  If the data source is already at the logical end of file
//        and the browse cursor is already on the bottom row, TBrowse:hitBottom
//        is set true (.T.).
//
METHOD Down() CLASS TBrowse

   ::MoveRow( 1 )

RETURN Self

// Repositions the data source upward
//
//     pageUp() --> self
//
//        Repositions the data source upward and refills the display.  If the
//        data source is already at logical beginning of file (i.e., the
//        topmost available record is already shown), the browse cursor is
//        simply moved to the top data row.  If the data source is already at
//        logical beginning of file and the browse cursor is already on the
//        first data row, TBrowse:hitTop is set true (.T.).
//
METHOD PageUp() CLASS TBrowse

   // Go to 1st line of previous page with skip, rowpos inaltered
   ::MoveRow( 1 - ::rowPos - ::rowCount )

RETURN Self

// Repositions the data source downward
//
//     pageDown() --> self
//
//        Repositions the data source downward and refills the display.  If the
//        data source is already at the logical end of file (i.e., the
//        bottommost available record is already shown), the browse cursor is
//        simply moved to the lowermost row containing data.  If the data
//        source is already at the logical end of file and the browse cursor is
//        already on the bottom row, TBrowse:hitBottom is set true (.T.).
//
METHOD PageDown() CLASS TBrowse

   // Go to last line of nexts page with skip, rowpos inaltered
   ::MoveRow( ::rowCount + ::rowCount - ::rowPos )

RETURN Self

// Repositions the data source to the top of file
//
//     goTop() --> self
//
//        Repositions the data source to the logical beginning of file (by
//        evaluating the TBrowse:goTopBlock), refills the display with the
//        topmost available data, and moves the browse cursor to the uppermost
//        data row for which data is available.  The pan position of the window
//        is not changed.
//
METHOD GoTop() CLASS TBrowse

   IF !::lUserDeHilite
      ::deHilite()
   ENDIF
   Eval( ::goTopBlock )
   ::nSkipped := 0
   ::refreshAll()
   ::lVSMoveDown := TRUE
   ::rowPos := 1

RETURN Self

// Repositions the data source to the bottom of file
//
//     goBottom() --> self
//
//        Repositions the data source to logical bottom of file (by evaluating
//        the TBrowse:goBottomBlock), refills the display with the bottommost
//        available data, and moves the browse cursor to the lowermost data row
//        for which data is available.  The pan position of the window is not
//        changed.
//
METHOD GoBottom() CLASS TBrowse

   IF !::lUserDeHilite
      ::deHilite()
   ENDIF
   Eval( ::goBottomBlock )
   ::nSkipped := 0
   ::refreshAll()
   ::lVSMoveDown := FALSE
   ::rowPos := ::rowCount // ::VSDataRows()

RETURN Self

// **** Internal method, move display cursor between ROWS ****
METHOD MoveRow( nToSkip ) CLASS TBrowse
   LOCAL nSkipped
   //TraceLog( "MoveRow()" )

   DEFAULT nToSkip TO 0

   nSkipped := Eval( ::skipBlock, nToSkip )
   //__OutDebug( "nToSkip, nSkipped, ::rowPos, ::rowCount", nToSkip, nSkipped, ::rowPos, ::rowCount )

   IF nSkipped <> nToSkip  // Hit top or bottom
      IF nToSkip < 0 .AND. ::rowPos == 1
         ::hitTop    := .T.
         ::hitBottom := .F.
      ELSEIF nToSkip < 0
         //::rowPos    := 1
         ::hitTop    := .F.
         ::hitBottom := .F.
      ELSEIF nToSkip > 0 .AND. ::rowPos == ::rowCount
         ::hitTop    := .F.
         ::hitBottom := .T.
      ELSEIF nToSkip > 0
         //::rowPos    := ::rowCount
         ::hitTop    := .F.
         ::hitBottom := .F.
      ENDIF
   ELSE
      ::hitTop    := .F.
      ::hitBottom := .F.
   ENDIF

   IF nSkipped > 0 //::rowCount
      ::lVSMoveDown := FALSE
   ELSE
      ::lVSMoveDown := TRUE
   ENDIF

   IF Abs( nSkipped ) <> 0
      IF Abs( nToSkip ) >= ::rowCount
         ::refreshAll()
      ELSE
         //::rowPos += nSkipped
         ::refreshCurrent()
      ENDIF
   ENDIF
   ::nSkipped := nSkipped


RETURN Self

// ------- END CURSOR ROW MOVEMENTS --------

// ------------------- END MOVEMENT METHODS ----------------------------------------------


#ifdef HB_COMPAT_C53

METHOD ApplyKey(nKey) CLASS TBrowse

RETURN ::TApplyKey(nKey, self)


METHOD InitKeys(o) CLASS TBROWSE

   Default o:aKeys to {{K_DOWN,{|Ob,nKey| Ob:Down(),0}},;
              {K_END,{|Ob,nKey| Ob:End(),0}},;
              {K_CTRL_PGDN,{|Ob,nKey| Ob:GoBottom(),0}},;
              {K_CTRL_PGUP,{|Ob,nKey| Ob:GoTop(),0}},;
              {K_HOME,{|Ob,nKey| Ob:Home(),0}},;
              {K_LEFT,{|Ob,nKey| Ob:Left(),0}},;
              {K_PGDN,{|Ob,nKey| Ob:PageDown(),0}},;
              {K_PGUP,{|Ob,nKey| Ob:PageUp(),0}},;
              {K_CTRL_END,{|Ob,nKey| Ob:PanEnd(),0}},;
              {K_CTRL_HOME,{|Ob,nKey| Ob:PanHome(),0}},;
              {K_CTRL_LEFT,{|Ob,nKey| Ob:PanLeft(),0}},;
              {K_CTRL_RIGHT,{|Ob,nKey| Ob:PanRight(),0}},;
              {K_RIGHT,{|Ob,nKey| Ob:Right(),0}},;
              {K_UP,{|Ob,nKey| Ob:Up(),0}},;
              {K_ESC,{|Ob,nKey| -1 }},;
              {K_LBUTTONDOWN,{|Ob,nKey| tbmouse(ob,mrow(),mcol())}}}
RETURN o


METHOD SetKey(nKey,bBlock) CLASS TBrowse

   LOCAL bReturn,nPos

   ::InitKeys(self)

   IF (nPos:=ascan(::aKeys,{|x| x[1]==nkey}))==0
      IF ( ISBLOCK( bBlock ) )
         bReturn:= bBlock
         aadd(::aKeys,{nKey,bBlock})
      ENDIF
      bReturn:=bBlock

   ELSEIF (ISBLOCK(bBlock))
      ::aKeys[npos][2]:=bBlock
      bReturn:=bBlock

   ELSEIF PCOUNT()==1
      bReturn:= ::aKeys[npos][2]

   ELSEIF ( bReturn:= ::aKeys[ nPos ][ 2 ], PCount() == 2 .AND. ;
           ISNIL( bBlock ) .AND. nKey != 0 )
      adel(::aKeys, nPos)
      asize(::akeys, Len(::aKeys) - 1)
   ENDIF

RETURN bReturn


METHOD TApplyKey( nKey, oBrowse ) CLASS tBrowse

   LOCAL bBlock := oBrowse:setkey(nKey), nReturn := TBR_CONTINUE  // 0

   DEFAULT bBlock TO oBrowse:setkey(0)

   IF ( ISNIL( bBlock ) )
      nReturn := TBR_EXCEPTION  // 1
   ELSE
      nReturn := eval(bBlock, oBrowse, nKey)
   ENDIF

RETURN nReturn
#endif

#ifdef HB_COMPAT_C53
// NOTE: Not tested, could be broken
METHOD MGotoYX(nRow, nCol) CLASS TBrowse

   LOCAL nColsLen, nI, nNewRow

   // Am I inside TBrowse display area ?
   IF nRow > ::FnTop .AND. nRow < ::FnBottom .AND. ;
      nCol > ::FnLeft .AND. nCol < ::FnRight

      // if not stable force repositioning of data source; maybe this is not first Stabilize() call after
      // TBrowse became unstable, but we need to call Stabilize() al least one time before moving again to be sure
      // data source is under cursor position
      IF ! ::stable
         ::Stabilize()

      ELSE
         ::Moved()

      ENDIF

      // Set new row position
      nNewRow := ::nwRow - ::FnTop + iif(::lHeaders, ::nHeadersHeight, 0) + iif(Empty(::HeadSep), 0, 1) - 1
      ::nRecsToSkip := nNewRow - ::nNewRowPos

      // move data source accordingly
      ::Stabilize()

      // Now move to column under nCol
      nColsLen := 0
      // NOTE: I don't think it is correct, have to look up docs
      nI := iif(::Ffreeze > 0, ::Ffreeze, ::FleftVisible)

      while nColsLen < nCol .AND. nI < ::FrightVisible

         nColsLen += ::aColsWidth[nI]
         IF nI >= 1 .AND. nI < ::FcolCount
            nColsLen += iif(::aColumns[nI]:ColSep != NIL, Len(::aColumns[nI]:ColSep), Len(::ColSep))
         ENDIF

         nI++

      enddo

      ::ColPos := nI

      // Force redraw of current row with new cell position
      ::RefreshCurrent()

   ENDIF

RETURN Self
#endif

#ifdef HB_COMPAT_C53
function TBMOUSE( oBrowse, nMouseRow, nMouseCol )

   LOCAL Local1
   IF ( oBrowse:hittest(nMouseRow, nMouseCol) == -5121 )

      Local1 := oBrowse:mrowpos - oBrowse:rowpos

      do while ( Local1 < 0 )
         Local1++
         oBrowse:up()
      enddo

      do while ( Local1 > 0 )
         Local1--
         oBrowse:down()
      enddo

      Local1 := oBrowse:mcolpos - oBrowse:colpos

      do while ( Local1 < 0 )
         Local1++
         oBrowse:left()
      enddo

      do while ( Local1 > 0 )
         Local1--
         oBrowse:right()
      enddo

      RETURN 0
   ENDIF

   RETURN 1

METHOD HitTest(mrow,mcol) CLASS TBROWSE
  LOCAL i
  ::mRowPos := ::rowPos
  ::mColPos := ::colPos


  IF mRow< ::rect[1] .or. mRow > ::rect[3]
     RETURN HTNOWHERE
  ENDIF

  IF mCol < ::rect[2] .or. mCol > ::rect[4]
     RETURN HTNOWHERE
  ENDIF

  ::mRowPos := mRow - ::rect[1]+1
  FOR i = 1 TO len(::aVisibleCols)
      IF ::aVisibleCols[i] > mcol
         exit
      ENDIF
  NEXT

  ::mColpos := ::aVisibleCols[i]

RETURN HTCELL

METHOD SetStyle( nMode, lSetting ) CLASS TBROWSE
  LOCAL lRet := .F.

  IF nMode > LEN( ::aSetStyle )
     RETURN .F.
  ENDIF

  lRet := ::aSetStyle[ nMode ]

  IF ISLOGICAL( lSetting )
     ::aSetStyle[ nMode ] := lSetting
  ENDIF

RETURN lRet
#endif


// ------- BEGIN UTILITY FUNCTIONS --------

static function LenVal( xVal, cType, cPict )
   LOCAL nLen

   IF !ISCHARACTER( cType )
      cType := Valtype( xVal )
   ENDIF

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

RETURN nLen

STATIC FUNCTION aFillCloning( aArray, xValue )
  LOCAL n, nLen := Len( aArray )
  FOR n := 1 TO nLen
      aArray[ n ] := IIF( ValType( xValue ) $ "AO", AClone( xValue ), xValue )
  NEXT
RETURN aArray

// ------- END   UTILITY FUNCTIONS --------

CLASS TBColumnInfo
   DATA width       INIT 0
   DATA Heading
   DATA Footing
   DATA headSep
   DATA footSep
   DATA colSep

   DATA nPos        INIT 0
   DATA nFullWidth  INIT 0
   DATA nOffset     INIT 0

   METHOD New() CONSTRUCTOR
ENDCLASS

METHOD New( nPos, width, Heading, Footing, headSep, footSep, colSep ) CLASS TBColumnInfo

   ::nPos    := nPos
   ::width   := width
   ::Heading := Heading
   ::Footing := Footing
   ::headSep := headSep
   ::footSep := footSep
   ::colSep  := colSep

RETURN Self


   