/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Printing subsystem for Win32 using GUI printing
 *     Copyright 2004 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
*/

/*

  win32prn() was designed to make it easy to emulate Clipper Dot Matrix printing.
  Dot Matrix printing was in CPI ( Characters per inch & Lines per inch ).
  Even though "Mapping Mode" for win32prn() is MM_TEXT, ::SetFont() accepts the
  nWidth parameter in CPI not Pixels. Also the default ::LineHeight is for
  6 lines per inch so ::NewLine() works as per "LineFeed" on Dot Matrix printers.
  If you do not like this then inherit from the class and override anything you want

  Simple example


  TO DO:    Colour printing
            etc....

  Peter Rees 21 January 2004 <peter@rees.co.nz>

  Peter Rees - 20/07/2007 added support for Custom Forms

*/

#ifndef __PLATFORM__Windows

FUNCTION WIN32PRN()

   RETURN nil

#else

#include "hbclass.ch"

#include "common.ch"

#include "win32prn.ch"

CLASS WIN32PRN

   METHOD New( cPrinter )
   METHOD CREATE()                // CreatesDC and sets "Courier New" font, set Orientation, Copies, Bin#
// Create() ( & StartDoc() ) must be called before printing can start.
   METHOD Destroy()               // Calls EndDoc() - restores default font, Deletes DC.
// Destroy() must be called to avoid memory leaks
   METHOD StartDoc( cDocame )       // Calls StartPage()
   METHOD EndDoc( lAbortDoc )       // Calls EndPage() if lAbortDoc not .T.
   METHOD StartPage()
   METHOD EndPage( lStartNewPage )      // If lStartNewPage = .T. then StartPage() is called for the next page of output
   METHOD NewLine()
   METHOD NewPage()
   METHOD SetFont( cFontName, nPointSize, nWidth, nBold, lUnderline, lItalic, nCharSet )
// NB: nWidth is in "CharactersPerInch"
//     _OR_ { nMul, nDiv } which equates to "CharactersPerInch"
//     _OR_ ZERO ( 0 ) which uses the default width of the font
//          for the nPointSize
//   IF nWidth (or nDiv) is < 0 then Fixed font is emulated

   METHOD SetDefaultFont()

   METHOD GetFonts()                                   // Returns array of { "FontName", lFixed, lTrueType, nCharSetRequired }
   METHOD Bold( nBoldWeight )
   METHOD UnderLine( lOn )
   METHOD Italic( lOn )
   METHOD SetDuplexType( nDuplexType )                       // Get/Set current Duplexmode
   METHOD SetPrintQuality( nPrintQuality )               // Get/Set Printquality
   METHOD CharSet( nCharSet )


   METHOD SetPos( nX, nY )                               // **WARNING** : (Col,Row) _NOT_ (Row,Col)
   METHOD SetColor( nClrText, nClrPane, nAlign ) INLINE ( ;
      ::TextColor := nClrText, ::BkColor := nClrPane, ::TextAlign := nAlign, ;
      SetColor( ::hPrinterDC, nClrText, nClrPane, nAlign ) )

   METHOD TextOut( cString, lNewLine, lUpdatePosX, nAlignHori, nAlighVert )     // set TA_* defines above
   METHOD TextOutAt( nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlignHori, nAlighVert ) // **WARNING** : (Col,Row) _NOT_ (Row,Col)
   METHOD TextOutW( wString, lNewLine, lUpdatePosX, nAlignHori, nAlighVert )     // set TA_* defines above
   METHOD TextOutWAt( nPosX, nPosY, wString, lNewLine, lUpdatePosX, nAlignHori, nAlighVert ) // **WARNING** : (Col,Row) _NOT_ (Row,Col)



   METHOD SetPen( nStyle, nWidth, nColor ) INLINE ( ;
      ::PenStyle := nStyle, ::PenWidth := nWidth, ::PenColor := nColor, ;
      SetPen( ::hPrinterDC, nStyle, nWidth, nColor ) )
   METHOD Line( nX1, nY1, nX2, nY2 ) INLINE LineTo( ::hPrinterDC, nX1, nY1, nX2, nY2 )
   METHOD Box( nX1, nY1, nX2, nY2, nWidth, nHeight ) INLINE Rectangle( ::hPrinterDC, nX1, nY1, nX2, nY2, nWidth, nHeight )
   METHOD Arc( nX1, nY1, nX2, nY2 ) INLINE Arc( ::hPrinterDC, nX1, nY1, nX2, nY2 )
   METHOD Ellipse( nX1, nY1, nX2, nY2 ) INLINE Ellipse( ::hPrinterDC, nX1, nY1, nX2, nY2 )
   METHOD FillRect( nX1, nY1, nX2, nY2, nColor, nStyle, nHatch ) INLINE FillRect( ::hPrinterDC, nX1, nY1, nX2, nY2, nColor, nStyle, nHatch )
   METHOD GetCharWidth()
   METHOD GetCharHeight()
   METHOD GetTextWidth( cString )
   METHOD GetTextHeight( cString )
   METHOD DrawBitMap( oBmp )
   METHOD DrawText( nX1, nY1, nX2, nY2, cString, nAlign ) INLINE DrawText( ::hPrinterDC, nX1, nY1, nX2, nY2, cString, nAlign )

//  Clipper DOS compatible functions.
   METHOD SetPRC( nRow, nCol )        // Based on ::LineHeight and current ::CharWidth
   METHOD PRow()
   METHOD PCol()
   METHOD MaxRow()                  // Based on ::LineHeight & Form dimensions
   METHOD MaxCol()                  // Based on ::CharWidth & Form dimensions

   METHOD MM_TO_POSX( nMm )      // Convert position on page from MM to pixel location Column
   METHOD MM_TO_POSY( nMm )      //   "       "      "    "    "   "  "   "      "     Row
   METHOD INCH_TO_POSX( nInch )  // Convert position on page from INCH to pixel location Column
   METHOD INCH_TO_POSY( nInch )  //   "       "      "    "    "   "    "   "       "    Row

   METHOD TextAtFont( nPosX, nPosY, cString, cFont, nPointSize, ;     // Print text string at location
      nWidth, nBold, lUnderLine, lItalic, lNewLine, ; // in specified font and color.
   lUpdatePosX, nColor, nAlignHori, nAlignVert )                  // Restore original font and colour
// after printing.
   METHOD SetBkMode( nMode )  INLINE SetBkMode( ::hPrinterDc, nMode ) // OPAQUE= 2 or TRANSPARENT= 1
// Set Background mode

   METHOD GetDeviceCaps( nCaps ) INLINE GetDeviceCaps( ::hPrinterDC, nCaps )

   VAR PrinterName    INIT ""
   VAR Printing       INIT .F.
   VAR HavePrinted    INIT .F.
   VAR hPrinterDc     INIT 0

   VAR AskProperties  INIT .F.

// These next 4 variables must be set before calling ::Create() if
// you wish to alter the defaults
   VAR FormType       INIT 0
   VAR BinNumber      INIT 0
   VAR Landscape      INIT .F.
   VAR Copies         INIT 1

   VAR PaperLength    INIT 0                        // Value is * 1/10 of mm   1000 = 10cm
   VAR PaperWidth     INIT 0                        //   "    "    "     "       "     "

   VAR SetTextHori    INIT TA_LEFT    // Default horizontal alignment SetTextAlign() (TEXTOUT)
   VAR SetTextVert    INIT TA_BOTTOM  // Default vertical alignment for SetTextAlign() (TEXTOUT)

   VAR SetFontOk      INIT .F.
   VAR FontName       INIT ""                        // Current Point size for font
   VAR FontPointSize  INIT 12                        // Point size for font
   VAR FontWidth      INIT { 0, 0 }                     // {Mul, Div} Calc width: nWidth:= MulDiv(nMul, GetDeviceCaps(shDC,LOGPIXELSX), nDiv)
// If font width is specified it is in "characters per inch" to emulate DotMatrix
   VAR fBold           INIT 0      HIDDEN            // font darkness weight ( Bold). See wingdi.h or WIN SDK CreateFont() for valid values
   VAR fUnderLine      INIT .F.    HIDDEN            // UnderLine is on or off
   VAR fItalic         INIT .F.    HIDDEN            // Italic is on or off
   VAR fCharSet        INIT 1      HIDDEN            // Default character set == DEFAULT_CHARSET ( see wingdi.h )

   VAR PixelsPerInchY
   VAR PixelsPerInchX
   VAR PageHeight       INIT 0
   VAR PageWidth        INIT 0
   VAR TopMargin        INIT 0
   VAR BottomMargin     INIT 0
   VAR LeftMargin       INIT 0
   VAR RightMargin      INIT 0
   VAR LineHeight       INIT 0
   VAR CharHeight       INIT 0
   VAR CharWidth        INIT 0
   VAR fCharWidth       INIT 0      HIDDEN
   VAR BitmapsOk        INIT .F.
   VAR NumColors        INIT 1

   VAR fDuplexType      INIT 0      HIDDEN              //DMDUP_SIMPLEX, 22/02/2007 change to 0 to use default printer settings
   VAR fPrintQuality    INIT 0      HIDDEN              //DMRES_HIGH, 22/02/2007 change to 0 to use default printer settings
   VAR fNewDuplexType   INIT 0      HIDDEN
   VAR fNewPrintQuality INIT 0      HIDDEN
   VAR fOldLandScape    INIT .F.    HIDDEN
   VAR fOldBinNumber    INIT 0      HIDDEN
   VAR fOldFormType     INIT 0      HIDDEN
   VAR fOldPaperLength  INIT 0      HIDDEN
   VAR fOldPaperWidth   INIT 0      HIDDEN

   VAR PosX             INIT 0
   VAR PosY             INIT 0

   VAR TextColor
   VAR BkColor
   VAR TextAlign

   VAR PenStyle
   VAR PenWidth
   VAR PenColor

ENDCLASS

METHOD New( cPrinter ) CLASS WIN32PRN

   ::PrinterName := iif( !Empty( cPrinter ), cPrinter, GetDefaultPrinter() )

   RETURN( Self )

METHOD CREATE() CLASS WIN32PRN

   LOCAL Result := .F.

   ::Destroy()                            // Finish current print job if any
   IF !Empty( ::hPrinterDC := CreateDC( ::PrinterName ) )

      // Set Form Type
      // Set Number of Copies
      // Set Orientation
      // Set Duplex mode
      // Set PrintQuality

      IF !::AskProperties
         // old code, //x added some error possibility
         IF !SetDocumentProperties( ::hPrinterDC, ::PrinterName, ::FormType, ::Landscape, ::Copies, ::BinNumber, ::fDuplexType, ::fPrintQuality, ::PaperLength, ::PaperWidth )
            ::Destroy()
            RETURN .F.
         ENDIF
      ELSE
         // pass some vars byref
         IF !SetDocumentProperties( ::hPrinterDC, ::PrinterName, @::FormType, @::Landscape, @::Copies, @::BinNumber, @::fDuplexType, @::fPrintQuality, @::PaperLength, @::PaperWidth )
            // might be an error, or cancelled
            ::Destroy()
            RETURN .F.
         ENDIF
         // must update fnew* if * was changed
      ENDIF

      // Set mapping mode to pixels, topleft down
      SetMapMode( ::hPrinterDC, MM_TEXT )
      //    SetTextCharacterExtra(::hPrinterDC,0); // do not add extra char spacing even if bold
      // Get Margins etc... here
      ::PageWidth        := GetDeviceCaps( ::hPrinterDC, PHYSICALWIDTH )
      ::PageHeight       := GetDeviceCaps( ::hPrinterDC, PHYSICALHEIGHT )
      ::LeftMargin       := GetDeviceCaps( ::hPrinterDC, PHYSICALOFFSETX )
      ::RightMargin      := ( ::PageWidth - ::LeftMargin ) + 1
      ::PixelsPerInchY   := GetDeviceCaps( ::hPrinterDC, LOGPIXELSY )
      ::PixelsPerInchX   := GetDeviceCaps( ::hPrinterDC, LOGPIXELSX )
      ::LineHeight       := Int( ::PixelsPerInchY / 6 )  // Default 6 lines per inch == # of pixels per line
      ::TopMargin        := GetDeviceCaps( ::hPrinterDC, PHYSICALOFFSETY )
      ::BottomMargin     := ( ::PageHeight - ::TopMargin ) + 1

      // Set .T. if can print bitmaps
      ::BitMapsOk :=  BitMapsOk( ::hPrinterDC )

      // supports Colour
      ::NumColors := GetDeviceCaps( ::hPrinterDC, NUMCOLORS )


      // Set the standard font
      ::SetDefaultFont()
      ::HavePrinted := ::Printing := .F.
      ::fOldFormType := ::FormType  // Last formtype used
      ::fOldLandScape := ::LandScape
      ::fOldBinNumber := ::BinNumber
      ::fNewDuplexType := ::fDuplexType
      ::fNewPrintQuality := ::fPrintQuality
      ::fOldPaperLength  := ::PaperLength
      ::fOldPaperWidth   := ::PaperWidth

      Result := .T.
   ENDIF

   RETURN( Result )

METHOD Destroy() CLASS WIN32PRN

   IF !Empty( ::hPrinterDc )
      IF ::Printing
         ::EndDoc()
      ENDIF
      ::hPrinterDC := DeleteDC( ::hPrinterDC )
   ENDIF

   RETURN( .T. )

METHOD StartDoc( cDocName ) CLASS WIN32PRN

   LOCAL Result

   IF cDocName == NIL
      cDocName := GetExeFileName() + " [" + DToC( Date() ) + ' - ' + Time() + "]"
   ENDIF
   IF ( Result := StartDoc( ::hPrinterDc, cDocName ) )
      IF !( Result := ::StartPage( ::hPrinterDc ) )
         ::EndDoc( .T. )
      ELSE
         ::Printing := .T.
      ENDIF
   ENDIF

   RETURN( Result )

METHOD EndDoc( lAbortDoc ) CLASS WIN32PRN

   IF lAbortDoc == NIL
      lAbortDoc := .F.
   ENDIF
   IF !::HavePrinted
      lAbortDoc := .T.
   ENDIF
   IF !lAbortDoc
      ::EndPage( .F. )
   ENDIF
   EndDoc( ::hPrinterDC, lAbortDoc )
   ::Printing := .F.
   ::HavePrinted := .F.

   RETURN( .T. )

METHOD StartPage() CLASS WIN32PRN

   LOCAL lLLandScape, nLBinNumber, nLFormType, nLDuplexType, nLPrintQuality
   LOCAL lChangeDP := .F. , nLPaperLength, nLPaperWidth

   IF ::LandScape <> ::fOldLandScape  // Direct-modify property
      lLLandScape := ::fOldLandScape := ::LandScape
      lChangeDP := .T.
   ENDIF
   IF ::BinNumber <> ::fOldBinNumber  // Direct-modify property
      nLBinNumber := ::fOldBinNumber := ::BinNumber
      lChangeDP := .T.
   ENDIF
   IF ::FormType <> ::fOldFormType  // Direct-modify property
      nLFormType := ::fOldFormType := ::FormType
      lChangeDP := .T.
   ENDIF
   IF ::fDuplexType <> ::fNewDuplexType  // Get/Set property
      nLDuplexType := ::fDuplexType := ::fNewDuplexType
      lChangeDP := .T.
   ENDIF
   IF ::fPrintQuality <> ::fNewPrintQuality  // Get/Set property
      nLPrintQuality := ::fPrintQuality := ::fNewPrintQuality
      lChangeDP := .T.
   ENDIF
   IF ::fOldPaperLength <> ::PaperLength  // Get/Set property
      nLPaperLength := ::fOldPaperLength := ::PaperLength
      lChangeDP := .T.
   ENDIF
   IF ::fOldPaperWidth <> ::PaperWidth  // Get/Set property
      nLPaperWidth := ::fOldPaperWidth := ::PaperWidth
      lChangeDP := .T.
   ENDIF
   IF lChangeDP
      SetDocumentProperties( ::hPrinterDC, ::PrinterName, nLFormType, lLLandscape, , nLBinNumber, nLDuplexType, nLPrintQuality, nLPaperLength, nLPaperWidth )
   ENDIF
   StartPage( ::hPrinterDC )
   ::PosX := ::LeftMargin
   ::PosY := ::TopMargin

   RETURN( .T. )

METHOD EndPage( lStartNewPage ) CLASS WIN32PRN

   IF lStartNewPage == NIL
      lStartNewPage := .T.
   ENDIF
   EndPage( ::hPrinterDC )
   IF lStartNewPage
      ::StartPage()
      IF OS_ISWIN9X() // Reset font on Win9X
         ::SetFont()
      ENDIF
   ENDIF

   RETURN( .T. )

METHOD NewLine() CLASS WIN32PRN

   ::PosX := ::LeftMargin
   ::PosY += ::LineHeight

   RETURN( ::PosY )

METHOD NewPage() CLASS WIN32PRN

   ::EndPage( .T. )

   RETURN( .T. )

// If font width is specified it is in "characters per inch" to emulate DotMatrix
// An array {nMul,nDiv} is used to get precise size such a the Dot Matric equivalent
// of Compressed print == 16.67 char per inch == { 3,-50 }
// If nDiv is < 0 then Fixed width printing is forced via ExtTextOut()

METHOD SetFont( cFontName, nPointSize, nWidth, nBold, lUnderline, lItalic, nCharSet ) CLASS WIN32PRN

   LOCAL cType

   IF cFontName != NIL
      ::FontName := cFontName
   ENDIF
   IF nPointSize != NIL
      ::FontPointSize := nPointSize
   ENDIF
   IF nWidth != NIL
      cType := ValType( nWidth )
      IF cType = 'A'
         ::FontWidth     := nWidth
      ELSEIF cType = 'N' .AND. !Empty( nWidth )
         ::FontWidth     := { 1, nWidth }
      ELSE
         ::FontWidth     := { 0, 0 }
      ENDIF
   ENDIF
   IF nBold != NIL
      ::fBold := nBold
   ENDIF
   IF lUnderLine != NIL
      ::fUnderline := lUnderLine
   ENDIF
   IF lItalic != NIL
      ::fItalic := lItalic
   ENDIF
   IF nCharSet != NIL
      ::fCharSet := nCharSet
   ENDIF
   IF ( ::SetFontOk := CreateFont( ::hPrinterDC, ::FontName, ::FontPointSize, ::FontWidth[1], ::FontWidth[2], ::fBold, ::fUnderLine, ::fItalic, ::fCharSet ) )
      ::fCharWidth        := ::GetCharWidth()
      ::CharWidth := Abs( ::fCharWidth )
      ::CharHeight := ::GetCharHeight()
   ENDIF
   ::FontName := GetPrinterFontName( ::hPrinterDC )  // Get the font name that Windows actually used

   RETURN( ::SetFontOk )

METHOD SetDefaultFont()

   RETURN( ::SetFont( "Courier New",12,{ 1, 10 }, 0, .F. , .F. , 0 ) )

METHOD Bold( nWeight ) CLASS WIN32PRN

   LOCAL Result := ::fBold

   IF nWeight != NIL
      ::fBold := nWeight
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN( Result )

METHOD Underline( lUnderLine ) CLASS WIN32PRN

   LOCAL Result := ::fUnderline

   IF lUnderLine != NIL
      ::fUnderLine := lUnderLine
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN( Result )

METHOD Italic( lItalic ) CLASS WIN32PRN

   LOCAL Result := ::fItalic

   IF lItalic != NIL
      ::fItalic := lItalic
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN( Result )

METHOD CharSet( nCharSet ) CLASS WIN32PRN

   LOCAL Result := ::fCharSet

   IF nCharSet != NIL
      ::fCharSet := nCharSet
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN( Result )

METHOD SetDuplexType( nDuplexType ) CLASS WIN32PRN

   LOCAL Result := ::fDuplexType

   IF nDuplexType != NIL
      ::fNewDuplexType := nDuplexType
      IF !::Printing
         ::fDuplexType := nDuplexType
      ENDIF
   ENDIF

   RETURN( Result )

METHOD SetPrintQuality( nPrintQuality ) CLASS WIN32PRN

   LOCAL Result := ::fPrintQuality

   IF nPrintQuality != NIL
      ::fNewPrintQuality := nPrintQuality
      IF !::Printing
         ::fPrintQuality := nPrintQuality
      ENDIF
   ENDIF

   RETURN( Result )

METHOD GetFonts() CLASS WIN32PRN

   RETURN( ENUMFONTS( ::hPrinterDC ) )

METHOD SetPos( nPosX, nPosY ) CLASS WIN32PRN

   LOCAL Result := { ::PosX, ::PosY }

   IF nPosX != NIL
      ::PosX := Int( nPosX )
   ENDIF
   IF nPosY != NIL
      ::PosY := Int( nPosY )
   ENDIF

   RETURN( Result )

METHOD TextOut( cString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert ) CLASS WIN32PRN

   LOCAL nPosX

   IF nAlignHori == NIL
      nAlignHori := ::SetTextHori
   ENDIF
   IF nAlignVert == NIL
      nAlignVert := ::SetTextVert
   ENDIF
   IF lUpdatePosX == NIL
      lUpdatePosX := .T.
   ENDIF
   IF lNewLine == NIL
      lNewLine := .F.
   ENDIF
   IF cString != NIL
      nPosX := TextOut( ::hPrinterDC, ::PosX, ::PosY, cString, Len( cString ), ::fCharWidth, nAlignHori, nAlignVert )
      ::HavePrinted := .T.
      IF lUpdatePosX
         ::PosX += nPosX
      ENDIF
      IF lNewLine
         ::NewLine()
      ENDIF
   ENDIF

   RETURN( .T. )

METHOD TextOutW( wString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert ) CLASS WIN32PRN

   LOCAL nPosX

   IF nAlignHori == NIL
      nAlignHori := ::SetTextHori
   ENDIF
   IF nAlignVert == NIL
      nAlignVert := ::SetTextVert
   ENDIF
   IF lUpdatePosX == NIL
      lUpdatePosX := .T.
   ENDIF
   IF lNewLine == NIL
      lNewLine := .F.
   ENDIF
   IF wString != NIL
      //#TODO: count len of unicode, for now assume as 2 bytes per charactor
      nPosX := TextOutW( ::hPrinterDC, ::PosX, ::PosY, wString, Len( wString )/2, ::fCharWidth, nAlignHori, nAlignVert )
      ::HavePrinted := .T.
      IF lUpdatePosX
         ::PosX += nPosX
      ENDIF
      IF lNewLine
         ::NewLine()
      ENDIF
   ENDIF

   RETURN( .T. )

METHOD TextOutAt( nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert ) CLASS WIN32PRN

   IF lNewLine == NIL
      lNewLine := .F.
   ENDIF
   IF lUpdatePosX == NIL
      lUpdatePosX := .T.
   ENDIF
   ::SetPos( nPosX, nPosY )
   ::TextOut( cString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert )

   RETURN( .T. )

METHOD TextOutWAt( nPosX, nPosY, wString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert ) CLASS WIN32PRN

   IF lNewLine == NIL
      lNewLine := .F.
   ENDIF
   IF lUpdatePosX == NIL
      lUpdatePosX := .T.
   ENDIF
   ::SetPos( nPosX, nPosY )
   ::TextOutW( wString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert )

   RETURN( .T. )

METHOD GetCharWidth() CLASS WIN32PRN

   LOCAL nWidth

   IF ::FontWidth[2] < 0 .AND. !Empty( ::FontWidth[1] )
      nWidth := MulDiv( ::FontWidth[1], ::PixelsPerInchX, ::FontWidth[2] )
   ELSE
      nWidth := GetCharSize( ::hPrinterDC )
   ENDIF

   RETURN( nWidth )

METHOD GetCharHeight() CLASS WIN32PRN

   RETURN( GetCharSize( ::hPrinterDC, .T. ) )

METHOD GetTextWidth( cString ) CLASS WIN32PRN

   LOCAL nWidth

   IF ::FontWidth[2] < 0 .AND. !Empty( ::FontWidth[1] )
      nWidth := Len( cString ) * ::CharWidth
   ELSE
      nWidth := GetTextSize( ::hPrinterDC, cString, Len( cString ) )  // Return Width in device units
   ENDIF

   RETURN( nWidth )

METHOD GetTextHeight( cString ) CLASS WIN32PRN

   RETURN( GetTextSize( ::hPrinterDC, cString, Len(cString ), .F. ) )  // Return Height in device units

METHOD DrawBitMap( oBmp ) CLASS WIN32PRN

   LOCAL Result := .F.

   IF ::BitMapsOk .AND. ::Printing .AND. !Empty( oBmp:BitMap )
      IF ( Result := DrawBitMap( ::hPrinterDc, oBmp:BitMap,oBmp:Rect[1], oBmp:Rect[2], oBmp:rect[3], oBmp:Rect[4] ) )
         ::HavePrinted := .T.
      ENDIF
   ENDIF

   RETURN( Result )

METHOD SetPRC( nRow, nCol ) CLASS WIN32PRN

   ::SetPos( ( nCol * ::CharWidth ) + ::LeftMArgin, ( nRow * ::LineHeight ) + ::TopMargin )

   RETURN( NIL )

METHOD PRow() CLASS WIN32PRN

   RETURN( Int( (::PosY - ::TopMargin )/::LineHeight ) )   // No test for Div by ZERO

METHOD PCol() CLASS WIN32PRN

   RETURN( Int( (::PosX - ::LeftMargin )/::CharWidth ) )   // Uses width of current character

METHOD MaxRow() CLASS WIN32PRN

   RETURN( Int( ((::BottomMargin - ::TopMargin ) + 1 ) / ::LineHeight ) - 1 )

METHOD MaxCol() CLASS WIN32PRN

   RETURN( Int( ((::RightMargin - ::LeftMargin ) + 1 ) / ::CharWidth ) - 1 )

METHOD MM_TO_POSX( nMm ) CLASS WIN32PRN

   RETURN( Int( ( ( nMM * ::PixelsPerInchX ) / MM_TO_INCH ) - ::LeftMargin ) )

METHOD MM_TO_POSY( nMm ) CLASS WIN32PRN

   RETURN( Int( ( ( nMM * ::PixelsPerInchY ) / MM_TO_INCH ) - ::TopMargin ) )

METHOD INCH_TO_POSX( nInch ) CLASS WIN32PRN

   RETURN( Int( ( nInch * ::PixelsPerInchX  ) - ::LeftMargin ) )

METHOD INCH_TO_POSY( nInch ) CLASS WIN32PRN

   RETURN( Int( ( nInch * ::PixelsPerInchY ) - ::TopMargin ) )

METHOD TextAtFont( nPosX, nPosY, cString, cFont, nPointSize, nWidth, nBold, lUnderLine, lItalic, nCharSet, lNewLine, lUpdatePosX, nColor, nAlignHori, nAlignVert ) CLASS WIN32PRN

   LOCAL lCreated := .F. , nDiv := 0, cType

   DEFAULT nPointSize TO ::FontPointSize
   IF cFont != NIL
      cType := ValType( nWidth )
      IF cType = 'A'
         nDiv  := nWidth[ 1 ]
         nWidth := nWidth[ 2 ]
      ELSEIF cType = 'N' .AND. !Empty( nWidth )
         nDiv := 1
      ENDIF
      lCreated := CreateFont( ::hPrinterDC, cFont, nPointSize, nDiv, nWidth, nBold, lUnderLine, lItalic, nCharSet )
   ENDIF
   IF nColor != NIL
      nColor := SetColor( ::hPrinterDC, nColor )
   ENDIF
   ::TextOutAt( nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert )
   IF lCreated
      ::SetFont()  // Reset font
   ENDIF
   IF nColor != NIL
      SetColor( ::hPrinterDC, nColor )  // Reset Color
   ENDIF

   RETURN( .T. )

// Bitmap class

CLASS WIN32BMP

   EXPORTED:

   METHOD New()
   METHOD LoadFile( cFileName )
   METHOD CREATE()
   METHOD Destroy()
   METHOD Draw( oPrn, arectangle )
   VAR Rect     INIT { 0, 0, 0, 0 }        // Coordinates to print BitMap
//   XDest,                    // x-coord of destination upper-left corner
//   YDest,                    // y-coord of destination upper-left corner
//   nDestWidth,               // width of destination rectangle
//   nDestHeight,              // height of destination rectangle
// See WinApi StretchDIBits()
   VAR BitMap   INIT ""
   VAR FileName INIT ""

ENDCLASS

METHOD New() CLASS WIN32BMP

   RETURN( Self )

METHOD LoadFile( cFileName ) CLASS WIN32BMP

   ::FileName := cFileName
   ::Bitmap := LoadBitMapFile( ::FileName )

   RETURN( !Empty( ::Bitmap ) )

METHOD CREATE() CLASS WIN32BMP  // Compatibility function for Alaska Xbase++

   RETURN( Self )

METHOD Destroy() CLASS WIN32BMP  // Compatibility function for Alaska Xbase++

   RETURN( NIL )

METHOD Draw( oPrn, aRectangle ) CLASS WIN32BMP // Pass a TPRINT class reference & Rectangle array

   ::Rect := aRectangle

   RETURN( oPrn:DrawBitMap( Self ) )

CLASS XBPBITMAP FROM WIN32BMP // Compatibility Class for Alaska Xbase++

ENDCLASS

#pragma BEGINDUMP

#include <windows.h>
#include <winspool.h>

#include "hbapiitm.h"

#ifndef INVALID_FILE_SIZE
   #define INVALID_FILE_SIZE ( DWORD ) 0xFFFFFFFF
#endif

HB_FUNC_STATIC( CREATEDC )
{
   HB_ISIZ Result = 0;

   if( ISCHAR( 1 ) )
   {
      Result = ( HB_ISIZ ) CreateDC( "", hb_parc( 1 ), NULL, NULL );
      // Result = (LONG)  CreateDC("",hb_parc(1),NULL, NULL) ;
   }
   hb_retns( Result );
}

HB_FUNC_STATIC( STARTDOC )
{
   HDC      hDC      = ( HDC ) hb_parns( 1 );
   DOCINFO  sDoc;
   BOOL     Result   = FALSE;

   if( hDC )
   {
      sDoc.cbSize       = sizeof( DOCINFO );
      sDoc.lpszDocName  = ( char * ) hb_parc( 2 );
      sDoc.lpszOutput   = NULL;
      sDoc.lpszDatatype = NULL;
      sDoc.fwType       = 0;
      Result            = ( BOOL ) ( StartDoc( hDC, &sDoc ) > 0 );
   }
   hb_retl( Result );
}

HB_FUNC_STATIC( ENDDOC )
{
   BOOL  Result   = FALSE;
   HDC   hDC      = ( HDC ) hb_parns( 1 );

   if( hDC )
   {
      if( ISLOG( 2 ) && hb_parl( 2 ) )
      {
         Result = ( AbortDoc( hDC ) > 0 );
      }
      else
      {
         Result = ( EndDoc( hDC ) > 0 );
      }
   }
   hb_retl( Result );
}

HB_FUNC_STATIC( DELETEDC )
{
   HDC hDC = ( HDC ) hb_parns( 1 );

   if( hDC )
   {
      DeleteDC( hDC );
   }
   hb_retnl( 0 ); // Return zero as a new handle even if fails
}

HB_FUNC_STATIC( STARTPAGE )
{
   BOOL  Result   = FALSE;
   HDC   hDC      = ( HDC ) hb_parns( 1 );

   if( hDC )
   {
      Result = ( StartPage( hDC ) > 0 );
   }
   hb_retl( Result );
}

HB_FUNC_STATIC( ENDPAGE )
{
   BOOL  Result   = FALSE;
   HDC   hDC      = ( HDC ) hb_parns( 1 );

   if( hDC )
   {
      Result = ( EndPage( hDC ) > 0 );
   }
   hb_retl( Result );
}

HB_FUNC_STATIC( TEXTOUT )
{
   LONG  Result      = 0;
   HDC   hDC         = ( HDC ) hb_parns( 1 );
   UINT  uiAlignHori = ( UINT ) hb_parnl( 7 );
   UINT  uiAlignVert = ( UINT ) hb_parnl( 8 );
   SIZE  sSize;

   if( hDC )
   {
      int iLen = ( int ) hb_parnl( 5 );
      if( iLen > 0 )
      {
         int      iRow     = ( int ) hb_parnl( 2 );
         int      iCol     = ( int ) hb_parnl( 3 );
         char *   pszData  = ( char * ) hb_parc( 4 );
         int      iWidth   = ISNUM( 6 ) ? ( int ) hb_parnl( 6 ) : 0;
         SetTextAlign( ( HDC ) hDC, uiAlignHori | uiAlignVert | TA_NOUPDATECP );
         if( iWidth < 0 && iLen < 1024 )
         {
            int n = iLen, aFixed[ 1024 ];
            iWidth = -iWidth;
            while( n )
            {
               aFixed[ --n ] = iWidth;
            }
            if( ExtTextOut( hDC, iRow, iCol, 0, NULL, pszData, iLen, aFixed ) )
            {
               Result = ( LONG ) ( iLen * iWidth );
            }
         }
         else if( TextOut( hDC, iRow, iCol, pszData, iLen ) )
         {
            GetTextExtentPoint32( hDC, pszData, iLen, &sSize );   // Get the length of the text in device size
            Result = ( LONG ) sSize.cx;                           // return the width so we can update the current pen position (::PosY)
         }
      }
   }
   hb_retnl( Result );
}

HB_FUNC_STATIC( TEXTOUTW )
{
   LONG  Result      = 0;
   HDC   hDC         = ( HDC ) hb_parns( 1 );
   UINT  uiAlignHori = ( UINT ) hb_parnl( 7 );
   UINT  uiAlignVert = ( UINT ) hb_parnl( 8 );
   SIZE  sSize;

   if( hDC )
   {
      int iLen = ( int ) hb_parnl( 5 );
      if( iLen > 0 )
      {
         int      iRow     = ( int ) hb_parnl( 2 );
         int      iCol     = ( int ) hb_parnl( 3 );
         WCHAR *  pszData  = ( WCHAR * ) hb_parc( 4 );
         int      iWidth   = ISNUM( 6 ) ? ( int ) hb_parnl( 6 ) : 0;
         SetTextAlign( ( HDC ) hDC, uiAlignHori | uiAlignVert | TA_NOUPDATECP );
         if( iWidth < 0 && iLen < 1024 )
         {
            int n = iLen, aFixed[ 1024 ];
            iWidth = -iWidth;
            while( n )
            {
               aFixed[ --n ] = iWidth;
            }
            if( ExtTextOutW( hDC, iRow, iCol, 0, NULL, pszData, iLen, aFixed ) )
            {
               Result = ( LONG ) ( iLen * iWidth );
            }
         }
         else if( TextOutW( hDC, iRow, iCol, pszData, iLen ) )
         {
            GetTextExtentPoint32W( hDC, pszData, iLen, &sSize );  // Get the length of the text in device size
            Result = ( LONG ) sSize.cx;                           // return the width so we can update the current pen position (::PosY)
         }
      }
   }
   hb_retnl( Result );
}

HB_FUNC_STATIC( GETTEXTSIZE )
{
   LONG  Result   = 0;
   HDC   hDC      = ( HDC ) hb_parns( 1 );
   SIZE  sSize;

   if( hDC )
   {
      char *   pszData  = ( char * ) hb_parc( 2 );
      int      iLen     = ( int ) hb_parnl( 3 );
      GetTextExtentPoint32( hDC, pszData, iLen, &sSize ); // Get the length of the text in device size
      if( ISLOG( 4 ) && ! hb_parl( 4 ) )
      {
         Result = ( LONG ) sSize.cy; // return the height
      }
      else
      {
         Result = ( LONG ) sSize.cx; // return the width
      }
   }
   hb_retnl( Result );
}

HB_FUNC_STATIC( GETCHARSIZE )
{
   LONG  Result   = 0;
   HDC   hDC      = ( HDC ) hb_parns( 1 );

   if( hDC )
   {
      TEXTMETRIC tm;
      GetTextMetrics( hDC, &tm );
      if( ISLOG( 2 ) && hb_parl( 2 ) )
      {
         Result = ( LONG ) tm.tmHeight;
      }
      else
      {
         Result = ( LONG ) tm.tmAveCharWidth;
      }
   }
   hb_retnl( Result );
}

HB_FUNC_STATIC( GETDEVICECAPS )
{
   LONG  Result   = 0;
   HDC   hDC      = ( HDC ) hb_parns( 1 );

   if( hDC && ISNUM( 2 ) )
   {
      Result = ( LONG ) GetDeviceCaps( hDC, hb_parnl( 2 ) );
   }
   hb_retnl( Result );
}

HB_FUNC_STATIC( SETMAPMODE )
{
   LONG  Result   = 0;
   HDC   hDC      = ( HDC ) hb_parns( 1 );

   if( hDC && ISNUM( 2 ) )
   {
      Result = SetMapMode( hDC, hb_parnl( 2 ) );
   }
   hb_retnl( Result );
}

HB_FUNC( MULDIV )
{
   hb_retnl( MulDiv( hb_parnl( 1 ), hb_parnl( 2 ), hb_parnl( 3 ) ) );
}

HB_FUNC_STATIC( CREATEFONT )
{
   BOOL     Result      = FALSE;
   HDC      hDC         = ( HDC ) hb_parns( 1 );
   HFONT    hFont, hOldFont;
   char *   pszFont     = ( char * ) hb_parc( 2 );
   int      iHeight     = ( int ) hb_parnl( 3 );
   int      iMul        = ( int ) hb_parnl( 4 );
   int      iDiv        = ( int ) hb_parnl( 5 );
   int      iWidth;
   int      iWeight     = ( int ) hb_parnl( 6 );
   DWORD    dwUnderLine = ( DWORD ) hb_parl( 7 );
   DWORD    dwItalic    = ( DWORD ) hb_parl( 8 );
   DWORD    dwCharSet   = ( DWORD ) hb_parnl( 9 );

   iWeight  = iWeight > 0 ? iWeight : FW_NORMAL;
   iHeight  = -MulDiv( iHeight, GetDeviceCaps( hDC, LOGPIXELSY ), 72 );
   if( iDiv )
   {
      iWidth = MulDiv( abs( iMul ), GetDeviceCaps( hDC, LOGPIXELSX ), abs( iDiv ) );
   }
   else
   {
      iWidth = 0; // Use the default font width
   }

   hFont = CreateFont( iHeight, iWidth, 0, 0, iWeight, dwItalic, dwUnderLine, 0,
                       dwCharSet, OUT_DEVICE_PRECIS, CLIP_DEFAULT_PRECIS, DRAFT_QUALITY, DEFAULT_PITCH | FF_DONTCARE, pszFont );
   if( hFont )
   {
      Result   = TRUE;
      hOldFont = ( HFONT ) SelectObject( hDC, hFont );
      if( hOldFont )
      {
         DeleteObject( hOldFont );
      }
   }
   hb_retl( Result );
}

HB_FUNC_STATIC( GETPRINTERFONTNAME )
{
   HDC hDC = ( HDC ) hb_parns( 1 );

   if( hDC )
   {
      unsigned char cFont[ 128 ];
      GetTextFace( hDC, 127, ( LPTSTR ) cFont );
      hb_retc( ( char * ) cFont );
   }
   else
   {
      hb_retc( "" );
   }
}

HB_FUNC_STATIC( BITMAPSOK )
{
   BOOL  Result   = FALSE;
   HDC   hDC      = ( HDC ) hb_parns( 1 );

   if( hDC )
   {
      Result = ( GetDeviceCaps( hDC, RASTERCAPS ) & RC_STRETCHDIB );
   }
   hb_retl( Result );
}

/* //x
   SetDocumentProperties(hPrinterDC, PrinterName, FormType, Landscape, Copies, BinNumber, fDuplexType, fPrintQuality)
   NOTE: FormType, Landscape, Copies, BinNumber, fDuplexType, fPrintQuality, PaperLength, PaperWidth
        (3)       (4)        (5)     (6)        (7)          (8)              (9)          (10)
        may be passed byref.
        In that case, user will be prompted with Document Properties
 */
HB_FUNC_STATIC( SETDOCUMENTPROPERTIES )
{
   BOOL           bW9X, Result = FALSE;
   HDC            hDC = ( HDC ) hb_parns( 1 );
   OSVERSIONINFO  osvi;

   osvi.dwOSVersionInfoSize   = sizeof( OSVERSIONINFO );
   GetVersionEx( &osvi );
   bW9X                       = ( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
   if( hDC )
   {
      HANDLE   hPrinter;
      LPTSTR   pszPrinterName = ( LPSTR ) hb_parc( 2 );

      if( OpenPrinter( pszPrinterName, &hPrinter, NULL ) )
      {

         PDEVMODE pDevMode = NULL;
         LONG     lSize    = DocumentProperties( 0, hPrinter, pszPrinterName, pDevMode, pDevMode, 0 );

         if( lSize > 0 )
         {
            pDevMode = ( PDEVMODE ) hb_xgrab( lSize );

            if( pDevMode && DocumentProperties( 0, hPrinter, pszPrinterName, pDevMode, pDevMode, DM_OUT_BUFFER ) == IDOK ) // Get the current settings
            {
               BOOL  bAskUser          = ISBYREF( 3 ) || ISBYREF( 4 ) || ISBYREF( 5 ) ||
                                         ISBYREF( 6 ) || ISBYREF( 7 ) || ISBYREF( 8 ) || ISBYREF( 9 ) || ISBYREF( 10 ); //x 20070421
               DWORD dInit             = 0;                                                                             //x 20070421
               DWORD fMode;
               BOOL  bCustomFormSize   = ( ISNUM( 9 ) && hb_parnl( 9 ) > 0 ) && ( ISNUM( 10 ) && hb_parnl( 10 ) > 0 );  // Must set both Length & Width

               if( bCustomFormSize )
               {
                  pDevMode->dmPaperLength = ( short ) hb_parnl( 9 );
                  dInit                   |= DM_PAPERLENGTH;

                  pDevMode->dmPaperWidth  = ( short ) hb_parnl( 10 );
                  dInit                   |= DM_PAPERWIDTH;

                  pDevMode->dmPaperSize   = DMPAPER_USER;
                  dInit                   |= DM_PAPERSIZE;
               }
               else
               {
                  if( ISCHAR( 3 ) ) // this doesn't work for Win9X
                  {
                     if( ! bW9X )
                     {
                        BYTE *   cForm = ( BYTE  * ) hb_parc( 3 );
                        size_t   iLen  = ( size_t ) hb_parclen( 3 );
                        if( cForm && iLen && iLen < CCHFORMNAME )
                        {
                           HB_MEMCPY( pDevMode->dmFormName, cForm, iLen + 1 ); // Copy the trailing '\0'
                           dInit |= DM_FORMNAME;
                        }
                     }
                  }
                  else if( ISNUM( 3 ) && hb_parnl( 3 ) ) // 22/02/2007 don't change if 0
                  {
                     pDevMode->dmPaperSize   = ( short ) hb_parnl( 3 );
                     dInit                   |= DM_PAPERSIZE;
                  }
               }

               if( ISLOG( 4 ) )
               {
                  pDevMode->dmOrientation = ( short ) ( hb_parl( 4 ) ? 2 : 1 );
                  dInit                   |= DM_ORIENTATION;
               }

               if( ISNUM( 5 ) && hb_parnl( 5 ) > 0 )
               {
                  pDevMode->dmCopies   = ( short ) hb_parnl( 5 );
                  dInit                |= DM_COPIES;
               }

               if( ISNUM( 6 ) && hb_parnl( 6 ) ) // 22/02/2007 don't change if 0
               {
                  pDevMode->dmDefaultSource  = ( short ) hb_parnl( 6 );
                  dInit                      |= DM_DEFAULTSOURCE;
               }

               if( ISNUM( 7 ) && hb_parnl( 7 ) ) // 22/02/2007 don't change if 0
               {
                  pDevMode->dmDuplex   = ( short ) hb_parnl( 7 );
                  dInit                |= DM_DUPLEX;
               }

               if( ISNUM( 8 ) && hb_parnl( 8 ) ) // 22/02/2007 don't change if 0
               {
                  pDevMode->dmPrintQuality   = ( short ) hb_parnl( 8 );
                  dInit                      |= DM_PRINTQUALITY;
               }

               fMode = DM_IN_BUFFER | DM_OUT_BUFFER;

               if( bAskUser )
               {
                  fMode |= DM_IN_PROMPT;
               }

               pDevMode->dmFields = dInit;

               /* NOTES:
                  For unknown reasons, Windows98/ME returns IDCANCEL if user clicks OK without changing anything in DocumentProperties.
                  Therefore, we ignore the return value in Win9x, and assume user clicks OK.
                  IOW, DocumentProperties is not cancelable in Win9X.
                */
               if( DocumentProperties( 0, hPrinter, pszPrinterName, pDevMode, pDevMode, fMode ) == IDOK || bW9X )
               {
                  if( ISBYREF( 3 ) && ! bCustomFormSize )
                  {
                     if( ISCHAR( 3 ) )
                     {
                        if( ! bW9X )
                        {
                           hb_storc( ( char * ) pDevMode->dmFormName, 3 );
                        }
                     }
                     else
                     {
                        hb_stornl( ( LONG ) pDevMode->dmPaperSize, 3 );
                     }
                  }
                  if( ISBYREF( 4 ) )
                  {
                     hb_storl( pDevMode->dmOrientation == 2, 4 );
                  }
                  if( ISBYREF( 5 ) )
                  {
                     hb_stornl( ( LONG ) pDevMode->dmCopies, 5 );
                  }
                  if( ISBYREF( 6 ) )
                  {
                     hb_stornl( ( LONG ) pDevMode->dmDefaultSource, 6 );
                  }
                  if( ISBYREF( 7 ) )
                  {
                     hb_stornl( ( LONG ) pDevMode->dmDuplex, 7 );
                  }
                  if( ISBYREF( 8 ) )
                  {
                     hb_stornl( ( LONG ) pDevMode->dmPrintQuality, 8 );
                  }
                  if( ISBYREF( 9 ) )
                  {
                     hb_stornl( ( LONG ) pDevMode->dmPaperLength, 9 );
                  }
                  if( ISBYREF( 10 ) )
                  {
                     hb_stornl( ( LONG ) pDevMode->dmPaperWidth, 10 );
                  }
#if defined( HB_OS_WIN_64 )
                  Result   = ( BOOL ) ( HB_LONG ) ResetDC( hDC, pDevMode );
#else
                  Result   = ( BOOL ) ResetDC( hDC, pDevMode );
#endif
               }

               hb_xfree( pDevMode );
            }
         }

         ClosePrinter( hPrinter );

      }
   }
   hb_retl( Result );
}

// Functions for Loading & Printing bitmaps

HB_FUNC_STATIC( LOADBITMAPFILE )
{
   PTSTR                pstrFileName   = ( PTSTR ) hb_parc( 1 );
   BOOL                 bSuccess       = FALSE;
   DWORD                dwFileSize     = 0, dwHighSize, dwBytesRead;
   HANDLE               hFile;
   BITMAPFILEHEADER *   pbmfh          = NULL;

   hFile = CreateFile( pstrFileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL );
   if( hFile != INVALID_HANDLE_VALUE )
   {
      dwFileSize = GetFileSize( hFile, &dwHighSize );
      if( ( dwFileSize != INVALID_FILE_SIZE ) && ! dwHighSize ) // Do not continue if File size error or TOO big for memory
      {
         pbmfh = ( BITMAPFILEHEADER * ) hb_xgrab( dwFileSize );
         if( pbmfh )
         {
            bSuccess = ReadFile( hFile, pbmfh, dwFileSize, &dwBytesRead, NULL );
            bSuccess = bSuccess && ( dwBytesRead == dwFileSize ) && ( pbmfh->bfType == *( WORD * ) "BM" ); //&& (pbmfh->bfSize == dwFileSize) ;
         }
      }
      CloseHandle( hFile );
   }
   if( bSuccess )
   {
      hb_retclenAdoptRaw( ( char * ) pbmfh, dwFileSize );
   }
   else
   {
      hb_retc( "" );
      if( pbmfh != NULL )
      {
         hb_xfree( pbmfh );
      }
   }
}

HB_FUNC_STATIC( DRAWBITMAP )
{
   HDC                  hDC   = ( HDC ) hb_parns( 1 );
   BITMAPFILEHEADER *   pbmfh = ( BITMAPFILEHEADER * ) hb_parc( 2 );
   BITMAPINFO *         pbmi;
   BYTE *               pBits;
   int                  cxDib, cyDib;

   pbmi  = ( BITMAPINFO * ) ( pbmfh + 1 );
   pBits = ( BYTE * ) pbmfh + pbmfh->bfOffBits;

   if( pbmi->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) ) // Remember there are 2 types of BitMap File
   {
      cxDib = ( ( BITMAPCOREHEADER * ) pbmi )->bcWidth;
      cyDib = ( ( BITMAPCOREHEADER * ) pbmi )->bcHeight;
   }
   else
   {
      cxDib = pbmi->bmiHeader.biWidth;
      cyDib = abs( pbmi->bmiHeader.biHeight );
   }

   SetStretchBltMode( hDC, COLORONCOLOR );
   hb_retl( StretchDIBits( hDC, hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ),
                           0, 0, cxDib, cyDib, pBits, pbmi,
                           DIB_RGB_COLORS, SRCCOPY ) != ( int ) GDI_ERROR );
}

static int CALLBACK FontEnumCallBack( LOGFONT * lplf, TEXTMETRIC * lpntm, DWORD FontType, LPVOID pArray )
{
   HB_ITEM SubItems;

   SubItems.type = HB_IT_NIL;

   hb_arrayNew( &SubItems, 4 );
   hb_arraySetC( &SubItems, 1, lplf->lfFaceName                      );
   hb_arraySetL( &SubItems, 2, lplf->lfPitchAndFamily & FIXED_PITCH  );
   hb_arraySetL( &SubItems, 3, FontType && TRUETYPE_FONTTYPE         );
   hb_arraySetNL( &SubItems, 4, lpntm->tmCharSet                      );
   hb_arrayAddForward( ( PHB_ITEM ) pArray, &SubItems );

   return TRUE;
}

HB_FUNC_STATIC( ENUMFONTS )
{
   BOOL  Result   = FALSE;
   HDC   hDC      = ( HDC ) hb_parns( 1 );

   if( hDC )
   {
      HB_ITEM Array;

      Array.type = HB_IT_NIL;
      hb_arrayNew( &Array, 0 );

      EnumFonts( hDC, ( LPCTSTR ) NULL, ( FONTENUMPROC ) FontEnumCallBack, ( LPARAM ) &Array );

      hb_itemReturnForward( &Array );

      Result = TRUE;
   }

   if( ! Result )
   {
      hb_ret();
   }
}

HB_FUNC_STATIC( GETEXEFILENAME )
{
   unsigned char pBuf[ 1024 ];

   GetModuleFileName( NULL, ( LPTSTR ) pBuf, 1023 );
   hb_retc( ( char * ) pBuf );
}

HB_FUNC_STATIC( SETCOLOR )
{
   HDC hDC = ( HDC ) hb_parns( 1 );

   SetTextColor( hDC, ( COLORREF ) hb_parnl( 2 ) );
   if( ISNUM( 3 ) )
   {
      SetBkColor( hDC, ( COLORREF ) hb_parnl( 3 ) );
   }
   if( ISNUM( 4 ) )
   {
      SetTextAlign( hDC, hb_parni( 4 ) );
   }
}

HB_FUNC_STATIC( SETPEN )
{
   HDC   hDC   = ( HDC ) hb_parns( 1 );
   HPEN  hPen  = CreatePen(
      hb_parni( 2 ),             // pen style
      hb_parni( 3 ),             // pen width
      ( COLORREF ) hb_parnl( 4 ) // pen color
      );
   HPEN hOldPen = ( HPEN ) SelectObject( hDC, hPen );

   if( hOldPen )
      DeleteObject( hOldPen );

   hb_retns( ( HB_ISIZ ) hPen );
}

HB_FUNC_STATIC( FILLRECT )
{
   HDC      hDC      = ( HDC ) hb_parns( 1 );
   int      x1       = hb_parni( 2 );
   int      y1       = hb_parni( 3 );
   int      x2       = hb_parni( 4 );
   int      y2       = hb_parni( 5 );
   COLORREF dColor   = ( COLORREF ) hb_parni( 6 ); // brush color
   int      iStyle   = hb_parni( 7 );              // brush style
   LONG     lHatch   = hb_parnl( 8 );              // brush hatch
   HBRUSH   hBrush;
   RECT     rct;

   rct.top     = y1;
   rct.left    = x1;
   rct.bottom  = y2;
   rct.right   = x2;

   if( iStyle != BS_SOLID )
   {
      LOGBRUSH br;
      br.lbStyle  = iStyle;
      br.lbColor  = dColor;
      br.lbHatch  = lHatch;
      hBrush      = CreateBrushIndirect( &br );
   }
   else
   {
      hBrush = CreateSolidBrush( dColor );
   }

   FillRect( hDC, &rct, hBrush );

   DeleteObject( hBrush );

   hb_ret();
}

HB_FUNC_STATIC( LINETO )
{
   HDC   hDC   = ( HDC ) hb_parns( 1 );
   int   x1    = hb_parni( 2 );
   int   y1    = hb_parni( 3 );
   int   x2    = hb_parni( 4 );
   int   y2    = hb_parni( 5 );

   MoveToEx( hDC, x1, y1, NULL );

   hb_retl( LineTo( hDC, x2, y2 ) );
}

HB_FUNC_STATIC( RECTANGLE )
{
   HDC   hDC      = ( HDC ) hb_parns( 1 );
   int   x1       = hb_parni( 2 );
   int   y1       = hb_parni( 3 );
   int   x2       = hb_parni( 4 );
   int   y2       = hb_parni( 5 );
   int   iWidth   = hb_parni( 6 );
   int   iHeight  = hb_parni( 7 );

   if( iWidth && iHeight )
   {
      hb_retl( RoundRect( hDC, x1, y1, x2, y2, iWidth, iHeight ) );
   }
   else
   {
      hb_retl( Rectangle( hDC, x1, y1, x2, y2 ) );
   }
}

HB_FUNC_STATIC( ARC )
{
   HDC   hDC   = ( HDC ) hb_parns( 1 );
   int   x1    = hb_parni( 2 );
   int   y1    = hb_parni( 3 );
   int   x2    = hb_parni( 4 );
   int   y2    = hb_parni( 5 );

   hb_retl( Arc( hDC, x1, y1, x2, y2, 0, 0, 0, 0 ) );
}

HB_FUNC_STATIC( ELLIPSE )
{
   HDC   hDC   = ( HDC ) hb_parns( 1 );
   int   x1    = hb_parni( 2 );
   int   y1    = hb_parni( 3 );
   int   x2    = hb_parni( 4 );
   int   y2    = hb_parni( 5 );

   hb_retl( Ellipse( hDC, x1, y1, x2, y2 ) );
}

HB_FUNC_STATIC( SETBKMODE )
{
   hb_retnl( SetBkMode( ( HDC ) hb_parns( 1 ), hb_parnl( 2 ) ) );
}

HB_FUNC_STATIC( DRAWTEXT )
{
   HDC      hDC   = ( HDC ) hb_parns( 1 );
   int      x1    = hb_parni( 2 );
   int      y1    = hb_parni( 3 );
   int      x2    = hb_parni( 4 );
   int      y2    = hb_parni( 5 );

   RECT     rect;
   UINT     uFormat;

   SIZE     sSize;
   char *   pszData  = ( char * ) hb_parc( 6 );
   HB_SIZE  iLen     = strlen( pszData );
   int      iStyle   = hb_parni( 7 );
   LONG     w, h;

   SetRect( &rect, x1, y1, x2, y2 );

   GetTextExtentPoint32( hDC, pszData, ( int ) iLen, &sSize );
   w  = ( LONG ) sSize.cx; // text width
   h  = ( LONG ) sSize.cy; // text height

   // Center text vertically within rectangle.
   if( w < rect.right - rect.left )
   {
      rect.top = rect.top + ( rect.bottom - rect.top + ( h / 2 ) ) / 2;
   }
   else
   {
      rect.top = rect.top + ( rect.bottom - rect.top - ( h / 2 ) ) / 2;
   }

   uFormat = DT_NOCLIP | DT_NOPREFIX | DT_WORDBREAK | DT_END_ELLIPSIS;

   if( iStyle == 0 )
   {
      uFormat = uFormat | DT_LEFT;
   }
   else if( iStyle == 2 )
   {
      uFormat = uFormat | DT_RIGHT;
   }
   else if( iStyle == 1 )
   {
      uFormat = uFormat | DT_CENTER;
   }

   hb_retni( DrawText( hDC, pszData, -1, &rect, uFormat ) );

}

#pragma ENDDUMP
