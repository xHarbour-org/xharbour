/*
 * $Id: tprint.prg,v 1.4 2004/01/21 22:08:08 peterrees Exp $
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

  TPRINT() was designed to make it easy to emulate Clipper Dot Matrix printing.
  Dot Matrix printing was in CPI ( Characters per inch & Lines per inch ).
  Even though "Mapping Mode" for TPRINT() is MM_TEXT, ::SetFont() accepts the
  nWidth parameter in CPI not Pixels. Also the default ::LineHeight is for
  6 lines per inch so ::NewLine() works as per "LineFeed" on Dot Matrix printers.
  If you do not like this then inherit from the class and override anything you want

  Simple example


  TO DO:    Colour printing
            Line & Box drawing
            etc....

  Peter Rees 21 January 2004 <peter@rees.co.nz>

*/


#include "hbclass.ch"

// Cut from wingdi.h

#define MM_TEXT             1
#define MM_LOMETRIC         2
#define MM_HIMETRIC         3
#define MM_LOENGLISH        4
#define MM_HIENGLISH        5

// Device Parameters for GetDeviceCaps()

#define HORZSIZE      4     // Horizontal size in millimeters
#define VERTSIZE      6     // Vertical size in millimeters
#define HORZRES       8     // Horizontal width in pixels
#define VERTRES       10    // Vertical height in pixels
#define NUMBRUSHES    16    // Number of brushes the device has
#define NUMPENS       18    // Number of pens the device has
#define NUMFONTS      22    // Number of fonts the device has
#define NUMCOLORS     24    // Number of colors the device supports
#define RASTERCAPS    38    // Bitblt capabilities

#define LOGPIXELSX    88    // Logical pixels/inch in X
#define LOGPIXELSY    90    // Logical pixels/inch in Y

#define PHYSICALWIDTH   110 // Physical Width in device units
#define PHYSICALHEIGHT  111 // Physical Height in device units
#define PHYSICALOFFSETX 112 // Physical Printable Area x margin
#define PHYSICALOFFSETY 113 // Physical Printable Area y margin
#define SCALINGFACTORX  114 // Scaling factor x
#define SCALINGFACTORY  115 // Scaling factor y

CLASS TPRINT

  METHOD New(cPrinter)
  METHOD Create()                // CreatesDC and sets "Courier New" font, set Orientation, Copies, Bin#
                                 // Create() ( & StartDoc() ) must be called before printing can start.
  METHOD Destroy()               // Calls EndDoc() - restores default font, Deletes DC.
                                 // Destroy() must be called to avoid memory leaks
  METHOD StartDoc(cDocame)       // Calls StartPage()
  METHOD EndDoc(lAbortDoc)       // Calls EndPage() if lAbortDoc not .T.
  METHOD StartPage()
  METHOD EndPage(lFinished)      // If lFinished = .F. then StartPage() is called for the next page of output
  METHOD NewLine()
  METHOD NewPage()
  METHOD SetFont(cFontName, nPointSize, nWidth, nBold, lUnderline, lItalic, nCharSet)
                                                                // NB: nWidth is in "CharactersPerInch"
                                                                //     _OR_ { nMul, nDiv } which equates to "CharactersPerInch"
                                                                //     _OR_ ZERO ( 0 ) which uses the default width of the font
                                                                //          for the nPointSize
                                                                //   IF nWidth (or nDiv) is < 0 then Fixed font is emulated
  METHOD SetDefaultFont()
  METHOD GetFonts()                                   // Returns array of { "FontName", lFixed, lTrueType, nCharSetRequired }
  METHOD Bold(nBoldWeight)
  METHOD UnderLine(lOn)
  METHOD Italic(lOn)
  METHOD CharSet(nCharSet)


  METHOD SetPos(nX, nY)                               // **WARNING** : (Col,Row) _NOT_ (Row,Col)
  METHOD TextOut(cString, lNewLine, lUpdatePosX)
  METHOD TextOutAt(nPosX,nPosY, cString, lNewLine, lUpdatePosX) // **WARNING** : (Col,Row) _NOT_ (Row,Col)
  METHOD GetCharWidth()
  METHOD GetCharHeight()
  METHOD GetTextWidth(cString)
  METHOD GetTextHeight(cString)
  METHOD DrawBitMap(oBmp)

//  Clipper DOS compatible functions.
  METHOD SetPrc(nRow, nCol)        // Based on ::LineHeight and current ::CharWidth
  METHOD PRow()
  METHOD PCol()
  METHOD MaxRow()                  // Based on ::LineHeight & Form dimensions
  METHOD MaxCol()                  // Based on ::CharWidth & Form dimensions

  VAR PrinterName    INIT ""
  VAR Printing       INIT .F.
  VAR HavePrinted    INIT .F.
  VAR hPrinterDc     INIT 0

// These next 4 variables must be set before calling ::Create() if
// you wish to alter the defaults
  VAR FormType       INIT 0
  VAR BinNumber      INIT 0
  VAR Landscape      INIT .F.
  VAR Copies         INIT 1

  VAR SetFontOk      INIT .F.
  VAR FontName       INIT ""                        // Current Point size for font
  VAR FontPointSize  INIT 12                        // Point size for font
  VAR FontWidth      INIT {0,0}                     // {Mul, Div} Calc width: nWidth:= MulDiv(nMul, GetDeviceCaps(shDC,LOGPIXELSX), nDiv)
                                                    // If font width is specified it is in "characters per inch" to emulate DotMatrix
  VAR fBold           INIT 0      HIDDEN            // font darkness weight ( Bold). See wingdi.h or WIN SDK CreateFont() for valid values
  VAR fUnderLine      INIT .F.    HIDDEN            // UnderLine is on or off
  VAR fItalic         INIT .F.    HIDDEN            // Italic is on or off
  VAR fCharSet        INIT 0      HIDDEN            // Default character set is OEM_CHARSET

  VAR PixelsPerInchY
  VAR PixelsPerInchX
  VAR PageHeight     INIT 0
  VAR PageWidth      INIT 0
  VAR TopMargin      INIT 0
  VAR BottomMargin   INIT 0
  VAR LeftMargin     INIT 0
  VAR RightMargin    INIT 0
  VAR LineHeight     INIT 0
  VAR CharHeight     INIT 0
  VAR CharWidth      INIT 0
  VAR fCharWidth     INIT 0      HIDDEN
  VAR BitmapsOk      INIT .F.
  VAR NumColors      INIT 1

  VAR PosX           INIT 0
  VAR PosY           INIT 0
ENDCLASS

METHOD New(cPrinter) CLASS TPRINT
  ::PrinterName := IIF(!EMPTY(cPrinter), cPrinter, GetDefaultPrinter())
  RETURN(Self)

METHOD Create() CLASS TPRINT
  LOCAL Result:= .F.
  ::Destroy()                            // Finish current print job if any
  IF !EMPTY(::hPrinterDC:= CreateDC(::PrinterName))

    // Set Form Type
    // Set Number of Copies
    // Set Orientation
    SetDocumentProperties(::hPrinterDC, ::PrinterName, ::FormType, ::Landscape, ::Copies, ::BinNumber)
    // Set mapping mode to pixels, topleft down
    SetMapMode(::hPrinterDC,MM_TEXT)
//    SetTextCharacterExtra(::hPrinterDC,0); // do not add extra char spacing even if bold
    // Get Margins etc... here
    ::PageWidth        := GetDeviceCaps(::hPrinterDC,PHYSICALWIDTH)
    ::PageHeight       := GetDeviceCaps(::hPrinterDC,PHYSICALHEIGHT)
    ::LeftMargin       := GetDeviceCaps(::hPrinterDC,PHYSICALOFFSETX)
    ::RightMargin      := (::PageWidth - ::LeftMargin)+1
    ::PixelsPerInchY   := GetDeviceCaps(::hPrinterDC,LOGPIXELSY)
    ::PixelsPerInchX   := GetDeviceCaps(::hPrinterDC,LOGPIXELSX)
    ::LineHeight       := INT(::PixelsPerInchY / 6)  // Default 6 lines per inch == # of pixels per line
    ::TopMargin        := GetDeviceCaps(::hPrinterDC,PHYSICALOFFSETY)
    ::BottomMargin     := (::PageHeight - ::TopMargin)+1

    // Set .T. if can print bitmaps
    ::BitMapsOk :=  BitMapsOk(::hPrinterDC)

    // supports Colour
    ::NumColors := GetDeviceCaps(::hPrinterDC,NUMCOLORS)

    // Set the standard font
    ::SetDefaultFont()
    ::HavePrinted:= ::Printing:= .F.
    Result:= .T.
  ENDIF
  RETURN(Result)

METHOD Destroy() CLASS TPRINT
  IF !EMPTY(::hPrinterDc)
    IF ::Printing
      ::EndDoc()
    ENDIF
    ::hPrinterDC:= DeleteDC(::hPrinterDC)
  ENDIF
  RETURN(.T.)

METHOD StartDoc(cDocName) CLASS TPRINT
  LOCAL Result:= .F.
  IF cDocName == NIL
    cDocName:= GetExeFileName()+" ["+DTOC(DATE())+' - '+TIME()+"]"
  ENDIF
  IF (Result:= StartDoc(::hPrinterDc, cDocName))
    IF !(Result:= ::StartPage(::hPrinterDc))
      ::EndDoc(.T.)
    ELSE
      ::Printing:= .T.
    ENDIF
  ENDIF
  RETURN(Result)

METHOD EndDoc(lAbortDoc) CLASS TPRINT
  IF lAbortDoc == NIL
    lAbortDoc:= .F.
  ENDIF
  IF !::HavePrinted
    lAbortDoc:= .T.
  ENDIF
  IF !lAbortDoc
    ::EndPage(.F.)
  ENDIF
  EndDoc(::hPrinterDC,lAbortDoc)
  ::Printing:= .F.
  ::HavePrinted:= .F.
  RETURN(.T.)

METHOD StartPage() CLASS TPRINT
  StartPage(::hPrinterDC)
  ::PosX:= ::LeftMargin
  ::PosY:= ::TopMargin
  RETURN(.T.)

METHOD EndPage(lStartNewPage) CLASS TPRINT
  IF lStartNewPage == NIL
    lStartNewPage:= .T.
  ENDIF
  EndPage(::hPrinterDC)
  IF lStartNewPage
    ::StartPage()
  ENDIF
  RETURN(.T.)

METHOD NewLine() CLASS TPRINT
  ::PosX:= ::LeftMargin
  ::PosY+= ::LineHeight
  RETURN(::PosY)

METHOD NewPage() CLASS TPRINT
  ::EndPage(.T.)
  RETURN(.T.)


// If font width is specified it is in "characters per inch" to emulate DotMatrix
// An array {nMul,nDiv} is used to get precise size such a the Dot Matric equivalent
// of Compressed print == 16.67 char per inch == { 3,-50 }
// If nDiv is < 0 then Fixed width printing is forced via ExtTextOut()
METHOD SetFont(cFontName, nPointSize, nWidth, nBold, lUnderline, lItalic, nCharSet) CLASS TPRINT
  LOCAL cType
  IF cFontName !=NIL
    ::FontName:= cFontName
  ENDIF
  IF nPointSize!=NIL
    ::FontPointSize:= nPointSize
  ENDIF
  IF nWidth != NIL
    cType:= VALTYPE(nWidth)
    IF cType='A'
      ::FontWidth     := nWidth
    ELSEIF cType='N' .AND. !EMPTY(nWidth)
      ::FontWidth     := {1,nWidth }
    ELSE
      ::FontWidth     := {0, 0 }
    ENDIF
  ENDIF
  IF nBold != NIL
    ::fBold := nBold
  ENDIF
  IF lUnderLine != NIL
    ::fUnderline:= lUnderLine
  ENDIF
  IF lItalic != NIL
    ::fItalic := lItalic
  ENDIF
  IF nCharSet != NIL
    ::fCharSet := nCharSet
  ENDIF
  IF (::SetFontOk:= CreateFont(::hPrinterDC, ::FontName, ::FontPointSize, ::FontWidth[1], ::FontWidth[2], ::fBold, ::fUnderLine, ::fItalic, ::fCharSet))
    ::fCharWidth        := ::GetCharWidth()
    ::CharWidth:= ABS(::fCharWidth)
    ::CharHeight:= ::GetCharHeight()
  ENDIF
  ::FontName:= GetPrinterFontName(::hPrinterDC)  // Get the font name that Windows actually used
  RETURN(::SetFontOk)

METHOD SetDefaultFont()
  RETURN(::SetFont("Courier New",12,{1, 10}, 0, .F., .F., 0))

METHOD Bold(nWeight) CLASS TPRINT
  LOCAL Result:= ::fBold
  IF nWeight!= NIL
    ::fBold:= nWeight
    IF ::Printing
      ::SetFont()
    ENDIF
  ENDIF
  RETURN(Result)

METHOD Underline(lUnderLine) CLASS TPRINT
  LOCAL Result:= ::fUnderline
  IF lUnderLine!= NIL
    ::fUnderLine:= lUnderLine
    IF ::Printing
      ::SetFont()
    ENDIF
  ENDIF
  RETURN(Result)

METHOD Italic(lItalic) CLASS TPRINT
  LOCAL Result:= ::fItalic
  IF lItalic!= NIL
    ::fItalic:= lItalic
    IF ::Printing
      ::SetFont()
    ENDIF
  ENDIF
  RETURN(Result)

METHOD CharSet(nCharSet) CLASS TPRINT
  LOCAL Result:= ::fCharSet
  IF nCharSet!= NIL
    ::fCharSet:= nCharSet
    IF ::Printing
      ::SetFont()
    ENDIF
  ENDIF
  RETURN(Result)

METHOD GetFonts() CLASS TPRINT
  RETURN(ENUMFONTS(::hPrinterDC))

METHOD SetPos(nPosX, nPosY) CLASS TPRINT
  LOCAL Result:= {::PosX, ::PosY}
  IF nPosX != NIL
    ::PosX:= INT(nPosX)
  ENDIF
  IF nPosY != NIL
    ::PosY:= INT(nPosY)
  ENDIF
  RETURN(Result)

METHOD TextOut(cString, lNewLine, lUpdatePosX) CLASS TPRINT
  LOCAL Result:= .F., nPosX
  IF lUpdatePosX == NIL
     lUpdatePosX:=.T.
  ENDIF
  IF lNewLine == NIL
    lNewLine:= .F.
  ENDIF
  IF cString!=NIL
    nPosX:= TextOut(::hPrinterDC,::PosX, ::PosY, cString, LEN(cString), ::fCharWidth)
    ::HavePrinted:= .T.
    IF lUpdatePosX
      ::PosX+= nPosX
    ENDIF
    IF lNewLine
      ::NewLine()
    ENDIF
  ENDIF
  RETURN(Result)

METHOD TextOutAt(nPosX,nPosY, cString, lNewLine, lUpdatePosX) CLASS TPRINT
  IF lNewLine == NIL
    lNewLine:= .F.
  ENDIF
  IF lUpdatePosX == NIL
    lUpdatePosX:= .T.
  ENDIF
  ::SetPos(nPosX,nPosY)
  ::TextOut(cString, lNewLine, lUpdatePosX)
  RETURN(.T.)

METHOD GetCharWidth() CLASS TPRINT
  LOCAL nWidth:= 0
  IF ::FontWidth[2] < 0 .AND. !EMPTY(::FontWidth[1])
    nWidth:= MulDiv(::FontWidth[1], ::PixelsPerInchX,::FontWidth[2])
  ELSE
    nWidth:= GetCharSize(::hPrinterDC)
  ENDIF
  RETURN(nWidth)

METHOD GetCharHeight() CLASS TPRINT
  RETURN(GetCharSize(::hPrinterDC, .T.))

METHOD GetTextWidth(cString) CLASS TPRINT
  LOCAL nWidth:= 0
  IF ::FontWidth[2] < 0 .AND. !EMPTY(::FontWidth[1])
    nWidth:= LEN(cString) + MulDiv(::FontWidth[1], ::PixelsPerInchX,::FontWidth[2])
  ELSE
    nWidth:= GetTextSize(::hPrinterDC, cString, LEN(cString))  // Return Width in device units
  ENDIF
  RETURN(nWidth)

METHOD GetTextHeight(cString) CLASS TPRINT
  RETURN(GetTextSize(::hPrinterDC, cString, LEN(cString), .F.))  // Return Height in device units

METHOD DrawBitMap(oBmp) CLASS TPRINT
  LOCAL Result:= .F.
  IF ::BitMapsOk .AND. ::Printing .AND. !EMPTY(oBmp:BitMap)
    IF (Result:= DrawBitMap(::hPrinterDc, oBmp:BitMap,oBmp:Rect[1], oBmp:Rect[2], oBmp:rect[3], oBmp:Rect[4]))
      ::HavePrinted:= .T.
    ENDIF
  ENDIF
  RETURN(Result)

METHOD SetPrc(nRow, nCol) CLASS TPRINT
  ::SetPos((nCol * ::CharWidth)+ ::LeftMArgin, (nRow * ::LineHeight) + ::TopMargin)
  RETURN(NIL)

METHOD PROW()
  RETURN(INT((::PosY- ::TopMargin)/::LineHeight))   // No test for Div by ZERO

METHOD PCOL()
  RETURN(INT((::PosX - ::LeftMargin)/::CharWidth))   // Uses width of current character

METHOD MaxRow() CLASS TPRINT
  RETURN(INT(((::BottomMargin-::TopMargin)+1) / ::LineHeight) - 1)

METHOD MaxCol() CLASS TPRINT
  RETURN(INT(((::RightMargin-::LeftMargin)+1 ) / ::CharWidth) - 1)

// Bitmap class

CLASS TBITMAP

EXPORTED:

  METHOD New()
  METHOD LoadFile(cFileName)
  METHOD Create()
  METHOD Destroy()
  METHOD Draw(oPrn,arectangle)
  VAR Rect     INIT { 0,0,0,0 }        // Coordinates to print BitMap
                                       //   XDest,                    // x-coord of destination upper-left corner
                                       //   YDest,                    // y-coord of destination upper-left corner
                                       //   nDestWidth,               // width of destination rectangle
                                       //   nDestHeight,              // height of destination rectangle
                                       // See WinApi StretchDIBits()
  VAR BitMap   INIT ""
  VAR FileName INIT ""
ENDCLASS

METHOD New() CLASS TBITMAP
  RETURN(Self)

METHOD LoadFile(cFileName) CLASS TBITMAP
  ::FileName:= cFileName
  ::Bitmap := LoadBitMapFile(::FileName)
  RETURN(!EMPTY(::Bitmap))

METHOD Create() CLASS TBITMAP  // Compatibility function for Alaska Xbase++
  Return(Self)

METHOD Destroy() CLASS TBITMAP  // Compatibility function for Alaska Xbase++
  RETURN(NIL)

METHOD Draw(oPrn, aRectangle) CLASS TBITMAP // Pass a TPRINT class reference & Rectangle array
  ::Rect:= aRectangle
  RETURN(oPrn:DrawBitMap(Self))

CLASS XBPBITMAP FROM TBITMAP   // Compatibility Class for Alaska Xbase++

ENDCLASS

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"

static HB_FUNC( CREATEDC )
{
  LONG Result = 0 ;
  if (ISCHAR(1))
  {
    Result = (LONG)  CreateDC("",hb_parc(1),NULL, NULL) ;
  }
  hb_retnl(Result) ;
}

static HB_FUNC( STARTDOC )
{
  HDC hDC = (HDC) hb_parnl(1) ;
  DOCINFO sDoc ;
  BOOL Result = FALSE;
  if (hDC )
  {
    sDoc.cbSize= sizeof(DOCINFO) ;
    sDoc.lpszDocName= hb_parc(2) ;
    sDoc.lpszOutput = NULL ;
    sDoc.lpszDatatype= NULL ;
    sDoc.fwType      = 0 ;
    Result = (BOOL) (StartDoc(hDC, &sDoc)  >0 ) ;
  }
  hb_retl(Result);
}


static HB_FUNC(ENDDOC)
{
  BOOL Result = FALSE;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC)
  {
    if (ISLOG(2) && hb_parl(2))
    {
      Result = (AbortDoc(hDC) > 0) ;
    }
    else
    {
      Result = (EndDoc( hDC) > 0) ;
    }
  }
  hb_retl(Result) ;
}

static HB_FUNC( DELETEDC )
{
  HDC hDC =  (HDC) hb_parnl(1) ;
  if (hDC)
  {
    DeleteDC( hDC ) ;
  }
  hb_retnl(0) ;  // Return zero as a new handle even if fails
}

static HB_FUNC(STARTPAGE)
{
  BOOL Result = FALSE ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC)
  {
    Result = ( StartPage( hDC ) > 0) ;
  }
  hb_retl(Result) ;
}

static HB_FUNC(ENDPAGE)
{
  BOOL Result = FALSE ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC)
  {
    Result = (EndPage( hDC ) > 0) ;
  }
  hb_retl(Result) ;
}

static HB_FUNC(TEXTOUT)
{
  LONG Result = 0 ;
  HDC hDC = (HDC) hb_parnl(1) ;
  SIZE sSize ;
  if (hDC)
  {
    int iLen   = (int) hb_parnl(5) ;
    if ( iLen > 0 )
    {
      int iRow   = (int) hb_parnl(2) ;
      int iCol   = (int) hb_parnl(3) ;
      char *pszData = hb_parc(4) ;
      int iWidth = ISNUM(6) ? (int) hb_parnl(6) : 0 ;
      SetTextAlign((HDC) hDC, TA_BOTTOM | TA_LEFT | TA_NOUPDATECP) ;
      if (iWidth < 0 && iLen < 1024 )
      {
        int n= iLen, aFixed[1024] ;
        iWidth = -iWidth ;
        while( n )
        {
          aFixed[ --n ] = iWidth;
        }
        if (ExtTextOut( hDC, iRow, iCol, 0, NULL, pszData, iLen, aFixed ))
        {
          Result = (LONG) (iLen * iWidth) ;
        }
      }
      else if (TextOut(hDC, iRow, iCol, pszData, iLen))
      {
        GetTextExtentPoint32(hDC,pszData, iLen , &sSize) ;  // Get the length of the text in device size
        Result = (LONG) sSize.cx ;   // return the width so we can update the current pen position (::PosY)
      }
    }
  }
  hb_retnl(Result) ;
}

static HB_FUNC(GETTEXTSIZE)
{
  LONG Result = 0 ;
  HDC hDC = (HDC) hb_parnl(1) ;
  SIZE sSize ;
  if (hDC)
  {
    char *pszData = hb_parc(2) ;
    int iLen   = (int) hb_parnl(3) ;
    GetTextExtentPoint32(hDC,pszData, iLen , &sSize) ;  // Get the length of the text in device size
    if (ISLOG(4) && !hb_parl(4))
    {
      Result = (LONG) sSize.cy ;   // return the height
    }
    else
    {
      Result = (LONG) sSize.cx ;   // return the width
    }
  }
  hb_retnl(Result) ;
}


static HB_FUNC( GETCHARSIZE )
{
  LONG Result = 0 ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC)
  {
    TEXTMETRIC tm;
    GetTextMetrics( hDC, &tm );
    if ( ISLOG(2) && hb_parl(2) )
    {
      Result = (LONG) tm.tmHeight;
    }
    else
    {
      Result = (LONG) tm.tmAveCharWidth;
    }
  }
  hb_retnl(Result) ;
}

static HB_FUNC( GETDEVICECAPS )
{
  LONG Result = 0 ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC && ISNUM(2))
  {
    Result = (LONG) GetDeviceCaps( hDC, hb_parnl(2)) ;
  }
  hb_retnl( Result) ;
}

static HB_FUNC( SETMAPMODE )
{
  LONG Result = 0 ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC && ISNUM(2))
  {
    Result = SetMapMode( hDC, hb_parnl(2)) ;
  }
  hb_retnl( Result) ;
}

static HB_FUNC( SETTEXTCHARACTEREXTRA )
{
  LONG Result = 0 ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC && ISNUM(2))
  {
    Result = SetTextCharacterExtra(hDC,hb_parnl(2));
  }
  hb_retnl( Result) ;
}

static HB_FUNC( SELECTOBJECT )
{
  LONG Result = 0 ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC && ISNUM(2))
  {
    Result = (LONG) SelectObject( hDC, (HGDIOBJ) hb_parnl(2)) ;
  }
  hb_retnl( Result) ;
}

static HB_FUNC( DELETEOBJECT )
{
  HGDIOBJ hObj = (HGDIOBJ) hb_parnl(1) ;
  if (hObj)
  {
    DeleteObject( hObj) ;
  }
  hb_retnl(0) ;
}

HB_FUNC( MULDIV )
{
  hb_retnl(MulDiv(hb_parnl(1), hb_parnl(2), hb_parnl(3)));
}

static HB_FUNC( CREATEFONT )
{
  BOOL Result = FALSE ;
  HDC hDC = (HDC) hb_parnl(1) ;
  HFONT hFont, hOldFont ;
  char *pszFont = hb_parc(2) ;
  int iHeight = (int) hb_parnl(3) ;
  int iMul = (int) hb_parnl(4) ;
  int iDiv = (int) hb_parnl(5) ;
  int iWidth ;
  int iWeight = (int) hb_parnl(6) ;
  DWORD dwUnderLine = (DWORD) hb_parl(7) ;
  DWORD dwItalic    = (DWORD) hb_parl(8) ;
  DWORD dwCharSet   = (DWORD) hb_parl(9) ;
  iWeight = iWeight > 0 ? iWeight : FW_NORMAL ;
  iHeight = -MulDiv(iHeight, GetDeviceCaps(hDC, LOGPIXELSY), 72);
  if (iDiv )
  {
    iWidth = MulDiv(abs(iMul), GetDeviceCaps(hDC,LOGPIXELSX), abs(iDiv)) ;
  }
  else
  {
    iWidth = 0 ; // Use the default font width
  }

  hFont = CreateFont(iHeight, iWidth, 0, 0, iWeight, dwUnderLine, dwItalic, 0,
        dwCharSet ? dwCharSet : OEM_CHARSET, OUT_DEVICE_PRECIS, CLIP_DEFAULT_PRECIS, DRAFT_QUALITY, DEFAULT_PITCH | FF_DONTCARE,  pszFont) ;
  if (hFont)
  {
    Result = TRUE;
    hOldFont = SelectObject(hDC, hFont) ;
    if ( hOldFont )
    {
      DeleteObject(hOldFont) ;
    }
  }
  hb_retl( Result ) ;
}

static HB_FUNC( GETPRINTERFONTNAME )
{
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC)
  {
    unsigned char cFont[128] ;
    GetTextFace(hDC, 127, (LPTSTR) cFont) ;
    hb_retc( cFont ) ;
  }
  else
  {
    hb_retc("") ;
  }
}

static HB_FUNC(BITMAPSOK)
{
  BOOL Result = FALSE ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC)
  {
    Result = (GetDeviceCaps(hDC, RASTERCAPS)  & RC_STRETCHDIB) ;
  }
  hb_retl(Result) ;
}

static HB_FUNC( SETDOCUMENTPROPERTIES )
{
  BOOL Result = FALSE ;
  HDC hDC = (HDC) hb_parnl(1) ;
  if (hDC)
  {
    HANDLE hPrinter ;
    LPTSTR pszPrinterName = hb_parc(2) ;
    PDEVMODE pDevMode ;
    LONG lSize ;
    if (OpenPrinter(pszPrinterName, &hPrinter, NULL))
    {
      lSize= DocumentProperties(0,hPrinter,pszPrinterName, pDevMode,pDevMode,0);
      if (lSize > 0 )
      {
        pDevMode= (PDEVMODE) hb_xgrab(lSize) ;
        if (pDevMode )
        {
          DocumentProperties(0,hPrinter,pszPrinterName, pDevMode,pDevMode,DM_OUT_BUFFER) ;
          pDevMode->dmPaperSize     = ISNUM(3) && hb_parnl(3) > 0 ? hb_parnl(3) : pDevMode->dmPaperSize ;
          pDevMode->dmOrientation   = ISLOG(4) ? (hb_parl(4) ? 2 : 1) : pDevMode->dmOrientation;
          pDevMode->dmCopies        = ISNUM(5) && hb_parnl(5) > 0 ? hb_parnl(5) : pDevMode->dmCopies;
          pDevMode->dmDefaultSource = ISNUM(6) && hb_parnl(6) > 0 ? hb_parnl(6) : pDevMode->dmDefaultSource;
          Result= (BOOL) ResetDC(hDC, pDevMode) ;
          hb_xfree(pDevMode) ;
        }
      }
      ClosePrinter(hPrinter) ;
    }
  }
  hb_retl(Result) ;
}

// Functions for Loading & Printing bitmaps

static HB_FUNC( LOADBITMAPFILE )
{
  PTSTR pstrFileName = hb_parc(1) ;
  BOOL               bSuccess= FALSE ;
  DWORD              dwFileSize, dwHighSize, dwBytesRead ;
  HANDLE             hFile ;
  BITMAPFILEHEADER * pbmfh = NULL ;
  hFile = CreateFile (pstrFileName, GENERIC_READ, FILE_SHARE_READ, NULL,OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL) ;
  if (hFile != INVALID_HANDLE_VALUE)
  {
    dwFileSize = GetFileSize (hFile, &dwHighSize) ;
    if ((dwFileSize != INVALID_FILE_SIZE) && !dwHighSize) // Do not continue if File size error or TOO big for memory
    {
      pbmfh = hb_xgrab(dwFileSize) ;
      if (pbmfh)
      {
        bSuccess = ReadFile (hFile, pbmfh, dwFileSize, &dwBytesRead, NULL) ;
        bSuccess = bSuccess && (dwBytesRead == dwFileSize) && (pbmfh->bfType == * (WORD *) "BM") && (pbmfh->bfSize == dwFileSize) ;
      }
    }
    CloseHandle (hFile) ;
  }
  if (bSuccess)
  {
    hb_retclenAdoptRaw( (char *) pbmfh, dwFileSize );
  }
  else
  {
    hb_retc("") ;
    if (pbmfh != NULL)
    {
      hb_xfree (pbmfh) ;
    }
  }
}

static HB_FUNC( DRAWBITMAP ) {
  HDC hDC                  = (HDC) hb_parnl(1) ;
  BITMAPFILEHEADER * pbmfh = (BITMAPFILEHEADER *) hb_parc(2) ;
  BITMAPINFO       * pbmi ;
  BYTE             * pBits ;
  int                cxDib, cyDib ;
  pbmi  = (BITMAPINFO *) (pbmfh + 1) ;
  pBits = (BYTE *) pbmfh + pbmfh->bfOffBits ;

  if (pbmi->bmiHeader.biSize == sizeof (BITMAPCOREHEADER)) { // Remember there are 2 types of BitMap File
    cxDib = ((BITMAPCOREHEADER *) pbmi)->bcWidth ;
    cyDib = ((BITMAPCOREHEADER *) pbmi)->bcHeight ;
  }
  else {
    cxDib =      pbmi->bmiHeader.biWidth ;
    cyDib = abs (pbmi->bmiHeader.biHeight) ;
  }
  SetStretchBltMode (hDC, COLORONCOLOR) ;
  hb_retl(StretchDIBits (hDC,(int) hb_parnl(3),(int) hb_parnl(4),(int) (hb_parnl(5)-hb_parnl(3)+1), (int) (hb_parnl(6)-hb_parnl(4)+1), 0, 0, cxDib, cyDib, pBits, pbmi, DIB_RGB_COLORS, SRCCOPY)!= GDI_ERROR) ;
}

static int CALLBACK FontEnumCallBack(LOGFONT *lplf, TEXTMETRIC *lpntm, DWORD FontType, LPVOID pArray)
{
    PHB_ITEM pSubItems, pFontName, pFixed, pTrueType, pCharSet;
//    HB_SYMBOL_UNUSED( lpntm );
    pSubItems = hb_itemArrayNew( 4 );
    pFontName = hb_itemPutC( NULL, lplf->lfFaceName);
    pFixed    = hb_itemPutL( NULL, lplf->lfPitchAndFamily & FIXED_PITCH  );
    pTrueType = hb_itemPutL( NULL, FontType & TRUETYPE_FONTTYPE);
    pCharSet  = hb_itemPutNL( NULL, lpntm->tmCharSet);
    hb_arraySet( pSubItems , 1 , pFontName ) ;
    hb_arraySet( pSubItems , 2 , pFixed ) ;
    hb_arraySet( pSubItems , 3 , pTrueType ) ;
    hb_arraySet( pSubItems , 4 , pCharSet ) ;
    hb_arrayAdd((PHB_ITEM) pArray , pSubItems);
    hb_itemRelease( pFontName ) ;
    hb_itemRelease( pFixed ) ;
    hb_itemRelease( pTrueType );
    hb_itemRelease( pCharSet );
    hb_itemRelease( pSubItems );
    return(TRUE);
}

static HB_FUNC( ENUMFONTS )
{
  BOOL Result= FALSE ;
  HDC hDC                  = (HDC) hb_parnl(1) ;
  if (hDC)
  {
    PHB_ITEM pArray = hb_itemArrayNew( 0 );
    if ( pArray )
    {
      EnumFonts(hDC, (LPCTSTR) NULL, (FONTENUMPROC) FontEnumCallBack, (LPARAM) pArray);
      hb_itemReturn( pArray) ;
      Result = TRUE ;
    }
  }
  if (!Result)
  {
    hb_ret() ;
  }
}

static HB_FUNC( GETEXEFILENAME )
{
  unsigned char pBuf[1024] ;
  GetModuleFileName(NULL, (LPTSTR) pBuf, 1023) ;
  hb_retc(pBuf) ;
}
