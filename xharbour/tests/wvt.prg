/*
 * $Id: wvt.prg,v 1.1 2004/01/17 02:36:21 fsgiudice Exp $
 */

/*
 * Harbour Project source code:
 * WVT Functions at Prg Level
 *
 * Copyright 2004 Francesco Saverio Giudice <info@fsgiudice.com>
 * www - http://www.xharbour.org http://www.harbour-project.org
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

#include "common.ch"

STATIC WVT_ObjList
STATIC WVT_Stack

INIT PROCEDURE WVT_INIT()
  LOCAL screenWidth := Wvt_GetScreenWidth()
  //DO CASE
  //   CASE screenWidth >= 1024
  //        Wvt_SetFont('Terminal', 20, 10)
  //   CASE screenWidth >= 800
  //        Wvt_SetFont('System', 16, 8, 600, 2)
  //   OTHERWISE
  //        Wvt_SetFont('Terminal', 12, 6)
  //ENDCASE
  //Wvt_SetCodePage(255)  // #define OEM_CHARSET 255 - from wingdi.h
  //SetMode(32,98)
  SetMode(25,80)
  WVT_ObjList := {}
  WVT_Stack   := {}
RETURN

/*
PROCEDURE WVT_Say( nRow, nCol, xExpr, cPict, cColor )
   //aAdd( WVT_ObjList, {|| Wvt_DrawBoxGet( nRow, nCol, Len( Transform( cVar, cPict ) ) ) } )
   DevPos( nRow, nCol )
   IF cPict <> NIL
      DevOutPict( xExpr, cPict, cColor )
   ELSE
      DevOut( xExpr, cColor )
   ENDIF
RETURN
*/

PROCEDURE WVT_Get( nRow, nCol, xVar, cVar, cPict, bValid, bWhen, pMsg, GetList )
   SetPos( nRow, nCol )
   AAdd( GetList, _GET_( xVar, cVar, cPict, bValid, bWhen ):display() )
   aAdd( WVT_ObjList, {|| Wvt_DrawBoxGet( nRow, nCol, Len( Transform( xVar, cPict ) ) ) } )
   /*
   IF pMsg <> NIL
      ATail(GetList):<msg>
   ENDIF
   */
RETURN

PROCEDURE WVT_READ()
RETURN

PROCEDURE WVT_Cls()
  WVT_ObjList := {}
RETURN

PROCEDURE WVT_AddPaintList( bBlock )
  aAdd( WVT_ObjList, bBlock )
RETURN

PROCEDURE WVT_DispBox( nTop, nLeft, nBottom, nRight, lRaised )

  IF PCount() == 4
     aAdd( WVT_ObjList, {|| Wvt_DrawLine( nTop, nLeft, nBottom, nRight ) } )
  ELSEIF PCount() == 5
     DEFAULT lRaised TO FALSE
     IF lRaised
        aAdd( WVT_ObjList, {|| Wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight ) } )
     ELSE
        aAdd( WVT_ObjList, {|| Wvt_DrawBoxRecessed( nTop, nLeft, nBottom, nRight ) } )
     ENDIF
  ENDIF
RETURN


PROCEDURE WVT_DispGroupBox( nTop, nLeft, nBottom, nRight, lRaised )
  DEFAULT lRaised TO FALSE
  IF lRaised
     aAdd( WVT_ObjList, {|| Wvt_DrawBoxGroupRaised( nTop, nLeft, nBottom, nRight ) } )
  ELSE
     aAdd( WVT_ObjList, {|| Wvt_DrawBoxGroup( nTop, nLeft, nBottom, nRight ) } )
  ENDIF
RETURN

PROCEDURE WVT_DispImage( nTop, nLeft, nBottom, nRight, cImage )
  aAdd( WVT_ObjList, {|| Wvt_DrawImage( nTop, nLeft, nBottom, nRight, cImage ) } )
RETURN


// From wingdi.h
#define TA_LEFT                      0
#define TA_RIGHT                     2
#define TA_CENTER                    6
#define TA_TOP                       0
#define TA_BOTTOM                    8
#define TA_BASELINE                  24


PROCEDURE WVT_DispLabel( nRow, nCol, cLabel, cHorAlign, cVerAlign, ncFgColor, ncBgColor, ;
                         cFontName, nFontSize, lItalic, lUnderline, lStrikeOut )
  LOCAL nPos, nColor
  LOCAL hAlign := { ;
                    "LEFT"     => TA_LEFT     ,;
                    "RIGHT"    => TA_RIGHT    ,;
                    "CENTER"   => TA_CENTER   ,;
                    "CENTERED" => TA_CENTER   ,;
                    "TOP"      => TA_TOP      ,;
                    "BOTTOM"   => TA_BOTTOM   ,;
                    "BASELINE" => TA_BASELINE  ;
                  }

  //__OutDebug( "nRow, nCol, cLabel, cHorAlign, cVerAlign" )
  //__OutDebug( "ncFgColor, ncBgColor, cFontName, nFontSize, lItalic, lUnderline, lStrikeOut" )
  //__OutDebug( nRow, nCol, cLabel, cHorAlign, cVerAlign, ;
  //            ncFgColor, ncBgColor, cFontName, nFontSize, lItalic, lUnderline, lStrikeOut )

  DEFAULT cHorAlign TO "LEFT"
  DEFAULT cVerAlign TO "TOP"
  cHorAlign := Upper( cHorAlign )
  cVerAlign := Upper( cVerAlign )

  //__OutDebug( "ncFgColor, ncBgColor, hb_ColorToN( ncFgColor ), WVT_GetRGBColor( 7 ), WVT_GetRGBColor( 23 )", ncFgColor, ncBgColor, hb_ColorToN( ncFgColor ), WVT_GetRGBColor( 7 ), WVT_GetRGBColor( 23 ) )

  IF ncFgColor <> NIL
     IF ValType( ncFgColor ) == "C"
        IF ( nPos := AT( "/", ncFgColor ) ) > 0
           nColor := hb_ColorToN( ncFgColor )
           ncBgColor := INT( nColor / 16 ) // Substr( ncFgColor, nPos+1 )
           ncFgColor := nColor - ncBgColor - 15 // Substr( ncFgColor, 1, nPos-1 )
           //__OutDebug( "ncFgColor, ncBgColor", ncFgColor, ncBgColor )
           ncFgColor := WVT_GetRGBColor( ncFgColor )
           ncBgColor := WVT_GetRGBColor( ncBgColor )
        ELSE
           ncFgColor := hb_ColorToN( ncFgColor )
           ncFgColor := WVT_GetRGBColor( ncFgColor )
        ENDIF
        //ncFgColor := WVT_GetRGBColor( hb_ColorToN( ncFgColor ) )
     ENDIF
  ENDIF
  IF ncBgColor <> NIL
     IF ValType( ncBgColor ) == "C"
        IF ( nPos := AT( "/", ncBgColor ) ) > 0
           nColor := hb_ColorToN( ncBgColor )
           ncBgColor := INT( nColor / 16 ) // Substr( ncFgColor, nPos+1 )
           ncBgColor := WVT_GetRGBColor( ncBgColor )
        ELSE
           ncBgColor := WVT_GetRGBColor( hb_ColorToN( ncBgColor ) )
        ENDIF
     ENDIF
  ENDIF
  //__OutDebug( "ncFgColor, ncBgColor", ncFgColor, ncBgColor )

  //__OutDebug( "nRow, nCol, cLabel, cHorAlign, hAlign[ cHorAlign ], cVerAlign, hAlign[ cVerAlign ]" )
  //__OutDebug( "ncFgColor, ncBgColor, cFontName, nFontSize, lItalic, lUnderline, lStrikeOut" )
  //__OutDebug( nRow, nCol, cLabel, cHorAlign, hAlign[ cHorAlign ], cVerAlign, hAlign[ cVerAlign ], ;
  //            ncFgColor, ncBgColor, cFontName, nFontSize, lItalic, lUnderline, lStrikeOut )

  aAdd( WVT_ObjList, {|| Wvt_DrawLabel( nRow, nCol, cLabel, hAlign[ cHorAlign ] + hAlign[ cVerAlign ],;
                                        /* nEscapement */,;
                                        ncFgColor, ncBgColor, ;
                                        cFontName, nFontSize, lItalic, lUnderline, lStrikeOut ) } )
RETURN

FUNCTION WVT_Color( ncColor )
  LOCAL nFgColor, nBgColor, nRGBColor, nColor
  LOCAL nPos
  IF ncColor <> NIL
     IF ValType( ncColor ) == "C"
        IF ( nPos := AT( "/", ncColor ) ) > 0
           nColor := hb_ColorToN( ncColor )
           nBgColor := INT( nColor / 16 )
           nFgColor := nColor - nBgColor - 15
           nRGBColor := WVT_GetRGBColor( nFgColor )
        ELSE
           nColor := hb_ColorToN( ncColor )
           nRGBColor := WVT_GetRGBColor( nColor )
        ENDIF
     ELSE
        nRGBColor := ncColor
     ENDIF
  ENDIF
  //__OutDebug( "ncColor, nRGBColor", ncColor, nRGBColor )
RETURN nRGBColor

FUNCTION WVT_PaintList( aNewList )
  LOCAL aOldList := WVT_ObjList
  IF aNewList <> NIL
     WVT_ObjList := aNewList
  ENDIF
RETURN aOldList

FUNCTION WVT_SaveWindow( nTop, nLeft, nBottom, nRight )
  DEFAULT nTop    TO 0
  DEFAULT nLeft   TO 0
  DEFAULT nBottom TO maxrow()
  DEFAULT nRight  TO maxcol()
RETURN { ;
         aClone( WVT_ObjList )                      ,;
         SaveScreen( nTop, nLeft, nBottom, nRight ) ,;
         SetColor()                                 ,;
         Col()                                      ,;
         Row()                                      ,;
         SetCursor()                                 ;
       }

PROCEDURE WVT_RestoreWindow( nTop, nLeft, nBottom, nRight, aWindow )
  DEFAULT nTop    TO 0
  DEFAULT nLeft   TO 0
  DEFAULT nBottom TO maxrow()
  DEFAULT nRight  TO maxcol()
  WVT_ObjList := aWindow[1]
  RestScreen( nTop, nLeft, nBottom, nRight, aWindow[2] )
  SetColor( aWindow[3] )
  SetPos( aWindow[4], aWindow[5] )
  SetCursor( aWindow[6] )
RETURN

PROCEDURE WVT_PushWindow()
  aAdd( WVT_Stack, WVT_SaveWindow( 0, 0, Maxrow(), Maxcol() ) )
  WVT_ObjList := {}
  //__OutDebug( "WVT_PushWindow(): WVT_Stack", WVT_Stack )
RETURN

PROCEDURE WVT_PopWindow()
  LOCAL aWindow
  IF Len( WVT_Stack ) > 0
     aWindow := aTail( WVT_Stack )
     WVT_RestoreWindow( 0, 0, Maxrow(), Maxcol(), aWindow )
     aSize( WVT_Stack, Len( WVT_Stack ) - 1 )
  ENDIF
  //__OutDebug( "WVT_PopWindow(): WVT_Stack", WVT_Stack )
RETURN

PROCEDURE WVT_PasteFromClipboard()
   LOCAL cPaste := WVT_GetClipboard()
   LOCAL nPos
   LOCAL oGet   := GetActive()
   //__OutDebug( cPaste )
   IF cPaste <> NIL .AND. oGet <> NIL
      IF ( nPos := AT( HB_OSNewLine(), cPaste ) ) > 0
         __Keyboard( Substr( cPaste, 1, nPos - 1 ) )
      ELSE
         __Keyboard( cPaste )
      ENDIF
   ENDIF
RETURN

PROCEDURE WVT_CopyToClipboard()
   LOCAL oGet   := GetActive()
   //__OutDebug( cPaste )
   IF oGet <> NIL
      WVT_SetClipBoard( cStr( oGet:buffer ) )
   ENDIF
RETURN

FUNCTION WVT_PAINT( hWnd, msg, wParam, lParam )
  //local aBlocks := Wvt_SetBlocks()

//aeval( aBlocks, {|e| eval( e ) } )
  //__OutDebug( "WVT_PAINT: WVT_Stack, WVT_ObjList, hWnd, msg, wParam, lParam", WVT_Stack, WVT_ObjList, hWnd, msg, wParam, lParam )
  aeval( WVT_ObjList, {|e| eval( e ) } )

return 0

