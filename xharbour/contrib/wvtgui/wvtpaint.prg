/*
 * $Id: wvtpaint.prg,v 1.1 2005/02/06 15:52:30 fsgiudice Exp $
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2004 Pritpal Bedi <pritpal@vouchcac.com>
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
 *
 */
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                           WvtPaint.prg
//
//            Routines to manage Wvt*Classes Gui Painting
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#include 'wvtwin.ch'
#include 'common.ch'

//-------------------------------------------------------------------//

static paint_:= { { '', {} } }

//-------------------------------------------------------------------//
//
//        This function must have to be defined in your appls
//
// function Wvt_Paint()

   //  Call this function from this funtion
   //
   // WvtPaintObjects()

// return nil

//-------------------------------------------------------------------//

function WvtPaintObjects()
LOCAL i, lExe, nLeft, nRight, b, tlbr_, aBlocks, nBlocks

aBlocks := WvtSetPaint()

if ( nBlocks := len( aBlocks ) ) > 0
   tlbr_:= Wvt_GetPaintRect()

   for i := 1 to nBlocks
      lExe := .t.

      if aBlocks[ i,3 ] <> nil .and. !empty( aBlocks[ i,3 ] )
         //  Check parameters against tlbr_ depending upon the
         //  type of object and attributes contained in aAttr
         //
         do case
         case aBlocks[ i,3,1 ] == WVT_BLOCK_GRID_V
            b := aBlocks[ i,3,6 ]
            if len( b:aColumnsSep ) == 0
               lExe := .f.
            else
               nLeft  := b:aColumnsSep[ 1 ]
               nRight := b:aColumnsSep[ len( b:aColumnsSep ) ]
               if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; // top   < bottom
                     tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; // bootm > top
                     tlbr_[ 2 ] <= nRight + 1       .and. ; // left  < right
                     tlbr_[ 4 ] >= nLeft  - 2             ) // right > left
                  lExe := .f.
               endif
            endif

         case aBlocks[ i,3,1 ] == WVT_BLOCK_GETS
            if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; // top   < bott
                  tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; // bootm > top
                  tlbr_[ 2 ] <= aBlocks[ i,3,5 ] .and. ; // left  < righ
                  tlbr_[ 4 ] >= aBlocks[ i,3,3 ]       ) // right > left
               lExe := .f.
            endif

         otherwise
            // If refreshing rectangle's top is less than objects' bottom
            // and left is less than objects' right
            //
            if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; // top   <= bottom
                  tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; // bootm >= top
                  tlbr_[ 2 ] <= aBlocks[ i,3,5 ] .and. ; // left  < right
                  tlbr_[ 4 ] >= aBlocks[ i,3,3 ]       ) // right > left
               lExe := .f.
            endif
         endcase
      endif

      if lExe
         eval( aBlocks[ i,2 ] )
      endif
   next
endif

return ( 0 )

//-------------------------------------------------------------------//

function WvtSetPaint( a_ )
local o
static s := {}

o := s

if a_ <> nil
   s := a_
endif

return o

//-------------------------------------------------------------------//

function SetPaint( cID, nAction, xData, aAttr )
local n, n1, oldData

if xData <> nil
   if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
      if ( n1 := ascan( paint_[ n,2 ], {|e_| e_[ 1 ] == nAction } ) ) > 0
         oldData := paint_[ n,2,n1,2 ]
         paint_[ n,2,n1,2 ] := xData
         paint_[ n,2,n1,3 ] := aAttr
      else
         aadd( paint_[ n,2 ], { nAction,xData,aAttr } )
      endif
   else
      aadd( paint_, { cID, {} } )
      n := len( paint_ )
      aadd( paint_[ n,2 ], { nAction, xData, aAttr } )
   endif
endif

return oldData

//-------------------------------------------------------------------//

function GetPaint( cID )
local n

if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
   return paint_[ n,2 ]
endif

return {}

//-------------------------------------------------------------------//

function DelPaint( cID, nAction )
local xData, n1, n

if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
   if ( n1 := ascan( paint_[ n,2 ], {|e_| e_[ 1 ] == nAction } ) ) > 0
      xData := paint_[ n,2,n1,2 ]
      paint_[ n,2,n1,2 ] := {|| .t. }
   endif
endif

return xData

//-------------------------------------------------------------------//

function PurgePaint( cID,lDummy )
local n, aPaint

DEFAULT lDummy TO .f.

if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
   aPaint := paint_[ n ]
   ADel( paint_, n )
   aSize( paint_, len( paint_ ) - 1 )
endif

if lDummy
   WvtSetPaint( {} )
endif

return ( aPaint )

//-------------------------------------------------------------------//

function InsertPaint( cID, aPaint, lSet )
local n

DEFAULT lSet TO .f.

if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
   paint_[ n ] := aPaint
else
   aadd( paint_, aPaint )
endif

if lSet
   WvtSetPaint( aPaint )
endif

return nil

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//               RunTime Dialog Generation Routines
//
//                      Courtesy What32.lib
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

FUNCTION Wvt_MakeDlgTemplate( nTop, nLeft, nRows, nCols, aOffSet, cTitle, nStyle, ;
                              cFaceName, nPointSize, nWeight, lItalic, nHelpId, nExStyle )

   LOCAL aDlg := { {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {} }
   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL aFont

   aFont := Wvt_GetFontInfo()

   DEFAULT aOffSet TO {}

   aSize( aOffSet,4 )

   DEFAULT aOffSet[ 1 ] TO 0
   DEFAULT aOffSet[ 2 ] TO 0
   DEFAULT aOffSet[ 3 ] TO 0
   DEFAULT aOffSet[ 4 ] TO 0

   nBaseUnits  := Win_GetDialogBaseUnits()
   nBaseUnitsX := Win_LoWord( nBaseUnits )
   nBaseUnitsY := Win_HiWord( nBaseUnits )

   nW := aFont[ 7 ] * nCols + aOffSet[ 4 ]
   nH := aFont[ 6 ] * nRows + aOffSet[ 3 ]

   // Position it exactly where user has requested
   //
   aXY := Wvt_ClientToScreen( nTop,nLeft )
   nX  := aXY[ 1 ] + aOffSet[ 2 ]
   nY  := aXY[ 2 ] + aOffSet[ 1 ]

   // MSDN says DlgBaseUnits and Screen Coordinates has multiplier of 4,8 for x & Y.
   // But in my practice, the values below are 99% accurate.
   // I have tested it on many fonts but on 1280/800 resolution.
   // Please feel free to experiment if you find thses values inappropriate.
   //
   nXM :=  5.25
   nYM := 10.25

   nX  := ( nX * nXM / nBaseUnitsX )
   nY  := ( nY * nYM / nBaseUnitsY )
   nW  := ( nW * nXM / nBaseUnitsX )
   nH  := ( nH * nYM / nBaseUnitsY )

   If ValType( nStyle ) <> "N"
      nStyle := + WS_CAPTION    + WS_SYSMENU              ;
                + WS_GROUP      + WS_TABSTOP + DS_SETFONT ;
                + WS_THICKFRAME + WS_VISIBLE + WS_POPUP   ;
                + DS_3DLOOK
   EndIf

   aAdd( aDlg[ 1 ] , If( Empty( nHelpId  ), 0, nHelpId  ) )
   aAdd( aDlg[ 1 ] , If( Empty( nExStyle ), 0, nExStyle ) )
   aAdd( aDlg[ 1 ] , nStyle  )
   aAdd( aDlg[ 1 ] , 0       )
   aAdd( aDlg[ 1 ] , nX      )
   aAdd( aDlg[ 1 ] , nY      )
   aAdd( aDlg[ 1 ] , nW      )
   aAdd( aDlg[ 1 ] , nH      )
   aAdd( aDlg[ 1 ] , 0       )
   aAdd( aDlg[ 1 ] , 0       )
   aAdd( aDlg[ 1 ] , If( ValType( cTitle ) == "C", cTitle, "" ) )

   if ( nStyle & DS_SETFONT ) == DS_SETFONT
      aAdd( aDlg[ 1 ], If( ValType( nPointSize ) == "N", nPointSize, 8               ) )
      aAdd( aDlg[ 1 ], If( ValType( nWeight    ) == "N", nWeight   , 400             ) )
      aAdd( aDlg[ 1 ], If( ValType( lItalic    ) == "L", lItalic   , .F.             ) )
      aAdd( aDlg[ 1 ], If( ValType( cFaceName  ) == "C", cFaceName , "MS Sans Serif" ) )
   EndIf

   Return( aDlg )

//-------------------------------------------------------------------//

Function Wvt_AddDlgItem( aDlg, nTop, nLeft, nRows, nCols, aOffSet,;
                         cnId, cnDlgClass, nStyle, cText, nHelpId, nExStyle )
   LOCAL aXY, nX, nY, nW, nH, nXM, nYM
   LOCAL nBaseUnits, nBaseUnitsX, nBaseUnitsY
   LOCAL nBottom, nRight

   nBottom := nTop  + nRows - 1
   nRight  := nLeft + nCols - 1

   DEFAULT aOffSet TO {}

   aSize( aOffSet,4 )

   DEFAULT aOffSet[ 1 ] TO 0
   DEFAULT aOffSet[ 2 ] TO 0
   DEFAULT aOffSet[ 3 ] TO 0
   DEFAULT aOffSet[ 4 ] TO 0

   nBaseUnits  := Win_GetDialogBaseUnits()
   nBaseUnitsX := Win_LoWord( nBaseUnits )
   nBaseUnitsY := Win_HiWord( nBaseUnits )

   aXY := Wvt_GetXYFromRowCol( nTop, nLeft )
   nX  := aXY[ 1 ] + aOffSet[ 2 ]
   nY  := aXY[ 2 ] + aOffSet[ 1 ]

   aXY := Wvt_GetXYFromRowCol( nBottom+1, nRight+1 )
   nW  := aXY[ 1 ] + aOffSet[ 4 ] - nX
   nH  := aXY[ 2 ] + aOffSet[ 3 ] - nY

   nXM :=  5.25
   nYM := 10.25

   nX  := ( nX * nXM / nBaseUnitsX )
   nY  := ( nY * nYM / nBaseUnitsY )
   nW  := ( nW * nXM / nBaseUnitsX )
   nH  := ( nH * nYM / nBaseUnitsY )

   aDlg[ 1,4 ]++      // item count

   aAdd( aDlg[  2 ] , If( ValType( nHelpId  ) == "N", nHelpId , 0                     ) )
   aAdd( aDlg[  3 ] , If( ValType( nExStyle ) == "N", nExStyle, 0                     ) )
   aAdd( aDlg[  4 ] , If( ValType( nStyle   ) == "N", nStyle  , WS_CHILD + WS_VISIBLE ) )
   aAdd( aDlg[  5 ] , nX         )
   aAdd( aDlg[  6 ] , nY         )
   aAdd( aDlg[  7 ] , nW         )
   aAdd( aDlg[  8 ] , nH         )
   aAdd( aDlg[  9 ] , cnId       )
   aAdd( aDlg[ 10 ] , cnDlgClass )
   aAdd( aDlg[ 11 ] , If( ValType( cText ) <> "C", If( ValType( cText ) == "N", cText, "" ) , cText ) )
   aAdd( aDlg[ 12 ] , 0 )

   Return aDlg

//-------------------------------------------------------------------//

Function Wvt_CreateDialog( acnDlg, lOnTop, cbDlgProc, ncIcon, nTimerTicks, hMenu )
   LOCAL hDlg, cType, xTemplate, nDlgMode

   if valtype( cbDlgProc ) == 'C'
      cbDlgProc := upper( cbDlgProc )
   endif

   hDlg     := 0
   cType    := Valtype( acnDlg )
   nDlgMode := if( cType == 'C', 0, if( cType == 'N', 1, 2 ) )

   if cType == 'A'
      xTemplate := Wvt__MakeDlgTemplate( acnDlg[ 1 ] , acnDlg[  2 ] , acnDlg[  3 ] , acnDlg[  4 ] , ;
                                         acnDlg[ 5 ] , acnDlg[  6 ] , acnDlg[  7 ] , acnDlg[  8 ] , ;
                                         acnDlg[ 9 ] , acnDlg[ 10 ] , acnDlg[ 11 ] , acnDlg[ 12 ] )
   else
      xTemplate := acnDlg
   endif

   hDlg := Wvt_CreateDialogDynamic( xTemplate, lOnTop, cbDlgProc, nDlgMode )

   if hDlg <> 0
      if ncIcon <> nil
         Wvt_DlgSetIcon( hDlg, ncIcon )

      endif

      if valtype( nTimerTicks ) == 'N'
         Win_SetTimer( hDlg, 1001, nTimerTicks )

      endif

      if hMenu <> nil
         Win_SetMenu( hDlg, hMenu )

      endif

   endif

   Return hDlg

//-------------------------------------------------------------------//

Function Wvt_DialogBox( acnDlg, cbDlgProc, hWndParent )
   LOCAL nResult, cType, xTemplate, nDlgMode

   if valtype( cbDlgProc ) == 'C'
      cbDlgProc := upper( cbDlgProc )
   endif

   cType    := Valtype( acnDlg )
   nDlgMode := if( cType == 'C', 0, if( cType == 'N', 1, 2 ) )

   if cType == 'A'
      xTemplate := Wvt__MakeDlgTemplate( acnDlg[ 1 ] , acnDlg[  2 ] , acnDlg[  3 ] , acnDlg[  4 ] , ;
                                         acnDlg[ 5 ] , acnDlg[  6 ] , acnDlg[  7 ] , acnDlg[  8 ] , ;
                                         acnDlg[ 9 ] , acnDlg[ 10 ] , acnDlg[ 11 ] , acnDlg[ 12 ] )
   else
      xTemplate := acnDlg
   endif

   nResult := Wvt_CreateDialogModal( xTemplate, .f., cbDlgProc, nDlgMode, hWndParent )

   Return nResult

//-------------------------------------------------------------------//

